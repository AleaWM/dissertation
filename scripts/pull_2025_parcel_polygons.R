library(httr2)
library(sf)
library(dplyr)
library(purrr)
library(glue)

base <- "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/2025/query"

get_count <- function(where = "1=1") {
  req <- request(base) |>
    req_url_query(where = where, returnCountOnly = "true", f = "json") |>
    req_retry(max_tries = 5)
  resp <- req_perform(req)
  resp_body_json(resp)$count
}

fetch_page_retry <- function(where = "1=1",
                             offset = 0L,
                             n = 1000L,              # <- start smaller than 2000
                             out_sr = 3435,
                             max_tries = 6L,
                             base_sleep = 1.5) {

  req <- request(base) |>
    req_url_query(
      where = where,
      outFields = "*",
      returnGeometry = "true",
      outSR = as.character(out_sr),
      resultOffset = as.character(offset),
      resultRecordCount = as.character(n),
      f = "geojson"
    ) |>
    # curl options: timeout + accept compression
    req_options(timeout = 120) |>
    req_headers(`Accept-Encoding` = "gzip") |>
    # httr2-level retry (helps on transient 5xx, timeouts, etc.)
    req_retry(max_tries = 3)

  for (i in seq_len(max_tries)) {
    tmp <- tempfile(fileext = ".geojson")
    ok <- tryCatch(
      {
        resp <- req_perform(req)
        writeBin(resp_body_raw(resp), tmp)
        g <- suppressWarnings(st_read(tmp, quiet = TRUE))
        g
      },
      error = function(e) {
        NULL
      })

    if (!is.null(ok)) return(ok)

    sleep_s <- base_sleep * (2^(i - 1)) + runif(1, 0, 0.5)
    message(glue("Offset {offset}: attempt {i} failed; sleeping {round(sleep_s,1)}s"))
    Sys.sleep(sleep_s)
  }

  stop(glue("Offset {offset} failed after {max_tries} attempts"))
}

fetch_all_parcels <- function(where = "1=1",
                              page_size = 1000L,
                              out_sr = 3435,
                              start_offset = 0L,
                              pause = 0.25) {

  total <- get_count(where)
  message(glue("Total features: {total}"))

  offsets <- seq(start_offset, max(0L, total - 1L), by = page_size)

  pages <- vector("list", length(offsets))
  failed_offsets <- integer(0)

  for (k in seq_along(offsets)) {
    off <- offsets[k]
    message(glue("[{k}/{length(offsets)}] Downloading offset {off}"))

    pg <- tryCatch(
      fetch_page_retry(where = where, offset = off, n = page_size, out_sr = out_sr),
      error = function(e) {
        message(glue("FAILED offset {off}: {conditionMessage(e)}"))
        failed_offsets <<- c(failed_offsets, off)
        NULL
      }
    )

    pages[[k]] <- pg
    Sys.sleep(pause)  # be polite so the server doesn’t hate you more
  }

  pages_ok <- compact(pages)

  out <- bind_rows(pages_ok) |>
    st_make_valid()

  attr(out, "failed_offsets") <- failed_offsets
  out
}

parcels_2025 <- fetch_all_parcels(page_size = 1000)

failed <- attr(parcels_2025, "failed_offsets")
failed

st_write(parcels_2025, "inputs/cook_parcels_2025.gpkg", layer = "parcels_2025", append = FALSE)
