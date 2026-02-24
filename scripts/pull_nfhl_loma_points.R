# pulls LOMA point data from NFHL

library(httr2)
library(sf)
library(dplyr)
library(purrr)
library(glue)

base <- "https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/34/query"

# Cook County bbox (lon/lat) from IL county coordinates listing
xmin <- -88.266667
ymin <-  41.450000
xmax <- -87.516667
ymax <-  42.150000

geom_env <- paste(xmin, ymin, xmax, ymax, sep = ",")

get_count <- function() {
  req <- request(base) |>
    req_url_query(
      where = "1=1",
      geometry = geom_env,
      geometryType = "esriGeometryEnvelope",
      inSR = "4326",
      spatialRel = "esriSpatialRelIntersects",
      returnCountOnly = "true",
      f = "json"
    ) |>
    req_retry(max_tries = 5) |>
    req_options(timeout = 120)

  resp <- req_perform(req)
  resp_body_json(resp)$count
}

fetch_page <- function(offset = 0L, n = 2000L) {
  req <- request(base) |>
    req_url_query(
      where = "1=1",
      geometry = geom_env,
      geometryType = "esriGeometryEnvelope",
      inSR = "4326",
      spatialRel = "esriSpatialRelIntersects",

      outFields = "CASENUMBER,STATUS,PROJECTNAME,PROJECTCATEGORY,DATEENDED,DATEENDEDSTR,CID,COMMUNITYNAME,DETERMINATIONTYPE,PDFHYPERLINKID,REVAL_STAT,LOTTYPE,OUTCOME,LAT,LON",
      returnGeometry = "true",
      outSR = "4326",

      resultOffset = as.character(offset),
      resultRecordCount = as.character(n),

      f = "geojson"
    ) |>
    req_headers(`Accept-Encoding` = "gzip") |>
    req_retry(max_tries = 3) |>
    req_options(timeout = 180)

  tmp <- tempfile(fileext = ".geojson")
  resp <- req_perform(req)
  writeBin(resp_body_raw(resp), tmp)

  g <- suppressWarnings(st_read(tmp, quiet = TRUE))

  # enforce only expected columns + geometry (prevents bind_rows type drama)
  keep <- c("CASENUMBER", "STATUS", "PROJECTNAME", "PROJECTCATEGORY", "DATEENDED", "DATEENDEDSTR",
    "CID", "COMMUNITYNAME", "DETERMINATIONTYPE", "PDFHYPERLINKID", "REVAL_STAT",
    "LOTTYPE", "OUTCOME", "LAT", "LON")
  g <- g |> select(any_of(keep))
  g
}

fetch_all_lomas_cook <- function(page_size = 2000L, pause = 0.2) {
  total <- get_count()
  message(glue("Total LOMA points (Cook bbox): {total}"))

  offsets <- seq(0L, max(0L, total - 1L), by = page_size)
  pages <- vector("list", length(offsets))
  failed <- integer(0)

  for (k in seq_along(offsets)) {
    off <- offsets[k]
    message(glue("[{k}/{length(offsets)}] offset {off}"))

    pg <- tryCatch(
      fetch_page(offset = off, n = page_size),
      error = function(e) {
        message(glue("FAILED offset {off}: {conditionMessage(e)}"))
        failed <<- c(failed, off)
        NULL
      }
    )

    pages[[k]] <- pg
    Sys.sleep(pause)
  }

  out <- bind_rows(compact(pages))
  attr(out, "failed_offsets") <- failed
  out
}

lomas_cook <- fetch_all_lomas_cook(page_size = 2000)

attr(lomas_cook, "failed_offsets")

st_write(lomas_cook, "inputs/nfhl_lomas_cook_bbox.gpkg", layer = "lomas", append = FALSE)
