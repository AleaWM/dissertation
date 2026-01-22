# R/sfha_targets_functions.R
library(sf)
library(dplyr)
library(readr)
library(stringr)

read_border <- function(border_shp, crs_out = 6454) {
  st_read(border_shp, quiet = TRUE) |>
    st_transform(crs_out)
}

read_parcels_2018 <- function(gdb_path, border, crs_out) {
  st_read(gdb_path, layer = "parcel_2018_parcel", quiet = TRUE) |>
    st_transform(crs_out) |>
    (\(x) x[st_is_valid(x), ])()
}

read_parcels_2022 <- function(gdb_path, border, crs_out) {
  st_read(gdb_path, layer = "parcel_2022_parcel2022_enhanced", quiet = TRUE) |>
    st_transform(crs_out) |>
    (\(x) x[st_is_valid(x), ])()
}

read_nfhl_sfha <- function(gdb_path, layer, border, sfha_tf_field = "SFHA_TF") {
  sf::read_sf(gdb_path, layer = layer, quiet = TRUE) |>
    filter(.data[[sfha_tf_field]] == "T") |>
    st_transform(st_crs(border)) |>
    st_intersection(border) |>
    st_cast("MULTIPOLYGON")
}

read_nfhl_lomr <- function(gdb_path, layer, border) {
  sf::read_sf(gdb_path, layer = layer, quiet = TRUE) |>
    st_transform(st_crs(border)) |>
    st_intersection(border) |>
    st_cast("MULTIPOLYGON")
}



read_prelim_sfha <- function(prelim_shp, border, crs_out = sf::st_crs(border)) {
  st_read(prelim_shp, quiet = TRUE) |>
    filter(SFHA_TF == "T") |>
    st_transform(st_crs(border)) |>
    st_intersection(border) |>
    st_cast("MULTIPOLYGON")
}


# only use 2026 FIRM panel shapefile now
read_firm_panels_shp <- function(shp_path, border, crs_out) {
  x <-  sf::st_read(shp_path, quiet = TRUE)

  x |>
    sf::st_transform("EPSG:4326") |>
    # sf::st_intersection(border) |>
    dplyr::select(
      dplyr::any_of(c("PANEL", "FIRM_PAN", "DFIRM_ID", "EFF_DAT", "EFF_DATE"))
    )
}


# identifies parcels in SFHA == T Zones
join_parcels_to_zone <- function(parcels, zone_poly, id_field) {
  # NOTE: st_join is expensive; keep it in one place.
  sf_use_s2(TRUE)
  out <- st_join(parcels, zone_poly, join = st_intersects)
  out |> filter(!is.na(.data[[id_field]]))
}

make_sfha_indicator_parcels <- function(sfha2018, sfha2024, prelim, lomr2018, lomr2024) {
  # Helper: only filter on end_year if it exists (PTAXSIM inputs)
  maybe_filter_end_year <- function(x, cutoff) {
    x_df <- x |> as.data.frame()
    if ("end_year" %in% names(x_df)) {
      x_df |> dplyr::filter(.data$end_year >= cutoff)
    } else {
      x_df
    }
  }

  # ---- SFHA rollups ----
  sfha2018_df <- sfha2018 |>
    maybe_filter_end_year(2018) |>
    dplyr::arrange(.data$pin10, .data$FLD_ZONE, .data$ZONE_SUBTY) |>
    dplyr::select(.data$pin10, .data$FLD_ZONE) |>
    dplyr::group_by(.data$pin10) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  sfha2024_df <- sfha2024 |>
    maybe_filter_end_year(2022) |>
    dplyr::arrange(.data$pin10, .data$FLD_ZONE, .data$ZONE_SUBTY) |>
    dplyr::select(.data$pin10, .data$FLD_ZONE) |>
    dplyr::group_by(.data$pin10) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  prelim_df <- prelim |>
    maybe_filter_end_year(2021) |>
    dplyr::arrange(.data$pin10, .data$FLD_ZONE, .data$ZONE_SUBTY) |>
    dplyr::select(.data$pin10, .data$FLD_ZONE) |>
    dplyr::group_by(.data$pin10) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(in_prelim = "PrePanel")

  # ---- LOMR rollups ----
  # (Optional: you *can* also filter LOMRs by end_year if the inputs have it,
  # but most of the time LOMR layers won’t.)
  lomr2018_df <- lomr2018 |>
    as.data.frame() |>
    dplyr::mutate(lomr_year = "2018") |>
    dplyr::select(.data$pin10, .data$lomr_year, .data$EFF_DATE, .data$CASE_NO) |>
    dplyr::distinct()

  lomr2024_df <- lomr2024 |>
    as.data.frame() |>
    dplyr::mutate(lomr_year = "2024") |>
    dplyr::select(.data$pin10, .data$lomr_year, .data$EFF_DATE, .data$CASE_NO) |>
    dplyr::distinct()

  lomr_join <- lomr2018_df |>
    dplyr::full_join(
      lomr2024_df,
      by = c("pin10", "CASE_NO", "EFF_DATE"),
      suffix = c("lomr2018", "lomr2024")
    ) |>
    dplyr::group_by(.data$pin10) |>
    dplyr::arrange(.data$pin10, .data$EFF_DATE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::rename(lomr_date = .data$EFF_DATE)

  # ---- Combine ----
  out <- sfha2018_df |>
    dplyr::full_join(sfha2024_df, by = "pin10", suffix = c("2018", "2024")) |>
    dplyr::full_join(prelim_df, by = "pin10") |>
    dplyr::full_join(lomr_join, by = "pin10")

  out
}
#
# make_sfha_indicator_parcels <- function(sfha2018, sfha2024, prelim, lomr2018, lomr2024) {
#   # create and use 10 digit parcel number for identifying land in sfhas or lomrs
#   sfha2018_df <- sfha2018 |>
#     as.data.frame() |>
#     filter(end_year >= 2018) |>
#     arrange(pin10, FLD_ZONE, ZONE_SUBTY) |>
#     select(pin10, FLD_ZONE) |>
#     group_by(pin10) |>
#     slice_head(n = 1) |>
#     ungroup()
#
#   sfha2024_df <- sfha2024 |>
#     filter(end_year >= 2022) |>
#     as.data.frame() |>
#     arrange(pin10, FLD_ZONE, ZONE_SUBTY) |>
#     select(pin10, FLD_ZONE) |>
#     group_by(pin10) |>
#     slice_head(n = 1) |>
#     ungroup()
#
#   prelim_df <- prelim |>
#     filter(end_year >= 2021) |>
#     as.data.frame() |>
#     arrange(pin10, FLD_ZONE, ZONE_SUBTY) |>
#     select(pin10, FLD_ZONE) |>
#     group_by(pin10) |>
#     slice_head(n = 1) |>
#     ungroup() |>
#     mutate(in_prelim = "PrePanel")
#
#   lomr2018_df <- lomr2018 |>
#     as.data.frame() |>
#     mutate(lomr_year = "2018") |>
#     select(pin10, lomr_year, EFF_DATE, CASE_NO) |>
#     distinct()
#
#
#   lomr2024_df <- lomr2024 |>
#     as.data.frame() |>
#     mutate(lomr_year = "2024") |>
#     select(pin10, lomr_year, EFF_DATE, CASE_NO) |>
#     distinct()
#
#   lomr_join <- lomr2018_df |>
#     full_join(lomr2024_df,
#       by = c("pin10", "CASE_NO", "EFF_DATE"),
#       suffix = c("lomr2018", "lomr2024")
#     )  |>
#     group_by(pin10) |>     # some parcels are in 2 LOMRs. keep the first one.
#     arrange(pin10, EFF_DATE) |>
#     slice_head(n = 1) |>
#     ungroup() |>
#     rename(lomr_date = EFF_DATE)
#
#
#   # combine parcels flagged as in SFHAs or LOMRs
#   out <- sfha2018_df |>
#     full_join(sfha2024_df, by = "pin10",  suffix = c("2018", "2024")) |>
#     full_join(prelim_df, by = "pin10") |>
#     full_join(lomr_join, by = "pin10")
#
#   # out |> group_by(pin10) |> slice_head(n = 1) |> ungroup()
# }


read_buildings <- function(buildings_files, border, crs_out) {
  # buildings_files: vector that includes the .shp (and sidecars tracked by targets)
  shp <- buildings_files[grepl("\\.shp$", buildings_files)]
  x <- sf::st_read(shp, quiet = TRUE) |>
    sf::st_transform(crs_out)

  # building footprints are often invalid
  x <- sf::st_make_valid(x)

  # keep only features intersecting your border to reduce size
  x <- sf::st_intersection(x, sf::st_make_valid(border))

  x
}

assign_buildings_to_parcels <- function(buildings, parcels, parcel_pin_field = "pin10") {
  # Keep only what we need from parcels
  p <- parcels |>
    dplyr::select(pin10, shape, municipality, politicaltownship, assessorbldgclass) |> distinct()

  # Join: each building gets the parcel pin it falls in
  # left = FALSE drops buildings that fail to match a parcel
  b <- sf::st_join(buildings, p, join = sf::st_intersects, left = FALSE)

  b |>
    dplyr::filter(!is.na(.data$pin10))
}

join_buildings_to_zone <- function(buildings_with_pin, zone_poly, id_field) {
  sf::sf_use_s2(TRUE)
  out <- sf::st_join(buildings_with_pin, zone_poly, join = sf::st_intersects)
  out |> dplyr::filter(!is.na(.data[[id_field]]))
}

make_sfha_indicator_buildings <- function(b2018, b2024, bprelim, blomr2018, blomr2024) {
  # --- SFHA zone rollups (pin10-level) ---
  roll_zone <- function(x, out_name) {
    x |>
      sf::st_drop_geometry() |>
      dplyr::select(pin10, FLD_ZONE) |>
      dplyr::filter(!is.na(.data$pin10), !is.na(.data$FLD_ZONE)) |>
      dplyr::group_by(.data$pin10) |>
      dplyr::summarise(
        "{out_name}" := paste(sort(unique(.data$FLD_ZONE)), collapse = ";"),
        .groups = "drop"
      )
  }

  b2018_df  <- roll_zone(b2018,  "bldg_FLD_ZONE_2018")
  b2024_df  <- roll_zone(b2024,  "bldg_FLD_ZONE_2024")
  bpre_df   <- roll_zone(bprelim, "bldg_FLD_ZONE_2026")


  lomr2018_df <- blomr2018 |>
    as.data.frame() |>
    mutate(lomr_year = "2018") |>
    select(pin10, lomr_year, EFF_DATE, CASE_NO) |>
    distinct()

  lomr2024_df <- blomr2024 |>
    as.data.frame() |>
    mutate(lomr_year = "2024") |>
    select(pin10, lomr_year, EFF_DATE, CASE_NO) |>
    distinct()

  lomr_join <- lomr2018_df |>
    full_join(lomr2024_df,
      by = c("pin10", "CASE_NO", "EFF_DATE"),
      suffix = c("lomr2018", "lomr2024")
    )  |>
    rename(lomr_date = EFF_DATE)

  # --- combine hits-only table ---
  out <- b2018_df |>
    dplyr::full_join(b2024_df, by = "pin10") |>
    dplyr::full_join(bpre_df,  by = "pin10") |>
    dplyr::full_join(lomr_join, by = "pin10")

  out |> group_by(pin10) |> slice_head(n = 1) |> ungroup()





}
