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
  # create and use 10 digit parcel number for identifying land in sfhas or lomrs
  sfha2018_df <- sfha2018 |>
    as.data.frame() |>
    select(pin10, FLD_ZONE) |>
    distinct()

  sfha2024_df <- sfha2024 |>
    as.data.frame() |>
    select(pin10, FLD_ZONE) |>
    distinct()

  prelim_df <- prelim |>
    as.data.frame() |>
    select(pin10, FLD_ZONE) |>
    distinct()

  lomr2018_df <- lomr2018 |>
    as.data.frame() |>
    mutate(lomr_year = "2018") |>
    select(pin10, lomr_year, EFF_DATE, CASE_NO) |>
    distinct()

  lomr2024_df <- lomr2024 |>
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


  # combine parcels flagged as in SFHAs or LOMRs
  out <- sfha2018_df |>
    full_join(sfha2024_df, by = "pin10",  suffix = c("2018", "2024")) |>
    full_join(prelim_df, by = "pin10", suffix = c("_", "prelim")) |>
    full_join(lomr_join, by = "pin10")

  out |> group_by(pin10) |> slice_head(n = 1) |> ungroup()
}
