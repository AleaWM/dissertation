# R/sfha_targets_functions.R
library(sf)
library(dplyr)
library(readr)
library(stringr)

read_border <- function(border_shp, crs_out = "EPSG:4326"
) {
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
    st_transform("EPSG:4326") |>
    st_intersection(border) |>
    st_cast("MULTIPOLYGON")
}


# only use 2026 FIRM panel shapefile now
read_firm_panels_shp <- function(shp_path, border, crs_out) {
  x <-  sf::st_read(shp_path, quiet = TRUE)
  x |>
    sf::st_transform("EPSG:4326") |>
    sf::st_intersection(border) |>
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


make_sfha_indicator_parcels <- function(sfha2018, sfha2024, prelim, lomr2018, lomr2024 # ,
                                        # indicator_style = c("01", "NA1")
) {
  indicator_style <- match.arg(indicator_style)

  # build pin10 consistently from parcel name field (your scripts use `name`)
  sfha2018_df <- sfha2018 |>
    as.data.frame() |>
    transmute(pin = name, pin10 = str_sub(name, 1, 10), sfha2018 = 1L) |>
    distinct(pin10, .keep_all = TRUE)

  sfha2024_df <- sfha2024 |>
    as.data.frame() |>
    transmute(pin = name, pin10 = str_sub(name, 1, 10), sfha2024 = 1L) |>
    distinct(pin10, .keep_all = TRUE)

  prelim_df <- prelim |>
    as.data.frame() |>
    transmute(pin = name, pin10 = str_sub(name, 1, 10), prelimsfha = 1L) |>
    distinct(pin10, .keep_all = TRUE)

  lomr2018_df <- lomr2018 |>
    as.data.frame() |>
    transmute(pin = name, pin10 = str_sub(name, 1, 10), lomr2018 = 1L, EFF_DAT = EFF_DATE, CASE_NO = CASE_NO) |>
    mutate(lomr_year = "2018") |>
    distinct(pin10, .keep_all = TRUE)

  lomr2024_df <- lomr2024 |>
    as.data.frame() |>
    transmute(pin = name, pin10 = str_sub(name, 1, 10), lomr2024 = 1L, EFF_DAT = EFF_DATE, CASE_NO = CASE_NO) |>
    mutate(lomr_year = "2024") |>
    distinct(pin10, .keep_all = TRUE)

  #  lomr18_dates <- lomr_date_by_pin(lomr2018, "lomr_date_2018")

  # lomr24_dates <- lomr_date_by_pin(lomr2024, "lomr_date_2024")

  lomr_join <- lomr2018_df |>
    full_join(lomr2024_df,
      by = c("pin10", "CASE_NO", "EFF_DAT"),
      suffix = c("lomr2018", "lomr2024")
    )  |>
    group_by(CASE_NO) |>
    arrange(pin10) |>
    rename(lomr_date = EFF_DAT)


  # combine parcels flagged as in SFHAs or LOMRs
  out <- sfha2018_df |>
    full_join(sfha2024_df, by = "pin10") |>
    full_join(prelim_df, by = "pin10") |>
    full_join(lomr_join, by = "pin10")

  # # Then recode indicators
  # out <- out_v1 |>
  #   mutate(
  #     sfha2018 = ifelse(is.na(sfha2018), NA_integer_, sfha2018),
  #     sfha2024 = ifelse(is.na(sfha2024), NA_integer_, sfha2024),
  #     prelimsfha = ifelse(is.na(prelimsfha), NA_integer_, prelimsfha),
  #     lomr2018 = ifelse(is.na(lomr2018), NA_integer_, lomr2018),
  #     lomr2024 = ifelse(is.na(lomr2024), NA_integer_, lomr2024)
  #   ) |>
  #   distinct(pin10, .keep_all = TRUE)

  #   # keeps option in function for 01 coding instead of TRUE FALSE coding?
  #   if (indicator_style == "01") {
  #     out <- out |>
  #       mutate(
  #         sfha2018 = ifelse(is.na(sfha2018), 0L, sfha2018),
  #         sfha2024 = ifelse(is.na(sfha2024), 0L, sfha2024),
  #         prelimsfha = ifelse(is.na(prelimsfha), 0L, prelimsfha),
  #         lomr2018 = ifelse(is.na(lomr2018), 0L, lomr2018),
  #         lomr2024 = ifelse(is.na(lomr2024), 0L, lomr2024)
  #       )
  #   }

  out
}
