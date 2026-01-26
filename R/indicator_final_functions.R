# R/indicator_final_functions.R


# R/make_sfha_indicator_final.R
make_sfha_indicator_final <- function(parcels_with_firms,
                                      indicator_df,
                                      zone_2018,
                                      zone_2024,
                                      zone_2026,
                                      lomr_date_col = "lomr_date",
                                      out_prefix = "sfha",
                                      join_by = "pin10") {
  z18 <- rlang::sym(zone_2018)
  z24 <- rlang::sym(zone_2024)
  z26 <- rlang::sym(zone_2026)
  ld  <- rlang::sym(lomr_date_col)

  out18 <- paste0(out_prefix, "2018")
  out24 <- paste0(out_prefix, "2024")
  out26 <- paste0(out_prefix, "2026")

  parcels_with_firms |>
    dplyr::left_join(indicator_df, by = join_by, relationship = "many-to-one") |>
    dplyr::mutate(
      !!out18 := dplyr::if_else(!is.na(!!z18), 1L, 0L),
      !!out24 := dplyr::if_else(!is.na(!!z24), 1L, 0L),
      !!out26 := dplyr::case_when(
        in_prelim_panels == TRUE  & !is.na(!!z26) ~ 1L,
        in_prelim_panels == TRUE  &  is.na(!!z26) ~ 0L,
        in_prelim_panels == FALSE                 ~ .data[[out24]],
        TRUE                                      ~ NA_integer_
      ),
      lomr_date = as.Date(!!ld)
    ) |>
    dplyr::select(
      pin10, longitude, latitude, start_year, end_year,
      FIRM_PAN, VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
      dplyr::all_of(c(out18, out24, out26)),
      lomr_date
    )
}

#
#
# make_indicator_final_buildings <- function(parcels_with_firms,
#                                            indicator_df,
#                                            zone_2018,
#                                            zone_2024,
#                                            zone_2026,
#                                            lomr_2018 = "lomr_yearlomr2018",
#                                            lomr_2024 = "lomr_yearlomr2024",
#                                            out_prefix = "bldg_sfha") {
#   stopifnot(is.data.frame(parcels_with_firms), is.data.frame(indicator_df))
#
#   z18 <- rlang::sym(zone_2018)
#   z24 <- rlang::sym(zone_2024)
#   z26 <- rlang::sym(zone_2026)
#   l18 <- rlang::sym(lomr_2018)
#   l24 <- rlang::sym(lomr_2024)
#
#   out18 <- paste0(out_prefix, "2018")
#   out24 <- paste0(out_prefix, "2024")
#   out26 <- paste0(out_prefix, "2026")
#
#   parcels_with_firms |>
#     dplyr::left_join(indicator_df, by = "pin10", relationship = "many-to-one") |>
#     dplyr::mutate(
#       !!out18 := dplyr::if_else(!is.na(!!z18), 1L, 0L),
#       !!out24 := dplyr::if_else(!is.na(!!z24), 1L, 0L),
#       !!out26 := dplyr::case_when(
#         in_prelim_panels == TRUE  & !is.na(!!z26) ~ 1L,
#         in_prelim_panels == TRUE  &  is.na(!!z26) ~ 0L,
#         in_prelim_panels == FALSE                 ~ .data[[out24]],
#         TRUE                                      ~ NA_integer_
#       ),
#       bldg_lomr2018 = dplyr::if_else(!is.na(!!l18), 1L, 0L),
#       bldg_lomr2024 = dplyr::if_else(!is.na(!!l24), 1L, 0L)
#     ) |>
#     dplyr::select(
#       pin10, longitude, latitude, start_year, end_year,
#       FIRM_PAN, VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
#       dplyr::all_of(c(out18, out24, out26)),
#       bldg_lomr2018, bldg_lomr2024
#     )
# }
