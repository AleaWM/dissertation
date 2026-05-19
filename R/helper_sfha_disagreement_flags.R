mismatch_flag <- function(a, b, na_counts_as_mismatch = TRUE) {
  if (na_counts_as_mismatch) {
    dplyr::coalesce(a, -9999L) != dplyr::coalesce(b, -9999L)
  } else {
    dplyr::if_else(is.na(a) | is.na(b), NA, a != b)
  }
}

a1_b0 <- function(a, b) {
  dplyr::if_else(is.na(a) | is.na(b), NA, a == 1 & b == 0)
}

add_indicator_disagreement_flags <- function(df, years = c("2018", "2026"),
                                             na_counts_as_mismatch = TRUE) {
  for (yr in years) {
    land_sfha <- paste0("land_sfha", yr)
    bldg_sfha <- paste0("bldg_sfha", yr)

    land_r500 <- paste0("risk500_land_", yr)
    bldg_r500 <- paste0("risk500_bldg_", yr)

    if (all(c(land_sfha, bldg_sfha) %in% names(df))) {
      df[[paste0("diff_sfha_land_vs_bldg_", yr)]] <-
        mismatch_flag(df[[land_sfha]], df[[bldg_sfha]], na_counts_as_mismatch)

      df[[paste0("dir_sfha_land1_bldg0_", yr)]] <-
        a1_b0(df[[land_sfha]], df[[bldg_sfha]])

      df[[paste0("dir_sfha_bldg1_land0_", yr)]] <-
        a1_b0(df[[bldg_sfha]], df[[land_sfha]])
    }

    if (all(c(land_r500, bldg_r500) %in% names(df))) {
      df[[paste0("diff_risk500_land_vs_bldg_", yr)]] <-
        mismatch_flag(df[[land_r500]], df[[bldg_r500]], na_counts_as_mismatch)

      df[[paste0("dir_risk500_land1_bldg0_", yr)]] <-
        a1_b0(df[[land_r500]], df[[bldg_r500]])

      df[[paste0("dir_risk500_bldg1_land0_", yr)]] <-
        a1_b0(df[[bldg_r500]], df[[land_r500]])
    }

    if (all(c(land_sfha, land_r500) %in% names(df))) {
      df[[paste0("diff_land_1pct_vs_0p2pct_", yr)]] <-
        mismatch_flag(df[[land_sfha]], df[[land_r500]], na_counts_as_mismatch)
    }

    if (all(c(bldg_sfha, bldg_r500) %in% names(df))) {
      df[[paste0("diff_bldg_1pct_vs_0p2pct_", yr)]] <-
        mismatch_flag(df[[bldg_sfha]], df[[bldg_r500]], na_counts_as_mismatch)
    }
  }

  df |>
    dplyr::mutate(
      any_diff_sfha_land_vs_bldg =
        rowSums(dplyr::across(dplyr::matches("^diff_sfha_land_vs_bldg_"), ~ .x %in% TRUE), na.rm = TRUE) > 0,

      any_diff_risk500_land_vs_bldg =
        rowSums(dplyr::across(dplyr::matches("^diff_risk500_land_vs_bldg_"), ~ .x %in% TRUE), na.rm = TRUE) > 0,

      any_diff_land_1pct_vs_0p2pct =
        rowSums(dplyr::across(dplyr::matches("^diff_land_1pct_vs_0p2pct_"), ~ .x %in% TRUE), na.rm = TRUE) > 0,

      any_diff_bldg_1pct_vs_0p2pct =
        rowSums(dplyr::across(dplyr::matches("^diff_bldg_1pct_vs_0p2pct_"), ~ .x %in% TRUE), na.rm = TRUE) > 0
    )
}
