# R/sales_data_functions.R
# Functions used in Stage 2 of the _targets pipeline:
# read sales -> join SFHA/FIRM indicators -> create dissertation-ready variables
#
# NOTE:
# - These functions return objects. Do NOT save files inside these functions.
# - Any manual overrides (e.g., filling missing PRE/EFF dates) should be supplied
#   as data frames so targets can track them as inputs.

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

#' Read Cook County Assessor parcel sales and do minimal cleaning
#'
#' @param sales_csv Path to the raw sales CSV.
#' @param min_year Keep sales with year > min_year (default 2005 to match your script).
#' @return A tibble of sales with pin10 and parsed sale_date.
read_sales_assessor <- function(sales_csv, min_year = 2005) {
  readr::read_csv(sales_csv, show_col_types = FALSE) |>
    dplyr::filter(.data$year > min_year) |>
    dplyr::mutate(
      class_1dig = stringr::str_sub(.data$class, 1, 1),
      pin10      = stringr::str_sub(.data$pin, 1, 10),
      sale_date  = lubridate::mdy(.data$sale_date)
    )
}


#' Join sales to SFHA indicator table created in Stage 1 (sfha_indicator_final)
#'
#' @param sales Sales tibble.
#' @param sfha_indicator_final Output from Stage 1. Must contain pin10 and SFHA/FIRM columns.
#' @return Sales with SFHA/FIRM fields attached.
merge_sales_sfha <- function(sales, sfha_indicator_final) {
  sales |>
    dplyr::left_join(sfha_indicator_final, by = "pin10")
}

#' Create SFHA timing variables, LOMR indicator, and add/remove change flags
#'
#' This encapsulates the logic you were using in data_prep_new.R, but as a pure function.
#' Assumptions:
#' - sfha2018/sfha2026 are 0/1 indicators
#' - PRE_DATE / EFF_DATE are Date (or coercible)
#' - lomr_date exists and is coercible to Date (if absent, the LOMR flag will be FALSE)
#'
#' @param df Sales joined to sfha_indicator_final
#' @param min_analysis_year Keep records with year > min_analysis_year (default 2009 from your script)
#' @param min_price Keep sale_price > min_price when building lags (default 5000 from your script)
#' @return df with in_eff_sfha, in_prelim_sfha, in_lomr, ins_req, lag vars, and added/removed flags
make_sfha_timing_vars <- function(df,
                                  min_analysis_year = 2009,
                                  min_price = 5000) {

  out <- df |>
    dplyr::filter(.data$year > min_analysis_year) |>
    dplyr::mutate(
      PRE_DATE = as.Date(.data$PRE_DATE),
      EFF_DATE = as.Date(.data$EFF_DATE),

      EFF_DATE = ifelse(EFF_DATE == as.Date("2026-01-23"), as.Date("2008-08-19"), as.Date(as.character(EFF_DATE))),

      EFF_DATE = case_when(
        is.na(EFF_DATE) & neighborhood_code %in% c("10024", "18030", "19020", "19060", "24032", "25160",  "30011",
          "38040", "38110", "70080", "71074", "74022",  "74030",   "77120", "77131") ~ as.Date("2008-08-19"),
        is.na(EFF_DATE) & neighborhood_code %in% c("13032", "28039", "28100", "15907",  "39081", "39200", "39211") ~ as.Date("2019-11-01"),
        is.na(EFF_DATE) & neighborhood_code %in% c("23092", "23171", "70010",  "73032", "73041",  "73084",
          "73093",  "74013",  "76010", "76011") ~ as.Date("2021-09-10"),
        TRUE ~ as.Date(EFF_DATE)),
      PRE_DATE = case_when(
        is.na(PRE_DATE) & neighborhood_code %in% c("19020", "19060", "24032", "25160",  "30011",
          "38040", "38110", "70080", "71074", "74022",  "74030", "77120", "77131") ~ as.Date("2005-01-01"),
        is.na(PRE_DATE) &  neighborhood_code %in% c("13032", "15907", "28039", "28100", "39200", "39081", "39211") ~ as.Date("2015-02-12"),
        is.na(PRE_DATE) & neighborhood_code %in% c("23092", "23171", "70010",  "73032", "73041",  "73084",
          "73093",  "74013",  "76010", "76011") ~ as.Date("2019-07-01"),
        is.na(PRE_DATE) & neighborhood_code %in% c("10024", "18030") ~ as.Date("2021-09-22"),

        TRUE ~ as_date(PRE_DATE)),

      sfha2026 = as.numeric(sfha2026),

      # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
      in_eff_sfha = case_when(
        EFF_DATE == as.Date("2008-08-19") ~ sfha2018,
        sale_date >= EFF_DATE ~ sfha2024,
        sale_date < EFF_DATE ~ sfha2018),


      # create similar variable but for the preliminary date: model must deal with anticipation to change
      in_prelim_sfha = case_when(
        PRE_DATE == as.Date("2005-01-01") ~ sfha2018,
        sale_date >= PRE_DATE  ~ sfha2026,
        sale_date < PRE_DATE ~ sfha2018),

      in_lomr       = ifelse(!is.na(lomr_date) & sale_date >= lomr_date, TRUE, FALSE),
    )


  # Lags + change flags require sorting within PIN
  out <- out |>
    dplyr::filter(.data$sale_price > min_price) |>
    dplyr::group_by(.data$pin) |>
    dplyr::arrange(.data$sale_date, .by_group = TRUE) |>
    dplyr::mutate(
      lag_eff = dplyr::lag(.data$in_eff_sfha),
      lag_pre = dplyr::lag(.data$in_prelim_sfha)) |>
    ungroup() |>

    mutate(
      added_eff_thisyear =
        !is.na(lag_eff) &
          !is.na(in_eff_sfha) &
          lag_eff == 0 & in_eff_sfha == 1,

      removed_eff_thisyear =
        !is.na(lag_eff) &
          !is.na(in_eff_sfha) &
          lag_eff == 1 & in_eff_sfha == 0,

      added_pre_thisyear =
        !is.na(lag_pre) &
          !is.na(in_prelim_sfha) &
          lag_pre == 0 & in_prelim_sfha == 1,

      removed_pre_thisyear =
        !is.na(lag_pre) &
          !is.na(in_prelim_sfha) &
          lag_pre == 1 & in_prelim_sfha == 0
    ) |>
    dplyr::ungroup()

  out |>
    group_by(pin) |>
    arrange(pin, sale_date) |>
    mutate(
      addedto_prelim_sfha = (cumany(added_pre_thisyear == T)),
      removedfrom_prelim_sfha = (cumany(removed_pre_thisyear == T)),
      addedto_eff_sfha = (cumany(added_eff_thisyear == T)),
      removedfrom_eff_sfha = (cumany(removed_eff_thisyear == T)),
    ) |>
    ungroup() |>
    select(pin, year, sale_date, sale_price, in_eff_sfha, in_prelim_sfha,
      addedto_eff_sfha, removedfrom_eff_sfha, addedto_prelim_sfha, removedfrom_prelim_sfha,
      added_eff_thisyear:removed_pre_thisyear,
      lag_eff, lag_pre, in_eff_sfha, in_lomr, everything())
}

#' Build residential sales dataset (your dissertation "main" sample)
#'
#' Mirrors the filters and variables you used in data_prep_new.R.
#' @param df Output of make_sfha_timing_vars()
#' @return residential sales tibble
build_res_sales <- function(df) {
  out <- df |>
    dplyr::filter(!(.data$class %in% c(213, 218, 219))) |>
    dplyr::filter(.data$num_parcels_sale < 6) |>
    dplyr::mutate(
      class = as.numeric(.data$class),
      res_c2 = .data$class > 200 & .data$class < 300,
      condo = dplyr::if_else(.data$class %in% c(298, 299), "Condo", "Not Condo")
    ) |>
    dplyr::group_by(.data$pin) |>
    dplyr::arrange(.data$year, .by_group = TRUE) |>
    dplyr::filter(any(.data$res_c2)) |>
    dplyr::mutate(
      times_sold      = dplyr::n(),
      years_btw_sales = .data$year - dplyr::lag(.data$year),
      sold_once       = .data$times_sold == 1,
      sold_multi      = .data$times_sold > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$times_sold < 15)

  out
}

#' Convenience: repeat-sales subset of res_sales
build_repeat_res_sales <- function(res_sales) {
  res_sales |>
    dplyr::filter(.data$times_sold > 1)
}
