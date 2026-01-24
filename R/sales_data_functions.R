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
      sale_date  = lubridate::mdy(.data$sale_date),
      numeric_price =  as.numeric(gsub("[$,]", "", sale_price))
    ) |> mutate(sale_price = numeric_price) |>
    select(-numeric_price)
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
make_sfha_timing_vars <- function(df, method = c("land", "bldg", "ptaxsim", "risk500"),
                                  min_analysis_year = 2009,
                                  min_price = 5000) {
  method <- match.arg(method)

  if (method == "land") {
    df <- df |>
      rename(
        sfha2018_col = sfha2018,
        sfha2024_col = sfha2024,
        sfha2026_col = sfha2026)

  } else if (method == "bldg") {
    df <- df |>
      rename(
        sfha2018_col =  bldg_sfha2018,
        sfha2024_col = bldg_sfha2024,
        sfha2026_col = bldg_sfha2026)

  } else if (method == "ptaxsim") {

    df <- df |>
      rename(
        sfha2018_col = ptax_sfha2018,
        sfha2024_col = ptax_sfha2024,
        sfha2026_col = ptax_sfha2026
      )
  } else if (method == "risk500") {

    df <- df |>
      rename(
        sfha2018_col = risk500_bldg_2018,
        sfha2024_col = risk500_bldg_2024,
        sfha2026_col = risk500_bldg_2026
      )
  }


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

      sfha2026 = as.numeric(sfha2026_col),

      # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
      in_eff_sfha = case_when(
        EFF_DATE == as.Date("2008-08-19") ~ sfha2018_col,
        sale_date >= EFF_DATE ~ sfha2024_col,
        sale_date < EFF_DATE ~ sfha2018_col),


      # create similar variable but for the preliminary date: model must deal with anticipation to change
      in_prelim_sfha = case_when(
        PRE_DATE == as.Date("2005-01-01") ~ sfha2018_col,    # never had FIRM update. Use SFHA polygons from 2018 NFHL before any updates occurred.
        PRE_DATE == as.Date("2021-09-22") & sale_date > PRE_DATE ~ sfha2026_col,
        sale_date >= PRE_DATE  ~ sfha2024_col,
        sale_date < PRE_DATE ~ sfha2018_col),

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
    select(pin, year, sale_date, sale_price,
      addedto_eff_sfha, removedfrom_eff_sfha, addedto_prelim_sfha, removedfrom_prelim_sfha,
      in_eff_sfha, in_prelim_sfha, FIRM_PAN,
      added_eff_thisyear:removed_pre_thisyear,
      lag_eff, lag_pre, in_lomr, everything())
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


# sales_data_functions.R --------------------------------------------------------
# Functions for dissertation-ready datasets (works with targets)

read_pin_muni_key <- function(pin_muni_key_file, muni_nicknames_file, floodfactor_file) {
  stopifnot(file.exists(pin_muni_key_file), file.exists(muni_nicknames_file))

  pin_muni_key <- readr::read_csv(pin_muni_key_file, show_col_types = FALSE)
  nicknames <- readxl::read_xlsx(muni_nicknames_file)
  puni_pins <- readr::read_csv(floodfactor_file)


  out <- pin_muni_key |>
    dplyr::left_join(nicknames) |>
    left_join(puni_pins)

  out
}


make_repeat_sales <- function(sales_prepped_bldg) {
  # problem pins for various reasons
  drop_pins <- c("05272000470000",  # co op
    "17103180800000",  # became 325 W Wacker Drive, the building that looks like water waves, condo in loop
    "14281050350000",  # is a condo building, whole building bought in 2006, Currently classed as land? probably due to remodeling permit?
    "10191030030000",     # land trust sale and then maybe split into other pins?
    "17032220150000",   #  becomes 880 N LakeShore Drive and is a CoOp
    "17032220180000",   # also part of 880 N Lakeshore Dr  and is  a CoOp
    "17032220200000",    # ditto
    "05272000010000",     # another CoOp
    "21301140150000",  # vacant lot by the lake,
    "21301140160000"   # vacant lot by the lake
  )

  out <- sales_prepped_bldg |>
    filter(!pin %in% drop_pins) |>
    mutate(sale_price =  # price corrections randomly spotted
        case_when(
          pin == "15154220240000" & sale_date == as_date("2006-11-01") ~ 180000,   # not 180 million
          pin == "08153010051151" ~ 81000, # not 81 million
          TRUE ~ sale_price)) |>
    # a couple outliers were in there
    filter(sale_price < 5000000)


  out |>
    dplyr::group_by(.data$pin) |>
    dplyr::mutate(times_sold = dplyr::n()) |>
    dplyr::filter(.data$times_sold > 1) |>
    dplyr::ungroup()
}

make_df_prep <- function(repeat_sales, pin_muni_key_tbl) {

  df <- repeat_sales |>
    dplyr::mutate(
      sale_date = as.Date(.data$sale_date),
      sale_year = lubridate::year(.data$sale_date),

      eff_date = as.Date(.data$EFF_DATE),
      pre_date = as.Date(.data$PRE_DATE),

      # insure requirement definition you used: in_eff_sfha == 1 and NOT in a LOMR
      ins_req = if_else(in_eff_sfha == 1 & in_lomr == FALSE, TRUE, FALSE)
    ) |>
    # keep only what you said you need
    dplyr::select(
      dplyr::any_of(c(
        "pin", "sale_date", "sale_year", "sale_price",
        "ins_req", "in_eff_sfha", "in_prelim_sfha", "in_lomr",
        "EFF_DATE", "PRE_DATE", "eff_date", "pre_date",
        "addedto_eff_sfha", "removedfrom_eff_sfha",
        "addedto_prelim_sfha", "removedfrom_prelim_sfha",
        "condo", "class", "FIRM_PAN",
        "nbhd_code", "zip_code", "Triad", "Township", "clean_name"
      ))
    )

  df <- left_join(df, pin_muni_key_tbl)


  # create event + FF variables + default clean_name
  df <- df |>
    dplyr::mutate(
      new_info_released = as.Date("2020-06-29"),
      event = dplyr::if_else(.data$sale_date <= .data$new_info_released, "1. Pre", "2. Post"),
      high_ff_score = ifelse(env_flood_fs_factor > 4, TRUE, FALSE),
    ) |>
    # drop columns you said you remove (only if they exist)
    dplyr::select(-dplyr::any_of(c("agency_name", "agency_num", "short_name", "shpfile_name"))) |>
    # cleanup helper fields
    dplyr::select(-dplyr::any_of(c("clean_name_manual", "flood_factor_score_manual", "in_lomr_flag")))

  # change_type factors (your exact logic, but robust)
  df <- df |>
    group_by(pin) |>
    dplyr::mutate(
      times_sold = n(),
      change_type = case_when(
        sum(in_eff_sfha) == times_sold ~ "Always SFHA",
        sum(in_eff_sfha) == 0 ~ "Never SFHA",
        T ~ "Changes SFHA"),
      change_type_prelim =
        case_when(
          sum(in_prelim_sfha) == times_sold ~ "Always SFHA",
          sum(in_prelim_sfha) == 0 ~ "Never SFHA",
          T ~ "Changes SFHA")) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      change_type = factor(.data$change_type, levels = c("Never SFHA", "Always SFHA", "Changes SFHA")),
      change_type_prelim = factor(.data$change_type_prelim, levels = c("Never SFHA", "Always SFHA", "Changes SFHA"))
    )

  df
}

fill_missing_muni_by_nbhd_zip <- function(df_prep) {
  # Uses first match (your warning). Some zips/nbhds span multiple munis.
  if (!all(c("zip_code", "nbhd_code") %in% names(df_prep))) return(df_prep)

  # build lookup from non-missing rows
  muni_nbhd_lookup <- df_prep |>
    dplyr::distinct(.data$nbhd_code, .data$zip_code, .data$clean_name, .data$Triad, .data$Township) |>
    dplyr::filter(
      !is.na(.data$clean_name),
      !is.na(.data$nbhd_code),
      !is.na(.data$zip_code),
      !is.na(.data$Triad),
      !is.na(.data$Township)
    )

  out <- df_prep |>
    dplyr::left_join(
      muni_nbhd_lookup,
      by = c("zip_code", "nbhd_code"),
      multiple = "first",
      suffix = c(".x", ".y")
    ) |>
    dplyr::mutate(
      clean_name = dplyr::coalesce(.data$clean_name.x, .data$clean_name.y),
      Triad = dplyr::coalesce(.data$Triad.x, .data$Triad.y),
      Township = dplyr::coalesce(.data$Township.x, .data$Township.y)
    ) |>
    dplyr::select(-dplyr::any_of(c(
      "clean_name.x", "clean_name.y",
      "Triad.x", "Triad.y",
      "Township.x", "Township.y"
    )))

  manual_ff_scores <- readxl::read_xlsx("data/processed/pins_with_some_addresses_forgoogle.xlsx") |>
    mutate(pin = str_pad(pin, 14, "left", "0"),
      pin10 = str_sub(pin, 1, 10)) |>
    select(pin10, flood_factor_score, clean_name)



  out <- out  |>
    mutate(clean_name = ifelse(is.na(clean_name), "Unincorporated", clean_name)) |>
    mutate(pin10 = str_sub(pin, 1, 10)) |>
    left_join(manual_ff_scores, by = "pin10")

  out <- out |>
    mutate(flood_factor_score = ifelse(is.na(env_flood_fs_factor), flood_factor_score, env_flood_fs_factor),
      clean_name = ifelse(is.na(clean_name.x), clean_name.y, clean_name.x),
      high_ff_score = ifelse(env_flood_fs_factor > 4 | flood_factor_score > 4, TRUE, FALSE))

  out
}

# keeps sales from 2010 and onward
final_df_prep_filters <- function(df_prep, min_sale_date = as.Date("2010-01-01")) {
  df_prep |>
    dplyr::filter(.data$sale_date > min_sale_date)
}
