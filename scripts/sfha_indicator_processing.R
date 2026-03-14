# sfha_indicator_processing.R

# This script reads flood‑risk indicator spreadsheets produced from the
# National Flood Hazard Layer (NFHL) downloads and builds pin‑level
# indicator variables.  Each input spreadsheet corresponds to a
# particular combination of vintage (2018 vs 2026), geographic unit
# (building vs parcel), and risk definition (SFHA vs 1‑in‑500 year
# flood zone).  The goal is to create a single, wide table in which
# each row is a unique 10‑digit PIN (pin10) and each column is a
# boolean indicator flagging whether that PIN falls in the relevant
# risk polygon.

# The code assumes the nine Excel files live in your working
# directory.  Filenames are listed in the `files` list below.  If
# they reside elsewhere, adjust the paths accordingly.  When run,
# the script produces two CSV files:
#   • indicator_combined.csv  – a wide table of pin10 and indicator
#     variables
#   • indicator_summary.csv   – a long format summary of how many
#     unique pins are flagged in each indicator


library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)
library(tidyverse)
library(readxl)

source("R/helper_pins_to_drop.R")

# -------------------------------------------------------------------
# Define the input spreadsheets.  Names on the left become column
# names in the output; values on the right are the filenames.  These
# names follow the conventions shown in the provided screenshots:
#   bldg_parcel_sfha_2018.xlsx, bldg_parcel_sfha_2026.xlsx,
#   bldg_parcel_1in500_2018.xlsx, bldg_parcel_1in500_2026.xlsx,
#   parcel_SFHA_2018.xlsx, parcel_SFHA_2026.xlsx,
#   parcel_1in500_2018.xlsx, parcel_1in500_2026.xlsx
# Adjust these names if your actual files differ.
files <- list(
  bldg_sfha_2018      = "./data/processed/bldg_parcel_sfha_2018.xlsx",
  bldg_sfha_2026      = "./data/processed/bldg_parcel_sfha_2026.xlsx",
  bldg_1in500_2018    = "./data/processed/bldg_parcel_1in500_2018.xlsx",
  bldg_1in500_2026    = "./data/processed/bldg_parcel_1in500_2026.xlsx",
  parcel_sfha_2018    = "./data/processed/parcel_SFHA_2018.xlsx",
  parcel_sfha_2026    = "./data/processed/parcel_SFHA_2026.xlsx",
  parcel_1in500_2018  = "./data/processed/parcel_1in500_2018.xlsx",
  parcel_1in500_2026  = "./data/processed/parcel_1in500_2026.xlsx",
  parcel_lomrs_2025  =  "./data/processed/parcel_2025_lomr_2026.xlsx"
)

lomr_dates <- read_xlsx("./data/processed/parcel_2025_lomr_2026.xlsx") |>
  select(pin10 = PIN10,
    lomr_date = EFF_DATE,
    LOMR_ID) |>
  distinct() |>    # still has pins with multiple LOMR dates, keep the earliest one!
  group_by(pin10) |>
  arrange(lomr_date) |>
  summarize(lomr_date = min(lomr_date),
    LOMR_ID = first(LOMR_ID))

# -------------------------------------------------------------------
# Helper function to read a single indicator spreadsheet, extract
# pin10 values, and summarise to a unique set of pin10s with a
# TRUE/FALSE indicator.  It handles various field names for the
# parcel identifier and pads numeric values to ensure 10 digits.
read_indicator <- function(path, indicator_name) {
  df <- readxl::read_xlsx(path)
  # Standardise column names to lower case for easier matching
  names(df) <- tolower(names(df))

  # Identify the pin column.  Some files use pin10, others might
  # label it as pin or name (Name is often a 14‑digit PIN; we take
  # the first 10 digits).  Throw an error if none are found.
  pin_column <- NULL
  if ("pin10" %in% names(df)) {
    pin_column <- df$pin10
  } else if ("pin" %in% names(df)) {
    pin_column <- df$pin
  } else if ("name" %in% names(df)) {
    pin_column <- substr(as.character(df$name), 1, 10)
  } else {
    stop("No pin10/pin/name column found in file: ", path)
  }

  # Convert to character and pad with leading zeros to guarantee
  # ten digits.  The as.character call prevents scientific notation.
  pin10_chr <- str_pad(as.character(pin_column), width = 10, side = "left", pad = "0")

  # Produce a tibble with one row per pin10 and a logical indicator
  tibble(pin10 = pin10_chr, indicator = TRUE) %>%
    group_by(pin10) %>%
    summarise("{indicator_name}" := any(indicator), .groups = "drop")
}

# -------------------------------------------------------------------
# Read and summarise each file into its own indicator table.  The
# purrr::imap function passes both the file path (.x) and the name of
# the list element (.y) into the helper function, allowing us to
# create appropriately named columns.
indicator_list <- purrr::imap(files, ~ read_indicator(.x, .y))

# Combine all indicators into one wide table.  A full join ensures
# that pins appearing in any file are represented.  Reduce iterates
# pairwise through the list.  After combining, missing values are
# replaced with FALSE to indicate that the pin was not present in
# that particular risk layer.
indicator_combined <- Reduce(function(x, y) full_join(x, y, by = "pin10"), indicator_list)

indicator_combined <- indicator_combined %>%
  left_join(lomr_dates) |>
  mutate(across(-c(pin10, lomr_date, LOMR_ID), ~ if_else(is.na(.), FALSE, .)))

# -------------------------------------------------------------------
# Write the combined indicator dataset to folder  This CSV can be
# joined to other parcel datasets via pin10.
write_rds(indicator_combined, "data/processed/indicator_combined.RDS")

skimr::skim(indicator_combined)

# any parcel that didn't exist in 2025 didn't have a polygon when using the newest parcel download from cook county
# fill in th missing values from the previous parcels with firms file that used 2018 and 2022 parcel shapefiles
# that parcels with FIRM panel had its own issues was missing newly created parcels
# and was created from piecing together a preliminary NFHL and effective NFHL
# where as this version is using the 2026 effective NFHL that now incorporates the preliminar map
# because yes, this dissertation took so long that the delayed FIRMs in northern cook county finally became effective.
library(targets)
tar_load(sfha_compare_pin10_fourway)


# -------------------------------------------------------------------
# Create a summary of how many unique pins are flagged in each
# indicator.  The wide summary is a single row with counts for each
# risk layer.  The long format (pivoted) is more convenient for
# comparison and plotting.
# summary_counts <- indicator_combined %>%
#   summarise(across(-pin10, ~ sum(.), .names = "n_{col}"))
#
# summary_long <- summary_counts %>%
#   pivot_longer(everything(), names_to = "indicator", values_to = "n_pins")
#
# write.csv(summary_long, "indicator_summary.csv", row.names = FALSE)
#
# message("Indicator processing complete.  Output files written:\n",
#   "  - indicator_combined.csv\n",
#   "  - indicator_summary.csv")
#


# made in ArcGIS using 2025 parcels. Is missing any parcel that didn't still exist in 2025.
parcels_with_firms <- readxl::read_xlsx("data/processed/parcel_centroids_FIRM_PAN.xlsx") |>
  select(pin10 = PIN10, FIRM_PAN) |> distinct()

# add dates to join to FIRM_PAN. Uses file  where I manually added preliminary dates since the NFHL object doesn't have those
firm_dates_file <- read_xlsx("inputs/Cook_2026_download/S_FIRM_PAN.xlsx") |>
  select(FIRM_PAN, VERSION_ID, PRE_DATE, EFF_DATE) |>
  distinct()

# make sure FIRM panels don't have more than one Preliminary or Effective Date in the file
stopifnot(firm_dates_file |> group_by(FIRM_PAN) |> summarize(n = n()) |> filter(n > 1) |> count(FIRM_PAN) > 0)

# keep 1 observation per 10 digit parcel
parcels_with_firm_dates <- left_join(parcels_with_firms, firm_dates_file, by = "FIRM_PAN") |>
  group_by(pin10) |>
  arrange(desc(PRE_DATE)) |>
  summarize(FIRM_PAN = first(FIRM_PAN),
    PRE_DATE = first(as.Date(PRE_DATE)),
    EFF_DATE = first(as.Date(EFF_DATE)),
    VERSION_ID = first(VERSION_ID)) |>
  ungroup()

# check if parcels mapped to more than one FIRM panel
stopifnot(parcels_with_firm_dates |> group_by(pin10) |> summarize(n = n()) |> filter(n > 1) |> count(pin10) > 0)

# indicator file created from ArcGIS selection of intersecting features.
# Made in Feb 2026 for checking R targets pipeline
indicator_combined <- read_rds("data/processed/indicator_combined.RDS")

# tried full join, March 5 2025
nfhl_indicators <- parcels_with_firm_dates |>
  left_join(indicator_combined, by = "pin10")

# functions from the targets pipeline
source("R/sales_data_functions.R")
source("R/sfha_targets_functions.R")

sales_file <- "data/raw/Assessor_-_Parcel_Sales_20260308.csv"

# sales_file <- "data/raw/Assessor_-_Parcel_Sales_20251229.csv"

sales <- read_sales_assessor(sales_file, min_year = 2008)

# identifying what parcels I actually need to care about for matching to polygons gut check
parcels_sold <- sales |> mutate(township = str_sub(pin10, 1, 2)) |>
  group_by(township, pin10) |> summarize(n = n(),
    max_class = max(class_1dig),
    min_class = min(class_1dig),
    max_yearsold = max(year),
    min_yearsold = min(year))
# ignoring ones in the city because they just aren't key to my research question and are giant condo buldings that aren't treated
# parcels_sold |> filter(!township %in%  c(17, 14)) |> arrange(desc(n)) |> filter(n > 150) |>
#  writexl::write_xlsx("parcels_with_tons_ofsales2.xlsx")


# part of build_res_sales() function in _targets workflow
res_sales <- sales |>
  dplyr::mutate(
    class_num = as.numeric(.data$class),
    res_c2 = class_num > 200 & class_num < 300,
    condo = dplyr::if_else(.data$class %in% c(298, 299), "Condo", "Not Condo")
  ) |>
  filter(res_c2 == TRUE) |>
  dplyr::filter(!(.data$class %in% c(213, 218, 219)))  #|>  dplyr::filter(.data$num_parcels_sale < 6) # commented out Feb 25 2026. Not sure if needed anymore

# for TWFE, need 2 or more sales per PIN
repeat_res_sales <- res_sales |>
  filter(sale_price > 25000) |>
  group_by(pin) |>
  mutate(times_sold = n()) |>
  filter(times_sold > 1) |> ungroup()

# just to know what is being excluded
single_res_sales <- res_sales |>
  filter(sale_price > 25000) |>
  group_by(pin) |>
  mutate(times_sold = n()) |>
  filter(times_sold == 1) |> ungroup()

# 900+ obs
single_res_sales |> filter(pin10 %in% indicator_combined$pin10[indicator_combined$parcel_sfha_2026 == TRUE] & pin10 %in% indicator_combined$pin10[indicator_combined$parcel_sfha_2018 == FALSE])

# 1000+ obs
single_res_sales |> filter(pin10 %in% indicator_combined$pin10[indicator_combined$parcel_sfha_2018 == TRUE] & pin10 %in% indicator_combined$pin10[indicator_combined$parcel_sfha_2026 == FALSE])

# single_res_sales |> filter(pin10 %in% indicator_combined$pin10[indicator_combined$bldg_sfha_2026 == TRUE])
# single_res_sales |> filter(pin10 %in% indicator_combined$pin10[indicator_combined$bldg_sfha_2018 == TRUE])


# add the flood risk indicators to the sales data
# tries to do initial pass of filling in missing FIRM panel.
# This is a manually created list of parcels that needed FIRMS identified much earlier in the data cleaning process
# fuction made in ./R/sfha_targets_functions.R
sales_joined <- repeat_res_sales |>
  mutate(pin10 = str_sub(pin, 1, 10)) |>
  left_join(nfhl_indicators, by = "pin10", relationship = "many-to-one") |> # has sfha and 1 in 500 polygon flags (TRUE/FALSE), lomr_date and LOMR_ID
  fill_missing_firm_pan() # list of 10 digit parcels and the FIRM they should be in.

sales_joined |> filter(is.na(pin10)) # none missing, good.

# parcels missing FIRM data. That is a lot.
sales_joined |> # filter(!pin10 %in% drop_parcels & is.na(FIRM_PAN)) |>
  group_by(pin10) |> summarize(n = n()) |> arrange(desc(n))


fallback_cols <- c(
  "FIRM_PAN", "PRE_DATE", "EFF_DATE", "VERSION_ID", "lomr_date",
  # "parcel_sfha_2018", "parcel_sfha_2024", "parcel_sfha_2026",
  # "bldg_sfha_2018", "bldg_sfha_2024", "bldg_sfha_2026",
  # "parcel_1in500_2018", "parcel_1in500_2024", "parcel_1in500_2026",
  # "bldg_1in500_2018", "bldg_1in500_2024", "bldg_1in500_2026",
  "bldg_sfha2018", "bldg_sfha2026", "bldg_lomr_date",
  "land_sfha2018", "land_sfha_2026",
  "risk500_land_2026", "risk500_land_2018",
  "risk500_bldg_2026", "risk500_bldg_2018",
  # "sfha2024", "sfha2018", "lomr2024", "LOMR_IDlomr2024", "LOMR_DATE",
  "land_sfha2026", "land_sfha2018"
)
# fill in missing values from old file that had its own flaws ------------------
# was made with archived parcel polygons from 2022 and 2018 and used R to make it
# made in _targets before 2025 parcel polygons were downloaded
# and before the 2026 effective NFHL was released
# used to fill in missing values from the 2025 parcel join done in ArcGIS
old_fallback <- sfha_compare_pin10_fourway |>
  # old_parcelswithfirms |>
  select(any_of(c("pin10", fallback_cols))) |>
  mutate(
    PRE_DATE = as.Date(PRE_DATE),
    EFF_DATE = as.Date(EFF_DATE),
    lomr_date = as.Date(lomr_date)
  ) |>
  rename(
    # Note: the variables from the sfha_compare object used preliminary FIRM
    # panel shapefile combined with effective FIRM shape panels to make
    # complicated 2026 indicator variable. No longer need to do it the hard way
    parcel_sfha_2018 = land_sfha2018,
    parcel_sfha_2026 = land_sfha2026,
    bldg_sfha_2018 = bldg_sfha2018,
    bldg_sfha_2026 = bldg_sfha2026,
    bldg_1in500_2018 = risk500_bldg_2018,
    bldg_1in500_2026 = risk500_bldg_2026,
    parcel_1in500_2018 = risk500_land_2018,
    parcel_1in500_2026 = risk500_land_2026,
  )

sales_joined2 <- sales_joined |>
  left_join(old_fallback, by = "pin10", suffix = c("", "_old"))

# fill in missing values for pins that were missing information
# default is new data, backup is old sfha_comparison object made in targets
# only fills in SFHA risk indicators for parcels that were flagged for being in SFHA or 1 in 500 risk areas or LOMRs
# still needs to fill in parcels missing FIRM panels that were NOT in risk areas or lomrs
# but that is easier
sales_joined2 <- sales_joined2 |>
  mutate(
    lomr_date = as.Date(lomr_date),
    FIRM_PAN = coalesce(FIRM_PAN, FIRM_PAN_old),
    PRE_DATE = coalesce(PRE_DATE, PRE_DATE_old),
    EFF_DATE = coalesce(EFF_DATE, EFF_DATE_old),
    VERSION_ID = coalesce(VERSION_ID, VERSION_ID_old),
    lomr_date = coalesce(lomr_date, lomr_date_old),
    parcel_sfha_2026 = coalesce(parcel_sfha_2026, parcel_sfha_2026_old),
    parcel_sfha_2018 = coalesce(parcel_sfha_2018, parcel_sfha_2018_old),
    bldg_sfha_2026 = coalesce(bldg_sfha_2026, bldg_sfha_2026_old),
    bldg_sfha_2018 = coalesce(bldg_sfha_2018, bldg_sfha_2018_old),
    parcel_1in500_2026 = coalesce(parcel_1in500_2026, parcel_1in500_2026_old),
    parcel_1in500_2018 = coalesce(parcel_1in500_2018, parcel_1in500_2018_old),
    bldg_1in500_2026 = coalesce(bldg_1in500_2026, bldg_1in500_2026_old),
    bldg_1in500_2018 = coalesce(bldg_1in500_2018, bldg_1in500_2018_old), ) |>
  # drop the variables used to fill in the missing values so you don't go crazy(er)
  select(-ends_with("_old"))

filled_report <- sales_joined |>
  transmute(
    pin10,
    was_missing = is.na(FIRM_PAN)
  ) |>
  left_join(
    sales_joined2 |>
      transmute(pin10, now_missing = is.na(FIRM_PAN)),
    by = "pin10"
  ) |>
  summarize(
    missing_before = sum(was_missing, na.rm = TRUE),
    missing_after  = sum(now_missing, na.rm = TRUE),
    filled         = missing_before - missing_after
  )

filled_report



sales_joined <- sales_joined2
# sales_joined |> filter(is.na(PRE_DATE)) |> filter(!pin10 %in% drop_parcels)
# sales_joined |> group_by(pin10) |> summarize(n = n()) |> arrange(desc(n))

missing_data <- sales_joined |>
  filter(is.na(FIRM_PAN)) |>
  distinct(pin,  pin10, sale_date, sale_price)


dropped_data <- sales_joined |> filter(!pin10 %in% drop_parcels)
# sales_joined |> filter(is.na(FIRM_PAN)) |> filter(!pin10 %in% drop_parcels)
#
# missing_data_parcels <- missing_data |> group_by(pin10) |> summarize(n = n()) |> arrange(desc(n))
#
# missing_data |> filter(pin10 %in% sfha_compare_pin10_fourway$pin10[sfha_compare_pin10_fourway$land_sfha2018 == 1])
#
# missing_and_soggy <- missing_data |> filter(pin10 %in% sfha_compare_pin10_fourway$pin10[sfha_compare_pin10_fourway$land_sfha2018 == 1] |
#   pin10 %in% sfha_compare_pin10_fourway$pin10[sfha_compare_pin10_fourway$land_sfha2024 == 1] |
#   pin10 %in% indicator_combined$pin10[indicator_combined$parcel_1in500_2026 == TRUE])

# missing_and_soggy |> group_by(pin10) |> summarize(n = n()) |> arrange(desc(n))
# missing_and_soggy |> filter(pin10 %in% sfha_fillers$pin10 &
#   !pin10 %in% drop_parcels) |>
#   group_by(pin10) |> summarize(n = n())

# writexl::write_xlsx(missing_data, "data/processed/missing_firm_data_Feb23.xlsx")

sales_joined2 |> filter(is.na(PRE_DATE)) |>
  filter(!pin10 %in% drop_parcels)
sales_joined2 |> filter(is.na(parcel_sfha_2026)) |>
  filter(!pin10 %in% drop_parcels) |>
  group_by(pin10) |> summarize(n = n())
sales_joined2 |> filter(is.na(parcel_sfha_2018)) |>
  filter(!pin10 %in% drop_parcels) |>
  group_by(pin10) |> summarize(n = n())


sales_joined <- sales_joined |>
  mutate(
    across(parcel_sfha_2018:bldg_1in500_2026,
      ~ replace_na(.x, FALSE)
    )
  )

# parcels that don't exist anymore
sales_joined |> filter(is.na(FIRM_PAN)) |>
  group_by(pin10) |> distinct(pin10)

# try to fill in any missing muni names
fill_missing_muni_by_nbhd_zip <- function(df_prep) {
  # Uses first match. Some zips/nbhds span multiple munis.
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
    )))  |>
    mutate(clean_name = ifelse(is.na(clean_name), "Unincorporated", clean_name)) |>
    mutate(pin10 = str_sub(pin, 1, 10))
  out
}

skimr::skim(df_prep_final)


pin_muni_key_file <-  "data/raw/pin_muni_key.csv"
muni_nicknames_file <- "../Merriman RA/ptax/Necessary_Files/muni_shortnames.xlsx"
floodfactor_file <- "data/processed/floodfactor_scores.csv"
manual_ff_scores <- readxl::read_xlsx("data/processed/pins_with_some_addresses_forgoogle.xlsx") |>
  mutate(pin10 = str_pad(pin10, 10, side = "left", pad = "0"))


pin_muni_key_tbl <- read_pin_muni_key(pin_muni_key_file, muni_nicknames_file, floodfactor_file)



sales_joined <- sales_joined |>
  select(-c(is_mydec_date, sale_filter_less_than_10k, row_id,
    sale_filter_deed_type, sale_filter_same_sale_within_365,
    # township_code, neighborhood_code,
    is_multisale, sale_type,
    sale_buyer_name, sale_seller_name,
    mydec_deed_type
  ))



specs <- list(
  parcel_sfha  = c(pre = "parcel_sfha_2018",  eff = "parcel_sfha_2026"),
  bldg_sfha    = c(pre = "bldg_sfha_2018",    eff = "bldg_sfha_2026"),
  parcel_1in500 = c(pre = "parcel_1in500_2018", eff = "parcel_1in500_2026"),
  bldg_1in500  = c(pre = "bldg_1in500_2018",  eff = "bldg_1in500_2026")
)




build_variant_dataset <- function(df_base, key, specs) {
  pre_col <- specs[[key]][["pre"]]
  eff_col <- specs[[key]][["eff"]]

  if (!all(c(pre_col, eff_col) %in% names(df_base))) {
    stop("Missing columns for key ", key, ": ",
      paste(setdiff(c(pre_col, eff_col), names(df_base)), collapse = ", "))
  }

  df <- df_base |>
    mutate(
      sale_date = as.Date(sale_date),
      sale_year = year(sale_date),
      log_price = log(sale_price),

      PRE_DATE = as.Date(PRE_DATE),
      EFF_DATE = as.Date(EFF_DATE),
      pre_date = PRE_DATE,
      eff_date = EFF_DATE,
      pre_date_chr = as.factor(pre_date),
      eff_date_chr = as.factor(eff_date),

      # canonical “input” columns for this variant
      sfha2018_col = replace_na(.data[[pre_col]], FALSE),
      sfha2026_col = replace_na(.data[[eff_col]], FALSE)
    ) |>
    filter(!is.na(pre_date), !is.na(eff_date)) |>
    mutate(
      in_eff_sfha = case_when(
        sale_date >= eff_date ~ sfha2026_col,
        sale_date <  eff_date ~ sfha2018_col
      ),
      in_prelim_sfha = case_when(
        sale_date >= pre_date ~ sfha2026_col,
        sale_date <  pre_date ~ sfha2018_col
      ),

      in_lomr = if ("lomr_date" %in% names(df_base))
        ifelse(!is.na(lomr_date) & sale_date >= lomr_date, TRUE, FALSE)
      else FALSE
    ) |>
    group_by(pin) |>
    arrange(sale_date, .by_group = TRUE) |>
    mutate(
      lag_eff = lag(in_eff_sfha),
      lag_pre = lag(in_prelim_sfha),

      added_eff_thisyear   = !is.na(lag_eff) & lag_eff == FALSE & in_eff_sfha == TRUE,
      removed_eff_thisyear = !is.na(lag_eff) & lag_eff == TRUE  & in_eff_sfha == FALSE,

      added_pre_thisyear   = !is.na(lag_pre) & lag_pre == FALSE & in_prelim_sfha == TRUE,
      removed_pre_thisyear = !is.na(lag_pre) & lag_pre == TRUE  & in_prelim_sfha == FALSE,

      addedto_prelim_sfha     = cumany(added_pre_thisyear),
      removedfrom_prelim_sfha = cumany(removed_pre_thisyear),
      addedto_eff_sfha        = cumany(added_eff_thisyear),
      removedfrom_eff_sfha    = cumany(removed_eff_thisyear)
    ) |>
    ungroup() |>
    group_by(pin) |>
    mutate(
      # treat years (canonical)
      treat_year_add_eff = ifelse(any(addedto_eff_sfha) & eff_date_chr != "2008-08-19", year(eff_date), 10000),
      treat_year_remove_eff = ifelse(any(removedfrom_eff_sfha) & eff_date_chr != "2008-08-19", year(eff_date), 10000),

      treat_year_add_prelim = ifelse(any(addedto_prelim_sfha), year(pre_date), 10000),
      treat_year_remove_prelim = ifelse(any(removedfrom_prelim_sfha), year(pre_date), 10000)
    ) |>
    ungroup() |>
    mutate(
      # rel years
      rel_year_add_eff    = ifelse(treat_year_add_eff != 10000, sale_year - treat_year_add_eff, -1000),
      rel_year_remove_eff = ifelse(treat_year_remove_eff != 10000, sale_year - treat_year_remove_eff, -1000),
      rel_year_add_prelim    = ifelse(treat_year_add_prelim != 10000, sale_year - treat_year_add_prelim, -1000),
      rel_year_remove_prelim = ifelse(treat_year_remove_prelim != 10000, sale_year - treat_year_remove_prelim, -1000),

      # same-year adjustment
      rel_year_add_prelim    = ifelse(rel_year_add_prelim == 0 & sale_date < pre_date, -1, rel_year_add_prelim),
      rel_year_remove_prelim = ifelse(rel_year_remove_prelim == 0 & sale_date < pre_date, -1, rel_year_remove_prelim),
      rel_year_add_eff       = ifelse(rel_year_add_eff == 0 & sale_date < eff_date, -1, rel_year_add_eff),
      rel_year_remove_eff    = ifelse(rel_year_remove_eff == 0 & sale_date < eff_date, -1, rel_year_remove_eff),

      # group names
      group_name_eff = case_when(
        eff_date == as.Date("2008-08-19") ~ 0,
        eff_date == as.Date("2019-11-01") ~ 2019,
        eff_date == as.Date("2021-09-10") ~ 2021,
        TRUE ~ 0
      ),
      group_name_prelim = factor(case_when(
        pre_date == as.Date("2005-01-01") ~ 0,
        pre_date == as.Date("2015-02-12") ~ 2015,
        pre_date == as.Date("2019-07-01") ~ 2019,
        pre_date == as.Date("2021-09-22") ~ 2021,
        TRUE ~ 0
      )),

      treated_group_eff = case_when(
        eff_date == as.Date("2008-08-19") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 0,
        eff_date == as.Date("2019-11-01") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2019,
        eff_date == as.Date("2021-09-10") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2021,
        TRUE ~ 0
      ),
      treated_group_prelim = case_when(
        pre_date == as.Date("2005-01-01") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 0,
        pre_date == as.Date("2015-02-12") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2015,
        pre_date == as.Date("2019-07-01") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2019,
        pre_date == as.Date("2021-09-22") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2021,
        TRUE ~ 0
      )
    ) |>
    group_by(pin) |>
    mutate(
      times_sold = n(),
      change_type = case_when(
        sum(in_eff_sfha) == times_sold ~ "Always SFHA",
        sum(in_eff_sfha) == 0 ~ "Never SFHA",
        TRUE ~ "Changes SFHA"
      ),
      change_type_prelim = case_when(
        sum(in_prelim_sfha) == times_sold ~ "Always SFHA",
        sum(in_prelim_sfha) == 0 ~ "Never SFHA",
        TRUE ~ "Changes SFHA"
      ),
      n_sales = n(),
      ever_added_prelim   = any(addedto_prelim_sfha),
      ever_removed_prelim = any(removedfrom_prelim_sfha)
    ) |>
    ungroup() |>
    mutate(
      pre_post_firm = ifelse(sale_date < pre_date, "Not Yet Updated", "Post FIRM Update"),
      event_remapped = ifelse(
        sale_date < pre_date & treated_group_prelim != 0, "Pre",
        ifelse(sale_date > pre_date & treated_group_prelim != 0, "Post", "NotRemapped")
      ),
      prelim_sfha_category = case_when(
        ever_added_prelim & !ever_removed_prelim ~ "Added to prelim SFHA",
        ever_removed_prelim & !ever_added_prelim ~ "Removed from prelim SFHA",
        change_type_prelim == "Always SFHA" ~ "Always SFHA",
        change_type_prelim == "Never SFHA" ~ "Never SFHA",
        TRUE ~ "CHECK ME"
      )
    ) |>
    mutate(variant_key = key)

  df <- df |>
    mutate( # insure requirement definition you used: in_eff_sfha == 1 and NOT in a LOMR
      ins_req = if_else(in_eff_sfha == 1 & in_lomr == FALSE, TRUE, FALSE)) |>
    left_join(pin_muni_key_tbl, by = "pin") |>    # could clean up to parcel level and then join in but... maybe if i have extra time
    left_join((manual_ff_scores |>
      select(pin10, flood_factor_score, clean_name, zip_code, nbhd_code)), by = "pin10") |>
    mutate(
      flood_factor_score = ifelse(is.na(env_flood_fs_factor), flood_factor_score, env_flood_fs_factor),
      clean_name = ifelse(is.na(clean_name.x), clean_name.y, clean_name.x),
      high_ff_score = ifelse(env_flood_fs_factor > 4 | flood_factor_score > 4, TRUE, FALSE),
      zip_code = ifelse(is.na(zip_code.x), zip_code.y, zip_code.x),
      nbhd_code = ifelse(is.na(nbhd_code.x), nbhd_code.y, nbhd_code.x),
    ) |>
    select(-c(contains(".x"))) |>
    select(-c(contains(".y"))) |>
    mutate(
      Triad = case_when(
        clean_name == "Chicago" ~ "City",
        as.numeric(str_sub(pin, 1, 2)) < 13 ~ "North",
        as.numeric(str_sub(pin, 1, 2)) >= 13 ~ "South"
      ),
      Triad = factor(Triad, levels = c("South", "North", "City"))
    )

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



  df <- df |> fill_missing_muni_by_nbhd_zip()

  df

}


dir.create("data/processed/variants", recursive = TRUE, showWarnings = FALSE)

keys <- names(specs)

for (k in keys) {
  df_k <- build_variant_dataset(df_base = sales_joined, key = k, specs = specs)
  saveRDS(df_k, file.path("data/processed/variants", paste0("df_prep_", k, "2026sales_notfiltered.rds")))
}
