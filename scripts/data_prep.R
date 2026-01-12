# data_prep.R -------------------------------------------------------------
# INPUTS:
#  - ./data/raw/Assessor_Parcel_Sales_20250105.csv
#  - ./data/processed/parcels_wFIRMS.csv
#  - ./data/processed/sfha_indicator_pins.csv

# Outputs:
#  - ./data/processed/sales_prepped.RDS
#  - ./data/processed/res_sales.RDS

# Load required libraries
library(tidyverse)
library(sf)
library(DescTools)    # for Winsorize(), if you later decide to use it
library(httr)
library(jsonlite)
library(glue)
library(lubridate)


# 1. Read and preprocess sales data
# sales <- read_csv("./data/raw/Assessor_Parcel_Sales_20250105.csv") |> # 1,832,419
sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20250709.csv") |> # 1,838,476
  filter(year > 2005) |>
  mutate(
    class_1dig = str_sub(class, 1, 1),
    class       = as.numeric(class),
    pin10       = str_sub(pin, 1, 10),
    sale_date   = mdy(sale_date)
  )

# 2. Read flood‐related lookup tables
# includes the PIN and the FIRM_ID/FIRM_Panel that the PIN was in according to the effective NFHL from 2022
# pin10_firms  <- read_csv("./data/processed/parcels_wFIRMS_20250604.csv")   |>
# pin10_firms  <- read_csv("./data/processed/targets/sfha/parcels_withFIRMS.csv")   |>
pin10_firms  <- read_csv("./data/processed/parcels_wFIRMS_2026.csv")   |>

  select(-c(PRE_DATE))
# mutate(letter = str_sub(FIRM_PAN, -1, -1))

table(pin10_firms$EFF_DATE)
# table(pin10_firms$letter)

# Bring in FIRM PANELS from FEMA NFIP geodatabase (before the new update that has 2026 effective days)
# prelim_FIRMS <- readxl::read_xlsx("./inputs/Cook_2026_download/S_FIRM_PAN.xlsx") |>
#
#   # prelim_FIRMS <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx") |>
#   filter(VERSION_ID == "2.6.3.6") |>
#   mutate(old_firm_panel = ifelse(is.na(old_firm_panel), FIRM_PAN, old_firm_panel),
#   )

# trial code
# pin10_firms <- pin10_firms |>
#   mutate(PRE_DATE = case_when(
#     FIRM_PAN %in% prelim_FIRMS$old_firm_panel ~ as_date("2021-09-22"),
#     VERSION_ID == "2.4.3.5" ~ as_date("2015-02-15"),
#     VERSION_ID == "2.4.3.0" ~ as_date("2019-06-28"),
#     TRUE ~ as_date("2005-01-01")),
#
#   EFF_DATE = case_when(
#     VERSION_ID == "2.4.3.5" ~ as_date("2019-11-01"),
#     VERSION_ID == "2.4.3.0" ~ as_date("2021-09-10"),
#     TRUE ~ as_date("2008-08-19"))
#   )

# pin10_firms |> filter(is.na(EFF_DATE))
# table(pin10_firms$PRE_DATE)
# table(pin10_firms$EFF_DATE)

sfha_ind <- read_csv("./data/processed/sfha_indicator_parcels_20260107.csv")

# sfha_ind <- read_csv("./data/processed/sfha_indicator_parcels.csv")
# sfha_ind <- readr::read_csv("./data/processed/targets/sfha/sfha_indicator_final.csv",
#  show_col_types = FALSE) |> mutate(version = str_sub(firm_pan, -1, -1))

# table(sfha_ind$firm_pan)
# sfha_ind <- sfha_ind |> group_by(pin10) |>
#   summarize(sfha2018 = max(sfha2018),
#     sfha2024 = max(sfha2024),
#     prelimsfha = max(prelimsfha),
#     lomr2018 = max(lomr2018),
#     lomr2024 = max(lomr2024),
#     lomr_date = max(lomr_date)
#   )


# 3. Merge firms & SFHA indicators into sales
# sales <- sales |>
#   left_join(pin10_firms,    by = "pin10") |>
#   left_join(sfha_ind, by = "pin10") |>
# mutate(
#   prelim_covered = ifelse(VERSION_ID=="2.6.3.6", 1, 0),
#
#   class    = as.character(class),
#   lomr2018 = (ifelse(is.na(lomr2018), 0, lomr2018)),
#   sfha2018 = (ifelse(is.na(sfha2018), 0, sfha2018)),
#   lomr2024 = (ifelse(is.na(lomr2024), 0, lomr2024)),
#   sfha2024 = (ifelse(is.na(sfha2024), 0, sfha2024)),
#   prelimsfha = (ifelse(is.na(prelimsfha), 0, prelimsfha))
# )

sales <- sales |>
  left_join(pin10_firms, by = "pin10") |>
  left_join(sfha_ind,    by = "pin10") |>
  mutate(
    prelim_covered = ifelse(VERSION_ID == "2.6.3.6", 1, 0),
    class    = as.character(class),
    lomr2018 = ifelse(is.na(lomr2018), 0L, lomr2018),
    sfha2018 = ifelse(is.na(sfha2018), 0L, sfha2018),
    lomr2024 = ifelse(is.na(lomr2024), 0L, lomr2024),
    sfha2024 = ifelse(is.na(sfha2024), 0L, sfha2024),
    prelimsfha = ifelse(is.na(prelimsfha) & prelim_covered == TRUE, 0L, prelimsfha),
    prelimsfha = ifelse(is.na(prelimsfha), sfha2024, prelimsfha),
  )

table(sales$sfha2024)
table(sales$prelimsfha)
table(sales$prelim_covered)

# firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx") |>
firm_dates <- readxl::read_xlsx("./inputs/Cook_2026_Download/S_FIRM_PAN.xlsx")  |>

  mutate(
    eff_date = ymd(EFF_DATE),
    pre_date = ymd(PRE_DATE),

    EFF_DATE = ifelse(eff_date == as_date("2026-01-23"), "2008-08-19", as.character(eff_date))
  )

# mutate(EFF_DATE = ifelse(is.na(EFF_DATE), "2008-08-19", EFF_DATE)) |> # newly updated FIRMs originally had no effective date in hopes that they would become effective before dissertation was done.
# mutate(PRE_DATE = as_date(PRE_DATE),
#   EFF_DATE = as_date(EFF_DATE)) |>
# select(FIRM_PAN, old_panel, PRE_DATE, EFF_DATE)

# 4. Join LOMR table and create SFHA/LOMR flags
sales <- sales |>
  select(-c(EFF_DATE)) |>
  left_join(firm_dates, by = "FIRM_PAN") # |>

table(sales$sfha2024)
table(sales$EFF_DATE)
table(sales$PRE_DATE)

table(sales$prelimsfha)



sales <- sales |>
  mutate(
    # # CCAO SFHA status post‐2021
    # ccao_sfha = case_when(
    #   sfha2024 == "1" & year >= 2021 ~ "SFHA",
    #   TRUE                            ~ "Not SFHA"
    # ),


    #
    #     # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
    #     in_eff_sfha = ifelse(sfha2018 == 1, "SFHA", "Not SFHA"),
    #
    #     in_eff_sfha = ifelse(sfha2024 == 1 & sale_date >= EFF_DATE, "SFHA",
    #       ifelse(sale_date >= EFF_DATE & sfha2024 == 0, "Not SFHA", in_eff_sfha)),
    #
    #     in_eff_sfha = ifelse((sfha2018 == 0 & sfha2024 == 0) | (is.na(sfha2018) & is.na(sfha2024)), "Not SFHA", in_eff_sfha),
    #
    #
    #
    #
    #
    #
    #     # create similar variable but for the preliminary date: model must deal with anticipation to change
    #
    #     # in_prelim_sfha = ifelse(prelim_covered == 1L & prelimsfha == 1L & sale_date >= PRE_DATE, "SFHA", "Not SFHA"),
    #
    #     in_prelim_sfha = ifelse(sfha2018 == 1 & sale_date >= PRE_DATE, "SFHA", "Not SFHA"),
    #
    #     in_prelim_sfha = ifelse(sfha2024 == 1 & sale_date >= PRE_DATE, "SFHA",
    #       ifelse(sale_date >= (PRE_DATE) & sfha2024 == 0, "Not SFHA", in_prelim_sfha)),
    #
    #     in_prelim_sfha = ifelse((sfha2018 == 0 & sfha2024 == 0) | (is.na(sfha2018) & is.na(sfha2024)), "Not SFHA", in_prelim_sfha),
    #
    #     in_prelim_sfha = ifelse(prelimsfha == 1 & sale_date >= PRE_DATE, "SFHA", in_prelim_sfha),
    #

    # LOMR indicator
    in_lomr       = if_else(sale_date >= (lomr_date),
      "Received LOMR", "Not in LOMR"),
    in_lomr = ifelse(is.na(in_lomr), "Not in LOMR", in_lomr),

    # addedto_eff_sfha = ifelse((sfha2018 == 0 & sfha2024 == 1 &
    #                              sale_date > EFF_DATE), "MappedIn", "0"),
    # addedto_prelim_sfha = ifelse((sfha2018 == 0 & prelimsfha == 1 &
    #                                 sale_date > PRE_DATE), "MappedIn", "0"),
    #
    # removedfrom_eff_sfha = ifelse((sfha2018 == 1 & sfha2024 == 0) &
    #                                 sale_date > EFF_DATE, "MappedOut", "0"),
    #
    # removedfrom_prelim_sfha = ifelse((sfha2018 == 1 & prelimsfha == 0 &
    #                                     sale_date > PRE_DATE), "MappedOut", "0"),
    #


    # addedto_prelim_sfha = ifelse(
    #   prelim_covered == 1L & sfha2018 == 0L & prelimsfha == 1L & sale_date > PRE_DATE,
    #   "MappedIn", "0"
    # ),
    #
    # removedfrom_prelim_sfha = ifelse(
    #   prelim_covered == 1L & sfha2018 == 1L & prelimsfha == 0L & sale_date > PRE_DATE,
    #   "MappedOut", "0"
    # ),
  )

table(sales$PRE_DATE)
sales1 <- sales |>
  mutate(
    # compute flags ONLY after dates are settled
    panel_updated_eff = EFF_DATE %in% as_date(c("2019-11-01", "2021-09-10")),
    panel_updated_pre = PRE_DATE %in% as_date(c("2015-02-12", "2019-07-01", "2021-09-22")),

    post_eff_update = ifelse((EFF_DATE == as_date("2019-11-01") & sale_date > EFF_DATE) | (EFF_DATE == as_date("2021-09-10") & sale_date > EFF_DATE), TRUE, FALSE),

    post_prelim_update = ifelse((PRE_DATE == as_date("2021-09-22") & sale_date > PRE_DATE) | (PRE_DATE == as_date("2019-06-28") & sale_date > PRE_DATE) | (PRE_DATE == as_date("2015-02-15") & sale_date > PRE_DATE), TRUE, FALSE),


    in_eff_sfha = case_when(
      post_eff_update ~ sfha2024,
      !post_eff_update ~ sfha2018
    ),
    in_eff_sfha = ifelse(in_eff_sfha == 1, T, F),  # fill in rest of parcels as FALSE if they were not identified as being in an SFHA polygon

    in_prelim_sfha = case_when(
      post_prelim_update ~ coalesce(prelimsfha),
      !post_eff_update ~ sfha2018),
    in_prelim_sfha = ifelse(in_prelim_sfha == 1, T, F)
  )


sales1 <- sales1 |>
  group_by(pin) |>
  arrange(sale_date, .by_group = TRUE) |>
  mutate(
    in_eff_sfha = ifelse(is.na(in_eff_sfha), FALSE, in_eff_sfha),
    in_prelim_sfha = ifelse(is.na(in_prelim_sfha), FALSE, in_prelim_sfha),

    lag_eff  = dplyr::lag(in_eff_sfha),
    lag_pre  = dplyr::lag(in_prelim_sfha)) |>
  ungroup() |>

  mutate(
    addedto_eff_sfha        = (lag_eff != T) & (in_eff_sfha == TRUE),    # flags year that event happened
    removedfrom_eff_sfha    = (lag_eff != F) & (in_eff_sfha == FALSE),   # flags year that event happened

    addedto_prelim_sfha     = (lag_pre != T) & (in_prelim_sfha == TRUE),   # flags year that event happened
    removedfrom_prelim_sfha = (lag_pre != F) & (in_prelim_sfha == FALSE)  # flags year that event happened
  ) |>
  ungroup()


# sales1 <- sales1 |>
#   mutate(
#     addedto_eff_sfha = ifelse(is.na(addedto_eff_sfha), FALSE, addedto_eff_sfha),
#     removedfrom_eff_sfha = ifelse(is.na(removedfrom_eff_sfha), FALSE, removedfrom_eff_sfha),
#
#     addedto_prelim_sfha = ifelse(is.na(addedto_prelim_sfha), FALSE, addedto_prelim_sfha),
#     removedfrom_prelim_sfha = ifelse(is.na(removedfrom_prelim_sfha), FALSE, removedfrom_prelim_sfha)
#   )

table(sales1$addedto_prelim_sfha)

table(sales1$year, sales1$in_eff_sfha)
sales1 |> filter(is.na(in_prelim_sfha))

# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales |>
  filter(class %nin% c(213, 218, 219)) |>
  filter(num_parcels_sale < 6) |>        # drop sales that involved a lot of parcels. Usually involves a CoOp, Condo, or landarea being bought for construction, not a normal residential sale.
  filter(sale_price > 5000) |>
  mutate(
    class = as.numeric(class),
    res_c2 = class > 200 & class < 300
  ) |>
  group_by(pin) |>
  arrange(year) |>
  filter(any(res_c2)) |>
  mutate(
    times_sold      = n(),
    years_btw_sales = year - lag(year),
    sold_once       = times_sold == 1,
    sold_multi      = times_sold > 1
  ) |>
  ungroup() # |>
# mutate(
#   sold_pre2014 = ifelse(year <= 2013, TRUE, FALSE),
#   sold_post2013 = ifelse(year > 2013, TRUE, FALSE),
#
#   sold_btwn_20132023 = ifelse(year > 2013 & year < 2024, TRUE, FALSE),
#
#   sold_pre2024 = ifelse(year <= 2023, TRUE, FALSE),
#   sold_post2023 = ifelse(year > 2023, TRUE, FALSE),
#   )
res_sales <- res_sales |> filter(times_sold < 15)


# Save objects for later use in your Quarto doc
# saveRDS(sales,       "./data/processed/sales_prepped_parcels.rds")
saveRDS(res_sales,   "./data/processed/res_sales_parcels7.rds")
