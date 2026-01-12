# data_prep.R -------------------------------------------------------------
# INPUTS:
#  - ./data/raw/Assessor_Parcel_Sales_20250105.csv
#  - ./data/processed/parcels_withFIRMS.csv (already has sfha indicators merged in)

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
sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20250709.csv") |> # 1,838,476
  filter(year > 2005) |>
  mutate(
    class_1dig = str_sub(class, 1, 1),
    pin10       = str_sub(pin, 1, 10),
    sale_date   = mdy(sale_date)
  )

pin10_firms  <- read_csv("./data/processed/parcels_withFIRMS_20260108.csv")  # |>

pin10_firms |> distinct(pin10) |> count()

table(pin10_firms$EFF_DATE)
table(pin10_firms$FIRM_PAN)



# Merge firms & SFHA indicators into sales
sales <- sales |>
  left_join(pin10_firms, by = "pin10", relationship = "many-to-one") |>
  mutate(
    lomr2018 = ifelse(is.na(lomr2018), 0L, lomr2018),
    sfha2018 = ifelse(is.na(sfha2018), 0L, sfha2018),
    lomr2024 = ifelse(is.na(lomr2024), 0L, lomr2024),
    sfha2024 = ifelse(is.na(sfha2024), 0L, sfha2024),
    sfha2026 = ifelse(is.na(sfha2026), 0L, sfha2026),
  )
#
# table(sales$sfha2024)
# table(sales$sfha2026)
# table(sales$sfha2018)
table(sales$PRE_DATE)

# missingdates <- sales |> filter(is.na(EFF_DATE))
#
# missingdates |>
#   mutate(
#     EFF_DATE = case_when(
#       is.na(EFF_DATE) & neighborhood_code %in% c("10024", "18030", "19020", "19060", "24032", "25160",  "30011",
#         "38040", "38110", "71074", "74022",  "74030",   "77120", "77131") ~ as_date("2008-08-19"),
#       is.na(EFF_DATE) & neighborhood_code %in% c("13032", "28039", "28100", "15907", "23092", "39081", "39200", "39211") ~ as_date("2019-11-01"),
#       is.na(EFF_DATE) & neighborhood_code %in% c("70010",  "73032", "73041",  "73084",
#         "73093",  "74013",  "76010", "76011") ~ as_date("2021-09-10"),
#       TRUE ~ as_date(EFF_DATE)),
#     PRE_DATE = case_when(
#       is.na(PRE_DATE) & neighborhood_code %in% c("19020", "19060", "24032", "25160",  "30011",
#         "38040", "38110", "71074", "74022",  "74030", "77120", "77131") ~ as_date("2005-01-01"),
#       is.na(PRE_DATE) &  neighborhood_code %in% c("13032", "15907", "28039", "28100", "39200", "23092", "39081", "39211") ~ as_date("2015-02-15"),
#       is.na(PRE_DATE) & neighborhood_code %in% c("70010",  "73032", "73041",  "73084",
#         "73093",  "74013",  "76010", "76011") ~ as_date("2019-06-28"),
#       is.na(PRE_DATE) & neighborhood_code %in% c("10024", "18030") ~ as_date("2021-09-22"),
#
#       TRUE ~ as_date(PRE_DATE))
#   ) |> group_by(pin) |> mutate(n = n()) |>
#   filter(is.na(PRE_DATE) & n > 1 & class_1dig == 2)

pin10_firms |> filter(is.na(EFF_DATE)) |> distinct(FIRM_PAN)
pin10_firms |> filter(is.na(PRE_DATE)) |> distinct(FIRM_PAN)

sales <- sales |> mutate(EFF_DATE = ifelse(EFF_DATE == as.Date("2026-01-23"), as.Date("2008-08-19"), as.Date(as.character(EFF_DATE))))


## fix this in the FIRM join in earlier steps later ##
# sales <- sales |>



sales <- sales |>
  # 23092 and 23171 are in new trier
  mutate(
    PRE_DATE = as.Date(PRE_DATE),
    EFF_DATE = as.Date(EFF_DATE),

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

      TRUE ~ as_date(PRE_DATE))
  )


table(sales$EFF_DATE)
table(sales$PRE_DATE)


sales <- sales |>
  filter(year > 2010) |>
  mutate(
    lomr_date = as.Date(lomr_date, format = "%m/%d/%Y"),

    # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
    in_eff_sfha = case_when(
      sale_date >= EFF_DATE ~ sfha2026,
      sale_date < EFF_DATE ~ sfha2018),


    # create similar variable but for the preliminary date: model must deal with anticipation to change
    in_prelim_sfha = case_when(
      sale_date >= PRE_DATE  ~ sfha2026,
      sale_date < PRE_DATE ~ sfha2024
    ),

    # LOMR indicator
    in_lomr       = ifelse(!is.na(lomr_date) & sale_date >= lomr_date, TRUE, FALSE),
  )

sales1 <- sales |>
  group_by(pin) |>
  arrange(sale_date) |>
  mutate(
    lag_eff  = dplyr::lag(in_eff_sfha),
    lag_pre  = dplyr::lag(in_prelim_sfha)
  )


table(sales1$in_eff_sfha)

table(sales1$in_prelim_sfha)
table(sales1$lag_pre)

sales1 <- sales1 |>
  group_by(pin) |>
  arrange(sale_date) |>
  mutate(
    timessold = n(),
    lag_eff = ifelse(is.na(lag_eff) & timessold > 1, first(in_eff_sfha), lag_eff),
    lag_pre = ifelse(is.na(lag_pre) & timessold > 1, first(in_prelim_sfha), lag_pre)) |>
  select(pin, year, sale_date, sale_price, lag_eff, lag_pre, in_eff_sfha, in_prelim_sfha, in_lomr, everything())

# sold_twice <- sales1 |> group_by(pin) |> mutate(n = n()) |>
#   filter(n > 1) |>
#   select(pin, year, sale_date, sale_price, lag_eff, lag_pre, in_eff_sfha,
#     in_prelim_sfha, in_lomr, everything()) |>
#   select(-c(is_mydec_date, AREA, `SFHA Change`, latitude, longitude, SOURCE_CIT,
#     Shape_Leng, Shape_Area, SHAPE_Leng, SHAPE_Area, row_id, sale_buyer_name, sale_document_num))

sales1 <- sales1 |>
  ungroup() |>
  mutate(
    addedto_eff_sfha        = (lag_eff == F) & (in_eff_sfha == TRUE) & timessold > 1,    # flags year that event happened
    addedto_prelim_sfha     = (lag_eff == F) & (in_prelim_sfha == TRUE) & timessold > 1,   # flags year that event happened
    removedfrom_eff_sfha    = (lag_eff == T) & (in_eff_sfha == FALSE) & timessold > 1,   # flags year that event happened
    removedfrom_prelim_sfha = (lag_eff == T) & (in_prelim_sfha == FALSE) & timessold > 1,  # flags year that event happened
  )

table(sales1$addedto_eff_sfha)
table(sales1$addedto_prelim_sfha)
table(sales1$removedfrom_eff_sfha)
table(sales1$removedfrom_prelim_sfha)


sales2 <- sales1 |>
  group_by(pin) |>
  mutate(
    addedto_prelim_sfha = (cumany(addedto_prelim_sfha)),
    removedfrom_prelim_sfha = (cumany(removedfrom_prelim_sfha)),
    addedto_eff_sfha = (cumany(addedto_eff_sfha)),
    removedfrom_eff_sfha = (cumany(removedfrom_eff_sfha)),
  ) |> ungroup()

table(sales2$addedto_eff_sfha)
table(sales2$addedto_prelim_sfha)
table(sales2$removedfrom_eff_sfha)
table(sales2$removedfrom_prelim_sfha)

sales2 <- sales2 |>
  ungroup() |>
  mutate(
    # properties that potentially have flood insurance requirement
    ins_req = if_else(in_eff_sfha == TRUE & in_lomr == FALSE, TRUE, FALSE)
  )


# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales2 |>
  filter(class %nin% c(213, 218, 219)) |>
  filter(num_parcels_sale < 6) |>        # drop sales that involved a lot of parcels. Usually involves a CoOp, Condo, or landarea being bought for construction, not a normal residential sale.
  filter(sale_price > 5000) |>
  mutate(
    class = as.numeric(class),
    res_c2 = class > 200 & class < 300,
    condo = if_else(class %in% c(298, 299), "Condo", "Not Condo"),  # NOTE: there are separate "single family homes" coded as condos because they are share a parcel and havea condo association.
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
  ungroup()


res_sales <- res_sales |> filter(times_sold < 15)
res_repeats <- res_sales |> filter(times_sold > 1)


# Save objects for later use in your Quarto doc
saveRDS(sales,       "./data/processed/sales_prepped_parcels_20260109.rds")
saveRDS(res_sales,   "./data/processed/res_sales_parcels_20260109.rds")
saveRDS(res_repeats,   "./data/processed/repeat_res_sales_parcels_20260109.rds")
