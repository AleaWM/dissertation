# new script

# outputs

#     "./data/processed/sales_prepped_buildings_TEST.rds"
#     "./data/processed/res_sales_buildings_TEST.rds"

# Load required libraries
library(tidyverse)
library(DescTools)    # for Winsorize(), not currently used
library(httr)
library(jsonlite)
library(glue)
library(lubridate)

firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx")

drop_parcels <- c( # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
  "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
  "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
  "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
  "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
  "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
  "2130124001", "2130124002", "2130124003", "2130124004",  # almost completely in the lake
  "2130999001", "2132213002", # actual water canal in calumet area
  "2608202004", "2608400034", "3017211033" # also water pins.
)


# 1. Read and preprocess sales data
# sales <- read_csv("./data/raw/Assessor_Parcel_Sales_20250105.csv") |>
sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20250709.csv") |>

  # sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20251229.csv") |>
  filter(year > 2005) |>
  mutate(
    class_1dig = str_sub(class, 1, 1),
    class       = as.numeric(class),
    pin10       = str_sub(pin, 1, 10),
    sale_date   = mdy(sale_date)
  ) |>
  filter(!pin10 %in% drop_parcels)

# Bring in FIRM PANELS from FEMA NFIP geodatabase (before the new update that has 2026 effective days)
prelim_FIRMS <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx") |>
  filter(VERSION_ID == "2.6.3.6") |>
  mutate(old_firm_panel = ifelse(is.na(old_firm_panel), FIRM_PAN, old_firm_panel),
  )


pin10_firms  <- read_csv("./data/processed/parcels_wFIRMS_20250604.csv")   |>
  select(-c(PRE_DATE, EFF_DATE)) |>
  filter(!pin10 %in% drop_parcels)

table(pin10_firms$VERSION_ID)


# trial code
pin10_firms <- pin10_firms |>
  mutate(PRE_DATE = case_when(
    FIRM_PAN %in% prelim_FIRMS$old_firm_panel ~ as_date("2021-09-22"),
    VERSION_ID == "2.4.3.5" ~ as_date("2015-02-15"),
    VERSION_ID == "2.4.3.0" ~ as_date("2019-06-28"),
    TRUE ~ as_date("2005-01-01")),

  EFF_DATE = case_when(
    VERSION_ID == "2.4.3.5" ~ as_date("2019-11-01"),
    VERSION_ID == "2.4.3.0" ~ as_date("2021-09-10"),
    TRUE ~ as_date("2008-08-19"))
  )

table(pin10_firms$PRE_DATE)
table(pin10_firms$EFF_DATE)


# only includes parcels that were flagged as having a BUILDING outline in the FEMA flood plain.
sfha_ind <- read_csv("./data/processed/sfha_indicator_buildings.csv")  |>
  mutate(EFF_DATlomr2018 = ifelse(EFF_DATlomr2018 %in% c("Inf", "-Inf"), NA, EFF_DATlomr2018),
    EFF_DATlomr2024 = ifelse(EFF_DATlomr2024 %in% c("Inf", "-Inf"), NA, EFF_DATlomr2024))

sfha_ind |> distinct(pin10)  # 41289 pins in an SFHA or LOMR in at least one geodatabase. Already were distinct when read in

summary(sfha_ind)

sfha_ind <- sfha_ind |>
  mutate(sfha2018 = case_when(
    pin10 == "1709410014" ~ 0,
    pin10 == "1716401017" ~ 0,
    pin10 == "1129315024" ~ 0,
    pin10 == "2423406029" ~ 0,
    TRUE ~ sfha2018
  ),
  sfha2024 = case_when(
    pin10 == "1729309054" ~ 0,
    TRUE ~ sfha2024
  ))

stopifnot(is.numeric(sfha_ind$sfha2018), is.numeric(sfha_ind$sfha2024))


sales <- sales |>
  left_join(pin10_firms,    by = "pin10") |>
  filter(!pin10 %in% drop_parcels) |>
  mutate(FIRM_PAN =
    case_when(
      pin10 == "0427302008" ~ "17031C0229J",
      pin10 == "0427302009" ~ "17031C0229J",
      pin10 == "0124100056" & is.na(FIRM_PAN) ~ "17031C0157J",
      #    pin10 == "0124100056" & is.na(FIRM_PAN) ~ "17031C0157K",
      pin10 == "0316112026" ~ "17031C0202J",
      pin10 == "0323109028" ~ "17031C0206J",
      pin10 == "0528412022" ~ "17031C0253J",
      pin10 == "1020101027" ~ "17031C0241J",
      pin10 == "1315403068" ~ "17031C0403J",
      pin10 == "1408315062" ~ "17031C0406K",
      pin10 == "1418330040" ~ "17031C0408K",
      pin10 == "1420327056" ~ "17031C0408K",
      pin10 == "1429301111" ~ "17031C0416J",
      pin10 == "1704217142" ~ "17031C0417K",
      pin10 == "1705214021" ~ "17031C0417K",
      pin10 == "1707122053" ~ "17031C0418J",
      pin10 == "1710122029" ~ "17031C0438K",
      pin10 == "1710207034" ~ "17031C0438K",
      pin10 == "1710207035" ~ "17031C0438K",
      pin10 == "1710207036" ~ "17031C0438K",
      pin10 == "1710207037" ~ "17031C0438K",
      pin10 == "1710207038" ~ "17031C0438K",
      pin10 == "1710207039" ~ "17031C0438K",
      pin10 == "1715101025" ~ "17031C0419J",
      pin10 == "1716238018" ~ "17031C0419J",
      pin10 == "1716401035" ~ "17031C0507J",
      pin10 == "1717117042" ~ "17031C0418J",
      pin10 == "1722315064" ~ "17031C0526K",
      pin10 == "2010313019" ~ "17031C0536K", # Tons of vacant land where houses used to be in this area. wow.
      pin10 == "2226204006" ~ "17031C0591J",
      pin10 == "2226304002" ~ "17031C0591J",
      pin10 == "2226304003" ~ "17031C0587J",
      pin10 == "2226304004" ~ "17031C0587J",
      pin10 == "2226304005" ~ "17031C0587J",
      pin10 == "2226304007" ~ "17031C0587J",
      pin10 == "2226304008" ~ "17031C0587J",
      pin10 == "2226304026" ~ "17031C0587J",
      pin10 == "2226304018" ~ "17031C0587J",
      pin10 == "2226307001" ~ "17031C0587J",
      pin10 == "2226307003" ~ "17031C0587J",
      pin10 == "2226307004" ~ "17031C0587J",
      pin10 == "2226307005" ~ "17031C0587J",
      pin10 == "2226308011" ~ "17031C0587J",
      pin10 == "2314411016" ~ "17031C0604J",
      pin10 == "2314411021" ~ "17031C0604J",
      pin10 == "2314411023" ~ "17031C0604J",
      pin10 == "2314411026" ~ "17031C0604J",
      pin10 == "2314411027" ~ "17031C0604J",
      pin10 == "2421429026" ~ "17031C0636K",
      pin10 == "2709217055" ~ "17031C0613K",
      pin10 == "2730212015" ~ "17031C0684J", # in lomr, house gone? weird vacant land
      pin10 == "0427302010" ~ "17031C0229J",
      TRUE ~ FIRM_PAN
    )
  ) |>

  left_join(sfha_ind, by = "pin10")

table(sales$prelimsfha)
table(sales$sfha2018)
table(sales$sfha2024)

# # sanity: each sale must EFF_DATE# sanity: each sale must know its panel’s EFF_DATE (baseline or updated)
# if (any(is.na(sales$EFF_DATE))) {
#   n <- sum(is.na(sales$EFF_DATE))
#   stop("Missing EFF_DATE for ", n, " sale(s). Fill baseline 2008-08-19 where appropriate.")
# }

## to fill in missing FIRM panel Dates for neighborhoods
missing_dates <- sales |>
  group_by(neighborhood_code, EFF_DATE, PRE_DATE) |>
  summarize(n(),
    EFF_DATE = max(EFF_DATE),
    PRE_DATE = max(PRE_DATE),
    sfha2018 = median(sfha2018, na.rm = TRUE),
    sfha2024 = median(sfha2024, na.rm = TRUE),
    prelimsfha = median(prelimsfha, na.rm = TRUE))

missing_dates |> filter(is.na(PRE_DATE)) |> View()

sales <- sales |>
  mutate(
    EFF_DATE = case_when(
      is.na(EFF_DATE) & neighborhood_code %in% c("10024", "18030", "19020", "19060", "24032", "25160",  "30011",
        "38040", "38110", "71074", "74022",  "74030",   "77120", "77131") ~ as_date("2008-08-19"),


      is.na(EFF_DATE) & neighborhood_code %in% c("13032", "28039", "28100", "15907", "23092", "39081", "39200", "39211") ~ as_date("2019-11-01"),

      is.na(EFF_DATE) & neighborhood_code %in% c("70010",  "73032", "73041",  "73084",
        "73093",  "74013",  "76010", "76011") ~ as_date("2021-09-10"),
      TRUE ~ as_date(EFF_DATE)),


    PRE_DATE = case_when(
      is.na(PRE_DATE) & neighborhood_code %in% c("19020", "19060", "24032", "25160",  "30011",
        "38040", "38110", "71074", "74022",  "74030", "77120", "77131") ~ as_date("2005-01-01"),


      is.na(PRE_DATE) &  neighborhood_code %in% c("13032", "15907", "28039", "28100", "39200", "23092", "39081", "39211") ~ as_date("2015-02-15"),

      is.na(PRE_DATE) & neighborhood_code %in% c("70010",  "73032", "73041",  "73084",
        "73093",  "74013",  "76010", "76011") ~ as_date("2019-06-28"),

      is.na(PRE_DATE) & neighborhood_code %in% c("10024", "18030") ~ as_date("2021-09-22"),

      TRUE ~ as_date(PRE_DATE))
  )

sales |> filter(is.na(PRE_DATE)) |> View()

#
# # sanity: each sale must EFF_DATE# sanity: each sale must know its panel’s EFF_DATE (baseline or updated)
# if (any(is.na(sales$PRE_DATE))) {
#   n <- sum(is.na(sales$PRE_DATE))
#   stop("Missing PRE_DATE for ", n, " sale(s). Fill baseline 2005-01-01 where appropriate.")
# }


# 2) After all backfills by neighborhood:
sales <- sales |>
  mutate(
    # compute flags ONLY after dates are settled
    panel_updated_eff = EFF_DATE %in% as_date(c("2019-11-01", "2021-09-10")),
    post_eff_update = ifelse((EFF_DATE == as_date("2019-11-01") & sale_date > EFF_DATE) | (EFF_DATE == as_date("2021-09-10") & sale_date > EFF_DATE), TRUE, FALSE),
    post_prelim_update = ifelse((PRE_DATE == as_date("2021-09-22") & sale_date > PRE_DATE) | (PRE_DATE == as_date("2019-06-28") & sale_date > PRE_DATE) | (PRE_DATE == as_date("2015-02-15") & sale_date > PRE_DATE), TRUE, FALSE)
  )

table(sales$post_eff_update)
table(sales$post_prelim_update)



sales <- sales |>
  mutate(
    sfha2018   = replace_na(sfha2018, 0L),
    sfha2024   = replace_na(sfha2024, 0L),
    lomr2018   = replace_na(lomr2018, 0L),
    lomr2024   = replace_na(lomr2024, 0L),

    # prelimsfha: keep NA outside the prelim coverage window to avoid fabricating zeros countywide
    # fills in 0s for PINs that were in the preliminary FIRM that were not in the SFHA with values from the effective firm in the 2022 state geodatabase
    prelimsfha = if_else(PRE_DATE == as_date("2021-09-22") & is.na(prelimsfha), sfha2024, prelimsfha)
  )

sales1 <- sales |>
  mutate(
    in_eff_sfha = case_when(
      post_eff_update ~ sfha2024,
      !post_eff_update ~ sfha2018
    ),
    in_eff_sfha = ifelse(in_eff_sfha == 1, T, F),  # fill in rest of parcels as FALSE if they were not identified as being in an SFHA polygon

    in_prelim_sfha = case_when(
      post_prelim_update ~ coalesce(prelimsfha, sfha2024),
      !post_eff_update ~ sfha2018),
    in_prelim_sfha = ifelse(in_prelim_sfha == 1, T, F)
  )


# stopifnot(!any(is.na(sales$PRE_DATE)))
stopifnot(!any(is.na(sales$sale_date)))
# prelim should be logical, no NAs:
# stopifnot(is.logical(sales1$in_prelim_sfha), !any(is.na(sales1$in_prelim_sfha)))

table(sales1$in_prelim_sfha)
table(sales1$in_eff_sfha)

table(sales1$PRE_DATE)
table(sales1$EFF_DATE)

table(sales1$prelimsfha[sales1$PRE_DATE == "2015-02-15"])  # empty - good
table(sales1$prelimsfha[sales1$PRE_DATE == "2019-06-28"])  # empty - good
table(sales1$prelimsfha[sales1$PRE_DATE == "2021-09-22"])

table(sales1$sfha2024[sales1$PRE_DATE == "2015-02-15"])
table(sales1$sfha2024[sales1$PRE_DATE == "2019-06-28"])
table(sales1$sfha2024[sales1$PRE_DATE == "2021-09-22"])

table(sales1$sfha2018[sales1$PRE_DATE == "2015-02-15"])
table(sales1$sfha2018[sales1$PRE_DATE == "2019-06-28"])
table(sales1$sfha2018[sales1$PRE_DATE == "2021-09-22"])


table(sales1$sfha2024[sales1$EFF_DATE == "2021-09-10"])
table(sales1$sfha2018[sales1$EFF_DATE == "2021-09-10"]) # should have more pins in SFHA than line of code above. PINs removed in Alsip area with their FIRM update


table(sales1$sfha2024[sales1$EFF_DATE == "2019-11-01"])
table(sales1$sfha2024[sales1$EFF_DATE == "2021-09-10"])
table(sales1$sfha2018[sales1$EFF_DATE == "2019-11-01"])
table(sales1$sfha2018[sales1$EFF_DATE == "2021-09-10"])
table(sales1$prelimsfha[sales1$EFF_DATE == "2019-11-01"])  # empty - good
table(sales1$prelimsfha[sales1$EFF_DATE == "2021-09-10"])  # empty - good

# 4) Transitions (no first-row flag)
sales1 <- sales1 |>
  group_by(pin) |>
  arrange(sale_date, .by_group = TRUE) |>
  mutate(
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


sales1 <- sales1 |>
  group_by(pin) |>
  arrange(sale_date, .by_group = TRUE) |>
  mutate(
    addedto_prelim_sfha   = dplyr::cumany(addedto_prelim_sfha),
    removedfrom_prelim_sfha = dplyr::cumany(removedfrom_prelim_sfha),

    addedto_eff_sfha  = dplyr::cumany(addedto_eff_sfha),
    removedfrom_eff_sfha = dplyr::cumany(removedfrom_eff_sfha)
  ) |> ungroup()


table(sales1$addedto_eff_sfha)
table(sales1$addedto_prelim_sfha)
table(sales1$removedfrom_eff_sfha)
table(sales1$removedfrom_prelim_sfha)

table(sales1$removedfrom_eff_sfha, sales1$EFF_DATE)
table(sales1$removedfrom_prelim_sfha, sales1$PRE_DATE)

table(sales1$addedto_prelim_sfha, sales1$PRE_DATE)
table(sales1$addedto_eff_sfha, sales1$EFF_DATE)


# LOMR and insurance requirement indicators
sales1 <- sales1 |>
  ungroup() |>
  mutate(
    in_lomr       = if_else(
      (sale_date >= as_date(EFF_DATlomr2018) | sale_date >= as_date(EFF_DATlomr2024)),
      TRUE, FALSE),
    in_lomr = if_else(is.na(in_lomr), FALSE, in_lomr),

    # properties that potentially have flood insurance requirement
    ins_req = if_else(in_eff_sfha == TRUE & in_lomr == FALSE, TRUE, FALSE)
  )
table(sales1$EFF_DATlomr2024)

table(sales1$in_lomr)
table(sales1$ins_req)

# Final: sales1 has in_eff_sfha, in_prelim_sfha, and added/removed flags




# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales1 |>
  ungroup() |>
  filter(!class %in% c(213, 218, 219)) |>
  filter(num_parcels_sale < 6) |>        # drop sales that involved a lot of parcels. Usually involves a CoOp, Condo, or land area being bought for construction, not a normal residential sale.
  filter(sale_price > 5000) |>
  mutate(
    class = as.numeric(class),
    res_c2 = class > 200 & class < 300,
    condo = if_else(class %in% c(298, 299), "Condo", "Not Condo"),  # NOTE: there are separate "single family homes" coded as condos because they are share a parcel and havea condo association.
    sale_year  = year(sale_date),
    eff_date = ymd(EFF_DATE),
    pre_date = ymd(PRE_DATE)
  ) |>
  group_by(pin) |>
  arrange(year) |>
  filter(any(res_c2)) |>
  mutate(
    times_sold      = n(),
    # years_btw_sales = as.numeric(sale_year) - as.numeric(lag(sale_year)),
    sold_once       = times_sold == 1,
    sold_multi      = times_sold > 1
  ) |>
  ungroup()

res_sales <- res_sales |> filter(times_sold < 15)


# Save objects for later use in your Quarto doc
saveRDS(sales1,       "./data/processed/sales_prepped_buildings_TEST2.rds")
saveRDS(res_sales,   "./data/processed/res_sales_buildings_TEST2.rds")
