# new script

# Load required libraries
library(tidyverse)
library(DescTools)    # for Winsorize(), not currently used
library(httr)
library(jsonlite)
library(glue)
library(lubridate)


drop_parcels <- c(   # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
  "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
  "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
  "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
  "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
  "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
  "2130124001", "2130124002","2130124003", "2130124004",  # almost completely in the lake
  "2130999001", "2132213002", # actual water canal in calumet area
  "2608202004", "2608400034", "3017211033" # also water pins. 
)


# 1. Read and preprocess sales data
#sales <- read_csv("./data/raw/Assessor_Parcel_Sales_20250105.csv") |>
sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20250709.csv") |>
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
  
table(firm_dates$EFF_DATE)

pin10_firms  <- read_csv("./data/processed/parcels_wFIRMS_20250604.csv")   |> 
  select(-c(PRE_DATE, EFF_DATE)) |> 
  filter(pin10 %nin% drop_parcels)

table(pin10_firms$VERSION_ID )


# trial code
pin10_firms <- pin10_firms |> 
  mutate(PRE_DATE = case_when(
   FIRM_PAN %in% prelim_FIRMS$old_firm_panel ~ as_date("2021-09-22"),
   VERSION_ID == "2.4.3.0" ~ as_date("2015-02-15") ,
   VERSION_ID == "2.4.3.5" ~ as_date("2019-06-28") ,
  TRUE ~ as_date("2005-01-01")),
 
   EFF_DATE = case_when(
    VERSION_ID == "2.4.3.0" ~ as_date("2019-11-01") ,
    VERSION_ID == "2.4.3.5" ~ as_date("2021-09-10") ,
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
  filter(pin10 %nin% drop_parcels) |>
  mutate(FIRM_PAN =
           case_when(
             pin10 == "0427302008" ~ "17031C0229J",
             pin10 == "0427302009" ~ "17031C0229J",
             pin10 == "0124100056" & is.na(FIRM_PAN) ~ "17031C0157J",
             pin10 == "0124100056" & is.na(FIRM_PAN) ~ "17031C0157K",
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



stopifnot(is.numeric(sales$sfha2018), is.numeric(sales$sfha2024))
stopifnot(!is.na(sales$FIRM_PAN))

# flag panels that actually got a 2019/2021 update
is_updated_panel_eff <- function(d) d %in% as_date(c("2019-11-01", "2021-09-10"))
is_updated_panel_prelim <- function(d) d %in% as_date(c("2015-02-15", "2019-06-28", "2021-09-22"))

sales <- sales |>
  mutate(
    panel_updated_eff = is_updated_panel_eff(EFF_DATE),
    has_prelim_window = is_updated_panel_prelim(PRE_DATE)
  )

sales <- sales |>
  mutate(
    sfha2018   = replace_na(sfha2018, 0L),
    sfha2024   = replace_na(sfha2024, 0L),
    lomr2018   = replace_na(lomr2018, 0L),
    lomr2024   = replace_na(lomr2024, 0L),
    
    # prelimsfha: keep NA outside the prelim coverage window to avoid fabricating zeros countywide
    prelimsfha = if_else(has_prelim_window & PRE_DATE == "2021-09-22", replace_na(prelimsfha, 0L), NA_integer_)
  )


table(sales$prelimsfha)

table(sales$sfha2018)
table(sales$sfha2024)

# # sanity: each sale must EFF_DATE# sanity: each sale must know its panel’s EFF_DATE (baseline or updated)
# if (any(is.na(sales$EFF_DATE))) {
#   n <- sum(is.na(sales$EFF_DATE))
#   stop("Missing EFF_DATE for ", n, " sale(s). Fill baseline 2008-08-19 where appropriate.")
# }

missing_dates <- sales |> 
  group_by(neighborhood_code) |> 
  summarize(n(),
            EFF_DATE = max(EFF_DATE),
            PRE_DATE = max(PRE_DATE),
            sfha2018 = max(sfha2018), 
            sfha2024 = max(sfha2024), 
            prelimsfhra = max(prelimsfha))

sales <- sales |>
  mutate(
    EFF_DATE = case_when(
      
      is.na(EFF_DATE) & neighborhood_code %in% c("10024", "19020", "19060", "24032","25160",  "30011", 
                                                 "38040", "38110", "71074", "74022",  "74030",   "77120", "77131") ~ as_date("2008-08-19"),
      
      
      is.na(EFF_DATE) & neighborhood_code %in% c("15907", "23092",  "70010",  "73032", "73041",  "73084", 
                                                 "73093",  "74013",  "76010", "76011") ~ as_date("2019-11-01"),
      
      is.na(EFF_DATE) & neighborhood_code %in% c("28039","28100", "39200") ~ as_date("2021-09-10"),
      TRUE ~ EFF_DATE),
    
    PRE_DATE = case_when(
      is.na(PRE_DATE) & neighborhood_code %in% c("10024", "19020", "19060", "24032","25160",  "30011", 
                                                 "38040", "38110", "71074", "74022",  "74030",   "77120", "77131") ~ as_date("2005-01-01"),
      
      
      is.na(PRE_DATE) &  neighborhood_code %in% c("15907", "23092",  "70010",  "73032", "73041",  "73084", 
                                                  "73093",  "74013",  "76010", "76011") ~ as_date("2015-02-15"),
      
      is.na(PRE_DATE) & neighborhood_code %in% c("28039","28100", "39200") ~ as_date("2019-06-28"),
      TRUE ~ PRE_DATE)
    
  )


# sanity: each sale must EFF_DATE# sanity: each sale must know its panel’s EFF_DATE (baseline or updated)
if (any(is.na(sales$EFF_DATE))) {
  n <- sum(is.na(sales$EFF_DATE))
  stop("Missing EFF_DATE for ", n, " sale(s). Fill baseline 2008-08-19 where appropriate.")
}


sales1 <- sales |>
  mutate(
    sfha2018_l = sfha2018 == 1L,
    sfha2024_l = sfha2024 == 1L,
    prelim_l   = prelimsfha == 1L
  ) |>
  mutate(
    # EFFECTIVE: before EFF_DATE use 2018; on/after use 2024; no update ⇒ always 2018
    in_eff_sfha = case_when(
      panel_updated_eff ~ if_else(sale_date < EFF_DATE, sfha2018_l, sfha2024_l),
      TRUE              ~ sfha2018_l
    ),
    
    # PRELIM: before PRE_DATE use 2018; on/after PRE_DATE inside coverage use prelim flag;
    in_prelim_sfha = case_when(
      has_prelim_window ~ if_else(sale_date < PRE_DATE, sfha2018_l, (prelim_l | sfha2024_l)),
      TRUE              ~ sfha2018_l)  # not updated, use 2018 values.
  )

sfha_ind |> filter(prelimsfha==1) |> count()

table(sales1$prelim_l)
table(sales1$in_prelim_sfha)
table(sales1$in_eff_sfha)

table(sales1$sfha2018_l)
table(sales1$sfha2024_l)

table(sales1$PRE_DATE)
table(sales1$EFF_DATE)
table(sales1$prelimsfha[sales1$PRE_DATE=="2021-09-22"])
table(sales1$prelimsfha[sales1$PRE_DATE=="2019-06-28"])
table(sales1$prelimsfha[sales1$PRE_DATE=="2015-02-15"]) 

table(sales1$sfha2024[sales1$PRE_DATE=="2021-09-22"])

table(sales1$sfha2024[sales1$EFF_DATE=="2021-09-10"]) 
table(sales1$sfha2018[sales1$EFF_DATE=="2021-09-10"]) # should have more pins in SFHA than line of code above. PINs removed in Alsip area with their FIRM update

sales1 <- sales1 |>
  group_by(pin) |>
  arrange(sale_date, .by_group = TRUE) |>
  mutate(
    lag_eff = coalesce(lag(in_eff_sfha), in_eff_sfha),     
    lag_pre = coalesce(lag(in_prelim_sfha), in_prelim_sfha),
    
    addedto_eff_sfha        = (lag_eff == FALSE & in_eff_sfha == TRUE),
    removedfrom_eff_sfha    = (lag_eff == TRUE  & in_eff_sfha == FALSE),
    
    addedto_prelim_sfha     = (lag_pre == FALSE & in_prelim_sfha == TRUE),
    removedfrom_prelim_sfha = (lag_pre == TRUE  & in_prelim_sfha == FALSE)
  ) |>
  ungroup()

table(sales1$addedto_eff_sfha)
table(sales1$addedto_prelim_sfha)
table(sales1$removedfrom_eff_sfha)
table(sales1$removedfrom_prelim_sfha)


# LOMR and insurance requirement indicators
sales1 <- sales1 |>
  ungroup() |>
  mutate(
    in_lomr       = if_else(
      (sale_date >= as_date(EFF_DATlomr2018) | sale_date >= as_date(EFF_DATlomr2024) ),
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
  filter(class %nin% c(213, 218, 219)) |>
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
    years_btw_sales = year - lag(year),
    sold_once       = times_sold == 1,
    sold_multi      = times_sold > 1
  ) |>
  ungroup() 

res_sales <- res_sales |> filter(times_sold < 15)


# Save objects for later use in your Quarto doc
saveRDS(sales1,       "./data/processed/sales_prepped_buildings_TEST.rds")
saveRDS(res_sales,   "./data/processed/res_sales_buildings_TEST.rds")
