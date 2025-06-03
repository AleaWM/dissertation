# data_prep.R -------------------------------------------------------------
# INPUTS:
#  - ./data/raw/Assessor_Parcel_Sales_20250105.csv
#  - ./data/processed/parcels_wFIRMS.csv
#  - ./data/processed/sfha_indicator_pins.csv
#  - ./data/processed/lomr_pins_2024.csv

# Load required libraries
library(tidyverse)
library(sf)
library(DescTools)    # for Winsorize(), if you later decide to use it
library(httr)
library(jsonlite)
library(glue)
library(lubridate)


# 1. Read and preprocess sales data
sales <- read_csv("./data/raw/Assessor_Parcel_Sales_20250105.csv") |>
  filter(year > 2005) |>
  mutate(
    class_1dig = str_sub(class, 1, 1),
    class       = as.numeric(class),
    date        = mdy(sale_date),
    pin10       = str_sub(pin, 1, 10),
    sale_date   = mdy(sale_date)
  )

# 2. Read flood‐related lookup tables
firms    <- read_csv("./data/processed/parcels_wFIRMS.csv") |>
  # preliminary SFHA date
  mutate(
    prelim_date = ifelse(PRE_DATE > "2026-01-01"  | is.na(PRE_DATE), "2005-01-01", PRE_DATE),
  
     ) |>  
  select(-c(PANEL_TYP, SCALE, PNP_REASON, BASE_TYP, geometry, PRE_DATE)) 



# only includes PINs that were in FEMA flood plain.
sfha_ind <- read_csv("./data/processed/sfha_indicator_pins.csv") |> 
  select(pin, sfha2018:lomr2024)


lomrs    <- read_csv("./data/processed/lomr_pins_2024.csv") |>
  rename(
    lomr_eff     = EFF_DAT,
    lomr_dfirm_id = DFIRM_I
  ) |>
  mutate(lomr_eff = ymd(lomr_eff))   |>  
  select(-c(mncplty, pltcltw, assssrn, assssrb, geoid, shp_Lng, shap_Ar, SCALE, STATUS, SOURCE_)) 


# 3. Merge firms & SFHA indicators into sales
sales <- sales |>
  left_join(firms,    by = "pin10") |>
  mutate(Location = if_else(pin %in% sfha_ind$pin, "In FP", "Outside FP")) |>
  left_join(sfha_ind, by = "pin") |>
  mutate(
    class    = as.character(class),
    lomr2018 = as.character(lomr2018),
    sfha2018 = as.character(sfha2018),
    lomr2024 = as.character(lomr2024),
    sfha2024 = as.character(sfha2024)
  )

# 4. Join LOMR table and create SFHA/LOMR flags
sales <- sales |>
  left_join(lomrs, by = c("pin","pin10") ) |>
  mutate(
 
    # # CCAO SFHA status post‐2021
    # ccao_sfha = case_when(
    #   sfha2024 == "1" & year >= 2021 ~ "SFHA",
    #   TRUE                            ~ "Not SFHA"
    # ),
    
    # baseline SFHA (from 2018) and updates via EFF_DATE
    # in_eff_sfha = case_when(
    #   
    #   sfha2018 == "1"                              ~ "SFHA",
    #   sfha2024 == "1" & year >= year(EFF_DATE)     ~ "SFHA",
    #   sfha2024 == "0" & year >= year(EFF_DATE)     ~ "Not SFHA",
    #   TRUE                                       ~ "Not SFHA"
    # ),
    

   # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
        in_eff_sfha = ifelse(sfha2018 == 1, "SFHA", "Not SFHA"),

        in_eff_sfha = ifelse(sfha2024 == 1 & year >= year(EFF_DATE), "SFHA",
                             ifelse(year >= year(EFF_DATE) & sfha2024 == 0, "Not SFHA", in_eff_sfha)),

        in_eff_sfha = ifelse((sfha2018 == 0 & sfha2024 == 0 ) | (is.na(sfha2018) & is.na(sfha2024)) , "Not SFHA", in_eff_sfha),

    # LOMR indicator
    in_lomr       = if_else(year >= year(lomr_eff) & !is.na(LOMR_ID),
                            "Received LOMR", "Not in LOMR"),
) 


# TODO: use grepl(...) on buyer_name + list of municipality names to flag buyouts

# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales |>
  filter(sale_price > 1000) |>
  mutate(
    class = as.numeric(class),
    res_c2 = class > 200 & class < 298
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


# # 6. Summarize into pin_groups
# pin_groups <- res_sales |>
#   group_by(pin) |>
#   arrange(year) |>
#   filter(n() < 6) |>
#   summarize(
#     times_sold     = n(),
#     multisales    = if_else(n() > 1, "Multi", "Once"),
#     sold_post2023 = as.integer(any(year > 2022)),
#     ccao_sfha     = first(ccao_sfha),
#     first_price   = first(sale_price),
#     min_price     = min(sale_price, na.rm = TRUE),
#     avg_price     = mean(sale_price, na.rm = TRUE),
#     max_price     = max(sale_price, na.rm = TRUE),
#     last_price    = last(sale_price),
#     .groups       = "drop"
#   )


# Save objects for later use in your Quarto doc
saveRDS(sales,       "./data/processed/sales_prepped.rds")
saveRDS(res_sales,   "./data/processed/res_sales.rds")
#saveRDS(pin_groups,  "./data/processed/pin_groups.rds")

