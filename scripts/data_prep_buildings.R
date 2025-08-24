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
#sales <- read_csv("./data/raw/Assessor_Parcel_Sales_20250105.csv") |>
  sales <- read_csv("./data/raw/Assessor_-_Parcel_Sales_20250709.csv") |>
  filter(year > 2005) |>
  mutate(
    class_1dig = str_sub(class, 1, 1),
    class       = as.numeric(class),
    pin10       = str_sub(pin, 1, 10),
    sale_date   = mdy(sale_date)
  )

# 2. Read flood‐related lookup tables
# includes the PIN and the FIRM_ID/FIRM_Panel that the PIN was in according to the effective NFHL from 2022
pin10_firms  <- read_csv("./data/processed/parcels_wFIRMS_20250604.csv")   |> 
  select(-c(PRE_DATE, EFF_DATE))


# only includes parcels that were flagged as having a BUILDING outline in the FEMA flood plain.
sfha_ind <- read_csv("./data/processed/sfha_indicator_buildings.csv")  

sfha_ind <- sfha_ind |> group_by(pin10) |> 
  summarize(sfha2018 = max(sfha2018),
            sfha2024 = max(sfha2024),
            prelimsfha = max(prelimsfha),
            lomr2018 = max(lomr2018),
            lomr2024 = max(lomr2024),
            EFF_DATlomr2018 = max(EFF_DATlomr2018),
            EFF_DATlomr2024 = max(EFF_DATlomr2024),
            )

# 3. Merge firms & SFHA indicators into sales
sales <- sales |>
  left_join(pin10_firms,    by = "pin10") |>
 # mutate(Location = if_else(pin %in% sfha_ind$pin, "In FP", "Outside FP")) |>
  left_join(sfha_ind, by = "pin10") |>
  mutate(
    class    = as.character(class),
    lomr2018 = (ifelse(is.na(lomr2018), 0, lomr2018)),
    sfha2018 = (ifelse(is.na(sfha2018), 0, sfha2018)),
    lomr2024 = (ifelse(is.na(lomr2024), 0, lomr2024)),
    sfha2024 = (ifelse(is.na(sfha2024), 0, sfha2024)),
    prelimsfha = (ifelse(is.na(prelimsfha), 0, prelimsfha))
  )


firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx") |>
  mutate(PRE_DATE = as_date(PRE_DATE),
         EFF_DATE = as_date(EFF_DATE)) |>
  select(FIRM_PAN, old_panel, PRE_DATE, EFF_DATE)
#firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN_2018and2024 NFHL Layers.xlsx") 

# 4. Join LOMR table and create SFHA/LOMR flags
sales <- sales |>
  left_join(firm_dates, by = "FIRM_PAN") |>
  mutate(PRE_DATE = ifelse(old_panel %in% c(15, 20, 155) & year >= 2021, as_date("2021-09-22"), as_date(PRE_DATE)),
         PRE_DATE = as_date(PRE_DATE),
  ) |>
  #left_join(lomrs, by = c("pin","pin10") ) |>
  mutate(
 
    # # CCAO SFHA status post‐2021
    # ccao_sfha = case_when(
    #   sfha2024 == "1" & year >= 2021 ~ "SFHA",
    #   TRUE                            ~ "Not SFHA"
    # ),
    
 

   # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
        in_eff_sfha = ifelse(sfha2018 == 1, "SFHA", "Not SFHA"),

        in_eff_sfha = ifelse(sfha2024 == 1 & sale_date >= EFF_DATE, "SFHA",
                             ifelse(sale_date >= EFF_DATE & sfha2024 == 0, "Not SFHA", in_eff_sfha)),

        in_eff_sfha = ifelse((sfha2018 == 0 & sfha2024 == 0 ) | (is.na(sfha2018) & is.na(sfha2024)) , "Not SFHA", in_eff_sfha),

        
        
   
        addedto_eff_sfha = ifelse((sfha2018 == 0 & sfha2024 == 1 &
                                     sale_date > EFF_DATE), "MappedIn", "0"),
        addedto_prelim_sfha = ifelse((sfha2018 == 0 & prelimsfha == 1 &
                                        sale_date > PRE_DATE), "MappedIn", "0"),
        
        removedfrom_eff_sfha = ifelse((sfha2018 == 1 & sfha2024 == 0 ) &
                                        sale_date > EFF_DATE, "MappedOut", "0"),
        
        
        
    # create similar variable but for the preliminary date: model must deal with anticipation to change
    
    in_prelim_sfha = ifelse(sfha2018 == 1 & sale_date >= PRE_DATE, "SFHA", "Not SFHA"),
    
    in_prelim_sfha = ifelse(sfha2024 == 1 & sale_date >= PRE_DATE, "SFHA",
                         ifelse(sale_date >= (PRE_DATE) & sfha2024 == 0, "Not SFHA", in_prelim_sfha)),
    
    in_prelim_sfha = ifelse((sfha2018 == 0 & sfha2024 == 0 ) | (is.na(sfha2018) & is.na(sfha2024)) , "Not SFHA", in_prelim_sfha),
      
    in_prelim_sfha = ifelse(prelimsfha == 1 & sale_date >= PRE_DATE, "SFHA", in_prelim_sfha),
    
    
    # LOMR indicator
    in_lomr       = if_else(sale_date >= (EFF_DATlomr2018) | sale_date >= (EFF_DATlomr2024) ,
                            "Received LOMR", "Not in LOMR"),
    in_lomr = ifelse(is.na(in_lomr), "Not in LOMR", in_lomr),
    
) 


# TODO: use grepl(...) on buyer_name + list of municipality names to flag buyouts

# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales |>
  filter(class %nin% c(213, 218, 219)) |>
  filter(num_parcels_sale < 6) |>        # drop sales that involved a lot of parcels. Usually involves a CoOp, Condo, or landarea being bought for construction, not a normal residential sale.
  filter(sale_price > 1000) |>
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
  ungroup() 

res_sales <- res_sales |> filter(times_sold < 15)


# Save objects for later use in your Quarto doc
saveRDS(sales,       "./data/processed/sales_prepped_buildings.rds")
saveRDS(res_sales,   "./data/processed/res_sales_buildings.rds")


