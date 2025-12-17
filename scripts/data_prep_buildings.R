# data_prep.R -------------------------------------------------------------
# INPUTS:
#  - ./data/raw/Assessor_Parcel_Sales_20250709.csv
#  - ./data/processed/parcels_wFIRMS.csv
#  - ./data/processed/sfha_indicator_buildings.csv
##  - ./data/raw/S_FIRM_PAN.xlsx



# Outputs:
#  - ./data/processed/sales_prepped_buildings.RDS
#  - ./data/processed/res_sales_buildings.RDS

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

sfha_ind <- sfha_ind |> 
  group_by(pin10) |> 
  summarize(sfha2018 = max(sfha2018),
            sfha2024 = max(sfha2024),
            prelimsfha = max(prelimsfha),
            lomr2018 = max(lomr2018),
            lomr2024 = max(lomr2024),
            EFF_DATlomr2018 = max(EFF_DATlomr2018),
            EFF_DATlomr2024 = max(EFF_DATlomr2024),
            )

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


# 3. Merge firms & SFHA indicators into sales
sales <- sales |>
  left_join(pin10_firms,    by = "pin10") |>
 # mutate(Location = if_else(pin %in% sfha_ind$pin, "In FP", "Outside FP")) |>
  left_join(sfha_ind, by = "pin10") #|>
  # mutate(
  #   class    = as.character(class),
  #   lomr2018 = (if_else(is.na(lomr2018), 0, lomr2018)),
  #   sfha2018 = (if_else(is.na(sfha2018), 0, sfha2018)),
  #   lomr2024 = (if_else(is.na(lomr2024), 0, lomr2024)),
  #   sfha2024 = (if_else(is.na(sfha2024), 0, sfha2024)),
  #   prelimsfha = (if_else(is.na(prelimsfha), 0, prelimsfha))
  # )

# excel file had PRE_Date as a POSIXct item and eff_date as a character item
firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN.xlsx") |>
  mutate(
    PRE_DATE = if_else(old_panel %in% c(15, 20, 155, 168, 186), as_date("2021-09-22"), as_date(PRE_DATE)),
    PRE_DATE = if_else(is.na(PRE_DATE), as_date("2005-01-01"), PRE_DATE)) |> # newly updated FIRMs originally had no effective date in hopes that they would become effective before dissertation was done. 
  
  mutate(EFF_DATE = if_else(is.na(EFF_DATE), "2008-08-19", EFF_DATE)) |> # newly updated FIRMs originally had no effective date in hopes that they would become effective before dissertation was done. 
  
  mutate(PRE_DATE = as_date(PRE_DATE),
         EFF_DATE = as_date(EFF_DATE)) |>
  select(FIRM_PAN, old_panel, PRE_DATE, EFF_DATE, Area)
#firm_dates <- readxl::read_xlsx("./data/raw/S_FIRM_PAN_2018and2024 NFHL Layers.xlsx") 

# 4. Join LOMR table and create SFHA/LOMR flags
sales <- sales |>
  left_join(firm_dates, by = "FIRM_PAN") |>
  mutate(
         PRE_DATE = as_date(PRE_DATE),
  ) |>
   mutate(
     
     in_eff_sfha = case_when(
       panel_updated_2019_2021 & sale_date >= EFF_DATE & sfha2024 == 1 ~ TRUE,
       panel_updated_2019_2021 & sale_date >= EFF_DATE & sfha2024 == 0 ~ FALSE,
       TRUE ~ sfha2018 == 1
     ),
     
     
  #   
  #   in_prelim_sfha = case_when(
  #     prelimsfha == 1 & sale_date >= PRE_DATE ~ TRUE,
  #     sfha2024 == 1 & sale_date >= PRE_DATE ~ TRUE,
  #     sfha2018 == 1 & sale_date >= PRE_DATE ~ TRUE
  #   )
     
     
    # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
    in_eff_sfha = if_else(sfha2018 == 1, TRUE, FALSE), # all years get assigned to SFHA if in sfha2018 because FIRM was updated in 2008
    # so if it was in an SFHA in 2018, it was in an SFHA in earlier years.
    
    in_eff_sfha = if_else(sfha2024 == 1 & sale_date >= EFF_DATE, TRUE,
                          if_else(sale_date >= EFF_DATE & sfha2024 == 0, FALSE, in_eff_sfha)),
    
    in_prelim_sfha = if_else(sfha2018 == 1 & sale_date >= PRE_DATE, TRUE, FALSE),
    
    in_prelim_sfha = if_else(sfha2024 == 1 & sale_date >= PRE_DATE, TRUE,
                             ifelse(sfha2024 == 0 & prelimsfha == 1 & sale_date >= PRE_DATE, TRUE, in_prelim_sfha)),
    
    in_prelim_sfha = ifelse(sfha2024 == 1 & sfha2018 == 0 & sale_date >= PRE_DATE, FALSE, in_prelim_sfha),
    
    in_prelim_sfha = if_else(prelimsfha == 1 & sale_date >= PRE_DATE, TRUE, 
                             ifelse(prelimsfha == 0 & sfha2024 == 1 & sale_date > PRE_DATE, FALSE, in_prelim_sfha)))


    
    group_by(pin) |>
      
      mutate(
        # directional effect of being added or removed from SFHA
        addedto_eff_sfha = if_else(lag(in_eff_sfha) == FALSE & in_eff_sfha == TRUE, TRUE, FALSE),
        removedfrom_eff_sfha = if_else(lag(in_eff_sfha) == TRUE & in_eff_sfha == FALSE, TRUE, FALSE),
        
        
        addedto_prelim_sfha = if_else(lag(in_prelim_sfha) == FALSE & in_prelim_sfha == TRUE, TRUE, FALSE),
        
    
        removedfrom_prelim_sfha = if_else(lag(in_prelim_sfha ) == TRUE & in_prelim_sfha == FALSE, TRUE, FALSE))       
    
    
    
    
    
    # # CCAO SFHA status post‐2021
    # ccao_sfha = case_when(
    #   sfha2024 == 1 & year >= 2021 ~ "SFHA",
    #   TRUE                            ~ "Not SFHA"
    # ),
    
 

   # # the flood zones that existed in the 2018 state NFHL were last updated in 2008. sfha2018 is the default SFHA status for the observations, then lomrs and future updates will be incorporated
   #      in_eff_sfha = if_else(sfha2018 == 1, "SFHA", "Not SFHA"), # all years get assigned to SFHA if in sfha2018 because FIRM was updated in 2008
   #      # so if it was in an SFHA in 2018, it was in an SFHA in earlier years.
   # 
   #      in_eff_sfha = if_else(sfha2024 == 1 & sale_date >= EFF_DATE, "SFHA",
   #                           if_else(sale_date >= EFF_DATE & sfha2024 == 0, "Not SFHA", in_eff_sfha)),
   # 
   #   #   in_eff_sfha = if_else((sfha2018 == 0 & sfha2024 == 0 ) | (is.na(sfha2018) & is.na(sfha2024)) , "Not SFHA", in_eff_sfha),
   # 
   #    addedto_eff_sfha = ifelse(lag(in_eff_sfha == ))  
   #      
   # 
   #      addedto_eff_sfha = if_else((sfha2018 == 0 & sfha2024 == 1 &
   #                                   sale_date > EFF_DATE), "MappedIn", "0"),
   #      removedfrom_eff_sfha = if_else((sfha2018 == 1 & sfha2024 == 0 ) &
   #                                      sale_date > EFF_DATE, "MappedOut", "0"),    
   # 
   #      addedto_prelim_sfha = if_else(((sfha2018 == 0 & prelimsfha == 1 &
   #                                      sale_date > PRE_DATE) |
   #                                      (sfha2018 == 0 & sfha2024 == 1 &
   #                                      sale_date > PRE_DATE) ), "MappedIn", "0"),
   #      
   #      removedfrom_prelim_sfha = if_else(((PRE_DATE == as_date("2021-09-22") & sfha2018 == 1 & prelimsfha == 0 &
   #                                      sale_date > PRE_DATE) |
   #                                        
   #                                        (sfha2018 == 1 & sfha2024 == 0 &
   #                                        sale_date > PRE_DATE)), "MappedOut", "0"),       
   # 
   #      
   #      
   #      
    # # create similar variable but for the preliminary date: model must deal with anticipation to change
    # 
    # in_prelim_sfha = if_else(sfha2018 == 1 & sale_date >= PRE_DATE, "SFHA", "Not SFHA"),
    # 
    # in_prelim_sfha = if_else(sfha2024 == 1 & sale_date >= PRE_DATE, "SFHA",
    #                      if_else(sale_date >= (PRE_DATE) & sfha2024 == 0, "Not SFHA", in_prelim_sfha)),
    # 
    # in_prelim_sfha = if_else((sfha2018 == 0 & sfha2024 == 0 ) | (is.na(sfha2018) & is.na(sfha2024)) , "Not SFHA", in_prelim_sfha),
    #   
    # in_prelim_sfha = if_else(prelimsfha == 1 & sale_date >= PRE_DATE, "SFHA", in_prelim_sfha),
    # 
    # 
    # LOMR indicator
    
  sales <- sales |>
    mutate(
    in_lomr       = if_else(
      (sale_date >= (EFF_DATlomr2018) | sale_date >= (EFF_DATlomr2024) ),
                            "Received LOMR", "Not in LOMR"),
    in_lomr = if_else(is.na(in_lomr), "Not in LOMR", in_lomr),
    
    # properties that potentially have flood insurance requirement
    ins_req = if_else(in_eff_sfha == "SFHA" & in_lomr == "Not in LOMR", TRUE, FALSE)
    
) 


# TODO: use grepl(...) on buyer_name + list of municipality names to flag buyouts

# 5. Build res_sales with timing‐of‐sale variables
res_sales <- sales |>
  filter(class %nin% c(213, 218, 219)) |>
  filter(num_parcels_sale < 6) |>        # drop sales that involved a lot of parcels. Usually involves a CoOp, Condo, or landarea being bought for construction, not a normal residential sale.
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
saveRDS(sales,       "./data/processed/sales_prepped_buildings.rds")
saveRDS(res_sales,   "./data/processed/res_sales_buildings.rds")


