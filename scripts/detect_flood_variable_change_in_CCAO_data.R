# Purpose: Compare CCAO variable for PINs in SFHA to 
# the indicator created from FEMA flood maps

## Check if PINs change their SFHA indicator in the data --------------------

library(tidyverse)
library(DBI)
library(data.table)
library(httr)
library(jsonlite)
library(ptaxsim)
library(glue)

# Previous link used
# base_url <- "https://datacatalog.cookcountyil.gov/resource/tx2p-k2g9.json"

# switched to December 8th.
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

nbh_pins <- GET(
  base_url,
  query = list(
      env_flood_fema_sfha = TRUE,
    `$select` = paste0(c("pin", "pin10", 
                         "class", "year",
                         "township_code", "township_name",
                         "nbhd_code", "census_puma_geoid",                                            
                         "env_flood_fema_sfha",
                         "env_flood_fema_data_year",
                         "env_flood_fs_risk_direction", 
                         "env_flood_fs_factor",
                         "env_flood_fs_data_year",
                         "lat", "lon", 
                         "triad_name" ),
                       collapse = ","),
    `$limit` = 500000000L
  )
)

nbh_pins <- fromJSON(rawToChar(nbh_pins$content))
# 59,022 obs old base_url
# 147,831 on April 9th, for all building types, ALL YEARS <- which is why there are more  

sfha_pins <- nbh_pins %>% filter(env_flood_fema_sfha==TRUE) %>% distinct(pin)
# 29,753 unique pins

sfha_parcels <- nbh_pins %>% filter(env_flood_fema_sfha==TRUE) %>% distinct(pin10)
# 23,438 unique parcels

table(nbh_pins$env_flood_fema_data_year)
table(nbh_pins$env_flood_fs_factor)
table(nbh_pins$env_flood_fs_risk_direction)
table(nbh_pins$env_flood_fs_data_year)
table(nbh_pins$year)


sfha_change <- nbh_pins |> 
  group_by(pin) |> 
  arrange(year) |> 
  mutate(years_inSFHA = n(),
         first_year = first(year),
         class = first(class)) |> 
  filter(years_inSFHA < 4) |> 
  distinct(pin, township_name, first_year, class)


## compare to list of PINs created from FEMA floodplain shapefiles
lomr_pins_2024 <- sf::read_sf("data/processed/lomr_pins_2024.csv")
sfha_pins_2024 <- sf::read_sf("data/processed/sfha_pins_2024.csv")

## Are there CCAO SFHA pins in LOMRs? Did CCAO remove the LOMR pins when they made the variable?


sfha_pins |> 
  filter(pin %in% lomr_pins_2024$pin[lubridate::as_date(lomr_pins_2024$EFF_DAT) < "2018-01-01"])

# So I think CCAO removed LOMRs that were effective in 2018ish but not LOMR pins 
# that were granted effective LOMRs more recently
# 300 in 2017, increases up to 800+ in 2024

 

# lomr_pins_2024 <- sf::read_sf("data/processed/parcels_lomrs_2024.shp")
# sfha_pins_2024 <- sf::read_sf("data/processed/parcels_sfha_2024.shp")

# searching pins in the 2018 version since there are more PINs in SFHAs then.
# sfha_pins_2018 <- sf::read_sf("data/processed/parcels_sfha_2018.shp")



 nbh_pins |> 
  group_by(township_name, pin) |> 
  arrange(year) |> 
  mutate(years_inSFHA = n() ) |> 
   ungroup() |>
  filter(years_inSFHA < 4) |> 
   group_by(township_name) |>
   summarize(changed = n())
 
 nbh_pins |> 
   group_by(triad_name , township_name, pin) |> 
   arrange(year) |> 
   mutate(years_inSFHA = n() ) |> 
   ungroup() |>
   filter(years_inSFHA < 4) |> 
   group_by(triad_name) |>
   summarize(changed = n())
 
# CCAO identified floodzone pins.
floodpins <- nbh_pins %>%  filter(env_flood_fema_sfha==TRUE) %>% distinct(pin)

# Create the DB connection with the default name expected by PTAXSIM functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")

sfha_pins_ptax <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT*
  FROM pin
  WHERE pin IN ({sfha_pins*})
  AND year > 2017
  ",
    .con = ptaxsim_db_conn
  ))

joined_pins <- sfha_pins_ptax %>% 
  filter(year > 2020) |>
  mutate(year = as.character(year)) %>%
  left_join(nbh_pins) %>%
  mutate(sfha_bi = ifelse(env_flood_fema_sfha == FALSE , 0, 
                          ifelse(env_flood_fema_sfha == TRUE, 1, NA) ) ) %>%
  group_by(pin) %>%
  mutate(years_existed = n(),
    sfha_change = ifelse(sfha_bi - lag(sfha_bi) == 0, "No Change", 
                              ifelse(sfha_bi - lag(sfha_bi) == 1, "Left SFHA",
                                     ifelse(sfha_bi - lag(sfha_bi) == -1, "Enters SFHA",
                                            ifelse(years_existed < 4, "PIN doesn't exist all years", 
                                                   ifelse(is.na(sfha_bi), "PINs Don't Exist"))
                            )
  )))
  
joined_pins |> 
  group_by(year, sfha_change) %>%
  summarize(n=n())





base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

nbh_pins <- GET(
  base_url,
  query = list(
     env_flood_fs_factor >= "4",
    `$select` = paste0(c("pin", "pin10", 
                         "class", "year",
                         "township_code", "township_name",
                         "nbhd_code", "census_puma_geoid",                                            
                         "env_flood_fema_sfha", 
                         "env_flood_fema_data_year",
                         "env_flood_fs_risk_direction", 
                         "env_flood_fs_factor",
                         "env_flood_fs_data_year",
                         
                         "lat", "lon", 
                         "triad_name" ),
                       collapse = ","),
    `$limit` = 500000000L
  )
)

nbh_pins <- fromJSON(rawToChar(nbh_pins$content))

