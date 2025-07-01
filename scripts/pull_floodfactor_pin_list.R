# ----
# Purpose: Pull Cook County parcel universe PINs with FEMA Flood Risk Variables
# and 2019 Flood Factor scores to assign flood risk to PINs
# Input(s): CCAO API Parcel data with PINs, Flood Factor 2019 snapshot, SFHA indicator
# Output(s): data/raw/floodfactor_scores.csv
#          : parcel_universe_currentyear.csv
# Use: Information that the assessor uses for their valuation! Starting in 2021-ish
# Last updated: 2025-07-01
# ----

## Pull list of PINs with any First Street Flood Rating Scores and SFHA indicators

library(tidyverse)
library(data.table)
library(jsonlite)
library(glue)
library(httr)


# API Endpoint
#new API, added to code January 5 2025
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"


puni_pins <- GET(
  base_url,
  query = list(
    `$select` = "DISTINCT pin, nbhd_code, zip_code, env_flood_fs_factor, env_flood_fema_sfha, env_flood_fs_risk_direction",
    `$where` = "year > 2018 AND env_flood_fs_factor IS NOT NULL AND env_flood_fema_sfha IS NOT NULL AND env_flood_fs_risk_direction IS NOT NULL",
    `$limit` = 5000000L
  )
)

# 1,857,670 observations pulled from API call
puni_pins <- fromJSON(rawToChar(puni_pins$content))

# 1,856,229 distinct pins 
n_distinct(puni_pins$pin) 

## Has duplicates and missing values for key variables:
write_csv(puni_pins, "./data/raw/floodfactor_scores.csv")


# puni_pins_2 <- puni_pins |>
#   mutate( 
#     pin = as.character(pin),
#     pin_prefix = str_sub(pin, 1, 7)
#   ) |>
#   
#   # Step 1: Sort by descending pin
#   arrange(desc(pin)) |>
#   
#   # Step 2: Fill down and up zip_code within nbhd_code group and 7 digit land blocks
#   group_by(nbhd_code, pin_prefix) |>
#   mutate(zip_code = ifelse(zip_code == "0", NA, zip_code)) |> 
#   fill(zip_code, .direction = "downup") |>
#   ungroup() |>
#   
#   
#   # Step 2B: Fill down and up zip_code within 7 digit land blocks
#   group_by(pin_prefix) |>
#   fill(zip_code, .direction = "downup") |>
#   ungroup() |>
#   
#   # Step 3: Fill down and up nbhd_code within zip_code + pin_prefix
#   group_by(zip_code, pin_prefix) |>
#   fill(nbhd_code, .direction = "downup") |>
#   ungroup() |>
#   
#   # Clean up
#   select(-pin_prefix) |>
#   distinct(pin, nbhd_code, zip_code, env_flood_fs_factor, 
#            env_flood_fema_sfha, env_flood_fs_risk_direction)
# 
# # 1,856,636 obs after filling in and removing dups
# n_distinct(puni_pins_2$pin)
# # 1,856,229 distinct pins 


# Save original for comparison
df_flagged <- puni_pins |>
  mutate(
    pin = as.character(pin),
    pin_prefix = str_sub(pin, 1, 7),
    zip_code_original = zip_code,
    nbhd_code_original = nbhd_code
  ) |>
  
  # Sort descending by PIN
  arrange(desc(pin)) |>
  
  # Fill in zip_code based on nbhd_code and 7 digit land block
  group_by(nbhd_code, pin_prefix) |>
  mutate(zip_code = ifelse(zip_code == "0", NA, zip_code)) |> 
  fill(zip_code, .direction = "downup") |>
  ungroup() |>
  
  
  # Step 2B: Fill down and up zip_code within 7 digit land blocks
  group_by(pin_prefix) |>
  fill(zip_code, .direction = "downup") |>
  ungroup() |>
  
  
  # Fill in nbhd_code based on zip_code + pin_prefix
  group_by(zip_code, pin_prefix) |>
  fill(nbhd_code, .direction = "downup") |>
  ungroup() |>

  # Create flags: TRUE if filled (i.e. original was NA and now it's not)
  mutate(
    zip_code_filled = is.na(zip_code_original) & !is.na(zip_code),
    nbhd_code_filled = is.na(nbhd_code_original) & !is.na(nbhd_code)
  ) |>
  
  # Drop helper columns
  select(-pin_prefix, -zip_code_original, -nbhd_code_original) |>
  distinct(pin, nbhd_code, zip_code, 
         #  env_flood_fema_sfha, # excluding because some pins change status. Will explore more when looking at the model updates impact on assessed values. 
           env_flood_fs_factor, 
           env_flood_fs_risk_direction,
           zip_code_filled, nbhd_code_filled
         ) 


dups <- df_flagged |> 
  mutate(n = n(), .by = pin) |>
  filter( n > 1)

dups_filled <- dups |>
  filter(zip_code_filled==TRUE | nbhd_code_filled == TRUE) |>
  mutate(n = n(), .by = pin)

dups_oldnbhd <- dups |> filter(nbhd_code != c("22360"))

dups_notfilled <-  dups |>
  filter(zip_code_filled==FALSE & nbhd_code_filled == FALSE) |>
  mutate(n = n(), .by = pin)
# 772 observations (or 386 pins) that had their SFHA code changed by CCAO
# but I am not using their SFHA code in my models YET so can ignore this data issue for slightly longer.

table(df_flagged$env_flood_fema_sfha) 
# FALSE    TRUE 
# 1828084   29582

table(df_flagged$env_flood_fs_factor)
# 1.0    10.0     2.0     3.0     4.0     5.0     6.0     7.0     8.0        9.0
# 1412236    5901   19107   94334   92253   37102  161501   20161    3653   11418 

df_flagged  |> 
  filter(env_flood_fs_factor>4) |> count() 
## 326,088> 4 (All Classes)

n_distinct(df_flagged$pin) # 1,856,229
# still at least 500 duplicates

sum(is.na(df_flagged$nbhd_code))
sum(is.na(df_flagged$zip_code)) # 12,126 observations missing zipcode 


# keep one observation per pin, even if it isn't perfect. 
df_flagged <-   df_flagged |> 
  group_by(pin) |>
  arrange(pin, desc(nbhd_code_filled), desc(zip_code_filled)) |>
  slice(1) |> ungroup()

write_csv(df_flagged, "./data/processed/floodfactor_scores.csv")




##############
# check for dup
scores <- read_csv("./data/raw/floodfactor_scores.csv")

n_distinct(scores$pin)

scores |> filter(is.na(zip_code))

scores <- scores |> 
  group_by(pin) |>
  mutate(n = n()) |>
  filter((!is.na(zip_code) & n > 1) | n == 1)



scores|> group_by(pin) |> filter(zip_code  != 60067 & zip_code != 0) |> mutate(n=n()) |> filter(n>1) |> View()


scores <- scores |> group_by(nbhd_code) |>
  mutate(zip_code = ifelse(zip_code %in% c(0, "0"), NA, zip_code) ) |> ungroup()


nbds_n_zips <- scores |> group_by(nbhd_code) |>
  summarize(zip_code_max = max(zip_code, na.rm=TRUE),
            zip_code_min = min(zip_code, na.rm=TRUE)) |>
  mutate(zip_code = ifelse(zip_code %in% c(0, "0"), zip_code_max, zip_code) )

nbds_n_zips |> filter(zip_code_max != zip_code_min) |> View()

n_distinct(scores$pin)


#### Current year Parcel Universe 
# Because API call to recreate raw scores wasn't working earlier. 
# It is working again now though so probably don't need the current year scores.
# file created above should be more complete than using just the current year's data 

base_url <- "https://datacatalog.cookcountyil.gov/resource/pabr-t5kh.json"

puni_pins <- GET(
  base_url,
  query = list(
    `$select` = "pin, class, nbhd_code, zip_code, env_flood_fs_factor, env_flood_fema_sfha, env_flood_fs_risk_direction",
    `$limit` = 5000000L
  )
)
puni_pins <- fromJSON(rawToChar(puni_pins$content))

puni_pins_2 <- puni_pins |>
  mutate(
    pin = as.character(pin),
    pin_prefix = str_sub(pin, 1, 7)
  ) |>
  
  # Step 1: Sort by descending pin
  arrange(desc(pin)) |>
  
  # Step 2: Fill down and up zip_code within nbhd_code group
  group_by(nbhd_code, pin_prefix) |>
  fill(zip_code, .direction = "downup") |>
  ungroup() |>
  
  # Step 3: Fill down and up nbhd_code within zip_code + pin_prefix
  group_by(zip_code, pin_prefix) |>
  fill(nbhd_code, .direction = "downup") |>
  ungroup() |>
  
  # Clean up
  select(-pin_prefix)



# Save original for comparison
df_flagged <- puni_pins |>
  mutate(
    pin = as.character(pin),
    pin_prefix = str_sub(pin, 1, 7),
    zip_code_original = zip_code,
    nbhd_code_original = nbhd_code
  ) |>
  
  # Sort descending by PIN
  arrange(desc(pin)) |>
  
  # Fill in zip_code based on nbhd_code
  group_by(nbhd_code) |>
  fill(zip_code, .direction = "downup") |>
  ungroup() |>
  
  # Fill in nbhd_code based on zip_code + pin_prefix
  group_by(zip_code, pin_prefix) |>
  fill(nbhd_code, .direction = "downup") |>
  ungroup() |>
  
  # Create flags: TRUE if filled (i.e. original was NA and now it's not)
  mutate(
    zip_code_filled = is.na(zip_code_original) & !is.na(zip_code),
    nbhd_code_filled = is.na(nbhd_code_original) & !is.na(nbhd_code)
  ) |>
  
  # Drop helper columns
  select(-pin_prefix, -zip_code_original, -nbhd_code_original)


write_csv(df_flagged, "./data/raw/parcel_universe_currentyear.csv")


#############################################################################

# Code for when Class was a variable in the API pull:
# puni_pins |> mutate(env_flood_fs_factor = as.numeric(env_flood_fs_factor) ) |> 
#   filter(env_flood_fs_factor>5 & class>199&class < 300) |> count() 
# ## 202,371 > 5 (all  classes)
# ## 157,822 Class 2 PINs
# 
# 
# puni_pins  |> 
#   filter(env_flood_fema_sfha == T & class>199&class < 300) |> count() 
# ## 29,539 in SFHA (all  classes)
# ## 19,383 Class 2 PINs
# 
# puni_pins_coded <- puni_pins |> 
#   mutate(SFHA = ifelse(env_flood_fema_sfha==TRUE, TRUE, FALSE),
#          FFS_4plus = ifelse(env_flood_fs_factor >= 4, TRUE, FALSE),
#          FFS_5plus = ifelse(env_flood_fs_factor >= 5, TRUE, FALSE),
#          FFS_6plus = ifelse(env_flood_fs_factor >= 6, TRUE, FALSE),
#          Res_C2 = ifelse(class > 199 & class < 300, TRUE, FALSE),
#   )
# 
# puni_pins_coded |> 
#   group_by(Res_C2, SFHA, FFS_4plus, FFS_5plus, FFS_6plus) |>
#   summarize(n=n()) |> View()
