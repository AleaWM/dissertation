## Pull list of PINs with First Street Flood Rating above Threshold

library(tidyverse)
library(data.table)
library(jsonlite)
library(glue)
library(httr)
library(rSocrata)


# API Endpoint
#new API, added to code January 5 2025
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

## Ideally I would use the env_flood_fs_factor > 3 in the filter but it never works??
puni_pins <- GET(
  base_url,
  query = list(
   year = 2021,
    `$select` = paste0(c("pin", "pin10", 
                         "class", "year",
                         "township_code", "township_name",
                         "nbhd_code", "census_puma_geoid",
                         "env_flood_fema_sfha", "env_flood_fema_data_year",
                         "env_flood_fs_risk_direction", "env_flood_fs_factor",
                         "lat", "lon", 
                         "triad_name" ),
                       collapse = ","),
    `$limit` = 5000000L
  )
)

puni_pins <- fromJSON(rawToChar(puni_pins$content))
# 1,867,818 obs
puni_pins <- puni_pins |> mutate(env_flood_fs_factor = as.numeric(env_flood_fs_factor) )

table(puni_pins$env_flood_fema_sfha) # 29,539  TRUE

table(puni_pins$env_flood_fs_factor)



puni_pins |> 
  filter(class>199&class < 300) |> count() 
## 1,585,866 residential PINs

puni_pins |> 
  filter(env_flood_fs_factor>3 & class>199&class < 300) |> count() 
## 331,556 > 3 (all classes)
## 276,004 Class 2 PINs


puni_pins  |> 
  filter(env_flood_fs_factor>4 & class>199&class < 300) |> count() 
## 239,425 > 4 (All Classes)
## 199,509 Class 2 PINs


puni_pins |> mutate(env_flood_fs_factor = as.numeric(env_flood_fs_factor) ) |> 
  filter(env_flood_fs_factor>5 & class>199&class < 300) |> count() 
## 202,371 > 5 (all  classes)
## 157,822 Class 2 PINs


puni_pins  |> 
  filter(env_flood_fema_sfha == T & class>199&class < 300) |> count() 
## 29,539 in SFHA (all  classes)
## 19,383 Class 2 PINs

puni_pins_coded <- puni_pins |> 
  mutate(SFHA = ifelse(env_flood_fema_sfha==TRUE, TRUE, FALSE),
         FFS_4plus = ifelse(env_flood_fs_factor >= 4, TRUE, FALSE),
         FFS_5plus = ifelse(env_flood_fs_factor >= 5, TRUE, FALSE),
         FFS_6plus = ifelse(env_flood_fs_factor >= 6, TRUE, FALSE),
         Res_C2 = ifelse(class > 199 & class < 300, TRUE, FALSE),
  )

puni_pins_coded |> 
  group_by(Res_C2, SFHA, FFS_4plus, FFS_5plus, FFS_6plus) |>
  summarize(n=n()) |> View()
