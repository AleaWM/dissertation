# Purpose: Identifies building footprints in SFHA and LOMR areas.

# Outputs: Creates list of buildings in LOMRs and SFHAs

# Inputs: building footprints


library(tidyverse)
library(sf)

library(tictoc)
library(beepr)
#library(geodata)

buildings <-  st_read("./data/raw/cook_MS.shp") |>  
  st_transform("EPSG:6454")

# Cook County Border
border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |> 
  st_transform("EPSG:6454")

parcels_sfha_2024 <- sf::read_sf("data/processed/parcels_sfha_2024.shp") |>
  st_transform("EPSG:6454")


# state flood hazard areas, filter for Cook County only.
st_clip_sfha_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb", 
                             layer = "S_FLD_HAZ_AR") |>
  filter(SFHA_TF == "T") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)

table(st_is_simple(buildings))
# 3 not valid

# can check ones that weren't valid
notvalid <- buildings[!st_is_valid(buildings), ]

# keep valid parcels only:
buildings <- buildings[st_is_valid(buildings), ]

# check stuff
st_geometry(buildings)
attributes(buildings)


common_crs <- st_crs(st_clip_sfha_2024)
buildings <- st_transform(buildings, common_crs)
sf_use_s2(TRUE)


## get buildings in SFHA zones
beep_on_error(buildings_in_sfha <- st_join(buildings, st_clip_sfha_2024,  join = st_intersects), sound = "wilhelm" )
buildings_in_sfha <- buildings_in_sfha |> filter(!is.na(DFIRM_ID))
# 20,590 building footprints in SFHAs

# preliminary file
write_sf(buildings_in_sfha, "./data/processed/buildings_in_sfha.shp" ) 


parcels_sfha_2024 <- st_transform(parcels_sfha_2024, common_crs)

## now add parcel data to the buildings in SFHA zones
beep_on_error(buildings_in_sfha2 <- st_join(buildings_in_sfha, parcels_sfha_2024, join = st_intersects), sound = "wilhelm" )

n_distinct(buildings_in_sfha2$pin10) ## 16,733 distinct buildings
n_distinct(buildings_in_sfha2$pin) ## 16,733 distinct buildings

buildings_in_sfha2 |> group_by(pin10) |> mutate(n = n()) |> 
  filter(n>1) |> filter(!is.na(capture_da )) 

write_sf(buildings_in_sfha2, "./data/processed/buildings_in_sfha_w_parcelinfo.shp" ) 

buildings_in_sfha3 <- buildings_in_sfha2 |> as.data.frame() |>
  select(
         pin10,
         capture_da, DFIRM_ID:ZONE_SUBTY, mncplty:assssrb, 
         FLD_AR_ID) |>
  group_by(pin10) |>
  slice(1) |>
  ungroup()

write_csv(buildings_in_sfha3, "./data/processed/buildings_distinct_in_sfha.csv" ) 



