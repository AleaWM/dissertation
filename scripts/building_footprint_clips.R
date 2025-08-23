# Purpose: Identifies building footprints in SFHA and LOMR areas.

# Outputs: Creates list of buildings in LOMRs and SFHAs

# Inputs: building footprints


library(tidyverse)
library(sf)

library(tictoc)
library(beepr)
#library(geodata)


pin_indicators <- read_csv("./data/processed/sfha_indicator_pins.csv")

parcel_indicators <- read_csv("./data/processed/sfha_indicator_parcels.csv")


buildings <-  st_read("./data/raw/cook_MS.shp") |>  
  st_transform("EPSG:6454")

# Cook County Border
border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |> 
  st_transform("EPSG:6454")


# brings in parcel information that is over inclusive and made in shapefile_sfha_changes.R
# parcels_sfha_2024 <- sf::read_sf("data/processed/parcels_sfha_2024.shp") |>
#  st_transform("EPSG:6454")



# state flood hazard areas, filter for Cook County only. 
# st_clip_sfha_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb", 
#                              layer = "S_FLD_HAZ_AR") |>
#   filter(SFHA_TF == "T") |>
#   st_transform("EPSG:6454") |>
#   st_intersection(border)

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


############ preliminary sfha  #################


prelim_sfha <-read_sf("inputs/FIRMDB_11182022_Cook-County_Illinois/S_Fld_Haz_Ar.shp")  |> 
  filter(SFHA_TF == "T") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)

prelim_sfha <- st_cast(prelim_sfha, "MULTIPOLYGON")
prelim_sfha <- st_transform(prelim_sfha, common_crs)

tic()
beep_on_error(buil_prelimdatabase <- st_join(buildings, prelim_sfha, join = st_intersects), sound = "wilhelm" )
buil_prelimdatabase <- buil_prelimdatabase |> filter(!is.na(DFIRM_ID))
write_sf(buil_prelimdatabase, "./data/processed/buil_prelimdatabase.shp" ) 
beep("coin")
toc()


## now add parcel data to the buildings in SFHA zones
parcels_prelim <- read_sf("./data/processed/parcels_2024_prelimdatabase.shp")
parcels_prelim <- st_transform(parcels_prelim, common_crs)

beep_on_error(buildings_in_sfha2 <- st_join(buil_prelimdatabase, parcels_prelim, join = st_intersects), sound = "wilhelm" )

n_distinct(buildings_in_sfha2$pin10) ## 231 distinct parcels

buildings_in_sfha2 |> group_by(pin10) |> mutate(n = n()) |> 
  filter(n>1) |> filter(!is.na(capture_da )) 


buildings_in_sfha3 <- buildings_in_sfha2 |> as.data.frame() |>
  select(pin = name, 
         DFIRM_ID, FLD_ZONE, ZONE_SUBTY, FLD_AR_ID) |>
  group_by(pin) |>
  slice(1) |>
  ungroup()

write_sf(buildings_in_sfha3, "./data/processed/buil_preliminary_sfha_20250816.csv" )


join <- inner_join(buildings_in_sfha2, parcel_indicators, by = c("pin10" = "pin10")) |>
  select(pin10)




############ Code in _functions.R script ##########33

buildings <-  st_read("./data/raw/cook_MS.shp") |>  
  st_transform("EPSG:6454")

# 3 not valid
# can check ones that weren't valid
notvalid <- buildings[!st_is_valid(buildings), ]

# keep valid parcels only:
buildings <- buildings[st_is_valid(buildings), ]

common_crs <- st_crs(buildings)
sf_use_s2(TRUE)

parcels_lomrs_2018 <- read_sf("./data/processed/parcels_lomrs_2018.shp")
lomrs2018 <- st_join(buildings, parcels_lomrs_2018, join = st_intersects) |>
  as.data.frame() |> 
  select(pin10,
         pin = name, 
         pin10,
         DFIRM_ID = DFIRM_I, LOMR_ID, EFF_DAT, CASE_NO) |>  
  group_by(pin10) |>
  slice(1) |>
  ungroup()


parcels_lomrs_2024 <- sf::read_sf("data/processed/parcels_lomrs_2024.shp")

lomrs2024 <- st_join(buildings, parcels_lomrs_2024, join = st_intersects) |>
  as.data.frame() |>
  select(pin10, 
         pin = name,
         pin10,
         DFIRM_ID = DFIRM_I,
         LOMR_ID, EFF_DAT, CASE_NO) |>
  group_by(pin10) |>
  slice(1) |>
  ungroup()

lomr_join <- lomrs2018 |>
  full_join(lomrs2024, 
            by = c("pin10", "CASE_NO"), 
            suffix = c("lomr2018", "lomr2024")
  )  |> 
  group_by(CASE_NO) |> 
  arrange(pin10) #|>
# rename(lomr_date = EFF_DAT)

lomr_join <- lomr_join|> distinct(pin10, CASE_NO, EFF_DATlomr2018, EFF_DATlomr2024
                                  #  lomr_date
)



## 2018 SFHA parcels and buildings in SFHAs
parcels_sfha_2018 <- read_sf("./data/processed/parcels_sfha_2018.shp")

sfha2018 <- st_join(buildings, parcels_sfha_2018, join = st_intersects) |>
  as.data.frame() |> 
  select(pin = name, 
         pin10,
         DFIRM_ID = DFIRM_I, FLD_ZON, ZONE_SU, FLD_AR_) |> 
  group_by(pin) |>
  slice(1) |>
  ungroup()


# 2024 parcels and buildings in SFHAs
parcels_sfha_2024 <- sf::read_sf("data/processed/parcels_sfha_2024.shp")

sfha2024 <- st_join(buildings, parcels_sfha_2024, join = st_intersects) |>
  as.data.frame() |>
  select(-c(GFID:geometry)) |>
  select(pin = name,
         pin10, 
         DFIRM_ID = DFIRM_I, FLD_ZON, ZONE_SU, FLD_AR_) |>
  group_by(pin) |>
  slice(1) |>
  ungroup()

parcels_2024_prelimdatabase <- sf::read_sf("./data/processed/parcels_2024_prelimdatabase.shp")
parcels_2024_prelimdatabase <- st_transform(parcels_2024_prelimdatabase, common_crs)

prelimsfha <- st_join(buildings, parcels_2024_prelimdatabase, join = st_intersects) |>
  as.data.frame() |>
  select(pin = name, 
         pin10,
         DFIRM_ID = DFIRM_I, FLD_ZONE_prelim= FLD_ZON, ZONE_SUBTY_prelim = ZONE_SU, FLD_AR_ID_prelim = FLD_AR_) |>
  group_by(pin) |>
  slice(1) |>
  ungroup()


## Combine SFHA and LOMR pins ##
pin_indicators <- sfha2018 |> full_join(sfha2024, by = c("pin", "pin10", "DFIRM_ID"), suffix = c("2018", "2024"))

pin_indicators <- pin_indicators |> full_join(prelimsfha, by = c("pin", "pin10", "DFIRM_ID"), suffix = c("", "prelim"))
#lomr_join <- lomrs2018 |> full_join(lomrs2024, by = c("pin", "pin10", "DFIRM_ID"), suffix = c("2018", "2024"))





pin_indicators <- pin_indicators |> full_join(lomr_join)

n_distinct(pin_indicators$pin) # 33434 unique PINs

pin_indicators <- pin_indicators|>
  mutate(
    sfha2018 = ifelse(!is.na(FLD_ZON2018), 1, 0),
    sfha2024 = ifelse(!is.na(FLD_ZON2024), 1, 0),
    prelimsfha = ifelse(!is.na(FLD_ZONE_prelim), 1, 0),
    lomr2018 =  ifelse(!is.na(EFF_DATlomr2018), 1, 0),
    lomr2024 = ifelse(!is.na(EFF_DATlomr2024), 1, 0)
  )
pin_indicators |> write_csv("./data/processed/sfha_indicator_buildings.csv")


