# Purpose: Identifies parcels in SFHA areas and LOMRs in 2018 and the 2024 NFHL.
# Outputs: 
#    sfha_indicator_pins.csv
#    sfha_indicator_parcels.csv
# Inputs:


library(tidyverse)
library(sf)

library(tictoc)
library(beepr)
#library(geodata)


# Cook County Border
border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |> 
  st_transform("EPSG:6454")

# pulled from ptaxsim parcel polygons 
# parcels_2024 <- st_read("./data/raw/parcel_shapefiles_ty2023.gpkg", layer = "parcels") |> 
#   st_transform("EPSG:6454")

# downloaded from Cook County Parcel Archive for 2022
#st_layers("inputs/Mapping_Firms/Historical_parcels_-_2022.gdb/ccao_2022parcels.gdb")
parcels_2024 <- st_read("./inputs/Mapping_Firms/Historical_Parcels_-_2022.gdb/ccao_2022parcels.gdb", 
                        layer = "parcel_2022_parcel2022_enhanced")
# 1,416,419 pin10 distinct parcels,  1,416,446 distinct pins
parcels_2024 <- parcels_2024 |>
  st_transform("EPSG:6454") 

# st_layers("inputs/Historical_parcels_-_2018.gdb/parcels_2018.gdb")
# from CCAO parcel shapefiles online
parcels_2018 <- st_read("inputs/Historical_Parcels_-_2018.gdb/parcels_2018.gdb", 
                   layer = "parcel_2018_parcel")

parcels_2018 <- parcels_2018 |>
  st_transform("EPSG:6454")

# NFHL as of 2018 from Miyuki archive  -----------------------
#### Identify FIRM updates Between Years ##### 
#st_layers("inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb")

firm_2018 <- st_read("inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb", 
                     layer = "S_FIRM_PAN") |>
  st_transform("EPSG:6454") |>
  filter(DFIRM_ID=="17031C")
  #st_intersection(border)

write_csv(firm_2018, "data/raw/S_FIRM_PAN_2018.csv")


lomrs2018 <- st_read("inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb", 
                     layer = "S_LOMR") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)

fld_haz_ar_2018 <- st_read("inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb", 
                           layer = "S_Fld_Haz_Ar") |>
  st_transform("EPSG:6454") |>
  filter(SFHA_TF == "T") |>
  st_intersection(border)

effective_firms_2018 <- ggplot() +
  geom_sf(data = border, color = "black") +
  geom_sf(data = firm_2018, aes(geometry = SHAPE, fill = as.character(EFF_DATE)),
          color = "black") +
  ggtitle(label = "2018 NFHL - Clipped from State NFHL",
          subtitle = "Data from Miyuki archive")
effective_firms_2018


table(st_is_valid(parcels_2018)) ## 100 were not valid for 2018
table(st_is_valid(parcels_2024)) ## 77 were not valid for 2024

# can check ones that weren't valid
# notvalid <- parcels_2018[!st_is_valid(parcels_2018), ]

# keep valid parcels only:
parcels_2018 <- parcels_2018[st_is_valid(parcels_2018), ]
parcels_2024 <- parcels_2024[st_is_valid(parcels_2024), ]

# check stuff
# st_geometry(parcels_2024)
#a ttributes(parcels_2024)

fld_haz_ar_2018 <- st_cast(fld_haz_ar_2018, "MULTIPOLYGON")
common_crs <- st_crs(parcels_2018)
parcels_2018 <- st_transform(parcels_2018, common_crs)
parcels_2024 <- st_transform(parcels_2024, common_crs)
fld_haz_ar_2018 <- st_transform(fld_haz_ar_2018, common_crs)
sf_use_s2(TRUE)

#parcels_2018 <- st_set_precision(parcels_2018, 1e6)    # Set precision to reduce computational load
#parcels_2024 <- st_set_precision(parcels_2024, 1e6)    # Set precision to reduce computational load

tic()
beep_on_error(
  parcels_sfha_2018 <- st_join(parcels_2018, fld_haz_ar_2018, join = st_intersects), sound = "wilhelm"  ) # kept all 1.43 million parcels_2024
parcels_sfha_2018 <- parcels_sfha_2018 |> filter(!is.na(DFIRM_ID))
write_sf(parcels_sfha_2018, "./data/processed/parcels_sfha_2018.shp")

beep("coin")
toc()

## Make a CSV of just parcels and indicators
parcels_sfha_2018 <- read_sf("./data/processed/parcels_sfha_2018.shp")

sfha_2018 <- parcels_sfha_2018 |> 
  as.data.frame() |> 
  select(pin = name, 
          DFIRM_ID = DFIRM_I, 
         FLD_ZONE = FLD_ZON, ZONE_SUBTY = ZONE_SU, FLD_AR_) |> 
  group_by(pin) |>
  slice(1) |>
  ungroup()

write_csv(sfha_2018, "./data/processed/parcels_sfha_2018.csv")

tic()
beep_on_error(
  parcels_lomrs_2018 <- st_join(parcels_2018, lomrs2018, join = st_intersects), sound = "wilhelm"  )
parcels_lomrs_2018 <- parcels_lomrs_2018 |> filter(!is.na(LOMR_ID))
write_sf(parcels_lomrs_2018, "./data/processed/parcels_lomrs_2018.shp", )
beep("coin")
toc()

## Make a CSV of just parcels and indicators
parcels_lomrs_2018 <- read_sf("./data/processed/parcels_lomrs_2018.shp")
lomrs_2018 <- parcels_lomrs_2018 |> as.data.frame() |> 
  select(pin = name, 
         pin10,
         DFIRM_ID = DFIRM_I, LOMR_ID, EFF_DAT, CASE_NO) |>  
  group_by(pin10) |>
  slice(1) |>
  ungroup()
write_csv(lomrs_2018, "./data/processed/parcels_lomrs_2018.csv")




# State NFHL Database ------------------------------------------------
# as of June 28 2024, filtered to just cook county when read in.
# otherwise shows entire state flood layers
st_layers("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb")

st_clip_firms_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb",
                             layer = "S_FIRM_PAN") |>  
  st_transform("EPSG:6454") |>
  st_intersection(border) |>
  mutate(
    pre_date = paste0(year(PRE_DATE), "-", 
                      str_pad(month(PRE_DATE), width = 2, side = "left", pad = "0"), "-", 
                      str_pad(day(PRE_DATE), width=2, side = "left", pad = "0")),
    eff_date = paste0(year(EFF_DATE), "-",
                      str_pad(month(EFF_DATE), width = 2, side = "left", pad = "0"), "-",
                      str_pad(day(PRE_DATE), width=2, side = "left", pad = "0"))
  )
write_csv(st_clip_firms_2024, "data/raw/S_FIRM_PAN.csv")

st_clip_lomr_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb",
                      layer = "S_LOMR") |>  
  st_transform("EPSG:6454") |>
  st_intersection(border) 

# state flood hazard areas, filter for Cook County only.
st_clip_sfha_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb", 
                         layer = "S_FLD_HAZ_AR") |>
  filter(SFHA_TF == "T") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)

# FIRMS as of 2024 from State NFHL
ggplot() +
  geom_sf(data = border , fill = "gray20", color = "black") +
  geom_sf(data = st_clip_firms_2024 |> filter(DFIRM_ID=="17031C"), linewidth = 0.3, aes(fill = eff_date)) +
  theme_void() +
  scale_fill_ordinal() +
  labs( title = "FIRM Effective Date" , fill = "", )




# SFHA areas, and LOMRs mapped on top. 
version233 <- ggplot() +
  geom_sf(data = st_clip_sfha_2024 |> filter(VERSION_ID == "2.4.3.5"), aes(geometry = SHAPE), fill = "blue", alpha = 0.5, color = "black") +
  geom_sf(data = st_clip_lomr_2024 |> filter(VERSION_ID == "2.4.3.5"), fill = "gray50", alpha = 0.7, color = "black") +
  theme_void() +
  labs(title = "FEMA Flood Hazard Areas and LOMRs",
       subtitle = "Version ID 2.4.3.5",
       fill = "Flood Zones",
       caption = "Source: FEMA NFHL database, as of June 2024")
version233



singularFIRM <- st_transform(st_clip_firms_2024[st_clip_firms_2024$FIRM_PAN == "17031C0702K", ], "EPSG:6454")

st_clip_sfha_2024 |> 
  st_intersection(singularFIRM) |> 
  ggplot() +
  geom_sf(aes(geometry = SHAPE), fill = "blue", alpha = 0.7) +
  geom_sf(data = singularFIRM, aes(geometry = SHAPE), color = "black", fill = NA) 





fld_haz_ar_2024 <- st_cast(st_clip_sfha_2024, "MULTIPOLYGON")
fld_haz_ar_2024 <- st_transform(fld_haz_ar_2024, common_crs)


## 2024 SFHA parcels
tic()
beep_on_error(parcels_sfha_2024 <- st_join(parcels_2024, fld_haz_ar_2024, join = st_intersects), sound = "wilhelm" )# kept all 1.44 million parcels_2024
parcels_sfha_2024 <- parcels_sfha_2024 |> filter(!is.na(DFIRM_ID))
write_sf(parcels_sfha_2024, "./data/processed/parcels_sfha_2024.shp" )
beep("coin")
toc()

# and as a CSV with less variables
parcels_sfha_2024 <- sf::read_sf("data/processed/parcels_sfha_2024.shp")

sfha_2024 <- parcels_sfha_2024 |> as.data.frame() |>
  select(-c(GFID:shape)) |>
  select(pin = name, 
         DFIRM_ID = DFIRM_ID, FLD_ZONE, ZONE_SUBTY, FLD_AR_ = FLD_AR_ID) |>
  group_by(pin) |>
  slice(1) |>
  ungroup()

sfha_2024 |>  write_csv("data/processed/sfha_pins_2024.csv")



### 2024 LOMR parcels
lomrs2024 <- st_cast(st_clip_lomr_2024, "MULTIPOLYGON")

tic()
beep_on_error(parcels_lomrs_2024 <- st_join(parcels_2024, lomrs2024, join = st_intersects), sound = "wilhelm")
parcels_lomrs_2024 <- parcels_lomrs_2024 |> filter(!is.na(LOMR_ID))
write_sf(parcels_lomrs_2024, "./data/processed/parcels_lomrs_2024.shp")
beep("coin")
toc()

parcels_lomrs_2024 <- sf::read_sf("data/processed/parcels_lomrs_2024.shp")
# Make it a smaller CSV
lomrs_2024 <- parcels_lomrs_2024 |> as.data.frame() |>
  select(pin = name,
         pin10,
         DFIRM_ID = DFIRM_I,
         LOMR_ID, EFF_DAT, CASE_NO) |>
  group_by(pin10) |>
  slice(1) |>
  ungroup()

lomrs_2024 |>  write_csv("data/processed/lomr_pins_2024.csv")

## 2021 County Flood hazard layer database -------------------------------------
## most recent County database is from Sept 2021
# firm_2021 <- st_read("inputs/Mapping_FIRMs/17031C_20240319/S_FIRM_PAN.shp") |>
#   st_transform("EPSG:6454")|>
#   st_intersection(border)
# 
# effective_firms_2021 <- ggplot() +
#   geom_sf(data = border, color = "black") +
#   geom_sf(data = firm_2021, aes(geometry = geometry, fill = as.character(EFF_DATE)),
#           color = "black") +
#   ggtitle(label = "2021 NFHL - County Database",
#           subtitle = "Data from FEMA effective map products")
# effective_firms_2021
# 
# lomrs2021 <- st_read("inputs/Mapping_FIRMs/17031C_20240319/S_LOMR.shp") |>
#   st_transform("EPSG:6454") |>
#   st_intersection(border)
# 
# fld_haz_ar_2021 <- st_read("inputs/Mapping_FIRMs/17031C_20240319/S_Fld_Haz_Ar.shp") |>
#   st_transform("EPSG:6454") |>
#   filter(SFHA_TF == "T") |>
#   st_intersection(border)




# Preliminary Changes for Northern Cook County # ----------------------------------
#prelim_sfha <-read_sf("inputs/Mapping_Firms/Prelim_DL20230702/Prelim_CSLF.shp")  |> 
  prelim_sfha <-read_sf("inputs/FIRMDB_11182022_Cook-County_Illinois/S_Fld_Haz_Ar.shp")  |> 
  filter(SFHA_TF == "T") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)
 
prelim_sfha <- st_cast(prelim_sfha, "MULTIPOLYGON")
prelim_sfha <- st_transform(prelim_sfha, common_crs)

tic()
beep_on_error(parcels_2024_prelimdatabase <- st_join(parcels_2024, prelim_sfha, join = st_intersects), sound = "wilhelm" )# kept all 1.44 million parcels_2024
parcels_2024_prelimdatabase <- parcels_2024_prelimdatabase |> filter(!is.na(DFIRM_ID))
write_sf(parcels_2024_prelimdatabase, "./data/processed/parcels_2024_prelimdatabase.shp" ) 
beep("coin")
toc()

# prelim_sfha <- parcels_2024_prelimdatabase |> as.data.frame() |>
#   select(pin = name,
#         SFHACHG) |>
#   group_by(pin) |>
#   slice(1) |>
#   ungroup()

#write_sf(prelim_sfha, "./data/processed/parcels_preliminary_sfha.csv" )


prelim_sfha <- parcels_2024_prelimdatabase |> as.data.frame() |>
#  select(-c(GFID:geometry)) |>
  select(pin = name, 
         DFIRM_ID, FLD_ZONE, ZONE_SUBTY, FLD_AR_ID) |>
  group_by(pin) |>
  slice(1) |>
  ungroup()

write_sf(prelim_sfha, "./data/processed/parcels_preliminary_sfha_20250822.csv" )

 
### Join PIN lists together 
 
prelim_sfha |>filter(pin %in% lomrs_2024$pin)
 
prelim_sfha |> filter(pin %in% sfha_2024$pin)
 
pin_indicators <- sfha_2018 |> full_join(sfha_2024, by = c("pin", "DFIRM_ID"), suffix = c("2018", "2024"))
 
pin_indicators <- pin_indicators |> full_join(prelim_sfha,
                                              by = c("pin", "DFIRM_ID"), 
                                              ) |>
  rename(FLD_ZONE_pre = FLD_ZONE, ZONE_SUBTY_pre = ZONE_SUBTY, FLD_AR_ID_pre = FLD_AR_ID) 


lomr_join <- lomrs_2018 |> full_join(lomrs_2024, by = c("pin", "DFIRM_ID"), suffix = c("2018", "2024"))
pin_indicators <- pin_indicators |> full_join(lomr_join)

n_distinct(pin_indicators$pin) # 58,363 unique PINs
# 57,836 as of August 22 2025

pin_indicators <-pin_indicators|>
  mutate(
    sfha2018 = ifelse(!is.na(FLD_ZONE2018), 1, 0),
    sfha2024 = ifelse(!is.na(FLD_ZONE2024), 1, 0),
    prelimsfha = ifelse(!is.na(FLD_ZONE_pre), 1, 
                        0),  # if it is not in a preliminary SFHA in the northern part of Cook, then use values for sfha2024.
    #changed back to 0 ^^
    lomr2018 =  ifelse(!is.na(LOMR_ID2018), 1, 0),
    lomr2024 = ifelse(!is.na(LOMR_ID2024), 1, 0)
         )
pin_indicators |> write_csv("./data/processed/sfha_indicator_pins.csv")




# Make PARCEL indicator File Below --------------------------------------------
### REDO THE JOIN, use pin10 instead of pin!!!

sfha2018 <- read_csv("data/processed/parcels_sfha_2018.csv") |> 
  mutate(pin10 = str_sub(pin, 1, 10)) |>
  select(pin10, FLD_ZONE, FLD_AR_, ZONE_SUBTY) |>
  distinct()

sfha2024 <- read_csv("data/processed/sfha_pins_2024.csv") |>
  mutate(pin10 = str_sub(pin, 1, 10))|>
  select(pin10, FLD_ZONE, FLD_AR_, ZONE_SUBTY) |>
  distinct()

prelim_sfha_parcels <- read_csv("./data/processed/parcels_preliminary_sfha_20250605.csv") |>
  mutate(pin10 = str_sub(pin, 1, 10)) |>
  select(pin10, FLD_ZONE, FLD_AR_ = FLD_AR_ID, ZONE_SUBTY) |>
  distinct()


lomrs2018 <- read_csv("./data/processed/parcels_lomrs_2018.csv") |>
 # mutate(pin10 = str_sub(pin, 1, 10))|>
  select(-c(pin, DFIRM_ID) ) |> distinct() |>
  mutate(lomr_year = "2018")

lomrs2024 <- read_csv("data/processed/lomr_pins_2024.csv") |>
#  mutate(pin10 = str_sub(pin, 1, 10))|>
  select(-c(pin, DFIRM_ID) ) |> distinct() |>
  mutate(lomr_year = "2024")


pin_indicators <- sfha2024 |> full_join(prelim_sfha_parcels, by = "pin10", suffix = c("2024", "prelim"))

pin_indicators <- pin_indicators |> full_join(sfha2018, by = c("pin10"), suffix = c("", "2018"))



lomr_join <- lomrs2018 |>
  full_join(lomrs2024, 
            by = c("pin10", "CASE_NO", "EFF_DAT"), 
                                    suffix = c("lomr2018", "lomr2024")
                                   )  |> 
group_by(CASE_NO) |> 
  arrange(pin10) |>
  rename(lomr_date = EFF_DAT)


write_csv(lomr_join, "./data/processed/joined_LOMR_parcels.csv")

pin_indicators <- pin_indicators |> full_join(lomr_join)

pin_indicators <- pin_indicators|>
  mutate(
    sfha2018 = ifelse(!is.na(FLD_ZONE), 1, NA),
    sfha2024 = ifelse(!is.na(FLD_ZONE2024), 1, NA),
    prelimsfha = ifelse(!is.na(FLD_ZONEprelim), 1, NA),
    lomr2018 =  ifelse(!is.na(lomr_yearlomr2018), 1, NA),
    lomr2024 = ifelse(!is.na(lomr_yearlomr2024) , 1, NA)
  )

distinct_parcels <- pin_indicators |> distinct(pin10, sfha2018, sfha2024, prelimsfha, lomr2018, lomr2024)

pin_indicators |> write_csv("./data/processed/sfha_indicator_parcels.csv")




# Make a map of rivers in cook county -----------------------------------------
county_rivers <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb", 
                             layer = "S_WTR_LN") |>
  st_transform("EPSG:6454") |>
  st_intersection(border)


# makes black county shape with white rivers
ggplot() +
  geom_sf(data = border, aes(geometry = geometry), fill = "gray20", color = "black") +
  geom_sf(data = county_rivers, aes(geometry = SHAPE), color = "white", linewidth = 0.3)+
  theme_void()



# # Join FIRM data to parcels --------------------------------------------------
# tic()
# beep_on_error(
#   parcels_sfha_2024 <- st_join(parcels_2024, st_clip_firms_2024, join = st_intersects), sound = "wilhelm"  ) # kept all 1.43 million parcels_2024
# parcels_sfha_2018 <- parcels_sfha_2024 |> filter(!is.na(name))
# write_sf(parcels_sfha_2024, "./data/processed/parcels_inFIRMs_2024.shp", )
# 
# beep("coin")
# toc()
# 
