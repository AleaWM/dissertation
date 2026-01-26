# File Info ----
# Purpose: Pull unique PIN and parcel shapefiles for 2018 and 2023 into one list.
# identifies which FIRM they are in! (using the 2024 State NFHL - WARNING some FIRMS changed shapes!)
#
# Input(s): ptaxsim database
# Output(s):
#             "data/processed/parcels_wFIRMs.csv"
#             "./data/raw/parcel_shapefiles_ty2023.gpkg" <---  Not used
#             "./data/raw/parcel_shapefiles_ty2018.gpkg" <--- not used
#    Stores outputs in the `data/processed/` directory.
# Author: AWM
# Last updated: 2025-10-23



# Setup --------------------------------------------------

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)
library(sf)
library(beepr)
library(tictoc)
library(readxl)


# PTAXSIM has parcel centroids
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "../Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")

border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |>
  st_transform("EPSG:4326")

common_crs <- st_crs(border)

st_bbox(border) # look at coordinate "box" of area

# All parcel geometries using SQL db -------------------------------------------
# pin_geoms <- DBI::dbGetQuery(
#   ptaxsim_db_conn,
#   glue_sql(
#     "SELECT DISTINCT pin10, start_year, end_year, longitude, latitude, geometry
#   FROM pin_geometry_raw
#   ",
#     .con = ptaxsim_db_conn
# ))
#
# class(pin_geoms)
# # Convert character WKT geometry to sf object
# sf_data <- pin_geoms |>
#   st_as_sf(wkt = "geometry", crs = 4326)
#
# class(sf_data)
# # Set the correct CRS if missing (update EPSG code as needed)
#
# sf_data <- st_transform(sf_data, crs = common_crs)
# class(sf_data)
#
# attributes(sf_data)
#
# sf_data <- st_cast(sf_data, to = "POLYGON")
#
# table(st_is_valid(sf_data$geometry))
#
# sf_data <- st_make_valid(sf_data)
# sf_data <- sf_data[st_is_valid(sf_data), ]
#


# Centroids -------------------------------------------------------

pin_centroids <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin10, start_year, end_year, longitude, latitude
  FROM pin_geometry_raw
  ",
    .con = ptaxsim_db_conn
))

# Manually convert lat/lon to point
# some parcels had start and stop years split up across observations
pin_centroids <- pin_centroids |>
  group_by(pin10) |>
  summarize(longitude = first(longitude),
    latitude = first(latitude),
    start_year = min(start_year),
    end_year = max(end_year)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = common_crs, remove = FALSE)


# pin_centroids <- sf_data |> mutate(centroid = st_centroid(geometry))


## reads in FIRM layer from statewide geodatabase.
## clips it to just Cook County with `border` shapefile
# Pretend it is named ...firms_2026 for now.
st_clip_firms_2024 <- st_read("inputs/Cook_2026_download/S_FIRM_Pan.shp")
# st_clip_firms_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb",
#                               layer = "S_FIRM_PAN") |>
st_clip_firms_2024 <- st_clip_firms_2024 |>
  st_transform(common_crs) |>
  st_intersection(border) |>
  st_transform(common_crs)


st_bbox(st_clip_firms_2024)


st_clip_firms_2024 <- st_make_valid(st_clip_firms_2024)
st_clip_firms_2024 <- st_clip_firms_2024[st_is_valid(st_clip_firms_2024), ]

st_geometry(st_clip_firms_2024) <- "SHAPE"

# EFFECTIVE FIRMS as of 2024 from State NFHL
ggplot() +
  geom_sf(data = st_clip_firms_2024 |> filter(DFIRM_ID == "17031C"),
    linewidth = 0.3, aes(fill = factor(EFF_DATE))) +
  geom_sf(data = border, fill = NA, color = "black", lwd = 1) +
  theme_void() +
  # scale_fill_date() +
  labs(title = "FIRM Effective Date", fill = "") +
  scale_fill_grey(start = 0.85, end = 0.2)



# 1,457,163  distinct pin10 obs
pin_centroids <- st_join(pin_centroids, st_clip_firms_2024, join = st_within)
table(pin_centroids$DFIRM_ID) # check to see if it worked

# are there duplicate rows for parcels? -- Yes
# there are also parcels that have start year 2006-2012 and then 2013-2023
n_distinct(pin_centroids$pin10)
# 1,457,163 distinct. Many duplicates. Mostly due to weird start and stop years.
dups <- pin_centroids |>
  group_by(pin10) |>
  mutate(dup_count = n(),
    in_cook = ifelse(DFIRM_ID == "17031C", 1, 0)) |>
  filter(dup_count > 1)

dups_cook <- dups |>
  filter(in_cook == 1)
dups_notcook <- dups |>
  filter(in_cook == 0)

pin_centroids_clean <- anti_join(as.data.frame(pin_centroids), as.data.frame(dups_notcook))

pin_centroids_clean <- pin_centroids_clean |> as.data.frame() |> select(-c(PCOMM, PANEL, SUFFIX, ST_FIPS))

pin_centroids_clean <- pin_centroids_clean |> select(-c(SCALE, PANEL_TYP, PNP_REASON, BASE_TYP,  geometry))

# file of parcels and the FIRM panel they are located in, based on Panel data from the 2026 pending maps
# has all parcels that ever existed, and FIRM panel they would currently be in
write_csv(pin_centroids_clean, "data/processed/parcels_wFIRMs_2026.csv")


### --- Additional step: Add SFHA indicators TO the parcels_wFIRMs file at this stage instead of later: --- ##

parcels_wFIRMs <- read_csv("data/processed/parcels_wFIRMs_2026.csv") |>
  select(-c(PRE_DATE, EFF_DATE, OBJECTID_1, VERSION_ID, FIRM_ID, DFIRM_ID)) |>
  filter(!pin10 %in% drop_parcels)

# Manually check parcels missing FIRM panels:
parcels_wFIRMs |> filter(is.na(FIRM_PAN))  # parcels not in FIRM panels

drop_parcels <- c( # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
  "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
  "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
  "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
  "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
  "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
  "2130124001", "2130124002", "2130124003", "2130124004",  # almost completely in the lake
  "2130999001", "2132213002", # actual water canal in calumet area
  "2608202004", "2608400034", "3017211033" # also water pins.
)

# recode residential parcel that was perfectly on the line of two FIRM panels:
parcels_wFIRMs <- parcels_wFIRMs |>
  mutate(FIRM_PAN = ifelse(pin10 == "2130111028", "17031C0539K", FIRM_PAN))
parcels_wFIRMs |> filter(is.na(FIRM_PAN))  # parcels not in FIRM panels
# Success: No more parcels without FIRM panels


### --- Join FIRM info (dates, version) to Parcels with FIRM panel --- #

# FIRM_PAN from pending map update was all FIRMs in Cook County, not just the updated ones in Northern Cook
# NOTE: I took the S_FIRM_PAN shapefile and added the PRE_DATE for all FIRM Panels
# Panels that were pending and become effective in January 2026 were changed to
#    the original effective date of 2008 to remain "untreated" in effective FIRM models
firm_info <- read_xlsx("inputs/Cook_2026_download/S_FIRM_PAN.xlsx") |>
  select(FIRM_PAN, VERSION_ID, FIRM_ID, PRE_DATE, EFF_DATE)


parcels_wFIRMs <- left_join(parcels_wFIRMs, firm_info, by = "FIRM_PAN")

table(parcels_wFIRMs$PRE_DATE)
table(parcels_wFIRMs$EFF_DATE)

pin_indicators <-  read_csv("./data/processed/sfha_indicator_parcels_20260112.csv") |>
  mutate(pin10 = str_pad(pin10, 10, "left", 0))

pin_indicators <- full_join(parcels_wFIRMs, pin_indicators, by = "pin10")

pin_indicators <- pin_indicators |>
  mutate(in_prelim_panels = ifelse(VERSION_ID == "2.6.3.6", TRUE, FALSE))

pin_indicators <- pin_indicators |>
  mutate(
    sfha2018 = ifelse(!is.na(FLD_ZONE), 1, 0),
    sfha2024 = ifelse(!is.na(FLD_ZONE2024), 1, 0),
    lomr2018 =  ifelse(!is.na(LOMR_IDlomr2018), 1, 0),
    lomr2024 = ifelse(!is.na(LOMR_IDlomr2024), 1, 0)
  ) |>
  # fills in sfha2026 areas that were not updated with NFHL sfha indicators from 2024 database
  # if in prelim panel, documents if it was a truly not in the
  mutate(sfha2026 =
    ifelse(in_prelim_panels == TRUE & !is.na(FLD_ZONEprelim), 1,
      ifelse(in_prelim_panels == TRUE & is.na(FLD_ZONEprelim), 0,
        ifelse(in_prelim_panels == FALSE, sfha2024, "CHECKME")))
  )
table(pin_indicators$sfha2018)

table(pin_indicators$sfha2024)

table(pin_indicators$sfha2026)

write_csv(pin_indicators, "./data/processed/parcels_withFIRMs_20260112.csv")


# ####### The rest is not currently used ##############
# tic()
# beep_on_error(
#   sf_data <- st_join(sf_data, st_clip_firms_2024, join = st_within), sound = "wilhelm")
# write_sf(sf_data, "./data/processed/parcel_polygons_wFIRMs.gpkg")
# beep("coin")
# toc()
#
# # st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.shp", layer = "parcels", driver = "ESRI Shapefile")
# st_write(sf_data, "./data/raw/parcel_shapefiles.gpkg", layer = "parcels")
#
#
#
#
# ## PINS and then parcels, not as efficient -------------------------------------
#
# all_pins <- DBI::dbGetQuery(
#   ptaxsim_db_conn,
#   glue_sql(
#     "SELECT DISTINCT pin
#   FROM pin
#   ",
#     .con = ptaxsim_db_conn
# ))
#
# all_parcels <- all_pins |>
#   mutate(pin = str_pad(pin, 14, side = "left", pad = "0"),
#     pin10 = str_sub(pin, 1, 10)) |>
#   distinct(pin10)
#
# ## 2018 shapefiles ---------------------------------------------------
#
#
#
#
# years <- c(2018)
# fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
# library(sf)
#
# # Convert character WKT geometry to sf object
# sf_data <- fh_pins_geo |>
#   st_as_sf(wkt = "geometry", crs = 6465)
#
# # Set the correct CRS if missing (update EPSG code as needed)
# st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
# sf_data <- st_transform(sf_data, crs = 6454)
# sf_data <- st_cast(sf_data, "MULTIPOLYGON")
# # st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.shp", layer = "parcels", driver = "ESRI Shapefile")
# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.gpkg", layer = "parcels")
#
#
# ## 2023 Shapefiles -------------------------------------------------------------
# years <- c(2023)
# fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
# library(sf)
#
# # Convert character WKT geometry to sf object
# sf_data <- fh_pins_geo |>
#   st_as_sf(wkt = "geometry", crs = 6454)
#
# # Set the correct CRS if missing (update EPSG code as needed)
# st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
# sf_data <- st_transform(sf_data, crs = 6454)
#
# # st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.shp", layer = "parcels", driver = "ESRI Shapefile")
# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.gpkg", layer = "parcels")
