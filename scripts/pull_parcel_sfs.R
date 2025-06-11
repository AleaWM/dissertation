# File Info ----
# Purpose: Pull unique PIN and parcel shapefiles for 2018 and 2023 into one list.
# identifies which FIRM they are in! (using the 2024 State NFHL - WARNING some FIRMS changed shapes!)
# Quarto files use the shapefiles downloaded from Cook County Data Portal
# which have differences in variables and naming convention.
#
# Input(s): ptaxsim database
# Output(s):  
#             "data/processed/parcels_wFIRMs.csv"
#             "./data/raw/parcel_shapefiles_ty2023.gpkg" <---  Not used
#             "./data/raw/parcel_shapefiles_ty2018.gpkg" <--- not used
#    Stores outputs in the `data/raw/` directory.
# Author: AWM
# Last updated: 2025-05-06


# Setup --------------------------------------------------

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)
library(sf)
library(beepr)
library(tictoc)

#PTAXSIM has parcel centroids
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "../Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")

border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |> 
  st_transform("EPSG:4326")

common_crs <- st_crs(border)
st_bbox(border)
# All parcel geometries using SQL db -------------------------------------------
pin_geoms <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin10, start_year, end_year, longitude, latitude, geometry
  FROM pin_geometry_raw
  ",
    .con = ptaxsim_db_conn
  ))

class(pin_geoms)
# Convert character WKT geometry to sf object
sf_data <- pin_geoms |>
  st_as_sf(wkt = "geometry", crs = 4326)

class(sf_data)
# Set the correct CRS if missing (update EPSG code as needed)

sf_data <- st_transform(sf_data, crs = common_crs)
class(sf_data)

attributes(sf_data)

sf_data <- st_cast(sf_data, to = "POLYGON")

table(st_is_valid(sf_data$geometry))

sf_data <- st_make_valid(sf_data)
sf_data <- sf_data[st_is_valid(sf_data), ]



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
pin_centroids <- pin_centroids |>
  group_by(pin10) |>
  summarize(longitude = first(longitude),
            latitude = first(latitude),
            start_year = min(start_year),
            end_year = max(end_year)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = common_crs, remove=FALSE)


#pin_centroids <- sf_data |> mutate(centroid = st_centroid(geometry))

st_clip_firms_2024 <- read_sf("inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb",
                              layer = "S_FIRM_PAN") |>  
  st_transform(common_crs) |>
  st_intersection(border) |>  
  st_transform(common_crs) #|>
# 
#   mutate(
#     pre_date = paste0(year(PRE_DATE), "-",
#                       str_pad(month(PRE_DATE), width = 2, side = "left", pad = "0"), "-",
#                       str_pad(day(PRE_DATE), width=2, side = "left", pad = "0")),
#     eff_date = paste0(year(EFF_DATE), "-",
#                       str_pad(month(EFF_DATE), width = 2, side = "left", pad = "0"), "-",
#                       str_pad(day(PRE_DATE), width=2, side = "left", pad = "0"))
#   )

st_bbox(st_clip_firms_2024) 


st_clip_firms_2024 <- st_make_valid(st_clip_firms_2024)
st_clip_firms_2024 <- st_clip_firms_2024[st_is_valid(st_clip_firms_2024), ]

st_geometry(st_clip_firms_2024) <- "SHAPE"

# FIRMS as of 2024 from State NFHL
ggplot() +
  geom_sf(data = st_clip_firms_2024, 
          linewidth = 0.3, aes(fill = EFF_DATE ) ) +
  geom_sf(data = border, fill = NA, color = "black", lwd=1)+
  theme_void() +
 #scale_fill_date() +
  labs( title = "FIRM Effective Date" , fill = "" )



# 1,457,163  distinct pin10 obs
pin_centroids <- st_join(pin_centroids, st_clip_firms_2024, join = st_within)
table(pin_centroids$DFIRM_ID) # check to see if it worked

# are there duplicate rows for parcels? -- Yes
# there are also parcels that have start year 2006-2012 and then 2013-2023
n_distinct(pin_centroids$pin10) 
#1,457,163 distinct. Many duplicates. Mostly due to weird start and stop years. 
dups <- pin_centroids |> 
  group_by(pin10) |> 
  mutate(dup_count = n(),
         in_cook = ifelse(DFIRM_ID == "17031C", 1, 0)) |> 
  filter(dup_count > 1)

dups_cook <- dups |> 
  filter( in_cook == 1)
dups_notcook <- dups |> 
  filter( in_cook == 0)

pin_centroids_clean <- anti_join(as.data.frame(pin_centroids), as.data.frame(dups_notcook))

pin_centroids_clean <- pin_centroids_clean |> as.data.frame() |> select(-c(PCOMM, PANEL, SUFFIX, ST_FIPS, GFID:SHAPE_Area.1))

pin_centroids_clean <- pin_centroids_clean |> select(-c(SCALE, PANEL_TYP, PNP_REASON, BASE_TYP,  geometry) )

write_csv(pin_centroids_clean, "data/processed/parcels_wFIRMs_20250604.csv")






####### The rest is not currently used ##############
tic()
beep_on_error(
  sf_data <- st_join(sf_data, st_clip_firms_2024, join = st_within), sound = "wilhelm"  )
write_sf(sf_data, "./data/processed/parcel_polygons_wFIRMs.gpkg" )
 beep("coin")
toc()

# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles.gpkg", layer = "parcels") 




## PINS and then parcels, not as efficient -------------------------------------

all_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin 
  FROM pin
  ",
    .con = ptaxsim_db_conn
  ))

all_parcels <- all_pins |> 
  mutate(pin = str_pad(pin, 14, side = "left", pad = "0"),
         pin10 = str_sub(pin, 1, 10)) |>
  distinct(pin10) 

## 2018 shapefiles ---------------------------------------------------




years <- c(2018)
fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
library(sf)

# Convert character WKT geometry to sf object
sf_data <- fh_pins_geo |>
  st_as_sf(wkt = "geometry", crs = 6465)

# Set the correct CRS if missing (update EPSG code as needed)
st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
sf_data <- st_transform(sf_data, crs = 6454)
sf_data <-st_cast(sf_data, "MULTIPOLYGON")
# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.gpkg", layer = "parcels") 


## 2023 Shapefiles -------------------------------------------------------------
years <- c(2023)
fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
library(sf)

# Convert character WKT geometry to sf object
sf_data <- fh_pins_geo |>
  st_as_sf(wkt = "geometry", crs = 6454)

# Set the correct CRS if missing (update EPSG code as needed)
st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
sf_data <- st_transform(sf_data, crs = 6454)

# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.gpkg", layer = "parcels") 
