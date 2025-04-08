# ----
# Purpose: Pull unique PIN and parcel shapefiles for 2018 and 2023 into one list.
# but NOT CURENTLY USED! 
# Quarto files use the shapefiles downloaded from Cook County Data Portal
# which have differences in variables and naming convention.
#
# Input(s): ptaxsim database
# Output(s):  "./data/raw/parcel_shapefiles_ty2023.gpkg" &  "./data/raw/parcel_shapefiles_ty2018.gpkg"
#    Stores outputs in the `data/raw/` directory.
# Author: AWM
# Last updated: 2025-04-07
# ----


library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "../Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")


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

# 2018 shapefiles ---------------------------------------------------
years <- c(2018)
fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
library(sf)

# Convert character WKT geometry to sf object
sf_data <- fh_pins_geo |>
  st_as_sf(wkt = "geometry", crs = 6465)

# Set the correct CRS if missing (update EPSG code as needed)
st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
sf_data <- st_transform(sf_data, crs = 6454)

# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles_ty2018.gpkg", layer = "parcels") 


# 2023 Shapefiles -------------------------------------------------------------
years <- c(2023)
fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
library(sf)

# Convert character WKT geometry to sf object
sf_data <- fh_pins_geo |>
  st_as_sf(wkt = "geometry", crs = 6465)

# Set the correct CRS if missing (update EPSG code as needed)
st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.
sf_data <- st_transform(sf_data, crs = 6454)

# st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.gpkg", layer = "parcels") 
