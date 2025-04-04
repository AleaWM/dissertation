# Pull unique PIN and parcel shapefiles for all years into one list.

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
  WHERE class < 400 OR class > 899
  ",
    .con = ptaxsim_db_conn
  ))

all_parcels <- all_pins |> 
  mutate(pin = str_pad(pin, 14, side = "left", pad = "0"),
  pin10 = str_sub(pin, 1, 10)) |>
  distinct(pin10) 

years <- c(2023)
fh_pins_geo <- lookup_pin10_geometry(year = years, pin10 = all_parcels$pin10)
library(sf)

# Convert character WKT geometry to sf object
sf_data <- fh_pins_geo |>
  st_as_sf(wkt = "geometry", crs = 6465)

# Set the correct CRS if missing (update EPSG code as needed)
st_crs(sf_data) <- 6454  # Projection for Illinois. Makes skinny Cook.

st_layers(sf_data)

st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.shp", layer = "parcels", driver = "ESRI Shapefile") 
st_write(sf_data, "./data/raw/parcel_shapefiles_ty2023.gpkg", layer = "parcels") 
