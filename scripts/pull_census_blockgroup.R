# ----
# Purpose: Pull Cook County parcel universe PINs with census block group IDs to join to FEMA individual assistance data
# Input(s): CCAO API Parcel data with PINs, Flood Factor 2019 snapshot, SFHA indicator
# Output(s): data/raw/census_blockgroups.csv
# Last updated: 2025-23-11
# ----

## Pull list of PINs with any First Street Flood Rating Scores and SFHA indicators

library(tidyverse)
library(data.table)
library(jsonlite)
library(glue)
library(httr)


# API Endpoint
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"


puni_pins <- GET(
  base_url,
  query = list(
    `$select` = "DISTINCT pin, census_place_geoid, census_block_group_geoid",
    `$where` = "year > 2018 AND census_block_group_geoid IS NOT NULL",
    `$limit` = 5000000L
  )
)

# 2,050,157 observations pulled from API call
puni_pins <- fromJSON(rawToChar(puni_pins$content))

# 1,886,908 distinct pins 
n_distinct(puni_pins$pin) 
n_distinct(puni_pins$census_block_group_geoid)

## Some PINs have two geoIDs:
write_csv(puni_pins, "./data/raw/census_blockgroups.csv")

