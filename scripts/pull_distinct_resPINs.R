### Helper File: Pull all Comm. and Ind. PINs in Cook County ###
### Years 2006 - 2022 ###
### Two multiple CSVs:
####   one has all PINs all years, one only has PINs that existed each year.

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)


# Pull and Prep Data ------------------------------------------------------

is.integer64 <- function(x){
  class(x)=="integer64"
}

# Instantiate DB connection.
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")

## Pull Muni Taxing Agency Names from agency_info table
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
  "
)

## PINS in UNINCORPORATED AREAS ARE NOT included in this data pull!!!

## Pulls ALL distinct PINs that existed between 2006 and 2023 in munis
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB
## 1,661,125 PINs when only including classes 400 to 899

tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, tax_code_num, tax_code_rate
  FROM tax_code
           ",
           .con = ptaxsim_db_conn
  ))


# 1,643,662 PINs in municipalities
distinct_res_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin
  FROM pin
  WHERE class > 199 AND class < 300
  AND tax_code_num IN ({tax_codes$tax_code_num*})
  ",
    .con = ptaxsim_db_conn
  ))


res_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_res_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate_if(is.integer64, as.double ) %>%
  mutate(class = as.character(class)) 

n_distinct(res_pins_ever$pin) # 1,708,501 PINs that were residential at some point in time
