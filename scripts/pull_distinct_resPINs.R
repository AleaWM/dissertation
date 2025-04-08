# ----
# Purpose: Pull all PINs with residential classification at any time from 2006–2023 and get associated assessed values
# Input(s): PTAXSIM database (local, downloaded from CCAO's website)
# Output(s): 
#    data/raw/muniname_taxcode_key.csv, 
#    data/raw/residential_pins_ever.csv, 
#    data/raw/pin_muni_key.csv
# Last updated: 2025-04-06
# ----

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)



# Pull and Prep Data ------------------------------------------------------

is.integer64 <- function(x){
  class(x)=="integer64"
}

# Instantiate DB connection.
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/aleaw/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")

## Pull Muni Taxing Agency Names from agency_info table
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
  "
)


## Pulls ALL distinct PINs that existed between 2006 and 2023 in munis
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB

tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, tax_code_num, tax_code_rate
  FROM tax_code
           ",
           .con = ptaxsim_db_conn
  ))

muni_names_key <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT tax_code_num, agency_num
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
           ",
           .con = ptaxsim_db_conn
  ))


muni_names_key <- left_join(muni_names_key, muni_agency_names) 

write_csv(muni_names_key, "./data/raw/muniname_taxcode_key.csv")


# 2,545,704
distinct_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin, tax_code_num
  FROM pin
  ",
    .con = ptaxsim_db_conn
  ))

distinct_pins_ever <- distinct_pins_ever |> left_join(muni_names_key) |> select(-minor_type)
distinct_pins_ever <- distinct_pins_ever |> distinct(pin, agency_num, agency_name)

# were any pins in more than one municipality? Yes :(
distinct_pins_ever |> summarize(distinct_pins = n_distinct(pin)) 

dups <- distinct_pins_ever |> group_by(pin) |>
 # summarize(agency_count = n_distinct(agency_name))  |> # find the pins with more than one agency name
  filter(n_distinct(agency_num) > 1)

# Note to self: if pin's muni was NA and thne got a muni name, 
# I include it in that municipality for all year
dups2 <- distinct_pins_ever |> 
  filter(!is.na(agency_name)) |> 
  group_by(pin) |>
  filter(n_distinct(agency_num) > 1)

dups3 <- distinct_pins_ever |> filter(pin %in% dups2$pin)
distinct_pins_ever <- distinct_pins_ever |> filter(!pin %in% dups2$pin)

# 1,994,706 <- 1,997,577
distinct_pins_ever <- distinct_pins_ever |> group_by(pin) |>
  summarize(agency_name = last(agency_name), 
            agency_num = last(agency_num))

write_csv(distinct_pins_ever, "./data/raw/pin_muni_key.csv")


# 1,643,662 PINs in municipalities
distinct_res_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin
  FROM pin
  WHERE class > 199 AND class < 300
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

distinct_res_pins |> filter(pin %in% dups$pin) |> n_distinct()
# 1382 pins were residential at some time and changed munis or became incorporated.
distinct_res_pins |> filter(pin %in% dups2$pin) |> n_distinct()
# 182 pins in multiple munis, unresolved: worry about later. 
# Ideally merging taxcode & muni names to the yearly pin data will address this and accurately reflect changing municipality location.
#  don't even know if this is a big deal yet. just making notes.

write_csv(res_pins_ever, "./data/raw/residential_pins_ever.csv")

