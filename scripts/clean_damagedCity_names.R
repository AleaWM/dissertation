# Clean Individual Assistance City Names
#

library(tidyverse)
library(modelsummary)

ind_assist <- read_csv("./data/raw/indiv_assistance_V2_CookCounty.csv") |> 
  filter(incidentTypeCode != "B" & disasterNumber != "1763") |>
  mutate(year = str_sub(declarationDate, 1, 4),
         year = as.numeric(year),
         joinyear = ifelse(
           year <= 2010, 2000, 
           ifelse(year < 2020 & year > 2010, 2010,
                  ifelse(year > 2020, 2020, year))),
         tract = str_sub(censusGeoid, 6, 9)) |>
  mutate(tract = str_pad(tract, width = 6, pad = "0", side = "right"))

# all disasters in Cook County, IL
cook_assist <- ind_assist %>% filter(county == "Cook (County)")

# # Just Flood events in 2023
# ind_assist <- ind_assist %>% 
#   filter(county == "Cook (County)" & disasterNumber %in% c("4728", "4749") )

# Include other big flood events:
ind_assist <- ind_assist %>% 
  filter(county == "Cook (County)" & disasterNumber %in% c("4728", "4749", "1935", "4116") )

table(ind_assist$joinyear)
#table(ind_assist$damagedZipCode[ind_assist$disasterNumber=="1935"])
#table(ind_assist$damagedZipCode[ind_assist$disasterNumber=="4728"])


cities <- cook_assist |> distinct(damagedCity)
# export CSV of messy names to make chatGPT identify ones that should be recoded
# cities |> write_csv("./data/raw/ia_city_names.csv") 

names <- read_csv("./data/processed/City_Name_Mapping.csv")

cook_assist <- cook_assist |> left_join(names)

table(cook_assist$damagedCity[ind_assist$disasterNumber=="4728"])

table(cook_assist$clean_name[ind_assist$disasterNumber=="4728"])

write_csv("./data/processed/indiv_assistance_V2_CookCounty_wNames.csv")
  
