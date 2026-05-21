# descriptive stats in dissertation text

puniverse <- read_csv("data/raw/parcel_universe_currentyear.csv")

# doesn't have class for pins =/
ff_scores <- read_csv("data/processed/floodfactor_scores.csv")

# only has manually coded FF scores
# ff_scores <- read_csv("data/processed/pins_with_some_addresses.csv")

res_pins <- read_csv("data/raw/residential_pins_ever.csv") |> filter(year == 2023) |> distinct(pin, class)

res_pins <- res_pins |> left_join(ff_scores)

res_sales <- read_rds("data/processed/res_sales.rds") |>  group_by(pin) |> summarize(year = max(year))

recent_res_sales <- res_sales |> filter(year > 2019)

pins <- left_join(res_pins, ff_scores)



# Code for when Class was a variable in the API pull:
pins |>
  filter(env_flood_fs_factor > 4 & class > 199 & class < 300) |> count()


# share of residential properties that have high FF scores:
196423 / 1476068
198309 / 1604301

# how many properties sold recently that were high risk already?
pins |>
  filter(env_flood_fs_factor > 4 & pin %in% recent_res_sales$pin) |> distinct(pin)
116957 # residential pins sold 1+ times
48232 # sold recently

# how many high risk properties have yet to capitalize the flood risk?
48232 / 198000 # 24% might have so far


puni_pins_coded <- pins |>
  mutate(
    # SFHA = ifelse(env_flood_fema_sfha == TRUE, TRUE, FALSE),
    FFS_4plus = ifelse(env_flood_fs_factor >= 4, TRUE, FALSE),
    FFS_5plus = ifelse(env_flood_fs_factor >= 5, TRUE, FALSE),
    FFS_6plus = ifelse(env_flood_fs_factor >= 6, TRUE, FALSE),
    Res_C2 = ifelse(class > 199 & class < 300, TRUE, FALSE),
  )

puni_pins_coded |>
  group_by(Res_C2) |>
  summarize(
    FFS_4plus = sum(FFS_4plus, na.rm = TRUE),
    FFS_5plus = sum(FFS_5plus, na.rm = TRUE),
    FFS_6plus = sum(FFS_6plus, na.rm = TRUE),
    n = n()) |> View()

198309 / 1586893



city_names <- read_csv("./data/processed/City_Name_Mapping.csv")

ind_assist <- read_csv("./data/raw/indiv_assistance_V2_CookCounty.csv") |>
  filter(incidentTypeCode != "B" & disasterNumber != "1763") |>
  mutate(year = str_sub(declarationDate, 1, 4),
    year = as.numeric(year),

    # for census shapefiles later
    joinyear = ifelse(
      year <= 2010, 2000,
      ifelse(year < 2020 & year > 2010, 2010,
        ifelse(year > 2020, 2020, year))),
    tract = str_sub(censusGeoid, 6, 9)
  ) |>
  mutate(tract = str_pad(tract, width = 6, pad = "0", side = "right")) |>
  # bring in file of dirty names matched with clean names
  left_join(city_names)

# all disasters in Cook County, IL
cook_assist <- ind_assist %>% filter(county == "Cook (County)" & disasterNumber != "4819")

# # Just Flood events in 2023
# ind_assist <- ind_assist %>%
#   filter(county == "Cook (County)" & disasterNumber %in% c("4728", "4749") )

# Include other big flood events:
ind_assist <- ind_assist %>%
  filter(county == "Cook (County)" & disasterNumber %in% c("4728", "4749" # , "1935", "4116"
  ))


ind_assist %>%
  filter(disasterNumber %in% c(4728, 4749)) |>

  filter(ihpAmount > 0) |>
  group_by(disasterNumber, floodInsurance) %>%
  summarize(claimcount = n(),
    # ihpAmount = round(sum(ihpAmount, na.rm=TRUE)),
  )  %>%
  arrange(desc(claimcount)) %>%
  pivot_wider(values_from = claimcount, names_from = floodInsurance) %>%
  arrange(desc(`TRUE`)) |>
  DT::datatable(rownames = FALSE)
