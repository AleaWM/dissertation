library(tidyverse)
library(openxlsx)
buyouts <- read.xlsx("data/processed/BuyoutPoints_Statewide.xlsx")


res_pins_ever <- read_csv("./data/raw/residential_pins_ever.csv")


buyouts <- buyouts |> filter(County == "Cook") |>
  select(-c(Parcel_Number4, Parcel_Number5, FirstFloorHt,Content_Cost, Abestos_Cost, Mgmt_Cost, Status)) |>
  pivot_longer(cols = c(Parcel_Number2:Parcel_Number3), names_to = "multiparcel") |>
  mutate(pin = str_remove_all(Parcel_Number, "-"),
         pin = str_pad(pin, width = 14, side = "right", pad = "0"))

buyout_pins <- buyouts |> filter(AWM.comments != "Not bought out")
  
ptax_buyoutpins <- res_pins_ever |> 
  filter(pin %in% buyout_pins$pin) 


# PINs that were actually bought out
ptax_buyoutpins |> ggplot() + geom_line(aes(x=year, y=av_clerk, group = pin))

# PINs that were actually bought out & the ones that were supposed to be bought out.
res_pins_ever |> 
  filter(pin %in% buyouts$pin) |> ggplot() + geom_line(aes(x=year, y=av_clerk, group = pin))
