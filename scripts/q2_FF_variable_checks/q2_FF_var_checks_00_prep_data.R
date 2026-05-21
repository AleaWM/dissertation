# q2_00_prep_data.R
# Purpose: Prepare the Q2 Flood Factor analysis dataset and save it for downstream model scripts.
# Run from the project root.

library(tidyverse)
library(readxl)

options(scipen = 999)

out_dir <- "outputs/q2_models"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- "data/processed/targets/sales/df_prep_bldg_v2026_03.RDS"
lake100_path <- "data/processed/parcels_2025_lakeMI_within100ft_ExportTable_TableToExcel.xlsx"
lake500_path <- "data/processed/parcels_2025_lakeMI_within500ft_ExportTable_TableToExcel.xlsx"

source("R/helper_pins_to_drop.R")

lake100ft <- readxl::read_xlsx(lake100_path)
lake500ft <- readxl::read_xlsx(lake500_path)

bldg_sfha <- readRDS(data_path)

bldg_sfha <- bldg_sfha |>
  filter(
    !pin10 %in% drop_parcels |
      (pin10 == "0113301013" & sale_year > 2014)
  ) |>
  mutate(
    event = sale_date > new_info_released,
    highff_post = high_ff_score == TRUE & sale_date > new_info_released,
    Triad = as.character(Triad),
    triad_coast = if_else(pin10 %in% lake100ft$PIN10, "Coastal", Triad),
    ff_score_ord = factor(env_flood_fs_factor, levels = 1:10)
  ) |>
  group_by(pin) |>
  mutate(
    pin_num = as.numeric(pin),
    ff_group = if_else(high_ff_score == TRUE & sale_date > new_info_released, "2020", "0")
  ) |>
  ungroup()

if (!"log_price" %in% names(bldg_sfha)) {
  bldg_sfha <- bldg_sfha |>
    mutate(log_price = log(sale_price))
}

saveRDS(bldg_sfha, file.path(out_dir, "bldg_sfha_q2_prepped.rds"))
saveRDS(lake100ft, file.path(out_dir, "lake100ft.rds"))
saveRDS(lake500ft, file.path(out_dir, "lake500ft.rds"))

message("Saved prepared Q2 data to: ", out_dir)
