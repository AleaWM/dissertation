# q2_dissertation_00_prep_data.R
# Purpose: Prepare the Q2 dissertation analysis dataset used by downstream scripts.
# Run from the project root.

library(tidyverse)
library(readxl)

options(scipen = 999)

out_dir <- "outputs/q2_marginaleffects"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source("R/helper_pins_to_drop.R")

lake100_path <- "data/processed/parcels_2025_lakeMI_within100ft_ExportTable_TableToExcel.xlsx"
lake500_path <- "data/processed/parcels_2025_lakeMI_within500ft_ExportTable_TableToExcel.xlsx"
data_path <- "data/processed/targets/sales/df_prep_bldg_v2026_03.RDS"

lake100ft <- readxl::read_xlsx(lake100_path)
lake500ft <- readxl::read_xlsx(lake500_path)

bldg_sfha <- readRDS(data_path)

bldg_sfha <- bldg_sfha |>
  filter(
    !pin10 %in% drop_parcels |
      (pin10 == "0113301013" & sale_year > 2014)
  ) |>
  group_by(pin) |>
  mutate(
    ever_added_eff = any(addedto_eff_sfha == TRUE, na.rm = TRUE),
    ever_removed_eff = any(removedfrom_eff_sfha == TRUE, na.rm = TRUE),
  ) |>
  ungroup() |>
  mutate(
    event = sale_date > new_info_released,
    highff_post = high_ff_score == TRUE & sale_date > new_info_released,
    Triad = as.character(Triad),
    triad_coast = if_else(pin10 %in% lake100ft$PIN10, "Coastal", Triad),
    ff_score_ord = factor(env_flood_fs_factor, levels = 1:10),
    eff_sfha_category = case_when(
      ever_added_eff & !ever_removed_eff ~ "Added to eff SFHA",
      ever_removed_eff & !ever_added_eff ~ "Removed from eff SFHA",
      change_type == "Always SFHA" ~ "Always SFHA",
      change_type == "Never SFHA" ~ "Never SFHA",
      TRUE ~ "CHECK ME!")
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

make_control_groups <- function(df,
                                ever_added,
                                ever_removed,
                                pre_eff_change_type,
                                type = c("pre", "eff")) {
  type <- match.arg(type)

  group_T_A <- df |>
    filter({{ ever_added }} == TRUE)

  group_T_B <- df |>
    filter({{ ever_removed }} == TRUE)

  if (type == "pre") {
    group_C_A <- df |>
      filter({{ pre_eff_change_type }} == "Never SFHA" & pre_date_chr %in% c("2005-01-01"))
  } else {
    group_C_A <- df |>
      filter({{ pre_eff_change_type }} == "Never SFHA" & eff_date_chr %in% c("2008-08-19"))
  }

  group_C_B <- df |>
    filter(
      {{ pre_eff_change_type }} == "Never SFHA" &
        (
          eff_date_chr %in% c("2019-11-01", "2021-09-10") |
            pre_date_chr %in% c("2015-02-12", "2019-07-01", "2021-09-22")
        )
    )

  if (type == "pre") {
    group_C_C <- df |>
      filter({{ pre_eff_change_type }} == "Always SFHA" & pre_date_chr %in% c("2005-01-01"))

    group_C_D <- df |>
      filter({{ pre_eff_change_type }} == "Always SFHA" & pre_date_chr %in% c("2015-02-12", "2019-07-01", "2021-09-22"))
  } else {
    group_C_C <- df |>
      filter({{ pre_eff_change_type }} == "Always SFHA" & eff_date_chr %in% c("2008-08-19"))

    group_C_D <- df |>
      filter({{ pre_eff_change_type }} == "Always SFHA" & eff_date_chr %in% c("2019-11-01", "2021-09-10"))
  }

  list(
    group_T_A = group_T_A,
    group_T_B = group_T_B,
    group_C_A = group_C_A,
    group_C_B = group_C_B,
    group_C_C = group_C_C,
    group_C_D = group_C_D
  )
}

groups_prelim <- make_control_groups(
  df = bldg_sfha,
  ever_added = ever_added_prelim,
  ever_removed = ever_removed_prelim,
  pre_eff_change_type = change_type_prelim,
  type = "pre"
)

groups_effective <- make_control_groups(
  df = bldg_sfha,
  ever_added = ever_added_eff,
  ever_removed = ever_removed_eff,
  pre_eff_change_type = change_type,
  type = "eff"
)


groups_prelim_long <- imap_dfr(groups_prelim, ~ mutate(.x, group = .y))
groups_eff_long <- imap_dfr(groups_effective, ~ mutate(.x, group = .y))

saveRDS(bldg_sfha, file.path(out_dir, "bldg_sfha_q2_dissertation_prepped.rds"))
saveRDS(groups_prelim, file.path(out_dir, "groups_prelim.rds"))
saveRDS(groups_effective, file.path(out_dir, "groups_effective.rds"))

saveRDS(groups_prelim_long, file.path(out_dir, "groups_prelim_long.rds"))
saveRDS(groups_eff_long, file.path(out_dir, "groups_eff_long.rds"))

saveRDS(lake100ft, file.path(out_dir, "lake100ft.rds"))
saveRDS(lake500ft, file.path(out_dir, "lake500ft.rds"))

message("Saved prepared dissertation Q2 data to: ", out_dir)
