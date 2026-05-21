
library(tidyverse)
library(fixest)
library(did)
library(etwfe)
library(tidyverse)
library(modelsummary)
library(gt)
library(marginaleffects)

options(scipen = 999)

source("R/helper_pins_to_drop.R")


# lake100ft <- readxl::read_xlsx("data/processed/parcels_2025_lakeMI_within100ft_ExportTable_TableToExcel.xlsx")
# lake500ft <- readxl::read_xlsx("data/processed/parcels_2025_lakeMI_within500ft_ExportTable_TableToExcel.xlsx")

bldg_sfha <- readRDS("data/processed/targets/sales/df_prep_bldg_v2026_03.RDS")

bldg_sfha <- bldg_sfha |> filter(!pin10 %in% drop_parcels  |
  (pin10 == "0113301013" & sale_year > 2014)
)

make_control_groups <- function(df,
                                ever_added,
                                ever_removed,
                                pre_eff_change_type,
                                type = c("pre", "eff")
) {
  # Treated Group A: Added to Risk Zone
  group_T_A <- df |>
    filter({{ ever_added }} == TRUE)

  # Treated Group B: Removed from Risk Zone
  group_T_B <- df |>
    filter({{ ever_removed }}  == TRUE)

  # Control Group A:  Never in SFHA, FIRM panels never updated

  if (type == "pre") {
    group_C_A <- df |>
      filter(
        {{ pre_eff_change_type }} == "Never SFHA" &
          pre_date_chr %in% c("2005-01-01")
      )
  } else if (type == "eff") {  # for effective maps
    group_C_A <- df |>
      filter(
        {{ pre_eff_change_type }} == "Never SFHA" &
          eff_date_chr %in% c("2008-08-19")
      )
  }

  # Control Group B: Never in SFHA, FIRM Panels are updated
  group_C_B  <- df |>
    filter(
      {{ pre_eff_change_type }} == "Never SFHA" &
        (eff_date_chr %in% c("2019-11-01", "2021-09-10") |
          pre_date_chr %in% c("2015-02-12", "2019-07-01", "2021-09-22")
        )
    )

  # Control Group C: Always in SFHA, FIRM Panels are not updated
  if (type == "pre") {
    group_C_C <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          pre_date_chr %in% c("2005-01-01")
      )
  } else if (type == "eff") {  # this probably is tainted by prelim map cohort in 2021 though....
    group_C_C <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          (eff_date_chr %in% c("2008-08-19"))
      )

    group_C_C_alt <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          (eff_date_chr %in% c("2008-08-19") |
            pre_date_chr %in% c("2005-01-01")
          )
      )
  }

  # Always in SFHA but FIRM panels have been recently updated

  if (type == "eff") {
    group_C_D <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          (eff_date_chr %in% c("2019-11-01", "2021-09-10"))
      )

    group_C_D_alt <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          (eff_date_chr %in% c("2019-11-01", "2021-09-10") |
            pre_date_chr %in% c("2015-02-12", "2019-07-01", "2021-09-22")
          )
      )
  } else if (type == "pre") {
    group_C_D <- df |>
      filter(
        {{ pre_eff_change_type }} == "Always SFHA" &
          pre_date_chr %in% c("2015-02-12", "2019-07-01", "2021-09-22")
      )
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

groups <- make_control_groups(df = bldg_sfha,
  ever_added = ever_added_prelim,
  ever_removed = ever_removed_prelim,
  pre_eff_change_type = change_type_prelim,
  type = "pre")

groups_long <- imap_dfr(groups, ~ {
  .x |>
    mutate(group = .y)
})

bldg_sfha <- bldg_sfha |> mutate(event = sale_date > new_info_released)

bldg_sfha <- bldg_sfha |> mutate(highff_post = high_ff_score == TRUE &  sale_date > new_info_released)

bldg_sfha <- bldg_sfha |>
  group_by(pin) |>
  mutate(pin_num = as.numeric(pin),
    # ff_group = ifelse(high_ff_score==TRUE & sale_date > new_info_released, as.character(min(sale_year[sale_date > new_info_released])), "0")) |>
    ff_group = ifelse(high_ff_score == TRUE & sale_date > new_info_released, "2020", "0")) |>

  ungroup()




## change type is for effective maps
triad_mod_3way <- feols(log(sale_price) ~ highff_post * change_type *  Triad | pin + sale_year, vcov = ~pin10, data = bldg_sfha)


compared_by2variables <- comparisons(triad_mod_3way,
  variables = "highff_post",
  by = c("change_type", "Triad"),
  hypothesis = ~pairwise)

write_rds(compared_by2variables, "outputs/compared_by2variables.rds")
results <- read_rds("outputs/compared_by2variables.rds")
results
