# q2_01_run_ff_models.R
# Purpose: Estimate Q2 Flood Factor models and save model objects, cleaned tables, and dropped-term diagnostics.
# Run from the project root after q2_00_prep_data.R.

library(tidyverse)
library(fixest)
library(broom)
library(modelsummary)
library(gt)

options(scipen = 999)

out_dir <- "outputs/q2_models"
table_dir <- file.path(out_dir, "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

prep_path <- file.path(out_dir, "bldg_sfha_q2_prepped.rds")
if (!file.exists(prep_path)) {
  source("scripts/q2_00_prep_data.R")
}

bldg_sfha <- readRDS(prep_path)

make_bad_terms <- function(models, se_threshold = 1000) {
  models |>
    purrr::map_dfr(~ broom::tidy(.x), .id = "model") |>
    filter(std.error > se_threshold | is.na(std.error))
}

make_bad_terms_regex <- function(dropped_terms) {
  bad_terms <- dropped_terms |>
    distinct(term) |>
    pull(term)

  if (length(bad_terms) > 0) {
    paste0(
      "^(",
      paste(stringr::str_escape(bad_terms), collapse = "|"),
      ")$"
    )
  } else {
    "$^" # matches nothing
  }
}

# -----------------------------------------------------------------------------
# Binary high Flood Factor models
# -----------------------------------------------------------------------------

binary_models <- list(
  "High FF × Post" = feols(
    log_price ~ high_ff_score * event |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "High FF × Post, identified term only" = feols(
    log_price ~ event + i(event, high_ff_score, ref = FALSE) |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  )
)

binary_table <- modelsummary(
  binary_models,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|RMSE",
  output = "gt"
)

gt::gtsave(binary_table, file.path(table_dir, "binary_ff_models.html"))
saveRDS(binary_models, file.path(out_dir, "binary_ff_models.rds"))
saveRDS(binary_table, file.path(out_dir, "binary_ff_table.rds"))

# -----------------------------------------------------------------------------
# Ordinal Flood Factor models
# -----------------------------------------------------------------------------

ordinal_data <- bldg_sfha |>
  filter(change_type != "Changes SFHA")

ordinal_models <- list(
  "Ordinal" = feols(
    log_price ~ ff_score_ord * event |
      pin + sale_year,
    vcov = ~pin10,
    data = ordinal_data
  ),

  "Ordinal × Triad" = feols(
    log_price ~ ff_score_ord * event * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = ordinal_data
  ),

  "Change Type × Ordinal × Event × Triad" = feols(
    log_price ~ change_type * ff_score_ord * event * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = ordinal_data
  ),

  "Change Type × Ordinal × Event" = feols(
    log_price ~ change_type * ff_score_ord * event |
      pin + sale_year,
    vcov = ~pin10,
    data = ordinal_data
  )
)

dropped_terms_ordinal <- make_bad_terms(ordinal_models)
bad_terms_regex_ordinal <- make_bad_terms_regex(dropped_terms_ordinal)

ordinal_table <- modelsummary(
  ordinal_models,
  stars = TRUE,
  coef_omit = bad_terms_regex_ordinal,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|RMSE",
  output = "gt"
)

gt::gtsave(ordinal_table, file.path(table_dir, "ordinal_ff_models.html"))
saveRDS(ordinal_models, file.path(out_dir, "ordinal_ff_models.rds"))
saveRDS(ordinal_table, file.path(out_dir, "ordinal_ff_table.rds"))
saveRDS(dropped_terms_ordinal, file.path(out_dir, "ordinal_dropped_terms.rds"))
readr::write_csv(dropped_terms_ordinal, file.path(out_dir, "ordinal_dropped_terms.csv"))

# -----------------------------------------------------------------------------
# Continuous Flood Factor models
# -----------------------------------------------------------------------------

continuous_models <- list(
  "Continuous" = feols(
    log_price ~ env_flood_fs_factor * event |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "Continuous × Triad" = feols(
    log_price ~ env_flood_fs_factor * event * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "Change Type × Continuous × Event × Triad" = feols(
    log_price ~ change_type * env_flood_fs_factor * event * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "Change Type × Continuous × Event" = feols(
    log_price ~ change_type * env_flood_fs_factor * event |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  )
)

dropped_terms_continuous <- make_bad_terms(continuous_models)
bad_terms_regex_continuous <- make_bad_terms_regex(dropped_terms_continuous)

continuous_table <- modelsummary(
  continuous_models,
  stars = TRUE,
  coef_omit = bad_terms_regex_continuous,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|RMSE",
  output = "gt"
)

gt::gtsave(continuous_table, file.path(table_dir, "continuous_ff_models.html"))
saveRDS(continuous_models, file.path(out_dir, "continuous_ff_models.rds"))
saveRDS(continuous_table, file.path(out_dir, "continuous_ff_table.rds"))
saveRDS(dropped_terms_continuous, file.path(out_dir, "continuous_dropped_terms.rds"))
readr::write_csv(dropped_terms_continuous, file.path(out_dir, "continuous_dropped_terms.csv"))

message("Saved Q2 model objects, tables, and dropped-term diagnostics to: ", out_dir)
