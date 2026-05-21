# q2_dissertation_01_basic_twfe_models.R
# Purpose: Estimate and save the basic TWFE models from dissertation_tables_Q2_marginaleffects.qmd.
# Run from the project root after q2_dissertation_00_prep_data.R.

library(tidyverse)
library(fixest)
library(modelsummary)
library(gt)
library(broom)
library(stringr)

source("scripts/q2_dissertation_helpers.R")

options(scipen = 999)

out_dir <- "outputs/q2_marginaleffects"
table_dir <- file.path(out_dir, "tables")
diagnostics_dir <- file.path(out_dir, "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

prep_path <- file.path(out_dir, "bldg_sfha_q2_dissertation_prepped.rds")
if (!file.exists(prep_path)) {
  source("scripts/q2_dissertation_00_prep_data.R")
}

bldg_sfha <- readRDS(prep_path)

# Effective map status models --------------------------------------------------

basic_twfe_effective_models <- list(
  "County" = feols(
    log_price ~ change_type * high_ff_score * event |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  ),
  "Triad Interactions" = feols(
    log_price ~ change_type * high_ff_score * event * Triad |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  )
)

dropped_terms_basic_twfe_effective <- flag_giant_se_terms(basic_twfe_effective_models)
bad_terms_basic_twfe_effective <- make_bad_terms_regex(dropped_terms_basic_twfe_effective)

basic_twfe_effective_table <- modelsummary(
  basic_twfe_effective_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_basic_twfe_effective,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC"
)

# Preliminary map status models ----------------------------------------------

basic_twfe_prelim_models <- list(
  "County" = feols(
    log_price ~ prelim_sfha_category * high_ff_score * event |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  ),
  "Triad Interactions" = feols(
    log_price ~ prelim_sfha_category * high_ff_score * event * Triad |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  )
)

dropped_terms_basic_twfe_prelim <- flag_giant_se_terms(basic_twfe_prelim_models)
bad_terms_basic_twfe_prelim <- make_bad_terms_regex(dropped_terms_basic_twfe_prelim)

basic_twfe_prelim_table <- modelsummary(
  basic_twfe_prelim_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_basic_twfe_prelim,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC"
)

# Combined indicator and preliminary SFHA status ------------------------------

basic_twfe_highff_prelim_models <- list(
  "County" = feols(
    log_price ~ highff_post * change_type_prelim |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  ),
  "Triad Interactions" = feols(
    log_price ~ highff_post * change_type_prelim * Triad |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  )
)

dropped_terms_basic_twfe_highff_prelim <- flag_giant_se_terms(basic_twfe_highff_prelim_models)
bad_terms_basic_twfe_highff_prelim <- make_bad_terms_regex(dropped_terms_basic_twfe_highff_prelim)

basic_twfe_highff_prelim_table <- modelsummary(
  basic_twfe_highff_prelim_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_basic_twfe_highff_prelim,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC"
)

# Save outputs -----------------------------------------------------------------

saveRDS(basic_twfe_effective_models, file.path(out_dir, "basic_twfe_effective_models.rds"))
saveRDS(basic_twfe_prelim_models, file.path(out_dir, "basic_twfe_prelim_models.rds"))
saveRDS(basic_twfe_highff_prelim_models, file.path(out_dir, "basic_twfe_highff_prelim_models.rds"))

save_diagnostics(dropped_terms_basic_twfe_effective, diagnostics_dir, "dropped_terms_basic_twfe_effective")
save_diagnostics(dropped_terms_basic_twfe_prelim, diagnostics_dir, "dropped_terms_basic_twfe_prelim")
save_diagnostics(dropped_terms_basic_twfe_highff_prelim, diagnostics_dir, "dropped_terms_basic_twfe_highff_prelim")

saveRDS(basic_twfe_effective_table, file.path(out_dir, "basic_twfe_effective_table.rds"))
saveRDS(basic_twfe_prelim_table, file.path(out_dir, "basic_twfe_prelim_table.rds"))
saveRDS(basic_twfe_highff_prelim_table, file.path(out_dir, "basic_twfe_highff_prelim_table.rds"))

gt::gtsave(basic_twfe_effective_table, file.path(table_dir, "basic_twfe_effective_table.html"))
gt::gtsave(basic_twfe_prelim_table, file.path(table_dir, "basic_twfe_prelim_table.html"))
gt::gtsave(basic_twfe_highff_prelim_table, file.path(table_dir, "basic_twfe_highff_prelim_table.html"))

message("Saved basic TWFE models and tables to: ", out_dir)
