# q2_dissertation_04_robustness_models.R
# Purpose: Estimate Q2 robustness models by Triad and save tables.
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

robust_prelim_high_ff_models <- list(
  "South" = feols(
    log_price ~ prelim_sfha_category * high_ff_score * event |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "South"),
    cluster = ~pin10
  ),
  "North" = feols(
    log_price ~ prelim_sfha_category * high_ff_score * event |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "North"),
    cluster = ~pin10
  ),
  "City" = feols(
    log_price ~ prelim_sfha_category * high_ff_score * event |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "City"),
    cluster = ~pin10
  )
)

robust_prelim_highff_post_models <- list(
  "South" = feols(
    log_price ~ prelim_sfha_category * highff_post |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "South"),
    cluster = ~pin10
  ),
  "North" = feols(
    log_price ~ prelim_sfha_category * highff_post |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "North"),
    cluster = ~pin10
  ),
  "City" = feols(
    log_price ~ prelim_sfha_category * highff_post |
      pin + sale_year,
    data = bldg_sfha |> filter(Triad == "City"),
    cluster = ~pin10
  )
)

dropped_terms_robust_prelim_high_ff <- flag_giant_se_terms(robust_prelim_high_ff_models)
bad_terms_robust_prelim_high_ff <- make_bad_terms_regex(dropped_terms_robust_prelim_high_ff)

robust_prelim_high_ff_table <- modelsummary(
  robust_prelim_high_ff_models,
  output = "gt",
  fmt = 3,
  # coef_omit = bad_terms_robust_prelim_high_ff,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC",
  title = "Q2 robustness: preliminary SFHA status, high Flood Factor, and post-release period by Triad"
)

dropped_terms_robust_prelim_highff_post <- flag_giant_se_terms(robust_prelim_highff_post_models)
bad_terms_robust_prelim_highff_post <- make_bad_terms_regex(dropped_terms_robust_prelim_highff_post)

robust_prelim_highff_post_table <- modelsummary(
  robust_prelim_highff_post_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_robust_prelim_highff_post,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC",
  title = "Q2 robustness: preliminary SFHA status and HighFF × Post by Triad"
)

save_diagnostics(dropped_terms_robust_prelim_high_ff, diagnostics_dir, "dropped_terms_robust_prelim_high_ff")
save_diagnostics(dropped_terms_robust_prelim_highff_post, diagnostics_dir, "dropped_terms_robust_prelim_highff_post")

saveRDS(robust_prelim_high_ff_models, file.path(out_dir, "robust_prelim_high_ff_models.rds"))
saveRDS(robust_prelim_highff_post_models, file.path(out_dir, "robust_prelim_highff_post_models.rds"))
saveRDS(robust_prelim_high_ff_table, file.path(out_dir, "robust_prelim_high_ff_table.rds"))
saveRDS(robust_prelim_highff_post_table, file.path(out_dir, "robust_prelim_highff_post_table.rds"))

gt::gtsave(robust_prelim_high_ff_table, file.path(table_dir, "robust_prelim_high_ff_table.html"))
gt::gtsave(robust_prelim_highff_post_table, file.path(table_dir, "robust_prelim_highff_post_table.html"))

message("Saved Q2 robustness models and tables to: ", out_dir)
