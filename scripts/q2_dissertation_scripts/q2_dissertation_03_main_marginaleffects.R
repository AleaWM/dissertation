# q2_dissertation_03_main_marginaleffects.R
# Purpose: Estimate the main Q2 models and save marginal effects/comparisons.
# Run from the project root after q2_dissertation_00_prep_data.R.

library(tidyverse)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(gt)
library(broom)
library(stringr)

source("scripts/q2_dissertation_helpers.R")

options(scipen = 999)

out_dir <- "outputs/q2_marginaleffects"
me_dir <- file.path(out_dir, "marginaleffects")
table_dir <- file.path(out_dir, "tables")
diagnostics_dir <- file.path(out_dir, "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(me_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

prep_path <- file.path(out_dir, "bldg_sfha_q2_dissertation_prepped.rds")
if (!file.exists(prep_path)) {
  source("scripts/q2_dissertation_00_prep_data.R")
}

bldg_sfha <- readRDS(prep_path)

# Main models ------------------------------------------------------------------

q2_main_models <- list(
  "Post × HighFF" = feols(
    log_price ~ high_ff_score * event |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),


  "Post × HighFF × SFHA" = feols(
    log_price ~ change_type * high_ff_score * event |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "Post × HighFF × Triad × SFHA" = feols(
    log_price ~ change_type * high_ff_score * event * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  ),

  "Post × HighFF × Triad" = feols(
    log_price ~ high_ff_score * event  * Triad |
      pin + sale_year,
    vcov = ~pin10,
    data = bldg_sfha
  )
)

dropped_terms_q2_main <- flag_giant_se_terms(q2_main_models)
bad_terms_q2_main <- make_bad_terms_regex(dropped_terms_q2_main)

q2_main_table <- modelsummary(
  q2_main_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_q2_main,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC"
)

# Marginal effects/comparisons from original exploratory QMD -------------------

# no sfha or tria_ _ontrols
me_way_event_sequential_simp <- avg_comparisons(
  q2_main_models[["Post × HighFF"]],
  variables = list(event = c(FALSE, TRUE))
) # 0.0431

me_way_event_ff_sfha <- avg_comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = "event",
  by = c("change_type", "high_ff_score")
)
me_way_event_ff_sfha

me_way_event_sequential <- avg_comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = list(event = c(FALSE, TRUE))
)  # 0.0403

# me_change_type_sequential <- avg_comparisons(
#   q2_main_models[["Post × HighFF × SFHA"]],
#   variables = list(change_type = "sequential")
# )


me_event_by_change_type <- avg_comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = list(event = c(FALSE, TRUE)),
  by = "change_type"
)


me_event_by_high_ff <- avg_comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = "event",
  by = "high_ff_score"
)
me_event_by_high_ff
# TRUE  -0.0456


me_never_sfha_event <- avg_comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = "event",
  newdata = datagrid(
    change_type = "Never SFHA",
    high_ff_score = c(TRUE, FALSE)
  ),
  by = NULL
)

# me_never_prelim_event_by_triad <- avg_comparisons(
#   q2_main_models[["Preliminary × Triad"]],
#   variables = "event",
#   newdata = datagrid(
#     change_type_prelim = "Never SFHA",
#     high_ff_score = c(TRUE, FALSE)
#   ),
#   by = "Triad"
# )

me_all_effective_grid <- comparisons(
  q2_main_models[["Post × HighFF × SFHA"]],
  newdata = datagrid(
    event = unique,
    high_ff_score = unique,
    change_type = unique
  )
)

me_high_ff_post_by_change_type <- comparisons(
  q2_main_models[["Post × HighFF × Triad × SFHA"]],
  variables = "event",
  newdata = datagrid(
    high_ff_score = TRUE,
    change_type = c("Never SFHA", "Always SFHA", "Changes SFHA"),
    Triad = c("South", "North", "City")
  ),
  vcov = ~pin10
)

# Alternative highff_post moderation models -----------------------------------

highff_post_models <- list(
  "Change Type × HighFF Post" = feols(
    log_price ~ change_type * highff_post |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  ),
  "Change Type × HighFF Post × Triad" = feols(
    log_price ~ change_type * highff_post * Triad |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  ),
  "Partially interacted" = feols(
    log_price ~ change_type:highff_post + highff_post:Triad |
      pin + sale_year,
    data = bldg_sfha,
    cluster = ~pin10
  )
)

dropped_terms_highff_post <- flag_giant_se_terms(highff_post_models)
bad_terms_highff_post <- make_bad_terms_regex(dropped_terms_highff_post)

highff_post_table <- modelsummary(
  highff_post_models,
  output = "gt",
  fmt = 3,
  coef_omit = bad_terms_highff_post,
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|AIC|BIC"
)

me_highff_post_by_change_type <- avg_comparisons(
  highff_post_models[["Change Type × HighFF Post"]],
  variables = "highff_post",
  by = "change_type"
)

hyp_highff_post_by_change_type <- avg_comparisons(
  highff_post_models[["Change Type × HighFF Post"]],
  variables = "highff_post",
  by = "change_type",
  hypothesis = "b1=b2"
)

me_highff_post_triad_by_change_type <- avg_comparisons(
  highff_post_models[["Change Type × HighFF Post × Triad"]],
  variables = "highff_post",
  by = "change_type"
)

hyp_highff_post_triad_by_change_type <- avg_comparisons(
  highff_post_models[["Change Type × HighFF Post × Triad"]],
  variables = "highff_post",
  by = "change_type",
  hypothesis = "b1=b2"
)

me_highff_post_partial_by_change_type <- avg_comparisons(
  highff_post_models[["Partially interacted"]],
  variables = "highff_post",
  by = "change_type"
)

me_highff_post_grid_by_change_type_triad <- comparisons(
  highff_post_models[["Change Type × HighFF Post × Triad"]],
  variables = "highff_post",
  newdata = datagrid(
    change_type = c("Never SFHA", "Always SFHA", "Changes SFHA"),
    Triad = c("South", "North", "City")
  ),
  vcov = ~pin10
)

# Save outputs -----------------------------------------------------------------

saveRDS(q2_main_models, file.path(out_dir, "q2_main_models.rds"))
saveRDS(q2_main_table, file.path(out_dir, "q2_main_table.rds"))
saveRDS(highff_post_models, file.path(out_dir, "highff_post_models.rds"))
saveRDS(highff_post_table, file.path(out_dir, "highff_post_table.rds"))

saveRDS(q2_main_models, file.path(out_dir, "q2_main_models.rds"))
saveRDS(q2_main_table, file.path(out_dir, "q2_main_table.rds"))
saveRDS(highff_post_models, file.path(out_dir, "highff_post_models.rds"))
saveRDS(highff_post_table, file.path(out_dir, "highff_post_table.rds"))


save_diagnostics(dropped_terms_q2_main, diagnostics_dir, "dropped_terms_q2_main")
save_diagnostics(dropped_terms_highff_post, diagnostics_dir, "dropped_terms_highff_post")

gt::gtsave(q2_main_table, file.path(table_dir, "q2_main_table.html"))
gt::gtsave(highff_post_table, file.path(table_dir, "highff_post_table.html"))

main_me_outputs <- list(
  me_way_event_sequential_simp = me_way_event_sequential_simp,
  me_way_event_ff_sfha = me_way_event_ff_sfha,
  me_way_event_sequential = me_way_event_sequential,
  me_event_by_high_ff = me_event_by_high_ff,
  #  me_change_type_sequential = me_change_type_sequential,
  me_event_by_change_type = me_event_by_change_type,
  me_never_sfha_event = me_never_sfha_event,
  me_all_effective_grid = me_all_effective_grid,
  me_high_ff_post_by_change_type = me_high_ff_post_by_change_type,
  me_highff_post_by_change_type = me_highff_post_by_change_type,
  hyp_highff_post_by_change_type = hyp_highff_post_by_change_type,
  me_highff_post_triad_by_change_type = me_highff_post_triad_by_change_type,
  hyp_highff_post_triad_by_change_type = hyp_highff_post_triad_by_change_type,
  me_highff_post_partial_by_change_type = me_highff_post_partial_by_change_type,
  me_highff_post_grid_by_change_type_triad = me_highff_post_grid_by_change_type_triad
)

saveRDS(main_me_outputs, file.path(me_dir, "q2_main_marginaleffects_list.rds"))

purrr::iwalk(main_me_outputs, ~ {
  saveRDS(.x, file.path(me_dir, paste0(.y, ".rds")))
  readr::write_csv(as.data.frame(.x), file.path(me_dir, paste0(.y, ".csv")))
})

message("Saved main Q2 marginal effects to: ", me_dir)
