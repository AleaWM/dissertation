# q2_02_run_marginaleffects.R
# Purpose: Compute marginal effects/slopes from saved Q2 models and save RDS, CSV, and figures.
# Run from the project root after q2_01_run_ff_models.R.

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(gt)

options(scipen = 999)

out_dir <- "outputs/q2_models"
me_dir <- file.path(out_dir, "marginaleffects")
fig_dir <- file.path(out_dir, "figures")
table_dir <- file.path(out_dir, "tables")
dir.create(me_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

ordinal_models_path <- file.path(out_dir, "ordinal_ff_models.rds")
continuous_models_path <- file.path(out_dir, "continuous_ff_models.rds")
if (!file.exists(ordinal_models_path) || !file.exists(continuous_models_path)) {
  source("scripts/q2_01_run_ff_models.R")
}

ordinal_models <- readRDS(ordinal_models_path)
continuous_models <- readRDS(continuous_models_path)

pct_transform <- function(x) {
  x |>
    mutate(
      pct_change = 100 * (exp(estimate) - 1),
      pct_low = 100 * (exp(conf.low) - 1),
      pct_high = 100 * (exp(conf.high) - 1)
    )
}

# -----------------------------------------------------------------------------
# Ordinal model: post-release contrast by Flood Factor score and Triad
# -----------------------------------------------------------------------------

ordinal_event_effects <- avg_comparisons(
  ordinal_models[["Ordinal × Triad"]],
  variables = "event",
  by = c("ff_score_ord", "Triad")
) |>
  pct_transform() |>
  arrange(Triad, ff_score_ord)

saveRDS(ordinal_event_effects, file.path(me_dir, "ordinal_event_effects_by_score_triad.rds"))
readr::write_csv(ordinal_event_effects, file.path(me_dir, "ordinal_event_effects_by_score_triad.csv"))

ordinal_event_table <- ordinal_event_effects |>
  select(Triad, ff_score_ord, estimate, std.error, conf.low, conf.high,
         pct_change, pct_low, pct_high, p.value) |>
  modelsummary::datasummary_df(output = "gt")

gt::gtsave(ordinal_event_table, file.path(table_dir, "me_ordinal_event_effects_by_score_triad.html"))
saveRDS(ordinal_event_table, file.path(me_dir, "ordinal_event_effects_by_score_triad_table.rds"))

p_ordinal_event <- ordinal_event_effects |>
  mutate(ff_score_ord = as.numeric(as.character(ff_score_ord))) |>
  ggplot(aes(x = ff_score_ord, y = pct_change, ymin = pct_low, ymax = pct_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ Triad) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Flood Factor score",
    y = "Estimated post-release price change (%)",
    title = "Estimated post-release price change by Flood Factor score and Triad",
    subtitle = "Contrasts compare post-release sales to pre-release sales within the same Flood Factor score and Triad"
  )

saveRDS(p_ordinal_event, file.path(fig_dir, "p_ordinal_event_effects_by_score_triad.rds"))
ggsave(file.path(fig_dir, "ordinal_event_effects_by_score_triad.png"), p_ordinal_event, width = 8, height = 5.5, dpi = 300)
ggsave(file.path(fig_dir, "ordinal_event_effects_by_score_triad.pdf"), p_ordinal_event, width = 8, height = 5.5)

# -----------------------------------------------------------------------------
# Ordinal model: post-release contrast by Flood Factor score, Triad, and change type
# This is likely appendix material because it is high-dimensional.
# -----------------------------------------------------------------------------

ordinal_change_type_event_effects <- avg_comparisons(
  ordinal_models[["Change Type × Ordinal × Event × Triad"]],
  variables = "event",
  by = c("change_type", "ff_score_ord", "Triad")
) |>
  pct_transform() |>
  arrange(change_type, Triad, ff_score_ord)

saveRDS(ordinal_change_type_event_effects, file.path(me_dir, "ordinal_event_effects_by_change_type_score_triad.rds"))
readr::write_csv(ordinal_change_type_event_effects, file.path(me_dir, "ordinal_event_effects_by_change_type_score_triad.csv"))

p_ordinal_change_type <- ordinal_change_type_event_effects |>
  mutate(ff_score_ord = as.numeric(as.character(ff_score_ord))) |>
  ggplot(aes(x = ff_score_ord, y = pct_change, ymin = pct_low, ymax = pct_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_grid(change_type ~ Triad) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Flood Factor score",
    y = "Estimated post-release price change (%)",
    title = "Estimated post-release price change by Flood Factor score, SFHA change type, and Triad",
    subtitle = "Appendix-style diagnostic for the four-way ordinal model"
  )

saveRDS(p_ordinal_change_type, file.path(fig_dir, "p_ordinal_event_effects_by_change_type_score_triad.rds"))
ggsave(file.path(fig_dir, "ordinal_event_effects_by_change_type_score_triad.png"), p_ordinal_change_type, width = 10, height = 7, dpi = 300)
ggsave(file.path(fig_dir, "ordinal_event_effects_by_change_type_score_triad.pdf"), p_ordinal_change_type, width = 10, height = 7)

# -----------------------------------------------------------------------------
# Continuous model: post-release slope of FF score by Triad
# -----------------------------------------------------------------------------

continuous_ff_slopes <- avg_slopes(
  continuous_models[["Continuous × Triad"]],
  variables = "env_flood_fs_factor",
  by = c("event", "Triad")
) |>
  filter(event == TRUE) |>
  pct_transform() |>
  arrange(Triad)

saveRDS(continuous_ff_slopes, file.path(me_dir, "continuous_ff_slopes_post_by_triad.rds"))
readr::write_csv(continuous_ff_slopes, file.path(me_dir, "continuous_ff_slopes_post_by_triad.csv"))

continuous_slopes_table <- continuous_ff_slopes |>
  select(Triad, estimate, std.error, conf.low, conf.high,
         pct_change, pct_low, pct_high, p.value) |>
  modelsummary::datasummary_df(output = "gt")

gt::gtsave(continuous_slopes_table, file.path(table_dir, "me_continuous_ff_slopes_post_by_triad.html"))
saveRDS(continuous_slopes_table, file.path(me_dir, "continuous_ff_slopes_post_by_triad_table.rds"))

p_continuous_slopes <- continuous_ff_slopes |>
  ggplot(aes(x = Triad, y = pct_change, ymin = pct_low, ymax = pct_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated price change for one additional FF point (%)",
    title = "Post-release marginal association between Flood Factor score and sale price",
    subtitle = "Estimated from the continuous Flood Factor × event × Triad model"
  )

saveRDS(p_continuous_slopes, file.path(fig_dir, "p_continuous_ff_slopes_post_by_triad.rds"))
ggsave(file.path(fig_dir, "continuous_ff_slopes_post_by_triad.png"), p_continuous_slopes, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(fig_dir, "continuous_ff_slopes_post_by_triad.pdf"), p_continuous_slopes, width = 7, height = 4.5)

# -----------------------------------------------------------------------------
# Continuous model: post-release slope by SFHA change type and Triad
# -----------------------------------------------------------------------------

continuous_change_type_ff_slopes <- avg_slopes(
  continuous_models[["Change Type × Continuous × Event × Triad"]],
  variables = "env_flood_fs_factor",
  by = c("event", "change_type", "Triad")
) |>
  filter(event == TRUE) |>
  pct_transform() |>
  arrange(change_type, Triad)

saveRDS(continuous_change_type_ff_slopes, file.path(me_dir, "continuous_ff_slopes_post_by_change_type_triad.rds"))
readr::write_csv(continuous_change_type_ff_slopes, file.path(me_dir, "continuous_ff_slopes_post_by_change_type_triad.csv"))

p_continuous_change_type <- continuous_change_type_ff_slopes |>
  ggplot(aes(x = Triad, y = pct_change, ymin = pct_low, ymax = pct_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  coord_flip() +
  facet_wrap(~ change_type) +
  labs(
    x = NULL,
    y = "Estimated price change for one additional FF point (%)",
    title = "Post-release marginal association between Flood Factor score and sale price by SFHA change type",
    subtitle = "Use this figure to interpret the four-way continuous model"
  )

saveRDS(p_continuous_change_type, file.path(fig_dir, "p_continuous_ff_slopes_post_by_change_type_triad.rds"))
ggsave(file.path(fig_dir, "continuous_ff_slopes_post_by_change_type_triad.png"), p_continuous_change_type, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(fig_dir, "continuous_ff_slopes_post_by_change_type_triad.pdf"), p_continuous_change_type, width = 9, height = 5.5)

message("Saved Q2 marginal effects, tables, and figures to: ", out_dir)
