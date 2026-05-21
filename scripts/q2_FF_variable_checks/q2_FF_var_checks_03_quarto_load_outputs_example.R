# q2_03_quarto_load_outputs_example.R
# Purpose: Copy/paste these chunks into Quarto to load saved outputs instead of rerunning models.

# Model tables
binary_table <- readRDS("outputs/q2_models/binary_ff_table.rds")
ordinal_table <- readRDS("outputs/q2_models/ordinal_ff_table.rds")
continuous_table <- readRDS("outputs/q2_models/continuous_ff_table.rds")

binary_table
ordinal_table
continuous_table

# Dropped-term diagnostics
ordinal_dropped_terms <- readRDS("outputs/q2_models/ordinal_dropped_terms.rds")
continuous_dropped_terms <- readRDS("outputs/q2_models/continuous_dropped_terms.rds")

ordinal_dropped_terms
continuous_dropped_terms

# Marginal effects RDS outputs
ordinal_event_effects <- readRDS("outputs/q2_models/marginaleffects/ordinal_event_effects_by_score_triad.rds")
continuous_ff_slopes <- readRDS("outputs/q2_models/marginaleffects/continuous_ff_slopes_post_by_triad.rds")

ordinal_event_effects
continuous_ff_slopes

# Saved plots as ggplot objects
p_ordinal_event <- readRDS("outputs/q2_models/figures/p_ordinal_event_effects_by_score_triad.rds")
p_continuous_slopes <- readRDS("outputs/q2_models/figures/p_continuous_ff_slopes_post_by_triad.rds")

p_ordinal_event
p_continuous_slopes

# Or include saved image files directly in markdown:
# ![](outputs/q2_models/figures/ordinal_event_effects_by_score_triad.png)
# ![](outputs/q2_models/figures/continuous_ff_slopes_post_by_triad.png)
