# Create Q2 assumption-test model and table objects once, before rendering.
# Run from the project root after 00_create_q2_assumption_data.R:
# source("scripts/q2_assumption_tests/02_create_q2_model_and_table_objects.R")

library(tidyverse)
data_path <- "outputs/q2_models/bldg_sfha_q2_prepped.RDS"
df_prep <- readRDS(data_path)

out_dir <- "outputs/q2_assumption_tests"
models_path <- file.path(out_dir, "q2_assumption_models.rds")
tables_path <- file.path(out_dir, "q2_assumption_tables.rds")

models <- list(
  ff_transformations = list(
    `Sale price` = lm(sale_price ~ high_ff_score * event * sale_year, data = df_prep),
    `Log sale price` = lm(log(sale_price) ~ high_ff_score * event * sale_year, data = df_prep),
    `asinh sale price` = lm(asinh(sale_price) ~ high_ff_score * event * sale_year, data = df_prep)
  ),
  sfha_transformations = list(
    `Sale price` = lm(sale_price ~ in_eff_sfha * event * sale_year, data = df_prep),
    `Log sale price` = lm(log(sale_price) ~ in_eff_sfha * event * sale_year, data = df_prep),
    `asinh sale price` = lm(asinh(sale_price) ~ in_eff_sfha * event * sale_year, data = df_prep)
  )
)

tables <- list(
  missing_ff_score = df_prep |>
    filter(is.na(high_ff_score) | is.na(in_eff_sfha)),

  ff_sfha_log_by_year = df_prep |>
    filter(!is.na(high_ff_score), !is.na(in_eff_sfha)) |>
    mutate(log_price = log(sale_price)) |>
    group_by(sale_year, high_ff_score, in_eff_sfha, event) |>
    summarise(
      avg_log_price = mean(log_price, na.rm = TRUE),
      median = median(log_price, na.rm = TRUE),
      n = n(),
      distinct_pins = n_distinct(pin),
      .groups = "drop"
    ),

  ff_sfha_price_by_year = df_prep |>
    filter(!is.na(high_ff_score), !is.na(in_eff_sfha)) |>
    group_by(sale_year, high_ff_score, in_eff_sfha, event) |>
    summarise(
      avg_price = mean(sale_price, na.rm = TRUE),
      median = median(sale_price, na.rm = TRUE),
      n = n(),
      distinct_pins = n_distinct(pin),
      .groups = "drop"
    ),

  ff_sfha_log_since_2016 = df_prep |>
    filter(sale_year > 2015, !is.na(high_ff_score), !is.na(in_eff_sfha)) |>
    mutate(log_price = log(sale_price)) |>
    group_by(high_ff_score, in_eff_sfha, event) |>
    summarise(
      avg_log_price = mean(log_price, na.rm = TRUE),
      median = median(log_price, na.rm = TRUE),
      n = n(),
      distinct_pins = n_distinct(pin),
      .groups = "drop"
    ),

  ff_sfha_price_since_2016 = df_prep |>
    filter(sale_year > 2015, !is.na(high_ff_score), !is.na(in_eff_sfha)) |>
    group_by(high_ff_score, in_eff_sfha, event) |>
    summarise(
      avg_price = mean(sale_price, na.rm = TRUE),
      median = median(sale_price, na.rm = TRUE),
      n = n(),
      distinct_pins = n_distinct(pin),
      .groups = "drop"
    )
)

saveRDS(models, models_path)
saveRDS(tables, tables_path)

message("Saved Q2 model objects to: ", models_path)
message("Saved Q2 table objects to: ", tables_path)
