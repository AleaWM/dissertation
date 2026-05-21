# q2_dissertation_02_etwfe_models.R
# Purpose: Estimate and save ETWFE models and marginal effects.
# Run from the project root after q2_dissertation_00_prep_data.R.

library(tidyverse)
library(etwfe)
library(modelsummary)
library(gt)
library(broom)

source("scripts/q2_dissertation_helpers.R")

options(scipen = 999)

out_dir <- "outputs/q2_marginaleffects"
etwfe_dir <- file.path(out_dir, "etwfe")
table_dir <- file.path(out_dir, "tables")
dir.create(etwfe_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

prep_path <- file.path(out_dir, "bldg_sfha_q2_dissertation_prepped.rds")
if (!file.exists(prep_path)) {
  source("scripts/q2_dissertation_00_prep_data.R")
}

bldg_sfha <- readRDS(prep_path)

mod_etwfe_base <- etwfe(
  fml = log(sale_price) ~ 1,
  data = bldg_sfha,
  ivar = pin_num,
  tvar = sale_year,
  tref = "2019",
  gref = "0",
  gvar = ff_group,
  vcov = ~pin10
)

etwfe_base_emfx <- emfx(mod_etwfe_base)

mod_etwfe_triad <- etwfe(
  fml = log(sale_price) ~ 1,
  data = bldg_sfha,
  ivar = pin_num,
  tvar = sale_year,
  tref = "2019",
  gref = "0",
  gvar = ff_group,
  vcov = ~pin10,
  xvar = Triad
)

etwfe_triad_emfx <- emfx(mod_etwfe_triad, type = "group")

# --------------------------------------------------------------------
# Create labeled ETWFE tables
# --------------------------------------------------------------------
# modelsummary() can collapse ETWFE emfx rows when several rows share the
# raw term name `.Dtreat`. We tidy the emfx objects first and create a
# row label from any available grouping/time columns before sending to gt.

make_etwfe_table_data <- function(
    emfx_obj,
    fallback_prefix = "Effect",
    label_by = NULL,
    fixed_label = NULL
) {
  df <- tryCatch(
    broom::tidy(emfx_obj) |> dplyr::as_tibble(),
    error = function(e) as.data.frame(emfx_obj) |> dplyr::as_tibble()
  )

  if (!is.null(fixed_label)) {
    df <- df |>
      dplyr::mutate(label = fixed_label)
  } else if (!is.null(label_by) && all(label_by %in% names(df))) {
    df <- df |>
      tidyr::unite(
        col = "label",
        dplyr::all_of(label_by),
        sep = " × ",
        remove = FALSE,
        na.rm = TRUE
      )
  } else {
    df <- df |>
      dplyr::mutate(label = paste(fallback_prefix, dplyr::row_number()))
  }

  df |>
    dplyr::mutate(
      stars = add_stars(p.value),
      estimate_display = paste0(fmt_3(estimate), stars),
      se_display = paste0("(", fmt_3(std.error), ")")
    ) |>
    dplyr::transmute(
      Effect = label,
      Estimate = estimate_display,
      `Std. Error` = se_display
    )
}

etwfe_base_table_data <- make_etwfe_table_data(
  etwfe_base_emfx,
  fixed_label = "High Flood Factor × Post Release"
)

etwfe_triad_table_data <- make_etwfe_table_data(
  etwfe_triad_emfx,
  label_by = "Triad"
) |>
  dplyr::mutate(
    Effect = paste0("High Flood Factor × Post Release: ", Effect)
  )

etwfe_base_table <- etwfe_base_table_data |>
  gt::gt() |>
  gt::tab_header(title = "ETWFE: Base Model") |>
  gt::tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )

etwfe_triad_table <- etwfe_triad_table_data |>
  gt::gt() |>
  gt::tab_header(title = "ETWFE: Regional Heterogeneity") |>
  gt::tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )

saveRDS(mod_etwfe_base, file.path(etwfe_dir, "mod_etwfe_base.rds"))
saveRDS(mod_etwfe_triad, file.path(etwfe_dir, "mod_etwfe_triad.rds"))
saveRDS(etwfe_base_emfx, file.path(etwfe_dir, "etwfe_base_emfx.rds"))
saveRDS(etwfe_triad_emfx, file.path(etwfe_dir, "etwfe_triad_emfx.rds"))
saveRDS(etwfe_base_table_data, file.path(etwfe_dir, "etwfe_base_table_data.rds"))
saveRDS(etwfe_triad_table_data, file.path(etwfe_dir, "etwfe_triad_table_data.rds"))
saveRDS(etwfe_base_table, file.path(etwfe_dir, "etwfe_base_table.rds"))
saveRDS(etwfe_triad_table, file.path(etwfe_dir, "etwfe_triad_table.rds"))

gt::gtsave(etwfe_base_table, file.path(table_dir, "etwfe_base_table.html"))
gt::gtsave(etwfe_triad_table, file.path(table_dir, "etwfe_triad_table.html"))

message("Saved ETWFE models and marginal effects to: ", etwfe_dir)
