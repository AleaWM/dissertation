library(dplyr)
library(tidyverse)
library(targets)
library(stringr)
library(purrr)
library(tidyr)

tar_load(sfha_compare_pin10_fourway)


# Helper: NA-safe mismatch (choose your philosophy)
# - If na_counts_as_mismatch = TRUE: NA vs 0/1 counts as mismatch
# - If FALSE: NA vs 0/1 treated as "unknown" (returns NA), and only 0 vs 1 is mismatch
mismatch_flag <- function(a, b, na_counts_as_mismatch = TRUE) {
  if (na_counts_as_mismatch) {
    dplyr::coalesce(a, -9999L) != dplyr::coalesce(b, -9999L)
  } else {
    dplyr::if_else(is.na(a) | is.na(b), NA, a != b)
  }
}

# Helper: directional disagreement (useful when you care about "mapped in" vs "mapped out")
# Returns TRUE when a==1 and b==0 (and NA if either missing)
a1_b0 <- function(a, b) dplyr::if_else(is.na(a) | is.na(b), NA, a == 1 & b == 0)

# Main function: add disagreement flags to your compare table
add_indicator_disagreement_flags <- function(df, years = c("2018", "2024", "2026"),
                                             na_counts_as_mismatch = TRUE) {
  # Define which "methods" exist for each concept/year
  # SFHA: land, bldg, ptax_land
  # RISK500: land, bldg
  for (yr in years) {
    # --- SFHA: land vs bldg vs ptax_land ---
    land_sfha <- paste0("land_sfha", yr)
    bldg_sfha <- paste0("bldg_sfha", yr)
    ptax_sfha <- paste0("ptax_land_sfha", yr)

    if (all(c(land_sfha, bldg_sfha) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_sfha_land_vs_bldg_", yr) :=
            mismatch_flag(.data[[land_sfha]], .data[[bldg_sfha]], na_counts_as_mismatch),
          !!paste0("dir_sfha_land1_bldg0_", yr) := a1_b0(.data[[land_sfha]], .data[[bldg_sfha]]),
          !!paste0("dir_sfha_bldg1_land0_", yr) := a1_b0(.data[[bldg_sfha]], .data[[land_sfha]])
        )
    }

    if (all(c(land_sfha, ptax_sfha) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_sfha_land_vs_ptax_", yr) :=
            mismatch_flag(.data[[land_sfha]], .data[[ptax_sfha]], na_counts_as_mismatch),
          !!paste0("dir_sfha_land1_ptax0_", yr) := a1_b0(.data[[land_sfha]], .data[[ptax_sfha]]),
          !!paste0("dir_sfha_ptax1_land0_", yr) := a1_b0(.data[[ptax_sfha]], .data[[land_sfha]])
        )
    }

    if (all(c(bldg_sfha, ptax_sfha) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_sfha_bldg_vs_ptax_", yr) :=
            mismatch_flag(.data[[bldg_sfha]], .data[[ptax_sfha]], na_counts_as_mismatch),
          !!paste0("dir_sfha_bldg1_ptax0_", yr) := a1_b0(.data[[bldg_sfha]], .data[[ptax_sfha]]),
          !!paste0("dir_sfha_ptax1_bldg0_", yr) := a1_b0(.data[[ptax_sfha]], .data[[bldg_sfha]])
        )
    }

    # --- RISK500 (0.2%): land vs bldg ---
    land_r500 <- paste0("risk500_land_", yr)
    bldg_r500 <- paste0("risk500_bldg_", yr)

    if (all(c(land_r500, bldg_r500) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_risk500_land_vs_bldg_", yr) :=
            mismatch_flag(.data[[land_r500]], .data[[bldg_r500]], na_counts_as_mismatch),
          !!paste0("dir_risk500_land1_bldg0_", yr) := a1_b0(.data[[land_r500]], .data[[bldg_r500]]),
          !!paste0("dir_risk500_bldg1_land0_", yr) := a1_b0(.data[[bldg_r500]], .data[[land_r500]])
        )
    }

    # --- 1% vs 0.2% comparison within-method (if you want it) ---
    # Note: This assumes your SFHA indicators are the 1% (100-year) concept,
    # and risk500_* are 0.2% (500-year) concept.
    if (all(c(land_sfha, land_r500) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_land_1pct_vs_0p2pct_", yr) :=
            mismatch_flag(.data[[land_sfha]], .data[[land_r500]], na_counts_as_mismatch),
          !!paste0("dir_land_1pct1_0p2pct0_", yr) := a1_b0(.data[[land_sfha]], .data[[land_r500]]),
          !!paste0("dir_land_0p2pct1_1pct0_", yr) := a1_b0(.data[[land_r500]], .data[[land_sfha]])
        )
    }

    if (all(c(bldg_sfha, bldg_r500) %in% names(df))) {
      df <- df |>
        mutate(
          !!paste0("diff_bldg_1pct_vs_0p2pct_", yr) :=
            mismatch_flag(.data[[bldg_sfha]], .data[[bldg_r500]], na_counts_as_mismatch),
          !!paste0("dir_bldg_1pct1_0p2pct0_", yr) := a1_b0(.data[[bldg_sfha]], .data[[bldg_r500]]),
          !!paste0("dir_bldg_0p2pct1_1pct0_", yr) := a1_b0(.data[[bldg_r500]], .data[[bldg_sfha]])
        )
    }
  }

  # Summary flags: “ever differs across years”
  df <- df |>
    mutate(
      any_diff_sfha_land_vs_bldg =
        if (any(str_detect(names(df), "^diff_sfha_land_vs_bldg_"))) {
          rowSums(across(matches("^diff_sfha_land_vs_bldg_"), ~ .x %in% TRUE), na.rm = TRUE) > 0
        } else FALSE,
      any_diff_risk500_land_vs_bldg =
        if (any(str_detect(names(df), "^diff_risk500_land_vs_bldg_"))) {
          rowSums(across(matches("^diff_risk500_land_vs_bldg_"), ~ .x %in% TRUE), na.rm = TRUE) > 0
        } else FALSE,
      any_diff_land_1pct_vs_0p2pct =
        if (any(str_detect(names(df), "^diff_land_1pct_vs_0p2pct_"))) {
          rowSums(across(matches("^diff_land_1pct_vs_0p2pct_"), ~ .x %in% TRUE), na.rm = TRUE) > 0
        } else FALSE,
      any_diff_bldg_1pct_vs_0p2pct =
        if (any(str_detect(names(df), "^diff_bldg_1pct_vs_0p2pct_"))) {
          rowSums(across(matches("^diff_bldg_1pct_vs_0p2pct_"), ~ .x %in% TRUE), na.rm = TRUE) > 0
        } else FALSE
    )

  df
}

# --- Usage (inside a target or interactively) ---
sfha_compare_pin10_fourway_flagged <- add_indicator_disagreement_flags(sfha_compare_pin10_fourway)

# If you want NA-vs-value to *not* count as mismatch:
# sfha_compare_pin10_fourway_flagged <- add_indicator_disagreement_flags(sfha_compare_pin10_fourway,
#   na_counts_as_mismatch = FALSE
# )


# Helper: TRUE if any of these columns equals 1/TRUE (rowwise), ignoring NAs
row_any_true01 <- function(df, cols) {
  # cols: character vector of column names that exist in df
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  x <- df[, cols, drop = FALSE]

  # handle logical or 0/1 numeric/integer
  x01 <- lapply(x, function(v) {
    if (is.logical(v)) return(v %in% TRUE)
    v %in% 1
  }) |>
    as.data.frame()

  rowSums(x01, na.rm = TRUE) > 0
}

filter_to_ever_sfha_or_lomr <- function(df) {
  # --- SFHA columns: anything like *_sfha2018, *_sfha2024, *_sfha2026 ---
  sfha_cols <- names(df)[grepl("sfha(2018|2024|2026)$", names(df))]

  # --- LOMR columns: you have lomr_date and bldg_lomr_date (dates)
  # Keep if either is non-missing
  lomr_cols <- intersect(c("lomr_date", "bldg_lomr_date"), names(df))

  df |>
    mutate(
      ever_sfha = row_any_true01(cur_data(), sfha_cols),
      ever_lomr = if (length(lomr_cols) > 0) {
        rowSums(across(all_of(lomr_cols), ~ !is.na(.x)), na.rm = TRUE) > 0
      } else {
        FALSE
      }
    ) |>
    filter(ever_sfha | ever_lomr)
}

# Usage:
sfha_compare_small <- sfha_compare_pin10_fourway |>
  filter_to_ever_sfha_or_lomr()

# If you already added disagreement flags, filter after:
sfha_compare_flagged_small <- sfha_compare_pin10_fourway_flagged |>
  filter_to_ever_sfha_or_lomr()


## Summarize Differences ---------------

summarize_diff_rates <- function(df) {

  df |>
    summarise(
      n_pins = n(),

      # SFHA (1% risk)
      n_sfha_land_vs_bldg =
        sum(any_diff_sfha_land_vs_bldg, na.rm = TRUE),

      # 0.2% risk
      n_risk500_land_vs_bldg =
        sum(any_diff_risk500_land_vs_bldg, na.rm = TRUE),

      # 1% vs 0.2%
      n_land_1pct_vs_0p2pct =
        sum(any_diff_land_1pct_vs_0p2pct, na.rm = TRUE),
      n_bldg_1pct_vs_0p2pct =
        sum(any_diff_bldg_1pct_vs_0p2pct, na.rm = TRUE)
    ) |>
    mutate(across(starts_with("n_"), ~ .x / n_pins))
}

summarize_diff_rates(sfha_compare_flagged_small)


### Directional differences ---------------
# which is stricter?
# and/or which flags more risk?
summarize_direction <- function(df) {
  df |>
    summarise(
      # SFHA land vs building
      land_more_sfha =
        sum(rowSums(across(matches("^dir_sfha_land1_bldg0_"), ~ .x %in% TRUE),
          na.rm = TRUE) > 0),

      bldg_more_sfha =
        sum(rowSums(across(matches("^dir_sfha_bldg1_land0_"), ~ .x %in% TRUE),
          na.rm = TRUE) > 0),

      # 1% vs 0.2% (land)
      land_1pct_only =
        sum(rowSums(across(matches("^dir_land_1pct1_0p2pct0_"), ~ .x %in% TRUE),
          na.rm = TRUE) > 0),

      land_0p2pct_only =
        sum(rowSums(across(matches("^dir_land_0p2pct1_1pct0_"), ~ .x %in% TRUE),
          na.rm = TRUE) > 0)
    )
}

summarize_direction(sfha_compare_flagged_small)

## Remapping noise or geometry differences ---------

summarize_persistence <- function(df) {

  df |>
    mutate(
      n_years_sfha_land_vs_bldg =
        rowSums(across(matches("^diff_sfha_land_vs_bldg_"), ~ .x %in% TRUE),
          na.rm = TRUE)
    ) |>
    count(n_years_sfha_land_vs_bldg) |>
    mutate(share = n / sum(n))
}

summarize_persistence(sfha_compare_flagged_small)

# Key interpretation:
# Mostly 1 year → remapping / temporal artifacts
# Mostly 2–3 years → structural spatial disagreement


## LOMRs vs non-lomrs -----------------------
sfha_compare_flagged_small |>
  mutate(
    lomr_any = !is.na(lomr_date) | !is.na(bldg_lomr_date)
  ) |>
  group_by(lomr_any) |>
  summarise(
    share_diff_sfha_land_vs_bldg =
      mean(any_diff_sfha_land_vs_bldg, na.rm = TRUE),
    share_diff_risk500_land_vs_bldg =
      mean(any_diff_risk500_land_vs_bldg, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

## High conflict PINs --------------------

sfha_conflict_scored <- sfha_compare_flagged_small |>
  mutate(
    conflict_score =
      rowSums(
        cbind(
          any_diff_sfha_land_vs_bldg,
          any_diff_risk500_land_vs_bldg,
          any_diff_land_1pct_vs_0p2pct,
          any_diff_bldg_1pct_vs_0p2pct
        ),
        na.rm = TRUE
      )
  ) |> select(pin10, conflict_score, any_diff_sfha_land_vs_bldg:any_diff_bldg_1pct_vs_0p2pct, everything()) |>
  arrange(desc(any_diff_bldg_1pct_vs_0p2pct))


library(sf)
tar_load(ptaxsim_parcels_sf)

sfha_conflict_sf <- ptaxsim_parcels_sf |>
  #select(pin10, geometry) |>
  sf::st_as_sf(wkt = "geometry", crs = 4326) |>
  
  inner_join(
    sfha_conflict_scored |>
      select(pin10, conflict_score),
    by = "pin10"
  )


ggplot(sfha_conflict_sf  |> mutate(township = str_sub(pin10, 1, 2)) |> filter(township == "03")) +
  geom_sf(aes(fill = factor(conflict_score)), linewidth = 0) +
  scale_fill_manual(
    values = c(
      "1" = "#fee08b",
      "2" = "#f46d43",
      "3" = "#d73027",
      "4" = "#7f0000"
    ),
    name = "Conflict score"
  ) +
  labs(
    title = "Parcels with conflicting flood-risk classifications",
    subtitle = "Counts disagreements across spatial methods and risk thresholds"
  ) +
  theme_void()


library(tidyr)

# agreement_long <- sfha_compare_flagged_small |>
#   select(
#     starts_with("diff_sfha_land_vs_bldg_"),
#     starts_with("diff_risk500_land_vs_bldg_"),
#     starts_with("diff_land_1pct_vs_0p2pct_"),
#     starts_with("diff_bldg_1pct_vs_0p2pct_")
#   ) |>
#   pivot_longer(
#     cols = everything(),
#     names_to = "comparison",
#     values_to = "diff"
#   ) |>
#   group_by(comparison) |>
#   summarise(
#     disagreement_rate = mean(diff, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# agreement_long <- agreement_long |>
#   mutate(
#     comparison = case_when(
#       grepl("sfha_land_vs_bldg", comparison) ~ "SFHA: Land vs Building",
#       grepl("risk500_land_vs_bldg", comparison) ~ "0.2% Risk: Land vs Building",
#       grepl("land_1pct_vs_0p2pct", comparison) ~ "Land: 1% vs 0.2%",
#       grepl("bldg_1pct_vs_0p2pct", comparison) ~ "Building: 1% vs 0.2%",
#       TRUE ~ comparison
#     )
#   )


agreement_long <- sfha_compare_flagged_small |>
  select(
    starts_with("diff_sfha_land_vs_bldg_"),
    starts_with("diff_risk500_land_vs_bldg_"),
    starts_with("diff_land_1pct_vs_0p2pct_"),
    starts_with("diff_bldg_1pct_vs_0p2pct_")
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "comparison_raw",
    values_to = "diff"
  ) |>
  mutate(
    comparison = case_when(
      grepl("sfha_land_vs_bldg", comparison_raw) ~ "SFHA: Land vs Building",
      grepl("risk500_land_vs_bldg", comparison_raw) ~ "0.2% Risk: Land vs Building",
      grepl("land_1pct_vs_0p2pct", comparison_raw) ~ "Land: 1% vs 0.2%",
      grepl("bldg_1pct_vs_0p2pct", comparison_raw) ~ "Building: 1% vs 0.2%",
      TRUE ~ comparison_raw
    )
  ) |>
  group_by(comparison) |>
  summarise(
    disagreement_rate = mean(diff, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(agreement_long,
       aes(x = comparison, y = "Disagreement rate", fill = disagreement_rate)) +
  geom_tile() +
  geom_text(
    aes(label = scales::percent(disagreement_rate, accuracy = 0.1)),
    color = "black"
  ) +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(
    title = "Disagreement rates across flood-risk definitions",
    x = NULL,
    y = NULL,
    fill = "Share disagreeing"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())
