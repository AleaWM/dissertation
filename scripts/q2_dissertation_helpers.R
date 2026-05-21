# q2_dissertation_helpers.R
# Shared helper functions for Q2 dissertation scripts.

add_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ "+",
    TRUE ~ ""
  )
}

flag_giant_se_terms <- function(model_list, se_threshold = 1000) {
  model_list |>
    purrr::map_dfr(
      ~ broom::tidy(.x),
      .id = "model"
    ) |>
    dplyr::filter(is.na(std.error) | std.error > se_threshold) |>
    dplyr::select(
      dplyr::any_of(c(
        "model", "term", "estimate", "std.error", "statistic", "p.value"
      ))
    )
}

make_bad_terms_regex <- function(dropped_terms) {
  bad_terms <- dropped_terms |>
    dplyr::distinct(term) |>
    dplyr::pull(term)

  if (length(bad_terms) == 0) {
    "$^" # matches nothing
  } else {
    paste0(
      "^(" ,
      paste(stringr::str_escape(bad_terms), collapse = "|"),
      ")$"
    )
  }
}

save_diagnostics <- function(dropped_terms, diagnostics_dir, file_stem) {
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(dropped_terms, file.path(diagnostics_dir, paste0(file_stem, ".rds")))
  readr::write_csv(dropped_terms, file.path(diagnostics_dir, paste0(file_stem, ".csv")))
  invisible(dropped_terms)
}

fmt_3 <- function(x) {
  formatC(x, digits = 3, format = "f")
}
