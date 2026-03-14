library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

to_logical01 <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x) || is.integer(x)) return(x == 1)
  if (is.character(x)) {
    return(tolower(x) %in% c("1", "true", "t", "yes", "y"))
  }
  as.logical(x)
}


add_q1_assumption_vars <- function(df) {

  df <- df |>
    mutate(
      #   dataset_id = dplyr::coalesce(.data$dataset_id, NA_character_),

      sale_date = as.Date(sale_date),
      sale_year = lubridate::year(sale_date),
      log_price = log(sale_price),

      eff_date = EFF_DATE,
      pre_date = PRE_DATE,
      eff_date_chr = as.character(eff_date),
      pre_date_chr = as.character(pre_date),

      in_eff_sfha = to_logical01(in_eff_sfha),
      in_prelim_sfha = to_logical01(in_prelim_sfha),
      addedto_eff_sfha = to_logical01(addedto_eff_sfha),
      removedfrom_eff_sfha = to_logical01(removedfrom_eff_sfha),
      addedto_prelim_sfha = to_logical01(addedto_prelim_sfha),
      removedfrom_prelim_sfha = to_logical01(removedfrom_prelim_sfha)
    )

  # if (drop_problem_pins && !is.null(drop_parcels) && "pin10" %in% names(df)) {
  #   df <- df |>
  #     filter(
  #       !pin10 %in% drop_parcels |
  #         (pin10 == "0113301013" & sale_year > 2014)
  #     )
  # }

  df |>
    filter(!is.na(in_eff_sfha) & !is.na(in_prelim_sfha)) |>
    mutate(
      Triad = case_when(
        clean_name == "Chicago" ~ "City",
        as.numeric(str_sub(pin, 1, 2)) < 13 ~ "North",
        as.numeric(str_sub(pin, 1, 2)) >= 13 ~ "South",
        TRUE ~ NA_character_
      ),
      Triad = factor(Triad, levels = c("South", "North", "City"))
    ) |>
    group_by(pin) |>
    arrange(sale_date, .by_group = TRUE) |>
    mutate(
      n_sales = n(),
      d_price_pct = round((sale_price - lag(sale_price)) / lag(sale_price), 2),

      ever_added_prelim = any(addedto_prelim_sfha == TRUE, na.rm = TRUE),
      ever_removed_prelim = any(removedfrom_prelim_sfha == TRUE, na.rm = TRUE),

      treat_year_add_eff = ifelse(
        any(addedto_eff_sfha == TRUE, na.rm = TRUE) & eff_date_chr != "2008-08-19",
        year(eff_date),
        10000
      ),
      treat_year_remove_eff = ifelse(
        any(removedfrom_eff_sfha == TRUE, na.rm = TRUE) & eff_date_chr != "2008-08-19",
        year(eff_date),
        10000
      ),
      treat_year_add_prelim = ifelse(
        any(addedto_prelim_sfha == TRUE, na.rm = TRUE),
        year(pre_date),
        10000
      ),
      treat_year_remove_prelim = ifelse(
        any(removedfrom_prelim_sfha == TRUE, na.rm = TRUE),
        year(pre_date),
        10000
      )
    ) |>
    ungroup() |>
    mutate(
      rel_year_add_eff = ifelse(treat_year_add_eff != 10000, sale_year - treat_year_add_eff, -1000),
      rel_year_remove_eff = ifelse(treat_year_remove_eff != 10000, sale_year - treat_year_remove_eff, -1000),
      rel_year_add_prelim = ifelse(treat_year_add_prelim != 10000, sale_year - treat_year_add_prelim, -1000),
      rel_year_remove_prelim = ifelse(treat_year_remove_prelim != 10000, sale_year - treat_year_remove_prelim, -1000),

      rel_year_add_prelim = ifelse(rel_year_add_prelim == 0 & sale_date < pre_date, -1, rel_year_add_prelim),
      rel_year_remove_prelim = ifelse(rel_year_remove_prelim == 0 & sale_date < pre_date, -1, rel_year_remove_prelim),
      rel_year_add_eff = ifelse(rel_year_add_eff == 0 & sale_date < eff_date, -1, rel_year_add_eff),
      rel_year_remove_eff = ifelse(rel_year_remove_eff == 0 & sale_date < eff_date, -1, rel_year_remove_eff),

      qtr = quarter(sale_date),
      year = year(sale_date),
      sale_qtr_dec = year + (qtr - 1) / 4,

      group_name_eff = case_when(
        eff_date == as.Date("2008-08-19") ~ 0,
        eff_date == as.Date("2019-11-01") ~ 2019,
        eff_date == as.Date("2021-09-10") ~ 2021,
        TRUE ~ 0
      ),

      group_name_prelim = factor(case_when(
        pre_date == as.Date("2005-01-01") ~ 0,
        pre_date == as.Date("2015-02-12") ~ 2015,
        pre_date == as.Date("2019-07-01") ~ 2019,
        pre_date == as.Date("2021-09-22") ~ 2021,
        TRUE ~ 0
      )),

      treated_group_eff = case_when(
        eff_date == as.Date("2008-08-19") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 0,
        eff_date == as.Date("2019-11-01") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2019,
        eff_date == as.Date("2021-09-10") & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2021,
        TRUE ~ 0
      ),

      treated_group_prelim = case_when(
        pre_date == as.Date("2005-01-01") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 0,
        pre_date == as.Date("2015-02-12") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2015,
        pre_date == as.Date("2019-07-01") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2019,
        pre_date == as.Date("2021-09-22") & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2021,
        TRUE ~ 0
      ),

      treated_group_prelim_add = case_when(
        pre_date == as.Date("2005-01-01") & addedto_prelim_sfha ~ 0,
        pre_date == as.Date("2015-02-12") & addedto_prelim_sfha ~ 2015,
        pre_date == as.Date("2019-07-01") & addedto_prelim_sfha ~ 2019,
        pre_date == as.Date("2021-09-22") & addedto_prelim_sfha ~ 2021,
        TRUE ~ 0
      ),

      treated_group_prelim_remove = case_when(
        pre_date == as.Date("2005-01-01") & removedfrom_prelim_sfha ~ 0,
        pre_date == as.Date("2015-02-12") & removedfrom_prelim_sfha ~ 2015,
        pre_date == as.Date("2019-07-01") & removedfrom_prelim_sfha ~ 2019,
        pre_date == as.Date("2021-09-22") & removedfrom_prelim_sfha ~ 2021,
        TRUE ~ 0
      ),

      event_remapped = ifelse(
        sale_date < pre_date & treated_group_prelim != 0,
        "Pre",
        ifelse(
          sale_date > pre_date & treated_group_prelim != 0,
          "Post",
          "NotRemapped"
        )
      ),

      prelim_sfha_category = case_when(
        ever_added_prelim & !ever_removed_prelim ~ "Added to prelim SFHA",
        ever_removed_prelim & !ever_added_prelim ~ "Removed from prelim SFHA",
        change_type_prelim == "Always SFHA" ~ "Always SFHA",
        change_type_prelim == "Never SFHA" ~ "Never SFHA",
        TRUE ~ "CHECK ME!"
      )
    ) |>
    filter(is.na(d_price_pct) | d_price_pct < 20)
}

read_q1_dataset_bundle <- function(dataset_map) {
  out <- purrr::imap(dataset_map, function(path, id) {
    df <- readRDS(path)
    df$dataset_id <- id
    df
  })
  names(out) <- names(dataset_map)
  out
}
