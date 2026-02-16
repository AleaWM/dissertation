## Makes final variables for Q1_insurance,Q1_continuedstaggered,  and Question1.qmd

source("./R/helper_pins_to_drop.R")

df_prep <- readRDS("data/processed/targets/sales/df_prep_land_updated_final.rds")  |>
  filter(!pin10 %in% drop_parcels) |>
  select(-c(clean_name.x, clean_name.y))



df_prep <- df_prep |>
  mutate(log_price = log(sale_price),
    in_eff_sfha = in_eff_sfha == 1,
    in_prelim_sfha = in_prelim_sfha == 1)

df_prep <- df_prep |>
  filter( # !is.na(high_ff_score) &
    !is.na(eff_date) &
      #     !is.na(clean_name) &
      !is.na(pre_date) # &
    #     !is.na(Triad)
  )


# are there pins that sell multiple times?
# df_prep |> group_by(sale_year, pin) |> mutate(n = n()) |> filter(n > 1)
# yes



df_prep <- df_prep |>

  mutate(
    # so I can subset by FIRM date in later steps
    eff_date_chr = as.factor(eff_date),
    pre_date_chr = as.factor(pre_date)) |>
  group_by(pin) |>
  arrange(sale_date) |>
  mutate(
    # first year it was mapped IN (per PIN)
    treat_year_add_eff = ifelse((any(addedto_eff_sfha == TRUE))                                &
      eff_date_chr != "2008-08-19",
    year(eff_date), 10000),  # never treated set to 10,000

    # first year it was mapped OUT (per PIN)
    treat_year_remove_eff = ifelse(any(removedfrom_eff_sfha == TRUE)                                   &
      eff_date_chr != "2008-08-19",
    year(eff_date), 10000),


    # Added or removed from Preliminary FIRMs
    treat_year_add_prelim = ifelse((any(addedto_prelim_sfha == TRUE)), year(pre_date), 10000),  # never treated set to 10,000

    # first year it was mapped OUT (per PIN)
    treat_year_remove_prelim = ifelse(any(removedfrom_prelim_sfha == TRUE), year(pre_date), 10000)
  ) |>
  ungroup() |>
  mutate(
    # Event time (relative year) for each outcome
    # coded to match S&A's method of relative time
    rel_year_add_eff    = ifelse(treat_year_add_eff != 10000,    sale_year - treat_year_add_eff,    -1000),
    rel_year_remove_eff = ifelse(treat_year_remove_eff != 10000, sale_year - treat_year_remove_eff, -1000),


    rel_year_add_prelim    = ifelse(treat_year_add_prelim != 10000,    sale_year - treat_year_add_prelim,    -1000),
    rel_year_remove_prelim = ifelse(treat_year_remove_prelim != 10000, sale_year - treat_year_remove_prelim, -1000),

    # adjustment for if a sale occurs in the same year that it is treated
    # if sale is before preliminary firm, it is coded as relative year == -1, not yet treated
    rel_year_add_prelim = ifelse(rel_year_add_prelim == 0 & sale_date < pre_date, -1, rel_year_add_prelim),
    rel_year_remove_prelim = ifelse(rel_year_remove_prelim == 0 & sale_date < pre_date, -1, rel_year_remove_prelim),

    rel_year_add_eff = ifelse(rel_year_add_eff == 0 & sale_date < eff_date, -1, rel_year_add_eff),
    rel_year_remove_eff = ifelse(rel_year_remove_eff == 0 & sale_date < eff_date, -1, rel_year_remove_eff)
  ) |>
  mutate(
    qtr  = quarter(sale_date),     # 1 to 4
    year = year(sale_date),
    sale_qtr_dec = year + (qtr - 1) / 4,        # 2012.00, .25, .50, .75

    group_name_eff = case_when( # group variable, based on the time period that FIRMs were updated. which is when they were first treated.
      # for ALL properties in the updated firms
      eff_date == "2008-08-19" ~ 0,
      eff_date == "2019-11-01" ~ 2019,
      eff_date == "2021-09-10" ~ 2021,
      TRUE ~ 0),

    # 2005-01-01 2015-02-15 2019-06-28 2021-09-22

    group_name_prelim = factor(case_when( # group variable, based on the time period that FIRMs were updated. which is when they were first treated.
      # for ALL properties in the updated firms
      pre_date == "2005-01-01" ~ 0,
      pre_date == "2015-02-12" ~ 2015,
      pre_date == "2019-07-01" ~ 2019,
      pre_date == "2021-09-22" ~ 2021,
      TRUE ~ 0),
  ))

df_prep <- df_prep |>
  mutate(
    treated_group_eff = (case_when(
      # for TREATED propertes in updated FIRMs!
      eff_date == "2008-08-19" & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 0,
      eff_date == "2019-11-01" & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2019,
      eff_date == "2021-09-10" & (addedto_eff_sfha | removedfrom_eff_sfha) ~ 2021,
      TRUE ~ 0)),

    treated_group_prelim = (case_when(
      # for TREATED propertes in updated FIRMs!
      pre_date == "2005-01-01" & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 0,
      pre_date == "2015-02-12" &  (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2015,
      pre_date == "2019-07-01" & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2019,
      pre_date == "2021-09-22" & (addedto_prelim_sfha | removedfrom_prelim_sfha) ~ 2021,
      TRUE ~ 0)),

    treated_group_prelim_add = (case_when(
      # for TREATED properties in updated FIRMs!
      pre_date == "2005-01-01" & (addedto_prelim_sfha) ~ 0,
      pre_date == "2015-02-12" &  (addedto_prelim_sfha) ~ 2015,
      pre_date == "2019-07-01" & (addedto_prelim_sfha) ~ 2019,
      pre_date == "2021-09-22" & (addedto_prelim_sfha) ~ 2021,
      TRUE ~ 0)),

    treated_group_prelim_remove = (case_when(
      # for TREATED propertes in updated FIRMs!
      pre_date == "2005-01-01" & (removedfrom_prelim_sfha) ~ 0,
      pre_date == "2015-02-12" &  (removedfrom_prelim_sfha) ~ 2015,
      pre_date == "2019-07-01" & (removedfrom_prelim_sfha) ~ 2019,
      pre_date == "2021-09-22" & (removedfrom_prelim_sfha) ~ 2021,
      TRUE ~ 0))
  )


df_prep <- df_prep |>
  group_by(pin) |>
  mutate(
    n_sales = n(),
    ever_added_prelim   = any(addedto_prelim_sfha == TRUE),
    ever_removed_prelim = any(removedfrom_prelim_sfha == TRUE),
  ) |>
  ungroup() |>
  mutate(
    event_remapped = ifelse(sale_date < pre_date & treated_group_prelim != 0, "Pre",
      ifelse(sale_date > pre_date & treated_group_prelim != 0, "Post", "NotRemapped")),

    prelim_sfha_category =
      case_when(
        ever_added_prelim & !ever_removed_prelim ~ "Added to prelim SFHA",
        ever_removed_prelim & !ever_added_prelim ~ "Removed from prelim SFHA",
        change_type_prelim == "Always SFHA" ~ "Always SFHA",
        change_type_prelim == "Never SFHA" ~ "Never SFHA",
        TRUE ~ "CHECK ME")
  ) |>
  ungroup()

df_prep <- df_prep |>
  # select(any_of(-c(clean_name.x, clean_name.y)) )|>
  mutate(Triad = case_when(
    clean_name == "Chicago" ~  "City",
    as.numeric(str_sub(pin, 1, 2)) < 13 ~ "North",
    as.numeric(str_sub(pin, 1, 2)) >= 13 ~ "South"
  ),
  Triad = factor(Triad, levels = c("South", "North", "City"))
  )

saveRDS(df_prep, "data/processed/df_prep_forQ1_assumptiontests.RDS")
