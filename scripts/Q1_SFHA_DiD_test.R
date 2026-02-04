# remotes::install_github("pedrohcgs/didFF")

library(didFF)     # for testing if functional form matters for the analysis. Tests if parallel trends assumption is sensitive to functional form (i.e. logged variable vs unlogged variable)
library(tidyverse)


# mw_df<-didFF::Cengiz_df
#
# start_t = 2010
# mw_df_2010_2015 <-
#   mw_df %>%
#   dplyr::filter((start_t <= year) & (year <= end_t)) %>%
#   dplyr::group_by(statenum) %>%
#   dplyr::mutate(group = max(treated_quarter > 0,na.rm=TRUE)*end_t) %>%
#   dplyr::filter((year == start_t) | (year == end_t)) %>%
#   dplyr::group_by(statenum, wagebins) %>%
#   dplyr::mutate(wagebins=wagebins/100, wgt=overallcountpc*population) %>%
#   dplyr::filter(wgt >= 0)
#


df_prep <- readRDS("data/processed/targets/sales/df_prep_land_updated_final.rds") |>
  select(-c(clean_name.x, clean_name.y))

df_prep <- df_prep |>
  mutate(log_price = log(sale_price))

df_prep <- df_prep |>
  filter(!is.na(high_ff_score) &
    !is.na(pre_date) &
    !is.na(Triad)
  )


# are there pins that sell multiple times
df_prep |> group_by(sale_year, pin) |> mutate(n = n()) |> filter(n > 1)
# yes

############## new attempt, feb 3rd -------------

start_t <- 2014
end_t <- 2022

df_clean <- df_prep |>
  filter((start_t <= sale_year) & (sale_year <= end_t)) |>
  group_by(pin) |>
  mutate(
    group = ifelse(any(change_type_prelim == "Changes SFHA"), end_t, 0),
    pin_num = as.numeric(pin)
  ) |> ungroup() |>
  filter(sale_year == start_t | sale_year == end_t) |>

  # keep only 1 observation per pin per year
  group_by(pin_num, sale_year, group) |>
  summarize(
    log_price = mean(log_price, na.rm = TRUE),
    sale_price = mean(sale_price, na.rm = TRUE)
  ) |> ungroup()

table(df_clean$group)
table(df_clean$sale_year)



# with sale price, not transformed
out <- didFF(
  yname = "sale_price",
  idname = "pin_num",
  tname = "sale_year",
  gname = "group",
  allow_unbalanced_panel = TRUE,
  data = df_clean,
  lb_graph    = 0,
  ub_graph    = 3000000,
)

summary(out)

out$plot
out$pval
saveRDS(out, file = "outputs/didFF_Q1_model_notlogged.rds")
p <- out$plot
saveRDS(p, file = "outputs/didFF_plot_Q1_notlogged.rds")

p + labs(
  title = "Implied density tests of distributional parallel trends",
  x = "Sale Price (Not Logged)",
) +
  scale_x_continuous(labels = scales::dollar)

#### running it with logged version of variable,
# which it fails, but I don't think you are supposed to log it in advance.
out <- didFF(
  yname = "log_price",
  idname = "pin_num",
  tname = "sale_year",
  gname = "group",
  allow_unbalanced_panel = TRUE,
  data = df_clean
)

summary(out)

out$plot
out$pval
saveRDS(out, file = "outputs/didFF_Q1_model_out.rds")
p <- out$plot
saveRDS(p, file = "outputs/didFF_plot_Q1_out.rds")

p + labs(
  title = "Implied density tests of distributional parallel trends",
  x = "Logged Sale Price",
)


#####################################

df_clean <- df_prep |>
  group_by(pin) |>
  mutate(
    group = ifelse(any(change_type_prelim == "Changes SFHA") &
      sale_date > pre_date, 2021, 0),
    pin_num = as.numeric(pin)
  ) |> ungroup() |>
  group_by(pin_num, sale_year, group) |>
  summarize(
    log_price = mean(log_price, na.rm = TRUE)
  ) |> ungroup()

table(df_clean$group)
table(df_clean$sale_year)
## Check if same if keeping most recent sale
# just in case first sale is the original sale of a condo unit when its built or something

## Drop sales under $10K in original data cleaning stages .... later....

# are there pins that sell multiple times
df_clean |> group_by(sale_year, pin_num) |>
  mutate(n = n()) |> filter(n > 1)
# not any more


out <- didFF(
  yname = "log_price",
  idname = "pin_num",
  tname = "sale_year",
  gname = "group",
  allow_unbalanced_panel = TRUE,
  data = df_clean
)

summary(out)

out$plot
out$pval
saveRDS(out, file = "outputs/didFF_Q1_model_out.rds")
p <- out$plot
saveRDS(p, file = "outputs/didFF_plot_Q1_out.rds")

p + labs(
  title = "Implied density tests of distributional parallel trends",
  x = "Logged Sale Price",
)


df_clean |> filter(log_price < 9)

df_prep |> filter(log(sale_price) < 9)  |>
  select(sale_price, Triad, in_eff_sfha, high_ff_score, everything()) |> View()



df_clean2 <- df_prep |>
  group_by(pin) |>
  mutate(
    group = ifelse(any(change_type_prelim == "Changes SFHA") &
      sale_date > pre_date, 2021, 0),
    pin_num = as.numeric(pin)
  ) |> ungroup() |>
  group_by(pin_num, sale_year, group) |> summarize(
    sale_price = mean(sale_price, na.rm = TRUE)
  ) |> ungroup()


out2 <- didFF(yname = "sale_price",
  idname = "pin_num",
  tname = "sale_year",
  gname = "group",
  allow_unbalanced_panel = TRUE,
  data = df_clean2
)

summary(out2)
p2 <- out2$plot
out2$pval
saveRDS(out2, file = "outputs/didFF_model_Q1_out2.rds")
saveRDS(p2, file = "outputs/didFF_plot_Q1_out2.rds")
p2 +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Implied density tests of distributional parallel trends",
    x = "Sale Price",
  )
