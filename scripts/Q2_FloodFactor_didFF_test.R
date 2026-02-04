# remotes::install_github("pedrohcgs/didFF")

library(didFF)     # for testing if functional form matters for the analysis. Tests if parallel trends assumption is sensitive to functional form (i.e. logged variable vs unlogged variable)
library(tidyverse)

df_prep <- readRDS("data/processed/df_prep_buildings.rds") |>
  select(-c(clean_name.x, clean_name.y))

df_prep <- df_prep |>
  filter(pin10 != "0710101038") |>
  mutate(log_price = log(sale_price))

df_prep <- df_prep |>
  filter(!is.na(high_ff_score) &
    !is.na(eff_date) &
    !is.na(clean_name) &
    !is.na(pre_date) &
    !is.na(Triad)
  )


# are there pins that sell multiple times
df_prep |> group_by(sale_year, pin) |> mutate(n = n()) |> filter(n > 1)
# yes


df_clean <- df_prep |>
  group_by(pin) |>
  mutate(
    group = ifelse(any(high_ff_score == TRUE & sale_date > new_info_released), 2020, 0),
    pin_num = as.numeric(pin)
  ) |> ungroup() |>
  group_by(pin_num, sale_year, group) |> summarize(
    log_price = mean(log_price, na.rm = TRUE)
  ) |> ungroup()


## Check if same if keeping most recent sale
# just in case first sale is the original sale of a condo unit when its built or something

## Drop sales under $10K in original data cleaning stages .... later....

# are there pins that sell multiple times
df_clean |> group_by(sale_year, pin_num) |> mutate(n = n()) |> filter(n > 1)
# not any more


out <- didFF(yname = "log_price",
  idname = "pin_num",
  tname = "sale_year",
  gname = "group",
  allow_unbalanced_panel = TRUE,

  data = df_clean
)

summary(out)

out$plot
out$pval
saveRDS(out, file = "outputs/didFF_model_out.rds")
p <- out$plot
saveRDS(p, file = "outputs/didFF_plot_out.rds")

p + labs(
  title = "Implied density tests of distributional parallel trends",
  x = "Logged Sale Price",
)


df_clean |> filter(log_price < 9)

df_prep |> filter(log(sale_price) < 9)  |> select(sale_price, Triad, in_eff_sfha, high_ff_score, everything()) |> View()



df_clean2 <- df_prep |>
  group_by(pin) |>
  mutate(
    group = ifelse(any(high_ff_score == TRUE & sale_date > new_info_released), 2020, 0),
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
saveRDS(out2, file = "outputs/didFF_model_out2.rds")
saveRDS(p2, file = "outputs/didFF_plot_out2.rds")
p2 +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Implied density tests of distributional parallel trends",
    x = "Sale Price",
  )
