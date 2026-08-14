# Create Q2 assumption-test plot objects once, before rendering.
# Run from the project root after 00_create_q2_assumption_data.R:
# source("scripts/q2_assumption_tests/01_create_q2_plot_objects.R")

library(tidyverse)
library(scales)

source("scripts/q2_assumption_tests/_plot_helpers.R")

data_path <- "outputs/q2_models/bldg_sfha_q2_prepped.RDS"
df_prep <- readRDS(data_path)

out_dir <- "outputs/q2_assumption_tests"
out_path <- file.path(out_dir, "q2_assumption_plots.rds")

plots <- list()

plots$figure6_prelim_cohort_include_chicago <- make_sfha_lineplot(
  df_prep |> filter(change_type_prelim %in% c("Always SFHA", "Never SFHA")),
  y_var = "log_price",
  facet_var = "pre_date",
  title = "Trends by FIRM Date and SFHA Status - Include Chicago"
)

plots$figure7_prelim_cohort_exclude_chicago <- make_sfha_lineplot(
  df_prep |> filter(change_type_prelim %in% c("Always SFHA", "Never SFHA"), Triad != "City"),
  y_var = "log_price",
  facet_var = "pre_date",
  title = "Trends by FIRM Date and SFHA Status - Exclude Chicago"
)

plots$sfha_trends_by_triad <- make_sfha_lineplot(
  df_prep |> filter(change_type_prelim %in% c("Always SFHA", "Never SFHA")),
  y_var = "log_price",
  facet_var = "Triad",
  title = "Trends by Triad and SFHA Status"
)

plots$appendix_fig13_price_by_cohort <- make_sfha_lineplot(
  df_prep |> filter(!pin10 %in% c("0122400028", "0516106013")),
  y_var = "sale_price",
  facet_var = "pre_date",
  title = "Price Trends by Cohort and SFHA Status"
)

plots$appendix_fig14_log_by_cohort <- make_sfha_lineplot(
  df_prep |> filter(!pin10 %in% c("0122400028", "0516106013")),
  y_var = "log_price",
  facet_var = "pre_date",
  title = "Price Trends by Cohort and SFHA Status"
)

prelim_triad <- df_prep |>
  group_by(sale_year, in_prelim_sfha, Triad) |>
  summarise(avg_price = mean(log_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$prelim_by_triad <- ggplot(prelim_triad, aes(x = sale_year, y = avg_price, lty = in_prelim_sfha, shape = in_prelim_sfha)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray", lwd = 1, alpha = .5) +
  geom_point() +
  geom_line(color = "black") +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  facet_wrap(~Triad) +
  labs(
    title = "Pre- and Post-Event Slopes by Triad and SFHA Status",
    caption = "",
    lty = "In SFHA",
    shape = "In SFHA",
    x = NULL,
    y = "Avg. Logged Sale Price"
  ) +
  scale_x_continuous(breaks = seq(2010, 2026, 4), limits = c(2009, 2027)) +
  theme_classic() +
  theme(legend.position = "bottom")

prelim_date <- df_prep |>
  group_by(sale_year, in_prelim_sfha, pre_date) |>
  summarise(avg_price = mean(log_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$prelim_by_pre_date <- ggplot(prelim_date, aes(x = sale_year, y = avg_price, lty = in_prelim_sfha, shape = in_prelim_sfha)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray", lwd = 1, alpha = .5) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2020, lty = 2) +
  facet_wrap(~pre_date) +
  labs(
    title = "Pre- and Post-Event Slopes by Cohort and SFHA Status",
    caption = "",
    lty = "In SFHA",
    shape = "In SFHA",
    x = NULL,
    y = "Avg. Logged Sale Price"
  ) +
  scale_x_continuous(breaks = seq(2010, 2026, 4), limits = c(2009, 2027)) +
  theme_classic() +
  theme(legend.position = "bottom")

coastal_summary <- df_prep |>
  filter(!is.na(high_ff_score)) |>
  mutate(period = if_else(sale_date < new_info_released, "1.Pre", "2.Post")) |>
  group_by(period, sale_year, in_eff_sfha, high_ff_score, Triad) |>
  summarise(median_price = median(log_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$coastal_sensitivity <- ggplot(coastal_summary, aes(x = sale_year, y = median_price, shape = high_ff_score)) +
  geom_vline(xintercept = 2020) +
  facet_wrap(vars(Triad, in_eff_sfha), ncol = 2) +
  geom_smooth(aes(linetype = period), method = "lm", se = FALSE, color = "darkgray", alpha = .5) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Pre vs Post Slopes by Triad and FEMA SFHA Status",
    shape = "FF Score > 4",
    lty = "Info Released",
    x = NULL,
    y = "Median Logged Price"
  ) +
  scale_x_continuous(breaks = c(2010, 2020, 2024), limits = c(2015, 2025)) +
  theme_classic() +
  theme(legend.position = "bottom")

# Note: the original QMD labels this as excluding coastal properties but does not actually filter any rows here.
plots$coastal_sensitivity_excludes_coastal_label_only <- plots$coastal_sensitivity +
  labs(subtitle = "Excludes Coastal properties")

# Parallel trends: Flood Factor score
ff_price <- df_prep |>
  group_by(sale_year, high_ff_score, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$ff_sale_price_overall <- plot_avg_price(
  ff_price,
  y = avg_price,
  shape = high_ff_score,
  title = "Pre- and Post- Event Treatment Trends",
  y_lab = "Avg. Sale Price",
  shape_lab = "FF > 4",
  dollar_y = TRUE,
  x_limits = c(2009, 2026)
)

ff_price_triad <- df_prep |>
  group_by(sale_year, high_ff_score, Triad, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(),
    .groups = "drop")

# has error with triad
plots$ff_sale_price_triad <- plot_avg_price(
  ff_price_triad,
  y = avg_price,
  shape = high_ff_score,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. Sale Price",
  shape_lab = "FF > 4",
  dollar_y = TRUE,
  x_limits = c(2009, 2026)
)

ff_log <- df_prep |>
  group_by(sale_year, high_ff_score, event) |>
  summarise(avg_logprice = mean(log(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$ff_log_overall <- plot_avg_price(
  ff_log,
  y = avg_logprice,
  shape = high_ff_score,
  title = "Pre- and Post-Event Trends",
  y_lab = "Avg. Logged Sale Price",
  shape_lab = "FF > 4"
)

ff_log_triad <- df_prep |>
  group_by(sale_year, high_ff_score, event, Triad) |>
  summarise(avg_logprice = mean(log(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$ff_log_triad <- plot_avg_price(
  ff_log_triad,
  y = avg_logprice,
  shape = high_ff_score,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. Logged Sale Price",
  shape_lab = "FF > 4"
)

ff_inverse <- df_prep |>
  group_by(sale_year, high_ff_score, event) |>
  summarise(inverse_price = mean(1 / sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$ff_inverse_overall <- plot_avg_price(
  ff_inverse,
  y = inverse_price,
  shape = high_ff_score,
  title = "Pre- and Post-Event Trends",
  y_lab = "1/Sale Price",
  shape_lab = "FF > 4",
  dollar_y = TRUE,
  x_breaks = c(2010, 2015, 2020, 2024)
)

ff_inverse_triad <- df_prep |>
  group_by(sale_year, high_ff_score, Triad, event) |>
  summarise(inverse_price = mean(1 / sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$ff_inverse_triad <- plot_avg_price(
  ff_inverse_triad,
  y = inverse_price,
  shape = high_ff_score,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. Inverse of Sale Price",
  shape_lab = "FF > 4",
  dollar_y = TRUE,
  x_breaks = c(2010, 2015, 2020, 2024),
  caption = "Inverse Price = 1 / sale_price"
)

# Parallel trends: SFHA status
sfha_price <- df_prep |>
  group_by(sale_year, in_eff_sfha, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_sale_price_overall <- plot_avg_price(
  sfha_price,
  y = avg_price,
  shape = in_eff_sfha,
  title = "Pre- and Post- Event Treatment Trends",
  y_lab = "Avg. Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE
)

sfha_price_triad <- df_prep |>
  group_by(sale_year, in_eff_sfha, Triad, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_sale_price_triad <- plot_avg_price(
  sfha_price_triad,
  y = avg_price,
  shape = in_eff_sfha,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE
)

sfha_price_2014 <- df_prep |>
  filter(sale_year > 2013) |>
  group_by(sale_year, in_eff_sfha, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_sale_price_since_2014_overall <- plot_avg_price(
  sfha_price_2014,
  y = avg_price,
  shape = in_eff_sfha,
  title = "Pre- and Post- Event Treatment Trends",
  y_lab = "Avg. Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE
)

sfha_price_2014_triad <- df_prep |>
  filter(sale_year > 2013) |>
  group_by(sale_year, in_eff_sfha, Triad, event) |>
  summarise(avg_price = mean(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_sale_price_since_2014_triad <- plot_avg_price(
  sfha_price_2014_triad,
  y = avg_price,
  shape = in_eff_sfha,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE
)

sfha_log <- df_prep |>
  group_by(sale_year, in_eff_sfha, event) |>
  summarise(avg_logprice = mean(log(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_log_overall <- plot_avg_price(
  sfha_log,
  y = avg_logprice,
  shape = in_eff_sfha,
  title = "Pre- and Post-Event Trends by SFHA Status",
  y_lab = "Avg. Logged Sale Price",
  shape_lab = "In SFHA",
  x_breaks = c(2010, 2015, 2020, 2024)
)

sfha_log_triad <- df_prep |>
  group_by(sale_year, in_eff_sfha, Triad, event) |>
  summarise(avg_logprice = mean(log(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_log_triad <- plot_avg_price(
  sfha_log_triad,
  y = avg_logprice,
  shape = in_eff_sfha,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad & SFHA Status",
  y_lab = "Avg. Logged Sale Price",
  shape_lab = "In SFHA",
  x_breaks = c(2010, 2015, 2020, 2024)
)

sfha_inverse <- df_prep |>
  group_by(sale_year, in_eff_sfha, event) |>
  summarise(inverse_price = mean(1 / sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_inverse_overall <- plot_avg_price(
  sfha_inverse,
  y = inverse_price,
  shape = in_eff_sfha,
  title = "Pre- and Post-Event Trends by SFHA Status",
  y_lab = "Avg. Inverse of Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE,
  x_breaks = c(2010, 2015, 2020, 2024),
  caption = "Inverse Price = 1 / sale_price"
)

sfha_inverse_triad <- df_prep |>
  group_by(sale_year, in_eff_sfha, Triad, event) |>
  summarise(inverse_price = mean(1 / sale_price, na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_inverse_triad <- plot_avg_price(
  sfha_inverse_triad,
  y = inverse_price,
  shape = in_eff_sfha,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad and SFHA Status",
  y_lab = "Avg. Inverse of Sale Price",
  shape_lab = "In SFHA",
  dollar_y = TRUE,
  x_breaks = c(2010, 2015, 2020, 2024),
  caption = "Inverse Price = 1 / sale_price"
)

sfha_asinh <- df_prep |>
  group_by(sale_year, in_eff_sfha, event) |>
  summarise(asinh_price = mean(asinh(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_asinh_overall <- plot_avg_price(
  sfha_asinh,
  y = asinh_price,
  shape = in_eff_sfha,
  title = "Pre- and Post-Event Trends",
  y_lab = "Avg. asinh(Sale Price)",
  shape_lab = "In SFHA"
)

sfha_asinh_triad <- df_prep |>
  group_by(sale_year, in_eff_sfha, Triad, event) |>
  summarise(asinh_price = mean(asinh(sale_price), na.rm = TRUE), n = n(), .groups = "drop")

plots$sfha_asinh_triad <- plot_avg_price(
  sfha_asinh_triad,
  y = asinh_price,
  shape = in_eff_sfha,
  facet = Triad,
  title = "Pre- and Post-Event Trends by Triad",
  y_lab = "Avg. asinh(Sale Price)",
  shape_lab = "In SFHA"
)

saveRDS(plots, out_path)

message("Saved Q2 plot objects to: ", out_path)
message("Plot objects: ", paste(names(plots), collapse = ", "))
