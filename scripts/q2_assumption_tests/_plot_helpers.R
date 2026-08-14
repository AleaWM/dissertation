# Helper functions for Q2 assumption-test plots.

library(tidyverse)

make_sfha_lineplot <- function(
  df,
  y_var = c("sale_price", "log_price"),
  facet_var,
  group_var = "prelim_sfha_category",
  title = "Market Trends by SFHA Status",
  y_label = NULL,
  x_breaks = seq(2010, 2026, 4),
  x_limits = c(2009, 2027)
) {
  y_var <- match.arg(y_var)
  grp_vars <- c("sale_year", group_var, facet_var)

  df_summarised <- df |>
    group_by(across(all_of(grp_vars))) |>
    summarise(
      avg_value = mean(.data[[y_var]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  if (is.null(y_label)) {
    y_label <- ifelse(
      y_var == "sale_price",
      "Average sale price",
      "Average logged sale price"
    )
  }

  ggplot(
    df_summarised,
    aes(
      x = sale_year,
      y = avg_value,
      group = .data[[group_var]],
      linetype = .data[[group_var]],
      shape = .data[[group_var]]
    )
  ) +
    geom_smooth(
      alpha = .5,
      color = "darkgray",
      size = 1,
      method = "lm",
      se = FALSE
    ) +
    geom_line(lwd = 1) +
    geom_point(size = 2, color = "black") +
    facet_wrap(as.formula(paste("~", facet_var))) +
    labs(
      title = title,
      x = "Sale year",
      y = y_label,
      linetype = "Group",
      shape = "Group",
      caption = NULL
    ) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

# plot_avg_price <- function(
#   data,
#   x = sale_year,
#   y,
#   shape,
#   lty = event,
#   facet = NULL,
#   title,
#   y_lab,
#   shape_lab,
#   lty_lab = "Event",
#   dollar_y = FALSE,
#   x_breaks = NULL,
#   x_limits = NULL,
#   caption = NULL
# ) {
#   p <- ggplot(data, aes(x = {{ x }}, y = {{ y }}, shape = {{ shape }})) +
#     geom_smooth(aes(linetype = {{ lty }}), method = "lm", se = FALSE, color = "darkgray", lwd = 1, alpha = .5) +
#     geom_line(aes(linetype = {{ lty }})) +
#     geom_point(size = 2) +
#     geom_vline(xintercept = 2020, lty = 2) +
#     theme_classic() +
#     theme(legend.position = "bottom") +
#     labs(
#       title = title,
#       y = y_lab,
#       x = NULL,
#       shape = shape_lab,
#       lty = lty_lab,
#       caption = caption
#     )
#
#   if (!is.null(facet)) {
#     p <- p + facet_wrap(vars({{ facet }}))
#   }
#
#   if (dollar_y) {
#     p <- p + scale_y_continuous(labels = scales::dollar)
#   }
#
#   if (!is.null(x_breaks) || !is.null(x_limits)) {
#     p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
#   }
#
#   p
# }
#

# plot_avg_price <- function(data,
#                            y,
#                            shape,
#                            facet = NULL,
#                            title = NULL,
#                            y_lab = NULL,
#                            shape_lab = NULL,
#                            dollar_y = FALSE,
#                            x_limits = NULL) {
#
#   y_var <- rlang::enquo(y)
#   shape_var <- rlang::enquo(shape)
#   facet_var <- rlang::enquo(facet)
#
#   p <- ggplot(
#     data,
#     aes(
#       x = sale_year,
#       y = !!y_var,
#       shape = as.factor(!!shape_var),
#       group = interaction(!!shape_var, event)
#     )
#   ) +
#     geom_line(aes(linetype = as.factor(event))) +
#     geom_point(size = 2) +
#     labs(
#       title = title,
#       x = "Sale Year",
#       y = y_lab,
#       shape = shape_lab,
#       linetype = "Post"
#     ) +
#     theme_minimal()
#
#   if (!rlang::quo_is_null(facet_var)) {
#     p <- p + facet_wrap(vars(!!facet_var))
#   }
#
#   if (dollar_y) {
#     p <- p + scale_y_continuous(labels = scales::dollar)
#   }
#
#   if (!is.null(x_limits)) {
#     p <- p + scale_x_continuous(limits = x_limits)
#   }
#
#   p
# }
#
#
#

plot_avg_price <- function(data,
                           y,
                           shape,
                           facet = NULL,
                           title = NULL,
                           y_lab = NULL,
                           shape_lab = NULL,
                           lty_lab = "Post",
                           dollar_y = FALSE,
                           x_breaks = NULL,
                           x_limits = NULL,
                           caption = NULL) {

  y_var <- rlang::enquo(y)
  shape_var <- rlang::enquo(shape)
  facet_var <- rlang::enquo(facet)

  has_facet <- !rlang::quo_is_null(facet_var)

  if (has_facet) {
    data <- data |>
      mutate(.plot_group = interaction({{ shape }}, event, {{ facet }}, drop = TRUE))
  } else {
    data <- data |>
      mutate(.plot_group = interaction({{ shape }}, event, drop = TRUE))
  }

  p <- ggplot(
    data,
    aes(
      x = sale_year,
      y = !!y_var,
      shape = as.factor(!!shape_var),
      group = .plot_group
    )
  ) +
    geom_line(aes(linetype = as.factor(event)), linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2020, linetype = 2) +
    labs(
      title = title,
      x = NULL,
      y = y_lab,
      shape = shape_lab,
      linetype = lty_lab,
      caption = caption
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  if (has_facet) {
    p <- p + facet_wrap(vars(!!facet_var))
  }

  if (dollar_y) {
    p <- p + scale_y_continuous(labels = scales::dollar)
  }

  if (!is.null(x_breaks) || !is.null(x_limits)) {
    p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
  }

  p
}
