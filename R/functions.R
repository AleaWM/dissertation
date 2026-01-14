# pull penguin data
pull_data <- function() {
  return(palmerpenguins::penguins)
}


# cleaning and wrangling data
clean_data <- function(data) {
  data |> # raw data
    janitor::clean_names() |> # lower case columns
    mutate(name = stringr::word(species, 1)) |> # species name column
    dplyr::select(name, island, flipper_length_mm, body_mass_g, sex) # selecting variables
}


# plot data
plot_data <- function(data) {
  ggplot(
    data = data,
    aes(x = body_mass_g, y = flipper_length_mm, color = name)
  ) +
    geom_point() + # add data points
    geom_smooth(method = "lm", se = FALSE) + # Add trend lines for each species
    labs(
      y = "Flipper Length (mm)",
      x = "Body Mass (g)",
      title = "Penguin Body Mass by Flipper Length",
      color = "Species"
    ) +
    scale_color_manual(
      # changing colors
      values = c(
        "Adelie" = "blue4",
        "Chinstrap" = "tan",
        "Gentoo" = "darkgreen"
      )
    ) +
    theme_minimal() # minimal appearance
}


# fit model with bill length
fit_model <- function(data) {
  lm(flipper_length_mm ~ body_mass_g + name + sex + bill_length_mm, data)
}

# model coefficients
model_coefs <- function(model) {
  summary(model)$coefficients |>
    as.data.frame() |>
    mutate(across(where(is.numeric), ~ round(., 3))) |>
    knitr::kable()
}


# model r squared
model_r2 <- function(model) {
  summary(model)$r.squared
}