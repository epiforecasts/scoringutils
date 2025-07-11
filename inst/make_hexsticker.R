library(hexSticker)
library(ggplot2)
library(dplyr)
library(magick)
library(scoringutils)

# Make standard plot
scores <- example_quantile %>%
  as_forecast_quantile() %>%
  score() %>%
  summarise_scores(by = c("model", "target_type")) |>
  filter(target_type == "Cases")

hex_plot <- plot_wis(scores, x = "model", relative_contributions = FALSE) +
  facet_wrap(~target_type, scales = "free_x") +
  labs(x = "", y = "") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Make and save hexsticker
sticker(
  hex_plot,
  s_x = 0,
  s_y = 0.9,
  s_width = 3.5,
  s_height = 1.4,
  package = "scoringutils",
  p_color = "#646770",
  p_size = 76,
  p_x = 1,
  p_y = 1.5,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

# Make outside of hex sticker transparent
p <- image_read(file.path("man", "figures", "logo.png"))
fuzz <- 50

pp <- p |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = "+1+1"
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+1")
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+1", "+", image_info(p)$height - 1)
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+", image_info(p)$height - 1)
  )

image_write(image = pp, path = file.path("man", "figures", "logo.png"))

usethis::use_logo(file.path("man", "figures", "logo.png"))
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
