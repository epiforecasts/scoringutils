library(data.table)
library(dplyr)
library(scoringutils)
library(ggplot2)
library(tidyr)
library(patchwork)

## Real Data
ex <- example_continuous |>
  filter(model == "EuroCOVIDhub-ensemble")

scores <- ex |>
  score()

setnames(scores, old = c("dss", "crps", "log_score"),
         new = c("DSS", "CRPS", "Log score"))

df <- ex[sample == 1] |>
  merge(scores) |>
  melt(measure.vars = c("DSS", "CRPS", "Log score"),
       variable.name = "Scoring rule", value.name = "Score")

df[, `Scoring rule` := factor(`Scoring rule`, levels = c("CRPS", "DSS", "Log score"))]

 p_true <- df |>
  filter(horizon == 3, location == "DE") |>
  ggplot(aes(x = true_value, y = Score, ,group = `Scoring rule`,
             colour = `Scoring rule`)) +
  geom_line() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Observed value") +
  theme_scoringutils() +
  theme(legend.position = "bottom")


# ------------------------------------------------------------------------------
# illustration:
# in this we see that the mean as well as the variance of the scores scale
# for crps, while the variance stays constant for dss and log score

library(tidyr)

simulate <- function(n_samples = 5e3,
                     n_replicates = 1e3,
                     true_value = 1,
                     scale_mean = 1,
                     scale_sd = scale_mean) {
  pred <- rnorm(n_replicates * n_samples,
                mean = true_value * scale_mean,
                sd = true_value * scale_sd)

  df <- data.table(
    true_value = true_value * scale_mean,
    prediction = pred,
    sample = 1:n_samples,
    id = paste0("id", rep(1:n_replicates, each = n_samples))
  )

  scores <- score_simulation(df, scale_mean = scale_mean, scale_sd = scale_sd)
  return(scores)
}

score_simulation <- function(df, scale_mean = 1, scale_sd = scale_mean) {
  scores <- score(df)
  m <- summarise_scores(scores, by = "model", fun = mean) |>
    melt(id.vars = "model", value.name = "mean", variable.name = "score")

  s <- summarise_scores(scores, by = "model", fun = stats::sd) |>
    melt(id.vars = "model", value.name = "sd", variable.name = "score")

  out <- merge(m, s, by = c("model", "score")) |>
    melt(id.vars = c("model", "score"), variable.name = "type")

  return(out[])
}

scales_mean <- scales_sd <- c(1, 2, 5, 10, 20, 50)

grid <- expand.grid(
  scale_mean = scales_mean,
  scale_sd = scales_sd
) |>
  setDT()


if (!file.exists("inst/manuscript/output/relation-to-scale-example.Rda")) {
  res <- grid |>
    rowwise() |>
    mutate(simulation := list(simulate(scale_mean = scale_mean, scale_sd = scale_sd)))

  saveRDS(res, file = "inst/manuscript/output/relation-to-scale-example.Rda")
} else {
  res <- readRDS("inst/manuscript/output/relation-to-scale-example.Rda")
}

df <- res |>
  tidyr::unnest(cols = "simulation")

df <- df |>
  filter(score != "bias") |>
  rename(`Scoring rule` = score) |>
  mutate(type = ifelse(type == "mean", "Mean score", "Sd score")) |>
  mutate(`Scoring rule` = ifelse(`Scoring rule` == "dss",
                                 "DSS",
                                 ifelse(`Scoring rule` == "crps", "CRPS", "Log score")))

p1 <- df |>
  filter(scale_mean == 1,
         scale_sd < 20) |>
  ggplot(aes(y = value, x = scale_sd,
             group = `Scoring rule`, color = `Scoring rule`)) +
  geom_line() +
  facet_wrap(~ type, scales = "free") +
  scale_y_log10() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_x_log10() +
  theme_scoringutils() +
  labs(y = "Score", x = "Sd of F and G (mean constant)")


p2 <- df |>
  filter(scale_sd == 1,
         scale_mean < 20) |>
  ggplot(aes(y = value, x = scale_mean,
             group = `Scoring rule`, color = `Scoring rule`)) +
  geom_line() +
  facet_wrap(~ type, scales = "free") +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  theme_scoringutils() +
  labs(y = "Score", x = "Mean of F and G (sd constant)")

layout <- "
AAACC
BBBCC
"

p2 + p1 + p_true +
  plot_layout(guides = "collect", design = layout) &
  theme(legend.position = "bottom") &
  plot_annotation(tag_levels = 'A')

ggsave("inst/manuscript/output/illustration-effect-scale.png",
       height = 4.3, width = 8)
