library(data.table)
library(dplyr)
library(devtools)
library(here)
library(covidHubUtils) # devtools::install_github("reichlab/covidHubUtils@v0.1.8") # nolint
library(purrr)
library(data.table)
library(stringr)
library(scoringutils)


# download data from the European Forecast Hub Github Repository using
# subversion. You can also download the folders manually instead.

system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/EuroCOVIDhub-ensemble") # nolint
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/EuroCOVIDhub-baseline") # nolint
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/UMass-MechBayes") # nolint
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/epiforecasts-EpiNow2") # nolint

# load truth data using the covidHubutils package ------------------------------
truth <- covidHubUtils::load_truth(hub = "ECDC") |>
  filter(target_variable %in% c("inc case", "inc death")) |>
  mutate(target_variable = ifelse(target_variable == "inc case",
    "Cases", "Deaths"
  )) |>
  rename(
    target_type = target_variable,
    observed = value
  ) |>
  select(-model)

# get the correct file paths to all forecasts ----------------------------------
folders <- here(c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline", "UMass-MechBayes", "epiforecasts-EpiNow2"))

file_paths <- purrr::map(folders,
  .f = function(folder) {
    files <- list.files(folder)
    out <- here::here(folder, files)
    return(out)
  }
) %>%
  unlist()
file_paths <- file_paths[grepl(".csv", file_paths)]

# load all past forecasts ------------------------------------------------------
# ceate a helper function to get model name from a file path
get_model_name <- function(file_path) {
  split <- str_split(file_path, pattern = "/")[[1]]
  model <- split[length(split) - 1]
  return(model)
}

# load forecasts
prediction_data <- map_dfr(file_paths,
  .f = function(file_path) {
    data <- fread(file_path)
    data[, `:=`(
      target_end_date = as.Date(target_end_date),
      quantile = as.numeric(quantile),
      forecast_date = as.Date(forecast_date),
      model = get_model_name(file_path)
    )]
    return(data)
  }
) %>%
  filter(grepl("case", target) | grepl("death", target)) %>%
  mutate(
    target_type = ifelse(grepl("death", target),
      "Deaths", "Cases"
    ),
    horizon = as.numeric(substr(target, 1, 1))
  ) %>%
  rename(predicted = value) %>%
  filter(
    type == "quantile",
    grepl("inc", target)
  ) %>%
  select(
    location, forecast_date, quantile, predicted,
    model, target_end_date, target, target_type, horizon
  )

# harmonise forecast dates to be the date a submission was made
hub_data <- mutate(prediction_data,
  forecast_date = calc_submission_due_date(forecast_date)
)

hub_data <- hub_data |>
  filter(
    horizon <= 3,
    forecast_date > "2021-05-01",
    forecast_date < "2021-07-15",
    # quantile %in% c(seq(0.05, 0.45, 0.1), 0.5, seq(0.55, 0.95, 0.1)),
    location %in% c("DE", "GB", "FR", "IT")
  ) |>
  select(-target) |>
  rename(quantile_level = quantile)

truth <- truth |>
  filter(
    target_end_date > "2021-01-01",
    target_end_date < max(hub_data$target_end_date),
    location %in% c("DE", "GB", "FR", "IT")
  ) |>
  select(-population)

# save example data with forecasts only
example_quantile_forecasts_only <- hub_data
example_quantile_forecasts_only[, quantile_level := round(quantile_level, 3)]

usethis::use_data(example_quantile_forecasts_only, overwrite = TRUE)

example_truth_only <- truth
usethis::use_data(example_truth_only, overwrite = TRUE)

# merge forecast data and truth data and save
example_quantile <- merge_pred_and_obs(hub_data, truth)
data.table::setDT(example_quantile)
example_quantile <- as_forecast_quantile(example_quantile)
usethis::use_data(example_quantile, overwrite = TRUE)


# create data with point forecasts ---------------------------------------------
example_point <- data.table::copy(example_quantile)
example_point <- example_point[quantile %in% c(NA, 0.5)][, quantile_level := NULL]
example_point <- as_forecast_point(example_point)
usethis::use_data(example_point, overwrite = TRUE)


# get continuous sample data ---------------------------------------------------
# define gamma function
fn_gamma <- function(par, x) {
  quantiles <- as.numeric(names(x))
  quantiles <- quantiles[!is.na(x)]
  x <- x[!is.na(x)]
  return(sum((qgamma(quantiles, shape = par[1], rate = par[2]) - x)**2))
}

# define function to fit gamma
fit_gamma <- function(values, quantiles, init) {
  x <- values
  names(x) <- quantiles

  if (missing(init)) {
    init <- c(shape = 1, rate = 1)
  }

  res <- nloptr::sbplx(
    x0 = init, fn = fn_gamma, x = x,
    lower = c(shape = 0, rate = 0),
    control = list(xtol_rel = 1.0e-6, ftol_rel = 1.0e-6)
  )
  sol <- res$par
  names(sol) <- names(init)

  return(as.list(sol))
}

# function to obtain samples
get_samples <- function(values, quantiles, n_samples = 1000) {
  if (any(is.na(values))) {
    return(NA_real_)
  }
  fit <- fit_gamma(values, quantiles)
  samples <- rgamma(n = n_samples, rate = fit$rate, shape = fit$shape)
}

# calculate samples
setDT(example_quantile)
n_samples <- 40
example_sample_continuous <- example_quantile[, .(
  predicted = get_samples(
    predicted,
    quantile,
    n_samples = n_samples
  ),
  sample_id = 1:n_samples,
  observed = unique(observed)
),
by = c(
  "location", "location_name",
  "target_end_date", "target_type",
  "forecast_date", "model", "horizon"
)
]
# remove unnecessary rows where no predictions are available
example_sample_continuous[is.na(predicted), sample_id := NA]
example_sample_continuous <- unique(example_sample_continuous)
example_sample_continuous <- as_forecast_sample(example_sample_continuous)
usethis::use_data(example_sample_continuous, overwrite = TRUE)


# get integer sample data ------------------------------------------------------
example_sample_discrete <- data.table::copy(example_sample_continuous)
example_sample_discrete <- example_sample_discrete[, predicted := round(predicted)]
example_sample_discrete <- as_forecast_sample(example_sample_discrete)
usethis::use_data(example_sample_discrete, overwrite = TRUE)


# get binary example data ------------------------------------------------------
# construct a binary prediction by looking at the number of samples below the
# mean prediction. Construct the outcome as whether or not the actually
# observed value was below or above that mean prediction.
# Take this as a way to create example data, not as sound statistical practice

example_binary <- data.table::copy(example_sample_continuous)

# store grouping variable
by <- c(
  "location", "target_end_date", "target_type",
  "forecast_date", "model", "horizon"
)

# calculate mean value
example_binary[, mean_val := mean(predicted),
  by = by
]

# calculate binary prediction as percentage above mean
example_binary[, predicted := mean(predicted > mean_val),
  by = by
]

# calculate true value as whether or not observed was above mean
example_binary[, observed := observed > mean_val]

# delete unnecessary columns and take unique values
example_binary[, `:=`(
  sample_id = NULL, mean_val = NULL,
  observed = factor(as.numeric(observed))
)]
example_binary <- unique(example_binary)
example_binary <- as_forecast_binary(example_binary)
usethis::use_data(example_binary, overwrite = TRUE)


# get nominal example data ------------------------------------------------------
# construct a nominal prediction by splitting the forecast into "low", "high",
# and "medium". Again, this is only an illustrative example.

example_nominal <- data.table::copy(example_sample_continuous)

# store grouping variable
by_vars <- c(
  "location", "location_name", "target_end_date",
  "target_type", "forecast_date", "horizon"
)

# generate low, medium, and high bounds and predicted label
example_nominal[, low_bound := quantile(predicted, 0.25, na.rm = TRUE), by = by_vars]
example_nominal[, high_bound := quantile(predicted, 0.75, na.rm = TRUE), by = by_vars]

example_nominal[, low := predicted < low_bound]
example_nominal[, medium := (predicted >= low_bound & predicted <= high_bound)]
example_nominal[, high := (predicted > high_bound)]

example_nominal[, observed := ifelse(
  observed < low_bound, "low",
  ifelse(observed >= low_bound & observed <= high_bound,
         "medium", "high"))]

example_nominal <- example_nominal[
  , .(low = mean(low),
      medium = mean(medium),
      high = mean(high)),
  by = c(by_vars, "model", "observed")
]
example_nominal <- melt(
  example_nominal, measure.vars = c("low", "high", "medium"),
  value.name = "predicted", variable.name = "predicted_label"
)

example_nominal[, `:=`(
  observed = factor(observed, levels = c("low", "medium", "high")),
  predicted_label = factor(predicted_label, levels = c("low", "medium", "high"))
)]
example_nominal <- as_forecast_nominal(example_nominal)
usethis::use_data(example_nominal, overwrite = TRUE)
