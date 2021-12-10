library(data.table)
library(dplyr)
library(devtools)
library(here)
library(covidHubUtils) # devtools::install_github("reichlab/covidHubUtils")
library(purrr)
library(data.table)
library(stringr)


# download data from the European Forecast Hub Github Repository using
# subversion. You can also download the folders manually instead.

system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/EuroCOVIDhub-ensemble")
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/EuroCOVIDhub-baseline")
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/UMass-MechBayes")
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/epiforecasts-EpiNow2")
system("svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed/UVA-Ensemble")

# load truth data using the covidHubutils package ------------------------------
truth <- covidHubUtils::load_truth(hub = "ECDC") |>
  filter(target_variable %in% c("inc case", "inc death")) |>
  mutate(target_variable = ifelse(target_variable == "inc case",
                                  "Cases", "Deaths")) |>
  rename(target_type = target_variable,
         true_value = value) |>
  select(-model)

# get the correct file paths to all forecasts ----------------------------------
folders <- here(c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline", "UMASS-MechBayes", "UVA-Ensemble"))

file_paths <- purrr::map(folders,
                         .f = function(folder) {
                           files <- list.files(folder)
                           out <- here::here(folder, files)
                           return(out)}) %>%
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
                           }) %>%
  filter(grepl("case", target) | grepl("death", target)) %>%
  mutate(target_type = ifelse(grepl("death", target),
                              "Deaths", "Cases"),
         horizon = as.numeric(substr(target, 1, 1))) %>%
  rename(prediction = value) %>%
  filter(type == "quantile",
         grepl("inc", target)) %>%
  select(location, forecast_date, quantile, prediction,
         model, target_end_date, target, target_type, horizon)

# merge forecast data and truth data and save
hub_data <- merge_pred_and_obs(prediction_data, truth,
                               by = c("location", "target_end_date",
                                      "target_type")) |>
  filter(target_end_date >= "2021-01-01") |>
  select(-location_name, -population, -target)

# harmonise forecast dates to be the date a submission was made
hub_data <- mutate(hub_data,
                   forecast_date = calc_submission_due_date(forecast_date))

hub_data <- hub_data |>
  filter(horizon <= 3,
         forecast_date > "2021-05-01",
         forecast_date < "2021-07-15",
         location %in% c("DE", "GB", "FR", "IT"))

truth <- truth |>
  filter(target_end_date > "2021-01-01",
         target_end_date < max(hub_data$target_end_date),
         location %in% c("DE", "GB", "FR", "IT"))

# save example data with forecasts only
example_quantile_forecasts_only <- hub_data
usethis::use_data(example_quantile_forecasts_only, overwrite = TRUE)

example_truth_only <- truth
usethis::use_data(example_truth_only, overwrite = TRUE)


# join
example_quantile <- dplyr::left_join(truth, hub_data) %>%
  dplyr::mutate(model = as.character(model))
data.table::setDT(example_quantile)
# make model a character instead of a factor
usethis::use_data(example_quantile, overwrite = TRUE)




# create long range example ----------------------------------------------------
example_range_long <- quantile_to_range_long(example_quantile,
                                             keep_quantile_col = FALSE)
usethis::use_data(example_range_long, overwrite = TRUE)



# create wide range example ----------------------------------------------------
example_range_wide <- range_long_to_wide(example_range_long)
example_range_wide[, NA_NA := NULL]
usethis::use_data(example_range_wide, overwrite = TRUE)


#create semi-wide range example ------------------------------------------------
example_range_semi_wide <- data.table::copy(example_range_long)
example_range_semi_wide <- data.table::dcast(example_range_semi_wide,
                                                  ... ~ boundary,
                                                  value.var = "prediction")
example_range_semi_wide[, "NA" := NULL]
usethis::use_data(example_range_semi_wide, overwrite = TRUE)



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

  res <- nloptr::sbplx(x0 = init, fn = fn_gamma, x = x,
                       lower = c(shape = 0, rate = 0),
                       control = list(xtol_rel = 1.0e-6, ftol_rel = 1.0e-6))
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
example_continuous <- example_quantile[, .(prediction = get_samples(prediction,
                                                                    quantile,
                                                                    n_samples = n_samples),
                                           sample = 1:n_samples,
                                           true_value = unique(true_value)),
                                       by = c("location", "target_end_date", "target_type",
                                              "forecast_date", "model", "horizon")]
# remove unnecessary rows where no predictions are available
example_continuous[is.na(prediction), sample := NA]
example_continuous <- unique(continuous_example_data)
usethis::use_data(example_continuous, overwrite = TRUE)


# get integer sample data ------------------------------------------------------
example_integer <- data.table::copy(example_continuous)
example_integer <- integer_example_data[, prediction := round(prediction)]
usethis::use_data(example_integer, overwrite = TRUE)





# get binary example data ------------------------------------------------------
# construct a binary prediction by looking at the number of samples below the
# mean prediction. Construct the outcome as whether or not the actually
# observed value was below or above that mean prediction.
# Take this as a way to create example data, not as sound statistical practice

example_binary <- data.table::copy(example_continuous)

# store grouping variable
by = c("location", "target_end_date", "target_type",
       "forecast_date", "model", "horizon")

# calculate mean value
example_binary[, mean_val := mean(prediction),
               by = by]

# calculate binary prediction as percentage above mean
example_binary[, prediction := mean(prediction > mean_val),
               by = by]

# calculate true value as whether or not observed was above mean
example_binary[, true_value := true_value > mean_val]

# delete unnecessary columns and take unique values
example_binary[, `:=`(sample = NULL, mean_val = NULL,
                      true_value = as.numeric(true_value))]
example_binary <- unique(binary_example_data)
usethis::use_data(example_binary, overwrite = TRUE)

