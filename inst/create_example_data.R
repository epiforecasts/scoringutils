library(data.table)
library(dplyr)
library(devtools)

# install package from github repository
# devtools::install_github("epiforecasts/covid19.forecasts.uk")





# create quantile example ------------------------------------------------------
# load forecasts and do some filtering
data <- covid19.forecasts.uk::uk_forecasts %>%
  dplyr::mutate(horizon = as.numeric(value_date - creation_date),
                quantile = round(quantile, 3)) %>%
  dplyr::filter(model %in% c("EpiSoon", "SIRCOVID", "DetSEIRwithNB MCMC"),
                creation_date > "2020-06-01",
                geography %in% c("England", "Scotland", "Wales", "Northern Ireland"),
                horizon %in% c(7, 14, 21)) %>%
  dplyr::rename(prediction = value)

# get available dates
dates <- data$value_date %>%
  unique()

# load observations and keep a couple of weeks before any forecasts were made
obs <- covid19.forecasts.uk::covid_uk_data %>%
  dplyr::filter(value_date %in% c(as.Date(c("2020-06-08", "2020-06-01", "2020-05-25",
                                            "2020-05-18", "2020-05-11", "2020-05-04")),
                                  dates),
                geography %in% c("England", "Scotland", "Wales", "Northern Ireland")) %>%
  dplyr::rename(true_value = value)

# save example data with forecasts only
example_quantile_forecasts_only <- data
usethis::use_data(example_quantile_forecasts_only, overwrite = TRUE)

example_truth_data_only <- obs
usethis::use_data(example_truth_data_only, overwrite = TRUE)


# join
quantile_example_data <- dplyr::left_join(obs, data) %>%
  dplyr::mutate(model = as.character(model))
data.table::setDT(quantile_example_data)
# make model a character instead of a factor
usethis::use_data(quantile_example_data, overwrite = TRUE)




# create long range example ----------------------------------------------------
range_example_data_long <- quantile_to_range_long(quantile_example_data,
                                                 keep_quantile_col = FALSE)
usethis::use_data(range_example_data_long, overwrite = TRUE)



# create wide range example ----------------------------------------------------
range_example_data_wide <- range_long_to_wide(range_example_data_long)
range_example_data_wide[, NA_NA := NULL]
usethis::use_data(range_example_data_wide, overwrite = TRUE)




#create semi-wide range example ------------------------------------------------
range_example_data_semi_wide <- data.table::copy(range_example_data_long)
range_example_data_semi_wide <- data.table::dcast(range_example_data_semi_wide,
                                                  ... ~ boundary,
                                                  value.var = "prediction")
range_example_data_semi_wide[, "NA" := NULL]
usethis::use_data(range_example_data_semi_wide, overwrite = TRUE)



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
setDT(quantile_example_data)
n_samples <- 50
continuous_example_data <- quantile_example_data[, .(prediction = get_samples(prediction,
                                                                              quantile,
                                                                              n_samples = n_samples),
                                                     sample = 1:n_samples,
                                                     true_value = unique(true_value)),
                                                 by = c("value_date", "value_type", "geography",
                                                        "value_desc", "model", "creation_date",
                                                        "horizon")]
# remove unnecessary rows where no predictions are available
continuous_example_data[is.na(prediction), sample := NA]
continuous_example_data <- unique(continuous_example_data)
usethis::use_data(continuous_example_data, overwrite = TRUE)


# get integer sample data ------------------------------------------------------
integer_example_data <- data.table::copy(continuous_example_data)
integer_example_data <- integer_example_data[, prediction := round(prediction)]
usethis::use_data(integer_example_data, overwrite = TRUE)





# get binary example data ------------------------------------------------------
# construct a binary prediction by looking at the number of samples below the
# mean prediction. Construct the outcome as whether or not the actually
# observed value was below or above that mean prediction.
# Take this as a way to create example data, not as sound statistical practice

binary_example_data <- data.table::copy(continuous_example_data)

# store grouping variable
by <-  c("value_date", "value_type", "geography", "value_desc",
         "model", "creation_date", "horizon")

# calculate mean value
binary_example_data[, mean_val := mean(prediction),
                    by = by]

# calculate binary prediction as percentage above mean
binary_example_data[, prediction := mean(prediction > mean_val),
                    by = by]

# calculate true value as whether or not observed was above mean
binary_example_data[, true_value := true_value > mean_val]

# delete unnecessary columns and take unique values
binary_example_data[, `:=`(sample = NULL, mean_val = NULL,
                           true_value = as.numeric(true_value))]
binary_example_data <- unique(binary_example_data)
usethis::use_data(binary_example_data, overwrite = TRUE)

