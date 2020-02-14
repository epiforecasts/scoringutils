##' Applies forecast assessments and scores to the forecasts
##'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param prediction_type
#' @param outcome_type
#' @metrics vector of names of the metrics
#'
##'
##' @return list of data frames with the forecast scores
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @export


y = sim_true = true_values <- rpois(100, lambda = 1:100)

dat = sim_estim = predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))


eval_forecsts <- function(true_values,
                          predictions,
                          prediction_type = "probabilistic",
                          outcome_type = "integer",
                          metrics = c()) {

  res = data.frame("metric" = NULL,
                   "mean" = NULL,
                   "sd" = NULL)


  tmp = scoringutils::PIT(true_values, predictions)$p_values
  res <- rbind(res, data.frame(metric = "PIT_AD_calibration",
                                mean = mean(tmp),
                                sd = sd(tmp)))


  tmp = scoringutils::sharpness(predictions)
  res <- rbind(res, data.frame(metric = "Sharpness",
                               mean = mean(tmp),
                               sd = sd(tmp)))

  tmp = scoringutils::bias(true_values, predictions)
  res <- rbind(res, data.frame(metric = "Bias",
                               mean = mean(tmp),
                               sd = sd(tmp)))

  tmp = scoringutils::bias(true_values, predictions)
  res <- rbind(res, data.frame(metric = "Bias",
                               mean = mean(tmp),
                               sd = sd(tmp)))

  tmp = scoringRules::dss_sample(true_values, predictions)
  res <- rbind(res, data.frame(metric = "DSS",
                               mean = mean(tmp),
                               sd = sd(tmp)))

  tmp = scoringRules::crps_sample(true_values, predictions)
  res <- rbind(res, data.frame(metric = "CRPS",
                               mean = mean(tmp),
                               sd = sd(tmp)))







data.frame(res)

}


assess_all_forecasts <- function (max_horizon) {
  df <- samples_semi_mechanistic %>%
    filter(stochasticity == "deterministic" &
             start_n_week_before == 0 &
             weeks_averaged == 1 &
             transmission_rate == "fixed") %>%
    bind_rows(samples_bsts) %>%
    bind_rows(samples_deterministic) %>%
    bind_rows(samples_unfocused) %>%
    mutate(model=factor(model, levels=c("Semi-mechanistic",
                                        setdiff(model, "Semi-mechanistic"))))

  if (!missing(max_horizon)) {
    df <- df %>%
      filter((date - last_obs)/7 <= max_horizon)
  }

  df <- df %>%
    select_if(~ !any(is.na(.)))

  ret <- list()

  ret[["Calibration"]] <- df %>%
    assess_incidence_forecast(calibration_sample)

  ret[["Bias"]] <- df %>%
    assess_incidence_forecast(bias_sample) %>%
    group_by(model, horizon) %>%
    summarise(mean=mean(bias), sd=sd(bias))

  ret[["Sharpness"]] <- df %>%
    assess_incidence_forecast(sharpness_sample) %>%
    group_by(model, horizon) %>%
    summarise(mean=mean(sharpness), sd=sd(sharpness))

  ret[["RPS"]] <- df %>%
    assess_incidence_forecast(scoringRules::crps_sample) %>%
    group_by(model, horizon) %>%
    summarise(mean=mean(score), sd=sd(score))

  ret[["DSS"]] <- df %>%
    assess_incidence_forecast(scoringRules::dss_sample) %>%
    group_by(model, horizon) %>%
    summarise(mean=mean(score), sd=sd(score))

  ret[["AE"]] <- df %>%
    assess_incidence_forecast(ae_sample) %>%
    group_by(model, horizon) %>%
    summarise(mean=mean(ae), sd=sd(ae))

  return(ret)
}
