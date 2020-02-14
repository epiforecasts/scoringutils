# ##' Applies forecast assessments and scores to the forecasts
# ##'
# ##'
# ##' @return list of data frames with the forecast scores
# ##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
# ##' @export
#
#
#
# assess_all_forecasts <- function (max_horizon) {
#   df <- samples_semi_mechanistic %>%
#     filter(stochasticity == "deterministic" &
#              start_n_week_before == 0 &
#              weeks_averaged == 1 &
#              transmission_rate == "fixed") %>%
#     bind_rows(samples_bsts) %>%
#     bind_rows(samples_deterministic) %>%
#     bind_rows(samples_unfocused) %>%
#     mutate(model=factor(model, levels=c("Semi-mechanistic",
#                                         setdiff(model, "Semi-mechanistic"))))
#
#   if (!missing(max_horizon)) {
#     df <- df %>%
#       filter((date - last_obs)/7 <= max_horizon)
#   }
#
#   df <- df %>%
#     select_if(~ !any(is.na(.)))
#
#   ret <- list()
#
#   ret[["Calibration"]] <- df %>%
#     assess_incidence_forecast(calibration_sample)
#
#   ret[["Bias"]] <- df %>%
#     assess_incidence_forecast(bias_sample) %>%
#     group_by(model, horizon) %>%
#     summarise(mean=mean(bias), sd=sd(bias))
#
#   ret[["Sharpness"]] <- df %>%
#     assess_incidence_forecast(sharpness_sample) %>%
#     group_by(model, horizon) %>%
#     summarise(mean=mean(sharpness), sd=sd(sharpness))
#
#   ret[["RPS"]] <- df %>%
#     assess_incidence_forecast(scoringRules::crps_sample) %>%
#     group_by(model, horizon) %>%
#     summarise(mean=mean(score), sd=sd(score))
#
#   ret[["DSS"]] <- df %>%
#     assess_incidence_forecast(scoringRules::dss_sample) %>%
#     group_by(model, horizon) %>%
#     summarise(mean=mean(score), sd=sd(score))
#
#   ret[["AE"]] <- df %>%
#     assess_incidence_forecast(ae_sample) %>%
#     group_by(model, horizon) %>%
#     summarise(mean=mean(ae), sd=sd(ae))
#
#   return(ret)
# }
