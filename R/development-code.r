# library(scoringRules)
# library(scoringutils)
# library(dplyr)
# library(data.table)


# # example(es_sample)
# # d <- 10  # number of dimensions
# # m <- 50  # number of samples from multivariate forecast distribution

# # # parameters for multivariate normal example
# # mu0 <- rep(0, d)

# # mu <- rep(1, d)

# # S0 <- S <- diag(d)

# # S0[S0==0] <- 0.2

# # S[S==0] <- 0.1

# # # generate samples from multivariate normal distributions
# # obs <- drop(mu0 + rnorm(d) %*% chol(S0))

# # fc_sample <- replicate(m, drop(mu + rnorm(d) %*% chol(S)))

# # # compute Energy Score
# # es_sample(y = obs, dat = fc_sample)
# # [1] 3.700608

# # # in the univariate case, Energy Score and CRPS are the same
# # # illustration: Evaluate forecast sample for the first variable
# # es_sample(y = obs[1], dat = fc_sample[1, , drop = FALSE])
# # [1] 0.3226259

# # crps_sample(y = obs[1], dat = fc_sample[1, ])
# # [1] 0.3226259

# # # illustration of observation weights for Energy Score
# # # example: equal weights for first half of draws; zero weights for other draws
# # w <- rep(c(1, 0), each = .5*m)/(.5*m)

# # es_sample(y = obs, dat = fc_sample, w = w)
# # [1] 3.847658

# # # weighting matrix for variogram score
# # # note that, unlike for w, weights in w_vs refer to dimensions
# # # (rows of dat) rather than draws (cols of dat)
# # w_vs <- outer(1:d, 1:d, function(x, y) .5^abs(x-y))

# # vs_sample(y = obs, dat = fc_sample)
# # [1] 12.15482

# # vs_sample(y = obs, dat = fc_sample, w_vs = w_vs)
# # [1] 1.852571

# # vs_sample(y = obs, dat = fc_sample, w_vs = w_vs, p = 1)
# # [1] 7.993775



# # development plan:
# # X 0. get some example data
# # X 1. implement a first version of the function in matrix format
# # 2. build the wrapper that transforms the input dataframe into matrix format


# # several forecasts for different countries together form a single forecast
# example <- example_sample_continuous |>
#     filter(
#         forecast_date == "2021-07-12", horizon == 2,
#         target_type == "Cases", model == "EuroCOVIDhub-ensemble"
#     )

# forecast_unit <- get_forecast_unit(example)
# # transpose the forecasts that belong to the same forecast unit
# # this is the same thing we do for every other univariate scoring function
# f_transposed <- example[, .(predicted = list(predicted),
#                             observed = unique(observed),
#                             scoringutils_N = length(list(sample_id))),
#                         by = forecast_unit]

# # make example data in matrix format
# observed <- f_transposed$observed
# predicted <- do.call(rbind, f_transposed$predicted)


# # compare my function with the scoringRules function
# es_sample(y = observed, dat = predicted)
# scoringutils:::energy_score_multivariate(observed, predicted, grouping_id = rep(1, 4))

# # normalfall: eine große Matrix, die scoring Funktion arbeitet mit einer großen Matrix und macht
# # einen score pro Zeile
# scoringRules::logs_sample(y = observed, dat = predicted)

# # for multivariate scores, we therefore need something that is able to split the matrix into separate matrices
# # the function should take a "grouping" variable according to which it gets split

# # we assume the grouping variable here is the country
# grouping <- c("target_type", "target_end_date", "model")


# score_forecast_multivariate <- function(forecast, metrics = get_metrics(forecast), grouping, ...) {
#     #   forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
#     forecast_unit <- get_forecast_unit(forecast)
#     #   metrics <- validate_metrics(metrics)
#     forecast <- as.data.table(forecast)

#     # transpose the forecasts that belong to the same forecast unit
#     f_transposed <- forecast[, .(
#         predicted = list(predicted),
#         observed = unique(observed),
#         scoringutils_N = length(list(sample_id))
#     ),
#     by = forecast_unit
#     ]


#     #   # univariate scores ==========================================================
#     #   # split according to number of samples and do calculations for different
#     #   # sample lengths separately
#     #   # for univariate scores we compute one score per row for the univariate forecasts
#     #   # but it all happens as part of a single matric, so it doesn't matter much
#     #   f_split_univ <- split(f_transposed, f_transposed$scoringutils_N)

#     #   split_result_univ <- lapply(f_split_univ, function(data) {
#     #     # create a matrix
#     #     observed <- data$observed
#     #     predicted <- do.call(rbind, data$predicted)
#     #     data[, c("observed", "predicted", "scoringutils_N") := NULL]

#     #     result_univ <- apply_metrics(
#     #         data, metrics,
#     #         observed, predicted
#     #     )
#     #     return(result_univ)
#     #   })
#     #   scores_univ <- rbindlist(split_result_univ, fill = TRUE)
#     #   scores_univ <- as_scores(scores_univ, metrics = names(metrics))


#     # multivariate scores ==========================================================
#     # for multivariate scores, we have a a lot more splits, because every single forecast
#     # (made up of several single forecasts) is one separate list element
#     # we also have to enforce that all rows belonging to a single forecast
#     # have the same number of samples
#     f_transposed[, .scoringutils_group_id := .GRP, by = grouping]
#     f_split_multiv <- split(f_transposed, f_transposed$scoringutils_N)

#     metrics = list(es = energy_score_multivariate)

#     split_result_multiv <- lapply(f_split_multiv, function(data) {
#         observed <- data$observed
#         predicted <- do.call(rbind, data$predicted)
#         grouping_id <- data$.scoringutils_group_id
#         data[, c("observed", "predicted", ".scoringutils_group_id") := NULL]

#         result_univ <- apply_metrics(
#             data, metrics,
#             observed, predicted,
#             grouping_id = grouping_id
#         )
#         return(result_univ)
#     })


#     scores_multiv <- rbindlist(split_result_multiv, fill = TRUE)
#     #   scores_multiv <- as_scores(scores_multiv, metrics = names(metrics))

#     return(scores_multiv[])
# }


# score_forecast_multivariate(example, metrics = get_metrics(example), grouping = grouping)

# # same as
# es_sample(y = observed, dat = predicted)
