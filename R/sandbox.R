# library(magrittr)
#
# # make new binary example_data
# data <- scoringutils::binary_example_data
#
# binary_example_data <- rbind(data %>%
#                 dplyr::mutate(horizon =1),
#               data %>%
#                 dplyr::mutate(horizon = 2,
#                               predictions = predictions * 0.7))
#
# usethis::use_data(binary_example_data, overwrite = TRUE)
#
# # make new integer example_data
# data <- scoringutils::integer_example_data
#
# integer_example_data <- rbind(data %>%
#                                dplyr::mutate(horizon =1),
#                              data %>%
#                                dplyr::mutate(horizon = 2,
#                                              predictions = predictions + rpois(30000, 0.5)))
#
# usethis::use_data(integer_example_data, overwrite = TRUE)
#
# a <- eval_forecasts(integer_example_data, summarised = TRUE)
#
#
#
# # make new continuous example_data
# data <- scoringutils::continuous_example_data
#
# continuous_example_data <- rbind(data %>%
#                                 dplyr::mutate(horizon =1),
#                               data %>%
#                                 dplyr::mutate(horizon = 2,
#                                               predictions = predictions + rnorm(3000, 0.5)))
#
# usethis::use_data(continuous_example_data, overwrite = TRUE)
#
# a <- eval_forecasts(continuous_example_data, by = c("model", "horizon"), summarised = TRUE)
#
#
#
#
# # make new quantile example_data_long
# data <- scoringutils::quantile_example_data_long
#
# quantile_example_data_long <- rbind(data %>%
#                                    dplyr::mutate(horizon =1),
#                                  data %>%
#                                    dplyr::mutate(horizon = 2,
#                                                  predictions = predictions + rnorm(360, 0.2)))
#
#
# usethis::use_data(quantile_example_data_long, overwrite = TRUE)
# a <- eval_forecasts(quantile_example_data_long, by = c("model"), summarised = TRUE)
#
#
# # make new quantile example_data_wide
# quantile_to_wide <- function(data) {
#   data.table::dcast(data, ... ~ boundary + range,
#                     value.var = "predictions")
# }
#
# data <- quantile_to_wide(quantile_example_data_long)
# quantile_example_data_wide <- data[order(model, id)]
#
#
# usethis::use_data(quantile_example_data_wide, overwrite = TRUE)
#
# a <- eval_forecasts(quantile_example_data_wide, by = c("model", "horizon"), summarised = TRUE)
#
