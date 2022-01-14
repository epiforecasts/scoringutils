#' Quantile Example Data
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the European Forecast Hub.
#'
#' @format A data frame with
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{quantile}{quantile of the corresponding prediction}
#'   \item{prediction}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#' }
#' @source \url{https://github.com/epiforecasts/covid19-forecast-hub-europe/}
"example_quantile"


#' Continuous Forecast Example Data
#'
#' A data set with continuous predictions for COVID-19 cases and deaths constructed from data
#' submitted to the European Forecast Hub.
#'
#' @format A data frame with 13,429 rows and 10 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{prediction}{predicted value}
#'   \item{sample}{id for the corresponding sample}
#' }
"example_continuous"


#' Integer Forecast Example Data
#'
#' A data set with integer predictions for COVID-19 cases and deaths constructed from data
#' submitted to the European Forecast Hub.
#'
#' @format A data frame with 13,429 rows and 10 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{prediction}{predicted value}
#'   \item{sample}{id for the corresponding sample}
#' }
"example_integer"


#' Binary Forecast Example Data
#'
#' A data set with binary predictions for COVID-19 cases and deaths constructed from data
#' submitted to the European Forecast Hub.
#'
#' Predictions in the data set were constructed based on the continuous example
#' data by looking at the number of samples below the mean prediction.
#' The outcome was constructed as whether or not the actually
#' observed value was below or above that mean prediction.
#' This should not be understood as sound statistical practice, but rather
#' as a practical way to create an example data set.
#'
#' @format A data frame with 346 rows and 10 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{prediction}{predicted value}
#' }
"example_binary"


#' Quantile Example Data - Forecasts only
#'
#' A data set with quantile predictions for COVID-19 cases and deaths
#' submitted to the European Forecast Hub.
#'
#' @format A data frame with 7,581 rows and 9 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{quantile}{quantile of the corresponding prediction}
#'   \item{prediction}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#' }
"example_quantile_forecasts_only"


#' Truth data only
#'
#' A data set with truth values for COVID-19 cases and deaths
#' submitted to the European Forecast Hub.
#'
#' @format A data frame with 140 rows and 5 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#' }
"example_truth_only"

#' Summary information for selected metrics
#'
#' A data set with summary information on selected metrics implemented in
#' \pkg{scoringutils}
#' @keywords info
"metrics_summary"
