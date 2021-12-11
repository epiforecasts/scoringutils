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


#' Range Forecast Example Data (Long Format)
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the European Forecast Hub.
#'
#' @format A data frame with:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{prediction}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{boundary}{indicate lower or upper bound of prediction interval}
#'   \item{range}{range of the corresponding prediction interval}
#' }
"example_range_long"



#' Range Forecast Example Data (Wide Format)
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the European Forecast Hub.
#'
#' @format A data frame with:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{lower_0}{prediction for the lower bound of the 0% interval range (median)}
#'   \item{lower_10}{prediction for the lower bound of the 10% interval range}
#'   \item{lower_20}{prediction for the lower bound of the 20% interval range}
#'   \item{lower_30}{prediction for the lower bound of the 30% interval range}
#'   \item{lower_40}{prediction for the lower bound of the 40% interval range}
#'   \item{lower_50}{prediction for the lower bound of the 50% interval range}
#'   \item{lower_60}{prediction for the lower bound of the 60% interval range}
#'   \item{lower_70}{prediction for the lower bound of the 70% interval range}
#'   \item{lower_80}{prediction for the lower bound of the 80% interval range}
#'   \item{lower_90}{prediction for the lower bound of the 90% interval range}
#'   \item{lower_95}{prediction for the lower bound of the 95% interval range}
#'   \item{lower_98}{prediction for the lower bound of the 98% interval range}
#'   \item{upper_0}{prediction for the upper bound of the 0% interval range}
#'   \item{upper_10}{prediction for the upper bound of the 1% interval range}
#'   \item{upper_20}{prediction for the upper bound of the 20% interval range}
#'   \item{upper_30}{prediction for the upper bound of the 30% interval range}
#'   \item{upper_40}{prediction for the upper bound of the 40% interval range}
#'   \item{upper_50}{prediction for the upper bound of the 50% interval range}
#'   \item{upper_60}{prediction for the upper bound of the 60% interval range}
#'   \item{upper_70}{prediction for the upper bound of the 70% interval range}
#'   \item{upper_80}{prediction for the upper bound of the 80% interval range}
#'   \item{upper_90}{prediction for the upper bound of the 90% interval range}
#'   \item{upper_95}{prediction for the upper bound of the 95% interval range}
#'   \item{upper_98}{prediction for the upper bound of the 98% interval range}
#' }
"example_range_wide"


#' Range Forecast Example Data (Semi-Wide Format)
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the European Forecast Hub.
#'
#' @format A data frame with 5,419 rows and 12 columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{true_value}{true observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{range}{range of the corresponding prediction interval}
#'   \item{lower}{prediction for the lower bound of the corresponding interval}
#'   \item{upper}{prediction for the upper bound of the corresponding interval}
#' }
"example_range_semi_wide"


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
