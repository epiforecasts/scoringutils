#' Quantile Example Data
#'
#' A data set with predictions for different quantities relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 5,152 rows and 10 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{true_value}{true observed values}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{quantile}{quantile of the corresponding prediction}
#'   \item{prediction}{quantile predictions}
#'   \item{horizon}{forecast horizon in days}
#'
#' }
"quantile_example_data"


#' Range Forecast Example Data (Long Format)
#'
#' A data set with predictions with different interval ranges relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 5,419 rows and 12 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{true_value}{true observed values}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{prediction}{value for the lower or upper bound of the given prediction interval}
#'   \item{horizon}{forecast horizon in days}
#'   \item{boundary}{indicate lower or upper bound of prediction interval}
#'   \item{range}{range of the corresponding prediction interval}
#' }
"range_example_data_long"



#' Range Forecast Example Data (Wide Format)
#'
#' A data set with predictions with different interval ranges relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 346 rows and 28 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{true_value}{true observed values}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{horizon}{forecast horizon in days}
#'   \item{lower_0}{prediction for the lower bound of the 0\% interval range (median)}
#'   \item{lower_10}{prediction for the lower bound of the 10\% interval range}
#'   \item{lower_20}{prediction for the lower bound of the 20\% interval range}
#'   \item{lower_30}{prediction for the lower bound of the 30\% interval range}
#'   \item{lower_40}{prediction for the lower bound of the 40\% interval range}
#'   \item{lower_50}{prediction for the lower bound of the 50\% interval range}
#'   \item{lower_60}{prediction for the lower bound of the 60\% interval range}
#'   \item{lower_70}{prediction for the lower bound of the 70\% interval range}
#'   \item{lower_80}{prediction for the lower bound of the 80\% interval range}
#'   \item{lower_90}{prediction for the lower bound of the 90\% interval range}
#'   \item{upper_0}{prediction for the upper bound of the 0\% interval range}
#'   \item{upper_10}{prediction for the upper bound of the 1\% interval range}
#'   \item{upper_20}{prediction for the upper bound of the 20\% interval range}
#'   \item{upper_30}{prediction for the upper bound of the 30\% interval range}
#'   \item{upper_40}{prediction for the upper bound of the 40\% interval range}
#'   \item{upper_50}{prediction for the upper bound of the 50\% interval range}
#'   \item{upper_60}{prediction for the upper bound of the 60\% interval range}
#'   \item{upper_70}{prediction for the upper bound of the 70\% interval range}
#'   \item{upper_80}{prediction for the upper bound of the 80\% interval range}
#'   \item{upper_90}{prediction for the upper bound of the 90\% interval range}
#' }
"range_example_data_wide"


#' Range Forecast Example Data (Semi-Wide Format)
#'
#' A data set with predictions with different interval ranges relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 5,419 rows and 12 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{true_value}{true observed values}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{horizon}{forecast horizon in days}
#'   \item{range}{range of the corresponding prediction interval}
#'   \item{lower}{prediction for the lower bound of the corresponding interval}
#'   \item{upper}{prediction for the upper bound of the corresponding interval}
#' }
"range_example_data_semi_wide"


#' Continuous Forecast Example Data
#'
#' A data set with continuous predictions in a sample-based format relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 13,429 rows and 10 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{horizon}{forecast horizon in days}
#'   \item{prediction}{prediction value for the corresponding sample}
#'   \item{sample}{id for the corresponding sample}
#'   \item{true_value}{true observed values}
#' }
"continuous_example_data"


#' Integer Forecast Example Data
#'
#' A data set with integer predictions in a sample-based format relevant in the
#' 2020 UK Covid-19 epidemic.
#'
#' @format A data frame with 13,429 rows and 10 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{horizon}{forecast horizon in days}
#'   \item{prediction}{prediction value for the corresponding sample}
#'   \item{sample}{id for the corresponding sample}
#'   \item{true_value}{true observed values}
#' }
"integer_example_data"


#' Binary Forecast Example Data
#'
#' A data set with (constructed) binary predictions relevant in the
#' 2020 UK Covid-19 epidemic.
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
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{horizon}{forecast horizon in days}
#'   \item{prediction}{probability prediction that true value would be 1}
#'   \item{true_value}{true observed values}
#' }
"binary_example_data"


#' Quantile Example Data - Forecasts only
#'
#' A data set with predictions for different quantities relevant in the
#' 2020 UK Covid-19 epidemic, but no true_values
#'
#' @format A data frame with 7,581 rows and 9 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{creation_date}{date on which the forecast was made}
#'   \item{quantile}{quantile of the corresponding prediction}
#'   \item{prediction}{quantile predictions}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{horizon}{forecast horizon in days}
#'
#' }
"example_quantile_forecasts_only"


#' Truth data only
#'
#' A data set with truth data for different quantities relevant in the
#' 2020 UK Covid-19 epidemic, but no predictions
#'
#' @format A data frame with 140 rows and 5 columns:
#' \describe{
#'   \item{value_date}{the date for which a prediction was made}
#'   \item{value_type}{the target to be predicted (short form)}
#'   \item{geography}{the region for which a prediction was made}
#'   \item{value_desc}{long form description of the prediction target}
#'   \item{true_value}{true observed values}
#'
#' }
"example_truth_data_only"




