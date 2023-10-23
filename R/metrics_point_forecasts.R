#' @title Absolute Error of the Median (Sample-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{%
#'   \textrm{abs}(\textrm{observevd} - \textrm{median\_prediction})
#' }{%
#'   abs(observed - median_prediction)
#' }
#'
#' @param observed A vector with the observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the number of Monte
#' Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @return vector with the scoring values
#' @seealso [ae_median_quantile()], [abs_error()]
#' @importFrom stats median
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_sample(observed, predicted_values)
#' @export
#' @keywords metric

ae_median_sample <- function(observed, predicted) {
  median_predictions <- apply(
    as.matrix(predicted), MARGIN = 1, FUN = median # this is rowwise
  )

  ae_median <- abs(observed - median_predictions)

  return(ae_median)
}

#' @title Squared Error of the Mean (Sample-based Version)
#'
#' @description
#' Squared error of the mean calculated as
#'
#' \deqn{
#'   \textrm{mean}(\textrm{observed} - \textrm{prediction})^2
#' }{
#'   mean(observed - mean_prediction)^2
#' }
#'
#' @param observed A vector with the observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the number of Monte
#' Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @return vector with the scoring values
#' @seealso [squared_error()]
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' se_mean_sample(observed, predicted_values)
#' @export
#' @keywords metric

se_mean_sample <- function(observed, predicted) {
  mean_predictions <- rowMeans(as.matrix(predicted))
  se_mean <- (observed - mean_predictions)^2

  return(se_mean)
}


#' @title Absolute Error of the Median (Quantile-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{prediction})
#' }{
#'   abs(observed - median_prediction)
#' }
#'
#' The function was created for internal use within [score()], but can also
#' used as a standalone function.
#'
#' @param predicted numeric vector with predictions, corresponding to the
#' quantiles in a second vector, `quantiles`.
#' @param quantiles numeric vector that denotes the quantile for the values
#' in `predicted`. Only those predictions where `quantiles == 0.5` will
#' be kept. If `quantiles` is `NULL`, then all `predicted` and
#' `observed` will be used (this is then the same as [abs_error()])
#' @return vector with the scoring values
#' @seealso [ae_median_sample()], [abs_error()]
#' @importFrom stats median
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_quantile(observed, predicted_values, quantiles = 0.5)
#' @export
#' @keywords metric

ae_median_quantile <- function(observed, predicted, quantiles = NULL) {
  if (!is.null(quantiles)) {
    if (!any(quantiles == 0.5) && !anyNA(quantiles)) {
      return(NA_real_)
      warning(
        "in order to compute the absolute error of the median, `0.5` must be ",
        "among the quantiles given. Maybe you want to use `abs_error()`?"
      )
    }
    observed <- observed[quantiles == 0.5]
    predicted <- predicted[quantiles == 0.5]
  }
  abs_error_median <- abs(observed - predicted)
  return(abs_error_median)
}




#' @title Absolute Error
#'
#' @description
#' Calculate absolute error as
#'
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{median\_prediction})
#' }{
#'   abs(observed - prediction)
#' }
#'
#' @return vector with the absolute error
#' @inheritParams ae_median_quantile
#' @seealso [ae_median_sample()], [ae_median_quantile()]
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' abs_error(observed, predicted_values)
#' @export
#' @keywords metric

abs_error <- function(observed, predicted) {
  return(abs(observed - predicted))
}


#' @title Squared Error
#'
#' @description
#' Squared Error SE calculated as
#'
#' \deqn{
#'   (\textrm{observed} - \textrm{predicted})^2
#' }{
#'   (observed - predicted)^2
#' }
#'
#' @param predicted A vector with predicted values of size n
#' @return vector with the scoring values
#' @inheritParams ae_median_sample
#' @export
#' @keywords metric
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' squared_error(observed, predicted_values)

squared_error <- function(observed, predicted) {
  assert_input_point(observed, predicted)
  se <- (observed - predicted)^2
  return(se)
}
