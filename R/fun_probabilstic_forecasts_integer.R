#' PIT
#'
#' @description Performs an Anderson-Darling test for uniformity for a
#' randomised PIT histogram using predictive Monte-Carlo samples
#'
#' See Eqs. 1 and 2 in Czado, Geniting and Held (2009),
#' Predictive Model Assessment for Count Data,
#' Biometrics Vol. 65, No. 4 (Dec., 2009), pp. 1254-1261
#'
#' @param true_values A vector with the true observed values of size n
#' @param samples nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param num_bins the number of bins in the PIT histogram.
#' If not given, will use the square root of n
#' @param n_replicates the number of tests to perform,
#' each time re-randomising the PIT
#' @return list
#' @importFrom stats runif sd
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @importFrom goftest ad.test
#' @export
#'
#' @examples

PIT <- function(true_values,
                samples,
                num_bins = NULL,
                n_replicates = 20) {

    n <- length(true_values)
    n_pred <- ncol(samples)

    # calculate emipirical cumulative distribution function as
    # Portion of (y_true <= y_predicted)
    P_x <- vapply(seq_along(true_values),
                  function(i) {
                    sum(samples[i,] <= true_values[i]) / n_pred
                  },
                  .0)

    # epirical cdf for (y-1). Only used for for integer-valued
    # predictions and true-values.
    P_xm1 <- vapply(seq_along(true_values),
                    function(i) {
                      sum(samples[i,] <= true_values[i] - 1) / n_pred
                    },
                    .0)

    u <- replicate(n_replicates, P_xm1 + runif(n) * (P_x - P_xm1))

    if (is.null(num_bins)) {
      num_bins = round(sqrt(n))
    }

    hist_PIT = ggplot(as.data.frame(rowMeans(u)),
                      aes(x = rowMeans(u))) + geom_histogram(color = 'darkblue',
                                                             fill = 'lightblue',
                                                             bins = num_bins)

    p_values = apply(
      u,
      MARGIN = 2,
      FUN = function (x) {
        goftest::ad.test(x)$p.value
      }
    )

    calibration <- data.frame(mean = mean(p_values),
                              sd = sd(p_values))

    return(list(p_values = p_values,
                hist_PIT = hist_PIT,
                calibration = calibration))
}


#' Determines sharpness of an incidence forecast as the width of the prediction interval
#'
#' explanation missing
#' @param samples nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @importFrom stats mad
#' @return data frame with sharpness for each interval by date
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @export

sharpness <- function (samples) {
  sharpness <- apply(samples, MARGIN = 1, mad)
  return(sharpness)
  # return(data.frame(date=as.Date(rownames(dat)),
  #                   sharpness=sharpness))
}


#' Determines bias of an incidence forecast from predictive Monte-Carlo
#' samples as the proportion of predictive samples greater than the data
#' @param true_values A vector with the true observed values of size n
#' @param samples nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return data frame with bias by date
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#'
#' @export


bias <- function(true_values, samples) {

  n_pred <- ncol(samples)
  # empirical cdf
  P_x <- vapply(seq_along(true_values),
                function(i) {
                  sum(samples[i,] <= true_values[i]) / n_pred
                },
                .0)

  # empirical cdf for (y-1)
  P_xm1 <- vapply(seq_along(true_values),
                  function(i) {
                    sum(samples[i,] <= true_values[i] - 1) / n_pred
                  },
                  .0)

  res = 1 - (P_x + P_xm1)
  return(res)
}



#' Wrapper around the scoringRules::dss_sample() function.
#' @param true_values A vector with the true observed values of size n
#' @param samples nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return data frame with bias by date
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @export


dss <- function(true_values, samples) {
  scoringRules::dss_sample(y, dat)
}









