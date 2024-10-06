#' @title Probability integral transformation (data.frame version)
#'
#' @description
#' Compute the Probability Integral Transformation (PIT) for
#' validated forecast objects.
#'
#' @inherit score params
#' @param by Character vector with the columns according to which the
#' PIT values shall be grouped. If you e.g. have the columns 'model' and
#' 'location' in the input data and want to have a PIT histogram for
#' every model and location, specify `by = c("model", "location")`.
#' @inheritParams pit_sample
#' @return A data.table with PIT values according to the grouping specified in
#' `by`.
#' @examples
#' example <- as_forecast_sample(example_sample_continuous)
#' result <- get_pit(example, by = "model")
#' plot_pit(result)
#'
#' # example with quantile data
#' example <- as_forecast_quantile(example_quantile)
#' result <- get_pit(example, by = "model")
#' plot_pit(result)
#' @export
#' @keywords scoring
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
get_pit <- function(forecast, by, ...) {
  UseMethod("get_pit")
}


#' @rdname get_pit
#' @importFrom cli cli_abort
#' @export
get_pit.default <- function(forecast, by, ...) {
  cli_abort(c(
    "!" = "The input needs to be a valid forecast object represented as quantiles or samples." # nolint
  ))
}


#' @title PIT histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param pit Either a vector with the PIT values, or a data.table as
#'   produced by [get_pit()].
#' @param num_bins The number of bins in the PIT histogram, default is "auto".
#'   When `num_bins == "auto"`, [plot_pit()] will either display 10 bins, or it
#'   will display a bin for each available quantile in case you passed in data in
#'   a quantile-based format.
#'   You can control the number of bins by supplying a number. This is fine for
#'   sample-based pit histograms, but may fail for quantile-based formats. In this
#'   case it is preferred to supply explicit breaks points using the `breaks`
#'   argument.
#' @param breaks Numeric vector with the break points for the bins in the
#'   PIT histogram. This is preferred when creating a PIT histogram based on
#'   quantile-based data. Default is `NULL` and breaks will be determined by
#'   `num_bins`. If `breaks` is used, `num_bins` will be ignored.
#' @importFrom stats as.formula
#' @importFrom ggplot2 geom_col
#' @importFrom stats density
#' @return A ggplot object with a histogram of PIT values
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' library(magrittr) # pipe operator
#'
#' # PIT histogram in vector based format
#' observed <- rnorm(30, mean = 1:30)
#' predicted <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit_sample(observed, predicted)
#' plot_pit(pit)
#'
#' # quantile-based pit
#' pit <- example_quantile %>%
#'   as_forecast_quantile() %>%
#'   get_pit(by = "model")
#' plot_pit(pit, breaks = seq(0.1, 1, 0.1))
#'
#' # sample-based pit
#' pit <- example_sample_discrete %>%
#'   as_forecast_sample %>%
#'   get_pit(by = "model")
#' plot_pit(pit)
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat theme_light after_stat
#' @importFrom checkmate assert check_set_equal check_number
#' @export
plot_pit <- function(pit,
                     num_bins = "auto",
                     breaks = NULL) {
  assert(
    check_set_equal(num_bins, "auto"),
    check_number(num_bins, lower = 1)
  )
  assert_numeric(breaks, lower = 0, upper = 1, null.ok = TRUE)

  # vector-format is always sample-based, for data.frames there are two options
  if ("quantile_level" %in% names(pit)) {
    type <- "quantile-based"
  } else {
    type <- "sample-based"
  }

  # use breaks if explicitly given, otherwise assign based on number of bins
  if (!is.null(breaks)) {
    plot_quantiles <- unique(c(0, breaks, 1))
  } else if (is.null(num_bins) || num_bins == "auto") {
    # automatically set number of bins
    if (type == "sample-based") {
      num_bins <- 10
      width <- 1 / num_bins
      plot_quantiles <- seq(0, 1, width)
    }
    if (type == "quantile-based") {
      plot_quantiles <- unique(c(0, pit$quantile_level, 1))
    }
  } else {
    # if num_bins is explicitly given
    width <- 1 / num_bins
    plot_quantiles <- seq(0, 1, width)
  }

  # function for data.frames
  if (is.data.frame(pit)) {
    facet_cols <- get_forecast_unit(pit)
    formula <- as.formula(paste("~", paste(facet_cols, collapse = "+")))

    # quantile version
    if (type == "quantile-based") {
      hist <- ggplot(
        data = pit[quantile_level %in% plot_quantiles],
        aes(x = quantile_level, y = pit_value)
      ) +
        geom_col(position = "dodge", colour = "grey") +
        facet_wrap(formula)
    }

    if (type == "sample-based") {
      hist <- ggplot(
        data = pit,
        aes(x = pit_value)
      ) +
        geom_histogram(
          aes(y = after_stat(width * density)),
          breaks = plot_quantiles,
          colour = "grey"
        ) +
        facet_wrap(formula)
    }
  } else {
    # non data.frame version
    hist <- ggplot(
      data = data.frame(x = pit, stringsAsFactors = TRUE),
      aes(x = x)
    ) +
      geom_histogram(
        aes(y = after_stat(width * density)),
        breaks = plot_quantiles,
        colour = "grey"
      )
  }

  hist <- hist +
    xlab("PIT") +
    ylab("Frequency") +
    theme_scoringutils()

  return(hist)
}
