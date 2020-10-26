#' @title Pivot Quantile Forecasts From Long to Wide Format
#'
#' @description
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_long}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_wide}}). This can be useful e.g. for
#' plotting.
#'
#' @param data a data.frame following the specifications from
#' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' \code{\link{quantile_example_data_long}})
#' @return a data.frame in wide format
#' @importFrom data.table dcast
#' @export
#' @examples
#' long <- scoringutils::quantile_example_data_long
#' wide <- scoringutils::quantile_to_wide(long)
#'

quantile_to_wide <- function(data) {
  data.table::dcast(data, ... ~ boundary + range,
                    value.var = "prediction")
}



#' @title Pivot Quantile Forecasts From Wide to Long Format
#'
#' @description
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_wide}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_long}}). This can be useful e.g. for
#' plotting.
#'
#' @param data a data.frame following the specifications from
#' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' \code{\link{quantile_example_data_wide}})
#' @return a data.frame in long format
#' @importFrom data.table melt
#' @export
#' @examples
#' wide <- scoringutils::quantile_example_data_wide
#' long <- scoringutils::quantile_to_long(wide)
#'

quantile_to_long <- function(data) {
  colnames <- colnames(data)
  ranges <- colnames[grepl("lower", colnames) | grepl("upper", colnames)]

  id_vars <- colnames[!(colnames %in% ranges)]

  data <- data.table::melt(data,
                           id.vars = id_vars,
                           measure.vars = ranges,
                           variable.name = "range",
                           value.name = "prediction")
  data[, boundary := gsub("_.*", "", range)]
  data[, range := as.numeric(gsub("^.*?_","", range))]

  return(data)
}



#' @title Change Data from a Range Format to a Plain Quantile Format
#'
#' @description
#'
#' Transform data from a format that uses interval ranges to denote quantiles
#' to a format that uses quantiles only.
#'
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_long}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_long}}). This can be useful e.g. for
#' plotting. If you're data.frame is in a different format, consider running
#' \code{\link{quantile_to_wide}} first.
#'
#' @param data a data.frame following the specifications from
#' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' \code{\link{quantile_example_data_long}})
#' @param keep_range_col keep the range and boundary columns after
#' transformation (default is FALSE)
#' @return a data.frame in a plain quantile format
#' @importFrom data.table copy
#' @export
#' @examples
#' wide <- scoringutils::quantile_example_data_wide
#' long <- scoringutils::quantile_to_long(wide)
#'
#' plain_quantile <- range_to_quantile(long)
#'


range_to_quantile <- function(data,
                              keep_range_col = FALSE) {
  data <- data.table::as.data.table(data)

  # filter out duplicated median
  data <- data[!(range == 0 & boundary == "upper"), ]

  data[, quantile := ifelse(boundary == "lower",
                            round((100 - range) / 200, 10),
                            round((1 - (100 - range) / 200), 10))]

  if (!keep_range_col) {
    data[, c("range", "boundary") := NULL]
  }


  return(unique(data))
}


#' @title Change Data from a Plain Quantile Format to a Range Format
#'
#' @description
#'
#' Transform data from a format that uses quantiles only to one that uses
#' interval ranges to denote quantiles.
#'
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_plain}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_long}}).
#'
#' @param data a data.frame following the specifications shown in the example
#' \code{\link{quantile_example_data_long}})
#' @param keep_quantile_col keep the quantile column in the final
#' output after transformation (default is FALSE)
#' @return a data.frame in a long interval range format
#' @importFrom data.table copy
#' @export
#'
#' @examples
#' quantile_plain <- scoringutils::quantile_example_data_plain
#'
#' long <- scoringutils::quantile_to_range(quantile_plain)
#'

quantile_to_range <- function(data,
                              keep_quantile_col = TRUE) {
  data <- data.table::as.data.table(data)

  data[, boundary := ifelse(quantile <= 0.5, "lower", "upper")]
  data[, range := ifelse(boundary == "lower",
                         round((1 - 2 * quantile) * 100, 10),
                         round((2 * quantile - 1) * 100, 10))]

  # add median quantile
  median <- data[quantile == 0.5, ]
  median[, boundary := "upper"]

  data <- data.table::rbindlist(list(data, median))

  if (!keep_quantile_col) {
    data[, "quantile" := NULL]
  }


  return(data)
}




#' @title Change Data from a Sample Based Format to a Quantile Format
#'
#' @description
#'
#' Transform data from a format that is based on predictive samples to a format
#' based on plain quantiles.
#'
#'
#' @param data a data.frame with samples
#' @param quantiles a numeric vector of quantiles to extract
#' @param type type argument passed down to the quantile function. For more
#' information, see \code{\link{quantile}}
#' @return a data.frame in a long interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' example_data <- scoringutils::integer_example_data
#'
#' quantile_data <- sample_to_quantile(example_data)
#'



sample_to_quantile <- function(data,
                               quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               type = 7) {

  data <- data.table::as.data.table(data)

  reserved_columns <- c("prediction", "sample")
  by <- setdiff(colnames(data), reserved_columns)

  data <- data[, .(quantile = quantiles,
                   prediction = quantile(prediction, prob = quantiles,
                                          type = type)),
               by = by]

  return(data)
}



#' @title Change Data from a Sample Based Format to a Interval Range Format
#'
#' @description
#'
#' Transform data from a format that is based on predictive samples to a format
#' based on interval ranges
#'
#'
#' @param data a data.frame with samples
#' @param range a numeric vector of interval ranges to extract
#' (e.g. \code{c(0, 50, 90)})
#' @param type type argument passed down to the quantile function. For more
#' information, see \code{\link{quantile}}
#' @param keep_quantile_col keep quantile column, default is TRUE
#' @return a data.frame in a long interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' example_data <- scoringutils::integer_example_data
#'
#' quantile_data <- sample_to_range(example_data)
#'



sample_to_range <- function(data,
                            range = c(0, 50, 90),
                            type = 7,
                            keep_quantile_col = TRUE) {

  data <- data.table::as.data.table(data)

  lower_quantiles <- (100 - range) / 200
  upper_quantiles <- 1 - lower_quantiles
  quantiles <- sort(unique(c(lower_quantiles, upper_quantiles)))

  data <- sample_to_quantile(data,
                             quantiles = quantiles,
                             type = type)

  data <- quantile_to_range(data)

  return(data)
}

