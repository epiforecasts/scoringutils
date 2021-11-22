#' @title Check forecasts
#'
#' @description missing
#'
#' @details missing
#'
#' @param data A data.frame or similiar as would be used as input to [eval_forecasts()]
#'
#' @return some output that tells you what scoringutils thinks you want to do.
#'
#' @importFrom data.table ':=' as.data.table
#' @importFrom methods hasArg
#'
#' @examples
#'
#' ## Probability Forecast for Binary Target
#' binary_example <- data.table::setDT(scoringutils::binary_example_data)
#' eval <- scoringutils::eval_forecasts(binary_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.5), sd = TRUE,
#'                                      verbose = FALSE)
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export

# questions:
# should this function also change the data, or should that part be duplicated in eval_forecasts?
# should this function also take in the other eval_forecasts arguments, e.g summarise_by etc.
# should this also do the checks for the pairwise_comparisons?
# should this also check available metrics?

check_forecasts <- function(data) {

  check <- list()

  # check data looks ok and remove columns with no prediction or no true value
  data <- check_clean_data(data)
  if (nrow(data) == 0) {
    "After cleaning, no observations to score are left"
  }

  # obtain unit of a single forecast
  protected_columns <- c("prediction", "true_value", "sample", "quantile",
                         "range", "boundary")
  obs_unit <- setdiff(colnames(data), protected_columns)
  check[["forecast unit"]] <- obs_unit

  # obtain truth type
  if (all.equal(data$true_value, as.integer(data$true_value)) == TRUE) {
    if (all(data$true_value %in% c(0,1)) && all(data$prediction >= 0) && all(data$prediction <= 1)) {
      check[["target_type"]] = "binary"
    } else {
      check[["target_type"]] = "integer"
    }
  } else {
    check[["target_type"]] = "continuous"
  }

  # obtain prediction type
  if (any(grepl("lower", names(data))) | "boundary" %in% names(data) |
      "quantile" %in% names(data) | "range" %in% names(data)) {
    check[["prediction_type"]] <- "quantile"
  } else if (all.equal(data$prediction, as.integer(data$prediction)) == TRUE) {
    check[["prediction_type"]] <- "integer"
  } else {
    check[["prediction_type"]] <- "continuous"
  }

  # check what format is has right now and tell user to convert it.
  if (!any(c("quantile", "sample") %in% colnames(data))) {
    if ("range" %in% colnames(data) | grepl("lower_" %in% colnames(data))) {
      warning("It seems like you have a format based on forecast intervals (see `example_data_long`, `example_data_semi_wide`, `example_data_wide`). You need to convert this to a quantile-based format first using `range_wide_to_long()` and `range_long_to_quantile()`")
    } else if (!check[["target_type"]] == "binary") {
      warning("Missing a column called quantile or sample")
    }
  }

  # check whether there is more than one prediction for the same target, i.e.
  # the length of prediction is greater 1 for a sample / quantile for
  # a single forecast
  type <- c("sample", "quantile")[c("sample", "quantile") %in% colnames(data)]
  data[, InternalDuplicateCheck := .N, by = c(obs_unit, type)]

  # print something if there are duplicates
  if (any(data$duplicatecheck) > 1) {
    warning("There are instances with more than one forecast. This can't be right and needs to be resolved. Maybe you need to run `unique(data)` or check the unit of a single forecast and add missing columns?")
    data <- data[InternalDuplicateCheck > 1]
    return(data[])
  }


  # some checks whether there are the same number of quantiles, samples

  # check whether all models have the same number of locations etc?

  # check whether all models have a point forecast?

  # check whether there is a model column present. And if not, state what that means



  # print results to user ------------------------------------------------------
  paste("Based on your input, scoringutils thinks: ")

  # unit of a single forecast
  paste0("- there is one unique forecast per `",
        paste(check[["forecast unit"]], collapse = "`, `"), "`. ",
              "If this is not the case, please DELETE UNNECESSARY columns or add new ones.")

  # prediction and target types
  paste0("- you want to score a `", check[["target_type"]], "` target ",
        "using a `", check[["prediction_type"]], "` prediction format.")



}






#   # error handling for relative skill computation
#   # should probably wrap this in a function warn_if_verbose(warning, verbose)
#   if (compute_relative_skill) {
#     if (!("model" %in% colnames(data))) {
#       if (verbose) {
#         warning("to compute relative skills, there must column present called 'model'. Relative skill will not be computed")
#       }
#       compute_relative_skill <- FALSE
#     }
#     models <- unique(data$model)
#     if (length(models) < 2 + (!is.null(baseline))) {
#       if (verbose) {
#         warning("you need more than one model non-baseline model to make model comparisons. Relative skill will not be computed")
#       }
#       compute_relative_skill <- FALSE
#     }
#     if (!is.null(baseline) && !(baseline %in% models)) {
#       if (verbose){
#         warning("The baseline you provided for the relative skill is not one of the models in the data. Relative skill will not be computed")
#       }
#       compute_relative_skill <- FALSE
#     }
#     if (rel_skill_metric != "auto" && !(rel_skill_metric %in% list_of_avail_metrics())) {
#       if (verbose) {
#         warning("argument 'rel_skill_metric' must either be 'auto' or one of the metrics that can be computed. Relative skill will not be computed")
#       }
#       compute_relative_skill <- FALSE
#     }
#   }
#
#
#
#   # check that the arguments in by and summarise_by are actually present
#   if (!all(c(by, summarise_by) %in% c(colnames(data), "range", "quantile"))) {
#     not_present <- setdiff(unique(c(by, summarise_by)),
#                            c(colnames(data), "range", "quantile"))
#     msg <- paste0("The following items in `by` or `summarise_by` are not",
#                   "valid column names of the data: '",
#                   paste(not_present, collapse = ", "),
#                   "'. Check and run `eval_forecasts()` again")
#     stop(msg)
#   }
#
#
#
#   # check metrics to be computed
#   available_metrics <- list_of_avail_metrics()
#   if (is.null(metrics)) {
#     metrics <- available_metrics
#   } else {
#     if (!all(metrics %in% available_metrics)) {
#       if (verbose) {
#         msg <- paste("The following metrics are not currently implemented and",
#                      "will not be computed:",
#                      paste(setdiff(metrics, available_metrics), collapse = ", "))
#       }
#       warning(msg)
#     }
#   }
#   }
# }






#' @title Clean forecast data
#'
#' @description Helper function to check that the input is in fact a data.frame
#' or similar and remove rows with no value for `prediction` or `true_value`
#'
#' @param data A data.frame or similar as it gets passed to [eval_forecasts()].
#'
#' @return cleaned data.table
#'
#' @importFrom data.table as.data.table

check_clean_data <- function(data) {
  if (!("data.frame" %in% class(data))) {
    stop("you should provide a data.frame or similar")
  }
  data <- as.data.table(data)

  # make sure necessary columns are present
  if (!all(c("true_value", "prediction") %in% colnames(data))) {
    stop("Data needs to have columns called `true_value` and `prediction`")
  }

  # remove rows where prediction or true value are NA
  if (any(is.na(data$true_value))) {
    warning("There are NA values in the true values provided. These will be removed")
  }
  data <- data[!is.na(true_value)]
  if (any(is.na(data$prediction))) {
    warning("There are NA values in the prediction values provided. These will be removed")
  }
  data <- data[!is.na(prediction)]
  if (nrow(data) == 0) {
    warning("After removing all NA true values and predictions, there were no observations left")
    # maybe this should be an error, but I left it like this in case someone wants to use eval_forecasts
    # in other code
  }

  return(data)
}
