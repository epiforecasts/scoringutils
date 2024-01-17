#' @title Select Scoring Rules From A List of Possible Scoring Rules
#' @description Helper function to return only the scoring rules selected by
#' the user from a list of possible scoring rules.
#' @param rules A list of scoring rules.
#' @param select A character vector of scoring rules to select from the list.
#' If `select` is `NULL` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#' If `select` is not `NULL`, this argument is ignored.
#' @return A list of scoring rules.
#' @keywords metric
#' @importFrom checkmate assert_subset assert_list
#' @export
#' @examples
#' select_rules(
#'   rules = rules_binary(),
#'   select = "brier_score"
#' )
#' select_rules(
#'   rules = rules_binary(),
#'   exclude = "log_score"
#' )
select_rules <- function(rules, select = NULL, exclude = NULL) {
  assert_character(x = c(select, exclude), null.ok = TRUE)
  assert_list(rules, names = "named")
  allowed <- names(rules)

  if (is.null(select) && is.null(exclude)) {
    return(rules)
  } else if (is.null(select)) {
    assert_subset(exclude, allowed)
    select <- allowed[!allowed %in% exclude]
    return(rules[select])
  } else {
    assert_subset(select, allowed)
    return(rules[select])
  }
}


#' @title Scoring Rules for Binary Forecasts
#' @description Helper function that returns a named list of default
#' scoring rules suitable for binary forecasts.
#'
#' The default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "log_score" = [logs_binary()]
#' @inherit select_rules params return
#' @export
#' @keywords metric
#' @examples
#' rules_binary()
#' rules_binary(select = "brier_score")
#' rules_binary(exclude = "log_score")
rules_binary <- function(select = NULL, exclude = NULL) {
  all <- list(
    brier_score = brier_score,
    log_score = logs_binary
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}


#' @title Scoring Rules for Categorical Forecasts
#' @description Helper function that returns a named list of default
#' scoring rules suitable for categorical forecasts.
#'
#' The default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "log_score" = [logs_binary()]
#' @inherit select_rules params return
#' @export
#' @keywords metric
#' @examples
#' rules_categorical()
#' rules_categorical(select = "brier_score")
#' rules_categorical(exclude = "log_score")
rules_categorical <- function(select = NULL, exclude = NULL) {
  all <- list(
    # brier_score = brier_score,
    # log_score = logs_binary
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}


#' @title Scoring Rules for Point Forecasts
#' @description Helper function that returns a named list of default
#' scoring rules suitable for point forecasts.
#'
#' The default scoring rules are:
#' - "ae_point" = [ae()][Metrics::ae()]
#' - "se_point" = [se()][Metrics::se()]
#' - "ape" = [ape()][Metrics::ape()]
#' @inherit select_rules params return
#' @export
#' @keywords metric
#' @examples
#' rules_point()
#' rules_point(select = "ape")
rules_point <- function(select = NULL, exclude = NULL) {
  all <- list(
    ae_point = Metrics::ae,
    se_point = Metrics::se,
    ape = Metrics::ape
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}


#' @title Scoring Rules for Sample-Based Forecasts
#' @description Helper function that returns a named list of default
#' scoring rules suitable for forecasts in a sample-based format
#'
#' The default scoring rules are:
#' - "mad" = [mad_sample()]
#' - "bias" = [bias_sample()]
#' - "dss" = [dss_sample()]
#' - "crps" = [crps_sample()]
#' - "log_score" = [logs_sample()]
#' - "mad" = [mad_sample()]
#' - "ae_median" = [ae_median_sample()]
#' - "se_mean" = [se_mean_sample()]
#' @inherit select_rules params return
#' @export
#' @keywords metric
#' @examples
#' rules_sample()
#' rules_sample(select = "mad")
rules_sample <- function(select = NULL, exclude = NULL) {
  all <- list(
    bias = bias_sample,
    dss = dss_sample,
    crps = crps_sample,
    log_score = logs_sample,
    mad = mad_sample,
    ae_median = ae_median_sample,
    se_mean = se_mean_sample
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}


#' @title Scoring Rules for Quantile-Based Forecasts
#' @description Helper function that returns a named list of default
#' scoring rules suitable for forecasts in a quantile-based format
#'
#' The default scoring rules are:
#' - "wis" = [wis]
#' - "overprediction" = [overprediction()]
#' - "underprediction" = [underprediction()]
#' - "dispersion" = [dispersion()]
#' - "bias" = [bias_quantile()]
#' - "interval_coverage_50" = [interval_coverage()]
#' - "interval_coverage_90" = function(...) \{
#'      run_safely(..., range = 90, fun = [interval_coverage])
#'   \}
#' - "interval_coverage_deviation" = [interval_coverage_deviation()],
#' - "ae_median" = [ae_median_quantile()]
#'
#' Note: The `coverage_90` scoring rule is created as a wrapper around
#' [interval_coverage()], making use of the function [run_safely()].
#' This construct allows the function to deal with arbitrary arguments in `...`,
#' while making sure that only those that [interval_coverage()] can
#' accept get passed on to it. `range = 90` is set in the function definition,
#' as passing an argument `range = 90` to [score()] would mean it would also
#' get passed to `coverage_50`.
#' @inherit select_rules params return
#' @export
#' @keywords metric
#' @examples
#' rules_quantile()
#' rules_quantile(select = "wis")
rules_quantile <- function(select = NULL, exclude = NULL) {
  all <- list(
    wis = wis,
    overprediction = overprediction,
    underprediction = underprediction,
    dispersion = dispersion,
    bias = bias_quantile,
    interval_coverage_50 = interval_coverage,
    interval_coverage_90 = function(...) {
      run_safely(..., range = 90, fun = interval_coverage)
    },
    interval_coverage_deviation = interval_coverage_deviation,
    ae_median = ae_median_quantile
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}
