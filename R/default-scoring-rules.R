#' @title Select Scoring Rules From A List of Possibilities
#' @description Helper function to return only the scoring rules selected by
#' the user from a list of possible scoring rules.
#' @param possibilities A list of scoring rules.
#' @param select A character vector of scoring rules to select from the list.
#' If `select = all` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#' If `select != "all"`, this argument is ignored.
#' @return A list of scoring rules.
#' @keywords internal
#' @importFrom checkmate assert_subset assert_list
#' @examples
#' scoringutils:::select_rules(
#'   possibilities = rules_binary(),
#'   select = "brier_score"
#' )
select_rules <- function(possibilities, select, exclude = NULL) {
  assert_character(x = c(select, exclude), null.ok = TRUE)
  assert_list(possibilities)
  allowed <- names(possibilities)

  if (select == "all" && is.null(exclude)) {
    return(possibilities)
  } else if (select == "all") {
    assert_subset(exclude, allowed)
    select <- allowed[!allowed %in% exclude]
    return(possibilities[select])
  } else {
    assert_subset(select, allowed)
    return(possibilities[select])
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
#' @keywords info
#' @examples
#' rules_binary()
#' rules_binary(select = "brier_score")
#' rules_binary(exclude = "log_score")
rules_binary <- function(select = "all", exclude = NULL) {
  all <- list(
    brier_score = brier_score,
    log_score = logs_binary
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
#' @keywords info
#' @examples
#' rules_point()
#' rules_point(select = "ape")
rules_point <- function(select = "all", exclude = NULL) {
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
#' @keywords info
#' @examples
#' rules_sample()
#' rules_sample(select = "mad")
rules_sample <- function(select = "all", exclude = NULL) {
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
#' - "coverage_50" = [interval_coverage_quantile()]
#' - "coverage_90" = \(...) \{
#'      run_safely(..., range = 90, fun = [interval_coverage_quantile])
#'   \}
#' - "coverage_deviation" = [interval_coverage_dev_quantile()],
#' - "ae_median" = [ae_median_quantile()]
#' @inherit select_rules params return
#' @export
#' @keywords info
#' @examples
#' rules_quantile()
#' rules_quantile(select = "wis")
rules_quantile <- function(select = "all", exclude = NULL) {
  all <- list(
    wis = wis,
    overprediction = overprediction,
    underprediction = underprediction,
    dispersion = dispersion,
    bias = bias_quantile,
    coverage_50 = interval_coverage_quantile,
    coverage_90 = \(...) {
      run_safely(..., range = 90, fun = interval_coverage_quantile)
    },
    coverage_deviation = interval_coverage_dev_quantile,
    ae_median = ae_median_quantile
  )
  selected <- select_rules(all, select, exclude)
  return(selected)
}
