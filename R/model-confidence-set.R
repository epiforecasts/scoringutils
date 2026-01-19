#' @title Compute model confidence set from pairwise comparisons
#'
#' @description
#'
#' Identify the set of models that cannot be statistically distinguished from
#' the best model at a given confidence level. This implements the Model
#' Confidence Set concept from Hansen, Lunde & Nason (2011) using the
#' pairwise comparison results from [get_pairwise_comparisons()].
#'
#' The procedure identifies the best model (lowest relative skill) and tests
#' whether each other model is significantly worse. Models where the null
#' hypothesis of equal performance cannot be rejected are included in the MCS.
#'
#' *Interpretation*
#'
#' Models in the MCS are those that cannot be ruled out as being the best at
#' the specified confidence level. A larger MCS indicates more uncertainty
#' about which model is truly best, often due to limited data, similar
#' performance, or high score variability.
#'
#' *Conditional MCS*
#'
#' To compute separate confidence sets for different subgroups (e.g., locations,
#' target types, epidemic phases), use the `by` argument in
#' [get_pairwise_comparisons()].
#'
#' *Sequential MCS*
#'
#' To track how the MCS evolves over time, filter scores cumulatively before
#' computing pairwise comparisons.
#'
#' @param pairwise A data.table of pairwise comparisons as produced by
#'   [get_pairwise_comparisons()].
#' @param compare Character vector with a single column name that defines the
#'   elements being compared. By default this is `"model"`.
#' @param alpha Numeric, the significance level for determining MCS membership.
#'   By default this is 0.1, meaning the MCS contains the best models with
#'   90 percent confidence.
#' @returns A data.table with the results of the model confidence set
#' computation, containing the relative skill (`relative_skill`), the p-value
#' from comparison against the best model (`pval_vs_best`), the adjusted
#' p-value using the Holm method (`adj_pval_vs_best`), and whether the model
#' is included in the MCS (`in_mcs`). Any grouping columns present in the
#' pairwise input are preserved.
#' @references
#' Hansen, P. R., Lunde, A., & Nason, J. M. (2011). The model confidence set.
#' Econometrica, 79(2), 453-497. \doi{10.3982/ECTA5771}
#'
#' Arnold, S., Henzi, A., & Ziegel, J. F. (2024). Sequential model confidence
#' sets. \url{https://arxiv.org/abs/2404.18678}
#'
#' Borusyak, K., & Jaravel, X. (2025). Conditional method confidence set.
#' \url{https://arxiv.org/abs/2505.21278}
#' @seealso [get_pairwise_comparisons()] for generating the input.
#'
#' @importFrom data.table as.data.table setnames rbindlist setcolorder
#' @importFrom checkmate assert_character assert_number assert_subset
#'   assert_data_frame
#' @importFrom cli cli_abort
#' @importFrom stats p.adjust
#' @export
#' @keywords scoring
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' # Basic MCS
#' scores |>
#'   get_pairwise_comparisons() |>
#'   get_model_confidence_set()
#'
#' # Conditional MCS by target type
#' scores |>
#'   get_pairwise_comparisons(by = "target_type") |>
#'   get_model_confidence_set()
#'
#' # More stringent confidence level
#' scores |>
#'   get_pairwise_comparisons() |>
#'   get_model_confidence_set(alpha = 0.05)

get_model_confidence_set <- function(
  pairwise,
  compare = "model",
  alpha = 0.1
) {
  # Input validation
  assert_data_frame(pairwise)
  pairwise <- data.table::as.data.table(pairwise)
  assert_number(alpha, lower = 0, upper = 1)

  # Use compare from attribute if available and user didn't override
  assert_character(compare, len = 1)
  pw_compare <- attr(pairwise, "compare")
  if (!is.null(pw_compare) && compare == "model") {
    compare <- pw_compare
  }
  assert_subset(compare, names(pairwise))

  # Check required columns exist
  required_cols <- c(compare, "compare_against", "pval")
  missing_cols <- setdiff(required_cols, names(pairwise))
  if (length(missing_cols) > 0) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Missing required columns: {.val {missing_cols}}",
        "i" = "Input should be output from {.fn get_pairwise_comparisons}."
      )
    )
    #nolint end
  }

  # Get metric from attribute or detect from column name

  metric <- attr(pairwise, "metric")
  if (is.null(metric)) {
    # Fall back to detecting from column names
    skill_col <- grep("_relative_skill$", names(pairwise), value = TRUE)
    skill_col <- setdiff(skill_col, grep("_scaled_relative_skill$", skill_col,
                                         value = TRUE))
    if (length(skill_col) == 0) {
      #nolint start: keyword_quote_linter
      cli_abort(
        c(
          "!" = "No relative skill column found.",
          "i" = "Expected a column ending in '_relative_skill'.",
          "i" = "Available columns: {.val {names(pairwise)}}",
          "i" = "Input should be output from {.fn get_pairwise_comparisons}."
        )
      )
      #nolint end
    }
  } else {
    skill_col <- paste0(metric, "_relative_skill")
  }

  # Get grouping columns from attribute
  group_cols <- attr(pairwise, "by")
  if (length(group_cols) == 0) {
    group_cols <- NULL
  }

  # Process each group (or all data if no grouping)
  if (is.null(group_cols)) {
    result <- compute_mcs_one_group(pairwise, compare, skill_col, alpha)
  } else {
    split_pw <- split(pairwise, by = group_cols)
    results <- lapply(names(split_pw), function(grp) {
      res <- compute_mcs_one_group(split_pw[[grp]], compare, skill_col, alpha)
      for (col in group_cols) {
        res[[col]] <- split_pw[[grp]][[col]][1]
      }
      res
    })
    result <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  }

  # Reorder columns
  first_cols <- c(compare, group_cols)
  other_cols <- setdiff(names(result), first_cols)
  data.table::setcolorder(result, c(first_cols, other_cols))

  return(result[])
}


#' @title Compute MCS for one group from pairwise results
#'
#' @description
#'
#' Internal function that computes the model confidence set for a single group.
#' Called by [get_model_confidence_set()].
#'
#' @param pairwise Pairwise comparison results for one group.
#' @param compare Column name for models.
#' @param skill_col Name of relative skill column.
#' @param alpha Significance level.
#' @returns A data.table with MCS membership.
#' @keywords internal
compute_mcs_one_group <- function(pairwise, compare, skill_col, alpha) {
  # Get unique models and their relative skills
  models_dt <- unique(pairwise[, c(compare, skill_col), with = FALSE])
  data.table::setnames(models_dt, skill_col, "relative_skill")

  # Find the best model (lowest relative skill)
  best_idx <- which.min(models_dt$relative_skill)
  best_model <- models_dt[[compare]][best_idx]

  # Get p-values comparing each model to the best
  pvals_vs_best <- pairwise[
    compare_against == best_model,
    c(compare, "pval"),
    with = FALSE
  ]

  # Merge with models
  result <- merge(models_dt, pvals_vs_best, by = compare, all.x = TRUE)

  # The best model compared to itself has pval = 1
  result[get(compare) == best_model, pval := 1]

  # Rename for clarity
  data.table::setnames(result, "pval", "pval_vs_best")

  # Adjust p-values using Holm method for comparisons vs best
  non_best <- result[[compare]] != best_model
  #nolint start: object_usage_linter
  if (sum(non_best) > 0) {
    result[non_best, adj_pval_vs_best := p.adjust(pval_vs_best, method = "holm")]
  }
  result[!non_best, adj_pval_vs_best := 1]

  # Determine MCS membership
  result[, in_mcs := adj_pval_vs_best > alpha]
  #nolint end

  # Sort by relative skill
  result <- result[order(relative_skill)]

  return(result)
}
