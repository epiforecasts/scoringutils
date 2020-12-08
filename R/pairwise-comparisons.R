#' @title Do pairwise Comparisons of Scores
#'
#' @description
#'
#' Make pairwise comparisons between models. The code for the pairwise
#' comparisons is inspired by an implementation by Johannes Bracher.
#'
#' The implementation of the permutation test follows the function
#' \link[surveillance]{permutationTest} from the `surveillance` package
#'  by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#'
#' @param scores A data.frame of unsummarised scores as produced by
#' \code{\link{eval_forecasts}}
#' @param var character vector of length one that denotes the column name
#' with the models or forecasters
#' to compare. Will probably be changed in the future such that a column
#' named "model" must be present.
#' @param metric A character vector of length one with the metric to do
#' the comparison on.
#' @param permutation_test logical. If TRUE, a permutation test is done
#' alongside the comparison. Default is FALSE
#' @param baseline character vector of length one that deontes
#' the baseline model against which to compare other models.
#' @return A ggplot2 object with a coloured table of summarised scores
#' @importFrom data.table as.data.table data.table setnames copy
#' @importFrom stats sd rbinom
#' @importFrom utils combn
#' @export
#' @author Johannes Bracher, https://jbracher.github.io/
#' @author Nikos Bosse
#' @examples
#' df <- data.frame(model = rep(c("model1", "model2", "model3"), each = 10),
#'                  id = rep(1:10),
#'                  interval_score = (abs(rnorm(30))),
#'                  aem = (abs(rnorm(30))))
#'
#' res <- scoringutils::pairwise_comparison(df, permutation_test = TRUE,
#'                                          baseline = "model1")
#' scoringutils::plot_pairwise_comparison(res, type = "ratio")
#'


pairwise_comparison <- function(scores,
                               var = "model", # alternatively (and probably better): just force user to have a column called "model" or "forecaster"
                               metric = "interval_score", # maybe the default can happen automatically
                               permutation_test = FALSE,
                               # also need to add arguments for the permutation test, i.e. ratio and number of permutations to do
                               baseline = NULL) { # comparison against a baseline model yet to be implemented

  # get list of models
  identifiers <- unique(scores[[var]])

  # identify unit of single observation
  # this basically recovers what was used in eval_forecasts as by argument
  # could think about adding a specific by argument to this function, but
  # really this should not be necessary I think
  all_metrics <- list_of_avail_metrics()
  by <- setdiff(names(scores), c(all_metrics, var))

  # for efficiency, remove everything that is not in c(by, var)
  scores <- data.table::as.data.table(scores)
  cols_to_remove <- setdiff(names(scores), c(by, var, metric))
  scores[, eval(cols_to_remove) := NULL]


  single_comparison <- function(model1, model2, permutation_test = FALSE) {
    # select only columns in c(by, var)
    a <- scores[model == model1, ]
    b <- scores[model == model2, ]

    overlap <- merge(a, b, by = by)

    if (nrow(overlap) == 0) {
      return(list(ratio = NA, pval = NA))
    }

    #-------------------------------------------------
    # need to do error handling if there is no overlap.
    #-------------------------------------------------

    values_x <- overlap[[paste0(metric, ".x")]]
    values_y <- overlap[[paste0(metric, ".y")]]

    # ratio should be equivalent to theta_ij in Johannes document
    # ratio < 1 --> model1 is better.
    ratio <- sum(values_x) / sum(values_y)

    if (permutation_test) {
      # taken from the surveillance package
      permutation_test <- function(score1, score2, nPermutation = 999, ratio = FALSE) {
        nTime = length(score1)
        meanScore1 <- mean(score1)
        meanScore2 <- mean(score2)

        if (ratio) {
          testStat_observed <- meanScore1 / meanScore2
        } else {
          testStat_observed <- meanScore1 - meanScore2
        }

        testStat_permuted <- replicate(nPermutation, {
          sel <- rbinom(nTime, size = 1, prob = 0.5)
          g1 <- (sum(score1[sel == 0]) + sum(score2[sel == 1]))/nTime
          g2 <- (sum(score1[sel == 1]) + sum(score2[sel == 0]))/nTime

          if (ratio) {
            return(g1 / g2)
          } else {
            return(g1 - g2)
          }
        })

        pVal <- (1 + sum(abs(testStat_permuted) >= abs(testStat_observed)))/(nPermutation + 1)
        # plus ones to make sure p-val is never 0?
        return(pVal)
      }

      pval <- permutation_test(values_x, values_y)
    } else {
      pval <- wilcox.test(values_x, values_y)
    }
    return(list(ratio = ratio, pval = pval))
  }

  # we only need to do the calculation once, because for the ratio that
  # should just be the inverse and for the permuation the result should
  # be the same

  combinations <-   data.table::as.data.table(t(combn(identifiers, m = 2)))
  colnames(combinations) <- c("model", "compare_against")

  combinations[, c("ratio", "pval") := single_comparison(model, compare_against,
                                                         permutation_test),
               by = seq_len(NROW(combinations))]
  combinations[sort(ratio)]

  # mirror computations
  combinations2 <- data.table::copy(combinations)
  data.table::setnames(combinations2, old = c("model", "compare_against"),
                       new = c("compare_against", "model"))
  combinations2[, ratio := 1/ratio]

  # add a one for those that are the same
  combinations3 <- data.table::data.table(model = identifiers,
                                          compare_against = identifiers,
                                          ratio = 1,
                                          pval = NA)

  result <- data.table::rbindlist(list(combinations,
                                       combinations2,
                                       combinations3),
                                  use.names = TRUE)

  # make result character instead of factor
  result[, `:=`("model" = as.character(model),
                "compare_against" = as.character(compare_against))]

  # calculate relative wis as geometric mean
  # need a different name for the variable! mean_ratio?
  # small theta_i is again better
  result[, `:=` (theta_i = geom_mean_helper(ratio),
                 theta_j = geom_mean_helper(1/ratio)),
         by = "model"]

  if(!is.null(baseline)) {
    baseline_theta <- unique(result[model == baseline, ]$theta_i)
    result[, rel_to_baseline := theta_i / baseline_theta]
  }

  return(result)
}






#' @title Plot Heatmap of Pairwise Comparisons
#'
#' @description
#' Creates a heatmap of the ratios or pvalues from a pairwise comparison
#' between models
#'
#' @param comparison_result A data.frame as produced by
#' \code{\link{pairwise_comparison}}
#' @param type character vector of length one that is either "ratio" or "pval".
#' This denotes whether to visualise the ratio or the p-value of the
#' pairwise comparison. Default is "ratio"
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs coord_cartesian
#' scale_fill_gradient2 theme_minimal element_text
#' @importFrom data.table as.data.table
#' @importFrom stats reorder
#' @export
#'
#' @examples
#' df <- data.frame(model = rep(c("model1", "model2", "model3"), each = 10),
#'                  id = rep(1:10),
#'                  interval_score = (abs(rnorm(30))),
#'                  aem = (abs(rnorm(30))))
#'
#' res <- pairwise_comparison(df, permutation_test = TRUE, baseline = "model1")
#' plot_pairwise_comparison(res, type = "ratio")



plot_pairwise_comparison <- function(comparison_result,
                                     type = c("ratio", "pval")) {

  comparison_result <- data.table::as.data.table(comparison_result)

  get_fill_scale <- function(values, breaks, scales) {
    scale <- cut(values, breaks = breaks,
                 include.lowest = TRUE,
                 right = FALSE,
                 labels = scales)
    return(as.numeric(as.character(scale)))
  }

  if (type[1] == "ratio") {
    comparison_result[, var_of_interest := round(ratio, 2)]
    comparison_result[, sort_var := round(ratio, 4)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, scales)]

  } else {
    comparison_result[, var_of_interest := pval]
    # reverse sorting for p-values, as lower means significant
    # maybe alternatively just always sort according to ratio...
    comparison_result[, sort_var := -pval]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    scales <- c(1, 0.5, 0.1, 0)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, scales)]
  }

  ggplot2::ggplot(comparison_result,
                  ggplot2::aes(y = reorder(model, 1/ratio, FUN = geom_mean_helper),
                               x = reorder(compare_against, ratio, FUN = geom_mean_helper),
                               fill = fill_col)) +
    ggplot2::geom_tile(color = "black",
                       width=0.98, height=0.98) +
    ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
                       na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white",
                                  high = "salmon",
                                  na.value = "white",
                                  midpoint = 0,
                                  name = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1),
                   legend.position = "none") +
    ggplot2::labs(x = "", y = "",
                  title = paste("Pairwise comparisons", type, sep = " - ")) +
    ggplot2::coord_cartesian(expand = FALSE)
}












