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
#' @importFrom stats sd rbinom wilcox.test p.adjust
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
#' scoringutils::plot_pairwise_comparison(res)
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
      permutation_test <- function(score1, score2, nPermutation = 999, oneSided = FALSE, ratio = FALSE) {
        nTime = length(score1)
        meanScore1 <- mean(score1)
        meanScore2 <- mean(score2)
        if (ratio) {
          # distinguish between on-sided and two-sided:
          testStat_observed <- ifelse(oneSided,
                                      meanScore1 / meanScore2,
                                      max(meanScore1 / meanScore2, meanScore2 / meanScore1))
        } else {
          testStat_observed <- ifelse(oneSided, meanScore1 - meanScore2, abs(meanScore1 - meanScore2))
        }
        testStat_permuted <- replicate(nPermutation, {
          sel <- rbinom(nTime, size = 1, prob = 0.5)
          g1 <- (sum(score1[sel == 0]) + sum(score2[sel == 1]))/nTime
          g2 <- (sum(score1[sel == 1]) + sum(score2[sel == 0]))/nTime
          if (ratio) {
            ifelse(oneSided, g1 / g2, max(g1 / g2, g2/g1))
          } else {
            ifelse(oneSided, g1 - g2, abs(g1 - g2))
          }
        })
        # abs needs to be removed here (messes with one sided vs two-sided)
        pVal <- (1 + sum(testStat_permuted >= testStat_observed))/(nPermutation + 1)
        # plus ones to make sure p-val is never 0?
        return(pVal)
      }

      pval <- permutation_test(values_x, values_y)
    } else {
      # this probably needs some more thought
      # alternative: do a paired t-test on ranks?
      pval <- wilcox.test(values_x, values_y, paired = TRUE)$p.value
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
  combinations[, adj_pval := p.adjust(pval)]

  # mirror computations
  combinations2 <- data.table::copy(combinations)
  data.table::setnames(combinations2, old = c("model", "compare_against"),
                       new = c("compare_against", "model"))
  combinations2[, ratio := 1/ratio]

  # add a one for those that are the same
  combinations3 <- data.table::data.table(model = identifiers,
                                          compare_against = identifiers,
                                          ratio = 1,
                                          pval = 1,
                                          adj_pval = 1)

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
#'                  interval_score = abs(rnorm(30, mean = rep(c(1, 1.3, 2), each = 10))),
#'                  aem = (abs(rnorm(30))))
#'
#' res <- scoringutils::pairwise_comparison(df, permutation_test = TRUE, baseline = "model1")
#' plot_pairwise_comparison(res, smaller_is_good = T)
#' plot_pairwise_comparison(res, smaller_is_good = T, type = "pval")


plot_pairwise_comparison <- function(comparison_result,
                                     type = c("together", "ratio", "pval"),
                                     smaller_is_good = TRUE) {

  comparison_result <- data.table::as.data.table(comparison_result)

  comparison_result[, model := reorder(model, -theta_i)]
  levels <- levels(comparison_result$model)


  get_fill_scale <- function(values, breaks, scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values, breaks = breaks,
                 include.lowest = TRUE,
                 right = FALSE,
                 labels = scales)
    # scale[is.na(scale)] <- 0
    return(as.numeric(as.character(scale)))
  }

  if (type[1] == "together") {
    # obtain only the upper triangle of the comparison
    # that is used for showing ratios
    # need to change the order if larger is good
    if (smaller_is_good) {
      unique_comb <- as.data.frame(t(combn(rev(levels), 2)))
    } else {
      unique_comb <- as.data.frame(t(combn((levels), 2)))
    }

    colnames(unique_comb) <- c("model", "compare_against")
    upper_triangle <- merge(comparison_result, unique_comb)

    # change levels for plotting order
    upper_triangle[, `:=` (model = factor(model, levels),
                           compare_against = factor(compare_against, levels))]

    # reverse y and x if larger is better
    if (!smaller_is_good) {
      data.table::setnames(upper_triangle,
                           c("model", "compare_against"),
                           c("compare_against", "model"))
    }

    # modify upper triangle ------------------------------------------------------
    # add columns where a model is compared with itself. make adj_pval NA
    # to plot it as grey later on
    equal <- data.table::data.table(model = levels,
                                    compare_against = levels,
                                    ratio = 1,
                                    pval = NA,
                                    adj_pval = NA)
    upper_triangle_complete <- data.table::rbindlist(list(upper_triangle,
                                                          equal), fill = TRUE)

    # define interest variable
    upper_triangle_complete[, var_of_interest := round(ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    if (!smaller_is_good) {
      scales <- rev(scales)
    }
    upper_triangle_complete[, fill_col := get_fill_scale(var_of_interest,
                                                         breaks, scales)]

    # create ratios in plot
    plot <- ggplot2::ggplot(upper_triangle_complete,
                            ggplot2::aes(x = compare_against,
                                         y = model,
                                         fill = fill_col)) +
      ggplot2::geom_tile(width=0.98, height=0.98) +
      ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
                         na.rm = TRUE) +
      ggplot2::scale_fill_gradient2(low = "skyblue", mid = "grey95",
                                    high = "brown1",
                                    na.value = "lightgrey",
                                    midpoint = 0,
                                    limits = c(-1,1),
                                    name = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                         hjust=1, color = "brown4"),
                     axis.text.y = ggplot2::element_text(color = "steelblue4"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # panel.background = ggplot2::element_rect(fill = "grey90"),
                     # axis.line.y = ggplot2::element_line(color = "steelblue4", size = 4),
                     # axis.line.x = ggplot2::element_line(color = "brown3", size = 4),
                     legend.position = "none") +
      ggplot2::labs(x = "", y = "",
                    title = "Pairwise comparisons - ratio (upper) and pval (lower)") +
      ggplot2::coord_cartesian(expand = FALSE)

    # add pvalues to plot --------------------------------------------------------
    # obtain lower triangle for the pvalues
    lower_triangle <- data.table::copy(upper_triangle)
    data.table::setnames(lower_triangle,
                         c("model", "compare_against"),
                         c("compare_against", "model"))

    lower_triangle[, var_of_interest := round(adj_pval, 3)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    scales <- c(0.8, 0.5, 0.1, 0.000001)
    lower_triangle[, fill_col := get_fill_scale(var_of_interest,
                                                breaks, scales)]

    # if (smaller_is_good) {
    #   fill_rule <- ifelse(lower_triangle$fill_col == 0, "white",
    #                       ifelse(lower_triangle$ratio <= 1, "skyblue3", "brown1"))
    # } else {
    #   fill_rule <- ifelse(lower_triangle$fill_col == 0, "white",
    #                       ifelse(lower_triangle$ratio >= 1, "skyblue3", "brown1"))
    # }

    fill_rule <- ifelse(lower_triangle$fill_col == 0.000001, "grey95", "palegreen3")
    lower_triangle[, var_of_interest := as.character(var_of_interest)]
    lower_triangle[, var_of_interest := ifelse(var_of_interest == "0",
                                               "< 0.001", var_of_interest)]

    plot <- plot +
      ggplot2::geom_tile(data = lower_triangle,
                         ggplot2::aes(alpha = fill_col),
                         fill = fill_rule,
                         color = "white",
                         width=0.97, height=0.97) +
      ggplot2::geom_text(data = lower_triangle,
                         ggplot2::aes(label = var_of_interest),
                         na.rm = TRUE)

    return(plot)
  } else if (type[1] == "ratio") {
    comparison_result[, var_of_interest := round(ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, scales)]

    high_col = "brown1"

  } else {
    comparison_result[, var_of_interest := round(pval, 3)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    scales <- c(1, 0.5, 0.1, 0)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, scales)]

    high_col = "palegreen3"
    comparison_result[, var_of_interest := as.character(var_of_interest)]
    comparison_result[, var_of_interest := ifelse(var_of_interest == "0",
                                               "< 0.001", var_of_interest)]
  }

  plot <- ggplot2::ggplot(comparison_result,
                          ggplot2::aes(y = reorder(model, 1/ratio, FUN = geom_mean_helper),
                                       x = reorder(compare_against, ratio, FUN = geom_mean_helper),
                                       fill = fill_col)) +
    ggplot2::geom_tile(color = "white",
                       width=0.97, height=0.97) +
    ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
                       na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = "skyblue", mid = "grey95",
                                  high = high_col,
                                  na.value = "lightgrey",
                                  midpoint = 0,
                                  limits = c(-1,1),
                                  name = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1),
                   legend.position = "none") +
    ggplot2::labs(x = "", y = "",
                  title = paste("Pairwise comparisons", type, sep = " - ")) +
    ggplot2::coord_cartesian(expand = FALSE)

  if (type == "ratio") {
    plot <- plot +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                         hjust=1, color = "brown4"),
                     axis.text.y = ggplot2::element_text(color = "steelblue4"))
  }

  return(plot)

}
 # plot_pairwise_comparison(res, smaller_is_good = T)









