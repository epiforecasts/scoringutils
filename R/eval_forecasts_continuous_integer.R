#' @title Evaluate forecasts in a Sample-Based Format (Integer or Continuous)
#'
#'
#' @inheritParams eval_forecasts
#' @param prediction_type character, should be either "continuous" or "integer"
#'
#' @return A data.table with appropriate scores. For more information see
#' \code{\link{eval_forecasts}}
#'
#' @importFrom data.table ':=' as.data.table rbindlist %like%
#'
#'
#' @examples
#'
#' ## Integer Forecasts
#' integer_example <- data.table::setDT(scoringutils::integer_example_data)
#' eval <- scoringutils::eval_forecasts(integer_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.1, 0.9),
#'                                      sd = TRUE,
#'                                      pit_plots = TRUE)
#' eval <- scoringutils::eval_forecasts(integer_example)
#'
#' ## Continuous Forecasts
#' continuous_example <- data.table::setDT(scoringutils::continuous_example_data)
#' eval <- scoringutils::eval_forecasts(continuous_example)#'
#'
#' eval <- scoringutils::eval_forecasts(continuous_example,
#'                                      quantiles = c(0.5, 0.9),
#'                                      sd = TRUE,
#'                                      summarise_by = c("model"))
#'
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @references Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ
#' (2019) Assessing the performance of real-time epidemic forecasts: A
#' case study of Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785. <doi:10.1371/journal.pcbi.1006785>


eval_forecasts_sample <- function(data,
                                  by,
                                  summarise_by,
                                  metrics,
                                  prediction_type,
                                  quantiles,
                                  sd,
                                  pit_plots,
                                  summarised,
                                  verbose) {

  if (missing(prediction_type)) {
    if (all.equal(data$prediction, as.integer(data$prediction)) == TRUE) {
      prediction_type <- "integer"
    } else {
      prediction_type <- "continuous"
    }
  }

  # calculate scores -----------------------------------------------------------
  # sharpness
  if ("sharpness" %in% metrics) {
    data[, sharpness := scoringutils::sharpness(t(prediction)), by = c(by)]
  }
  # bias
  if ("bias" %in% metrics) {
    data[, bias := scoringutils::bias(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # DSS
  if ("dss" %in% metrics) {
    data[, dss := scoringutils::dss(unique(true_value),
                                    t(prediction)), by = c(by)]
  }
  # CRPS
  if ("crps" %in% metrics) {
    data[, crps := scoringutils::crps(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # Log Score
  if ("log_score" %in% metrics) {
    # only compute if prediction type is continuous
    if (prediction_type == "continuous") {
      data[, log_score := scoringutils::logs(unique(true_value),
                                             t(prediction)), by = c(by)]
    }
  }
  # coverage
  if ("coverage" %in% metrics) {
  }


  # Compute PIT if specified ---------------------------------------------------
  if (any(grepl("pit", metrics)) || pit_plots) {

    # check if by == summarise_by - in that case no pit values can be computed
    if (identical(by, summarise_by)) {
      data[, c("pit_p_val", "pit_sd") := NA]
      if (verbose) {
        message("In order to compute PIT values, 'summarise_by' must be different from 'by'")
      }
    }

    # if they are not identical, pit p-values can be computed
    if (!identical(by, summarise_by)) {
      # if plots are not desired, a quick way to do computation can be chosen
      if (!pit_plots) {
        data <- pit_df_fast(data, by = summarise_by)
      } else {
        # split data into chunks as determined by summarise_by, since we need to
        # get one PIT per element of summarise_by
        split_data <- split(data, by = summarise_by)

        # calculate pit for every element of the split data.frame
        pits <- lapply(split_data,
                       FUN = pit_df, plot = pit_plots)

        # extract data frames with added p-values. Bind data together again
        data_with_pit_values <- extract_from_list(pits, "data")
        data <- data.table::rbindlist(data_with_pit_values)

        if (pit_plots) {
          # extract pit histograms if plots are desired
          pit_histograms <- extract_from_list(pits, "hist_PIT")

          # add another histogram for the entire data set
          pit_histograms[["overall_pit"]] <- scoringutils::pit_df(data)$hist_PIT
        }

      }
    }

    # remove sd if not asked for
    if (!sd) {
      data[, "pit_sd" := NULL]
    }
  }

  res <- data

  # make scores unique to avoid redundancy.
  res <- res[, lapply(.SD, unique),
             .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score|pit",
             by = c(by)]

  # summarise output if desired ------------------------------------------------
  if (summarised) {
    # add quantiles
    if (!is.null(quantiles)) {
      quantile_vars <- c("crps", "dss", "log_score", "pit_p_val", "bias", "sharpness")
      res <- add_quantiles(res, quantile_vars, quantiles, by = c(summarise_by))
    }

    if (sd) {
      # add standard deviations
      sd_vars <- c("crps", "dss", "log_score", "bias", "sharpness")
      res <- add_sd(res, sd_vars, by = c(summarise_by))
    }

    # take mean
    res <- res[, lapply(.SD, mean, na.rm = TRUE),
               .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score",
               by = summarise_by]
  }


  # if pit_plots is TRUE, add the plots as an output ---------------------------
  if (pit_plots) {
    res <- list(scores = res,
                pit_plots = pit_histograms)
  }

  return(res)
}
