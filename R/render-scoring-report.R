#' Render a Scoring Report
#'
#' @description This function renders a country level TB report.
#' @param format Character string, defaults to `"html_document"`. The format to render the report to.
#' See `?rmarkdown::render` for details.
#' @param truth_data data will all observed values
#' @param prediction_data dataframe with predictions
#' @param params parameters. Currently supported are locations, forecast_dates, target_types, horizons
#' @param interactive Logical, defaults to `FALSE`. When the format allows should graphs be interactive.
#' @param save_dir Character string, defaults to `NULL`.
#'  If not given then the report is rendered to a temporary directory (although only if `filename` is also not given).
#' @param filename Character string defaults `NULL`. Name to save the report under, defaults to `"scoring_report"`.
#' @return Renders a scoring report
#' @export
#' @importFrom utils installed.packages
#' @examples
#'
#'
#'
#'


render_scoring_report <- function(format = "html_document",
                                  truth_data,
                                  prediction_data,
                                  document_title = "Scoring Report",
                                  params = list(locations = "all",
                                                forecast_dates = "all",
                                                horizons = "all",
                                                target_types = "all"),
                                  interactive = FALSE,
                                  save_dir = NULL,
                                  filename = NULL) {

  required_packages <- c("rmarkdown", "scoringutils", "data.table",
                         "dplyr", "tibble", "here", "purrr", "readr",
                         "magrittr", "knitr", "ggbump")

  not_present <- sapply(required_packages, function(package) {
    not_present <- !(package %in% rownames(installed.packages()))

    if (not_present) {
      message(paste0(
        package,
        " is required to use render_scoring_report, please install it before using this function"
      ))
    }

    return(not_present)
  })

  if (any(not_present)) {
    stop("Packages required for this report are not installed,
         please use the following code to install the required packages \n\n
         install.packages(c('", paste(required_packages[not_present], collapse = "', '"), "'))")
  }

  report <- system.file("rmarkdown", "forecast-hub-style-report.Rmd", package = "scoringutils")
  if (report == "") {
    stop("Could not find the report. Try re-installing `scoringutils`.", call. = FALSE)
  }

  if (is.null(save_dir) & is.null(filename)) {
    save_dir <- tempdir()

    message("Rendering report to ", save_dir)
  }

  if ("locations" %in% names(params) && params$locations[1] == "all") {
    locations <- prediction_data$location_name %>%
      unique()
  }

  if ("forecast_dates" %in% names(params) && params$forecast_dates[1] == "all") {
    params$forecast_dates <- prediction_data %>%
      dplyr::pull(forecast_date) %>%
      unique() %>%
      as.character()
  }
  if ("target_types" %in% names(params) && params$target_types[1] == "all") {
    params$target_types <- prediction_data %>%
      dplyr::pull(target_type) %>%
      unique()
  }
  if ("horizons" %in% names(params) && params$horizons[1] == "all") {
    horizons <- prediction_data %>%
      dplyr::pull(horizon) %>%
      unique()
  }



  # store and load data to generate markdown report
  temp_dir <- tempdir()
  saveRDS(truth_data, file = paste0(temp_dir, "/truth_data.RDS"))
  saveRDS(prediction_data, file = paste0(temp_dir, "/prediction_data.RDS"))
  saveRDS(params, file = paste0(temp_dir, "/scoring_parameters.RDS"))

  format = "html_document"



  rmarkdown::render(report,
                    output_format = format,
                    output_file = filename,
                    output_dir = save_dir,
                    intermediates_dir = save_dir,
                    params = list(
                      temp_dir = temp_dir,
                      set_title = document_title
                    ),
                    envir = new.env(),
                    clean = TRUE
  )
}
