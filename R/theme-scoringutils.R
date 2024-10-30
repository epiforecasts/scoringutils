#' @title Scoringutils ggplot2 theme
#'
#' @description
#' A theme for ggplot2 plots used in `scoringutils`.
#' @returns A ggplot2 theme
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @keywords plotting
#' @export
theme_scoringutils <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
}
