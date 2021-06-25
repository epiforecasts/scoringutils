#' Cartesian coordinates per facet-panel
#'
#' This function mimics the behavior of [ggplot2::coord_cartesian()],
#' while supporting per-panel limits when faceted. It has been taken verbatim
#' from https://gist.github.com/r2evans/6057f7995c117bb787495dc14a228d5d
#'
#' @details
#'
#' A 'panel_limits' data frame may contain:
#'
#' - zero or more faceting variables, all of which must be found
#'   within the grob's 'layout' (i.e., defined by
#'   [ggplot2::facet_grid()] or [ggplot2::facet_wrap()];
#'
#' - zero or more of 'xmin', 'xmax', 'ymin', and 'ymax', where missing
#'   columns and 'NA' values within columns will default to ggplot2's
#'   normal min/max determination;
#'
#' - each panel in the plot must match no more than one row in
#'   'panel_limits';
#'
#' - each row may match more than one panel, such as when some
#'   faceting variables are not included (in 'panel_limits');
#'
#' - if no faceting variables are included, then 'panel_limits' must
#'   be at most one row (in which case it effectively falls back to
#'   [ggplot2::coord_cartesian()] behavior).
#'
#' It is an error if:
#'
#' - a panel is matched by more than one row (no matches is okay);
#'
#' - a faceting variable in 'panel_limits' is not found within the
#'   faceted layout.
#'
#' @section Thanks:
#'
#' - burchill (github) and the original version;
#'   https://gist.github.com/burchill/d780d3e8663ad15bcbda7869394a348a
#'
#' - Z.Lin (stackoverflow) for helping me through some of the
#'   initial errors; https://stackoverflow.com/a/63556918
#'
#' - teunbrand (github and stackoverflow), possible future extension
#'   of the non-list-index version; https://github.com/teunbrand/ggh4x
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' testdata <- tibble(
#'   x = rep(1:100, 2),
#'   y = rep(sin(seq(0,2*pi,length.out=100)), 2)
#' ) %>%
#'   mutate(y1 = y - 0.3, y2 = y + 0.3) %>%
#'   tidyr::crossing(
#'     tidyr::expand_grid(facet1 = c("aa", "bb"), facet2 = c("11", "22"))
#'   )
#'
#' gg <- ggplot(testdata, aes(x, y)) +
#'   geom_ribbon(aes(ymin = y1, ymax = y2), fill = "#ff8888aa") +
#'   geom_path(color = "red", size = 1) +
#'   facet_wrap(facet1 + facet2 ~ ., scales = "free")
#' gg
#'
#' # single-panel change,
#' gg + coord_cartesian_panels(
#'   panel_limits = tribble::tribble(
#'     ~facet1, ~facet2, ~ymin, ~ymax
#'   , "aa"   , "22"   , -0.75,   0.5
#'   )
#' )
#' }
#'
#' @param panel_limits 'data.frame' with faceting variables and
#'   limiting variables, see 'Details'
#' @param expand,default,clip as defined/used in
#'   [ggplot2::coord_cartesian()]
#' @md
#' @importFrom ggplot2 ggproto CoordCartesian
coord_cartesian_panels <- function(panel_limits,
                                   expand = TRUE, default = FALSE, clip = "on") {
  ggplot2::ggproto(NULL, UniquePanelCoords,
                   panel_limits = panel_limits,
                   expand = expand, default = default, clip = clip)
}

UniquePanelCoords <- ggplot2::ggproto(
  "UniquePanelCoords", ggplot2::CoordCartesian,

  num_of_panels = 1,
  panel_counter = 1,
  layout = NULL,

  setup_layout = function(self, layout, params) {
    self$num_of_panels <- length(unique(layout$PANEL))
    self$panel_counter <- 1
    self$layout <- layout # store for later
    layout
  },

  setup_panel_params =  function(self, scale_x, scale_y, params = list()) {
    train_cartesian <- function(scale, limits, name, given_range = c(NA, NA)) {
      if (anyNA(given_range)) {
        expansion <- ggplot2:::default_expansion(scale, expand = self$expand)
        range <- ggplot2:::expand_limits_scale(scale, expansion, coord_limits = limits)
        isna <- is.na(given_range)
        given_range[isna] <- range[isna]
      }
      out <- list(
        ggplot2:::view_scale_primary(scale, limits, given_range),
        sec = ggplot2:::view_scale_secondary(scale, limits, given_range),
        arrange = scale$axis_order(),
        range = given_range
      )
      names(out) <- c(name, paste0(name, ".", names(out)[-1]))
      out
    }

    this_layout <- self$layout[ self$panel_counter,, drop = FALSE ]
    self$panel_counter <-
      if (self$panel_counter < self$num_of_panels) {
        self$panel_counter + 1
      } else 1

    # determine merge column names by removing all "standard" names
    layout_names <- setdiff(names(this_layout),
                            c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
    limits_names <- setdiff(names(self$panel_limits),
                            c("xmin", "xmax", "ymin", "ymax"))

    limits_extras <- setdiff(limits_names, layout_names)
    if (length(limits_extras) > 0) {
      stop("facet names in 'panel_limits' not found in 'layout': ",
           paste(sQuote(limits_extras), collapse = ","))
    } else if (length(limits_names) == 0 && NROW(self$panel_limits) == 1) {
      # no panels in 'panel_limits'
      this_panel_limits <- cbind(this_layout, self$panel_limits)
    } else {
      this_panel_limits <- merge(this_layout, self$panel_limits, all.x = TRUE, by = limits_names)
    }

    if (isTRUE(NROW(this_panel_limits) > 1)) {
      stop("multiple matches for current panel in 'panel_limits'")
    }

    # add missing min/max columns, default to "no override" (NA)
    this_panel_limits[, setdiff(c("xmin", "xmax", "ymin", "ymax"),
                                names(this_panel_limits)) ] <- NA

    c(train_cartesian(scale_x, self$limits$x, "x",
                      unlist(this_panel_limits[, c("xmin", "xmax"), drop = TRUE])),
      train_cartesian(scale_y, self$limits$y, "y",
                      unlist(this_panel_limits[, c("ymin", "ymax"), drop = TRUE])))
  }
)
