# nocov start
#' @importFrom cli cli_inform
.onAttach <- function(libname, pkgname) {
  cli_inform(
    "scoringutils 2.0.0 introduces major changes. We'd love your feedback!
    {.url https://github.com/epiforecasts/scoringutils/issues}.
    To use the old version, run:
    {.code remotes::install_github('epiforecasts/scoringutils@v1.2.2')}",
    .frequency = "once",
    .frequency_id = "enw_startup_message",
    class = "packageStartupMessage"
  )
}
# nocov end
