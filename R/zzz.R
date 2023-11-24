.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Note: scoringutils is currently undergoing major development changes ",
    "(with an update planned for the first quarter of 2024). We would very ",
    "much appreciate your opinions and feedback on what should be included in ",
    "this major update: ",
    "https://github.com/epiforecasts/scoringutils/discussions/333"
  )
}
