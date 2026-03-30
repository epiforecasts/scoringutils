test_that("cli condition names use backtick quoting, not double quotes", {
  r_files <- list.files(
    system.file("../R", package = "scoringutils"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  # Fallback for when running from source

  if (length(r_files) == 0) {
    r_files <- list.files(
      file.path(testthat::test_path(), "..", "..", "R"),
      pattern = "\\.R$",
      full.names = TRUE
    )
  }
  expect_true(length(r_files) > 0)

  # Pattern matches "!" =, "i" =, "x" =, "v" =, "*" =
  # which are cli condition names that should use backticks
  pattern <- '"[!ixv*]"\\s*='
  violations <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    matches <- grep(pattern, lines)
    if (length(matches) > 0) {
      violations <- c(
        violations,
        paste0(basename(f), ":", matches, ": ", trimws(lines[matches]))
      )
    }
  }
  expect_equal(
    length(violations), 0,
    info = paste(
      "Double-quoted cli condition names found (use backticks):\n",
      paste(violations, collapse = "\n")
    )
  )
})
