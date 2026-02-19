test_that("_pkgdown.yml has math-rendering configured", {
  config_path <- testthat::test_path("..", "..", "_pkgdown.yml")
  skip_if_not(file.exists(config_path), "_pkgdown.yml not found")

  lines <- readLines(config_path, warn = FALSE)

  has_math_rendering <- any(grepl("^\\s*math-rendering:\\s*(katex|mathjax)\\s*$", lines))
  expect_true(
    has_math_rendering,
    info = "math-rendering must be set to 'katex' or 'mathjax' under template: in _pkgdown.yml"
  )
})

test_that("R documentation files with LaTeX formulas exist and are parseable", {
  latex_files <- c(
    "R/metrics-binary.R",
    "R/metrics-interval-range.R",
    "R/metrics-sample.R",
    "R/metrics-quantile.R"
  )
  pkg_root <- testthat::test_path("..", "..")

  # Skip if source files aren't available (e.g., during R CMD check)
  skip_if_not(
    file.exists(file.path(pkg_root, latex_files[1])),
    "Source R/ files not found (likely running in R CMD check)"
  )

  for (file in latex_files) {
    full_path <- file.path(pkg_root, file)
    expect_true(
      file.exists(full_path),
      info = paste(file, "should exist")
    )
    lines <- readLines(full_path, warn = FALSE)
    expect_true(
      any(grepl("\\\\(eqn|deqn)\\{", lines)),
      info = paste(file, "should contain \\eqn{} or \\deqn{} LaTeX formulas")
    )
  }
})
