# Helper to find the R/ source directory; works both during devtools::test()
# and R CMD check (where the working directory differs).
find_r_source_dir <- function() {
  candidates <- c(
    file.path(testthat::test_path(), "..", "..", "R"),
    "../../R"
  )
  for (d in candidates) {
    if (dir.exists(d)) return(d)
  }
  NULL
}

test_that("no R source files use double-quoted cli condition names", {
  r_dir <- find_r_source_dir()
  skip_if(is.null(r_dir), "R/ source directory not found")
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  matches <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    idx <- grep('"[!ixv]"\\s*=', lines)
    if (length(idx) > 0) {
      matches <- c(matches, paste0(basename(f), ":", idx, ": ", trimws(lines[idx])))
    }
  }
  expect_length(matches, 0)
})

test_that("no keyword_quote_linter nolint annotations remain in R source", {
  r_dir <- find_r_source_dir()
  skip_if(is.null(r_dir), "R/ source directory not found")
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  matches <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    idx <- grep("keyword_quote_linter", lines)
    if (length(idx) > 0) {
      matches <- c(matches, paste0(basename(f), ":", idx, ": ", trimws(lines[idx])))
    }
  }
  expect_length(matches, 0)
})

test_that("keyword_quote_linter is not disabled in .lintr config", {
  lintr_path <- file.path(testthat::test_path(), "..", "..", ".lintr")
  skip_if(!file.exists(lintr_path), ".lintr not found (likely R CMD check)")
  lintr_content <- readLines(lintr_path, warn = FALSE)
  expect_false(any(grepl("keyword_quote_linter\\s*=\\s*NULL", lintr_content)))
})

test_that("cli error and warning messages still render correctly after quoting change", {
  expect_error(
    assert_forecast(data.frame()),
    "valid forecast object"
  )
})

test_that("nolint blocks with only keyword_quote_linter are fully removed", {
  r_dir <- find_r_source_dir()
  skip_if(is.null(r_dir), "R/ source directory not found")
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  keyword_only_blocks <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    start_idx <- grep("#\\s*nolint start", lines)
    for (i in start_idx) {
      line <- lines[i]
      # Check if this nolint start mentions keyword_quote_linter
      if (grepl("keyword_quote_linter", line)) {
        # Check if it ONLY mentions keyword_quote_linter (no other linters)
        cleaned <- sub("#\\s*nolint start:\\s*", "", line)
        cleaned <- trimws(cleaned)
        linters <- trimws(strsplit(cleaned, "\\s+")[[1]])
        if (length(linters) == 1 && linters[1] == "keyword_quote_linter") {
          keyword_only_blocks <- c(
            keyword_only_blocks,
            paste0(basename(f), ":", i, ": ", trimws(line))
          )
        }
      }
    }
  }
  expect_length(keyword_only_blocks, 0)
})
