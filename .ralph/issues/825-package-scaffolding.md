# Issue #825: Package scaffolding

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/825
- **Action Summary**: (1) Replace `Metrics::ae/se/ape` with internal one-liners in `R/class-forecast-point.R`, remove from DESCRIPTION/NAMESPACE, update vignettes and tests. (2) Optionally evaluate adding `matrixStats` for 3 `apply()` calls — lower value, separate decision.
- **Status Note**: Still relevant. Neither recommendation acted upon: (1) `Metrics` package still imported for 3 trivial functions (`ae`, `se`, `ape`) used only in `R/class-forecast-point.R` — these are one-line base-R replacements. `Metrics` did receive a 2025 CRAN update, reducing urgency. (2) `matrixStats` not adopted; 3 `apply()` calls in `R/metrics-sample.R` could benefit. Suggest splitting into two sub-tasks, prioritizing `Metrics` removal as a quick win.

## Context

The `Metrics` package provides three trivially simple functions used by scoringutils:
- `Metrics::ae(actual, predicted)` = `abs(actual - predicted)`
- `Metrics::se(actual, predicted)` = `(actual - predicted)^2`
- `Metrics::ape(actual, predicted)` = `abs(actual - predicted) / abs(actual)`

These are referenced in:
1. `R/class-forecast-point.R` lines 148-150: `get_metrics.forecast_point()` uses `Metrics::ae`, `Metrics::se`, `Metrics::ape`
2. `R/class-forecast-point.R` line 78: `@importFrom Metrics se ae ape`
3. `R/class-forecast-point.R` lines 103-105: roxygen documentation cross-references `[Metrics::ae()]`, `[Metrics::se()]`, `[Metrics::ape()]`
4. `vignettes/scoring-rules.Rmd` lines 61-65: Direct usage in examples (`Metrics::ae()`, `Metrics::se()`)
5. `vignettes/scoring-rules.Rmd` lines 76, 88, 99: Documentation references (`?Metrics::ae`, `?Metrics::se`, `?Metrics::ape`)
6. `DESCRIPTION`: `Metrics` in Imports
7. `NAMESPACE`: three `importFrom(Metrics,ae)`, `importFrom(Metrics,ape)`, `importFrom(Metrics,se)` lines

The fix replaces `Metrics::ae`, `Metrics::se`, `Metrics::ape` with internal one-liner implementations and removes the `Metrics` package dependency.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-point.R` (existing) and `tests/testthat/test-metrics-point.R` (existing)
**Testing patterns observed**: testthat + data.table fixtures; `setup.R` pre-computes `scores_point <- score(example_point)`; existing tests validate `ae_point` via `abs(y - point_forecast)` pattern; no direct Metrics function testing exists — all validation goes through `score()`

### Test 1: test_that("internal ae replacement produces identical results to Metrics::ae", { ... })

**Reproduces**: Ensures the internal replacement for `Metrics::ae` is numerically identical to the original
**Setup**:
- Create two numeric vectors: `observed <- c(1, -15, 22, 0, 5.5)` and `predicted <- c(5, 6, 7, 0, 5.5)`
- Expected result: `abs(observed - predicted)` which equals `c(4, 21, 15, 0, 0)`
**Action**:
- Retrieve the `ae_point` metric function from `get_metrics(example_point, select = "ae_point")` (this is a list with one element)
- Call the function: `ae_fn <- get_metrics(example_point, select = "ae_point")[[1]]; ae_fn(observed, predicted)`
**Expected**:
- `expect_identical(ae_fn(observed, predicted), abs(observed - predicted))`
- Must return a numeric vector of the same length as input
- Must handle zero difference: `expect_identical(ae_fn(5, 5), 0)`
- Must handle negative values: `expect_identical(ae_fn(-10, 5), 15)`
**Fails now because**:
- This test should PASS both before and after the fix (the mathematical behavior is identical). This is a regression guard ensuring the replacement is correct.

### Test 2: test_that("internal se replacement produces identical results to Metrics::se", { ... })

**Reproduces**: Ensures the internal replacement for `Metrics::se` is numerically identical to the original
**Setup**:
- Use same vectors: `observed <- c(1, -15, 22, 0, 5.5)` and `predicted <- c(5, 6, 7, 0, 5.5)`
- Expected result: `(observed - predicted)^2` which equals `c(16, 441, 225, 0, 0)`
**Action**:
- Retrieve the `se_point` metric function: `se_fn <- get_metrics(example_point, select = "se_point")[[1]]; se_fn(observed, predicted)`
**Expected**:
- `expect_identical(se_fn(observed, predicted), (observed - predicted)^2)`
- Must return a numeric vector of the same length as input
- Must handle zero difference: `expect_identical(se_fn(5, 5), 0)`
- Must handle negative values correctly: `expect_identical(se_fn(-10, 5), 225)`
**Fails now because**:
- This test should PASS both before and after the fix (regression guard).

### Test 3: test_that("internal ape replacement produces identical results to Metrics::ape", { ... })

**Reproduces**: Ensures the internal replacement for `Metrics::ape` is numerically identical to the original
**Setup**:
- Use vectors where observed is non-zero: `observed <- c(1, -15, 22, 5.5, 100)` and `predicted <- c(5, 6, 7, 0, 100)`
- Expected result: `abs(observed - predicted) / abs(observed)` which equals `c(4, 21/15, 15/22, 1, 0)`
**Action**:
- Retrieve the `ape` metric function: `ape_fn <- get_metrics(example_point, select = "ape")[[1]]; ape_fn(observed, predicted)`
**Expected**:
- `expect_equal(ape_fn(observed, predicted), abs(observed - predicted) / abs(observed))`
- Must handle zero difference: `expect_identical(ape_fn(5, 5), 0)`
- Division by zero when observed = 0: `expect_identical(ape_fn(0, 5), Inf)` (matching `Metrics::ape` behavior)
**Fails now because**:
- This test should PASS both before and after the fix (regression guard).

### Test 4: test_that("Metrics package is not in DESCRIPTION Imports", { ... })

**Reproduces**: The core goal of issue #825 — removing the Metrics dependency
**Setup**:
- Read the DESCRIPTION file using `read.dcf("DESCRIPTION")` or `desc::desc_get_deps()` (if available), or simply read DESCRIPTION as text
**Action**:
- Parse the DESCRIPTION file to extract Imports
- Check whether "Metrics" appears in the Imports field
**Expected**:
- `expect_false(any(grepl("Metrics", readLines(system.file("DESCRIPTION", package = "scoringutils")))))` — or more precisely, check the parsed Imports list does not contain "Metrics"
- Alternatively: `expect_false("Metrics" %in% loadedNamespaces())` after detaching, but DESCRIPTION check is more robust
**Fails now because**:
- Currently `Metrics` IS listed in `DESCRIPTION` Imports. After the fix, it should be removed.

### Test 5: test_that("score() with point forecasts produces correct results after Metrics removal", { ... })

**Reproduces**: End-to-end regression check ensuring `score()` still works correctly for point forecasts
**Setup**:
- Use `example_point` (the built-in dataset)
- Compute expected absolute error manually: for each row, `abs(observed - predicted)`
**Action**:
- `scores <- score(example_point)`
- Extract `ae_point`, `se_point`, and `ape` columns
**Expected**:
- `expect_equal(scores$ae_point, abs(scores_input$observed - scores_input$predicted))` (where `scores_input` is derived from `example_point` with NAs removed)
- `expect_equal(scores$se_point, (scores_input$observed - scores_input$predicted)^2)`
- `expect_equal(scores$ape, abs(scores_input$observed - scores_input$predicted) / abs(scores_input$observed))`
- All three metric columns must be present in the output
**Fails now because**:
- This test should PASS both before and after the fix (regression guard ensuring end-to-end scoring is unchanged).

### Test 6: test_that("get_metrics.forecast_point() returns expected functions", { ... })

**Reproduces**: Validates the structure and names of the metric list after removing Metrics dependency
**Setup**:
- None beyond `example_point`
**Action**:
- `metrics <- get_metrics(example_point)`
**Expected**:
- `expect_type(metrics, "list")`
- `expect_named(metrics, c("ae_point", "se_point", "ape"))`
- Each element is a function: `expect_true(all(vapply(metrics, is.function, logical(1))))`
- Each function accepts exactly 2 arguments (observed/actual, predicted): `expect_true(all(vapply(metrics, function(f) length(formals(f)) == 2, logical(1))))`
**Fails now because**:
- The names and structure test PASSES now and should continue to PASS. The argument count check validates the internal implementations match the `Metrics` function signatures (both take exactly 2 args: `actual`, `predicted`).

### Note on vignette references

The vignette `scoring-rules.Rmd` uses `Metrics::ae()` and `Metrics::se()` in code chunks (lines 61-65) and references `?Metrics::ae`, `?Metrics::se`, `?Metrics::ape` in prose (lines 76, 88, 99). After the fix:
- Code chunks should use the internal functions or inline base-R equivalents (e.g., `abs(observed - predicted)`)
- Prose references should be updated to point to internal documentation or removed

No automated test is needed for the vignette changes — `R CMD check` will catch broken `Metrics::` references if the package is removed from DESCRIPTION.
