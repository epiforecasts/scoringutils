# Issue #975: Add link to score definitions in `summarise_score()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/975
- **Action Summary**: Create a `print.scores()` S3 method in `R/class-scores.R` that calls `NextMethod()` then appends a `cli_text()` link to the scoring rules vignette. Ideally coordinate with #667 to also show the metrics used.
- **Status Note**: Still relevant. No `print.scores` method exists anywhere in the codebase. Scores objects fall through to data.table's default print. The scoring rules vignette exists at the expected URL. Maintainers favor option 1 (link to docs) over printing definitions.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-scores.R` (existing)
**Testing patterns observed**: testthat + data.table fixtures; `expect_snapshot()` for print output; `expect_s3_class()` for class checks; pre-computed `scores_*` objects in `setup.R` (e.g., `scores_quantile`, `scores_binary`, `scores_point`, `scores_sample_continuous`); `suppressMessages(capture.output(print(...)))` pattern for isolating print output; `cli_text()` / `cli_inform()` used for formatted messages in existing `print.forecast()`.

### Test 1: test_that("print.scores() outputs metrics and vignette link", { ... })

**Reproduces**: the core issue — scores objects have no custom print method, so no link to score definitions is shown
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `print(scores_quantile)` and capture its output/messages
**Expected**:
- Output contains the names of the metrics used (retrieved from the `metrics` attribute), e.g., "wis", "overprediction", "underprediction", etc.
- Output contains a link or reference to the scoring rules vignette (e.g., text matching `"scoring-rules"` or the vignette URL `https://epiforecasts.io/scoringutils/articles/scoring-rules.html`)
- The underlying data.table content is still printed (i.e., `NextMethod()` is called, so the tabular data appears in the output)
- Use `expect_snapshot(print(scores_quantile))` to pin the exact formatted output
**Fails now because**:
- No `print.scores()` method exists. `print()` dispatches directly to `print.data.table()`, which shows only the raw table with no metrics summary or vignette link.

### Test 2: test_that("print.scores() works for all forecast types", { ... })

**Reproduces**: ensures the print method works across all score object types, not just one
**Setup**:
- Use all pre-computed scores objects from `setup.R`: `scores_quantile`, `scores_binary`, `scores_point`, `scores_sample_continuous`, `scores_sample_discrete`, `scores_nominal`
**Action**:
- Call `print()` on each scores object
**Expected**:
- Each call completes without error or unexpected warnings
- Use `expect_no_condition()` wrapping `capture.output(print(scores_obj))` for each (or `expect_snapshot()` for one representative type)
- Each output includes the metrics names relevant to its forecast type (e.g., `scores_binary` should mention `"brier_score"`, `scores_point` should mention `"ae_point"`)
**Fails now because**:
- No `print.scores()` method exists; there is no custom output to validate.

### Test 3: test_that("print.scores() returns x invisibly", { ... })

**Reproduces**: standard R print method contract — print should return the object invisibly
**Setup**:
- Use `scores_quantile` from `setup.R`
**Action**:
- Capture the return value: `ret <- capture.output(result <- print(scores_quantile))` (inside `suppressMessages()`)
**Expected**:
- `expect_identical(result, scores_quantile)` — the returned object is identical to the input
- The return is invisible (following the convention from `print.forecast()` which does `return(invisible(x))`)
**Fails now because**:
- `print.data.table()` returns `x` invisibly by default, so this test might pass even now. However, this test ensures the contract is maintained when the new `print.scores()` method is added — it's a regression guard.

### Test 4: test_that("print.scores() includes data.table output", { ... })

**Reproduces**: ensures the new print method still delegates to data.table printing (via `NextMethod()`) so users still see their data
**Setup**:
- Use `scores_quantile` from `setup.R`
**Action**:
- Capture output from the new `print.scores()`: `output_scores <- suppressMessages(capture.output(print(scores_quantile)))`
- Capture output from plain data.table print: `output_dt <- capture.output(print(data.table::as.data.table(scores_quantile)))`
**Expected**:
- `expect_true(all(output_dt %in% output_scores))` — all lines from the data.table print appear in the scores print output
- This mirrors the existing pattern from `print.forecast()` tests at `test-class-forecast.R:147-150`
**Fails now because**:
- Currently `print()` on a scores object IS `print.data.table()` directly, so this would pass trivially. After implementing `print.scores()`, this test ensures `NextMethod()` is called and data.table output is preserved.

### Test 5: test_that("print.scores() handles missing metrics attribute gracefully", { ... })

**Reproduces**: edge case where the `metrics` attribute has been stripped (e.g., by operations that don't preserve attributes)
**Setup**:
- Create a scores object and remove its metrics attribute: `ex <- data.table::copy(scores_quantile); attr(ex, "metrics") <- NULL`
- Note: `ex` still has class `"scores"` so `print.scores()` would dispatch
**Action**:
- Call `print(ex)` wrapped in output capture
**Expected**:
- The print call should not error. It should either:
  - Show a warning/message that metrics attribute is missing (similar to how `get_metrics.scores()` handles this), OR
  - Simply skip the metrics display and still print the underlying data.table
- Use `expect_no_error(capture.output(print(ex)))` at minimum
**Fails now because**:
- No `print.scores()` method exists, so this edge case is untested. After implementation, this ensures the method handles degraded objects gracefully rather than crashing.

### Test 6: test_that("print.scores() handles renamed metric columns with warning", { ... })

**Reproduces**: edge case where metric columns have been renamed, causing mismatch with `metrics` attribute
**Setup**:
- Copy a scores object and rename a metric column: `ex <- data.table::copy(scores_sample_continuous); data.table::setnames(ex, old = "crps", new = "changed")`
- The `metrics` attribute still contains `"crps"` but the column is now `"changed"`
**Action**:
- Call `print(ex)` wrapped in output capture
**Expected**:
- A warning should be emitted about the mismatch (inheriting from `get_metrics.scores()` behavior which already warns: `"scores have been previously computed, but are no longer column names"`)
- The print call should still complete without error
- Use `expect_warning(capture.output(print(ex)), "scores have been previously computed")`
**Fails now because**:
- No `print.scores()` method exists. The mismatch warning from `get_metrics.scores()` is never triggered during printing because print doesn't call `get_metrics.scores()`.
