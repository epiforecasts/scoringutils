# Issue #922: remove `check_columns_present`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/922
- **Action Summary**: Replace all 10 call sites of `check_columns_present` (and 1 of `test_columns_present`) with equivalent `checkmate` functions, then delete both functions and their tests. Consider batching with #839.
- **Status Note**: Still relevant. `check_columns_present` (and `test_columns_present`) are internal functions with 10 call sites across 9 files. All can be replaced by `checkmate::assert_subset`/`check_subset`/`test_subset` (already a dependency). One call site in `pairwise-comparisons.R:142` uses a custom error message and needs slightly different handling. No user-facing API impact.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-check-input-helpers.R` (existing, modified) + existing test files for call sites
**Testing patterns observed**: testthat with data.table fixtures; `example_quantile`, `example_binary`, `example_sample_continuous` etc. provided by scoringutils; `setup.R` pre-computes score objects; `expect_error()` with fixed string matching for assertion messages; `checkmate::assert()` wraps check functions.

## Summary of Changes

The fix replaces `check_columns_present(data, columns)` with `checkmate::check_subset(columns, colnames(data))` (and similarly `test_columns_present` with `checkmate::test_subset`, `assert(check_columns_present(...))` with `assert_subset(...)`). Note the **argument order reversal**: `check_columns_present(data, columns)` becomes `check_subset(columns, choices)` where `choices = colnames(data)`.

The error message format changes from:
- `"Column 'X' not found in data"` / `"Columns 'X', 'Y' not found in data"`
to:
- `"Must be a subset of {'a','b',...}, but has additional elements {'X','Y'}"` (checkmate's format)

### Call sites (10 `check_columns_present` + 1 `test_columns_present`):

1. `R/class-forecast.R:115` — `assert(check_columns_present(data, c("observed", "predicted")))` → `assert_subset(c("observed", "predicted"), colnames(data))`
2. `R/class-forecast.R:116` — `test_columns_present(data, c("sample_id", "quantile_level"))` → `test_subset(c("sample_id", "quantile_level"), colnames(data))`
3. `R/class-forecast-sample.R:62` — `assert(check_columns_present(forecast, "sample_id"))` → `assert_subset("sample_id", colnames(forecast))`
4. `R/class-forecast-quantile.R:84` — `assert(check_columns_present(forecast, "quantile_level"))` → `assert_subset("quantile_level", colnames(forecast))`
5. `R/class-forecast-nominal.R:78` — `assert(check_columns_present(forecast, "predicted_label"))` → `assert_subset("predicted_label", colnames(forecast))`
6. `R/class-forecast-ordinal.R:78` — `assert(check_columns_present(forecast, "predicted_label"))` → `assert_subset("predicted_label", colnames(forecast))`
7. `R/class-forecast-multivariate-sample.R:87` — `assert(check_columns_present(forecast, c("sample_id", ".mv_group_id")))` → `assert_subset(c("sample_id", ".mv_group_id"), colnames(forecast))`
8. `R/pairwise-comparisons.R:127` — `assert(check_columns_present(scores, compare))` → `assert_subset(compare, colnames(scores))`
9. `R/pairwise-comparisons.R:142` — `by_cols <- check_columns_present(scores, by)` used in custom error → `by_cols <- check_subset(by, colnames(scores))`
10. `R/plot-wis.R:53` — `assert(check_columns_present(scores, wis_components))` → `assert_subset(wis_components, colnames(scores))`
11. `R/get-correlations.R:88` — `col_present <- check_columns_present(correlations, "metric")` used as boolean check → `col_present <- check_subset("metric", colnames(correlations))`

---

### Test 1: test_that("check_columns_present and test_columns_present are removed", { ... })

**Reproduces**: Confirms the functions have been deleted
**File**: `tests/testthat/test-check-input-helpers.R`
**Setup**:
- No special setup needed
**Action**:
- Attempt to call `scoringutils:::check_columns_present(example_binary, "location_name")` and `scoringutils:::test_columns_present(example_binary, "location_name")`
**Expected**:
- Both calls should error (function not found), OR the existing tests that directly call `check_columns_present` should be entirely removed from `test-check-input-helpers.R` (lines 14-56 in current file)
**Fails now because**:
- The functions currently exist, so direct calls succeed. After deletion, the old test blocks (lines 14-56) must be removed.

**Implementation note**: The existing test blocks at lines 14-36 (`"check_columns_present works"`) and lines 48-56 (`"check_columns_present() works"`) should be deleted entirely. The `test_columns_not_present` test block (lines 39-46) should also be deleted if #839 is batched, but if not batched, it should remain.

---

### Test 2: test_that("assert_forecast_generic errors when 'observed' or 'predicted' columns are missing", { ... })

**Reproduces**: Validates that the replacement at `R/class-forecast.R:115` still produces an error when required columns are missing
**File**: `tests/testthat/test-class-forecast.R` (existing — likely already has this test, but error message expectations need updating)
**Setup**:
- Create a minimal data.table with no `observed` or `predicted` columns, e.g. `data.table(x = 1:3, y = 4:6)`
**Action**:
- Call `as_forecast_binary(data)` (or any `as_forecast_*` that goes through `assert_forecast_generic`)
**Expected**:
- Should error. The error message will change from containing `"not found in data"` to checkmate's `"Must be a subset of"` / `"additional elements"` format. Test should use a pattern that matches the new message.
**Fails now because**:
- Currently produces error with `"Column 'observed' not found in data"`. After the fix, will produce a `checkmate::assert_subset` error like `"Must be a subset of {'x','y'}, but has additional elements {'observed','predicted'}"`.

---

### Test 3: test_that("assert_forecast_generic detects both sample_id and quantile_level columns", { ... })

**Reproduces**: Validates the `test_columns_present` → `test_subset` replacement at `R/class-forecast.R:116`
**File**: `tests/testthat/test-class-forecast.R` (existing)
**Setup**:
- Create a data.table with both `sample_id` and `quantile_level` columns present alongside `observed` and `predicted`
**Action**:
- Call `as_forecast_sample(data)` or `as_forecast_quantile(data)` — the generic assertion should catch the conflict
**Expected**:
- Should error with message about both `quantile_level` and `sample_id` being present (this error message is from `cli_abort`, not from `check_columns_present`, so it should be unchanged)
**Fails now because**:
- This test should PASS both before and after the fix — it is a regression test ensuring the `test_subset` replacement doesn't break this detection logic. The underlying error message comes from `cli_abort` and is independent of the column-checking function used.

---

### Test 4: test_that("assert_forecast.forecast_sample errors when sample_id is missing", { ... })

**Reproduces**: Validates replacement at `R/class-forecast-sample.R:62`
**File**: `tests/testthat/test-class-forecast-sample.R` (existing)
**Setup**:
- Take `example_sample_continuous`, remove the `sample_id` column
**Action**:
- Call `as_forecast_sample(data)`
**Expected**:
- Should error. After fix, error message changes to checkmate's `assert_subset` format (e.g., `"Must be a subset of {colnames}, but has additional elements {'sample_id'}"`)
**Fails now because**:
- Currently produces `"Column 'sample_id' not found in data"`. After replacement with `assert_subset`, the error format changes. Any existing tests matching the old message string need updating.

---

### Test 5: test_that("assert_forecast.forecast_quantile errors when quantile_level is missing", { ... })

**Reproduces**: Validates replacement at `R/class-forecast-quantile.R:84`
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing — tests at lines 74, 81, 88 match old message format)
**Setup**:
- Take `example_quantile`, remove the `quantile_level` column, or remove `observed`/`predicted`
**Action**:
- Call `as_forecast_quantile(data)`
**Expected**:
- Should error with checkmate's subset error format instead of `"Column '...' not found in data"`
**Fails now because**:
- Existing tests at lines 74, 81, 88 of `test-class-forecast-quantile.R` match the old error message format (e.g., `"Assertion on 'data' failed: Column 'predicted' not found in data."` and `"Assertion on 'data' failed: Columns 'observed', 'predicted' not found in data."`). These string expectations must be updated to match `assert_subset`'s message format.

---

### Test 6: test_that("assert_forecast.forecast_sample_multivariate errors when sample_id or .mv_group_id is missing", { ... })

**Reproduces**: Validates replacement at `R/class-forecast-multivariate-sample.R:87`
**File**: `tests/testthat/test-class-forecast-multivariate-sample.R` (existing — test at line 216 matches old format)
**Setup**:
- Take multivariate sample forecast data, remove `sample_id`
**Action**:
- Call `as_forecast_sample_multivariate(data, ...)`
**Expected**:
- Should error with checkmate's subset error format
**Fails now because**:
- Existing test at line 216 expects `"Assertion on 'forecast' failed: Column 'sample_id' not found in data."`. After replacement with `assert_subset("sample_id", colnames(forecast))`, the message format changes to `"Assertion on 'c(\"sample_id\", \".mv_group_id\")' failed: Must be a subset of {...}, but has additional elements {'sample_id'}"` or similar.

---

### Test 7: test_that("get_pairwise_comparisons errors when compare column is missing", { ... })

**Reproduces**: Validates replacement at `R/pairwise-comparisons.R:127`
**File**: `tests/testthat/test-pairwise_comparison.R` (existing — tests at lines 363, 437)
**Setup**:
- Take `scores_sample_continuous`, remove the `model` column
**Action**:
- Call `get_pairwise_comparisons(scores, compare = "model", metric = "crps")`
**Expected**:
- Should error. After fix, error changes from `"Assertion on 'scores' failed: Column 'model' not found in data."` to `assert_subset`'s format.
**Fails now because**:
- Existing tests at lines 363 and 437 expect the old `"Column 'model' not found in data."` message. Must be updated.

---

### Test 8: test_that("get_pairwise_comparisons errors with custom message when by columns are missing", { ... })

**Reproduces**: Validates replacement at `R/pairwise-comparisons.R:142` (custom error message site)
**File**: `tests/testthat/test-pairwise_comparison.R` (existing — test at line 301)
**Setup**:
- Call `add_relative_skill(scores, compare = "model", by = "missing", metric = "crps")`
**Action**:
- The `by = "missing"` column doesn't exist
**Expected**:
- Should still error with `"Not all columns specified in `by` are present:"` (the custom `cli_abort` message). The embedded detail after the colon will change from `"Column 'missing' not found in data"` to checkmate's `"Must be a subset of {...}, but has additional elements {'missing'}"` format. The test at line 301 only checks the prefix `"Not all columns specified in `by` are present:"` so it should still pass if the match is prefix-only.
**Fails now because**:
- The test at line 301 matches `"Not all columns specified in \`by\` are present:"` which is the `cli_abort` message, not the inner `check_columns_present` message. This test should pass both before and after the fix, as the custom message wrapper is preserved. The inner format changes but the outer message stays the same. However, if the test matches the full message including the old inner text, it will need updating.

---

### Test 9: test_that("plot_wis errors when WIS component columns are missing", { ... })

**Reproduces**: Validates replacement at `R/plot-wis.R:53`
**File**: `tests/testthat/test-plot_wis.R` (existing — test at line 5)
**Setup**:
- Score a quantile forecast with only `wis` metric (no decomposition columns)
**Action**:
- Call `plot_wis(scores)`
**Expected**:
- Should error. After fix, the error message changes from `"Columns 'overprediction', 'underprediction', 'dispersion' not found in data."` to `assert_subset`'s format: `"Must be a subset of {...}, but has additional elements {'overprediction','underprediction','dispersion'}"`.
**Fails now because**:
- Existing test at line 11 expects `"Columns 'overprediction', 'underprediction', 'dispersion' not found in data."`. Must be updated to match new message format.

---

### Test 10: test_that("plot_correlations errors when 'metric' column is missing from non-correlation input", { ... })

**Reproduces**: Validates replacement at `R/get-correlations.R:88`
**File**: `tests/testthat/test-get-correlations.R` (existing or new test)
**Setup**:
- Pass a data object to `plot_correlations()` that is not a proper correlation matrix (no `metric` column)
**Action**:
- Call `plot_correlations(bad_input)`
**Expected**:
- Should error with `"Found correlations > 1 or missing \`metric\` column."` — this is the `cli_abort` message that wraps the check. This message is independent of `check_columns_present` format.
**Fails now because**:
- After the fix, `check_columns_present` is replaced by `check_subset`. The boolean usage `!isTRUE(col_present)` works identically with `check_subset` (returns error string on failure, `TRUE` on success). This test should pass both before and after the fix — it is a regression test.

---

### Test 11: test_that("check_columns_present NULL handling is preserved by replacement", { ... })

**Reproduces**: Edge case — `check_columns_present(data, NULL)` currently returns `TRUE`; `check_subset(NULL, choices)` also returns `TRUE` (treats NULL as empty set)
**File**: `tests/testthat/test-check-input-helpers.R` (to be removed with old tests, but the NULL-handling behavior should be verified at call sites)
**Setup**:
- Verify that any call site that could receive `NULL` columns still behaves correctly
**Action**:
- `check_subset(NULL, c("a", "b"))` should return `TRUE`
**Expected**:
- `TRUE` — empty/NULL set is trivially a subset of any set
**Fails now because**:
- This is a regression test. `check_columns_present` has explicit `if (is.null(columns)) return(TRUE)` handling. `check_subset` from checkmate handles NULL by treating it as an empty set (returns TRUE). The behavior should be equivalent, but should be verified.

---

## Existing Tests That Need Message Updates

The following existing test expectations match the old `"Column '...' not found in data"` format and must be updated to match `assert_subset`'s message format:

| File | Line(s) | Current expected message |
|------|---------|------------------------|
| `test-check-input-helpers.R` | 14-36, 48-56 | **Delete entirely** (tests the deleted function directly) |
| `test-class-forecast-quantile.R` | 74 | `"Column 'predicted' not found in data."` |
| `test-class-forecast-quantile.R` | 81 | `"Column 'observed' not found in data."` |
| `test-class-forecast-quantile.R` | 88 | `"Columns 'observed', 'predicted' not found in data."` |
| `test-class-forecast-multivariate-sample.R` | 216 | `"Column 'sample_id' not found in data."` |
| `test-pairwise_comparison.R` | 363 | `"Column 'model' not found in data."` |
| `test-pairwise_comparison.R` | 437 | `"Column 'model' not found in data."` |
| `test-plot_wis.R` | 11 | `"Columns 'overprediction', 'underprediction', 'dispersion' not found in data."` |

For each, update the expected string to match `checkmate::assert_subset`'s error message format. A safe approach is to use `expect_error(..., "Must be a subset of")` or `expect_error(..., "additional elements")` as the pattern.
