# Issue #454: Code refactoring: Deduplicate code in `get_coverage()` and `interval_coverage()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/454
- **Action Summary**: Update issue title to reference `interval_coverage()`. Refactor `get_coverage()` to delegate interval coverage computation to `interval_coverage()` or a shared internal helper, eliminating duplicated `quantile_to_interval()` + bounds-check pattern.
- **Status Note**: Original function `interval_coverage_deviation_quantile()` was removed. Issue title is outdated. Remaining duplication is between `get_coverage()` (data.table-level) and `interval_coverage()` (numeric scoring metric) — both call `quantile_to_interval()` + bounds check independently. Duplication is small (~5 lines of core logic).

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-get-coverage.R` (existing) and `tests/testthat/test-metrics-quantile.R` (existing)
**Testing patterns observed**: testthat 3e style, data.table fixtures, `example_quantile` built-in dataset, `setup.R` pre-computes scored objects, numeric vector test data defined at top of `test-metrics-quantile.R` (`observed = c(1, -15, 22)`, `predicted` as 3x5 matrix, `quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9)`), `vdiffr` for visual regression, `expect_no_condition()` for clean execution, `get_range_from_quantile()` as helper.

## Duplicated Code Analysis

Both functions share the same pattern:
1. Call `quantile_to_interval()` to convert quantile format to interval format with `lower`/`upper` columns
2. Perform bounds check: `(observed >= lower) & (observed <= upper)` to compute `interval_coverage`

**`get_coverage()`** (R/get-coverage.R:69-71): calls `quantile_to_interval(forecast, format = "wide")` on a data.table, then `interval_coverage := (observed <= upper) & (observed >= lower)`

**`interval_coverage()`** (R/metrics-quantile.R:361-363): calls `quantile_to_interval(observed, predicted, quantile_level)` on numeric vectors, then `interval_coverage := (observed >= lower) & (observed <= upper)`

The refactoring should extract the shared logic into a helper, or have `get_coverage()` delegate to `interval_coverage()` for the bounds-check step. Tests must verify that both functions produce identical results before and after refactoring.

### Test 1: test_that("get_coverage() interval coverage matches interval_coverage() for same data", { ... })

**Reproduces**: the core duplication — both functions independently compute the same bounds check, and we need to verify they agree
**File**: `tests/testthat/test-get-coverage.R`
**Setup**:
- Use `example_quantile` dataset, filtered to a single model (`"EuroCOVIDhub-ensemble"`) and a single target date to keep data small
- Call `as_forecast_quantile()` on the filtered data
- Extract the numeric vectors `observed`, `predicted` (as matrix), and `quantile_level` from the same filtered data
**Action**:
- Call `get_coverage(forecast, by = get_forecast_unit(forecast))` to get per-row interval coverage
- For the same forecast rows, call `interval_coverage(observed, predicted, quantile_level, interval_range = 50)` (and repeat for interval_range = 80 if those quantiles exist)
- Compare the interval_coverage column from `get_coverage()` against the boolean vector from `interval_coverage()` for matching interval ranges
**Expected**:
- The `interval_coverage` values from `get_coverage()` for a given `interval_range` must exactly equal the values returned by `interval_coverage()` for the same `interval_range` and same input data
- Use `expect_equal()` to compare the matched values
**Fails now because**:
- This test should PASS now — both implementations compute the same thing. This is a regression guard that ensures the refactored version remains equivalent. If the refactoring introduces a bug (e.g., off-by-one in bounds, wrong filtering), this test will catch it.

### Test 2: test_that("get_coverage() produces correct interval_coverage for known inputs", { ... })

**Reproduces**: verifies the bounds-check logic with hand-crafted data where the expected coverage values can be computed manually
**File**: `tests/testthat/test-get-coverage.R`
**Setup**:
- Create a minimal `forecast_quantile` object with known values:
  ```
  observed = 5 for all rows
  model = "m1", target = "t1", date = "2020-01-01", location = "loc1"
  quantile_level = c(0.25, 0.5, 0.75)
  predicted = c(3, 5, 7)   # observation IS inside the 50% interval [3, 7]
  ```
- Create a second forecast where `observed = 10` (OUTSIDE the interval [3, 7])
- Combine into a single `forecast_quantile` object with 2 forecast units
**Action**:
- Call `get_coverage()` with `by = get_forecast_unit(forecast)` to get per-forecast coverage
**Expected**:
- For observed=5, predicted=[3,5,7]: `interval_coverage` for the 50% interval should be `TRUE` (5 >= 3 and 5 <= 7)
- For observed=10, predicted=[3,5,7]: `interval_coverage` for the 50% interval should be `FALSE` (10 > 7)
- `quantile_coverage` for quantile_level=0.5 should be `TRUE` for observed=5 (5 <= 5) and `FALSE` for observed=10 (10 > 5)
- Use `expect_equal()` on specific rows
**Fails now because**:
- This test should PASS now. It is a regression guard to ensure the extracted/shared bounds-check logic computes the same values after refactoring.

### Test 3: test_that("interval_coverage() produces correct results for boundary cases", { ... })

**Reproduces**: boundary cases in the bounds check (observation exactly on lower/upper bound)
**File**: `tests/testthat/test-metrics-quantile.R`
**Setup**:
- Use numeric vectors:
  ```
  observed = c(3, 7, 5)   # exactly on lower, exactly on upper, inside
  predicted = matrix(c(3, 5, 7), nrow = 3, ncol = 3, byrow = TRUE)  # each row: [3, 5, 7]
  quantile_level = c(0.25, 0.5, 0.75)
  ```
**Action**:
- Call `interval_coverage(observed, predicted, quantile_level, interval_range = 50)`
**Expected**:
- All three should be `TRUE`: observation on lower bound (3 >= 3), on upper bound (7 <= 7), and inside (5 >= 3 and 5 <= 7)
- `expect_equal(result, c(TRUE, TRUE, TRUE))`
**Fails now because**:
- This test should PASS now. It guards against a refactoring that accidentally changes `>=`/`<=` to `>`/`<` in the bounds check.

### Test 4: test_that("interval_coverage() handles multiple interval ranges correctly", { ... })

**Reproduces**: verifying that when the shared helper handles multiple interval ranges, results are consistent
**File**: `tests/testthat/test-metrics-quantile.R`
**Setup**:
- Use numeric vectors with 5 quantile levels:
  ```
  observed = c(5)
  predicted = matrix(c(1, 3, 5, 7, 9), nrow = 1)  # quantiles for [0.1, 0.25, 0.5, 0.75, 0.9]
  quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9)
  ```
**Action**:
- Call `interval_coverage(observed, predicted, quantile_level, interval_range = 50)` — tests the 50% interval [3, 7]
- Call `interval_coverage(observed, predicted, quantile_level, interval_range = 80)` — tests the 80% interval [1, 9]
**Expected**:
- 50% interval: `TRUE` (5 is inside [3, 7])
- 80% interval: `TRUE` (5 is inside [1, 9])
- Use `expect_equal(result, TRUE)` for each
**Fails now because**:
- This test should PASS now. It ensures the refactored code correctly filters by interval_range and computes bounds on the right interval.

### Test 5: test_that("get_coverage() and interval_coverage() agree on edge case: observation outside all intervals", { ... })

**Reproduces**: regression guard for the case where the observation is outside all prediction intervals — both functions must agree
**File**: `tests/testthat/test-get-coverage.R`
**Setup**:
- Create a `forecast_quantile` object:
  ```
  observed = 100 for all rows
  model = "m1", target = "t1", date = "2020-01-01", location = "loc1"
  quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9)
  predicted = c(1, 3, 5, 7, 9)  # observation 100 is far outside all intervals
  ```
**Action**:
- Call `get_coverage()` with `by = get_forecast_unit(forecast)` and extract `interval_coverage` for all `interval_range` values
- Call `interval_coverage()` with same numeric data for `interval_range = 50` and `interval_range = 80`
**Expected**:
- All `interval_coverage` values should be `FALSE` (100 is outside both the 50% interval [3, 7] and 80% interval [1, 9])
- Both functions must agree: `expect_false()` for each
**Fails now because**:
- This test should PASS now. It guards against a refactoring that would incorrectly handle the case where the observation is completely outside all intervals.

### Test 6: test_that("get_coverage() works after refactoring with non-symmetric intervals", { ... })

**Reproduces**: the existing test for non-symmetric intervals continues to pass after refactoring
**File**: `tests/testthat/test-get-coverage.R`
**Setup**:
- This test already exists (test-get-coverage.R:32-54) but should be explicitly verified to still pass after refactoring
- Use `example_quantile` with some quantile levels removed (`0.2, 0.3, 0.5`)
**Action**:
- Call `get_coverage(test)` on the modified data
**Expected**:
- Missing interval_range values should have `NA` for `interval_coverage`
- Non-missing interval_range values should have no NAs
- `expect_no_condition()` — should not error or warn
**Fails now because**:
- This test PASSES now and already exists. It is listed here as a reminder that the refactoring must preserve NA-handling behavior when quantile pairs are incomplete. If the shared helper doesn't properly propagate NAs for missing quantile pairs, this test will catch it.

### Test 7: test_that("refactored interval coverage helper produces identical output to original implementations", { ... })

**Reproduces**: if a new shared internal helper is created (e.g., `compute_interval_coverage()`), this test verifies it directly
**File**: `tests/testthat/test-get-coverage.R` or `tests/testthat/test-helper-quantile-interval-range.R`
**Setup**:
- Use the full `example_quantile` dataset (all models, all dates)
- Compute coverage using the full `get_coverage()` pipeline
- Also score the same data using `score()` which calls `interval_coverage` under the hood
**Action**:
- Call `get_coverage(example_quantile, by = get_forecast_unit(example_quantile))` and capture the `interval_coverage` column
- Call `score(example_quantile)` and extract the `interval_coverage_50` and `interval_coverage_90` columns
- Compare these values at matching forecast unit + interval_range combinations
**Expected**:
- The `interval_coverage` from `get_coverage()` for `interval_range == 50` should match `interval_coverage_50` from `score()` across all forecast units
- The `interval_coverage` from `get_coverage()` for `interval_range == 90` should match `interval_coverage_90` from `score()` across all forecast units
- Use `expect_equal()` for comparison
**Fails now because**:
- This test should PASS now. It is a comprehensive regression guard using the full example dataset to ensure that the refactored shared logic produces results consistent with both the `get_coverage()` pathway and the `score()` → `interval_coverage()` pathway. Any bug in the shared helper would be caught by a disagreement between the two pathways.

## Notes for Implementation

- The refactoring is small: ~5 lines of core logic (call `quantile_to_interval()`, compute bounds check). The primary approaches are:
  1. **Have `get_coverage()` call `interval_coverage()` internally** for the bounds-check step, adapting the data format as needed
  2. **Extract a shared internal helper** (e.g., `compute_interval_coverage_from_bounds()`) that both functions call
- The bounds check in `get_coverage()` uses `(observed <= upper) & (observed >= lower)` while `interval_coverage()` uses `(observed >= lower) & (observed <= upper)` — these are logically identical but the order differs. The refactoring should standardize to one order.
- `get_coverage()` operates on data.table forecast objects; `interval_coverage()` operates on numeric vectors. The `quantile_to_interval()` dispatcher already handles both input types, so the shared helper should work at the data.table level (after `quantile_to_interval()` has been called).
- Existing tests in `test-get-coverage.R` and `test-metrics-quantile.R` already cover the basic functionality. The new tests above focus on cross-validation between the two code paths and boundary cases to guard against regression.
