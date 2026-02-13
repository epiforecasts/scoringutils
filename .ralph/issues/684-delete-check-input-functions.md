# Issue #684: Delete functions `check_input_sample()`, `check_input_quantile()`, `check_input_interval()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/684
- **Action Summary**: Close as duplicate of #939. If handled independently, delete the 3 function definitions, their man pages, roxygen cross-references, and corresponding test blocks.
- **Status Note**: Still relevant but fully superseded by #939, which covers all 5 `check_input_*` functions. These 3 functions are dead code: never called in production, not exported, only tested. A comment in `check-input-helpers.R` explicitly marks them for future deletion. Should be closed as duplicate of #939.

## Test Specifications

**Designed**: 2026-02-13
**Test files**: `tests/testthat/test-inputs-scoring-functions.R` (existing), `tests/testthat/test-metrics-quantile.R` (existing), `tests/testthat/test-metrics-interval-range.R` (existing)
**Testing patterns observed**: testthat with `expect_true`, `expect_match`, `expect_error`, `expect_no_condition`. No helper-*.R files exist. Setup in `setup.R` pre-computes scores from example datasets. Tests use inline numeric vectors/matrices as fixtures.

## Context

The three functions to delete are:

1. `check_input_sample()` — defined at `R/metrics-sample.R:33-36`, wraps `check_try(assert_input_sample(...))`
2. `check_input_quantile()` — defined at `R/metrics-quantile.R:49-54`, wraps `check_try(assert_input_quantile(...))`
3. `check_input_interval()` — defined at `R/metrics-interval-range.R:63-68`, wraps `check_try(assert_input_interval(...))`

None of these are exported. None are called anywhere in production code. Each is tested in exactly one test block. Their `assert_input_*` counterparts ARE used extensively by scoring functions and must NOT be affected.

Additional cleanup required:
- Delete man pages: `man/check_input_sample.Rd`, `man/check_input_quantile.Rd`, `man/check_input_interval.Rd`
- Fix roxygen `@inherit check_input_sample return description` tags in `check_input_quantile` and `check_input_interval` definitions (these are circular references among the deleted functions, but the `@inherit` tag in `check_input_quantile.R:47` and `check_input_interval.R:61` reference `check_input_sample` — since all 3 are deleted, these tags go away with the code)
- Do NOT delete `check_try()` — it is still used by `check_input_binary`, `check_input_point`, and `check_dims_ok_point`

### Test 1: test_that("check_input_sample is no longer defined", { ... })

**Reproduces**: confirms the dead code has been removed
**File**: `tests/testthat/test-inputs-scoring-functions.R` (replaces existing `check_input_sample()` test block at lines 23-36)
**Setup**:
- No special setup needed
**Action**:
- Call `check_input_sample(1:10, matrix(1:20, nrow = 10))` — this should error because the function no longer exists
**Expected**:
- `expect_error(check_input_sample(1:10, matrix(1:20, nrow = 10)))` — the function should not be found, producing an error like "could not find function"
**Fails now because**:
- The function currently exists and returns `TRUE` for valid input, so the call succeeds rather than erroring

### Test 2: test_that("assert_input_sample still works after check_input_sample removal", { ... })

**Reproduces**: ensures that the production function was not accidentally deleted
**File**: `tests/testthat/test-inputs-scoring-functions.R` (replaces the old `check_input_sample` test block)
**Setup**:
- `observed <- 1:10`
- `predicted <- matrix(1:20, nrow = 10)`
**Action**:
- Call `assert_input_sample(observed, predicted)` with valid input
- Call `assert_input_sample(1:10, 1:11)` with invalid input (dimension mismatch)
**Expected**:
- `expect_no_condition(assert_input_sample(observed, predicted))` — valid input returns silently
- `expect_error(assert_input_sample(1:10, 1:11))` — invalid input still throws an assertion error
**Fails now because**:
- This test should PASS both before and after the fix (regression guard). It only fails if the wrong function is accidentally deleted.

### Test 3: test_that("check_input_quantile is no longer defined", { ... })

**Reproduces**: confirms the dead code has been removed
**File**: `tests/testthat/test-metrics-quantile.R` (replaces existing `check_input_quantile()` test block at lines 22-39)
**Setup**:
- No special setup needed
**Action**:
- Call `check_input_quantile(1:10, matrix(1:20, nrow = 10), quantile_level = c(0.1, 0.9))`
**Expected**:
- `expect_error(check_input_quantile(1:10, matrix(1:20, nrow = 10), quantile_level = c(0.1, 0.9)))` — function should no longer exist
**Fails now because**:
- The function currently exists and returns `TRUE` for valid input

### Test 4: test_that("assert_input_quantile still works after check_input_quantile removal", { ... })

**Reproduces**: ensures that the production function was not accidentally deleted
**File**: `tests/testthat/test-metrics-quantile.R` (replaces the old `check_input_quantile` test block)
**Setup**:
- `observed <- 1:10`
- `predicted <- matrix(1:20, nrow = 10)`
- `quantile_level <- c(0.1, 0.9)`
**Action**:
- Call `assert_input_quantile(observed, predicted, quantile_level)` with valid input
- Call `assert_input_quantile(1:10, matrix(1:20, nrow = 10), quantile_level = seq(0.1, 0.9, length.out = 8))` with mismatched dimensions
**Expected**:
- `expect_no_condition(assert_input_quantile(observed, predicted, quantile_level))` — valid input returns silently
- `expect_error(assert_input_quantile(1:10, matrix(1:20, nrow = 10), quantile_level = seq(0.1, 0.9, length.out = 8)))` — dimension mismatch still detected
**Fails now because**:
- This test should PASS both before and after the fix (regression guard).

### Test 5: test_that("check_input_interval is no longer defined", { ... })

**Reproduces**: confirms the dead code has been removed
**File**: `tests/testthat/test-metrics-interval-range.R` (replaces existing `check_input_interval()` test block at lines 30-39)
**Setup**:
- Uses the existing file-level `observed`, `lower`, `upper`, `interval_range` fixtures already defined in that test file
**Action**:
- Call `check_input_interval(observed, lower, upper, interval_range)`
**Expected**:
- `expect_error(check_input_interval(observed, lower, upper, interval_range))` — function should no longer exist
**Fails now because**:
- The function currently exists and returns `TRUE` (no condition) for valid input

### Test 6: test_that("assert_input_interval still works after check_input_interval removal", { ... })

**Reproduces**: ensures that the production function was not accidentally deleted
**File**: `tests/testthat/test-metrics-interval-range.R` (replaces the old `check_input_interval` test block)
**Setup**:
- Uses the existing file-level `observed`, `lower`, `upper`, `interval_range` fixtures
**Action**:
- Call `assert_input_interval(observed, lower, upper, interval_range)` with valid input
- Call `assert_input_interval(observed, upper, lower, interval_range)` with upper < lower (reversed bounds)
**Expected**:
- `expect_no_condition(assert_input_interval(observed, lower, upper, interval_range))` — valid input returns silently
- `expect_error(assert_input_interval(observed, upper, lower, interval_range), "All values in `upper` need to be greater than or equal")` — reversed bounds still caught
**Fails now because**:
- This test should PASS both before and after the fix (regression guard).

### Test 7: test_that("scoring functions still work end-to-end without check_input_* wrappers", { ... })

**Reproduces**: ensures no scoring pipeline is broken by the deletion
**File**: `tests/testthat/test-inputs-scoring-functions.R` (add at end of file)
**Setup**:
- `observed_s <- 1:5`
- `predicted_s <- matrix(rnorm(50), nrow = 5)` (sample format)
- `observed_q <- 1:5`
- `predicted_q <- matrix(sort(rnorm(25)), nrow = 5)` (quantile format, pre-sorted per row)
- `quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)`
**Action**:
- Call `crps_sample(observed_s, predicted_s)` — uses `assert_input_sample` internally
- Call `bias_quantile(observed_q, predicted_q, quantile_level)` — uses `assert_input_quantile` internally
**Expected**:
- `expect_no_condition(crps_sample(observed_s, predicted_s))` — scoring function works without `check_input_sample`
- `expect_no_condition(bias_quantile(observed_q, predicted_q, quantile_level))` — scoring function works without `check_input_quantile`
**Fails now because**:
- This test should PASS both before and after the fix (regression guard confirming that production scoring functions use `assert_input_*`, not `check_input_*`).
