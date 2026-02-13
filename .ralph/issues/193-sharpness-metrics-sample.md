# Issue #193: Add more metrics to assess sharpness of forecasts in a sample format

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/193
- **Action Summary**: Implement `sd_sample()` and `iqr_sample()` in `R/metrics-sample.R` following the `mad_sample()` pattern. Add both to the default metric list in `get_metrics.forecast_sample()`. Add tests, documentation, and NAMESPACE exports.
- **Status Note**: Still relevant. `mad_sample()` is the only dedicated sharpness metric. SD and IQR (specifically requested) are not implemented. Both are trivial to add following the `mad_sample()` pattern (data-independent, apply function row-wise over predicted matrix). `dispersion_sample()` (CRPS component) also measures spread but is observation-dependent. `scoringRules` provides no additional sharpness measures.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-metrics-sample.R` (existing)
**Testing patterns observed**: testthat with `rpois()`/`rnorm()` generated data, `assert_input_sample()` for input validation, `apply(predicted, MARGIN = 1, FUN)` pattern for row-wise metric computation. Observation-independent sharpness metrics use `observed = NULL` placeholder parameter. Existing `mad_sample()` tests are minimal (only error-on-missing-predicted).

### Test 1: test_that("sd_sample() returns correct standard deviations", { ... })

**Reproduces**: the absence of `sd_sample()` as a function
**Setup**:
- Create a 3x5 numeric matrix `predicted` with known values where the SD of each row is manually calculable. For example:
  - Row 1: `c(1, 1, 1, 1, 1)` (SD = 0)
  - Row 2: `c(1, 2, 3, 4, 5)` (SD = `sd(1:5)`)
  - Row 3: `c(10, 10, 10, 10, 20)` (SD = `sd(c(10, 10, 10, 10, 20))`)
**Action**:
- Call `sd_sample(predicted = predicted)`
**Expected**:
- Returns a numeric vector of length 3
- `expect_equal(result[1], 0)` (constant row has zero SD)
- `expect_equal(result[2], sd(1:5))`
- `expect_equal(result[3], sd(c(10, 10, 10, 10, 20)))`
**Fails now because**:
- `sd_sample()` does not exist; calling it produces an error ("could not find function")

### Test 2: test_that("sd_sample() works with observed = NULL", { ... })

**Reproduces**: correct handling of the `observed` placeholder parameter
**Setup**:
- `predicted <- replicate(50, rpois(n = 10, lambda = 1:10))`
**Action**:
- Call `sd_sample(observed = NULL, predicted = predicted)`
- Call `sd_sample(predicted = predicted)` (omitting observed)
**Expected**:
- Both calls succeed without error
- Both return identical numeric vectors of length 10
- Results are identical (observed is truly ignored)
**Fails now because**:
- `sd_sample()` does not exist

### Test 3: test_that("sd_sample() throws an error when missing 'predicted'", { ... })

**Reproduces**: input validation following `mad_sample()` pattern
**Setup**:
- No setup needed
**Action**:
- Call `sd_sample()` with no arguments
**Expected**:
- `expect_error(sd_sample())`
**Fails now because**:
- `sd_sample()` does not exist

### Test 4: test_that("sd_sample() works with a single observation row", { ... })

**Reproduces**: edge case for single-row input (vector or 1-row matrix)
**Setup**:
- `predicted <- matrix(c(1, 2, 3, 4, 5), nrow = 1)`
**Action**:
- Call `sd_sample(predicted = predicted)`
**Expected**:
- Returns a numeric vector of length 1
- `expect_equal(result, sd(1:5))`
**Fails now because**:
- `sd_sample()` does not exist

### Test 5: test_that("iqr_sample() returns correct interquartile ranges", { ... })

**Reproduces**: the absence of `iqr_sample()` as a function
**Setup**:
- Create a 3x5 numeric matrix `predicted` with known values:
  - Row 1: `c(1, 1, 1, 1, 1)` (IQR = 0)
  - Row 2: `c(1, 2, 3, 4, 5)` (IQR = `IQR(1:5)`)
  - Row 3: `c(10, 10, 10, 10, 20)` (IQR = `IQR(c(10, 10, 10, 10, 20))`)
**Action**:
- Call `iqr_sample(predicted = predicted)`
**Expected**:
- Returns a numeric vector of length 3
- `expect_equal(result[1], 0)` (constant row has zero IQR)
- `expect_equal(result[2], IQR(1:5))`
- `expect_equal(result[3], IQR(c(10, 10, 10, 10, 20)))`
**Fails now because**:
- `iqr_sample()` does not exist

### Test 6: test_that("iqr_sample() works with observed = NULL", { ... })

**Reproduces**: correct handling of the `observed` placeholder parameter
**Setup**:
- `predicted <- replicate(50, rpois(n = 10, lambda = 1:10))`
**Action**:
- Call `iqr_sample(observed = NULL, predicted = predicted)`
- Call `iqr_sample(predicted = predicted)` (omitting observed)
**Expected**:
- Both calls succeed without error
- Both return identical numeric vectors of length 10
- Results are identical (observed is truly ignored)
**Fails now because**:
- `iqr_sample()` does not exist

### Test 7: test_that("iqr_sample() throws an error when missing 'predicted'", { ... })

**Reproduces**: input validation following `mad_sample()` pattern
**Setup**:
- No setup needed
**Action**:
- Call `iqr_sample()` with no arguments
**Expected**:
- `expect_error(iqr_sample())`
**Fails now because**:
- `iqr_sample()` does not exist

### Test 8: test_that("iqr_sample() works with a single observation row", { ... })

**Reproduces**: edge case for single-row input (vector or 1-row matrix)
**Setup**:
- `predicted <- matrix(c(1, 2, 3, 4, 5), nrow = 1)`
**Action**:
- Call `iqr_sample(predicted = predicted)`
**Expected**:
- Returns a numeric vector of length 1
- `expect_equal(result, IQR(1:5))`
**Fails now because**:
- `iqr_sample()` does not exist

### Test 9: test_that("sd_sample and iqr_sample are included in default metrics for forecast_sample", { ... })

**Reproduces**: the new metrics must be registered in `get_metrics.forecast_sample()`
**Setup**:
- Use `example_sample_continuous` (built-in dataset)
**Action**:
- Call `get_metrics(example_sample_continuous)`
**Expected**:
- `expect_true("sd" %in% names(result))`
- `expect_true("iqr" %in% names(result))`
- The metric functions are `sd_sample` and `iqr_sample` respectively
**Fails now because**:
- `sd_sample` and `iqr_sample` are not in the default metric list returned by `get_metrics.forecast_sample()`

### Test 10: test_that("score() output includes sd and iqr columns for sample forecasts", { ... })

**Reproduces**: end-to-end integration — new metrics must appear in scored output
**Setup**:
- Use `example_sample_continuous` (built-in dataset)
**Action**:
- Call `score(example_sample_continuous)`
**Expected**:
- `expect_true("sd" %in% colnames(result))`
- `expect_true("iqr" %in% colnames(result))`
- The `sd` and `iqr` columns contain finite numeric values (no NAs, no Infs)
- `expect_true(all(result$sd >= 0))` (SD is non-negative)
- `expect_true(all(result$iqr >= 0))` (IQR is non-negative)
**Fails now because**:
- `sd_sample` and `iqr_sample` do not exist and are not registered as default metrics

### Test 11: test_that("sd_sample and iqr_sample agree with apply-based computation", { ... })

**Reproduces**: correctness against a reference implementation
**Setup**:
- `set.seed(42)`
- `predicted <- replicate(200, rpois(n = 30, lambda = 1:30))`
**Action**:
- Call `sd_sample(predicted = predicted)` and `iqr_sample(predicted = predicted)`
- Compute reference: `apply(predicted, 1, sd)` and `apply(predicted, 1, IQR)`
**Expected**:
- `expect_equal(sd_result, apply(predicted, 1, sd))`
- `expect_equal(iqr_result, apply(predicted, 1, IQR))`
**Fails now because**:
- Neither function exists
