# Issue #880: Allow users to specify the quantile in `as_forecast_point.forecast_quantile()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/880
- **Action Summary**: Add a `quantile_level` parameter (default `0.5`) to `as_forecast_point.forecast_quantile()`, replacing the two hard-coded `0.5` references, then add tests and documentation.
- **Status Note**: Still relevant. Function hard-codes `0.5` on two lines (107, 115) in `R/class-forecast-quantile.R`. Design decisions already resolved in PR #876 discussion: parameter name `quantile_level`, default `0.5`, passed via `...`. No work done since issue opened.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-point.R` (existing)
**Testing patterns observed**: testthat with `expect_no_condition()`, `expect_error()`, `expect_s3_class()`, `expect_identical()`, `expect_equal()`. Data fixtures use built-in `example_quantile` and `example_point` datasets. `data.table::copy()` and `data.table::as.data.table()` used for test data manipulation. `na.omit()` used to clean example data before conversion. `suppressMessages()`/`suppressWarnings()` sometimes wrap `as_forecast_*()` calls.

### Test 1: test_that("as_forecast_point.forecast_quantile() uses default quantile_level = 0.5", { ... })

**Reproduces**: verifies the default behavior is preserved after adding the parameter
**Setup**:
- Use `na.omit(example_quantile)` as input
**Action**:
- Call `as_forecast_point(na.omit(as_forecast_quantile(example_quantile)))` with no `quantile_level` argument
**Expected**:
- Result should be a `forecast_point` object (classes: `forecast_point`, `forecast`, `data.table`, `data.frame`)
- The `predicted` column values should match the `predicted` values from the original data where `quantile_level == 0.5`
- No `quantile_level` column should be present in the result
- No error or warning should be raised
**Fails now because**:
- This test should PASS with current code (regression guard). The function currently hard-codes 0.5, and this test verifies that the new default matches the old behavior.

### Test 2: test_that("as_forecast_point.forecast_quantile() accepts custom quantile_level", { ... })

**Reproduces**: the core missing feature — inability to specify a different quantile level
**Setup**:
- Use `na.omit(example_quantile)` as input, converted to `forecast_quantile`
- Pick quantile level `0.25` (known to exist in `example_quantile`)
**Action**:
- Call `as_forecast_point(data, quantile_level = 0.25)` where `data` is the `forecast_quantile` object
**Expected**:
- Result should be a `forecast_point` object
- The `predicted` column values should match exactly the `predicted` values from the original data where `quantile_level == 0.25`
- No `quantile_level` column should be present in the result
- No error or warning should be raised
**Fails now because**:
- The current function signature is `as_forecast_point.forecast_quantile(data, ...)` with no `quantile_level` parameter. The `...` is unused. Passing `quantile_level = 0.25` would either be silently ignored (extracting 0.5 anyway due to hard-coding) or error. The hard-coded `0.5` on lines 107 and 115 means the function always filters to the median.

### Test 3: test_that("as_forecast_point.forecast_quantile() errors when requested quantile_level is not present", { ... })

**Reproduces**: edge case — requesting a quantile that doesn't exist in the data
**Setup**:
- Use `na.omit(example_quantile)` as input, converted to `forecast_quantile`
- Pick quantile level `0.33` (NOT present in `example_quantile` which has levels 0.01, 0.025, 0.05, 0.10, ..., 0.95, 0.975, 0.99)
**Action**:
- Call `as_forecast_point(data, quantile_level = 0.33)`
**Expected**:
- Should error with a message indicating that the requested `quantile_level` is not present in the data (matching the `assert_subset()` pattern already used on line 107 for 0.5)
**Fails now because**:
- Currently, `assert_subset(0.5, unique(data$quantile_level))` only checks for 0.5. After the fix, this assertion should use the user-supplied `quantile_level` value. This test verifies the error path for the new parameter.

### Test 4: test_that("as_forecast_point.forecast_quantile() errors when quantile_level is not a single numeric value", { ... })

**Reproduces**: input validation edge case — bad type or length for `quantile_level`
**Setup**:
- Use `na.omit(example_quantile)` as input, converted to `forecast_quantile`
**Action**:
- Call `as_forecast_point(data, quantile_level = c(0.25, 0.75))` (vector of length 2)
- Call `as_forecast_point(data, quantile_level = "0.5")` (character instead of numeric)
**Expected**:
- Both calls should error. The first because `quantile_level` must be a single value (point forecasts are scalar). The second because the type is wrong. Error messages should be informative (e.g., from `checkmate::assert_number()` or similar).
**Fails now because**:
- The current code doesn't accept a `quantile_level` argument at all. After the fix, the new parameter needs input validation to reject invalid values. These tests verify the validation works.

### Test 5: test_that("as_forecast_point.forecast_quantile() with custom quantile_level produces correct predicted values", { ... })

**Reproduces**: correctness check with manually constructed data
**Setup**:
- Create a minimal `forecast_quantile` object with known values:
  ```
  data.frame(
    observed = c(10, 10, 20, 20),
    predicted = c(8, 12, 18, 22),
    quantile_level = c(0.25, 0.75, 0.25, 0.75),
    id = c(1, 1, 2, 2)
  )
  ```
  Convert via `as_forecast_quantile()`
**Action**:
- Call `as_forecast_point(data, quantile_level = 0.25)`
**Expected**:
- Result should have 2 rows (one per forecast unit)
- `predicted` values should be `c(8, 18)` — the values at `quantile_level == 0.25`
- `observed` values should be `c(10, 20)`
- No `quantile_level` column present
**Fails now because**:
- The function hard-codes `quantile_level == 0.5` on line 115 of `R/class-forecast-quantile.R`. With the data above that only has 0.25 and 0.75, the function would error at `assert_subset(0.5, ...)` since 0.5 is not present. Even if 0.5 were added to the data, the function would always extract 0.5, not 0.25.
