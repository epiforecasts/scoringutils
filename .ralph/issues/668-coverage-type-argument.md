# Issue #668: Add argument to control what kind of coverage is computed by `get_coverage()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/668
- **Action Summary**: Add a `type = c("quantile", "interval")` argument to `get_coverage()` that conditionally computes interval coverage, quantile coverage, or both. Update downstream column handling, summarisation, and tests.
- **Status Note**: Still relevant and unimplemented. `get_coverage()` always computes all 4 coverage metrics with no user control. The issue body contains a near-complete code proposal (with a minor bug to fix). The function structure already cleanly separates interval and quantile computation paths. On `scoringutils-2.x` milestone.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-get-coverage.R` (existing)
**Testing patterns observed**: testthat with data.table operations; `example_quantile` used as primary test data; existing tests check column names via `sort(colnames(cov))`, check row counts, check output class; `get_forecast_unit()` and `get_range_from_quantile()` used as helpers; `expect_no_condition()` used to verify no warnings/errors; `na.omit()` applied to example data before use.

### Test 1: test_that("get_coverage() with type = 'interval' returns only interval coverage columns", { ... })

**Reproduces**: the core missing feature — inability to request only interval coverage
**Setup**:
- Use `example_quantile` as input (already a `forecast_quantile` object)
**Action**:
- Call `get_coverage(example_quantile, by = "model", type = "interval")`
**Expected**:
- Result is a `data.table` (class `c("data.table", "data.frame")`)
- Result contains columns `"interval_coverage"` and `"interval_coverage_deviation"`
- Result does NOT contain columns `"quantile_coverage"` or `"quantile_coverage_deviation"`
- Result contains `"interval_range"` column (needed for interval coverage grouping)
- Result contains `"model"` column (from `by`)
- `interval_coverage` values are all numeric and between 0 and 1
**Fails now because**:
- `get_coverage()` does not accept a `type` argument; calling with `type = "interval"` will produce an "unused argument" error

### Test 2: test_that("get_coverage() with type = 'quantile' returns only quantile coverage columns", { ... })

**Reproduces**: the core missing feature — inability to request only quantile coverage
**Setup**:
- Use `example_quantile` as input
**Action**:
- Call `get_coverage(example_quantile, by = "model", type = "quantile")`
**Expected**:
- Result is a `data.table`
- Result contains columns `"quantile_coverage"` and `"quantile_coverage_deviation"`
- Result does NOT contain columns `"interval_coverage"` or `"interval_coverage_deviation"`
- Result contains `"quantile_level"` column (needed for quantile coverage grouping)
- Result contains `"model"` column (from `by`)
- `quantile_coverage` values are all numeric and between 0 and 1
- Result should NOT contain `"interval_range"` column (not relevant when only quantile coverage requested)
**Fails now because**:
- `get_coverage()` does not accept a `type` argument; calling with `type = "quantile"` will produce an "unused argument" error

### Test 3: test_that("get_coverage() with type = c('quantile', 'interval') returns both (default behavior)", { ... })

**Reproduces**: verifies the default value preserves backward compatibility
**Setup**:
- Use `example_quantile` as input
**Action**:
- Call `get_coverage(example_quantile, by = "model")` (no `type` argument — uses default)
- Also call `get_coverage(example_quantile, by = "model", type = c("quantile", "interval"))` (explicit default)
**Expected**:
- Both calls produce identical results (use `expect_identical()`)
- Result contains all 4 coverage columns: `"interval_coverage"`, `"interval_coverage_deviation"`, `"quantile_coverage"`, `"quantile_coverage_deviation"`
- Result contains both `"quantile_level"` and `"interval_range"` columns
- This matches the current behavior of `get_coverage()` exactly — backward compatible
**Fails now because**:
- The explicit `type = c("quantile", "interval")` call will produce an "unused argument" error. The implicit (no `type`) call should work and produce the same results as current code.

### Test 4: test_that("get_coverage() with type = 'interval' produces correct coverage values", { ... })

**Reproduces**: validates numerical correctness of interval-only coverage
**Setup**:
- Create a small, hand-crafted `forecast_quantile` object with known coverage properties. For example:
  - 1 model, 1 target, 1 location, 1 date
  - Quantile levels: 0.25, 0.75 (forming a 50% prediction interval)
  - Predicted values: lower=10, upper=20
  - Observed value: 15 (inside interval → coverage = TRUE)
- Create a second forecast where observed = 25 (outside interval → coverage = FALSE)
- Wrap both in a single `forecast_quantile` object using `as_forecast_quantile()`
**Action**:
- Call `get_coverage(test_data, by = get_forecast_unit(test_data), type = "interval")`
**Expected**:
- For the first forecast unit: `interval_coverage` = 1 (TRUE)
- For the second forecast unit: `interval_coverage` = 0 (FALSE)
- `interval_coverage_deviation` = `interval_coverage - interval_range / 100` for each row
- No quantile coverage columns present
**Fails now because**:
- `get_coverage()` does not accept a `type` argument

### Test 5: test_that("get_coverage() with type = 'quantile' produces correct coverage values", { ... })

**Reproduces**: validates numerical correctness of quantile-only coverage
**Setup**:
- Create a small, hand-crafted `forecast_quantile` object:
  - 1 model, 1 target, 1 location, 1 date
  - Quantile levels: 0.25, 0.5, 0.75
  - Predicted values: 10, 15, 20
  - Observed value: 12 (above 0.25 quantile, below 0.5 and 0.75)
**Action**:
- Call `get_coverage(test_data, by = get_forecast_unit(test_data), type = "quantile")`
**Expected**:
- For quantile_level 0.25: `quantile_coverage` = 1 (12 > 10, observed > predicted, so `observed <= predicted` is FALSE = 0). Wait — re-read: `quantile_coverage := observed <= predicted`. So: observed=12, predicted=10 → 12 <= 10 is FALSE → quantile_coverage = 0
- For quantile_level 0.5: observed=12, predicted=15 → 12 <= 15 is TRUE → quantile_coverage = 1
- For quantile_level 0.75: observed=12, predicted=20 → 12 <= 20 is TRUE → quantile_coverage = 1
- `quantile_coverage_deviation` = `quantile_coverage - quantile_level` for each row
- No interval coverage columns present
**Fails now because**:
- `get_coverage()` does not accept a `type` argument

### Test 6: test_that("get_coverage() errors for invalid type argument", { ... })

**Reproduces**: validates input checking on the new `type` parameter
**Setup**:
- Use `example_quantile` as input
**Action**:
- Call `get_coverage(example_quantile, type = "invalid")`
- Call `get_coverage(example_quantile, type = c("quantile", "invalid"))`
- Call `get_coverage(example_quantile, type = NULL)`
**Expected**:
- All three calls should produce an error (via `checkmate::assert_subset`)
- Error message should indicate that the value is not a valid subset of `c("quantile", "interval")`
**Fails now because**:
- `get_coverage()` does not accept a `type` argument at all, so the error would be "unused argument" rather than the expected subset assertion error

### Test 7: test_that("get_coverage() with type = 'interval' works with non-symmetric prediction intervals", { ... })

**Reproduces**: edge case — ensures interval-only mode handles missing quantile pairs gracefully
**Setup**:
- Copy `example_quantile` and remove some quantile levels (e.g., 0.2, 0.3, 0.5) to create asymmetric intervals, same as the existing test on line 32 of `test-get-coverage.R`
**Action**:
- Call `get_coverage(test, type = "interval")` with `expect_no_condition()`
**Expected**:
- No warnings or errors
- Interval coverages for ranges corresponding to removed quantiles should be `NA`
- Other interval coverages should be valid (not NA)
- No quantile coverage columns present
**Fails now because**:
- `get_coverage()` does not accept a `type` argument

### Test 8: test_that("get_coverage() with type = 'quantile' summarises correctly with by argument", { ... })

**Reproduces**: verifies correct summarisation when requesting only quantile coverage with a specific `by` grouping
**Setup**:
- Use `example_quantile` as input
**Action**:
- Call `get_coverage(example_quantile, by = "model", type = "quantile")`
**Expected**:
- Result has one row per unique combination of `model` and `quantile_level`
- No `interval_range` column present (since interval coverage is not computed)
- `quantile_coverage` values should be proportions (between 0 and 1), representing the mean coverage across all forecast units within each model+quantile_level group
**Fails now because**:
- `get_coverage()` does not accept a `type` argument
