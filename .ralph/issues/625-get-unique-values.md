# Issue #625: Implement a `get_unique_values()` function

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/625
- **Action Summary**: Implement a new exported `get_unique_values()` function that takes a forecast object, calls `get_forecast_unit()` to identify relevant columns, and returns a summary table of unique value counts per column. Optionally integrate into `print.forecast()`.
- **Status Note**: Still relevant. No implementation work done. Function does not exist. Would compute `length(unique(values))` per forecast-unit column (via `get_forecast_unit()`), optionally grouped by model. Similar diagnostic purpose to `get_forecast_counts()` but different output.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-get-unique-values.R` (new)
**Testing patterns observed**: testthat + data.table fixtures, example datasets (`example_quantile`, `example_binary`, `example_sample_continuous`, etc.) used directly, `setup.R` pre-computes scores objects, `checkmate::assert_*` used for input validation, snapshot tests used for plot outputs (via `vdiffr`), tests verify types, dimensions, and exact values.

### Test 1: test_that("get_unique_values() works with a quantile forecast", { ... })

**Reproduces**: core functionality — returns correct unique value counts for each forecast unit column
**Setup**:
- Use `example_quantile` dataset, passed through `as_forecast_quantile()`
**Action**:
- Call `get_unique_values(forecast)` with no additional arguments
**Expected**:
- Returns a data.table (or data.frame) with at least two columns: one for the column name and one for the count of unique values
- The column name column should contain exactly the forecast unit columns: `"location"`, `"target_end_date"`, `"target_type"`, `"location_name"`, `"forecast_date"`, `"model"`, `"horizon"`
- The unique value counts should be: location=4, target_end_date=30 (or fewer after NA removal), target_type=2, location_name=4, forecast_date=12 (or fewer), model=5 (or 4 after NA removal), horizon=4
- Use `expect_s3_class(result, c("data.table", "data.frame"))` and `expect_true("column" %in% names(result))` (or equivalent column names the implementation uses)
**Fails now because**:
- `get_unique_values()` does not exist; calling it produces `Error: could not find function "get_unique_values"`

### Test 2: test_that("get_unique_values() works with different forecast types", { ... })

**Reproduces**: function works across all forecast types, not just quantile
**Setup**:
- Use `example_binary` (via `as_forecast_binary()`) and `example_sample_continuous` (via `as_forecast_sample()`)
**Action**:
- Call `get_unique_values()` on each forecast object
**Expected**:
- For `example_binary`: returns unique counts for its forecast unit columns (location, location_name, target_end_date, target_type, forecast_date, model, horizon). No `quantile_level` or `sample_id` columns should appear (these are protected).
- For `example_sample_continuous`: same pattern, forecast unit columns only. `sample_id` should not appear in the output (it is a protected column).
- Both results should be data.tables with the same structure
**Fails now because**:
- `get_unique_values()` does not exist

### Test 3: test_that("get_unique_values() returns correct output structure", { ... })

**Reproduces**: verifies exact output format
**Setup**:
- Create a minimal data.table with known values, e.g.:
  ```
  data.table(
    location = c("A", "A", "B"),
    model = c("m1", "m2", "m1"),
    observed = c(1, 2, 3),
    predicted = c(1.1, 2.1, 3.1)
  )
  ```
  Convert to a forecast object (e.g., `as_forecast_point()`)
**Action**:
- Call `get_unique_values(forecast)` on this minimal forecast
**Expected**:
- Returns a data.table with exactly 2 rows (one for `location`, one for `model` — the forecast unit columns)
- The unique count for `location` is 2 (A, B)
- The unique count for `model` is 2 (m1, m2)
- `observed` and `predicted` should NOT appear (they are protected columns)
- Use `expect_identical()` for exact value matching
**Fails now because**:
- `get_unique_values()` does not exist

### Test 4: test_that("get_unique_values() accepts a `by` argument for grouping", { ... })

**Reproduces**: optional grouping by a column (e.g., by model) to see unique values per group
**Setup**:
- Use `example_quantile` via `as_forecast_quantile()`
**Action**:
- Call `get_unique_values(forecast, by = "model")` (or similar grouping argument)
**Expected**:
- Returns a data.table where results are grouped by model
- Each model has its own row(s) showing unique value counts for the forecast unit columns (excluding "model" itself since it's the grouping variable)
- This tests the optional `by` parameter mentioned in the issue ("optionally grouped by model")
- Use `expect_true(all(c("model") %in% names(result)))` and verify per-model counts are plausible (e.g., some models may cover fewer locations than others)
**Fails now because**:
- `get_unique_values()` does not exist

### Test 5: test_that("get_unique_values() errors on non-forecast input", { ... })

**Reproduces**: proper input validation
**Setup**:
- Use a plain data.frame or character vector that is not a forecast object
**Action**:
- Call `get_unique_values("not a forecast")` and `get_unique_values(42)`
**Expected**:
- Produces an informative error (e.g., via `cli_abort()` or `checkmate::assert_*`), matching `expect_error(get_unique_values("not a forecast"))`
- The exact error message pattern depends on implementation, but should mention something about expected input type
**Fails now because**:
- `get_unique_values()` does not exist

### Test 6: test_that("get_unique_values() handles data with NAs correctly", { ... })

**Reproduces**: edge case where forecast unit columns contain NA values
**Setup**:
- Take a copy of `example_quantile`, introduce some `NA` values in a forecast unit column (e.g., set a few `location` values to `NA`)
- Convert to forecast (this may trigger NA removal via `clean_forecast`, depending on implementation)
**Action**:
- Call `get_unique_values()` on the modified forecast
**Expected**:
- Function either counts NAs as a distinct value or handles them gracefully (no error)
- The count for the modified column should be consistent with how `clean_forecast()` / `na.omit()` handles the NAs (if the function uses `clean_forecast` like `get_forecast_counts()` does, NAs would be removed first)
- Use `expect_no_error()` at minimum
**Fails now because**:
- `get_unique_values()` does not exist
