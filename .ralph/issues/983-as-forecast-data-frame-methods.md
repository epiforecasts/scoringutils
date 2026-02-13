# Issue #983: Create `.data.frame` methods for `as_forecast<type>`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/983
- **Action Summary**: Rename the 7 existing `.default` methods to `.data.frame` (since they assume data.frame-like input), then create new `.default` methods that error with a helpful message about unsupported types. Update NAMESPACE, docs, and tests.
- **Status Note**: Still relevant. Prerequisite #981 (making `as_forecast_*` S3 generics) is done. Currently all 7 types only have `.default` methods; no `.data.frame` methods exist. The `.default` methods silently accept any type and rely on `as.data.table()` coercion deep inside.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast.R` (existing) and individual `test-class-forecast-*.R` files (existing)
**Testing patterns observed**: testthat with data.table fixtures; `example_*` datasets from package; `suppressMessages()` wrappers around `as_forecast_*()` calls; `expect_error()`, `expect_no_condition()`, `expect_identical()` used extensively; `setup.R` pre-computes score objects.

### Important S3 dispatch note

`data.table` has class `c("data.table", "data.frame")` and tibble has class `c("tbl_df", "tbl", "data.frame")`. When `.default` is renamed to `.data.frame`, R's S3 dispatch will still route data.table and tibble inputs to the `.data.frame` method because both inherit from `data.frame`. The new `.default` will only fire for types that do NOT inherit from `data.frame`.

---

### Test 1: test_that("as_forecast_binary.data.frame works with a plain data.frame", { ... })

**Reproduces**: Confirms that the renamed `.data.frame` method works with plain data.frame input (same behavior as the old `.default` method).
**File**: `tests/testthat/test-class-forecast-binary.R`
**Setup**:
- Create a plain `data.frame` from `example_binary`: `df <- as.data.frame(na.omit(example_binary))`
**Action**:
- Call `as_forecast_binary(df)`
**Expected**:
- `expect_no_condition(as_forecast_binary(df))` — no errors or warnings
- `expect_s3_class(result, "forecast_binary")` — returns proper class
- The result should be scoreable: `expect_no_error(score(result))`
**Fails now because**:
- This test should PASS with current code (`.default` handles it) and continue to PASS after the fix. It's a regression guard.

### Test 2: test_that("as_forecast_quantile.data.frame works with a plain data.frame", { ... })

**Reproduces**: Confirms `.data.frame` method works for quantile forecasts with a data.frame (partially tested already at line 92-94 of test-class-forecast-quantile.R, but this is more thorough).
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `example_quantile_df` (already defined in `setup.R` as `as.data.frame(na.omit(example_quantile))`)
**Action**:
- Call `as_forecast_quantile(example_quantile_df)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(example_quantile_df))`
- `expect_s3_class(result, "forecast_quantile")`
**Fails now because**:
- This test should PASS both before and after. It's a regression guard confirming the existing data.frame test (line 93) keeps working.

### Test 3: test_that("as_forecast_sample.data.frame works with a plain data.frame", { ... })

**Reproduces**: Verifies sample forecast creation from a plain data.frame.
**File**: `tests/testthat/test-class-forecast-sample.R`
**Setup**:
- `df <- as.data.frame(na.omit(example_sample_continuous))`
**Action**:
- Call `as_forecast_sample(df)`
**Expected**:
- `expect_no_condition(as_forecast_sample(df))`
- `expect_s3_class(result, "forecast_sample")`
- `expect_no_error(score(result))`
**Fails now because**:
- Should PASS both before and after. Regression guard.

### Test 4: test_that("as_forecast_point.data.frame works with a plain data.frame", { ... })

**Reproduces**: Verifies point forecast creation from a plain data.frame.
**File**: `tests/testthat/test-class-forecast-point.R`
**Setup**:
- `df <- as.data.frame(na.omit(example_point))`
**Action**:
- Call `as_forecast_point(df)`
**Expected**:
- `expect_no_condition(as_forecast_point(df))`
- `expect_s3_class(result, "forecast_point")`
**Fails now because**:
- Should PASS both before and after. Regression guard.

### Test 5: test_that("as_forecast_nominal.data.frame works with a plain data.frame", { ... })

**Reproduces**: Verifies nominal forecast creation from a plain data.frame.
**File**: `tests/testthat/test-class-forecast-nominal.R`
**Setup**:
- `df <- as.data.frame(na.omit(example_nominal))`
**Action**:
- Call `as_forecast_nominal(df)`
**Expected**:
- `expect_no_condition(as_forecast_nominal(df))`
- `expect_s3_class(result, "forecast_nominal")`
**Fails now because**:
- Should PASS both before and after. Regression guard.

### Test 6: test_that("as_forecast_ordinal.data.frame works with a plain data.frame", { ... })

**Reproduces**: Verifies ordinal forecast creation from a plain data.frame.
**File**: `tests/testthat/test-class-forecast-ordinal.R`
**Setup**:
- `df <- as.data.frame(na.omit(example_ordinal))`
**Action**:
- Call `as_forecast_ordinal(df)`
**Expected**:
- `expect_no_condition(as_forecast_ordinal(df))`
- `expect_s3_class(result, "forecast_ordinal")`
**Fails now because**:
- Should PASS both before and after. Regression guard.

### Test 7: test_that("as_forecast_multivariate_sample.data.frame works with a plain data.frame", { ... })

**Reproduces**: Verifies multivariate sample forecast creation from a plain data.frame.
**File**: `tests/testthat/test-class-forecast-multivariate-sample.R`
**Setup**:
- Create a suitable data.frame from `example_sample_continuous` with a subset that has multiple locations:
  ```
  df <- as.data.frame(example_sample_continuous[
    target_type == "Cases" &
    forecast_date == "2021-05-03" &
    target_end_date == "2021-05-15" &
    horizon == 2 &
    model == "EuroCOVIDhub-ensemble"
  ])
  ```
**Action**:
- Call `as_forecast_multivariate_sample(df, c("location", "location_name"))`
**Expected**:
- `expect_no_condition(as_forecast_multivariate_sample(df, c("location", "location_name")))`
- `expect_s3_class(result, "forecast_sample_multivariate")`
**Fails now because**:
- Should PASS both before and after. Regression guard.

### Test 8: test_that("as_forecast_*.data.frame methods work with data.table input via S3 dispatch", { ... })

**Reproduces**: Confirms that data.table input still dispatches correctly to the `.data.frame` method after the rename (since data.table inherits from data.frame).
**File**: `tests/testthat/test-class-forecast.R`
**Setup**:
- Use the pre-existing `example_*` objects which are all data.tables
**Action**:
- Call each `as_forecast_*()` with its corresponding example dataset (which is a data.table)
**Expected**:
- All 7 calls succeed without error, produce the correct forecast class
- Specifically: `expect_s3_class(as_forecast_binary(example_binary), "forecast_binary")` and similarly for the other 6 types
**Fails now because**:
- Should PASS both before and after. Critical regression test ensuring data.table dispatch still works.

### Test 9: test_that("as_forecast_*.default errors with helpful message for non-data.frame input", { ... })

**Reproduces**: The core bug — currently `.default` silently accepts any type. After the fix, `.default` should error for non-data.frame types.
**File**: `tests/testthat/test-class-forecast.R`
**Setup**:
- Create several non-data.frame objects: `vec <- 1:10`, `mat <- matrix(1:12, nrow = 3)`, `lst <- list(a = 1, b = 2)`, `chr <- "not a data.frame"`
**Action**:
- Call `as_forecast_binary(vec)`, `as_forecast_binary(mat)`, `as_forecast_binary(lst)`, `as_forecast_binary(chr)`
- Also test at least one other type to confirm pattern: `as_forecast_sample(vec)`, `as_forecast_quantile(mat)`
**Expected**:
- Each call should produce an error with `expect_error()`
- Error message should mention that input must be a data.frame-like object: `expect_error(as_forecast_binary(vec), "data.frame")` (match on "data.frame" in the error message)
- Error message should indicate the type that was actually passed: the error should contain class info about the unsupported type
**Fails now because**:
- Currently `.default` accepts any input and tries `as.data.table()` coercion. For some inputs (like numeric vectors), this silently succeeds but produces nonsensical results. For others it produces a confusing error deep inside validation. After the fix, `.default` will immediately error with a clear message.

### Test 10: test_that("as_forecast_*.default errors for matrix input", { ... })

**Reproduces**: Edge case — matrix does NOT inherit from data.frame, so it should hit the new `.default` error.
**File**: `tests/testthat/test-class-forecast.R`
**Setup**:
- `mat <- matrix(1:20, nrow = 4, dimnames = list(NULL, c("observed", "predicted", "model", "date", "sample_id")))`
**Action**:
- Call `as_forecast_sample(mat)`
**Expected**:
- `expect_error(as_forecast_sample(mat), "data.frame")` — should error with clear message rather than silently coercing
**Fails now because**:
- Currently `as.data.table(mat)` succeeds and coerces the matrix, but the intent is to only accept data.frame-like objects.

### Test 11: test_that("as_forecast_*.data.frame methods preserve column renaming functionality", { ... })

**Reproduces**: Ensures the column renaming feature (e.g., `observed = "obs"`) works correctly after the method rename.
**File**: `tests/testthat/test-class-forecast.R`
**Setup**:
- Create a data.frame with non-standard column names:
  ```
  df <- data.frame(
    model = rep("m1", 10),
    date = as.Date("2020-01-01") + 1:10,
    obs = 1:10,
    pred = rnorm(10),
    sid = rep(1:2, each = 5)
  )
  ```
**Action**:
- Call `as_forecast_sample(df, observed = "obs", predicted = "pred", sample_id = "sid")`
**Expected**:
- `expect_no_error(result)` — method handles column renaming correctly
- `expect_true("observed" %in% colnames(result))` — column was renamed
- `expect_true("predicted" %in% colnames(result))` — column was renamed
- `expect_true("sample_id" %in% colnames(result))` — column was renamed
**Fails now because**:
- Should PASS both before and after. Guards that the renaming feature works after the method rename.

### Test 12: test_that("as_forecast_*.data.frame methods work with tibble input", { ... })

**Reproduces**: Confirms tibble input dispatches to `.data.frame` method correctly (tibble inherits from data.frame).
**File**: `tests/testthat/test-class-forecast.R`
**Setup**:
- Requires `tibble` package (check with `skip_if_not_installed("tibble")`)
- Create a tibble: `tbl <- tibble::as_tibble(na.omit(as.data.frame(example_binary)))`
**Action**:
- Call `as_forecast_binary(tbl)`
**Expected**:
- `expect_no_error(as_forecast_binary(tbl))`
- `expect_s3_class(result, "forecast_binary")`
**Fails now because**:
- Should PASS both before and after. But it's an important edge case because tibble is a common input type and has class `c("tbl_df", "tbl", "data.frame")`.
