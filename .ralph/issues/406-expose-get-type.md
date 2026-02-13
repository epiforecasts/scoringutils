# Issue #406: Maybe expose `get_type()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/406
- **Action Summary**: Rename `get_type()` to `get_vector_type()`, export it, and create `get_observed_type()` / `get_predicted_type()` wrappers that extract the relevant column from a forecast and call `get_vector_type()`. Decide whether to also export `get_forecast_type()` (tracked in #484/#885).
- **Status Note**: Still relevant. `get_type()` (vector type detection: classification/integer/continuous) and `get_forecast_type()` (forecast class type) both exist in `R/get-forecast-type.R` but are internal (`@keywords internal`), not exported. The proposed renames (`get_vector_type`, `get_observed_type`, `get_predicted_type`) were never implemented. No comments, no progress.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-get-forecast-type.R` (existing)
**Testing patterns observed**: testthat with `expect_identical`, `expect_error`; tests use package example datasets (`example_quantile`, `example_sample_continuous`, `example_sample_discrete`, `example_binary`, `example_point`, `example_nominal`); internal functions accessed by name (no `:::`); `setup.R` precomputes scores.

### Test 1: test_that("get_vector_type() works as expected with vectors", { ... })

**Reproduces**: verifies the renamed function exists and is exported
**Setup**:
- No special setup. Uses basic R vectors.
**Action**:
- Call `get_vector_type(1:3)`, `get_vector_type(factor(1:2))`, `get_vector_type(c(1.0, 2))`, `get_vector_type(c(1.0, 2.3))`
**Expected**:
- `expect_identical(get_vector_type(1:3), "integer")`
- `expect_identical(get_vector_type(factor(1:2)), "classification")`
- `expect_identical(get_vector_type(c(1.0, 2)), "integer")`
- `expect_identical(get_vector_type(c(1.0, 2.3)), "continuous")`
- `expect_error(get_vector_type(c("a", "b")))` (character input should error)
**Fails now because**:
- `get_vector_type()` does not exist. The function is currently named `get_type()` and is not exported.

### Test 2: test_that("get_vector_type() works as expected with matrices", { ... })

**Reproduces**: verifies the renamed function handles matrix inputs
**Setup**:
- No special setup. Uses basic R matrices.
**Action**:
- Call `get_vector_type()` with integer matrices, continuous matrices, factor matrices, character matrices
**Expected**:
- `expect_identical(get_vector_type(matrix(1:4, nrow = 2)), "integer")`
- `expect_identical(get_vector_type(matrix(c(1.0, 2:4))), "integer")`
- `expect_identical(get_vector_type(matrix(c(1.0, 2.3, 3, 4))), "continuous")`
- `expect_error(get_vector_type(matrix(factor(1:4), nrow = 2)))` (factor matrix → character coercion → error)
- `expect_error(get_vector_type(matrix(c("a", "b", "c", "d"))))` (character matrix → error)
**Fails now because**:
- `get_vector_type()` does not exist.

### Test 3: test_that("get_vector_type() handles NA values", { ... })

**Reproduces**: verifies NA handling in the renamed function
**Setup**:
- No special setup.
**Action**:
- Call `get_vector_type(c(1, NA, 3))`, `get_vector_type(c(1, NA, 3.2))`, `get_vector_type(NA)`
**Expected**:
- `expect_identical(get_vector_type(c(1, NA, 3)), "integer")`
- `expect_identical(get_vector_type(c(1, NA, 3.2)), "continuous")`
- `expect_error(get_vector_type(NA), "Can't get type")` (all-NA input should error)
**Fails now because**:
- `get_vector_type()` does not exist.

### Test 4: test_that("get_vector_type() is exported and accessible", { ... })

**Reproduces**: confirms the function is exported (user-facing)
**Setup**:
- No special setup.
**Action**:
- Call `scoringutils::get_vector_type(1:3)` using full namespace qualification
**Expected**:
- `expect_identical(scoringutils::get_vector_type(1:3), "integer")` — should succeed without `:::` (i.e., the function is exported)
**Fails now because**:
- `get_vector_type` does not exist in the package's exported namespace. `get_type` exists but is internal (`@keywords internal_input_check`).

### Test 5: test_that("get_observed_type() returns the type of the observed column", { ... })

**Reproduces**: verifies the new `get_observed_type()` wrapper works on forecast objects
**Setup**:
- Use `example_sample_continuous` (observed is continuous), `example_sample_discrete` (observed is integer), `example_binary` (observed is a factor)
**Action**:
- Call `get_observed_type(example_sample_continuous)`, `get_observed_type(example_sample_discrete)`, `get_observed_type(example_binary)`
**Expected**:
- `expect_identical(get_observed_type(example_sample_continuous), "continuous")`
- `expect_identical(get_observed_type(example_sample_discrete), "integer")`
- `expect_identical(get_observed_type(example_binary), "classification")`
**Fails now because**:
- `get_observed_type()` does not exist anywhere in the codebase.

### Test 6: test_that("get_predicted_type() returns the type of the predicted column", { ... })

**Reproduces**: verifies the new `get_predicted_type()` wrapper works on forecast objects
**Setup**:
- Use `example_sample_continuous` (predicted values are continuous), `example_sample_discrete` (predicted values are integer), `example_binary` (predicted values are continuous probabilities in [0,1])
**Action**:
- Call `get_predicted_type(example_sample_continuous)`, `get_predicted_type(example_sample_discrete)`, `get_predicted_type(example_binary)`
**Expected**:
- `expect_identical(get_predicted_type(example_sample_continuous), "continuous")`
- `expect_identical(get_predicted_type(example_sample_discrete), "integer")`
- `expect_identical(get_predicted_type(example_binary), "continuous")` (binary predictions are probabilities, which are continuous values)
**Fails now because**:
- `get_predicted_type()` does not exist anywhere in the codebase.

### Test 7: test_that("get_observed_type() and get_predicted_type() error on non-forecast objects", { ... })

**Reproduces**: verifies the wrappers reject invalid input
**Setup**:
- Create a plain data.frame with no forecast class: `df <- data.frame(x = 1:10, y = rnorm(10))`
**Action**:
- Call `get_observed_type(df)` and `get_predicted_type(df)`
**Expected**:
- Both should error, since the input is not a valid forecast object (no `observed` or `predicted` column in a properly classed forecast)
**Fails now because**:
- Functions do not exist.

### Test 8: test_that("get_observed_type() and get_predicted_type() are exported", { ... })

**Reproduces**: confirms both wrappers are user-accessible via `::` (not `:::`)
**Setup**:
- No special setup.
**Action**:
- Call `scoringutils::get_observed_type(example_sample_continuous)` and `scoringutils::get_predicted_type(example_sample_continuous)`
**Expected**:
- Both calls should succeed and return the expected type strings without requiring `:::`
**Fails now because**:
- Neither function exists in the package.

### Test 9: test_that("get_vector_type() is consistent with former get_type()", { ... })

**Reproduces**: regression test ensuring the rename preserves behavior
**Setup**:
- Create a list of diverse inputs: `1:2`, `c(1.0, 2)`, `c(1.0, 2.3)`, `matrix(1:4, nrow = 2)`, `matrix(c(1.0, 2:4))`, `matrix(c(1.0, 2.3, 3, 4))`, `factor(c("a", "b"))`
**Action**:
- For each input, call `get_vector_type(input)` and compare against expected known results (which match the old `get_type()` behavior)
**Expected**:
- Results match exactly: `"integer"`, `"integer"`, `"continuous"`, `"integer"`, `"integer"`, `"continuous"`, `"classification"` respectively
**Fails now because**:
- `get_vector_type()` does not exist.

### Test 10: test_that("get_predicted_type() works for quantile forecasts", { ... })

**Reproduces**: ensures the wrapper handles quantile forecast objects where `predicted` is a numeric column (not a matrix)
**Setup**:
- Use `example_quantile` (predicted is a numeric column of quantile values)
**Action**:
- Call `get_predicted_type(example_quantile)`
**Expected**:
- Returns `"continuous"` or `"integer"` depending on the nature of the predictions in the example dataset. For `example_quantile`, predictions are continuous real numbers, so `expect_identical(get_predicted_type(example_quantile), "continuous")`
**Fails now because**:
- `get_predicted_type()` does not exist.

### Notes for implementer

- The existing tests for `get_type()` in `test-get-forecast-type.R` (lines 35-106) should be **updated** to use the new name `get_vector_type()` rather than duplicated. The old `get_type()` name should either be removed or kept as a deprecated alias.
- For `get_predicted_type()` on sample-type forecasts: the `predicted` column contains individual sample values (not a matrix at the data.table level). The implementer should verify whether `get_predicted_type()` should call `get_vector_type()` on the `predicted` column directly.
- The `get_forecast_type()` export decision (mentioned in #484/#885) is a separate concern from this issue — tests here focus on the three new functions only.
