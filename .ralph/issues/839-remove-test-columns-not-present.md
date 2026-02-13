# Issue #839: Remove `test_columns_not_present`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/839
- **Action Summary**: Replace the single call in `R/class-forecast-binary.R:68` with `test_names(colnames(forecast), disjunct.from = ...)`, delete the function definition in `R/check-input-helpers.R`, delete the test block, and remove the generated man page. Optionally bundle with #922.
- **Status Note**: Still relevant. `test_columns_not_present()` is a trivial 1-line wrapper around `checkmate::test_names(..., disjunct.from = ...)` with exactly 1 production call site (`R/class-forecast-binary.R:68`) and 1 test block. `checkmate::test_names` is already imported. Straightforward removal.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-binary.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures, pre-computed scores in `setup.R`, built-in example datasets (`example_binary`, `example_quantile`, etc.), `expect_error()` with partial message matching, `suppressMessages()` around `as_forecast_*()` calls.

### Test 1: test_that("assert_forecast.forecast_binary() rejects data with sample_id column", { ... })

**Reproduces**: Verifies that the replacement of `test_columns_not_present()` with inline `checkmate::test_names()` correctly detects the presence of a `sample_id` column and errors.
**Setup**:
- Start with `na.omit(as.data.table(example_binary))`
- Add a column: `test[, "sample_id" := seq_len(nrow(test))]`
**Action**:
- Call `as_forecast_binary(test)`
**Expected**:
- `expect_error()` matching `"Input looks like a binary forecast, but an additional column"`
**Fails now because**:
- This test already exists in the current test file (line 28-37). After the fix, it must continue to pass with the same error message. The purpose here is to confirm that the inline `test_names()` replacement preserves behavior. The existing test covers this scenario; this spec confirms it must remain and pass.

### Test 2: test_that("assert_forecast.forecast_binary() rejects data with quantile_level column", { ... })

**Reproduces**: Edge case — the current check tests for both `sample_id` and `quantile_level`, but the existing test only adds `sample_id`. This test ensures the `quantile_level` branch is also covered after removal of the wrapper function.
**Setup**:
- Start with `na.omit(as.data.table(example_binary))`
- Add a column: `test[, "quantile_level" := 0.5]`
**Action**:
- Call `as_forecast_binary(test)`
**Expected**:
- `expect_error()` matching `"Input looks like a binary forecast, but an additional column"`
**Fails now because**:
- This test does NOT currently exist. The existing test only checks `sample_id`, not `quantile_level`. After the fix replaces `test_columns_not_present()` with `test_names(colnames(forecast), disjunct.from = c("sample_id", "quantile_level"))`, both column names must trigger the error. This test ensures the `quantile_level` path is exercised.

### Test 3: test_that("assert_forecast.forecast_binary() accepts valid binary data without sample_id or quantile_level", { ... })

**Reproduces**: Regression guard — confirms that valid binary data (which has neither `sample_id` nor `quantile_level` columns) continues to pass validation after the refactor.
**Setup**:
- Use `example_binary` directly (already a valid `forecast_binary` object)
**Action**:
- Call `assert_forecast(example_binary)` (or equivalently, `as_forecast_binary(as.data.table(example_binary))`)
**Expected**:
- `expect_no_error()` — validation passes without error
**Fails now because**:
- This test partly exists via the `as_forecast_binary()` output test (line 4-8). This spec makes the assertion explicit for `assert_forecast()` itself to guard against a faulty replacement that incorrectly flags valid data.

### Test 4: test_that("test_columns_not_present() is no longer exported or defined", { ... })

**Reproduces**: Confirms the function was actually deleted, not just unused.
**Setup**:
- None
**Action**:
- Check that `test_columns_not_present` is not accessible as a function in the package namespace
**Expected**:
- `expect_false(exists("test_columns_not_present", where = asNamespace("scoringutils"), mode = "function"))` — the function no longer exists in the scoringutils namespace
**Fails now because**:
- The function currently exists in the namespace. After the fix deletes it, this test confirms removal is complete. This prevents accidental re-introduction.

## Implementation Notes

The fix involves 4 file changes:

1. **`R/class-forecast-binary.R` line 68-70**: Replace `test_columns_not_present(forecast, c("sample_id", "quantile_level"))` with `test_names(colnames(forecast), disjunct.from = c("sample_id", "quantile_level"))`. The `@importFrom checkmate test_names` annotation should be added to `assert_forecast.forecast_binary` (or kept from the deleted function's location).

2. **`R/check-input-helpers.R` lines 111-121**: Delete the `test_columns_not_present()` function definition and its roxygen block.

3. **`tests/testthat/test-check-input-helpers.R` lines 39-46**: Delete the `test_columns_not_present` test block.

4. **`man/test_columns_not_present.Rd`**: Delete this generated man page file.

5. **NAMESPACE**: Remove the `test_columns_not_present` entry (if exported). Ensure `importFrom(checkmate,test_names)` remains (it currently exists at line 140).
