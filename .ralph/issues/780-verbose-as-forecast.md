# Issue #780: Add a `verbose` argument to `as_forecast()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/780
- **Action Summary**: Add `verbose = TRUE` parameter to each `as_forecast_<type>.default()` method, pass it to their `assert_forecast()` calls, guard direct `cli_warn()`/`cli_inform()` calls behind the flag, and replace `suppressMessages`/`suppressWarnings` wrappers in tests and internal code with `verbose = FALSE`.
- **Status Note**: Still relevant. `verbose` already exists on `assert_forecast()` and `assert_forecast_generic()` (PR #778) but has not been threaded through to any `as_forecast_<type>()` constructor. ~11 test lines and 1 internal R source line use `suppressMessages`/`suppressWarnings` wrappers around `as_forecast_*` calls, confirming ongoing need. Author acknowledged low urgency but wants it for test cleanup.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast.R` (existing) and `tests/testthat/test-class-forecast-quantile.R` (existing)
**Testing patterns observed**: testthat 3e with `expect_no_condition()`, `expect_warning()`, `expect_s3_class()`. data.table fixtures via built-in example datasets (`example_quantile`, `example_binary`, `example_sample_continuous`, `example_point`, `example_nominal`). `setup.R` precomputes some objects. `suppressMessages()`/`suppressWarnings()` used extensively where `verbose` param is missing.

### Test 1: test_that("as_forecast_quantile() accepts verbose argument and suppresses messages", { ... })

**Reproduces**: the core issue — `as_forecast_quantile()` has no `verbose` parameter, forcing users to use `suppressMessages()`
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `example_quantile` (contains NAs, so `assert_forecast_generic` will emit an `cli_inform` about NA removal)
**Action**:
- Call `as_forecast_quantile(example_quantile, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(example_quantile, verbose = FALSE))` — no messages or warnings emitted
- The returned object should still be a valid `forecast_quantile` (i.e., `expect_s3_class(result, "forecast_quantile")`)
**Fails now because**:
- `as_forecast_quantile.default()` does not have a `verbose` parameter in its signature, so passing `verbose = FALSE` either causes an error (unused argument) or is silently ignored via `...` without being passed to `assert_forecast()`

### Test 2: test_that("as_forecast_quantile() with verbose = TRUE emits messages by default", { ... })

**Reproduces**: confirms the default behavior is preserved
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `example_quantile` (contains NAs)
**Action**:
- Call `as_forecast_quantile(example_quantile)` (default `verbose = TRUE`)
**Expected**:
- `expect_message(as_forecast_quantile(example_quantile), "Some rows containing NA")` — the NA info message should still be emitted by default
**Fails now because**:
- This test would currently pass (messages ARE emitted by default). Included as a regression test to ensure the default behavior is not broken by the fix.

### Test 3: test_that("as_forecast_quantile() verbose = FALSE suppresses quantile rounding warning", { ... })

**Reproduces**: the quantile-specific `cli_warn` inside `as_forecast_quantile.default()` (lines 61-68) that is not gated by any verbose flag
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Create a data.table based on `example_quantile` but modify quantile levels to have a rounding issue (e.g., add a tiny epsilon like `1e-12` to some `quantile_level` values so that `diff(sort(unique(quantile_level)))` has entries `<= 1e-10`)
- Ensure no NAs so only the rounding warning is at issue
**Action**:
- Call `as_forecast_quantile(data, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(data, verbose = FALSE))` — the rounding warning should be suppressed
**Fails now because**:
- The `cli_warn()` at lines 61-68 of `R/class-forecast-quantile.R` is unconditional (not gated by `verbose`) and the function doesn't even accept `verbose`

### Test 4: test_that("as_forecast_sample() accepts verbose argument and suppresses messages", { ... })

**Reproduces**: same issue for sample forecasts
**File**: `tests/testthat/test-class-forecast-sample.R` (existing) or `tests/testthat/test-class-forecast.R`
**Setup**:
- Use `example_sample_continuous` or create sample data that contains NAs (to trigger the `cli_inform` message about NA removal)
**Action**:
- Call `as_forecast_sample(data, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_sample(data, verbose = FALSE))` — no messages or warnings
- Result should be a valid `forecast_sample`
**Fails now because**:
- `as_forecast_sample.default()` does not accept `verbose` and does not pass it to `assert_forecast()`

### Test 5: test_that("as_forecast_binary() accepts verbose argument and suppresses messages", { ... })

**Reproduces**: same issue for binary forecasts
**File**: `tests/testthat/test-class-forecast-binary.R` (existing) or `tests/testthat/test-class-forecast.R`
**Setup**:
- Use `example_binary` or modify it to contain NAs (to trigger the NA info message)
**Action**:
- Call `as_forecast_binary(data, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_binary(data, verbose = FALSE))` — no messages or warnings
- Result should be a valid `forecast_binary`
**Fails now because**:
- `as_forecast_binary.default()` does not accept `verbose` and does not pass it to `assert_forecast()`

### Test 6: test_that("as_forecast_point() accepts verbose argument and suppresses messages", { ... })

**Reproduces**: same issue for point forecasts
**File**: `tests/testthat/test-class-forecast-point.R` (existing) or `tests/testthat/test-class-forecast.R`
**Setup**:
- Use `example_point` or modify it to contain NAs
**Action**:
- Call `as_forecast_point(data, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_point(data, verbose = FALSE))` — no messages or warnings
- Result should be a valid `forecast_point`
**Fails now because**:
- `as_forecast_point.default()` does not accept `verbose` and does not pass it to `assert_forecast()`

### Test 7: test_that("as_forecast_nominal() accepts verbose argument", { ... })

**Reproduces**: same issue for nominal forecasts
**File**: `tests/testthat/test-class-forecast-nominal.R` (existing) or `tests/testthat/test-class-forecast.R`
**Setup**:
- Use `example_nominal`
**Action**:
- Call `as_forecast_nominal(example_nominal, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_nominal(example_nominal, verbose = FALSE))` — no messages or warnings
- Result should be a valid `forecast_nominal`
**Fails now because**:
- `as_forecast_nominal.default()` does not accept `verbose` and does not pass it to `assert_forecast()`

### Test 8: test_that("as_forecast_*() verbose argument does not affect error behavior", { ... })

**Reproduces**: edge case — `verbose = FALSE` should only suppress messages/warnings, NOT errors
**File**: `tests/testthat/test-class-forecast.R` (existing)
**Setup**:
- Create invalid data (e.g., a data.table missing the `predicted` column)
**Action**:
- Call `as_forecast_quantile(invalid_data, verbose = FALSE)`
**Expected**:
- `expect_error(as_forecast_quantile(invalid_data, verbose = FALSE), "Column 'predicted' not found")` — errors should still be thrown even with `verbose = FALSE`
**Fails now because**:
- Cannot currently pass `verbose = FALSE` to `as_forecast_quantile()`. After the fix, this test ensures errors are not swallowed.

### Test 9: test_that("as_forecast_quantile() verbose = FALSE suppresses different-number-of-quantiles warning", { ... })

**Reproduces**: the warning from `assert_forecast_generic()` line 132-134 (different numbers of quantiles/samples per forecast) is suppressed when `verbose = FALSE`
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing)
**Setup**:
- Use `na.omit(as.data.table(example_quantile)[-1000, ])` — removing one row causes unequal quantile counts across forecasts, triggering the "different numbers of rows" warning
**Action**:
- Call `as_forecast_quantile(data, verbose = FALSE)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(data, verbose = FALSE))` — the "Some forecasts have different numbers of rows" warning should be suppressed
**Fails now because**:
- `as_forecast_quantile.default()` does not accept `verbose` and does not pass it to `assert_forecast()`, so the warning always fires

## Implementation Notes

The fix requires changes in 7 files (one per `as_forecast_<type>.default` method):
1. Add `verbose = TRUE` to each `.default` method's signature
2. Pass `verbose` to the `assert_forecast()` call at the end of each method
3. In `as_forecast_quantile.default()` specifically, gate the `cli_warn()` about quantile level rounding (lines 61-68) behind `if (verbose)`
4. Optionally update tests that currently use `suppressMessages()`/`suppressWarnings()` around `as_forecast_*()` calls to use `verbose = FALSE` instead
