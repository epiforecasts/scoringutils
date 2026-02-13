# Issue #360: Create a wrapper around log score to warn about its use for integer-valued forecasts

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/360
- **Action Summary**: Add `get_type(predicted)` check to `logs_sample()` that emits `cli_warn()` when predictions are integer-valued. Optionally move `log_score` out of the default metric list in `get_metrics.forecast_sample()`. Add targeted tests.
- **Status Note**: Still relevant and unaddressed. `logs_sample()` is a thin wrapper with no integer detection or warning. `get_type()` already exists and is used by `bias_sample()` and `pit_histogram_sample()` for the same purpose. Consensus reached on MVP: add a warning when `get_type(predicted) == "integer"`.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-metrics-sample.R` (existing)
**Testing patterns observed**: testthat with `expect_warning()`, `expect_no_condition()`, `expect_length()`, `expect_type()`. Test data typically created inline with `rpois()` (integer) and `rnorm()` (continuous). `get_type()` is the internal helper that detects integer vs continuous predictions. No helper files; setup.R pre-computes `scores_sample_continuous` and `scores_sample_discrete` using `example_sample_continuous` and `example_sample_discrete`.

### Test 1: test_that("logs_sample() warns when predictions are integer-valued", { ... })

**Reproduces**: the core issue — no warning is emitted when users pass integer-valued predictions to `logs_sample()`, even though the log score uses kernel density estimation which is inappropriate for discrete data.
**Setup**:
- `observed <- rpois(30, lambda = 1:30)` (integer observed values)
- `predicted <- replicate(200, rpois(n = 30, lambda = 1:30))` (integer predictions, matrix form)
**Action**:
- Call `logs_sample(observed, predicted)`
**Expected**:
- `expect_warning(logs_sample(observed, predicted), <regexp matching the integer warning message>)` — the function should emit a `cli_warn()` indicating that predictions appear to be integer-valued and that the log score may not be appropriate for discrete forecasts.
- The function should still return a numeric vector of length 30 (warning, not error).
**Fails now because**:
- `logs_sample()` (R/metrics-sample.R:224-231) has no `get_type()` check; it passes directly through to `scoringRules::logs_sample()` without any integer detection, so no warning is emitted.

### Test 2: test_that("logs_sample() does not warn for continuous predictions", { ... })

**Reproduces**: regression guard — ensures the warning is not incorrectly triggered for continuous predictions.
**Setup**:
- `observed <- rnorm(30, mean = 1:30)` (continuous observed values)
- `predicted <- replicate(200, rnorm(30, mean = 1:30))` (continuous predictions, matrix form)
**Action**:
- Call `logs_sample(observed, predicted)`
**Expected**:
- `expect_no_condition(logs_sample(observed, predicted))` — no warning or message should be emitted.
- Result should be a numeric vector of length 30.
**Fails now because**:
- This test should PASS against current code (no warning emitted). It serves as a regression guard for the fix.

### Test 3: test_that("logs_sample() warns for integer-valued predictions even when stored as doubles", { ... })

**Reproduces**: edge case where predictions are integer-valued but stored as `double` type (e.g., `c(1.0, 2.0, 3.0)` rather than `c(1L, 2L, 3L)`). `get_type()` handles this case — it returns `"integer"` when `all.equal(x, as.integer(x))` is `TRUE`.
**Setup**:
- `observed <- as.numeric(rpois(30, lambda = 1:30))` (double-stored integer values)
- `predicted <- matrix(as.numeric(replicate(200, rpois(n = 30, lambda = 1:30))), nrow = 30)` (double-stored integer predictions)
**Action**:
- Call `logs_sample(observed, predicted)`
**Expected**:
- `expect_warning(logs_sample(observed, predicted), <regexp matching the integer warning message>)` — should detect that predictions are integer-valued even though they're stored as doubles.
**Fails now because**:
- No `get_type()` check exists in `logs_sample()`, so no warning is emitted regardless of prediction type.

### Test 4: test_that("logs_sample() returns valid scores alongside the integer warning", { ... })

**Reproduces**: ensures that the warning does not prevent computation — the function should still return valid scores after warning.
**Setup**:
- `observed <- rpois(30, lambda = 1:30)` (integer observed values)
- `predicted <- replicate(200, rpois(n = 30, lambda = 1:30))` (integer predictions)
**Action**:
- Call `result <- suppressWarnings(logs_sample(observed, predicted))`
**Expected**:
- `expect_type(result, "double")` — result is numeric
- `expect_length(result, 30)` — one score per observation
- `expect_true(all(is.finite(result)))` — no NA/NaN/Inf values (with 200 samples, kernel density estimation should still produce finite scores)
**Fails now because**:
- The function already returns valid scores for integer predictions, but this test codifies that the new warning should not break the return value. This test PASSES against current code (the `suppressWarnings` makes it compatible with the fix), so it serves as a regression guard.

### Test 5: test_that("score() emits a warning when scoring integer samples with log_score", { ... })

**Reproduces**: end-to-end test through the `score()` pipeline with discrete sample data, confirming the warning surfaces through `apply_metrics()` / `run_safely()`.
**Setup**:
- Use `example_sample_discrete` (the built-in integer-valued sample forecast dataset)
- Use default metrics (which include `log_score = logs_sample`)
**Action**:
- Call `score(example_sample_discrete)`
**Expected**:
- `expect_warning(score(example_sample_discrete), <regexp matching the integer warning message>)` — the warning should propagate through the scoring pipeline.
- Alternatively, if `run_safely()` catches and re-wraps the warning, the message may be transformed — the test should match whatever form it takes after passing through `run_safely()`.
**Fails now because**:
- `logs_sample()` produces no warning for integer predictions, so `score()` completes silently for discrete sample data.

### Test 6: test_that("score() does not warn about log_score for continuous samples", { ... })

**Reproduces**: regression guard for the end-to-end pipeline with continuous data.
**Setup**:
- Use `example_sample_continuous` (the built-in continuous sample forecast dataset)
- Use default metrics (which include `log_score = logs_sample`)
**Action**:
- Call `score(example_sample_continuous)`
**Expected**:
- `expect_no_warning(score(example_sample_continuous))` — no integer-related warning should be emitted.
**Fails now because**:
- This test should PASS against current code. It is a regression guard.
