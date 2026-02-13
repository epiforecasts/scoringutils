# Issue #763: We should check for consistency when scoring binary forecasts

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/763
- **Action Summary**: Add a check in `assert_input_binary()` that warns when observed values are "0"/"1" and level order is `c(1, 0)`. Optionally create `as_binary_observed()` helper with a `reference_level` argument for safe factor conversion.
- **Status Note**: Still relevant. No code warns when 0/1 factor levels are in counterintuitive order (e.g., `levels = c(1, 0)` making `0` the reference level). `assert_input_binary()` only validates factor with 2 levels and predicted in [0,1]. No helper for safe factor conversion exists. @seabbs questioned whether this is must-have or nice-to-have.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-binary.R` (existing) and `tests/testthat/test-metrics-binary.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures; `example_binary` dataset used throughout; `scores_binary` pre-computed in `setup.R`; `suppressMessages()` used around `as_forecast_binary()` calls; `cli_abort()`/`cli_warn()` condition checking via `expect_error()`/`expect_warning()`

### Test 1: test_that("assert_input_binary() warns when 0/1 factor levels are in counterintuitive order", { ... })

**Reproduces**: the core issue — no warning when factor levels `c(1, 0)` silently reverse the interpretation of predictions
**Setup**:
- `observed <- factor(c(0, 1, 1, 0, 1), levels = c("1", "0"))` — levels are reversed from the natural order
- `predicted <- c(0.1, 0.9, 0.8, 0.2, 0.7)` — probabilities intended for outcome "1"
**Action**:
- Call `assert_input_binary(observed, predicted)`
**Expected**:
- Should emit a `cli_warn()` indicating that the factor levels appear to be in counterintuitive order — specifically that the levels are `c("1", "0")` and predictions will be interpreted as the probability of observing `"0"` (the highest/last level), which is likely unintended
**Fails now because**:
- `assert_input_binary()` only checks `assert_factor(observed, n.levels = 2)`, `assert_numeric(predicted, lower = 0, upper = 1)`, and `assert_dims_ok_point()`. It performs no check on factor level ordering. The function returns `invisible(NULL)` without any warning.

### Test 2: test_that("assert_input_binary() does not warn for standard 0/1 level order", { ... })

**Reproduces**: ensures the warning is not a false positive for the normal/expected case
**Setup**:
- `observed <- factor(c(0, 1, 1, 0, 1), levels = c("0", "1"))` — standard level order where "1" is the highest level
- `predicted <- c(0.1, 0.9, 0.8, 0.2, 0.7)`
**Action**:
- Call `assert_input_binary(observed, predicted)`
**Expected**:
- Should return `invisible(NULL)` with no warning emitted
- Use `expect_no_warning(assert_input_binary(observed, predicted))`
**Fails now because**:
- This test should PASS against current code (no warning is emitted). It serves as a regression guard to ensure the new warning logic does not trigger for the standard case.

### Test 3: test_that("assert_input_binary() does not warn for non-numeric factor levels", { ... })

**Reproduces**: ensures the check only triggers for clearly 0/1-like level names, not arbitrary text labels
**Setup**:
- `observed <- factor(c("a", "b", "b", "a"), levels = c("a", "b"))` — text levels, no "natural" order to warn about
- `predicted <- c(0.3, 0.7, 0.6, 0.4)`
**Action**:
- Call `assert_input_binary(observed, predicted)`
**Expected**:
- Should return `invisible(NULL)` with no warning emitted
- Use `expect_no_warning(assert_input_binary(observed, predicted))`
**Fails now because**:
- This test should PASS against current code. It is a regression guard ensuring the warning only fires for 0/1-like levels where order is counterintuitive, not for arbitrary label names.

### Test 4: test_that("brier_score() produces different results with reversed factor levels", { ... })

**Reproduces**: demonstrates the silent incorrect scoring that the issue describes
**Setup**:
- `observed_correct <- factor(c(0, 1, 1, 0), levels = c("0", "1"))` — correct level order
- `observed_reversed <- factor(c(0, 1, 1, 0), levels = c("1", "0"))` — reversed level order
- `predicted <- c(0.1, 0.9, 0.8, 0.2)` — predictions intended as P(outcome = 1)
**Action**:
- `scores_correct <- brier_score(observed_correct, predicted)`
- `scores_reversed <- brier_score(observed_reversed, predicted)` — should now emit a warning
**Expected**:
- `scores_correct` should be `c(0.01, 0.01, 0.04, 0.04)` (small errors for good predictions)
- `scores_reversed` should NOT equal `scores_correct` (demonstrating the reversal causes different scores)
- After the fix, calling `brier_score()` with reversed levels should emit a warning (because `assert_input_binary()` is called inside `brier_score()`)
- Use `expect_warning(brier_score(observed_reversed, predicted), <pattern matching the level-order warning>)`
**Fails now because**:
- Currently `brier_score()` calls `assert_input_binary()` which does not warn about level order. The reversed-level call silently succeeds with `scores_reversed = c(0.81, 0.81, 0.64, 0.64)` — completely wrong — with no indication to the user.

### Test 5: test_that("logs_binary() warns with reversed 0/1 factor levels", { ... })

**Reproduces**: same issue as Test 4 but for the other binary scoring function
**Setup**:
- `observed_reversed <- factor(c(0, 1, 1, 0), levels = c("1", "0"))` — reversed level order
- `predicted <- c(0.1, 0.9, 0.8, 0.2)`
**Action**:
- Call `logs_binary(observed_reversed, predicted)`
**Expected**:
- Should emit the same level-order warning as `brier_score()`, since both call `assert_input_binary()`
- Use `expect_warning(logs_binary(observed_reversed, predicted), <pattern matching the level-order warning>)`
**Fails now because**:
- `logs_binary()` calls `assert_input_binary()` which does not warn about level order. The call silently succeeds with incorrect scores.

### Test 6: test_that("as_forecast_binary() warns when data has reversed 0/1 factor levels", { ... })

**Reproduces**: the issue at the forecast-object creation level, not just at scoring time
**Setup**:
- Create a small data.table with binary forecast structure:
  ```
  dt <- data.table(
    model = "m1",
    observed = factor(c(0, 1, 1, 0), levels = c("1", "0")),
    predicted = c(0.1, 0.9, 0.8, 0.2)
  )
  ```
**Action**:
- Call `as_forecast_binary(dt)`
**Expected**:
- Should emit a warning about the counterintuitive factor level order during the `assert_forecast()` step (which calls `check_input_binary()` → `assert_input_binary()`)
- The forecast object should still be created (it's a warning, not an error)
- Use `expect_warning(as_forecast_binary(dt), <pattern matching the level-order warning>)`
**Fails now because**:
- `assert_forecast.forecast_binary()` calls `check_input_binary()` which delegates to `assert_input_binary()`. None of these check factor level order, so no warning is emitted. The forecast object is created silently with the reversed levels.

### Test 7: test_that("assert_input_binary() warns for TRUE/FALSE levels in counterintuitive order", { ... })

**Reproduces**: a variant of the issue for boolean-like factor levels
**Setup**:
- `observed <- factor(c(TRUE, FALSE, TRUE), levels = c("TRUE", "FALSE"))` — "TRUE" is the first (lower) level, "FALSE" is the second (higher/reference) level. Predictions would be interpreted as P(outcome = FALSE), which is counterintuitive.
- `predicted <- c(0.8, 0.2, 0.9)`
**Action**:
- Call `assert_input_binary(observed, predicted)`
**Expected**:
- Should emit a warning that the factor level order may be counterintuitive. With `levels = c("TRUE", "FALSE")`, the highest level is `"FALSE"`, so `predicted` is interpreted as P(outcome = FALSE). Users almost certainly intend `predicted` to represent P(outcome = TRUE).
- Use `expect_warning(assert_input_binary(observed, predicted), <pattern matching the level-order warning>)`
**Fails now because**:
- `assert_input_binary()` does not inspect level names or their ordering semantics. No warning is emitted regardless of how misleading the level order is.

### Test 8: test_that("score() produces correct results with standard 0/1 factor levels", { ... })

**Reproduces**: regression guard ensuring the fix doesn't break normal scoring
**Setup**:
- Use `example_binary` dataset (which has standard `levels = c("0", "1")`)
**Action**:
- Call `score(example_binary)`
**Expected**:
- Should produce the same results as `scores_binary` (pre-computed in `setup.R`)
- No warnings should be emitted related to factor level order
- Use `expect_no_warning(score(example_binary))` (or `suppressMessages()` to ignore non-level-related messages, then check no level-order warning)
**Fails now because**:
- This test should PASS against current code. It is a regression guard ensuring that the new warning logic does not affect the standard `example_binary` dataset which has correct level ordering.
