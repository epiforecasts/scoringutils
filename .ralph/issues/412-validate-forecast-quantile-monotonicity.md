# Issue #412: Add check to `validate_forecast()` that predictions must not decrease with quantile

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/412
- **Action Summary**: Add monotonicity warning to `assert_forecast.forecast_quantile()` checking that predictions don't decrease with increasing quantile_level. Create a user-facing `get_non_monotonic_forecasts()` helper to identify affected rows (resolving both #412 and #895).
- **Status Note**: Monotonicity check only exists deep in `bias_quantile_single_vector()` (metrics-quantile.R:495) as a hard error. No early warning in `assert_forecast.forecast_quantile()`. No user-facing diagnostic function. Issue #412 is the maintainer-created parent with an agreed plan (steps 1 and 3 not done).

## Relationship to #895

Issue #895 is a near-duplicate of #412. Both require the same implementation:
1. A user-facing `get_non_monotonic_forecasts()` helper function
2. A warning in `assert_forecast.forecast_quantile()` for non-monotonic predictions
3. An internal `check_monotonicity()` helper (analogous to `check_duplicates()`)

**Comprehensive test specifications already exist in `.ralph/issues/895-quantile-prediction-order.md`** (12 test cases). The tests below focus specifically on #412's original ask: the three-part plan from the issue comments:
- (a) Write a helper function to check monotonicity
- (b) Enforce it in `bias_quantile()` as it's needed there (already done — hard error at `metrics-quantile.R:495`)
- (c) Check + warn in `validate_forecast()` (now `assert_forecast()`)

The tests below complement #895's specs by focusing on the `assert_forecast` integration and `bias_quantile` enforcement that #412 specifically calls out. Any test that duplicates #895 is noted.

## Test Specifications

**Designed**: 2026-02-13
**Test files**: `tests/testthat/test-class-forecast-quantile.R` (existing), `tests/testthat/test-metrics-quantile.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures; `example_quantile` used as baseline; `expect_no_condition()` for clean cases; `expect_error()`/`expect_warning()` with regex patterns for failures; `suppressMessages(suppressWarnings(...))` wrappers around `as_forecast_quantile()` calls; `expect_silent()` for functions that should produce no output; `setup.R` pre-computes `scores_quantile` etc.

---

### Test 1: test_that("assert_forecast.forecast_quantile() warns when predictions decrease with increasing quantile level", { ... })

**Reproduces**: the core gap — `assert_forecast.forecast_quantile()` (R/class-forecast-quantile.R:81-89) has no monotonicity check; non-monotonic data silently passes validation and only errors deep inside `bias_quantile_single_vector()` at scoring time
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Create a minimal data.table with non-monotonic predictions:
  ```
  data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)      # 7 > 4 violates monotonicity
  )
  ```
**Action**:
- Call `as_forecast_quantile(data)` which internally invokes `assert_forecast.forecast_quantile()`
**Expected**:
- `expect_warning(as_forecast_quantile(data), "non-monotonic|decreasing|Predictions must not be decreasing")` — a warning (NOT error) is emitted
- The function should still return a valid `forecast_quantile` object (non-monotonic data is suspicious but not fatal at the validation stage)
- The warning message should mention `get_non_monotonic_forecasts()` so users know how to diagnose further
**Fails now because**:
- `assert_forecast.forecast_quantile()` only checks: (1) `quantile_level` column exists, (2) generic forecast assertions, (3) forecast type matches, (4) quantile_level is numeric in [0,1]. No monotonicity check. Non-monotonic data passes silently.

*Note*: Overlaps with #895 Test 8. Kept here because this is the central ask of #412.

---

### Test 2: test_that("assert_forecast.forecast_quantile() does not warn for well-formed monotonic predictions", { ... })

**Reproduces**: regression guard — the new monotonicity check must not produce false positives on valid data
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `na.omit(data.table::copy(example_quantile))` — the built-in dataset with well-formed, monotonic predictions
**Action**:
- Call `as_forecast_quantile(data)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(data))` — no warnings, messages, or errors
**Fails now because**:
- This test should PASS both before and after the fix. It serves as a regression guard to confirm that well-formed data never triggers the new warning.

*Note*: Overlaps with #895 Test 9.

---

### Test 3: test_that("assert_forecast.forecast_quantile() respects verbose = FALSE for monotonicity warning", { ... })

**Reproduces**: the `verbose` parameter pattern — per the issue discussion, the warning should be suppressible
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Create non-monotonic data (same as Test 1)
- First convert to `forecast_quantile` while suppressing the expected warning
**Action**:
- Call `assert_forecast(forecast_obj, verbose = FALSE)` on the non-monotonic object
**Expected**:
- `expect_no_condition(assert_forecast(forecast_obj, verbose = FALSE))` — warning suppressed when `verbose = FALSE`
- This follows the existing pattern where `assert_forecast_generic()` guards its messages behind the `verbose` flag
**Fails now because**:
- No monotonicity check exists yet, so `verbose` has no effect on a non-existent check

*Note*: Overlaps with #895 Test 10.

---

### Test 4: test_that("bias_quantile() errors on predictions that decrease with increasing quantile level", { ... })

**Reproduces**: confirms that the existing hard error in `bias_quantile_single_vector()` (metrics-quantile.R:495) is preserved after the fix — the assert_forecast warning should NOT replace this error
**File**: `tests/testthat/test-metrics-quantile.R`
**Setup**:
- `predicted <- c(1, 2, 4, 3)` — prediction at q=0.9 (3) is less than q=0.5 (4)
- `quantiles <- c(0.1, 0.3, 0.5, 0.9)`
- `observed <- 3`
**Action**:
- Call `bias_quantile(observed = 3, predicted, quantiles)`
**Expected**:
- `expect_error(bias_quantile(observed = 3, predicted, quantiles), "Predictions must not be decreasing with increasing quantile level")` — hard error preserved
**Fails now because**:
- This test should PASS both before and after the fix. It already exists at `test-metrics-quantile.R:852-861`. Including it here to document that the #412 plan explicitly says "enforce it in `bias_quantile()` as it's needed there" — which is already done.

---

### Test 5: test_that("bias_quantile() succeeds when quantile levels are unordered but predictions are monotonic after sorting", { ... })

**Reproduces**: edge case — `bias_quantile_single_vector()` sorts by `quantile_level` before checking monotonicity; if predictions track quantile_level order correctly but are passed in scrambled order, the function should succeed
**File**: `tests/testthat/test-metrics-quantile.R`
**Setup**:
- `predicted <- c(5, 2, 8)` — out of order, but after sorting by quantile_level = c(0.5, 0.25, 0.75) they become c(2, 5, 8) which is monotonic
- `quantiles <- c(0.5, 0.25, 0.75)` — unsorted quantile levels
- `observed <- 4`
**Action**:
- Call `bias_quantile(observed = 4, predicted, quantiles)`
**Expected**:
- `expect_silent(bias_quantile(observed = 4, predicted, quantiles))` — no error, because after sorting by quantile_level the predictions are non-decreasing
- Returns a numeric bias value
**Fails now because**:
- This test should PASS both before and after the fix. It verifies that the existing sort-then-check logic in `bias_quantile_single_vector()` works correctly for scrambled input order.

---

### Test 6: test_that("score() with non-monotonic predictions warns at validation and handles bias metric gracefully", { ... })

**Reproduces**: the end-to-end user experience described in #412 — a user with non-monotonic predictions should get an early warning at `as_forecast_quantile()` time, and `score()` should handle the `bias_quantile` failure gracefully (via `run_safely()` converting the error to a warning)
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Create a data.table with 2 forecast units: one monotonic, one non-monotonic
  ```
  rbind(
    data.table(model = "good", date = as.Date("2020-01-01"), observed = 5,
               quantile_level = c(0.25, 0.5, 0.75), predicted = c(2, 5, 8)),
    data.table(model = "bad", date = as.Date("2020-01-01"), observed = 5,
               quantile_level = c(0.25, 0.5, 0.75), predicted = c(3, 7, 4))
  )
  ```
- Convert to `forecast_quantile` (expecting a warning about non-monotonic predictions)
**Action**:
- Call `score(forecast_obj)` with default metrics (which includes `bias_quantile`)
**Expected**:
- `score()` should NOT abort entirely — `run_safely()` converts the `bias_quantile` error to a per-metric warning
- The resulting scores object should contain WIS and other metrics for both models
- The `bias_quantile` metric should either be `NA` for the non-monotonic forecast unit or produce a per-metric warning
- The overall result should be a valid `scores` object
**Fails now because**:
- Currently, `as_forecast_quantile()` emits no warning about the non-monotonic data. The user only discovers the problem inside `score()` when `bias_quantile` errors per forecast unit, with warnings from `run_safely()`. After the fix, the user gets early feedback at `as_forecast_quantile()` time.

---

### Test 7: test_that("get_non_monotonic_forecasts() identifies the specific forecast units with monotonicity violations", { ... })

**Reproduces**: the helper function request from #412's discussion ("write a helper function to check that")
**File**: `tests/testthat/test-class-forecast-quantile.R` (or new `test-get-non-monotonic-forecasts.R`)
**Setup**:
- Create 3 forecast units:
  - model="A", date="2020-01-01": `predicted = c(2, 5, 8)` — monotonic (OK)
  - model="B", date="2020-01-01": `predicted = c(3, 7, 4)` — non-monotonic (BAD)
  - model="C", date="2020-01-01": `predicted = c(1, 1, 1)` — flat/constant (OK — non-decreasing)
- All with `quantile_level = c(0.25, 0.5, 0.75)`, `observed = 5`
- Convert to `forecast_quantile` (suppressing the warning)
**Action**:
- Call `get_non_monotonic_forecasts(data)`
**Expected**:
- Returns a `data.table` containing only the rows for model="B" (the non-monotonic forecast unit)
- `nrow(result) == 3` (all quantile rows for the affected unit, so the user can inspect the full picture)
- model="A" and model="C" are NOT in the result
- Model C with flat/constant predictions is correctly treated as non-decreasing (passes `diff(predicted) >= 0` check)
**Fails now because**:
- `get_non_monotonic_forecasts()` does not exist yet; call will error with "could not find function"

*Note*: Overlaps with #895 Tests 2-3, 12. Combined here into a single test covering the main case plus the flat-segment edge case.
