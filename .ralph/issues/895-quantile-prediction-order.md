# Issue #895: Check quantile prediction order helper function

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/895
- **Related**: https://github.com/epiforecasts/scoringutils/issues/412
- **Action Summary**: Create a user-facing helper (e.g., `get_non_monotonic_forecasts()`) to identify rows where predictions decrease with increasing quantile level, and add a warning in `assert_forecast.forecast_quantile()`. Resolves both #895 and #412.
- **Status Note**: Monotonicity check only exists deep in `bias_quantile_single_vector()` (metrics-quantile.R:495) as a hard error. No early warning in `assert_forecast.forecast_quantile()`. No user-facing diagnostic function. Issue #412 is the maintainer-created parent with an agreed plan (steps 1 and 3 not done).

## Test Specifications

**Designed**: 2026-02-13
**Test files**: `tests/testthat/test-class-forecast-quantile.R` (existing) and `tests/testthat/test-get-duplicate-forecasts.R` (existing, to add sibling tests for `get_non_monotonic_forecasts()`)
**Testing patterns observed**: testthat with data.table fixtures; `example_quantile` dataset used extensively; `expect_no_condition()` for passing cases, `expect_error()`/`expect_warning()` for failing cases; `suppressMessages(suppressWarnings(...))` around `as_forecast_quantile()` calls; setup.R pre-computes `scores_quantile` and `metrics_no_cov`; no helper-*.R files exist

## Implementation Context

The fix has three parts:
1. **New function `get_non_monotonic_forecasts()`** — user-facing diagnostic, modeled on `get_duplicate_forecasts()` (in `R/get-duplicate-forecasts.R`). Takes a forecast object (or data with `quantile_level` and `predicted` columns), groups by forecast unit, orders by `quantile_level` within each group, and returns rows where `predicted` decreases as `quantile_level` increases.
2. **Warning in `assert_forecast.forecast_quantile()`** — after the existing 4 checks (line 84-87 of `R/class-forecast-quantile.R`), add a call that detects non-monotonic forecasts and emits `cli_warn()` (not `cli_abort()`), guarded by `verbose`.
3. **Internal helper `check_monotonicity()`** — analogous to `check_duplicates()`, calls `get_non_monotonic_forecasts()` and returns `TRUE` or an error message string. Used by `assert_forecast.forecast_quantile()`.

---

### Test 1: test_that("get_non_monotonic_forecasts() returns empty data.table for well-formed quantile forecasts", { ... })

**Reproduces**: baseline correctness — well-formed data should produce no results
**File**: `tests/testthat/test-class-forecast-quantile.R` (or a new `test-get-non-monotonic-forecasts.R` following the `test-get-duplicate-forecasts.R` naming pattern)
**Setup**:
- Use `example_quantile` directly (built-in dataset with well-formed quantile forecasts)
**Action**:
- Call `get_non_monotonic_forecasts(example_quantile)`
**Expected**:
- `expect_identical(nrow(get_non_monotonic_forecasts(example_quantile)), 0L)` — no rows returned
- Result should be a `data.table`
**Fails now because**:
- Function `get_non_monotonic_forecasts()` does not exist yet; call will error with "could not find function"

---

### Test 2: test_that("get_non_monotonic_forecasts() detects predictions that decrease with increasing quantile level", { ... })

**Reproduces**: the core bug — non-monotonic predictions are not flagged until deep inside `bias_quantile()`
**File**: same as Test 1
**Setup**:
- Create a minimal quantile forecast data.table with one forecast unit having non-monotonic predictions:
  ```
  data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)    # 7 > 4 violates monotonicity at q=0.75
  )
  ```
- Convert to `forecast_quantile` using `suppressWarnings(suppressMessages(as_forecast_quantile(...)))`
**Action**:
- Call `get_non_monotonic_forecasts(data)` on the constructed data
**Expected**:
- Result has `nrow > 0` — the non-monotonic forecast unit is returned
- The returned rows should contain the forecast unit where the violation occurs (model="model1", date="2020-01-01")
- All quantile rows for that forecast unit should be included (so the user can see the full picture), i.e., `nrow(result) == 3`
**Fails now because**:
- Function does not exist yet

---

### Test 3: test_that("get_non_monotonic_forecasts() handles multiple forecast units with mixed monotonicity", { ... })

**Reproduces**: edge case — only some forecast units are non-monotonic; function should return only the bad ones
**File**: same as Test 1
**Setup**:
- Create a data.table with two forecast units:
  - Unit A (model="good_model", date="2020-01-01"): `quantile_level = c(0.25, 0.5, 0.75)`, `predicted = c(2, 5, 8)` — monotonic, well-formed
  - Unit B (model="bad_model", date="2020-01-01"): `quantile_level = c(0.25, 0.5, 0.75)`, `predicted = c(3, 7, 4)` — non-monotonic
  - Both have `observed = 5`
- Convert to `forecast_quantile`
**Action**:
- Call `get_non_monotonic_forecasts(data)`
**Expected**:
- Only rows for "bad_model" are returned
- Rows for "good_model" are NOT included
- `nrow(result) == 3` (all 3 quantile rows for the bad forecast unit)
**Fails now because**:
- Function does not exist yet

---

### Test 4: test_that("get_non_monotonic_forecasts() works with counts argument", { ... })

**Reproduces**: the `counts` parameter pattern from `get_duplicate_forecasts()`
**File**: same as Test 1
**Setup**:
- Create a data.table with 2 forecast units that are non-monotonic and 1 that is monotonic:
  - Unit A (model="bad1", date="2020-01-01"): non-monotonic predictions
  - Unit B (model="bad2", date="2020-01-01"): non-monotonic predictions
  - Unit C (model="good", date="2020-01-01"): monotonic predictions
  - All with `quantile_level = c(0.25, 0.5, 0.75)`, `observed = 5`
- Convert to `forecast_quantile`
**Action**:
- Call `get_non_monotonic_forecasts(data, counts = TRUE)`
**Expected**:
- Returns a summary data.table with one row per non-monotonic forecast unit
- `nrow(result) == 2` (for bad1 and bad2)
- Contains a count column (e.g., `n_non_monotonic`) indicating the number of quantile rows per affected forecast unit
**Fails now because**:
- Function does not exist yet

---

### Test 5: test_that("get_non_monotonic_forecasts() accepts custom forecast_unit argument", { ... })

**Reproduces**: the `forecast_unit` parameter pattern from `get_duplicate_forecasts()`
**File**: same as Test 1
**Setup**:
- Use `example_quantile` or a crafted dataset with extra columns
- A custom `forecast_unit` specification that differs from the default
**Action**:
- Call `get_non_monotonic_forecasts(data, forecast_unit = c("location", "target_end_date", "target_type", "model"))`
**Expected**:
- `expect_no_condition(get_non_monotonic_forecasts(example_quantile, forecast_unit = c("location", "target_end_date", "target_type", "location_name", "forecast_date", "model")))` succeeds
- Returns 0 rows for the well-formed `example_quantile`
**Fails now because**:
- Function does not exist yet

---

### Test 6: test_that("get_non_monotonic_forecasts() returns expected class", { ... })

**Reproduces**: output type contract
**File**: same as Test 1
**Setup**:
- Use `example_quantile`
**Action**:
- Call `get_non_monotonic_forecasts(example_quantile)`
**Expected**:
- `expect_s3_class(result, c("data.table", "data.frame"))` — matches the pattern from `get_duplicate_forecasts()`
**Fails now because**:
- Function does not exist yet

---

### Test 7: test_that("get_non_monotonic_forecasts() works with a plain data.frame input", { ... })

**Reproduces**: data.frame compatibility (matches `get_duplicate_forecasts()` test pattern)
**File**: same as Test 1
**Setup**:
- Create a data.frame (not data.table) with non-monotonic quantile predictions:
  ```
  data.frame(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)
  )
  ```
**Action**:
- Call `get_non_monotonic_forecasts(df_input)`
**Expected**:
- Returns rows identifying the non-monotonic forecast (same as data.table input)
- Output is a `data.table`/`data.frame`
**Fails now because**:
- Function does not exist yet

---

### Test 8: test_that("assert_forecast.forecast_quantile() warns about non-monotonic predictions", { ... })

**Reproduces**: the core issue — non-monotonic predictions silently pass `assert_forecast()` until they hit `bias_quantile()` deep in `score()`
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing)
**Setup**:
- Create a data.table with non-monotonic predictions:
  ```
  data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)
  )
  ```
**Action**:
- Call `as_forecast_quantile(data)` which internally calls `assert_forecast.forecast_quantile()`
**Expected**:
- `expect_warning(as_forecast_quantile(data), "non-monotonic|decreasing|predictions must not be decreasing")` — a warning (not error) is emitted mentioning non-monotonic predictions
- The function should still succeed (return a valid `forecast_quantile` object), just with a warning
- The warning message should mention `get_non_monotonic_forecasts()` so users know how to diagnose
**Fails now because**:
- Current `assert_forecast.forecast_quantile()` (lines 81-89 of `R/class-forecast-quantile.R`) has no monotonicity check at all. Non-monotonic data passes silently through `as_forecast_quantile()` and only errors later inside `bias_quantile_single_vector()` at scoring time.

---

### Test 9: test_that("assert_forecast.forecast_quantile() does not warn for well-formed data", { ... })

**Reproduces**: regression guard — well-formed data should not trigger the new warning
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing)
**Setup**:
- Use `na.omit(data.table::copy(example_quantile))` (known well-formed data)
**Action**:
- Call `as_forecast_quantile(data)`
**Expected**:
- `expect_no_condition(as_forecast_quantile(data))` — no warnings or messages emitted
- This is already partially tested by the existing test at line 19-29 of `test-class-forecast-quantile.R`, but should be explicitly confirmed in the context of the new monotonicity check
**Fails now because**:
- This test should PASS both before and after the fix. It guards against false positives.

---

### Test 10: test_that("assert_forecast.forecast_quantile() suppresses monotonicity warning when verbose = FALSE", { ... })

**Reproduces**: the `verbose` parameter behavior — user should be able to suppress the warning
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing)
**Setup**:
- Create the same non-monotonic data as Test 8
- First convert to `forecast_quantile` (suppressing the warning)
**Action**:
- Call `assert_forecast(forecast_obj, verbose = FALSE)` on the non-monotonic forecast object
**Expected**:
- `expect_no_condition(assert_forecast(forecast_obj, verbose = FALSE))` — no warning emitted when verbose is FALSE
**Fails now because**:
- No monotonicity check exists yet; `verbose` parameter has no effect on non-existent check

---

### Test 11: test_that("score() works end-to-end with non-monotonic predictions after fix, excluding bias", { ... })

**Reproduces**: the user's pain point — `score()` fails entirely because `bias_quantile()` errors on non-monotonic predictions, preventing all other metrics from being computed
**File**: `tests/testthat/test-class-forecast-quantile.R` (existing)
**Setup**:
- Create a forecast with non-monotonic predictions in one forecast unit (same data as Test 2)
- Convert to `forecast_quantile` (suppressing the new warning)
**Action**:
- Call `score(data, metrics = list(wis = wis))` — a metric that does NOT require monotonicity
**Expected**:
- After the fix, `score()` with non-bias metrics should succeed (WIS does not require monotonicity)
- `bias_quantile` should still error (or warn) when called individually on non-monotonic data
- This test verifies that `score()` overall is not broken by non-monotonic data when bias is excluded
**Fails now because**:
- Currently `score()` will error with "Predictions must not be decreasing" because `bias_quantile` is in the default metrics and its error propagation via `run_safely()` converts to a warning. However, the WIS metric itself does not require monotonicity and should compute fine. The core issue is that the user gets no early warning during `as_forecast_quantile()` — they only discover the problem at `score()` time.

---

### Test 12: test_that("get_non_monotonic_forecasts() handles equal predictions at adjacent quantile levels", { ... })

**Reproduces**: edge case — equal predictions at different quantile levels (flat segments) should NOT be flagged as non-monotonic; only strictly decreasing values are violations
**File**: same as Test 1
**Setup**:
- Create data with flat predictions:
  ```
  data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 5, 5)    # 5 == 5 is OK (non-decreasing)
  )
  ```
- Convert to `forecast_quantile`
**Action**:
- Call `get_non_monotonic_forecasts(data)`
**Expected**:
- `expect_identical(nrow(result), 0L)` — flat segments are valid (non-decreasing is the requirement, matching `diff(predicted) >= 0` in the existing `bias_quantile` check)
**Fails now because**:
- Function does not exist yet
