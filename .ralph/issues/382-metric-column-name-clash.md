# Issue #382: Feature: Input check to notify about clashes with metrics and column names

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/382
- **Action Summary**: Add a check after `validate_metrics(metrics)` in each `score.forecast_*` method comparing `names(metrics)` against `colnames(forecast)` and emitting `cli_warn()` on overlap. Uncomment and update the existing test skeleton in `test-score.R`.
- **Status Note**: Still relevant and unresolved. A pre-v2.0 implementation existed in `check_clean_data()` but was removed during the v2.0 rewrite. A commented-out test skeleton exists in `test-score.R` (lines 51-62). `apply_metrics()` silently overwrites any existing column whose name matches a metric name via `forecast[, (metric_name) := result]`. No pre-scoring clash detection exists.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-score.R` (existing)
**Testing patterns observed**: testthat 3, data.table fixtures via `setup.R`, `expect_warning()` / `expect_no_condition()` for condition checks, `example_binary` / `example_quantile` / `example_sample_continuous` / `example_point` as test data, custom metric functions defined inline as `function(observed, predicted, ...) { ... }`

### Test 1: test_that("score() warns when metric name clashes with existing column in binary forecast", { ... })

**Reproduces**: the core bug — `apply_metrics()` silently overwrites a forecast column (e.g., `horizon`) when a metric has the same name
**Setup**:
- Use `example_binary` as the forecast object
- Define a custom metric function: `horizon_metric <- function(observed, predicted) rep(1, length(observed))`
- Create a metric list: `metrics = list(brier_score = brier_score, horizon = horizon_metric)`
- Note: `horizon` is a forecast unit column in `example_binary`
**Action**:
- Call `score(example_binary, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message matching something like `"column name.*horizon"` or `"clashes.*horizon"` or `"metric.*horizon.*already.*column"` — the exact wording depends on implementation, but the warning must name the clashing column(s)
- Scoring should still succeed (the warning is informational, not an error)
**Fails now because**:
- No check exists in any `score.forecast_*` method to compare `names(metrics)` against `colnames(forecast)`. The `apply_metrics()` call at `score.R:159` (`forecast[, (metric_name) := result]`) silently overwrites the `horizon` column with metric output, destroying the original forecast unit data without any warning.

### Test 2: test_that("score() warns when metric name clashes with existing column in quantile forecast", { ... })

**Reproduces**: same bug for quantile forecasts, confirming fix works across forecast types
**Setup**:
- Use `example_quantile` as the forecast object
- Define a custom metric: `location_metric <- function(observed, predicted, quantile_level) rep(0, length(observed))`
- Create a metric list: `metrics = list(wis = wis, location = location_metric)`
- Note: `location` is a forecast unit column in `example_quantile`
**Action**:
- Call `score(example_quantile, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message mentioning `"location"` as a clashing column
- Scoring should still succeed
**Fails now because**:
- Same root cause as Test 1 — no pre-scoring clash detection in `score.forecast_quantile()`

### Test 3: test_that("score() warns when metric name clashes with existing column in sample forecast", { ... })

**Reproduces**: same bug for sample forecasts
**Setup**:
- Use `example_sample_continuous` as the forecast object
- Define a custom metric: `model_metric <- function(observed, predicted) rep(0.5, length(observed))`
- Create a metric list: `metrics = list(crps = crps_sample, model = model_metric)`
- Note: `model` is a forecast unit column in `example_sample_continuous`
**Action**:
- Call `score(example_sample_continuous, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message mentioning `"model"` as a clashing column
- Scoring should still succeed
**Fails now because**:
- Same root cause — no pre-scoring clash detection in `score.forecast_sample()`

### Test 4: test_that("score() warns when metric name clashes with existing column in point forecast", { ... })

**Reproduces**: same bug for point forecasts
**Setup**:
- Use `example_point` as the forecast object
- Define a custom metric: `horizon_metric <- function(observed, predicted) abs(observed - predicted)`
- Create a metric list: `metrics = list(ae_point = Metrics::ae, horizon = horizon_metric)`
**Action**:
- Call `score(example_point, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message mentioning `"horizon"` as a clashing column
**Fails now because**:
- Same root cause — no pre-scoring clash detection in `score.forecast_point()`

### Test 5: test_that("score() does not warn when metric names don't clash with column names", { ... })

**Reproduces**: regression prevention — confirms normal operation is unaffected
**Setup**:
- Use `example_binary` with default metrics (no clash since `brier_score` and `log_score` are not columns of `example_binary`)
**Action**:
- Call `score(example_binary)`
**Expected**:
- `expect_no_condition(score(example_binary))` — no warnings or messages emitted
**Fails now because**:
- This test should PASS against current code (no clash = no warning). It ensures the new check doesn't false-positive on normal operation.

### Test 6: test_that("score() warns about multiple clashing metric names at once", { ... })

**Reproduces**: edge case where multiple metric names clash with multiple columns simultaneously
**Setup**:
- Use `example_binary` as the forecast object
- Define custom metrics with names matching multiple columns:
  - `horizon_metric <- function(observed, predicted) rep(1, length(observed))`
  - `model_metric <- function(observed, predicted) rep(2, length(observed))`
- Create metric list: `metrics = list(brier_score = brier_score, horizon = horizon_metric, model = model_metric)`
**Action**:
- Call `score(example_binary, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message mentioning both `"horizon"` and `"model"` — a single warning that lists all clashing names, not separate warnings per clash
**Fails now because**:
- No clash detection exists. Both `horizon` and `model` columns would be silently overwritten.

### Test 7: test_that("score() warns when metric name clashes with a protected column name", { ... })

**Reproduces**: edge case where a metric name matches a protected column (e.g., `observed` or `predicted`) that is used internally during scoring
**Setup**:
- Use `example_binary` as the forecast object
- Define: `observed_metric <- function(observed, predicted) rep(0, length(observed))`
- Create metric list: `metrics = list(brier_score = brier_score, observed = observed_metric)`
- Note: `observed` is a protected column present in `example_binary`
**Action**:
- Call `score(example_binary, metrics = metrics)`
**Expected**:
- `expect_warning()` with a message mentioning `"observed"` as a clashing column
- The check should catch clashes with ALL existing columns (both forecast unit and protected columns), since `apply_metrics()` overwrites any column matching a metric name
**Fails now because**:
- No clash detection. Naming a metric `"observed"` would overwrite the `observed` column via `forecast[, (metric_name) := result]`, corrupting the forecast data mid-computation. The effect is potentially worse than a forecast-unit clash because `observed` is used as an argument to subsequent metric functions.
