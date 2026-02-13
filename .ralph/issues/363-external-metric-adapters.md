# Issue #363: Design interface for external functions with different input/output formats

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/363
- **Action Summary**: Implement 2-3 small exported adapter functions (e.g., `metric_adapt_binary_numeric()` to convert factor to 0/1, `metric_adapt_swap_args()` to reverse observed/predicted order). Document usage in `score()`'s "Customising metrics" section. Resolve naming convention with maintainers first.
- **Status Note**: Still relevant. No wrapper/adapter functions exist. Users must manually wrap external metrics with non-standard signatures (e.g., reversed arg order, numeric 0/1 instead of factor observed). Two small wrapper proposals in the issue body (`wrap_binary_numeric`, `wrap_switch_order`) were never implemented. The `purrr::partial()` mechanism alone cannot solve argument reordering or type conversion. Zero comments, no design consensus. 2+ years old.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-metric-adapters.R` (new)
**Testing patterns observed**: testthat with data.table fixtures; `example_binary`/`example_sample_continuous`/`example_point` datasets used directly; custom metrics passed as named lists to `score()`; `purrr::partial()` pattern used for metric customization (see `test-metrics.R:89`); `run_safely()` filters named args via `formals()` introspection; metrics called with unnamed positional args (observed, predicted, ...) via `apply_metrics()`.

### Test 1: test_that("metric_adapt_binary_numeric() converts factor observed to numeric 0/1", { ... })

**Reproduces**: the core use case — external binary metrics expecting numeric 0/1 fail when given factor observed values
**Setup**:
- Create factor observed: `observed <- factor(c(0, 1, 1, 0, 1))`
- Create predicted probabilities: `predicted <- c(0.2, 0.8, 0.6, 0.3, 0.9)`
- Define a simple external metric that requires numeric observed: `external_brier <- function(observed, predicted) { (observed - predicted)^2 }` (this would fail with factor input because arithmetic on factors is not supported)
**Action**:
- Call `adapted <- metric_adapt_binary_numeric(external_brier)`
- Call `result <- adapted(observed, predicted)`
**Expected**:
- `expect_equal(result, (c(0, 1, 1, 0, 1) - predicted)^2)` — the factor is converted to 0/1 numeric and the external function computes correctly
- Without the adapter, `external_brier(observed, predicted)` should produce an error or `NA` values because factor arithmetic is not meaningful
**Fails now because**:
- `metric_adapt_binary_numeric()` does not exist. There is no adapter mechanism to convert factor observed to numeric for external metrics.

### Test 2: test_that("metric_adapt_binary_numeric() works inside score() with binary forecasts", { ... })

**Reproduces**: end-to-end integration of the adapter within the `score()` pipeline
**Setup**:
- Use `example_binary` dataset
- Define an external metric expecting numeric 0/1: `numeric_brier <- function(observed, predicted) { (observed - predicted)^2 }`
- Wrap it: `adapted_brier <- metric_adapt_binary_numeric(numeric_brier)`
- Build custom metrics list: `metrics <- list(custom_brier = adapted_brier)`
**Action**:
- Call `result <- score(example_binary, metrics = metrics)`
**Expected**:
- `expect_s3_class(result, "scores")` — scoring completes without error
- `expect_true("custom_brier" %in% names(result))` — custom metric column is present
- The custom_brier values should match `brier_score()` output (since both compute squared error after factor-to-numeric conversion): compare against `score(example_binary, metrics = list(brier_score = brier_score))$brier_score`
**Fails now because**:
- `metric_adapt_binary_numeric()` does not exist, so there is no way to use a numeric-expecting external metric within `score()` for binary forecasts.

### Test 3: test_that("metric_adapt_swap_args() reverses observed/predicted argument order", { ... })

**Reproduces**: the core use case — external metrics with `function(predicted, observed)` signature fail when called as `function(observed, predicted)`
**Setup**:
- Create numeric data: `observed <- c(1, 2, 3, 4, 5)`, `predicted <- c(1.1, 2.2, 2.8, 4.1, 5.3)`
- Define an external metric with swapped arg order that records what it receives: `swapped_ae <- function(predicted, observed) { abs(predicted - observed) }` (this function expects predicted first, observed second)
- Also define a reference function: `normal_ae <- function(observed, predicted) { abs(observed - predicted) }`
**Action**:
- Call `adapted <- metric_adapt_swap_args(swapped_ae)`
- Call `result <- adapted(observed, predicted)`
**Expected**:
- `expect_equal(result, normal_ae(observed, predicted))` — both produce the same absolute errors
- Verify the swap actually happened: define a function that returns its first arg: `first_arg <- function(a, b) { a }`. Then `metric_adapt_swap_args(first_arg)(10, 20)` should return `20` (not `10`), because the adapter swaps the positional args before calling the inner function.
**Fails now because**:
- `metric_adapt_swap_args()` does not exist. Users must manually write wrapper functions or use `purrr::partial()` (which cannot reorder positional arguments).

### Test 4: test_that("metric_adapt_swap_args() works inside score() with point forecasts", { ... })

**Reproduces**: end-to-end integration of the swap adapter within `score()` for point forecasts
**Setup**:
- Use `example_point` dataset
- Define an external absolute error function with swapped order: `swapped_ae <- function(predicted, observed) { abs(predicted - observed) }`
- Wrap it: `adapted_ae <- metric_adapt_swap_args(swapped_ae)`
- Build metrics list: `metrics <- list(ae_adapted = adapted_ae)`
**Action**:
- Call `result <- score(example_point, metrics = metrics)`
**Expected**:
- `expect_s3_class(result, "scores")` — scoring completes without error
- `expect_true("ae_adapted" %in% names(result))` — custom metric column is present
- Values should match the standard absolute error: compare `result$ae_adapted` against `score(example_point, metrics = list(ae_point = Metrics::ae))$ae_point`
**Fails now because**:
- `metric_adapt_swap_args()` does not exist.

### Test 5: test_that("metric_adapt_swap_args() works inside score() with sample forecasts", { ... })

**Reproduces**: integration with sample forecasts where predicted is a matrix (not a vector)
**Setup**:
- Use `example_sample_continuous` dataset
- Define an external CRPS-like function with swapped order: `swapped_crps <- function(predicted, observed) { scoringRules::crps_sample(y = observed, dat = predicted) }` (note: `scoringRules::crps_sample` already takes `(y, dat)` which is `(observed, predicted)` order)
- Actually, a simpler approach: define `swapped_mad <- function(predicted, observed) { apply(predicted, 1, mad) }` which ignores observed but takes predicted first
- Wrap it: `adapted_mad <- metric_adapt_swap_args(swapped_mad)`
- Build metrics list: `metrics <- list(mad_adapted = adapted_mad)`
**Action**:
- Call `result <- score(example_sample_continuous, metrics = metrics)`
**Expected**:
- `expect_s3_class(result, "scores")` — scoring completes without error
- `expect_true("mad_adapted" %in% names(result))` — custom metric column is present
- Values should match the standard `mad_sample()` output: compare `result$mad_adapted` against `score(example_sample_continuous, metrics = list(mad = mad_sample))$mad`
**Fails now because**:
- `metric_adapt_swap_args()` does not exist.

### Test 6: test_that("adapted metrics return a function object", { ... })

**Reproduces**: adapters must return proper function objects that are compatible with `validate_metrics()`, `run_safely()`, and `purrr::partial()`
**Setup**:
- Define a dummy metric: `dummy <- function(observed, predicted) { 0 }`
**Action**:
- Call `adapted_numeric <- metric_adapt_binary_numeric(dummy)`
- Call `adapted_swap <- metric_adapt_swap_args(dummy)`
**Expected**:
- `checkmate::expect_class(adapted_numeric, "function")` — the result is a function
- `checkmate::expect_class(adapted_swap, "function")` — the result is a function
- `expect_true(length(formals(adapted_numeric)) >= 2)` — the function has at least `observed` and `predicted` formals (important for `run_safely()` arg filtering)
- `expect_true("..." %in% names(formals(adapted_numeric)))` — the function accepts `...` so `run_safely()` passes through all args
- Same checks for `adapted_swap`
**Fails now because**:
- Neither adapter function exists.

### Test 7: test_that("adapted metrics compose with purrr::partial()", { ... })

**Reproduces**: adapters should be composable with the existing `purrr::partial()` customization pattern
**Setup**:
- Define a metric with an extra parameter: `weighted_brier <- function(observed, predicted, weight = 1) { weight * (observed - predicted)^2 }` (expects numeric observed)
- Wrap with binary adapter: `adapted <- metric_adapt_binary_numeric(weighted_brier)`
- Apply partial to set weight: `custom <- purrr::partial(adapted, weight = 2)`
- Create test data: `observed <- factor(c(0, 1, 1, 0))`, `predicted <- c(0.3, 0.7, 0.8, 0.2)`
**Action**:
- Call `result <- custom(observed, predicted)`
**Expected**:
- `expect_equal(result, 2 * (c(0, 1, 1, 0) - predicted)^2)` — the weight=2 is applied after factor-to-numeric conversion
**Fails now because**:
- `metric_adapt_binary_numeric()` does not exist.

### Test 8: test_that("metric_adapt_binary_numeric() errors on non-function input", { ... })

**Reproduces**: input validation for the adapter
**Setup**:
- None
**Action**:
- Call `metric_adapt_binary_numeric("not_a_function")`
- Call `metric_adapt_binary_numeric(42)`
- Call `metric_adapt_binary_numeric(NULL)`
**Expected**:
- Each call should produce an error, e.g. `expect_error(metric_adapt_binary_numeric("not_a_function"))` — the adapter should validate that its argument is a function
**Fails now because**:
- The function does not exist.

### Test 9: test_that("metric_adapt_swap_args() errors on non-function input", { ... })

**Reproduces**: input validation for the swap adapter
**Setup**:
- None
**Action**:
- Call `metric_adapt_swap_args("not_a_function")`
- Call `metric_adapt_swap_args(42)`
**Expected**:
- Each call should produce an error, e.g. `expect_error(metric_adapt_swap_args("not_a_function"))`
**Fails now because**:
- The function does not exist.

### Test 10: test_that("metric_adapt_binary_numeric() preserves extra arguments via ...", { ... })

**Reproduces**: edge case where the wrapped function accepts extra named arguments — these must pass through the adapter and survive `run_safely()` arg filtering
**Setup**:
- Define a metric with an extra param: `my_metric <- function(observed, predicted, na.rm = FALSE) { if (na.rm) observed[is.na(observed)] <- 0; sum(observed != predicted) }`
- Wrap it: `adapted <- metric_adapt_binary_numeric(my_metric)`
- Create data with a factor observed: `observed <- factor(c(0, 1, NA, 0, 1))`, `predicted <- c(0.3, 0.7, 0.5, 0.2, 0.8)`
**Action**:
- Call `result <- adapted(observed, predicted, na.rm = TRUE)`
**Expected**:
- The `na.rm = TRUE` is forwarded to the inner function. The adapter should pass through `...` args to the wrapped function.
**Fails now because**:
- `metric_adapt_binary_numeric()` does not exist.
