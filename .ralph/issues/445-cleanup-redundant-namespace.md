# Issue #445: Cleanup: delete calls that use `::` or `:::` and add to imports instead

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/445
- **Action Summary**: Remove the `pkg::` prefix from 25 redundant function calls across 9 R files. Leave roxygen documentation cross-references and cli-formatting self-references unchanged.
- **Status Note**: Still relevant. 25 redundant `pkg::fn()` calls across 9 R files where functions are already imported in NAMESPACE (13 data.table, 5 scoringRules, 3 Metrics, 2 cli, 1 purrr, 1 utils). The oft-cited "64 occurrences" overstates scope — 37 are roxygen/comment references that should stay. Purely cosmetic cleanup.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-namespace-imports.R` (new)
**Testing patterns observed**: testthat 3e, data.table fixtures in `setup.R`, pre-computed `scores_*` objects, `expect_no_condition()` for regression checks, `expect_equal()`/`expect_identical()` for value checks.

### Context

This is a mechanical refactor — removing redundant `pkg::fn()` prefixes where the function is already `importFrom`'d in NAMESPACE. The change is behavior-preserving. There is no "bug" to reproduce; the primary risk is:

1. A function is not actually imported (would cause "could not find function" at runtime)
2. A function name shadows another package's function (would silently call the wrong function)

Existing test files (`test-pairwise_comparison.R`, `test-metrics-sample.R`, `test-get-correlations.R`, `test-class-forecast-sample.R`, `test-class-forecast-quantile.R`, `test-class-forecast.R`, `test-helper-quantile-interval-range.R`) already exercise all affected code paths. The tests below are supplementary validation that the NAMESPACE is correct and the cleanup is safe.

### Redundant `::` calls to be removed

All of the following functions are confirmed present in NAMESPACE via `importFrom`:

| File | Line(s) | Redundant Call | NAMESPACE Line |
|------|---------|----------------|----------------|
| `R/get-correlations.R` | 103 | `data.table::as.data.table(...)` | `importFrom(data.table,as.data.table)` (L154) |
| `R/get-correlations.R` | 104 | `data.table::melt(...)` | `importFrom(data.table,melt)` (L161) |
| `R/class-forecast-sample.R` | 217 | `data.table::dcast(...)` | `importFrom(data.table,dcast)` (L157) |
| `R/helper-quantile-interval-range.R` | 77 | `data.table::rbindlist(...)` | `importFrom(data.table,rbindlist)` (L163) |
| `R/pairwise-comparisons.R` | 252 | `data.table::rbindlist(...)` | `importFrom(data.table,rbindlist)` (L163) |
| `R/pairwise-comparisons.R` | 334 | `data.table::rbindlist(...)` | `importFrom(data.table,rbindlist)` (L163) |
| `R/pairwise-comparisons.R` | 301 | `data.table::as.data.table(...)` | `importFrom(data.table,as.data.table)` (L154) |
| `R/pairwise-comparisons.R` | 319 | `data.table::copy(...)` | `importFrom(data.table,copy)` (L155) |
| `R/pairwise-comparisons.R` | 327 | `data.table::data.table(...)` | `importFrom(data.table,data.table)` (L156) |
| `R/pairwise-comparisons.R` | 377, 385 | `data.table::setnames(...)` | `importFrom(data.table,setnames)` (L168) |
| `R/pairwise-comparisons.R` | 432 | `data.table::as.data.table(...)` | `importFrom(data.table,as.data.table)` (L154) |
| `R/metrics-sample.R` | 226 | `scoringRules::logs_sample(...)` | `importFrom(scoringRules,logs_sample)` (L208) |
| `R/metrics-sample.R` | 258 | `scoringRules::dss_sample(...)` | `importFrom(scoringRules,dss_sample)` (L206) |
| `R/metrics-sample.R` | 309, 321 | `scoringRules::crps_sample(...)` | `importFrom(scoringRules,crps_sample)` (L205) |
| `R/class-forecast-quantile.R` | 242 | `cli::cli_warn(...)` | `importFrom(cli,cli_warn)` (L146) |
| `R/class-forecast-quantile.R` | 208 | `purrr::partial(...)` | `importFrom(purrr,partial)` (L204) |
| `R/class-forecast.R` | 287 | `data.table::is.data.table(...)` | `importFrom(data.table,is.data.table)` (L159) |
| `R/class-forecast.R` | 396 | `utils::tail(...)` | `importFrom(utils,tail)` (L224) |
| `R/metrics-sample.R` | 550 | `cli::cli_warn(...)` | `importFrom(cli,cli_warn)` (L146) |

**Not redundant** (should be verified but likely also imported):
| `R/metrics-ordinal.R` | 78 | `scoringRules::rps_probs(...)` | `importFrom(scoringRules,rps_probs)` (L209) — this IS imported, so it IS redundant too |

### Test 1: test_that("all functions used without :: prefix are properly imported in NAMESPACE", { ... })

**Reproduces**: The risk that removing `::` exposes a missing import
**Setup**:
- No special setup needed. Uses the package's own NAMESPACE file and `getNamespaceImports()`.
**Action**:
- For each function that will have its `::` prefix removed, verify it is accessible in the scoringutils namespace without qualification:
  - `exists("as.data.table", envir = asNamespace("scoringutils"), inherits = TRUE)`
  - Repeat for: `melt`, `dcast`, `rbindlist`, `copy`, `data.table`, `setnames`, `is.data.table`, `logs_sample`, `dss_sample`, `crps_sample`, `cli_warn`, `partial`, `tail`, `rps_probs`
**Expected**:
- All `exists()` calls return `TRUE`, confirming every function is available in the package namespace without the `::` prefix.
**Fails now because**:
- This test should PASS against current code (functions are already imported). It serves as a safety net to confirm the cleanup is valid. If any function were NOT imported, this test would catch it.

### Test 2: test_that("scoringRules wrapper functions resolve to correct implementations", { ... })

**Reproduces**: The risk that removing `scoringRules::` prefix causes name collision (since scoringutils exports functions with the SAME names: `logs_sample`, `dss_sample`, `crps_sample`)
**Setup**:
- Create small test data: `observed <- rpois(10, lambda = 5)` and `predicted <- replicate(50, rpois(10, lambda = 5))`
**Action**:
- Call the scoringutils wrapper functions (`logs_sample()`, `dss_sample()`, `crps_sample()`) with the test data
- These wrappers internally call `scoringRules::logs_sample()`, etc. After the cleanup, they will call `logs_sample()` without the prefix
- Verify the wrappers don't accidentally recurse into themselves (infinite recursion) by checking they return numeric vectors of the correct length
**Expected**:
- `logs_sample(observed, predicted)` returns a numeric vector of length 10
- `dss_sample(observed, predicted)` returns a numeric vector of length 10
- `crps_sample(observed, predicted)` returns a numeric vector of length 10
- No errors or infinite recursion
**Fails now because**:
- This test should PASS against current code. **CRITICAL**: After removing the `scoringRules::` prefix from within the wrapper functions, there is a risk of infinite recursion since the wrapper function has the SAME name as the imported function. The implementer MUST verify that R's scoping rules resolve the unqualified call to the imported `scoringRules` function, not to the wrapper itself. If recursion occurs, the `::` prefix must be RETAINED for these three functions. This test will catch that failure.

### Test 3: test_that("get_correlations() produces correct output after namespace cleanup", { ... })

**Reproduces**: Regression in `get_correlations()` which uses `data.table::as.data.table()` and `data.table::melt()` at lines 103-104
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `get_correlations(summarise_scores(scores_quantile, by = get_forecast_unit(scores_quantile)))`
**Expected**:
- Returns a `scores` object (data.table) with metric columns matching the metrics in `scores_quantile`
- `expect_s3_class(result, c("scores", "data.table", "data.frame"), exact = TRUE)`
- No errors or warnings
**Fails now because**:
- This test should PASS against current code. It is a regression guard ensuring `as.data.table()` and `melt()` resolve correctly after `::` removal.

### Test 4: test_that("pairwise comparisons produce correct output after namespace cleanup", { ... })

**Reproduces**: Regression in `pairwise-comparisons.R` which has 9 redundant `data.table::` calls
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `get_pairwise_comparisons(scores_quantile, baseline = "EuroCOVIDhub-baseline")`
**Expected**:
- Returns a data.table with columns including `model`, `compare_against`, `mean_scores_ratio`, `pval`, `adj_pval`, `wis_relative_skill`, `wis_scaled_relative_skill`
- Numeric results are finite and non-NA
- `expect_no_condition(...)` — no errors or warnings
**Fails now because**:
- This test should PASS against current code. It is a regression guard for the many `data.table::` calls in `pairwise-comparisons.R` (`rbindlist`, `as.data.table`, `copy`, `data.table`, `setnames`).

### Test 5: test_that("PIT histogram for sample forecasts works after namespace cleanup", { ... })

**Reproduces**: Regression in `class-forecast-sample.R` which uses `data.table::dcast()` at line 217
**Setup**:
- Use `example_sample_continuous` from the package
**Action**:
- Call `get_pit_histogram(as_forecast_sample(example_sample_continuous))`
**Expected**:
- Returns a data.table with `density` and `bin` columns
- Density values are non-negative
- No errors
**Fails now because**:
- This test should PASS against current code. It guards the `dcast()` call in `get_pit_histogram.forecast_sample()`.

### Test 6: test_that("quantile_to_interval conversion works after namespace cleanup", { ... })

**Reproduces**: Regression in `helper-quantile-interval-range.R` which uses `data.table::rbindlist()` at line 77
**Setup**:
- Use `example_quantile` from the package
**Action**:
- Call `score(as_forecast_quantile(na.omit(example_quantile)))` — this exercises the `quantile_to_interval()` internal function which contains the `rbindlist()` call
**Expected**:
- Returns a `scores` object with expected metrics (wis, interval_coverage_50, interval_coverage_90, etc.)
- No errors
**Fails now because**:
- This test should PASS against current code. It guards the `rbindlist()` call in the quantile-to-interval conversion path.

### Test 7: test_that("get_metrics for quantile forecasts resolves partial() correctly", { ... })

**Reproduces**: Regression in `class-forecast-quantile.R` line 208 which uses `purrr::partial()`
**Setup**:
- Use `example_quantile` from the package
**Action**:
- Call `get_metrics(as_forecast_quantile(na.omit(example_quantile)))` and verify `interval_coverage_90` is in the result
- Call the returned `interval_coverage_90` function to verify it's a properly partially-applied `interval_coverage` with `interval_range = 90`
**Expected**:
- `"interval_coverage_90"` is in `names(metrics)`
- `metrics$interval_coverage_90` is a function
- The function has `interval_range` pre-filled to 90 (via `partial()`)
**Fails now because**:
- This test should PASS against current code. It guards the `purrr::partial()` call in `get_metrics.forecast_quantile()`.

## Implementation Notes

**CRITICAL WARNING for Test 2 / scoringRules wrappers**: The scoringutils package defines wrapper functions `logs_sample()`, `dss_sample()`, and `crps_sample()` that internally call the identically-named functions from the `scoringRules` package. Currently they use `scoringRules::logs_sample(...)` to explicitly call the external function. If the `scoringRules::` prefix is removed, R's lexical scoping rules mean the unqualified `logs_sample()` call inside the wrapper would refer to... the wrapper itself, causing infinite recursion.

**Recommendation**: The implementer should KEEP the `scoringRules::` prefix on these three functions (`logs_sample`, `dss_sample`, `crps_sample` in `R/metrics-sample.R`) because the wrapper functions shadow the imported names. This is NOT a redundant `::` — it is a necessary disambiguation. The same logic applies to `rps_probs` in `R/metrics-ordinal.R` if there's a wrapper with the same name (check: the function is `rps_ordinal`, not `rps_probs`, so `scoringRules::rps_probs` can safely have its prefix removed).

This reduces the actual cleanup from ~20 calls to ~15 calls (excluding the 5 scoringRules calls that must keep their prefix).

**Additional note on `utils::tail` in `tail.forecast()`**: The `utils::tail()` call at `R/class-forecast.R:396` is inside an S3 method `tail.forecast()`. Removing the `utils::` prefix would cause infinite recursion since `tail(as.data.table(x), ...)` would dispatch back to `tail.forecast()` if the object still has the forecast class. The `as.data.table()` call should strip the class, but the `utils::` prefix is a safety measure. **Recommendation**: KEEP the `utils::tail` prefix here as well.

**Summary of calls that should KEEP their `::` prefix**:
- `scoringRules::logs_sample()` in `R/metrics-sample.R:226` (self-shadowing)
- `scoringRules::dss_sample()` in `R/metrics-sample.R:258` (self-shadowing)
- `scoringRules::crps_sample()` in `R/metrics-sample.R:309,321` (self-shadowing)
- `utils::tail()` in `R/class-forecast.R:396` (potential S3 dispatch recursion)

**Summary of calls safe to remove `::` prefix**:
- All `data.table::` calls (13 instances across 4 files) — no shadowing risk
- `cli::cli_warn()` (2 instances) — no shadowing risk
- `purrr::partial()` (1 instance) — no shadowing risk
- `scoringRules::rps_probs()` (1 instance in `R/metrics-ordinal.R`) — no shadowing, `rps_ordinal` is the wrapper name
