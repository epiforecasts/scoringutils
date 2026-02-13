# Issue #942: Discrimination plots for binary forecasts

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/942
- **Action Summary**: Implement a new `plot_discrimination()` function that accepts a `forecast_binary` object, groups predicted probabilities by the two levels of observed, and renders overlapping density plots via ggplot2, with optional faceting by model. Add tests and docs.
- **Status Note**: Still relevant. No binary-specific plot functions exist in the codebase. No discrimination plot, reliability diagram, or any binary visualization exists. The `forecast_binary` class and `example_binary` dataset provide exactly the data structures needed.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-plot_discrimination.R` (new)
**Testing patterns observed**: testthat + data.table fixtures; pre-computed scores/forecasts defined in `setup.R`; `expect_s3_class(p, "ggplot")` for plot objects; `vdiffr::expect_doppelganger()` for visual regression (guarded by `skip_on_cran()`); `expect_error()` for input validation; `checkmate` assertions used for argument validation; plot functions use `ensure_data.table()` and `theme_scoringutils()`.

### Test 1: test_that("plot_discrimination() works with a forecast_binary object", { ... })

**Reproduces**: the core feature gap — no binary discrimination plot exists
**Setup**:
- Use `na.omit(example_binary)` as the input (same as `setup.R` convention for removing NA rows from example data)
**Action**:
- Call `plot_discrimination(na.omit(example_binary))`
**Expected**:
- Returns a ggplot object: `expect_s3_class(p, "ggplot")`
- The plot should contain density geom layers (one per observed level)
- Visual regression snapshot: `vdiffr::expect_doppelganger("plot_discrimination", p)` (guarded by `skip_on_cran()`)
**Fails now because**:
- `plot_discrimination()` does not exist — calling it will produce `Error: could not find function "plot_discrimination"`

### Test 2: test_that("plot_discrimination() works with faceting by model", { ... })

**Reproduces**: the optional faceting feature described in the action summary
**Setup**:
- Use `na.omit(example_binary)` as the input
**Action**:
- Call `plot_discrimination(na.omit(example_binary))` and add `+ facet_wrap(~model)` to the result (following the `plot_wis` pattern where faceting is applied externally by the user)
**Expected**:
- Returns a ggplot object: `expect_s3_class(p, "ggplot")`
- Visual regression snapshot: `vdiffr::expect_doppelganger("plot_discrimination_facet_model", p)` (guarded by `skip_on_cran()`)
**Fails now because**:
- `plot_discrimination()` does not exist

### Test 3: test_that("plot_discrimination() works with a plain data.frame input", { ... })

**Reproduces**: the pattern from other plot functions (e.g., `plot_wis`) that accept data.table or data.frame inputs interchangeably via `ensure_data.table()`
**Setup**:
- Create a minimal data.frame with columns `observed` (factor with levels "0", "1") and `predicted` (numeric probabilities between 0 and 1), plus a `model` column. Example: `data.frame(observed = factor(c("0","0","1","1"), levels = c("0","1")), predicted = c(0.1, 0.3, 0.7, 0.9), model = "test_model")`
**Action**:
- Call `plot_discrimination(df)` where `df` is the plain data.frame
**Expected**:
- Returns a ggplot object: `expect_s3_class(p, "ggplot")`
- Does not error — the function should accept raw data.frame inputs (not only `forecast_binary` objects), consistent with other plot functions in the package
**Fails now because**:
- `plot_discrimination()` does not exist

### Test 4: test_that("plot_discrimination() errors with missing required columns", { ... })

**Reproduces**: input validation edge case — ensure the function provides a helpful error when required columns are missing
**Setup**:
- Create a data.frame with only a `predicted` column (missing `observed`): `data.frame(predicted = c(0.1, 0.5, 0.9))`
- Create a data.frame with only an `observed` column (missing `predicted`): `data.frame(observed = factor(c("0", "1", "0"), levels = c("0", "1")))`
**Action**:
- Call `plot_discrimination()` on each of the above
**Expected**:
- Both calls should produce an error (`expect_error()`) with a message indicating the missing column(s), e.g., matching the pattern `"observed"` or `"predicted"` (following the `check_columns_present` or `assert_subset` pattern used by other plot functions)
**Fails now because**:
- `plot_discrimination()` does not exist

### Test 5: test_that("plot_discrimination() handles single-model data", { ... })

**Reproduces**: edge case where there is only one model in the data (no faceting needed)
**Setup**:
- Filter `na.omit(example_binary)` to a single model, e.g., `na.omit(example_binary)[model == "EuroCOVIDhub-ensemble"]`
**Action**:
- Call `plot_discrimination(single_model_data)`
**Expected**:
- Returns a ggplot object: `expect_s3_class(p, "ggplot")`
- The plot should render without error (no faceting issues with a single model)
**Fails now because**:
- `plot_discrimination()` does not exist

### Test 6: test_that("plot_discrimination() shows separation between observed levels", { ... })

**Reproduces**: the core visual purpose — when a model is perfectly discriminating, the two density curves should be well-separated; this test verifies the plot data is structured correctly
**Setup**:
- Create a minimal perfectly-discriminating forecast: `data.frame(observed = factor(c(rep("0", 50), rep("1", 50)), levels = c("0", "1")), predicted = c(rep(0.1, 50), rep(0.9, 50)), model = "perfect")`
**Action**:
- Call `p <- plot_discrimination(df)` and inspect the ggplot build object's data via `ggplot2::ggplot_build(p)`
**Expected**:
- `expect_s3_class(p, "ggplot")` succeeds
- The built plot data should have separate groups corresponding to the two observed levels ("0" and "1"), with their density peaks at different x positions (the "0" group near 0.1 and the "1" group near 0.9)
- Specifically, the build data layer should contain a grouping variable that maps to the observed factor levels
**Fails now because**:
- `plot_discrimination()` does not exist

### Test 7: test_that("plot_discrimination() handles edge case with all identical predictions", { ... })

**Reproduces**: edge case where all predicted probabilities are the same (density estimation may degenerate)
**Setup**:
- `data.frame(observed = factor(c("0", "0", "1", "1"), levels = c("0", "1")), predicted = c(0.5, 0.5, 0.5, 0.5), model = "constant")`
**Action**:
- Call `plot_discrimination(df)`
**Expected**:
- Should either return a ggplot object without error, or produce a clear warning/message. It should NOT crash with an obscure ggplot2 or density estimation error. `expect_no_error(plot_discrimination(df))` or at minimum `expect_s3_class(plot_discrimination(df), "ggplot")`
**Fails now because**:
- `plot_discrimination()` does not exist
