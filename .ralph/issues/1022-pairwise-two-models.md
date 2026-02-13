# Issue #1022: Let `get_pairwise_comparison()` run with two models?

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/1022
- **Action Summary**: Change the `cli_abort()` to `cli_warn()` at line 162 of `R/pairwise-comparisons.R` so that 2 models with one as baseline emits a warning instead of an error. Update test expectations and fix the "compairisons" typo.
- **Status Note**: Still relevant. The check at line 162 of `R/pairwise-comparisons.R` calls `cli_abort()` when `length(setdiff(comparators, baseline)) < 2`, blocking the 2-models-with-baseline case. Maintainer has expressed agreement to change to a warning. Also has typo "compairisons".

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-pairwise_comparison.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures, pre-computed scores in `setup.R` (`scores_quantile`, `scores_sample_continuous`, `scores_point`, etc.), `expect_error()`/`expect_warning()` for condition checks, `suppressMessages()` wrappers around `as_forecast_*()` calls, `expect_equal()` with tolerance for numeric comparisons.

### Test 1: test_that("get_pairwise_comparisons() warns but works with two models and a baseline", { ... })

**Reproduces**: the core bug — 2 models with one as baseline should warn, not error
**Setup**:
- Use pre-computed `scores_sample_continuous` from `setup.R`
- Subset to exactly 2 models: `eval_few <- scores_sample_continuous[model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")]`
**Action**:
- Call `get_pairwise_comparisons(eval_few, compare = "model", baseline = "EuroCOVIDhub-baseline", metric = "crps")`
**Expected**:
- The call should emit a warning (via `expect_warning()`) with a message about pairwise comparisons with limited non-baseline models (the warning message should mention that there is only one non-baseline model, or similar)
- The call should NOT error — it should return a `data.table` result
- The result should contain columns `model`, `compare_against`, `mean_scores_ratio`, `crps_relative_skill`, and `crps_scaled_relative_skill`
- For the non-baseline model compared against the baseline, `mean_scores_ratio` should be a finite numeric value
- `crps_scaled_relative_skill` should be `1.0` for the baseline model
**Fails now because**:
- Line 161-169 of `R/pairwise-comparisons.R` calls `cli_abort()` when `length(setdiff(comparators, baseline)) < 2`, which is the case when there are exactly 2 models and one is the baseline. The function errors instead of producing results.

### Test 2: test_that("add_relative_skill() warns but works with two models and a baseline", { ... })

**Reproduces**: the same bug via the `add_relative_skill()` wrapper
**Setup**:
- Use pre-computed `scores_sample_continuous` from `setup.R`
- Subset to exactly 2 models: `eval_few <- scores_sample_continuous[model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")]`
**Action**:
- Call `add_relative_skill(eval_few, compare = "model", baseline = "EuroCOVIDhub-baseline", metric = "crps")`
**Expected**:
- The call should emit a warning (via `expect_warning()`) about limited non-baseline models
- The call should return a `scores` object (i.e., a data.table with a `metrics` attribute)
- The result should contain the columns `crps_relative_skill` and `crps_scaled_relative_skill`
- The `crps_scaled_relative_skill` column should contain finite numeric values
- The baseline model's `crps_scaled_relative_skill` should be `1.0`
**Fails now because**:
- `add_relative_skill()` delegates to `get_pairwise_comparisons()`, which errors at line 163 of `R/pairwise-comparisons.R` with `cli_abort()`.

### Test 3: test_that("get_pairwise_comparisons() two-model ratio is mathematically correct", { ... })

**Reproduces**: verifies correctness of the computed ratio in the two-model case
**Setup**:
- Create a small deterministic test data.frame with 2 models and known scores:
  ```
  df <- data.frame(
    model = rep(c("model_a", "model_b"), each = 5),
    date = rep(as.Date("2020-01-01") + 1:5, 2),
    wis = c(2, 4, 6, 8, 10, 1, 2, 3, 4, 5)
  )
  attr(df, "metrics") <- "wis"
  ```
- The mean score ratio for model_a vs model_b should be `sum(2,4,6,8,10) / sum(1,2,3,4,5)` = `30/15` = `2.0`
**Action**:
- Call `get_pairwise_comparisons(df, compare = "model", baseline = "model_b", metric = "wis")` wrapped in `expect_warning()` (to capture the expected two-model warning)
**Expected**:
- The `mean_scores_ratio` for model_a vs model_b should be `2.0`
- The `mean_scores_ratio` for model_b vs model_a should be `0.5`
- The `wis_scaled_relative_skill` for model_b (baseline) should be `1.0`
- The `wis_scaled_relative_skill` for model_a should be `2.0` (since with only one non-baseline comparison, the geometric mean of ratios is just the single ratio itself, and scaled relative skill = theta / theta_baseline)
**Fails now because**:
- The function errors before computing any ratios due to the `cli_abort()` at line 163.

### Test 4: test_that("get_pairwise_comparisons() still errors with only one model", { ... })

**Reproduces**: ensures the fix doesn't break the legitimate single-model error case
**Setup**:
- Use pre-computed `scores_sample_continuous` from `setup.R`
- Subset to exactly 1 model: `eval_one <- scores_sample_continuous[model == "EuroCOVIDhub-ensemble"]`
**Action**:
- Call `get_pairwise_comparisons(eval_one, compare = "model", metric = "crps")`
**Expected**:
- The call should error (via `expect_error()`) because there is only 1 model total — no pairwise comparison is possible regardless of baseline
- The error should come from `pairwise_comparison_one_group()` at line 289-293 which checks `length(comparators) < 2`
**Fails now because**:
- N/A — this test should pass both before and after the fix (regression test). The error for 1 model comes from a different code path (`pairwise_comparison_one_group()`, line 289) than the 2-models-with-baseline check (line 161).

### Test 5: test_that("existing test update: two models without baseline still works", { ... })

**Reproduces**: confirms that the existing behavior for 2 models WITHOUT a baseline is preserved
**Setup**:
- Use pre-computed `scores_sample_continuous` from `setup.R`
- Subset to exactly 2 models: `eval_few <- scores_sample_continuous[model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")]`
**Action**:
- Call `add_relative_skill(eval_few, compare = "model", metric = "crps")` (no baseline specified)
**Expected**:
- The call should succeed without error or warning (via `expect_no_condition()`)
- This is the existing test at line 323-329 of the test file — it should continue passing unchanged
**Fails now because**:
- N/A — this already passes. Included as a regression guard since the code path at line 161 uses `setdiff(comparators, baseline)` which with `baseline = NULL` gives all comparators, so `length >= 2` passes for 2 models.

### Note: Existing test to update

The existing test at lines 330-337 of `test-pairwise_comparison.R` currently uses `expect_error()` for the two-model-with-baseline case:
```r
expect_error(
  add_relative_skill(
    eval_few,
    compare = "model", baseline = "EuroCOVIDhub-baseline",
    metric = "crps"
  ),
  "More than one non-baseline model is needed to compute pairwise compairisons."
)
```
This must be changed to `expect_warning()` (with the corrected spelling "comparisons") to reflect the new behavior. The warning message pattern should also be updated from "compairisons" to "comparisons".

### Note: Typo fix

The error/warning message at line 166 of `R/pairwise-comparisons.R` contains the typo "compairisons" which should be corrected to "comparisons". This affects the message pattern in the existing test at line 336 as well.

## Resolution

**Implemented**: 2026-02-13
**Files changed**:
- `R/pairwise-comparisons.R` — Changed `cli_abort()` to `cli_warn()` for the two-model-with-baseline case; added `!is.null(baseline)` guard; fixed "compairisons" typo; improved warning message
- `tests/testthat/test-pairwise_comparison.R` — Updated existing `expect_error` to `expect_warning`; added 5 new test cases for two-model scenarios

### What was changed
The check at line 161 of `R/pairwise-comparisons.R` previously called `cli_abort()` whenever `length(setdiff(comparators, baseline)) < 2`, blocking the legitimate case of 2 models with one as baseline. The fix:
1. Added `!is.null(baseline)` guard so the check only applies when a baseline is specified (single-model without baseline case is caught downstream by `pairwise_comparison_one_group()`)
2. Changed `cli_abort()` to `cli_warn()` so the function proceeds with a warning instead of erroring
3. Fixed the "compairisons" typo to "comparisons" and improved the message

### Test results
- `test_that("get_pairwise_comparisons() warns but works with two models and a baseline")` — PASS
- `test_that("add_relative_skill() warns but works with two models and a baseline")` — PASS
- `test_that("get_pairwise_comparisons() two-model ratio is mathematically correct")` — PASS
- `test_that("get_pairwise_comparisons() still errors with only one model")` — PASS
- `test_that("two models without baseline still works without warning")` — PASS
- Full test suite — PASS (695 tests)
- R CMD check — 0 errors, 0 warnings, 2 notes (pre-existing)
