# Issue #769: Rethink the way arguments are passed in `get_pairwise_comparisons()`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/769
- **Action Summary**: Promote `test_type`, `one_sided`, and `n_permutations` from `...` to explicit named arguments in `get_pairwise_comparisons()` and `add_relative_skill()`, improving API discoverability. Update roxygen `@param` tags accordingly.
- **Status Note**: Still relevant. `test_type`, `one_sided`, and `n_permutations` are still passed via `...` through 3 levels (`get_pairwise_comparisons` -> `pairwise_comparison_one_group` -> `compare_forecasts`). Users must read internal `compare_forecasts` docs to discover options. PR #994 added `test_type = NULL` but didn't address the `...` pattern. #750 (split out p-values) was deferred.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-pairwise_comparison.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures; pre-computed `scores_quantile`, `scores_sample_continuous`, `scores_point` from `setup.R`; `suppressMessages()` used around some calls; `expect_no_condition()` / `expect_no_warning()` used for smoke tests; vdiffr for visual regression of plots.

### Test 1: test_that("get_pairwise_comparisons() accepts test_type as explicit argument", { ... })

**Reproduces**: the core API issue — `test_type` is not a visible parameter of `get_pairwise_comparisons()`
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `get_pairwise_comparisons(scores_quantile, test_type = "non_parametric")` (explicit named arg)
- Call `get_pairwise_comparisons(scores_quantile, test_type = "permutation")` (explicit named arg)
- Call `get_pairwise_comparisons(scores_quantile, test_type = NULL)` (explicit named arg, skip p-values)
**Expected**:
- All three calls succeed without error
- When `test_type = NULL`, the `pval` column should be all `NA`
- When `test_type = "non_parametric"` or `"permutation"`, the `pval` column should contain numeric values (not NA)
- `test_type` should appear in `formals(get_pairwise_comparisons)`, i.e., `"test_type" %in% names(formals(get_pairwise_comparisons))` is TRUE
**Fails now because**:
- `test_type` is not a formal parameter of `get_pairwise_comparisons()`. It only works via `...` passthrough to `compare_forecasts()`. After the fix, it will be an explicit parameter. The `formals()` check will fail against current code because `test_type` is not in the function's parameter list.

### Test 2: test_that("get_pairwise_comparisons() accepts one_sided as explicit argument", { ... })

**Reproduces**: `one_sided` is not discoverable in `get_pairwise_comparisons()` API
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `get_pairwise_comparisons(scores_quantile, one_sided = TRUE, test_type = "permutation", n_permutations = 50)`
- Call `get_pairwise_comparisons(scores_quantile, one_sided = FALSE, test_type = "permutation", n_permutations = 50)`
**Expected**:
- Both calls succeed without error
- Both return data.tables with a `pval` column containing numeric values
- `one_sided` should appear in `formals(get_pairwise_comparisons)`, i.e., `"one_sided" %in% names(formals(get_pairwise_comparisons))` is TRUE
**Fails now because**:
- `one_sided` is not a formal parameter of `get_pairwise_comparisons()`. The `formals()` check will fail against current code.

### Test 3: test_that("get_pairwise_comparisons() accepts n_permutations as explicit argument", { ... })

**Reproduces**: `n_permutations` is not discoverable in `get_pairwise_comparisons()` API
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `result <- get_pairwise_comparisons(scores_quantile, test_type = "permutation", n_permutations = 10)`
**Expected**:
- Call succeeds without error
- Returns a data.table with `pval` column containing numeric values
- `n_permutations` should appear in `formals(get_pairwise_comparisons)`, i.e., `"n_permutations" %in% names(formals(get_pairwise_comparisons))` is TRUE
**Fails now because**:
- `n_permutations` is not a formal parameter of `get_pairwise_comparisons()`. The `formals()` check will fail against current code.

### Test 4: test_that("add_relative_skill() accepts test_type, one_sided, n_permutations as explicit arguments", { ... })

**Reproduces**: the same API discoverability issue in `add_relative_skill()`
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `add_relative_skill(scores_quantile, test_type = NULL)` — should work without warning
- Call `add_relative_skill(scores_quantile, test_type = "permutation", one_sided = TRUE, n_permutations = 50)`
- Check that `"test_type" %in% names(formals(add_relative_skill))` is TRUE
- Check that `"one_sided" %in% names(formals(add_relative_skill))` is TRUE
- Check that `"n_permutations" %in% names(formals(add_relative_skill))` is TRUE
**Expected**:
- Both calls succeed without error
- All three `formals()` checks return TRUE
- `add_relative_skill(scores_quantile, test_type = NULL)` produces no warnings (the current test at line 570 already tests a similar scenario but passes `test_type` via `...`)
**Fails now because**:
- `test_type`, `one_sided`, `n_permutations` are not formal parameters of `add_relative_skill()`. The `formals()` checks will fail against current code.

### Test 5: test_that("get_pairwise_comparisons() has correct default values for promoted arguments", { ... })

**Reproduces**: ensures backward compatibility — defaults must match what `compare_forecasts()` currently specifies
**Setup**:
- Inspect `formals(get_pairwise_comparisons)` after the fix
**Action**:
- Check `formals(get_pairwise_comparisons)$test_type` — should include `"non_parametric"` and `"permutation"` with `"non_parametric"` as the first/default option (matching `compare_forecasts()` current default)
- Check `formals(get_pairwise_comparisons)$one_sided` — should default to `FALSE`
- Check `formals(get_pairwise_comparisons)$n_permutations` — should default to `999`
**Expected**:
- `eval(formals(get_pairwise_comparisons)$one_sided)` equals `FALSE`
- `eval(formals(get_pairwise_comparisons)$n_permutations)` equals `999`
- The default for `test_type` is a character vector whose first element is `"non_parametric"` (used by `match.arg()`)
**Fails now because**:
- These parameters do not exist in `formals(get_pairwise_comparisons)` at all, so the checks will error.

### Test 6: test_that("get_pairwise_comparisons() results are unchanged after argument promotion", { ... })

**Reproduces**: regression test — ensures numerical results are identical whether args were previously passed via `...` or now as explicit parameters
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
- Set seed for reproducibility: `set.seed(42)`
**Action**:
- Call `result_default <- get_pairwise_comparisons(scores_quantile)` (using default test_type)
- Call `result_null <- get_pairwise_comparisons(scores_quantile, test_type = NULL)` (skip p-values)
- Call `set.seed(42); result_perm <- get_pairwise_comparisons(scores_quantile, test_type = "permutation", n_permutations = 50)`
**Expected**:
- `result_default` has the same structure and column names as before the fix (columns: model, compare_against, mean_scores_ratio, pval, adj_pval, wis_relative_skill)
- `result_null$pval` is all `NA_real_`
- `result_null$adj_pval` is all `NA_real_`
- `result_perm$pval` contains finite numeric values (not NA)
- `result_default$mean_scores_ratio` and `result_default$wis_relative_skill` are unchanged (same values as before the refactor — the existing test at line 504-533 pins these values and can serve as a cross-check)
**Fails now because**:
- This test should actually PASS against both current and fixed code (it's a pure regression guard). The important aspect is that it continues to pass after the fix, confirming backward compatibility. Include it to ensure the refactor doesn't break numerical results.

### Test 7: test_that("pairwise_comparison_one_group() passes test args through to compare_forecasts()", { ... })

**Reproduces**: ensures the internal call chain correctly threads explicit arguments from `pairwise_comparison_one_group()` down to `compare_forecasts()`
**Setup**:
- Use pre-computed `scores_quantile` from `setup.R`
**Action**:
- Call `pairwise_comparison_one_group(scores_quantile, metric = "wis", baseline = NULL, compare = "model", by = character(0), test_type = NULL)` — should succeed and return p-values as NA
- Call `pairwise_comparison_one_group(scores_quantile, metric = "wis", baseline = NULL, compare = "model", by = character(0), test_type = "non_parametric")` — should succeed and return numeric p-values
**Expected**:
- When `test_type = NULL`: result's `pval` column is all `NA_real_`
- When `test_type = "non_parametric"`: result's `pval` column is all finite numeric
- `"test_type" %in% names(formals(pairwise_comparison_one_group))` is TRUE
**Fails now because**:
- `pairwise_comparison_one_group()` does not have `test_type` as a formal parameter — it currently uses `...` to pass through to `compare_forecasts()`. The `formals()` check will fail.
