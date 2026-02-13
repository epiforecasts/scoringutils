# Issue #492: Expose `get_range_from_quantile()` to the user

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/492
- **Action Summary**: Rename `get_range_from_quantile()` to `get_interval_range()`, export it, and create a new exported `add_interval_range()` that adds an `interval_range` column to a quantile-format data.table. Update 3 internal call sites, NAMESPACE, docs, and tests.
- **Status Note**: Still relevant. Function exists at `R/helper-quantile-interval-range.R:185` as `@keywords internal`, not exported. Maintainer consensus reached: rename to `get_interval_range()` and create `add_interval_range()` wrapper. Neither implemented. Original motivating function `plot_ranges()` was deleted, reducing urgency. 3 internal call sites.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-helper-quantile-interval-range.R` (existing)
**Testing patterns observed**: testthat 3e, data.table fixtures, `expect_identical`/`expect_equal`/`expect_no_condition`/`expect_error` assertions, `example_quantile` built-in dataset used frequently for integration-level tests, `:::` used for internal functions in existing tests

### Test 1: test_that("get_interval_range() returns correct interval ranges for standard quantiles", { ... })

**Reproduces**: Verifies the renamed function works correctly
**Setup**:
- No special setup needed; pure function test
**Action**:
- Call `get_interval_range(c(0.05, 0.25, 0.5, 0.75, 0.95))` (the renamed version of `get_range_from_quantile`)
**Expected**:
- `expect_identical(result, c(90, 50, 0, 50, 90))` — symmetric quantiles around 0.5 should map to the same interval range; the median (0.5) maps to range 0
**Fails now because**:
- `get_interval_range()` does not exist yet. The function is still named `get_range_from_quantile()` and is not exported.

### Test 2: test_that("get_interval_range() handles edge cases correctly", { ... })

**Reproduces**: Edge case coverage for the renamed function
**Setup**:
- No special setup needed
**Action**:
- Call `get_interval_range(c(0, 1))` — extreme quantile levels
- Call `get_interval_range(0.5)` — median only
- Call `get_interval_range(c(0.1, 0.9))` — single symmetric pair
**Expected**:
- `expect_identical(get_interval_range(c(0, 1)), c(100, 100))` — 0 and 1 quantiles both correspond to 100% interval range
- `expect_identical(get_interval_range(0.5), 0)` — median corresponds to 0% interval range
- `expect_identical(get_interval_range(c(0.1, 0.9)), c(80, 80))` — symmetric pair maps to same range
**Fails now because**:
- `get_interval_range()` does not exist yet.

### Test 3: test_that("get_interval_range() is exported and accessible without :::", { ... })

**Reproduces**: The core issue — function should be user-accessible
**Setup**:
- No special setup needed
**Action**:
- Call `get_interval_range(0.25)` without using `scoringutils:::` prefix
**Expected**:
- `expect_no_error(get_interval_range(0.25))` — the function should be directly accessible
- `expect_identical(get_interval_range(0.25), 50)` — and return correct result
**Fails now because**:
- The function is named `get_range_from_quantile`, is marked `@keywords internal`, and is not in NAMESPACE. Calling `get_interval_range()` would error with "could not find function".

### Test 4: test_that("add_interval_range() adds interval_range column to quantile forecast", { ... })

**Reproduces**: The new `add_interval_range()` wrapper function
**Setup**:
- Create a small quantile-format data.table: `dt <- data.table::data.table(observed = 5, predicted = c(1, 3, 5, 7, 9), quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95))`
**Action**:
- Call `result <- add_interval_range(dt)`
**Expected**:
- `expect_true("interval_range" %in% colnames(result))` — new column exists
- `expect_identical(result$interval_range, c(90, 50, 0, 50, 90))` — correct interval ranges computed from quantile levels
- `expect_identical(ncol(result), ncol(dt) + 1L)` — exactly one new column added
- All original columns and data preserved (no rows dropped, no values changed)
**Fails now because**:
- `add_interval_range()` does not exist yet.

### Test 5: test_that("add_interval_range() works with example_quantile dataset", { ... })

**Reproduces**: Integration test with built-in example data
**Setup**:
- Use the built-in `example_quantile` dataset (after `na.omit`)
**Action**:
- Call `result <- add_interval_range(na.omit(example_quantile))`
**Expected**:
- `expect_true("interval_range" %in% colnames(result))` — column added
- `expect_identical(nrow(result), nrow(na.omit(example_quantile)))` — no rows dropped
- All original columns preserved
- The `interval_range` values should match `get_interval_range(result$quantile_level)` — internal consistency check
**Fails now because**:
- `add_interval_range()` does not exist yet.

### Test 6: test_that("add_interval_range() modifies input in place (data.table semantics)", { ... })

**Reproduces**: Checks whether the function follows data.table in-place modification convention (as is common with `add_*` functions in data.table-based packages)
**Setup**:
- `dt <- data.table::data.table(observed = 5, predicted = c(1, 3, 5, 7, 9), quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95))`
**Action**:
- Call `result <- add_interval_range(dt)`
**Expected**:
- Either: `expect_true("interval_range" %in% colnames(dt))` (if modifying in place by reference, as is typical for `add_*` data.table patterns)
- Or: the function returns a new copy with the column added while leaving `dt` unchanged (if copy semantics chosen)
- The implementing agent should verify which convention the maintainers chose and write the assertion accordingly. The `add_relative_skill()` function in `R/pairwise-comparisons.R` provides a precedent — check its semantics.
**Fails now because**:
- `add_interval_range()` does not exist yet.

### Test 7: test_that("add_interval_range() errors gracefully on non-quantile input", { ... })

**Reproduces**: Input validation for the new function
**Setup**:
- Create a data.table without a `quantile_level` column: `dt <- data.table::data.table(observed = 1:5, predicted = 2:6, sample_id = 1:5)`
**Action**:
- Call `add_interval_range(dt)`
**Expected**:
- `expect_error(add_interval_range(dt))` — should error because `quantile_level` column is missing
**Fails now because**:
- `add_interval_range()` does not exist yet.

### Test 8: test_that("internal call sites work correctly after rename", { ... })

**Reproduces**: Regression test ensuring the 3 internal call sites still work after rename
**Setup**:
- Use `example_quantile` dataset
**Action**:
- Call `get_coverage(example_quantile)` — which internally uses `get_range_from_quantile` (to be renamed)
- Call `score(example_quantile)` — which internally calls `quantile_to_interval_dataframe` → uses the function
**Expected**:
- `expect_no_error(get_coverage(example_quantile))` — coverage computation still works
- `expect_no_error(score(example_quantile))` — scoring still works end-to-end
- Results should be identical to pre-rename results (the rename is purely cosmetic — the logic is unchanged)
**Fails now because**:
- This test should PASS both before and after the fix. It is a regression guard to ensure the rename doesn't break internal callers. The existing test suite already covers this implicitly via `test-get-coverage.R` and `test-score.R`, but an explicit check is useful during the transition.
