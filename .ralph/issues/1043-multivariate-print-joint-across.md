# Issue #1043: Update print method for multivariate forecasts to show `joint_across`

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/1043
- **Action Summary**: Add a `print.forecast_sample_multivariate()` method that displays `joint_across` columns. Requires either storing `joint_across` as an attribute during object construction or computing it as the set difference between forecast unit and `get_grouping()` output. Update test snapshots.
- **Status Note**: Still relevant. No `print.forecast_sample_multivariate` method exists; the generic `print.forecast` handles all types. `joint_across` is not stored as an attribute -- `set_grouping()` computes `.mv_group_id` but discards the original `joint_across` value.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-multivariate-sample.R` (existing)
**Testing patterns observed**: testthat 3 with `expect_snapshot()` for print output validation, data.table fixtures, `example_multivariate_sample` built-in dataset created with `joint_across = c("location", "location_name")`. The parent `print.forecast` outputs "Forecast type:" and "Forecast unit:" as `cli_text()` messages. Existing snapshot for multivariate print is in `_snaps/class-forecast-multivariate-sample.md`.

### Test 1: test_that("print.forecast_sample_multivariate() displays joint_across columns", { ... })

**Reproduces**: the core missing feature -- multivariate print output does not show which columns are jointly forecasted
**Setup**:
- Use `example_multivariate_sample` directly (already has `joint_across = c("location", "location_name")`)
**Action**:
- Call `print(example_multivariate_sample)` inside `expect_snapshot()`
**Expected**:
- The snapshot output should include a new line showing "Joint across:" (or similar label) followed by "location and location_name"
- The existing "Forecast type: sample_multivariate" and "Forecast unit:" lines should still be present
- The `joint_across` information should appear as a `cli_text()` message (like forecast type and forecast unit)
**Fails now because**:
- No `print.forecast_sample_multivariate()` S3 method exists. The generic `print.forecast()` is dispatched instead, which only prints forecast type and forecast unit -- it has no awareness of `joint_across` or the `.mv_group_id` grouping structure.

### Test 2: test_that("print.forecast_sample_multivariate() shows correct joint_across for single-column grouping", { ... })

**Reproduces**: edge case with a single `joint_across` column
**Setup**:
- Create a multivariate forecast from `example_sample_continuous` (after `na.omit()`) with `joint_across = "location"` only (not including `location_name`), specifying `forecast_unit = c("location", "model", "target_type", "target_end_date", "horizon")`
**Action**:
- Call `print(result)` inside `expect_snapshot()`
**Expected**:
- The snapshot output should show "Joint across:" followed by "location" (single column)
- Forecast type should still be "sample_multivariate"
- Forecast unit should be "location, model, target_type, target_end_date, and horizon"
**Fails now because**:
- Same as Test 1 -- no dedicated print method exists, so `joint_across` is never displayed regardless of how many columns it contains.

### Test 3: test_that("print.forecast_sample_multivariate() computes joint_across correctly from grouping", { ... })

**Reproduces**: verification that the `joint_across` columns are correctly computed as `setdiff(get_forecast_unit(x), get_grouping(x))` minus internal columns
**Setup**:
- Use `example_multivariate_sample` (created with `joint_across = c("location", "location_name")`)
**Action**:
- Capture the print output via `expect_snapshot()` and additionally verify programmatically:
  - Compute `grouping <- get_grouping(example_multivariate_sample)`
  - Compute `forecast_unit <- get_forecast_unit(example_multivariate_sample)`
  - Compute `joint_across <- setdiff(forecast_unit, grouping)`
  - Assert `joint_across` equals `c("location", "location_name")` (or equivalent set)
**Expected**:
- `expect_true(setequal(joint_across, c("location", "location_name")))` passes
- The print output's "Joint across:" line matches these computed columns
**Fails now because**:
- While the computation itself (`setdiff(get_forecast_unit(), get_grouping())`) already works correctly, the print method does not call it. This test validates both the computation and its integration into the print output. Currently, the print method does not show `joint_across` at all.

### Test 4: test_that("print.forecast_sample_multivariate() returns object invisibly", { ... })

**Reproduces**: standard print method contract (consistent with `print.forecast()`)
**Setup**:
- Use `example_multivariate_sample`
**Action**:
- Call `result <- print(example_multivariate_sample)` (capturing the return value)
**Expected**:
- `expect_identical(result, example_multivariate_sample)` -- the print method should return the input object invisibly, matching the behavior of `print.forecast()`
- The return should be invisible (verified by `expect_invisible(print(example_multivariate_sample))`)
**Fails now because**:
- Currently dispatches to `print.forecast()` which does return invisibly. This test ensures the new dedicated method maintains that contract. If the implementer forgets `return(invisible(x))`, this test will catch it.

### Test 5: test_that("print.forecast_sample_multivariate() still calls parent print.forecast()", { ... })

**Reproduces**: regression guard ensuring the new method extends (not replaces) the parent print
**Setup**:
- Use `example_multivariate_sample`
**Action**:
- Call `print(example_multivariate_sample)` inside `expect_snapshot()`
**Expected**:
- The output should contain ALL of: (1) "Forecast type: sample_multivariate", (2) "Forecast unit:" with the unit columns, (3) "Joint across:" with the `joint_across` columns, and (4) the data.table print output from `NextMethod()`.
- Order should be: forecast type, forecast unit, joint across, then the data table.
**Fails now because**:
- Without a dedicated `print.forecast_sample_multivariate()`, only (1), (2), and (4) appear. The "Joint across:" line (3) is missing entirely.

### Implementation notes for the next agent

The recommended implementation approach:
1. Create `print.forecast_sample_multivariate()` in `R/class-forecast-multivariate-sample.R`
2. Inside it, compute `joint_across` as `setdiff(get_forecast_unit(x), get_grouping(x))`
3. Call the parent `print.forecast()` via `NextMethod()` first (which prints type + unit + data), OR restructure to print type, unit, joint_across, then data
4. Add `cli_text(col_blue("Joint across:"))` and `cli_text("{joint_across}")` following the existing pattern
5. Return `invisible(x)`
6. Add `@export` and update NAMESPACE
7. Update the existing snapshot in `_snaps/class-forecast-multivariate-sample.md` to reflect the new "Joint across:" line

Alternative approach: Store `joint_across` as an attribute during `as_forecast_multivariate_sample.default()` (e.g., `attr(data, "joint_across") <- joint_across`) so the print method can retrieve it directly without recomputing. This is more robust but requires updating `set_grouping()` or the constructor.
