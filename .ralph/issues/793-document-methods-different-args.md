# Issue #793: How should we document methods that have different arguments than the default method?

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/793
- **Action Summary**: Remove shared `@rdname` tags for S3 methods whose arguments differ from the generic (`as_forecast_quantile.forecast_sample`, `as_forecast_point.forecast_quantile`, `get_pit_histogram.forecast_sample`) and give each its own documentation page with proper title and description.
- **Status Note**: Still relevant. Consensus reached (separate pages per @seabbs), but not implemented. Three generics currently lump methods with differing arguments onto one `@rdname` page: `as_forecast_quantile` (`.forecast_sample` has different args), `as_forecast_point` (`.forecast_quantile`), and `get_pit_histogram` (`.forecast_sample` has extra args). Renders a confusing flat argument list.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-class-forecast-quantile.R` (existing) and `tests/testthat/test-class-forecast-sample.R` (existing) and `tests/testthat/test-class-forecast-point.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures. Example datasets (`example_quantile`, `example_sample_continuous`, `example_sample_discrete`, `example_point`) pre-loaded. `setup.R` creates pre-computed scores. Tests use `expect_no_condition()`, `expect_error()`, `expect_identical()`, etc.

### Test 1: test_that("as_forecast_quantile.forecast_sample() accepts probs and type args", { ... })

**Reproduces**: Verifies the `.forecast_sample` method works correctly with its method-specific `probs` and `type` arguments, which differ from the `.default` method's `forecast_unit`, `observed`, `predicted`, `quantile_level` args.
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `example_sample_continuous` (already available as a `forecast_sample` object)
**Action**:
- Call `as_forecast_quantile(example_sample_continuous, probs = c(0.1, 0.5, 0.9), type = 7)`
**Expected**:
- Returns a `forecast_quantile` object (check with `is_forecast_quantile()`)
- The resulting object has exactly the quantile levels 0.1, 0.5, 0.9 (check `unique(result$quantile_level)`)
- No warnings or errors emitted (`expect_no_condition()`)
**Fails now because**:
- This test should PASS with current code. It serves as a regression test ensuring the `.forecast_sample` method's parameter interface remains intact after the `@rdname` change. The current code works; the issue is only about documentation structure.

### Test 2: test_that("as_forecast_point.forecast_quantile() extracts median without extra args", { ... })

**Reproduces**: Verifies the `.forecast_quantile` method works with only `data` and `...` (no `forecast_unit`, `observed`, `predicted` args from `.default`).
**File**: `tests/testthat/test-class-forecast-point.R`
**Setup**:
- Use `na.omit(example_quantile)` piped through `as_forecast_quantile()`
**Action**:
- Call `as_forecast_point(quantile_forecast)` with no extra arguments
**Expected**:
- Returns a `forecast_point` object (check with `is_forecast_point()`)
- The `predicted` column contains the median values (0.5 quantile) from the input
- The result has no `quantile_level` or `sample_id` column
- No warnings or errors emitted (`expect_no_condition()`)
**Fails now because**:
- This test should PASS with current code. It is a regression test confirming that the `.forecast_quantile` method's minimal interface (no extra column-renaming args) continues working after the `@rdname` separation.

### Test 3: test_that("get_pit_histogram.forecast_sample() accepts integers and n_replicates args", { ... })

**Reproduces**: Verifies the `.forecast_sample` method works with its extra `integers` and `n_replicates` parameters, which don't exist on the `.forecast_quantile` method.
**File**: `tests/testthat/test-class-forecast-sample.R`
**Setup**:
- Use `example_sample_discrete` (integer-valued sample forecast)
**Action**:
- Call `get_pit_histogram(example_sample_discrete, by = "model", integers = "random", n_replicates = 50)`
**Expected**:
- Returns a data.table with columns `model`, `density`, `bin`, `mid`
- No errors or unexpected warnings
- The `density` values are non-negative
**Fails now because**:
- This test should PASS with current code. It is a regression test ensuring the `.forecast_sample`-specific `integers` and `n_replicates` parameters remain functional after the `@rdname` split.

### Test 4: test_that("get_pit_histogram.forecast_quantile() does not accept integers arg", { ... })

**Reproduces**: Confirms that the `.forecast_quantile` method does NOT accept the `integers` parameter (which belongs only to `.forecast_sample`). This validates the parameter separation that the documentation fix aims to clarify.
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- Use `example_quantile` (a `forecast_quantile` object)
**Action**:
- Call `get_pit_histogram(example_quantile, by = "model", integers = "random")`
**Expected**:
- Either raises a warning about unused arguments or silently ignores the `integers` argument (since it's passed through `...`). The key point is that `integers` is NOT a formal parameter of `get_pit_histogram.forecast_quantile()`.
- Verify via `expect_true(!"integers" %in% names(formals(scoringutils:::get_pit_histogram.forecast_quantile)))` that `integers` is not a formal arg of the quantile method.
**Fails now because**:
- This test should PASS with current code. It validates the existing parameter separation at the code level, which the documentation change aims to make visible to users.

### Test 5: test_that("separate .Rd files exist for methods with different args after roxygenise", { ... })

**Reproduces**: The core documentation bug — after the fix, three additional `.Rd` files should exist for the methods with differing parameter signatures.
**File**: `tests/testthat/test-class-forecast-quantile.R` (or a new `test-documentation.R` if preferred)
**Setup**:
- Check the `man/` directory for `.Rd` files
**Action**:
- Check that `man/as_forecast_quantile.forecast_sample.Rd` exists (or an equivalent separate page)
- Check that `man/as_forecast_point.forecast_quantile.Rd` exists (or an equivalent separate page)
- Check that `man/get_pit_histogram.forecast_sample.Rd` exists (or an equivalent separate page)
**Expected**:
- All three `.Rd` files exist as separate documentation pages
- `expect_true(file.exists("man/as_forecast_quantile.forecast_sample.Rd"))` (or whatever naming convention the implementer chooses)
**Fails now because**:
- Currently all three methods share their generic's `.Rd` file. `as_forecast_quantile.forecast_sample` is documented in `man/as_forecast_quantile.Rd`, `as_forecast_point.forecast_quantile` in `man/as_forecast_point.Rd`, and `get_pit_histogram.forecast_sample` in `man/get_pit_histogram.Rd`. No separate `.Rd` files exist for these specific methods.

**Note**: This test may not follow typical project conventions (no existing documentation-structure tests exist in the test suite). The implementer should decide whether this test is worth including or if `R CMD check` provides sufficient validation that documentation is correct. The `.Rd` file names will depend on the implementer's choice of `@rdname` or `@name` tags.

### Test 6: test_that("formals differ between as_forecast_quantile methods", { ... })

**Reproduces**: Programmatically validates that the `.default` and `.forecast_sample` methods have different formal arguments, justifying separate documentation pages.
**File**: `tests/testthat/test-class-forecast-quantile.R`
**Setup**:
- None needed beyond access to the functions
**Action**:
- Get formal argument names for `scoringutils:::as_forecast_quantile.default` and `scoringutils:::as_forecast_quantile.forecast_sample`
- Compare them
**Expected**:
- `expect_true("probs" %in% names(formals(scoringutils:::as_forecast_quantile.forecast_sample)))` — the sample method has `probs`
- `expect_false("probs" %in% names(formals(scoringutils:::as_forecast_quantile.default)))` — the default method does NOT have `probs`
- `expect_true("quantile_level" %in% names(formals(scoringutils:::as_forecast_quantile.default)))` — the default method has `quantile_level`
- `expect_false("quantile_level" %in% names(formals(scoringutils:::as_forecast_quantile.forecast_sample)))` — the sample method does NOT have `quantile_level`
**Fails now because**:
- This test should PASS with current code — the formals already differ. It is a regression test ensuring the parameter signatures remain distinct, which is the entire reason for separating documentation pages.
