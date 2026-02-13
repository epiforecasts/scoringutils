# Issue #807: Clean up linter statements around `cli` conditions

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/807
- **Action Summary**: Replace all `"!" =`, `"i" =`, `"x" =`, `"v" =` with backtick-quoted equivalents (`` `!` = ``, etc.) in `cli_abort()`/`cli_warn()` calls across ~20 R/ files, then delete the corresponding `#nolint start`/`#nolint end` blocks. Handle `object_usage_linter` suppressions separately.
- **Status Note**: Still relevant. ~43 `#nolint start: keyword_quote_linter` blocks across R/ wrap `cli_abort()`/`cli_warn()`/`cli_inform()` calls that use `"!" =`, `"i" =` named vector syntax. Only 12 instances use Hugo's backtick fix (`` `!` = ``). The fix is mechanical: replace `"!" =` with `` `!` = `` inside cli condition calls, then remove the `nolint` annotations. Additional `object_usage_linter` suppressions (14 blocks) exist for `.data[...]` patterns in data.table code — these need separate handling.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-cli-linting.R` (new)
**Testing patterns observed**: testthat + data.table fixtures. Helpers in `setup.R`. Pre-computed scores objects. Tests use `test_that()` with descriptive names. No existing lint-related test file. CI runs `lintr::lint_package()` on changed files via GitHub Actions.

### Test 1: test_that("no R source files use double-quoted cli condition names", { ... })

**Reproduces**: the core problem — `"!" =`, `"i" =`, `"x" =`, `"v" =` patterns in cli calls trigger `keyword_quote_linter` and require `#nolint` suppressions
**Setup**:
- List all `.R` files in `R/` directory
- Read each file's contents as text
**Action**:
- Search all R source files for the regex pattern `"[!ixv]"\s*=` inside `c()` calls that are arguments to `cli_abort`, `cli_warn`, or `cli_inform`
- More practically: grep for lines matching the pattern `"[!ixv]"\s*=` across all `R/*.R` files
**Expected**:
- `expect_length(matches, 0)` — zero matches found. All cli condition names should use backtick quoting (`` `!` = ``, `` `i` = ``, etc.)
**Fails now because**:
- 53 occurrences of double-quoted cli condition names (`"!" =`, `"i" =`, `"x" =`, `"v" =`) exist across 13 R files

### Test 2: test_that("no keyword_quote_linter nolint annotations remain in R source", { ... })

**Reproduces**: the symptom — `#nolint` blocks are used to suppress `keyword_quote_linter` warnings
**Setup**:
- List all `.R` files in `R/` directory
- Read each file's contents as text
**Action**:
- Search all R source files for lines containing `keyword_quote_linter` in `#nolint` or `# nolint` annotations
**Expected**:
- `expect_length(matches, 0)` — zero `keyword_quote_linter` nolint annotations remain. All such blocks should have been removed (or simplified to only mention other linters like `object_usage_linter` where those are still needed).
**Fails now because**:
- ~28 `#nolint start: keyword_quote_linter` / `#nolint end` blocks exist across R/ files, many also mentioning `object_usage_linter` and `duplicate_argument_linter`

### Test 3: test_that("keyword_quote_linter is not disabled in .lintr config", { ... })

**Reproduces**: the root cause configuration that allowed the bad pattern to accumulate
**Setup**:
- Read the `.lintr` configuration file
**Action**:
- Parse or search the `.lintr` file contents for `keyword_quote_linter = NULL` (which disables the linter)
**Expected**:
- `expect_false(any(grepl("keyword_quote_linter\\s*=\\s*NULL", lintr_content)))` — the linter is no longer disabled, meaning `keyword_quote_linter` will run and catch any regressions
**Fails now because**:
- `.lintr` line 18 contains `keyword_quote_linter = NULL`, explicitly disabling this linter

### Test 4: test_that("cli error and warning messages still render correctly after quoting change", { ... })

**Reproduces**: regression guard — ensures the backtick-quoting change doesn't alter cli message behavior
**Setup**:
- Use existing test data (e.g., `example_quantile`, `example_binary`)
**Action**:
- Trigger a representative cli error by calling `assert_forecast.default(data.frame())` — this exercises the backtick-quoted `!` and `i` condition names
- Trigger another cli error by calling `as_forecast_quantile()` with data containing both `sample_id` and `quantile_level` columns — this exercises the error in `assert_forecast_generic()`
**Expected**:
- `expect_error(assert_forecast(data.frame()), "valid forecast object")` — the error message content is unchanged
- `expect_error(...)` with the appropriate message for the dual-column case — the error message content is unchanged
- Both `"!"` (error bullet) and `"i"` (info bullet) formatting elements render identically whether using `"!" =` or `` `!` = `` syntax
**Fails now because**:
- This test should PASS both before and after the fix (it's a regression guard). The backtick-quoting change is purely syntactic — cli treats `"!" =` and `` `!` = `` identically. This test ensures nothing breaks during the migration.

### Test 5: test_that("nolint blocks with only keyword_quote_linter are fully removed", { ... })

**Reproduces**: ensures cleanup is complete — no orphaned `#nolint end` blocks or unnecessary remaining suppressions
**Setup**:
- List all `.R` files in `R/` directory
- Read each file's contents
**Action**:
- Count `#nolint start` and `#nolint end` annotations across all R source files
- Compare with expected count (blocks that legitimately suppress `object_usage_linter` or `duplicate_argument_linter` should remain)
**Expected**:
- Every remaining `#nolint start` block should mention a linter other than `keyword_quote_linter` (e.g., `object_usage_linter`, `duplicate_argument_linter`)
- No `#nolint start` block should mention ONLY `keyword_quote_linter`
- The count of `#nolint start` blocks should be strictly less than the pre-fix count (~28), matching only the blocks that suppress other linters
**Fails now because**:
- Many `#nolint start` blocks exist solely to suppress `keyword_quote_linter`, and would be entirely unnecessary after the quoting fix. Some blocks suppress multiple linters and should be simplified (not removed).
