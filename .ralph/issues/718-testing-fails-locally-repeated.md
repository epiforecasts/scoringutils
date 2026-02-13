# Issue #718: Testing fails locally when done repeatedly

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/718
- **Action Summary**: Add `rlang::reset_message_verbosity("small_interval_range")` before the `expect_warning()` call in `test-metrics-interval-range.R`. Audit the other 3 `.frequency = "once"` sites to ensure their tests are similarly resilient.
- **Status Note**: Still relevant and unfixed. Root cause: `cli_warn()` with `.frequency = "once"` and `.frequency_id = "small_interval_range"` suppresses warnings after first test run in same R session. 4 instances of `.frequency = "once"` in codebase. Tests pass on CI (fresh process) and on first local run.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-metrics-interval-range.R` (existing)
**Testing patterns observed**: testthat with top-level fixture vectors (`observed`, `lower`, `upper`, `interval_range`), `expect_no_condition()` for clean paths, `expect_warning()` for condition checks, `expect_error()` for validation failures. No helper files exist.

### Test 1: test_that("assert_input_interval() warns about small interval_range on repeated calls", { ... })

**Reproduces**: the core bug — `expect_warning()` fails on second `devtools::test()` run because `cli_warn(.frequency = "once")` suppresses the warning after its first firing in the same R session
**Setup**:
- Call `rlang::reset_message_verbosity("small_interval_range")` at the start of the test block to reset the one-time warning gate
- Use existing top-level fixture `observed` and `lower`/`upper` vectors already defined at lines 1-5 of the test file
**Action**:
- Call `assert_input_interval(observed, lower, upper, 0.5)` — passing `interval_range = 0.5` which is in the `(0, 1)` range that triggers the warning
**Expected**:
- `expect_warning(assert_input_interval(observed, lower, upper, 0.5), "Found interval ranges between 0 and 1")` should pass
- This must pass even when the test suite has been run previously in the same R session (the whole point of the fix)
**Fails now because**:
- `cli_warn()` at line 42-52 of `R/metrics-interval-range.R` uses `.frequency = "once"` and `.frequency_id = "small_interval_range"`. After the first invocation in an R session, `rlang` records that this frequency ID has fired and suppresses all subsequent warnings with that ID. On the second `devtools::test()` run in the same R session, the warning is suppressed, so `expect_warning()` fails with "did not throw the expected warning."

### Note: This is a modification of the existing test, not a new test

The existing test at lines 22-27 of `test-metrics-interval-range.R` already tests this exact warning:

```r
# expect warning if interval range is < 1
expect_warning(
  assert_input_interval(observed, lower, upper, 0.5),
  "Found interval ranges between 0 and 1. Are you sure that's right?"
)
```

The fix is to add a single line before this `expect_warning()` call:

```r
rlang::reset_message_verbosity("small_interval_range")
```

This resets rlang's internal record of whether the `"small_interval_range"` frequency ID has already fired, ensuring the warning fires fresh every time the test runs regardless of session state.

### Test 2: test_that("assert_input_interval() warning is idempotent within same test block", { ... })

**Reproduces**: edge case — verifies the warning fires reliably even if `assert_input_interval` with a small range has already been called earlier in the same test file or block
**Setup**:
- Call `rlang::reset_message_verbosity("small_interval_range")`
- Call `assert_input_interval(observed, lower, upper, 0.5)` once (to trigger and consume the one-time warning) using `suppressWarnings()` to discard it
- Then call `rlang::reset_message_verbosity("small_interval_range")` again to reset
**Action**:
- Call `assert_input_interval(observed, lower, upper, 0.5)` a second time
**Expected**:
- `expect_warning(..., "Found interval ranges between 0 and 1")` should pass on the second call
- This confirms that `reset_message_verbosity()` properly re-enables the warning even after it has already been consumed within the same test block
**Fails now because**:
- Without the reset, the second call would not produce a warning because `.frequency = "once"` means the warning is only emitted once per R session per frequency ID. This test validates that the fix mechanism itself works correctly.

### Design Note: Auditing other `.frequency = "once"` sites

The triage note mentions "4 instances of `.frequency = "once"` in codebase" but a grep of the current R source reveals only **1 instance** — the `"small_interval_range"` case in `R/metrics-interval-range.R:50`. The other 3 mentioned instances no longer exist in the current codebase. No other tests need similar treatment.
