# Issue #811: Should `score()`/`apply_metrics()` return warning about failed metrics in a batch?

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/811
- **Action Summary**: Modify `apply_metrics()`/`run_safely()` to collect error messages in a list during the `lapply` loop, then emit a single batched `cli_warn()` summarizing all failed metrics at the end. Update test expectations in `test-score.R`.
- **Status Note**: Still relevant. `run_safely()` in `R/score.R:210` uses `try()` + immediate `cli_warn()` per failed metric. No batching/deferring mechanism exists. Failed metrics silently dropped from `scores` object via `as_scores()` filtering. Blog post on deferred errors referenced in issue. Only 1 comment from maintainer ("I like this idea").

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-score.R` (existing)
**Testing patterns observed**: testthat with data.table fixtures; `setup.R` pre-computes `scores_*` objects from `example_*` datasets; `expect_warning()`, `expect_no_condition()`, `suppressWarnings()` used extensively; `run_safely()` and `apply_metrics()` are internal (`:::`) but tested directly

### Test 1: test_that("apply_metrics() emits a single batched warning when multiple metrics fail", { ... })

**Reproduces**: the core issue — currently each failed metric emits its own separate warning eagerly during the `lapply` loop
**Setup**:
- Create a `data.table` with a single column `x = 1:10`
- Define two metrics that always error:
  - `fail1 = function(x) stop("error in fail1")`
  - `fail2 = function(x) stop("error in fail2")`
- Also include one metric that succeeds: `good = function(x) x + 1`
- Pass as `metrics = list("fail1" = fail1, "good" = good, "fail2" = fail2)`
**Action**:
- Call `scoringutils:::apply_metrics(forecast = dt, metrics = metrics, dt$x)`
**Expected**:
- Exactly ONE warning is emitted (not two separate warnings)
- The single warning message mentions both `fail1` and `fail2` metric names and their respective error messages
- The successful metric `good` column is still computed and present in the output data.table
- The failed metric columns (`fail1`, `fail2`) are NOT present as columns in the output
**Fails now because**:
- `run_safely()` calls `cli_warn()` immediately on each failure inside the `lapply` loop, producing two separate warnings instead of one batched warning

### Test 2: test_that("apply_metrics() emits no warning when all metrics succeed", { ... })

**Reproduces**: regression guard — ensures batching logic doesn't introduce spurious warnings when nothing fails
**Setup**:
- Create a `data.table` with column `x = 1:10`
- Define two succeeding metrics: `m1 = function(x) x + 1`, `m2 = function(x) x * 2`
**Action**:
- Call `scoringutils:::apply_metrics(forecast = dt, metrics = list("m1" = m1, "m2" = m2), dt$x)`
**Expected**:
- `expect_no_condition()` — no warning, message, or error is emitted
- Both metric columns are present and have correct values
**Fails now because**:
- This test should PASS on both current and fixed code (regression guard)

### Test 3: test_that("score() emits a single batched warning when a metric fails", { ... })

**Reproduces**: the user-facing manifestation of the bug via the public `score()` API
**Setup**:
- Use `example_sample_continuous` with only 1 sample (i.e., filter to `sample_id == 20` after `na.omit()`)
- This causes `log_score` to fail (needs >= 2 data points) and `dss` returns `NaN` (but doesn't error)
- Add a second intentionally failing custom metric: e.g., `always_fail = function(observed, predicted) stop("intentional")`
- Construct metrics list with defaults plus the custom failing metric
**Action**:
- Call `score(onesample, metrics = c(get_metrics(onesample), list("always_fail" = always_fail)))`
**Expected**:
- Exactly ONE warning is emitted that mentions both `log_score` and `always_fail` with their respective error messages
- The returned scores object does not contain `log_score` or `always_fail` columns
- Other metrics (like `crps`, `mad`, etc.) are still computed successfully
**Fails now because**:
- Current code emits two separate `cli_warn()` calls — one for `log_score` and one for `always_fail` — as each metric fails during the loop

### Test 4: test_that("run_safely() returns NULL and collects error instead of warning when metric fails", { ... })

**Reproduces**: the internal mechanism change — `run_safely()` should no longer emit a warning itself, instead it should return error info for batching
**Setup**:
- Define `f = function(x) stop("test error")`
**Action**:
- Call `scoringutils:::run_safely(1, fun = f, metric_name = "f")`
**Expected**:
- Returns `NULL` (same as current behavior)
- Does NOT emit a warning (changed behavior — the warning is now deferred to the caller)
- Note: The exact mechanism depends on implementation choice. If `run_safely()` still returns `NULL` but no longer warns, then `apply_metrics()` must collect error info via an alternative mechanism (e.g., `run_safely()` returning a structured error object, or `apply_metrics()` using `tryCatch` directly). The key assertion is: **no warning is emitted by `run_safely()` itself**.
**Fails now because**:
- `run_safely()` currently calls `cli_warn()` immediately at line 214 of `R/score.R` upon catching a `try-error`

### Test 5: test_that("batched warning message includes metric name and error details for each failure", { ... })

**Reproduces**: the content/format requirement for the batched warning
**Setup**:
- Create a `data.table` with column `x = 1:5`
- Define three metrics:
  - `bad_a = function(x) stop("missing argument foo")`
  - `bad_b = function(x) stop("division by zero")`
  - `ok = function(x) x`
**Action**:
- Call `scoringutils:::apply_metrics(forecast = dt, metrics = list("bad_a" = bad_a, "ok" = ok, "bad_b" = bad_b), dt$x)` and capture the warning
**Expected**:
- The warning message contains the string `bad_a` and `missing argument foo`
- The warning message contains the string `bad_b` and `division by zero`
- The warning message does NOT mention `ok`
- Only one warning condition is raised total
**Fails now because**:
- Current code emits separate `cli_warn()` per failure; the new code should emit a single warning containing a summary of all failures

### Test 6: test_that("existing one-sample score() test still passes with batched warnings", { ... })

**Reproduces**: backward compatibility — the existing test at test-score.R lines 90-102 must still pass, possibly with adjusted expectations
**Setup**:
- Same as existing test: `onesample <- na.omit(example_sample_continuous)[sample_id == 20]`
**Action**:
- Call `score(onesample)`
**Expected**:
- Exactly one warning is emitted (instead of the current one warning for `log_score`)
- The warning message mentions `log_score` and the error "need at least 2 data points"
- The result is a valid `scores` object without a `log_score` column
- With two samples (`sample_id %in% c(20, 21)`), `expect_no_condition()` still holds
**Fails now because**:
- This test currently passes because only one metric fails (so one warning = one warning). After the fix, it should still pass because a single failure still produces a single batched warning. The test assertion may need updating if the warning message format changes (e.g., from "Computation for `log_score` failed" to a batched format like "The following metrics failed: ...").
