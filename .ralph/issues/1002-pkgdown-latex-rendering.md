# Issue #1002: LaTeX not rendering in function reference

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/1002
- **Action Summary**: Add `math-rendering: katex` (or `mathjax`) under the `template:` key in `_pkgdown.yml` and rebuild the pkgdown site. One-line config change fixes all 63 LaTeX expressions across 4 documentation files.
- **Status Note**: Still relevant. pkgdown defaults to `mathml` which doesn't handle the 63 `\eqn{}`/`\deqn{}` occurrences in 4 metrics files. Upstream r-lib/pkgdown#2704 is closed (won't fix mathml); recommended fix is to use `mathjax` or `katex`. No `math-rendering` setting exists in `_pkgdown.yml`.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: `tests/testthat/test-pkgdown-config.R` (new)
**Testing patterns observed**: testthat with data.table fixtures; setup.R precomputes scores; no existing config/infrastructure tests.

### Note on testability

This issue is a **pure configuration change** — adding one line to `_pkgdown.yml`. There is no R function whose behavior changes. The effect is only visible when pkgdown builds the HTML reference site. Standard testthat unit tests cannot verify HTML rendering output.

The tests below verify the configuration is correct and that the underlying R documentation containing LaTeX is well-formed. These are regression-prevention tests — they ensure the config line is not accidentally removed in future edits.

### Test 1: test_that("_pkgdown.yml has math-rendering configured", { ... })

**Reproduces**: the root cause — missing `math-rendering` setting in pkgdown config
**Setup**:
- Read `_pkgdown.yml` using `yaml::read_yaml(system.file("../_pkgdown.yml", package = "scoringutils"))` or `yaml::read_yaml("../../_pkgdown.yml")` — adjust path to project root. Alternatively, use `testthat::test_path("../../_pkgdown.yml")` or `rprojroot::find_package_root_file("_pkgdown.yml")` to locate the file reliably.
**Action**:
- Parse the YAML and check `config$template$`math-rendering`` (note: the key contains a hyphen, so use backtick quoting or `[["math-rendering"]]`)
**Expected**:
- `expect_true("math-rendering" %in% names(config$template))` — the key exists
- `expect_true(config$template[["math-rendering"]] %in% c("katex", "mathjax"))` — value is one of the two working renderers
**Fails now because**:
- `_pkgdown.yml` currently has no `math-rendering` key under `template:`, so pkgdown defaults to `mathml` which fails to render `\eqn{}`/`\deqn{}` in function reference pages.

### Test 2: test_that("R documentation files with LaTeX formulas exist and are parseable", { ... })

**Reproduces**: a sanity check that the documentation files containing LaTeX are present
**Setup**:
- Identify the 4 known files with `\eqn{}`/`\deqn{}`: `R/metrics-binary.R`, `R/metrics-interval-range.R`, `R/metrics-sample.R`, `R/metrics-quantile.R`
**Action**:
- For each file, verify it exists using `file.exists()` and contains at least one `\eqn{` or `\deqn{` pattern using `any(grepl("\\\\(eqn|deqn)\\{", readLines(file)))`.
**Expected**:
- `expect_true(file.exists(file))` for each of the 4 files
- `expect_true(any(grepl(...)))` — confirming LaTeX documentation is present and hasn't been accidentally removed
**Fails now because**:
- This test would actually PASS now (the LaTeX is already present — it just doesn't render). This is a regression-prevention test ensuring the formulas remain in the docs. Combined with Test 1, these tests ensure the config + content are both in place.

## Resolution

**Implemented**: 2026-02-13
**Files changed**:
- `_pkgdown.yml` — Added `math-rendering: katex` under `template:` key
- `tests/testthat/test-pkgdown-config.R` — New test file with 2 regression-prevention tests

### What was changed
Added `math-rendering: katex` to the `template:` section of `_pkgdown.yml`. pkgdown defaults to `mathml` which cannot render the 63 `\eqn{}`/`\deqn{}` LaTeX expressions in function reference pages. The `katex` renderer handles these correctly. Used plain text matching (grep) instead of `yaml::read_yaml()` to avoid adding a test dependency. Both tests skip gracefully during R CMD check since `_pkgdown.yml` and R source files aren't available in the installed package tree.

### Test results
- `test_that("_pkgdown.yml has math-rendering configured")` — PASS
- `test_that("R documentation files with LaTeX formulas exist and are parseable")` — PASS
- Full test suite — PASS (688 tests)
- R CMD check — 0 errors, 0 warnings, 2 pre-existing notes
