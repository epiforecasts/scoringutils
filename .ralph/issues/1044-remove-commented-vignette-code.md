# Issue #1044: Remove the commented out vignette code.

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/1044
- **Action Summary**: Delete the two HTML-commented-out code blocks from `vignettes/scoring-multivariate-forecasts.Rmd` and `vignettes/scoring-rules.Rmd`. Optionally confirm with maintainers whether the PIT/Calibration text should be preserved elsewhere.
- **Status Note**: Still relevant. Two HTML comment blocks remain: a WIP "Details on the grouping" section in `scoring-multivariate-forecasts.Rmd` (lines 88-102) and a ~50-line PIT/Calibration section in `scoring-rules.Rmd` (lines 444-493). Originated from PR #998 review comment.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: N/A
**Testing patterns observed**: N/A

### No tests required

This issue involves only deleting HTML comment blocks from two vignette `.Rmd` files. There are:

- **No R code changes** — no functions are added, modified, or removed
- **No behavior changes** — the commented-out blocks are invisible in rendered output
- **No testable code paths** — vignette content is not covered by `testthat` tests

The fix is purely mechanical: delete lines 88-100 from `vignettes/scoring-multivariate-forecasts.Rmd` and lines 444-493 from `vignettes/scoring-rules.Rmd`.

**Verification approach**: The implementing agent should confirm that both vignettes still render successfully after the deletions (e.g., via `rmarkdown::render()`), but this is a build check, not a unit test.

## Resolution

**Implemented**: 2026-02-13
**Files changed**:
- `vignettes/scoring-multivariate-forecasts.Rmd` — deleted WIP "Details on the grouping" HTML comment block (lines 88-100)
- `vignettes/scoring-rules.Rmd` — deleted ~50-line PIT/Calibration HTML comment block (lines 444-493)

### What was changed
Removed both HTML comment blocks that were flagged in the PR #998 review. These blocks were invisible in rendered output and contained only WIP/draft content that is no longer needed. No R code or rendered vignette output was affected.

### Test results
- No unit tests required (vignette-only change)
- Vignettes rebuild successfully
- R CMD check — 0 errors, 0 warnings, 2 notes (pre-existing)
