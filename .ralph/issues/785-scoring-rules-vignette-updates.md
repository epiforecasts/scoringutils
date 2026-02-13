# Issue #785: Small updates to the scoring rules Vignette

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/785
- **Action Summary**: Implement the 7 small documentation edits in `vignettes/scoring-rules.Rmd`: remove author field, replace Google Drive link with proper reference, add citations for proper scoring rules and transformed scores, improve `get_metrics` wording, clean whitespace, and add intro paragraph on probabilistic forecast types.
- **Status Note**: Still relevant. None of the 7 suggestions from @seabbs's review of PR #762 have been implemented: author field still present, Google Drive link still used, no citations for proper scoring rules or transformed scores paper, `get_metrics` wording not improved for website readers, no introductory paragraph on different probabilistic forecast types. Nearly 2 years open with no progress on these items.

## Test Specifications

**Designed**: 2026-02-13
**Test file**: N/A — no tests needed
**Reason**: This issue is purely documentation. All 7 changes are editorial edits to `vignettes/scoring-rules.Rmd` (removing author field, replacing links, adding citations, improving wording, adding introductory text). No R functions, classes, or behavior are modified. There is no testable code change.

The implementation agent should verify that the vignette renders without errors after edits by running `knitr::knit()` or `rmarkdown::render()` on the file, but no testthat tests are applicable.
