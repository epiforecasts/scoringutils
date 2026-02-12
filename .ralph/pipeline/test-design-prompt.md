# Design Tests for Next Automatable Issue

Pick up the next automatable issue that doesn't have test cases yet, and write test specifications for it.

## Step 0: Verify worktree

Before doing anything else, verify you are in the correct git worktree:

1. Run `git rev-parse --show-toplevel` — it must end with `scoringutils-pipeline`
2. Run `git branch --show-current` — it must be `ralph`
3. Run `git fetch origin main && git merge origin/main --ff-only` to ensure you are up to date with main

If any check fails, stop and report the error. Do NOT proceed in the wrong worktree or on the wrong branch.

## Step 1: Find the next issue

1. Read `.ralph/ISSUE_TRIAGE.md`
2. Find the first row where:
   - **Automatable** is `yes`
   - **Has Tests** is `[ ]` (unchecked)
3. If no such issue exists:
   - **You MUST edit `.ralph/ISSUE_TRIAGE.md`** to set `- [x] **Test design**` in the Pipeline section (change `[ ]` to `[x]`). The bash loop that invokes you relies on this checkbox to know the stage is done — if you skip this edit, the loop will call you again indefinitely.
   - Report that all test design is complete, and stop.
4. Note the issue number, title, **Action Summary**, and **Status Note** from the triage table — these serve as the research summary for this issue.

## Step 2: Read the full issue context

- Run `gh issue view <#> --comments` to get the full GitHub issue with all comments
- Re-read the **Action Summary** and **Status Note** columns from the triage table — these contain the researched analysis of root cause and proposed fix
- Note which files/functions are mentioned and what the expected fix approach is

## Step 3: Understand the test landscape

- Examine existing tests in `tests/testthat/` to understand:
  - testthat patterns and conventions used in this project
  - Fixtures and helpers in `tests/testthat/helper-*.R` and `tests/testthat/setup.R`
  - How existing tests for related functionality are structured
  - What test data and example datasets are available (e.g., `example_quantile`, `example_binary`, etc.)
- Identify what is and isn't currently tested for the code in question

## Step 4: Design test cases

Design test cases following testthat conventions. For each test case, specify:

- **Name**: descriptive test name for `test_that("description", { ... })`
- **File**: which test file it should go in (existing file if appropriate, or new file following `test-<topic>.R` naming)
- **Setup**: what test data, fixtures, or mocking is needed
- **Action**: what function to call and with what inputs
- **Expected behavior**: what should happen after the fix (use `expect_equal()`, `expect_warning()`, `expect_error()`, `expect_true()`, etc.)
- **Why it fails now**: explain how the current code causes this test to fail

Test cases should:
1. **Reproduce the bug** — capture the exact failure described in the issue. This test should FAIL against current code and PASS after the fix.
2. **Cover edge cases** — variations that stress the same code path
3. **Prevent regression** — ensure the fix doesn't break existing behavior

## Step 5: Write test specs to issue file

Create `.ralph/issues/<#>-<slug>.md` (e.g., `.ralph/issues/1022-pairwise-two-models.md`) with this structure:

```markdown
# Issue #<number>: <title>

## Source

- **GitHub**: https://github.com/epiforecasts/scoringutils/issues/<number>
- **Action Summary**: <from triage table>
- **Status Note**: <from triage table>

## Test Specifications

**Designed**: YYYY-MM-DD
**Test file**: `tests/testthat/test-<topic>.R` (existing | new)
**Testing patterns observed**: <brief note on project conventions, e.g., "testthat + data.table fixtures, helper functions in helper-*.R">

### Test 1: test_that("<descriptive name>", { ... })

**Reproduces**: the core bug
**Setup**:
- <test data / fixture setup>
**Action**:
- Call `function_name(args)` with <specific inputs that trigger the bug>
**Expected**:
- <what should happen after the fix, using expect_* assertions>
**Fails now because**:
- <why current code produces wrong result>

### Test 2: test_that("<descriptive name>", { ... })

...
```

## Step 6: Update the triage table

In `.ralph/ISSUE_TRIAGE.md`, update the row for this issue:
- Set **Has Tests** to `[x]`

Then check: are all rows with **Automatable** `yes` and **Has Tests** not `N/A` now `[x]`? If yes, set the `- [x] **Test design**` pipeline checkpoint.

## Important

- Do NOT write any actual R code. Write test specifications in plain language in the issue markdown file only.
- Do NOT modify any other issue's row in the triage table.
- **The `.ralph/ISSUE_TRIAGE.md` table is a permanent ledger — NEVER delete or remove rows, and NEVER rewrite or reformat the table.** All rows must remain present at all times. Only make the specific cell updates described in the steps above for the single issue you are working on. Do not touch any other row for any reason.
- **NEVER delete issue markdown files.**
- Be precise about inputs, expected outputs, and setup. The next agent will translate these specs directly into code — ambiguity here becomes bugs later.
- If the Action Summary is too vague to design concrete tests, note what's missing in the issue file and move on to the next issue.
- Process one issue per invocation. The next run will pick up the next untested issue.
