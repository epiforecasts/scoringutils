# Review Next scoringutils Fix PR

Pick up the next issue that has a PR but hasn't been reviewed, and do a thorough independent code review.

## Step 0: Verify worktree

Before doing anything else, verify you are in the correct git worktree:

1. Run `git rev-parse --show-toplevel` — it must end with `scoringutils-pipeline`
2. Run `git branch --show-current` — it must be `ralph`
3. Run `git fetch origin main && git merge origin/main --ff-only` to ensure you are up to date with main

If any check fails, stop and report the error. Do NOT proceed in the wrong worktree or on the wrong branch.

## Step 1: Find the next issue

1. Read `.ralph/ISSUE_TRIAGE.md`
2. Find the first row where:
   - **Implemented** is `[x]`
   - **PR** has a number (not `—`)
   - **Reviewed** is `[ ]` (unchecked)
3. If no such issue exists:
   - **You MUST edit `.ralph/ISSUE_TRIAGE.md`** to set `- [x] **Review**` in the Pipeline section (change `[ ]` to `[x]`). The bash loop that invokes you relies on this checkbox to know the stage is done — if you skip this edit, the loop will call you again indefinitely.
   - Report that all reviews are complete, and stop.
4. Read the corresponding issue file at `.ralph/issues/<#>-*.md` — especially the **Test Specifications** and **Resolution** sections
5. Note the PR number from the triage table

## Step 2: Fetch the PR

- Use `gh pr view <number>` to get the PR description and metadata
- Use `gh pr diff <number>` to get the full diff
- To read changed files in their full context, use `git show origin/fix/<#>-<slug>:<filepath>` (e.g., `git show origin/fix/1022-pairwise-two-models:R/pairwise-comparisons.R`). **Do NOT use `git checkout`** — there may be untracked `.ralph/` files that conflict with the branch.

## Step 3: Review the code changes

Evaluate the diff against these criteria:

### Correctness
- Does the fix actually address the root cause described in the Action Summary?
- Could the fix introduce new bugs or edge cases?
- Are there code paths where the bug could still occur despite the fix?
- For R-specific concerns: does it handle NA values, empty inputs, edge cases in data.table operations?

### Test coverage
- Do the tests match the Test Specifications from the issue file?
- Do the tests actually verify the fix (not just pass coincidentally)?
- Are there edge cases in the specs that the tests miss?
- Do tests use appropriate testthat expectations (`expect_equal`, `expect_warning`, etc.)?

### Scope
- Are all changes directly related to the issue?
- Are there any unrelated modifications, drive-by refactors, or unnecessary additions?
- Does the change touch only the files expected from the Action Summary?

### Code quality
- Does the fix follow existing patterns and conventions in the R package?
- Is the change minimal and easy to understand?
- Does it follow tidyverse/data.table conventions used elsewhere in the codebase?
- Are roxygen comments updated if function signatures changed?

### R CMD check compliance
- Would the changes pass `R CMD check` without new warnings or notes?
- Are any new exports properly documented with roxygen?
- Are NAMESPACE and DESCRIPTION updated if needed?

### Regression risk
- Could this change break other callers of the modified code?
- Are there related code paths that should be checked?
- Do existing tests still cover the modified functions?

## Step 4: Write the review

Update the issue file at `.ralph/issues/<#>-*.md`. Add a **Review** section after the **Resolution** section:

```markdown
## Review

**Reviewed**: YYYY-MM-DD
**PR**: #<number>
**Verdict**: approve | request changes | needs human

### Summary
<1-2 sentence overall assessment>

### Correctness
- <findings>

### Test coverage
- <findings>

### Scope
- <findings>

### Issues found
- <list any problems, or "None">

### Test run results
- New tests: PASS/FAIL (details)
- Full test suite: PASS/FAIL (N tests, details)
```

## Step 5: Post review on GitHub

- If verdict is **request changes**: `gh pr review <number> --request-changes --body "CLAUDE: <summary of issues>"`
- If verdict is **needs human**: `gh pr review <number> --comment --body "CLAUDE: <explanation of what needs human judgement>"`
- If verdict is **approve**: `gh pr review <number> --approve --body "CLAUDE: <summary of approval>"`

Always prefix the review body with `CLAUDE: ` so it's clear this is an automated review.

## Step 6: Update the triage table

In `.ralph/ISSUE_TRIAGE.md`, update the row for this issue:
- Set **Reviewed** to `[x]`

Then check: are all rows with a PR number now also **Reviewed** `[x]`? If yes, set the `- [x] **Review**` pipeline checkpoint.

## Important

- Do NOT modify any code. This is a review-only task.
- **Do NOT use `git checkout`, `git stash`, or any destructive git commands.** Use `gh pr diff` and `git show` to read PR contents without switching branches.
- **The `.ralph/ISSUE_TRIAGE.md` table is a permanent ledger — NEVER delete or remove rows, and NEVER rewrite or reformat the table.** All rows must remain present at all times. Only make the specific cell updates described in the steps above for the single issue you are reviewing. Do not touch any other row for any reason.
- **NEVER delete issue markdown files.** They are the permanent record of the triage.
- Be skeptical. Do not assume the fix is correct just because tests pass — verify the logic independently.
- If the Test Specifications or Resolution sections seem wrong or incomplete, flag that in the review.
- If you find issues, be specific: reference exact lines, explain what's wrong, and suggest what should change.
- Review one PR per invocation. The next run will pick up the next unreviewed PR.
