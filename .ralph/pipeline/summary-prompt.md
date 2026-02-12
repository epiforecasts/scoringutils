# Generate Pipeline Summary for Human Review

Read through the triage table and produce a concise summary of the pipeline results, categorizing all issues by outcome.

## Step 0: Verify worktree

Before doing anything else, verify you are in the correct git worktree:

1. Run `git rev-parse --show-toplevel` — it must end with `scoringutils-pipeline`
2. Run `git branch --show-current` — it must be `ralph`
3. Run `git fetch origin main && git merge origin/main --ff-only` to ensure you are up to date with main

If any check fails, stop and report the error. Do NOT proceed in the wrong worktree or on the wrong branch.

## Step 1: Read the triage state

1. Read `.ralph/ISSUE_TRIAGE.md` to get the full status table
2. Read every individual issue file in `.ralph/issues/`

## Step 2: Categorize the issues by outcome

Sort all issues into the following buckets:

### Completed
Issues where **Implemented** is `[x]` AND **Reviewed** is `[x]`. For each, note:
- Issue number, title, and PR number
- One-line description of what was fixed

### In progress
Issues where some pipeline column is checked (`Has Tests = [x]` or `Implemented = [x]`) but not fully reviewed. For each, note:
- Issue number, title, and current stage
- What's left to do

### Closeable without code
Issues where **Automatable** is `yes` but all pipeline columns are `N/A` because the Action Summary says to close the issue (no code needed). For each, note:
- Issue number and title
- Why it can be closed (already resolved, superseded, etc.)

### Not automatable
Issues where **Automatable** is `no` or `partially` (skipped by pipeline). For each, note:
- Issue number, title, and why it's not automatable
- Whether it needs human design decisions, has blocking dependencies, or is too complex

### Not yet started
Issues where **Has Tests** is `[ ]` (automatable but not yet picked up). For each, note:
- Issue number and title
- Recommendation

## Step 3: Write the summary

Create or overwrite `.ralph/PIPELINE_SUMMARY.md` with this format:

```markdown
# scoringutils Pipeline Summary

**Generated**: YYYY-MM-DD
**Total issues**: N

## Completed (N)

| # | Title | PR | Summary |
|---|-------|----|---------|
| 1022 | Let get_pairwise_comparison() run with two models | #1234 | Changed cli_abort to cli_warn for 2-model case |

## In Progress (N)

| # | Title | Stage | Remaining |
|---|-------|-------|-----------|
| ... | ... | tests designed | Implementation + review |

## Closeable Without Code (N)

| # | Title | Reason |
|---|-------|--------|
| 909 | Compact version of get_duplicate_forecasts() | Already implemented via `counts` parameter |

## Not Automatable (N)

| # | Title | Automatable | Reason |
|---|-------|-------------|--------|
| 1064 | Pre-ranking for multivariate calibration | no | Requires design decisions on API approach |

## Not Yet Started (N)

| # | Title | Recommendation |
|---|-------|----------------|
| ... | ... | Worth pursuing next cycle |
```

## Step 4: Update pipeline checkpoint

Set `- [x] **Summary**` in the Pipeline section of `.ralph/ISSUE_TRIAGE.md`.

## Step 5: Print the summary

After writing the file, print the full contents of `.ralph/PIPELINE_SUMMARY.md` to the console so the human can see it immediately without opening a file.

## Important

- Do NOT make any code changes. This is a read-only reporting task.
- The only checkbox you may update is the **Summary** pipeline checkpoint in `.ralph/ISSUE_TRIAGE.md`.
- Be concise but specific. The human should be able to act on this summary without reading every issue file.
- For "In Progress" items, clearly state what stage they're at and what remains.
- Include accurate counts in each section header.
- If all automatable issues are completed with no blockers, say so clearly at the top.
