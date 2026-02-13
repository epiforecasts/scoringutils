# Implement Fix for Next Test-Designed Issue

Pick up the next automatable issue that hasn't been implemented yet. If it has test specifications, write the tests, verify they fail, implement the fix, and verify the tests pass. If the test-design stage marked the issue as not needing tests (Has Tests = N/A), skip test writing and implement the fix directly.

**Important**: The `.ralph/` files only exist on the `ralph` branch. When you switch to a feature branch (based on `origin/main`), they disappear from disk. You must read everything you need BEFORE switching branches, and update `.ralph/` files only AFTER switching back.

## Step 0: Verify worktree

Before doing anything else, verify you are in the correct git worktree:

1. Run `git rev-parse --show-toplevel` — it must end with `scoringutils-pipeline`
2. Run `git branch --show-current` — it must be `ralph`
3. Run `git fetch origin main && git merge origin/main --ff-only` to ensure you are up to date with main

If any check fails, stop and report the error. Do NOT proceed in the wrong worktree or on the wrong branch.

## Step 1: Find the next issue and read everything needed

**You are on the `ralph` branch. Read all `.ralph/` files now — they will be unavailable after the branch switch in Step 3.**

1. Read `.ralph/ISSUE_TRIAGE.md`
2. Find the first row where:
   - **Automatable** is `yes`
   - **Has Tests** is `[x]` or `N/A` (test-design stage has processed it)
   - **Implemented** is `[ ]` (unchecked — not `N/A`)
3. If no such issue exists:
   - Set the `- [x] **Implement**` pipeline checkpoint in `.ralph/ISSUE_TRIAGE.md` if not already set
   - Report that all implementations are complete, and stop
4. Read the corresponding issue file at `.ralph/issues/<#>-*.md`. If **Has Tests** is `[x]`, pay close attention to the **Test Specifications** section and **memorize the full test specs** — you won't be able to re-read this file after switching branches. If **Has Tests** is `N/A`, the issue needs no tests (e.g., vignette-only changes).
5. Note the **Action Summary** and **Status Note** from the triage table row
6. Run `gh issue view <#> --comments` for additional context

## Step 2: Save `.ralph/` state before branch switch

If there are any uncommitted changes to `.ralph/` files (from previous pipeline runs), commit them now so the branch switch is clean:

```bash
git add .ralph/
git diff --cached --quiet || git commit -m "Pipeline: save triage state before implementing #<number>"
```

This ensures `git checkout` won't refuse to switch due to uncommitted tracked files.

## Step 3: Create feature branch from `origin/main`

```bash
git fetch origin main
git checkout -b fix/<#>-<slug> origin/main
```

For example: `git checkout -b fix/1022-pairwise-two-models origin/main`

**After this point, `.ralph/` files are gone from disk.** Work from memory of what you read in Step 1.

## Step 4: Write the tests (skip if Has Tests = N/A)

**If the issue has Has Tests = `N/A`**, skip this step and Step 5 — go directly to Step 6.

**If the issue has Has Tests = `[x]`**, translate the test specifications (from Step 1) into actual R test code:
- Follow the file path, naming, and setup described in the specs
- Match the project's existing testthat conventions (use `test_that()`, `expect_*()` functions)
- Each test should map directly to a spec from the issue file
- Place tests in the specified file under `tests/testthat/`

## Step 5: Verify tests fail (skip if Has Tests = N/A)

Run the new tests and confirm they fail against the current code:

```bash
Rscript -e "devtools::test(filter='<test-file-name-without-test-prefix>')"
```

- If a test passes when it should fail, the test is wrong or the bug is already fixed — investigate
- If tests fail for reasons unrelated to the bug (missing packages, syntax errors), fix the test setup, not the application code

## Step 6: Implement the fix

Using the **Action Summary** from memory (Step 1) as a guide:
- Make the minimal code change needed to fix the issue
- Do not refactor surrounding code, add unrelated improvements, or change things beyond what's necessary
- Keep the change small and reviewable
- Follow existing code patterns and conventions in the R package

## Step 7: Verify tests pass and check for regressions

1. Run the specific tests again:
   ```bash
   Rscript -e "devtools::test(filter='<test-file-name>')"
   ```
2. Run the full test suite to check for regressions:
   ```bash
   Rscript -e "devtools::test()"
   ```
3. Run R CMD check:
   ```bash
   Rscript -e "devtools::check()"
   ```

- If new tests pass but existing tests break, revisit the fix
- If R CMD check produces new warnings or errors, fix them

## Step 8: Commit, push, and create PR

1. **Stage only code files** — source code (`R/`), tests (`tests/`), NAMESPACE, DESCRIPTION, `man/` if docs regenerated. Do NOT stage any `.ralph/` files (they don't exist on this branch anyway).
2. **Commit** with a clear message: `Fixes #<number>: <brief description>`
3. **Push** and create a PR:
   ```bash
   git push -u origin fix/<#>-<slug>
   gh pr create --title "Fix #<number>: <brief description>" --body "..."
   ```
   The PR body should include:
   - What issue this fixes (link to GitHub issue)
   - Brief description of root cause
   - What the fix does and why
   - Test coverage added
4. **Note the PR number** — you'll need it in Step 10.

## Step 9: Switch back to `ralph` branch

```bash
git checkout ralph
```

**After this, `.ralph/` files are back on disk.** You can now update them.

## Step 10: Update `.ralph/` files

Now that you're back on `ralph`:

1. **Update the issue file** at `.ralph/issues/<#>-*.md` — add a **Resolution** section:

```markdown
## Resolution

**Implemented**: YYYY-MM-DD
**Files changed**:
- `R/<file>.R` — <brief description of change>
- `tests/testthat/test-<topic>.R` — <tests added>

### What was changed
<Brief explanation of the fix and why it works>

### Test results
- `test_that("...")` — PASS
- `test_that("...")` — PASS
- Full test suite — PASS (N tests)
- R CMD check — 0 errors, 0 warnings, N notes
```

2. **Update the triage table** in `.ralph/ISSUE_TRIAGE.md`:
   - Set **Implemented** to `[x]`
   - Set **PR** to `#<number>` (the PR number from Step 8)

3. **Check completion**: are all rows with **Has Tests** `[x]` now also **Implemented** `[x]`? If yes, set the `- [x] **Implement**` pipeline checkpoint.

## Important

- Only change code that is directly related to the issue. No drive-by improvements.
- **Do NOT commit `.ralph/` files in the PR branch.** Only commit R package source code and test files.
- **The `.ralph/ISSUE_TRIAGE.md` table is a permanent ledger — NEVER delete or remove rows, and NEVER rewrite or reformat the table.** Only make the specific cell updates described above for the single issue you are implementing. Do not touch any other row for any reason.
- **NEVER delete issue markdown files.** They are the permanent record of the triage.
- If the test specs are unclear or insufficient, switch back to `ralph`, note what's missing in the issue file, and move on to the next issue.
- If the fix turns out to be more complex than expected (touching more than ~5 files or requiring architectural changes), switch back to `ralph`, document what you found, and move on to the next issue. Delete the feature branch with `git branch -D fix/<#>-<slug>`.
- Run tests in the affected modules, not just the new ones, to catch regressions.
- Process one issue per invocation. The next run will pick up the next unimplemented issue.
