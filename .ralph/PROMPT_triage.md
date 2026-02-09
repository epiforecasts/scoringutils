# Triage All Open Issues for scoringutils

You are triaging all open GitHub issues for the `scoringutils` R package.
Your goal is to fill in the triage columns of the `## Issues` table in `.ralph/ISSUE_TRIAGE.md`.

## Instructions

1. Study `.ralph/ISSUE_TRIAGE.md` to see which issues still have empty triage columns (Category, Priority, etc.).
   Pick the next 10 untriaged issues (rows where these columns are empty), working top to bottom.

2. For each of these 10 issues — using subagents aggressively, where useful — do the following:
   a. Run `gh issue view --comments <number>` to read the full issue body and comments.
   b. Search the codebase to find and read the relevant code referenced in the issue.
      Study the package structure to understand everything you need: read `DESCRIPTION`, `NAMESPACE`,
      key files in `R/`, `tests/`, and `vignettes/`.
   c. Understand the current state — is the issue still relevant? Has it been partially addressed?
   d. Assess and categorize the issue.

3. Fill in the empty columns for each issue's row in the `## Issues` table in `.ralph/ISSUE_TRIAGE.md`:
   - **Category**: one of `bug`, `refactor`, `feature`, `documentation`, `discussion`, `meta`, `cleanup`, `enhancement`
   - **Priority**: `high`, `medium`, `low`
   - **Complexity**: `small` (< 1 hour), `medium` (1-4 hours), `large` (> 4 hours)
   - **Automatable**: The degree to which you'd feel confident in addressing this issue autonomously. `yes`, `partially` (needs human review after), or `no` (needs design decision)
   - **Status Note**: Is this still relevant? Is it partially done? Are there blockers?
   - **Dependencies**: Does it depend on or block other issues?
   - **Action Summary**: 1-2 sentences on what concretely needs to happen.

## Completion criteria

When ALL issues in the table (not only the ones you worked on!) have their triage columns filled in:

1. Add the text `TRIAGE_COMPLETE` to the very end of `.ralph/ISSUE_TRIAGE.md`
2. Then output the text `TRIAGE_COMPLETE` as your final message

## Important

- Triage exactly 10 issues, then stop. Do not try to do more.
- Do NOT implement any code changes. This is analysis only.
- Do NOT create branches or commits.
- Be honest about complexity — don't underestimate.
- If an issue is a discussion/question with no clear action, mark it as `discussion` and note that it needs maintainer input.
- Issues older than 1 year that reference code that no longer exists should be flagged as potentially stale.
- Read the actual code before assessing — don't guess whether something is already implemented.
