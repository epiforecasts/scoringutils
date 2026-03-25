## PR Monitoring

After opening a PR (`gh pr create`) or pushing to a branch with an open PR (`git push`), you must:

### Immediate check
1. **CI failures** — Check `gh pr checks` for any failing checks. Fix straightforward failures directly. For complex or ambiguous failures, consider asking the user for guidance.

### Background monitoring loop
After the immediate check, start a background monitoring loop using `/loop 2m` that runs for 15 minutes (use the loop skill with a 2-minute interval). Each iteration should:
1. Run `gh pr checks` and check for newly failed checks
2. If new failures are found, fix straightforward issues directly. For complex or ambiguous issues, consider asking the user for guidance instead of attempting a fix.

### How to check
```bash
# Get PR number for current branch
gh pr view --json number -q .number

# Check CI status
gh pr checks <number>
```

If fixes are needed, commit them, push, and re-check.
