#!/usr/bin/env bash
# Ralph triage loop for scoringutils
# Usage:
#   ./ralph-triage.sh          # unlimited iterations
#   ./ralph-triage.sh 15       # max 15 iterations

set -euo pipefail

MAX_ITERATIONS="${1:-0}"  # 0 = unlimited
ITERATION=0
PROMPT_FILE="$(dirname "$0")/PROMPT_triage.md"

if [ ! -f "$PROMPT_FILE" ]; then
  echo "Error: $PROMPT_FILE not found"
  exit 1
fi

echo "=== Ralph Triage Loop ==="
echo "Max iterations: ${MAX_ITERATIONS:-unlimited}"
echo "Prompt: $PROMPT_FILE"
echo "========================="

while true; do
  ITERATION=$((ITERATION + 1))
  echo ""
  echo ">>> Iteration $ITERATION ($(date '+%H:%M:%S'))"

  # Check max iterations
  if [ "$MAX_ITERATIONS" -gt 0 ] && [ "$ITERATION" -gt "$MAX_ITERATIONS" ]; then
    echo "Max iterations ($MAX_ITERATIONS) reached. Stopping."
    break
  fi

  # Check for completion in the output file
  if [ -f ".ralph/ISSUE_TRIAGE.md" ] && grep -q "TRIAGE_COMPLETE" ".ralph/ISSUE_TRIAGE.md"; then
    echo "Completion signal found in .ralph/ISSUE_TRIAGE.md. Triage complete!"
    break
  fi

  # Run Claude with the prompt, capture stdout to check for completion
  OUTPUT=$(cat "$PROMPT_FILE" | claude --dangerously-skip-permissions 2>&1) || true
  echo "$OUTPUT"

  # Check stdout for completion signal too
  if echo "$OUTPUT" | grep -q "TRIAGE_COMPLETE"; then
    echo "Completion signal found in Claude output. Triage complete!"
    break
  fi

  echo ">>> Iteration $ITERATION complete ($(date '+%H:%M:%S'))"
done

echo ""
echo "=== Ralph Triage Loop Finished ==="
echo "Total iterations: $ITERATION"
echo "Output: .ralph/ISSUE_TRIAGE.md"
