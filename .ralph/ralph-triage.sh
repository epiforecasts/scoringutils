#!/usr/bin/env bash
# Ralph triage loop for scoringutils
# Usage:
#   .ralph/ralph-triage.sh          # unlimited iterations
#   .ralph/ralph-triage.sh 15       # max 15 iterations
#
# Tips:
#   - Press Ctrl+C to stop the loop cleanly.
#   - Run with caffeinate to prevent sleep:
#       caffeinate -i .ralph/ralph-triage.sh 25
#   - Run in tmux for resilience:
#       tmux new -s ralph 'caffeinate -i .ralph/ralph-triage.sh 25'

set -euo pipefail

MAX_ITERATIONS="${1:-0}"  # 0 = unlimited
ITERATION=0
CONSECUTIVE_FAILURES=0
MAX_CONSECUTIVE_FAILURES=3
RALPH_DIR="$(cd "$(dirname "$0")" && pwd)"
PROMPT_FILE="$RALPH_DIR/PROMPT_triage.md"
LOG_DIR="$RALPH_DIR/logs/run-$(date '+%Y%m%d-%H%M%S')"

mkdir -p "$LOG_DIR"

# Ctrl+C handler: kill all child processes and exit
cleanup() {
  echo ""
  echo ">>> Interrupted! Cleaning up..."
  kill 0 2>/dev/null
  exit 130
}
trap cleanup INT TERM

if [ ! -f "$PROMPT_FILE" ]; then
  echo "Error: $PROMPT_FILE not found"
  exit 1
fi

# Check for jq (needed for streaming)
if ! command -v jq &> /dev/null; then
  echo "Warning: jq not found. Install it for live streaming output."
  echo "  brew install jq"
  STREAM=false
else
  STREAM=true
fi

echo "=== Ralph Triage Loop ==="
echo "Max iterations: ${MAX_ITERATIONS:-unlimited}"
echo "Prompt: $PROMPT_FILE"
echo "Logs:   $LOG_DIR/"
echo "Stream: $STREAM"
echo "Press Ctrl+C to stop."
echo "========================="

while true; do
  ITERATION=$((ITERATION + 1))
  LOG_FILE="$LOG_DIR/iteration-$(printf '%03d' $ITERATION).log"

  echo ""
  echo ">>> Iteration $ITERATION started ($(date '+%H:%M:%S'))"

  # Check max iterations
  if [ "$MAX_ITERATIONS" -gt 0 ] && [ "$ITERATION" -gt "$MAX_ITERATIONS" ]; then
    echo "Max iterations ($MAX_ITERATIONS) reached. Stopping."
    break
  fi

  # Check for completion in the triage file
  if [ -f "$RALPH_DIR/ISSUE_TRIAGE.md" ] && grep -q "TRIAGE_COMPLETE" "$RALPH_DIR/ISSUE_TRIAGE.md"; then
    echo "Completion signal found. Triage complete!"
    break
  fi

  # Run Claude
  EXIT_CODE=0
  if [ "$STREAM" = true ]; then
    claude -p "$(cat "$PROMPT_FILE")" \
      --dangerously-skip-permissions \
      --output-format stream-json \
      --verbose \
      --include-partial-messages \
      2>/dev/null \
      | tee "$LOG_FILE.raw" \
      | jq --unbuffered -rj '
        if .type == "stream_event" then
          if .event.delta.type? == "text_delta" then
            .event.delta.text
          elif .event.type? == "content_block_start" and .event.content_block.type? == "tool_use" then
            "\n\n  > [" + .event.content_block.name + "] "
          elif .event.delta.type? == "input_json_delta" then
            .event.delta.partial_json
          elif .event.type? == "content_block_start" and .event.content_block.type? == "text" then
            "\n\n"
          else empty end
        else empty end' \
      || EXIT_CODE=$?
    echo ""
  else
    claude -p "$(cat "$PROMPT_FILE")" \
      --dangerously-skip-permissions \
      > "$LOG_FILE" 2>&1 || EXIT_CODE=$?

    echo "    Last 5 lines of output:"
    tail -5 "$LOG_FILE" | sed 's/^/    /'
  fi

  # Check for errors in the raw log
  FAILED=false
  if [ "$EXIT_CODE" -ne 0 ]; then
    FAILED=true
  elif [ "$STREAM" = true ] && [ -f "$LOG_FILE.raw" ]; then
    # Only check the final result line, not the entire log.
    # Tool results with "is_error":true are normal during Claude operation
    # (e.g. a grep that finds nothing exits with code 1).
    RESULT_LINE=$(grep '"type":"result"' "$LOG_FILE.raw" | tail -1)
    if echo "$RESULT_LINE" | grep -q '"is_error":true'; then
      FAILED=true
    elif echo "$RESULT_LINE" | grep -q '"subtype":"error"'; then
      FAILED=true
    fi
  fi

  if [ "$FAILED" = true ]; then
    CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
    WAIT_SECS=$((CONSECUTIVE_FAILURES * 60))

    echo ">>> Iteration $ITERATION FAILED ($(date '+%H:%M:%S'))"

    if [ "$CONSECUTIVE_FAILURES" -ge "$MAX_CONSECUTIVE_FAILURES" ]; then
      echo ">>> $MAX_CONSECUTIVE_FAILURES consecutive failures. Stopping."
      echo "    Check logs in $LOG_DIR/ for details."
      break
    fi

    echo "    Failure $CONSECUTIVE_FAILURES/$MAX_CONSECUTIVE_FAILURES. Waiting ${WAIT_SECS}s before retry..."
    sleep "$WAIT_SECS"
  else
    CONSECUTIVE_FAILURES=0
    echo ">>> Iteration $ITERATION complete ($(date '+%H:%M:%S'))"
  fi

  # Check for completion signal
  if [ -f "$RALPH_DIR/ISSUE_TRIAGE.md" ] && grep -q "TRIAGE_COMPLETE" "$RALPH_DIR/ISSUE_TRIAGE.md"; then
    echo "Completion signal found. Triage complete!"
    break
  fi
done

echo ""
echo "=== Ralph Triage Loop Finished ==="
echo "Total iterations: $ITERATION"
echo "Output: $RALPH_DIR/ISSUE_TRIAGE.md"
echo "Logs:   $LOG_DIR/"
