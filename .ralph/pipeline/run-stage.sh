#!/usr/bin/env bash
# Run a single scoringutils pipeline stage in a loop.
#
# Usage:
#   ./run-stage.sh <prompt-file> <pipeline-checkbox-name> [max-iterations]
#
# Examples:
#   ./run-stage.sh test-design-prompt.md "Test design" 40
#   ./run-stage.sh implement-prompt.md Implement 40
#   ./run-stage.sh review-prompt.md Review 40
#   ./run-stage.sh summary-prompt.md Summary 1
#
# Full pipeline:
#   ./run-stage.sh test-design-prompt.md "Test design" 40 && \
#   ./run-stage.sh implement-prompt.md Implement 40 && \
#   ./run-stage.sh review-prompt.md Review 40 && \
#   ./run-stage.sh summary-prompt.md Summary 1
#
# Tips:
#   - Press Ctrl+C to stop cleanly.
#   - caffeinate -i ./run-stage.sh test-design-prompt.md "Test design" 40
#   - tmux new -s ralph 'caffeinate -i ./run-stage.sh test-design-prompt.md "Test design" 40'

set -euo pipefail

PROMPT_FILE="${1:?Usage: run-stage.sh <prompt-file> <pipeline-checkbox-name> [max-iterations]}"
CHECKPOINT_NAME="${2:?Usage: run-stage.sh <prompt-file> <pipeline-checkbox-name> [max-iterations]}"
MAX_ITERATIONS="${3:-20}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
PROMPT_PATH="$SCRIPT_DIR/$PROMPT_FILE"
INDEX_FILE="$REPO_DIR/.ralph/ISSUE_TRIAGE.md"
LOG_DIR="$SCRIPT_DIR/logs/$(date '+%Y%m%d-%H%M%S')-${PROMPT_FILE%.md}"

mkdir -p "$LOG_DIR"

# --- Worktree verification ---
TOPLEVEL="$(cd "$REPO_DIR" && git rev-parse --show-toplevel)"
if [[ "$TOPLEVEL" != *scoringutils-pipeline ]]; then
  echo "Error: Expected worktree ending with 'scoringutils-pipeline', got: $TOPLEVEL"
  echo "Run this script from the scoringutils-pipeline worktree."
  exit 1
fi

BRANCH="$(cd "$REPO_DIR" && git branch --show-current)"
if [[ "$BRANCH" != "ralph" ]]; then
  echo "Error: Expected branch 'ralph', got: $BRANCH"
  exit 1
fi

ITERATION=0
CONSECUTIVE_FAILURES=0
MAX_CONSECUTIVE_FAILURES=3
PREV_INDEX_HASH=""
NO_PROGRESS_COUNT=0
MAX_NO_PROGRESS=2

# Ctrl+C handler
cleanup() {
  trap - INT TERM
  echo ""
  echo ">>> Interrupted! Cleaning up..."
  pkill -P $$ 2>/dev/null || true
  sleep 1
  pkill -9 -P $$ 2>/dev/null || true
  wait 2>/dev/null
  exit 130
}
trap cleanup INT TERM

# Check if the pipeline checkpoint is done
is_stage_complete() {
  if [ ! -f "$INDEX_FILE" ]; then
    return 1
  fi
  grep -qE "^\- \[x\] \*\*${CHECKPOINT_NAME}\*\*" "$INDEX_FILE" 2>/dev/null
}

if [ ! -f "$PROMPT_PATH" ]; then
  echo "Error: Prompt file not found: $PROMPT_PATH"
  exit 1
fi

if [ ! -f "$INDEX_FILE" ]; then
  echo "Error: Index file not found: $INDEX_FILE"
  exit 1
fi

if ! command -v jq &> /dev/null; then
  echo "Warning: jq not found. Install with: brew install jq"
  STREAM=false
else
  STREAM=true
fi

echo "=== scoringutils Pipeline: $CHECKPOINT_NAME ==="
echo "Prompt:         $PROMPT_FILE"
echo "Max iterations: $MAX_ITERATIONS"
echo "Index file:     $INDEX_FILE"
echo "Logs:           $LOG_DIR/"
echo "Press Ctrl+C to stop."
echo "================================================"

# Check if already complete before starting
if is_stage_complete; then
  echo ">>> Stage '$CHECKPOINT_NAME' is already complete. Skipping."
  exit 0
fi

while true; do
  ITERATION=$((ITERATION + 1))
  LOG_FILE="$LOG_DIR/iteration-$(printf '%03d' $ITERATION).log"

  echo ""
  echo ">>> Iteration $ITERATION started ($(date '+%H:%M:%S'))"

  if [ "$ITERATION" -gt "$MAX_ITERATIONS" ]; then
    echo "Max iterations ($MAX_ITERATIONS) reached. Stopping."
    break
  fi

  # Run Claude with the prompt from the repo root
  EXIT_CODE=0
  if [ "$STREAM" = true ]; then
    (cd "$REPO_DIR" && claude -p "$(cat "$PROMPT_PATH")" \
      --dangerously-skip-permissions \
      --output-format stream-json \
      --verbose \
      2>/dev/null) \
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
    (cd "$REPO_DIR" && claude -p "$(cat "$PROMPT_PATH")" \
      --dangerously-skip-permissions) \
      > "$LOG_FILE" 2>&1 || EXIT_CODE=$?

    echo "    Last 5 lines:"
    tail -5 "$LOG_FILE" | sed 's/^/    /'
  fi

  # Check for errors
  FAILED=false
  RESULT_LINE=""
  if [ "$STREAM" = true ] && [ -f "$LOG_FILE.raw" ]; then
    RESULT_LINE=$(grep '"type":"result"' "$LOG_FILE.raw" | tail -1)
  fi

  if [ "$EXIT_CODE" -ne 0 ]; then
    FAILED=true
  elif [ -n "$RESULT_LINE" ]; then
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

    # Check for rate limit
    if [ -n "${RESULT_LINE:-}" ]; then
      ERROR_TEXT=$(echo "$RESULT_LINE" | jq -r '.result // empty' 2>/dev/null)
      if echo "${ERROR_TEXT:-}" | grep -iqE 'rate.?limit|token.*limit|too many requests|quota|overloaded|429'; then
        echo ">>> Rate limit detected. Waiting 5 minutes..."
        sleep 300
        CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES - 1))  # don't count rate limits
        continue
      fi
    fi

    if [ "$CONSECUTIVE_FAILURES" -ge "$MAX_CONSECUTIVE_FAILURES" ]; then
      echo ">>> $MAX_CONSECUTIVE_FAILURES consecutive failures. Stopping."
      break
    fi

    echo "    Failure $CONSECUTIVE_FAILURES/$MAX_CONSECUTIVE_FAILURES. Waiting ${WAIT_SECS}s..."
    sleep "$WAIT_SECS"
  else
    CONSECUTIVE_FAILURES=0
    echo ">>> Iteration $ITERATION complete ($(date '+%H:%M:%S'))"
  fi

  # Check if stage is now complete
  if is_stage_complete; then
    echo ">>> Stage '$CHECKPOINT_NAME' complete!"
    break
  fi

  # Detect spinning: if ISSUE_TRIAGE.md hasn't changed, agent made no progress
  CURR_INDEX_HASH=$(md5 -q "$INDEX_FILE" 2>/dev/null || md5sum "$INDEX_FILE" 2>/dev/null | cut -d' ' -f1 || echo "unknown")
  if [ -n "$PREV_INDEX_HASH" ] && [ "$CURR_INDEX_HASH" = "$PREV_INDEX_HASH" ]; then
    NO_PROGRESS_COUNT=$((NO_PROGRESS_COUNT + 1))
    echo "    Warning: ISSUE_TRIAGE.md unchanged ($NO_PROGRESS_COUNT/$MAX_NO_PROGRESS)"
    if [ "$NO_PROGRESS_COUNT" -ge "$MAX_NO_PROGRESS" ]; then
      echo ">>> No progress for $NO_PROGRESS_COUNT iterations — agent is not updating ISSUE_TRIAGE.md. Stopping."
      break
    fi
  else
    NO_PROGRESS_COUNT=0
  fi
  PREV_INDEX_HASH="$CURR_INDEX_HASH"
done

echo ""
echo "=== Stage '$CHECKPOINT_NAME' finished ==="
echo "Iterations: $ITERATION"
echo "Logs: $LOG_DIR/"
