#!/usr/bin/env bash
# Accept complete measured leaves; a suite exit code cannot create leaf evidence.
set -euo pipefail
ROOT="${1:?repository required}"; CID="${2:?case required}"
CDIR="${3:?case directory required}"; CMD="${V5_GATE_COMMAND:-${4:?launched command required}}"
RUN_DIR="${V5_RUN_DIR:-${RUN_DIR:-}}"
case "$RUN_DIR" in /*) ;; *) echo 'explicit absolute run directory required' >&2; exit 2;; esac
exec python3 "$ROOT/scripts/attachment_lifecycle_v5_runner.py" seal-case \
  --run-dir "$RUN_DIR" --task-id "${CID%%/*}" --case-id "$CID" --case-dir "$CDIR" \
  --receipts "$CDIR/measured-receipts.json" --command "$CMD"
