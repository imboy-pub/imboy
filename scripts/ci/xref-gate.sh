#!/usr/bin/env bash
# Fail closed on make/tee failures as well as undefined calls; no baseline bypass.
set -euo pipefail
log_file=${1:-xref.log}
set +e
IMBOYENV=local make xref 2>&1 | tee "$log_file"
statuses=("${PIPESTATUS[@]}")
set -e
if (( statuses[0] != 0 )); then
  printf 'ERROR: make xref failed (exit=%s)\n' "${statuses[0]}" >&2
  exit "${statuses[0]}"
fi
if (( statuses[1] != 0 )); then
  printf 'ERROR: xref evidence could not be written (tee exit=%s)\n' "${statuses[1]}" >&2
  exit "${statuses[1]}"
fi
if [[ ! -s "$log_file" ]]; then
  echo 'ERROR: xref produced no output; refusing empty evidence' >&2
  exit 1
fi
undef_count=$(awk '/Undefined function/ {n++} END {print n+0}' "$log_file")
printf 'Undefined function calls: %s\n' "$undef_count"
if (( undef_count != 0 )); then
  echo 'ERROR: xref found undefined calls' >&2
  exit 1
fi
echo 'XREF_GATE PASS make_exit=0 tee_exit=0 undefined_calls=0'
