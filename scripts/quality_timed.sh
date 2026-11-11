#!/bin/sh
# Non-authoritative phase wall times; retain the original command exit code.
set -u
if [ "$#" -lt 2 ]; then
  echo "usage: sh scripts/quality_timed.sh PHASE COMMAND [ARGS...]" >&2
  exit 64
fi
phase=$1
shift
started=$(date +%s)
"$@"
rc=$?
finished=$(date +%s)
printf 'QUALITY_TIMING phase=%s start_epoch_s=%s end_epoch_s=%s elapsed_s=%s exit_code=%s\n' \
  "$phase" "$started" "$finished" "$((finished - started))" "$rc"
exit "$rc"
