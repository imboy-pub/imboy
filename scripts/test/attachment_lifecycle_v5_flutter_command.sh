#!/usr/bin/env bash
# Argument construction only. This is not backend, device, or run ownership proof.
v5_w602_flutter_command() { # driver device [phase] [scenario]
  local driver="${1:-}" device="${2:-}" phase="${3:-}" scenario="${4:-}"
  local run_id="${W602_E2E_RUN_ID:-}" upload_id="${W602_E2E_UPLOAD_ID:-}"
  local uid="${W602_E2E_UID:-}" endpoint="${W602_BACKEND_URL:-}"
  if [[ ! "$driver" =~ ^(garage|disk)$ ]] || [[ -z "$device" ]] ||
     [[ ! "$run_id" =~ ^v5-e2e-[a-z0-9-]{1,64}$ ]] ||
     [[ ! "$upload_id" =~ ^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$ ]] ||
     [[ -z "$uid" || "$uid" =~ [[:cntrl:]] ]] ||
     [[ ! "$endpoint" =~ ^https?://127\.0\.0\.1(:[0-9]+)?(/[^[:space:]]*)?$ ]] ||
     [[ -n "$phase" && ! "$phase" =~ ^(put|restore)$ ]] ||
     [[ -n "$scenario" && ! "$scenario" =~ ^[a-z][a-z0-9_]{0,63}$ ]]; then
    echo 'BLOCKED_EXTERNAL: missing or invalid W6-02 Flutter inputs' >&2
    return 75
  fi
  # Each define is one argv item; do not print argv or identity values to logs.
  # An empty phase keeps the frozen seven-define invocation unchanged; the
  # two-phase orchestration passes put|restore as the eighth define. The
  # scenario suite appends V5_E2E_SCENARIO=<contract leaf name> as the ninth;
  # the kill_after_put leaf keeps the frozen no-scenario argv (its protocol is
  # the empty-scenario two-phase flow registered as 'kill_after_put').
  set -- test integration_test/attachment_lifecycle_v5_test.dart -d "$device" --reporter json \
    --dart-define=APP_ENV=local \
    --dart-define=V5_E2E_OPT_IN=true \
    "--dart-define=API_BASE_URL_OVERRIDE=$endpoint" \
    "--dart-define=V5_E2E_RUN_ID=$run_id" \
    "--dart-define=V5_E2E_UPLOAD_ID=$upload_id" \
    "--dart-define=V5_E2E_UID=$uid" \
    "--dart-define=V5_E2E_DRIVER=$driver"
  if [ -n "$phase" ]; then
    set -- "$@" "--dart-define=V5_E2E_PHASE=$phase"
  fi
  if [ -n "$scenario" ]; then
    set -- "$@" "--dart-define=V5_E2E_SCENARIO=$scenario"
  fi
  flutter "$@"
}
