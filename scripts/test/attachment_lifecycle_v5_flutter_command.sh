#!/usr/bin/env bash
# Argument construction only. This is not backend, device, or run ownership proof.
v5_w602_flutter_command() { # driver device
  local driver="${1:-}" device="${2:-}"
  local run_id="${W602_E2E_RUN_ID:-}" upload_id="${W602_E2E_UPLOAD_ID:-}"
  local uid="${W602_E2E_UID:-}" endpoint="${W602_BACKEND_URL:-}"
  if [[ ! "$driver" =~ ^(garage|disk)$ ]] || [[ -z "$device" ]] ||
     [[ ! "$run_id" =~ ^v5-e2e-[a-z0-9-]{1,64}$ ]] ||
     [[ ! "$upload_id" =~ ^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$ ]] ||
     [[ -z "$uid" || "$uid" =~ [[:cntrl:]] ]] ||
     [[ ! "$endpoint" =~ ^https?://127\.0\.0\.1(:[0-9]+)?(/[^[:space:]]*)?$ ]]; then
    echo 'BLOCKED_EXTERNAL: missing or invalid W6-02 Flutter inputs' >&2
    return 75
  fi
  # Each define is one argv item; do not print argv or identity values to logs.
  flutter test integration_test/attachment_lifecycle_v5_test.dart -d "$device" --reporter json \
    --dart-define=APP_ENV=local \
    --dart-define=V5_E2E_OPT_IN=true \
    "--dart-define=API_BASE_URL_OVERRIDE=$endpoint" \
    "--dart-define=V5_E2E_RUN_ID=$run_id" \
    "--dart-define=V5_E2E_UPLOAD_ID=$upload_id" \
    "--dart-define=V5_E2E_UID=$uid" \
    "--dart-define=V5_E2E_DRIVER=$driver"
}
