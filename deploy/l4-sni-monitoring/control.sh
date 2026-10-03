#!/usr/bin/env bash
# Prepare/check never starts containers. Start is a separate explicit operation.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ACTION="${1:---check}"
case "$ACTION" in --check|--start|--stop) ;; *) echo 'usage: control.sh --check|--start|--stop' >&2; exit 2;; esac
ENABLED="${L4_MONITORING_ENABLED:-false}"
case "$ENABLED" in true|false) ;; *) echo 'L4_MONITORING_ENABLED must be true or false' >&2; exit 2;; esac
RULE_DIR="${L4_MONITORING_RULES_DIR:-$ROOT/l4-sni-monitoring/generated}"
export L4_MONITORING_RULES_DIR="$RULE_DIR"
if [ -n "${L4_COMPOSE_BIN:-}" ]; then
  [ -x "$L4_COMPOSE_BIN" ] || { echo 'L4_COMPOSE_BIN is not executable' >&2; exit 2; }
  COMPOSE=("$L4_COMPOSE_BIN")
elif docker compose version >/dev/null 2>&1; then
  COMPOSE=(docker compose)
elif command -v docker-compose >/dev/null 2>&1; then
  COMPOSE=(docker-compose)
else
  echo 'BLOCKED_ENV: Docker Compose v2 required' >&2; exit 127
fi
COMPOSE+=(-f "$ROOT/docker-compose.l4-sni-monitoring.yml")
if [ "$ACTION" = --stop ]; then
  "${COMPOSE[@]}" --profile l4-sni-monitoring stop
  exit
fi
mkdir -p "$RULE_DIR"
# Extract the single group from the versioned authoritative rules; do not copy
# unrelated backend/PG rules into a host that has no exporters.
awk '
  BEGIN { print "groups:" }
  /^  - name:/ { active = ($0 == "  - name: imboy.l4_sni") }
  active { print }
' "$ROOT/prometheus/rules/imboy-alerts.yml" | \
  sed 's|127.0.0.1:9091|127.0.0.1:19091|g' > "$RULE_DIR/l4-sni.yml"
command -v promtool >/dev/null || { echo 'BLOCKED_ENV: promtool required' >&2; exit 127; }
promtool check rules "$RULE_DIR/l4-sni.yml"
TEMP_CONFIG="$(mktemp)"
trap 'rm -f "$TEMP_CONFIG"' EXIT
sed "s|/etc/prometheus/rules|$RULE_DIR|g" "$ROOT/l4-sni-monitoring/prometheus.yml" > "$TEMP_CONFIG"
promtool check config "$TEMP_CONFIG"
"${COMPOSE[@]}" --profile l4-sni-monitoring config --quiet
if [ "$ACTION" = --check ]; then
  echo "CONFIG_READY: enabled=$ENABLED; no containers started; external notification unconfigured"
  exit
fi
[ "$ENABLED" = true ] || { echo 'DISABLED: set L4_MONITORING_ENABLED=true after deployment approval' >&2; exit 2; }
"${COMPOSE[@]}" --profile l4-sni-monitoring up -d
echo 'STARTED: verify cron push, scrape, rules and receiver separately; not acceptance PASS'
