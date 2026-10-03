#!/usr/bin/env bash
# Verify switches without starting any real service.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"
cat > "$TMP/bin/docker" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$CALL_LOG"
EOF
cat > "$TMP/bin/promtool" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
chmod +x "$TMP/bin/"*
export CALL_LOG="$TMP/calls" L4_MONITORING_RULES_DIR="$TMP/rules"
export PATH="$TMP/bin:$PATH"
CONTROL="$ROOT/deploy/l4-sni-monitoring/control.sh"
L4_MONITORING_ENABLED=false bash "$CONTROL" --check > "$TMP/check.log"
grep -q CONFIG_READY "$TMP/check.log"
! grep -q 'up -d' "$CALL_LOG"
if L4_MONITORING_ENABLED=false bash "$CONTROL" --start > "$TMP/start.log" 2>&1; then
  echo 'FAIL: disabled start accepted' >&2; exit 1
else
  test "$?" = 2
fi
grep -q DISABLED "$TMP/start.log"
! grep -q 'up -d' "$CALL_LOG"
L4_MONITORING_ENABLED=true bash "$CONTROL" --check > /dev/null
! grep -q 'up -d' "$CALL_LOG"
L4_MONITORING_ENABLED=true bash "$CONTROL" --start > /dev/null
test "$(grep -c 'up -d' "$CALL_LOG")" = 1
grep -q -- '--profile l4-sni-monitoring up -d' "$CALL_LOG"
test "$(grep -c '^  - name:' "$TMP/rules/l4-sni.yml")" = 1
grep -q 'name: imboy.l4_sni' "$TMP/rules/l4-sni.yml"
grep -q '127.0.0.1:19091' "$TMP/rules/l4-sni.yml"
! grep -q '127.0.0.1:9091' "$TMP/rules/l4-sni.yml"
L4_MONITORING_ENABLED=false bash "$CONTROL" --stop
grep -q -- '--profile l4-sni-monitoring stop' "$CALL_LOG"
! grep -q 'down\|--volumes\|-v$' "$CALL_LOG"
test "$(grep -c 'up -d' "$CALL_LOG")" = 1
cat > "$TMP/bin/docker" <<'EOF'
#!/usr/bin/env bash
exit 125
EOF
cat > "$TMP/bin/docker-compose" <<'EOF'
#!/usr/bin/env bash
printf 'legacy %s\n' "$*" >> "$CALL_LOG"
EOF
chmod +x "$TMP/bin/docker-compose"
L4_MONITORING_ENABLED=false bash "$CONTROL" --check > /dev/null
grep -q 'legacy .*config --quiet' "$CALL_LOG"
if L4_COMPOSE_BIN="$TMP/missing" bash "$CONTROL" --check > /dev/null 2>&1; then
  echo 'FAIL: explicit invalid Compose binary accepted' >&2; exit 1
else
  test "$?" = 2
fi
if L4_MONITORING_ENABLED=invalid bash "$CONTROL" --start > /dev/null 2>&1; then
  echo 'FAIL: invalid switch accepted' >&2; exit 1
else
  test "$?" = 2
fi
cat > "$TMP/bin/promtool" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF
if L4_MONITORING_ENABLED=true bash "$CONTROL" --start > /dev/null 2>&1; then
  echo 'FAIL: invalid rules accepted' >&2; exit 1
else
  test "$?" = 1
fi
test "$(grep -c 'up -d' "$CALL_LOG")" = 1
echo 'PASS: disabled/check never starts; enabled start explicit; validation failure blocks'
