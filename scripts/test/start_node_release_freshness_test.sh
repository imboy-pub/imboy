#!/usr/bin/env bash
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_ROOT="$(mktemp -d /tmp/imboy_start_node_freshness.XXXXXX)"
trap 'rm -rf -- "$TMP_ROOT"' EXIT

mkdir -p "$TMP_ROOT/scripts" "$TMP_ROOT/ebin" \
  "$TMP_ROOT/_rel/imboy/bin" "$TMP_ROOT/_rel/imboy/releases/1.2.3" \
  "$TMP_ROOT/_rel/imboy/lib/imboy-1.2.3/ebin" "$TMP_ROOT/mock-bin"
cp "$ROOT/scripts/start_node.sh" "$TMP_ROOT/scripts/"
printf 'fresh\n' >"$TMP_ROOT/ebin/sample.beam"
cp "$TMP_ROOT/ebin/sample.beam" "$TMP_ROOT/_rel/imboy/lib/imboy-1.2.3/ebin/"

cat >"$TMP_ROOT/_rel/imboy/bin/imboy" <<'SH'
#!/usr/bin/env bash
exit 0
SH
cat >"$TMP_ROOT/mock-bin/make" <<'SH'
#!/usr/bin/env bash
[ "$1" = "compile" ]
SH
chmod +x "$TMP_ROOT/_rel/imboy/bin/imboy" "$TMP_ROOT/mock-bin/make"

run_start() {
  PATH="$TMP_ROOT/mock-bin:$PATH" bash "$TMP_ROOT/scripts/start_node.sh" test imboycookie 9800 "" daemon \
    >/dev/null 2>&1
}

run_start || { echo "FAIL: 相同 beam 应允许启动"; exit 1; }
printf 'stale\n' >"$TMP_ROOT/_rel/imboy/lib/imboy-1.2.3/ebin/sample.beam"
if run_start; then echo "FAIL: 陈旧 beam 必须拒绝启动"; exit 1; fi
rm -f "$TMP_ROOT/_rel/imboy/lib/imboy-1.2.3/ebin/sample.beam"
if run_start; then echo "FAIL: release 缺失 beam 必须拒绝启动"; exit 1; fi

echo "PASS: start_node release beam freshness gate"
