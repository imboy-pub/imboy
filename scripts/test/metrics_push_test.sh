#!/usr/bin/env bash
# ============================================================
# scripts/lib/metrics_push.sh 的 mock 测试
# ------------------------------------------------------------
# 断言 payload 内容与推送行为，不依赖真实 Pushgateway。
# 运行 / Run:  bash scripts/test/metrics_push_test.sh
# ============================================================
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PASS=0
FAIL=0

ok()   { PASS=$((PASS+1)); echo "  ✓ $1"; }
bad()  { FAIL=$((FAIL+1)); echo "  ✗ $1"; }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1 (期望 '$3'，实际 '$2')"; fi; }
contains() {
  if echo "$2" | grep -q -- "$3"; then ok "$1"; else bad "$1 (未包含 '$3')"; fi
}
not_contains() {
  if echo "$2" | grep -q -- "$3"; then bad "$1 (不应包含 '$3')"; else ok "$1"; fi
}

# shellcheck source=scripts/lib/metrics_push.sh
. "${ROOT}/scripts/lib/metrics_push.sh"

echo "== build_backup_payload：成功路径 =="
NOW="$(date -u +%s)"
OUT_OK="$(build_backup_payload pg 1 "$((NOW - 42))" 12345)"
contains "含 last_status=1"            "$OUT_OK" "imboy_backup_last_status 1"
contains "含 last_success_timestamp"   "$OUT_OK" "imboy_backup_last_success_timestamp"
contains "含 size_bytes"               "$OUT_OK" "imboy_backup_size_bytes 12345"
contains "duration 由 start_ts 推算"    "$OUT_OK" "imboy_backup_last_duration_seconds 4"

echo "== build_backup_payload：失败路径 =="
OUT_FAIL="$(build_backup_payload pg 0 "$NOW" 0)"
contains     "含 last_status=0"                    "$OUT_FAIL" "imboy_backup_last_status 0"
# 失败时不能刷新成功时间戳，否则 IMBoyBackupNotRunning 永远不会触发
not_contains "失败不刷新 last_success_timestamp"    "$OUT_FAIL" "imboy_backup_last_success_timestamp"

echo "== build_tls_payload =="
TLS_OUT="$(build_tls_payload 1800000000)"
contains "含证书到期时间戳" "$TLS_OUT" "imboy_tls_cert_expiry_timestamp 1800000000"

echo "== PUSHGATEWAY_URL 未设置时静默跳过且不失败 =="
# 用空串而非 unset：本测试开了 set -u，unset 后再引用会直接中断
PUSHGATEWAY_URL=""
push_backup_result pg 1 "$NOW" >/dev/null 2>&1
check "退出码为 0" "$?" "0"

echo "== Pushgateway 不可达时不拖垮调用方 =="
PUSHGATEWAY_URL="http://127.0.0.1:1"   # 必定连不上
PUSH_TIMEOUT_SEC=2
push_backup_result pg 1 "$NOW" >/dev/null 2>&1
check "推送失败仍返回 0（备份已成功，不因监控故障判失败）" "$?" "0"

echo "== backup 脚本已接入指标上报 =="
for s in backup_pg backup_garage; do
  if grep -q "push_backup_result" "${ROOT}/scripts/${s}.sh"; then
    ok "${s}.sh 调用 push_backup_result"
  else
    bad "${s}.sh 未调用 push_backup_result"
  fi
  if grep -q "trap .*push_backup_result.* EXIT" "${ROOT}/scripts/${s}.sh"; then
    ok "${s}.sh 用 EXIT trap 覆盖失败路径"
  else
    bad "${s}.sh 未用 EXIT trap 覆盖失败路径"
  fi
done

echo "== restore_smoke 生产库守卫 =="
GUARD_OUT="$(cd "$ROOT" && DRY_RUN=1 bash scripts/restore_smoke.sh 2>&1)"
check "DRY_RUN 守卫通过退出码" "$?" "0"
contains "临时库带 imboy_smoke_ 前缀" "$GUARD_OUT" "imboy_smoke_"
# 生产库名不得出现在被 DROP 的位置：脚本只 DROP 自己生成的临时库
if grep -q 'DROP DATABASE IF EXISTS \\"${SMOKE_DB}\\"' "${ROOT}/scripts/restore_smoke.sh"; then
  ok "只 DROP 脚本自建的临时库"
else
  bad "DROP 目标不是 SMOKE_DB"
fi

echo
echo "通过 ${PASS}，失败 ${FAIL}"
[ "$FAIL" -eq 0 ]
