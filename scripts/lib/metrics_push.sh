#!/usr/bin/env bash
# ============================================================
# Pushgateway 指标推送公共库 / Shared Pushgateway push helper
# ------------------------------------------------------------
# 被 scripts/backup_pg.sh、scripts/backup_garage.sh、
# scripts/check_tls_expiry.sh 引用（source）。
#
# 为什么需要：deploy/prometheus/rules/imboy-alerts.yml 的
# IMBoyBackupNotRunning 告警依赖 imboy_backup_last_success_timestamp，
# 但此前没有任何脚本推送该指标 —— absent() 分支会让告警永久 CRITICAL。
# 本库负责把「告警」和「指标产出方」接上。
#
# 用法 / Usage:
#   source "$(dirname "$0")/lib/metrics_push.sh"
#   push_backup_result pg 1 "$START_TS" /path/to/file.dump
#
# 环境变量 / Env:
#   PUSHGATEWAY_URL   未设置则静默跳过（本地开发不强制依赖 Pushgateway）
#   PUSH_TIMEOUT_SEC  单次推送超时，默认 10
# ============================================================

PUSHGATEWAY_URL="${PUSHGATEWAY_URL:-}"
PUSH_TIMEOUT_SEC="${PUSH_TIMEOUT_SEC:-10}"

# 内部：把 stdin 的 Prometheus 文本推到 <base>/metrics/job/<job>/component/<component>
# 推送失败只告警不中断调用方——备份本身已成功时，不能因为监控侧不可用就判定备份失败。
_metrics_push_raw() {
  local job="$1" component="$2" payload="$3"

  if [ -z "$PUSHGATEWAY_URL" ]; then
    echo "[metrics_push] PUSHGATEWAY_URL 未设置，跳过指标推送" >&2
    return 0
  fi

  local url="${PUSHGATEWAY_URL%/}/metrics/job/${job}/component/${component}"
  if printf '%s\n' "$payload" \
      | curl -sS --fail --max-time "$PUSH_TIMEOUT_SEC" --data-binary @- "$url" >/dev/null 2>&1; then
    echo "[metrics_push] 已推送 job=${job} component=${component}" >&2
    return 0
  fi

  echo "[metrics_push] 警告：推送失败 url=${url}（不影响本次作业结果）" >&2
  return 0
}

# 构造备份结果指标文本（纯函数，便于测试断言 payload）
# 参数：component  status(1成功/0失败)  start_ts  [size_bytes]
build_backup_payload() {
  local component="$1" status="$2" start_ts="$3" size_bytes="${4:-0}"
  local now duration
  now="$(date -u +%s)"
  duration="$((now - start_ts))"

  printf '%s\n' \
    "# TYPE imboy_backup_last_status gauge" \
    "imboy_backup_last_status ${status}" \
    "# TYPE imboy_backup_last_duration_seconds gauge" \
    "imboy_backup_last_duration_seconds ${duration}" \
    "# TYPE imboy_backup_size_bytes gauge" \
    "imboy_backup_size_bytes ${size_bytes}"

  # 只有成功才刷新 last_success_timestamp：失败时保留旧值，
  # 让 IMBoyBackupNotRunning 能按「距上次成功多久」正确计时。
  if [ "$status" = "1" ]; then
    printf '%s\n' \
      "# TYPE imboy_backup_last_success_timestamp gauge" \
      "imboy_backup_last_success_timestamp ${now}"
  fi
}

# 推送备份结果
# 参数：component  status(1/0)  start_ts  [file_path]
push_backup_result() {
  local component="$1" status="$2" start_ts="$3" file_path="${4:-}"
  local size_bytes=0

  if [ -n "$file_path" ] && [ -f "$file_path" ]; then
    size_bytes="$(wc -c < "$file_path" | tr -d ' ')"
  fi

  _metrics_push_raw "imboy_backup" "$component" \
    "$(build_backup_payload "$component" "$status" "$start_ts" "$size_bytes")"
}

# 构造恢复演练结果指标文本（B-22，纯函数便于测试断言 payload）
# 参数：status(1成功/0失败)  start_ts  [restored_rows]
build_restore_payload() {
  local status="$1" start_ts="$2" rows="${3:-0}"
  local now duration
  now="$(date -u +%s)"
  duration="$((now - start_ts))"

  printf '%s\n' \
    "# TYPE imboy_restore_drill_last_status gauge" \
    "imboy_restore_drill_last_status ${status}" \
    "# TYPE imboy_restore_drill_last_duration_seconds gauge" \
    "imboy_restore_drill_last_duration_seconds ${duration}" \
    "# TYPE imboy_restore_drill_restored_rows gauge" \
    "imboy_restore_drill_restored_rows ${rows}"

  # 与备份指标同样的取舍：只有成功才刷新 last_success_timestamp，
  # 失败时保留旧值，让"距上次成功多久"能正确计时。
  if [ "$status" = "1" ]; then
    printf '%s\n' \
      "# TYPE imboy_restore_drill_last_success_timestamp gauge" \
      "imboy_restore_drill_last_success_timestamp ${now}"
  fi
}

# 推送恢复演练结果
# 参数：status(1/0)  start_ts  [restored_rows]
push_restore_result() {
  _metrics_push_raw "imboy_restore_drill" "pg" \
    "$(build_restore_payload "$1" "$2" "${3:-0}")"
}

# 构造 TLS 证书到期指标文本
# 参数：expiry_unix_ts
build_tls_payload() {
  local expiry_ts="$1"
  printf '%s\n' \
    "# TYPE imboy_tls_cert_expiry_timestamp gauge" \
    "imboy_tls_cert_expiry_timestamp ${expiry_ts}"
}

# 推送 TLS 证书到期时间
# 参数：domain  expiry_unix_ts
push_tls_expiry() {
  local domain="$1" expiry_ts="$2"
  _metrics_push_raw "imboy_tls" "$domain" "$(build_tls_payload "$expiry_ts")"
}
