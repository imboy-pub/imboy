#!/usr/bin/env bash
# ============================================================
# l4_sni_alert_rules_test.sh — L4 SNI 监控接线聚焦测试（Task 2 / A5）
# ------------------------------------------------------------
# 合同来源:
#   docs/plans/2026-10-01-l4-sni-hardening-relay-verification-plan-v1.md (Task 2)
#   .Codex/runs/20261002T131801Z-l4-sni-hardening/control/interface-contract.md
#
# 校验对象（全部为 A5 独占交付物）：
#   1. deploy/prometheus/rules/imboy-alerts.yml
#      - promtool check rules 全文件（追加的 imboy.l4_sni group 不得破坏
#        既有语法；YAML 语法错误在此暴露，属规则错误而非工具缺失）
#      - 四条冻结告警名必须存在：L4SNIListenDrift / L4SNICheckFailed /
#        L4SNIStaleMetrics / L4SNIMetricsMissing
#   2. scripts/test/fixtures/l4_sni_rules/*.yml
#      - promtool test rules：健康 / 漂移 / 检测失败 / 过期 / 从未推送 /
#        单实例消失 六组场景，断言 alert 名与 firing 状态
#   3. deploy/cron/imboy-ops.cron
#      - 每 5 分钟巡检行存在且调用 check_l4_sni_listen.sh --strict --push
#
# 退出码：0=全部通过；1=存在失败（规则/YAML/断言）；127=promtool 不可用
# （BLOCKED_ENV）。promtool 不可用或校验失败均非零退出并输出原因；
# 绝不打印「跳过」返回成功。
#
# 兼容 bash 3.2（macOS /bin/bash）。
# ============================================================

set -Eeuo pipefail

PROG="${0##*/}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RULES="$ROOT/deploy/prometheus/rules/imboy-alerts.yml"
CRON="$ROOT/deploy/cron/imboy-ops.cron"
FIXTURE_DIR="$ROOT/scripts/test/fixtures/l4_sni_rules"

FROZEN_ALERTS="L4SNIListenDrift L4SNICheckFailed L4SNIStaleMetrics L4SNIMetricsMissing"

FAILURES=0

log()  { printf '%s\n' "$*"; }
ok()   { printf 'PASS  %s\n' "$*"; }
bad()  { printf 'FAIL  %s\n' "$*" >&2; FAILURES=$((FAILURES + 1)); }

# ── promtool 探测：PATH 优先，其次常见安装路径 ────────────────
find_promtool() {
  local p
  if p="$(command -v promtool 2>/dev/null)" && [ -x "$p" ]; then
    printf '%s' "$p"
    return 0
  fi
  for p in /opt/homebrew/bin/promtool /usr/local/bin/promtool /usr/bin/promtool; do
    if [ -x "$p" ]; then
      printf '%s' "$p"
      return 0
    fi
  done
  return 1
}

if ! PROMTOOL="$(find_promtool)"; then
  bad "promtool not found in PATH nor common locations (/opt/homebrew/bin, /usr/local/bin, /usr/bin)"
  bad "BLOCKED_ENV: 无法执行规则校验；本测试不因工具缺失而返回成功"
  exit 127
fi
log "promtool: $PROMTOOL ($("$PROMTOOL" --version 2>&1 | head -n 1))"

# ── 结构断言（不依赖 promtool，先跑）──────────────────────────
if [ ! -f "$RULES" ]; then
  bad "rules file missing: $RULES"
else
  ok "rules file exists: $RULES"
fi
if [ ! -f "$CRON" ]; then
  bad "cron file missing: $CRON"
else
  ok "cron file exists: $CRON"
fi

if [ -f "$RULES" ]; then
  for name in $FROZEN_ALERTS; do
    if grep -q "alert: ${name}\$" "$RULES"; then
      ok "frozen alert present: $name"
    else
      bad "frozen alert missing from $RULES: $name"
    fi
  done
fi

if [ -f "$CRON" ]; then
  if grep -q '^\*/5 \* \* \* \* root .*check_l4_sni_listen\.sh --strict --push' "$CRON"; then
    ok "cron patrol line present (*/5, check_l4_sni_listen.sh --strict --push)"
  else
    bad "cron patrol line missing in $CRON: expected '*/5 * * * * root ... check_l4_sni_listen.sh --strict --push'"
  fi
  if grep -q 'check_l4_sni_listen.*|| true' "$CRON"; then
    bad "cron line must not hide failures with '|| true'"
  else
    ok "cron line does not hide failures with '|| true'"
  fi
fi

# ── promtool check rules：全文件，防追加破坏既有语法 ──────────
rc=0
"$PROMTOOL" check rules "$RULES" || rc=$?
if [ "$rc" -eq 0 ]; then
  ok "promtool check rules $RULES (exit=0)"
else
  bad "promtool check rules $RULES FAILED (exit=$rc) — 规则/YAML 校验失败（非工具缺失），上方为原始输出"
fi

# ── promtool test rules：逐个 fixture ────────────────────────
if [ -d "$FIXTURE_DIR" ]; then
  found=0
  for fx in "$FIXTURE_DIR"/*.yml; do
    [ -e "$fx" ] || continue
    found=1
    rc=0
    "$PROMTOOL" test rules "$fx" || rc=$?
    if [ "$rc" -eq 0 ]; then
      ok "promtool test rules ${fx#"$ROOT/"} (exit=0)"
    else
      bad "promtool test rules ${fx#"$ROOT/"} FAILED (exit=$rc) — 场景断言失败，上方为原始输出"
    fi
  done
  if [ "$found" -eq 0 ]; then
    bad "no fixture *.yml found under ${FIXTURE_DIR#"$ROOT/"}"
  fi
else
  bad "fixture dir missing: ${FIXTURE_DIR#"$ROOT/"}"
fi

# ── 汇总 ─────────────────────────────────────────────────────
if [ "$FAILURES" -gt 0 ]; then
  bad "$PROG: $FAILURES failure(s)"
  exit 1
fi
log "ALL PASS: L4 SNI alert rules wiring verified"
exit 0
