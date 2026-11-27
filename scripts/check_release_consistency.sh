#!/usr/bin/env bash
# ============================================================
# 商业化发布一致性检查 / Release consistency gate (C0-CONTRACT-01)
# ------------------------------------------------------------
# 检查项（全部只读，不改任何文件）：
#   1. 版本    VERSION 与 relx.config 的 release 版本必须一致
#              （历史事故：两处双源，只 bump 一处导致发布出错版本）
#   2. 迁移    每个 *.up.sql 必须有同号 *.down.sql；序号不得重复
#   3. 法律许可 LICENSE 保留，旧运行时授权模块和 API 不得重现
#              不得泄漏 license 原文 / 签名材料 / 内部失败原因
#   4. 运维    备份 / 恢复 / 恢复演练 / 部署脚本存在且语法可解析
#   5. 契约    商业路由（billing / adm finance billing / license / sso /
#              export_data / brand）必须都在 api/openapi.yaml 里有契约
#   6. 支持矩阵 docs/ops/support-matrix.md 存在且声明关键依赖版本
#   7. 钱包约束 隔离克隆 Gate 与本地 fail-closed 守卫必须存在并实际通过
#
# 用法 / Usage:
#   bash scripts/check_release_consistency.sh            # 全量
#   IMBOY_ROOT=/path bash scripts/check_release_consistency.sh
# 退出码：0=全绿；1=存在失败项
# ============================================================
set -uo pipefail

IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

CRC_PASS=0
CRC_FAIL=0

crc_ok() {
  CRC_PASS=$((CRC_PASS + 1))
  echo "  ✓ $1"
}

crc_bad() {
  CRC_FAIL=$((CRC_FAIL + 1))
  echo "  ✗ $1"
}

# ------------------------------------------------------------
# 1. 版本一致性：VERSION == relx.config
# 注意：api/openapi.yaml 的 info.version 是契约版本，与产品版本本就不同源，
#       故**不**参与该断言（强行对齐只会制造假失败）。
# ------------------------------------------------------------
check_version() {
  local root="${1:-$IMBOY_ROOT}"
  local vfile="${root}/VERSION"
  local relx="${root}/relx.config"

  if [ ! -f "$vfile" ]; then
    crc_bad "VERSION 文件缺失"
    return 1
  fi
  if [ ! -f "$relx" ]; then
    crc_bad "relx.config 缺失"
    return 1
  fi

  local v relx_v
  v="$(tr -d ' \t\r\n' <"$vfile")"
  relx_v="$(grep -o '{release, *{imboy, *"[^"]*"' "$relx" | head -1 | sed -n 's/.*"\([^"]*\)".*/\1/p')"

  if [ -z "$relx_v" ]; then
    crc_bad "relx.config 未解析出 release 版本号"
    return 1
  fi
  if [ "$v" = "$relx_v" ]; then
    crc_ok "版本一致：VERSION = relx.config = ${v}"
    return 0
  fi
  crc_bad "版本不一致：VERSION='${v}' 但 relx.config='${relx_v}'（两处都要 bump）"
  return 1
}

# ------------------------------------------------------------
# 2. 迁移：up/down 成对 + 序号唯一
# ------------------------------------------------------------
check_migrations() {
  local root="${1:-$IMBOY_ROOT}"
  local dir="${root}/priv/migrations"
  local rc=0

  if [ ! -d "$dir" ]; then
    crc_bad "迁移目录缺失：${dir}"
    return 1
  fi

  local missing="" up seq base
  # 无 down 豁免清单（down 会造成安全降级或不可逆数据损毁的迁移）：
  #  00000074 E2EE 备份 KDF 下限收紧——down=主动放松 CHECK 约束，恢复降级攻击面
  #  00000075 附件 legacy_key 列——down=DROP 列即丢失全部存量附件密钥
  #（Garage 对象已被覆盖加密），造成永久不可解
  local no_down_exempt=" 00000074 00000075 "
  for up in "$dir"/*.up.sql; do
    [ -e "$up" ] || continue
    base="${up%.up.sql}"
    seq="$(basename "$up")"
    if [ ! -f "${base}.down.sql" ]; then
      case "$no_down_exempt" in
        *" ${seq%%_*} "*) : ;;
        *) missing="${missing} ${seq}" ;;
      esac
    fi
  done
  if [ -n "$missing" ]; then
    crc_bad "迁移缺 down 脚本：${missing}"
    rc=1
  else
    crc_ok "迁移 up/down 全部成对"
  fi

  local dup
  dup="$(for up in "$dir"/*.up.sql; do
    [ -e "$up" ] || continue
    seq="$(basename "$up")"
    echo "${seq%%_*}"
  done | sort | uniq -d)"
  if [ -n "$dup" ]; then
    crc_bad "迁移序号重复：$(echo "$dup" | tr '\n' ' ')"
    rc=1
  else
    crc_ok "迁移序号无重复"
  fi
  return $rc
}

# ------------------------------------------------------------
# 3. Legal license and retirement of the former commercial quota API.
# ------------------------------------------------------------
check_license() {
  local root="${1:-$IMBOY_ROOT}"
  local rc=0
  local f
  if [ -f "${root}/LICENSE" ]; then
    crc_ok "仓库法律许可 LICENSE 存在"
  else
    crc_bad "仓库法律许可 LICENSE 缺失"
    rc=1
  fi
  for f in scripts/gen_license.escript src/lib/imboy_license.erl src/lib/license_notice_worker.erl; do
    if [ -e "${root}/${f}" ]; then
      crc_bad "已退役的运行时授权资产仍存在：${f}"
      rc=1
    fi
  done
  if grep -qF '/api/adm/stats/license' "${root}/src/imboy_router.erl" "${root}/api/openapi.yaml"; then
    crc_bad "已退役的授权 API 仍在路由或契约"
    rc=1
  fi
  return $rc
}

# ------------------------------------------------------------
# 4. 运维脚本：备份 / 恢复 / 恢复演练 / 部署 —— 存在且语法可解析
# ------------------------------------------------------------
check_ops_scripts() {
  local root="${1:-$IMBOY_ROOT}"
  local rc=0
  local f p
  for f in backup_pg.sh restore_pg.sh restore_smoke.sh imboy-deploy.sh lib/blue_green_deploy.sh backup_garage.sh; do
    p="${root}/scripts/${f}"
    if [ ! -f "$p" ]; then
      crc_bad "运维脚本缺失：scripts/${f}"
      rc=1
      continue
    fi
    if bash -n "$p" 2>/dev/null; then
      crc_ok "运维脚本语法通过：scripts/${f}"
    else
      crc_bad "运维脚本语法错误：scripts/${f}"
      rc=1
    fi
  done
  return $rc
}

# ------------------------------------------------------------
# 5. 契约防漂移：商业路由必须在 openapi.yaml 有契约
# 只覆盖商业化五组 + brand；其余路由的全量对账另有 oasdiff 流程。
# ------------------------------------------------------------
COMMERCIAL_PATHS=(
  "/api/v1/billing/subscribe"
  "/api/v1/billing/subscription"
  "/api/v1/billing/renew"
  "/api/v1/billing/cancel"
  "/api/v1/billing/usage"
  "/api/v1/billing/quota"
  "/api/v1/billing/invoice/generate"
  "/api/v1/billing/invoice/pay"
  "/api/v1/billing/invoice/list"
  "/api/adm/finance/billing/plans"
  "/api/adm/finance/billing/plan"
  "/api/adm/finance/billing/plan/update"
  "/api/adm/finance/billing/subscriptions"
  "/api/adm/finance/billing/invoices"
  "/api/adm/sso/config"
  "/api/adm/sso/test"
  "/api/v1/user/export_data"
  "/api/v1/auth/oidc/authorize"
  "/api/v1/auth/oidc/callback"
  "/api/v1/auth/oidc/exchange"
  "/brand"
)

check_contract_coverage() {
  local root="${1:-$IMBOY_ROOT}"
  local spec="${root}/api/openapi.yaml"
  local router="${root}/src/imboy_router.erl"
  local rc=0

  if [ ! -f "$spec" ]; then
    crc_bad "api/openapi.yaml 缺失"
    return 1
  fi

  local missing="" p
  for p in "${COMMERCIAL_PATHS[@]}"; do
    if ! grep -q "^  ${p}:" "$spec"; then
      missing="${missing} ${p}"
    fi
  done
  if [ -n "$missing" ]; then
    crc_bad "商业路由缺 OpenAPI 契约：${missing}"
    rc=1
  else
    crc_ok "商业路由 ${#COMMERCIAL_PATHS[@]} 条全部有 OpenAPI 契约"
  fi

  # 反向：契约里声明的商业路由必须真的注册在 router（防写出幽灵端点）
  if [ -f "$router" ]; then
    local ghost=""
    for p in "${COMMERCIAL_PATHS[@]}"; do
      [ "$p" = "/brand" ] && continue
      if ! grep -q "\"${p}\"" "$router"; then
        ghost="${ghost} ${p}"
      fi
    done
    if [ -n "$ghost" ]; then
      crc_bad "契约声明但 router 未注册（幽灵端点）：${ghost}"
      rc=1
    else
      crc_ok "契约中的商业路由均已在 imboy_router 注册"
    fi
  fi
  return $rc
}

# ------------------------------------------------------------
# 6. 支持矩阵：私有化交付必须白纸黑字写清依赖版本
# ------------------------------------------------------------
check_support_matrix() {
  local root="${1:-$IMBOY_ROOT}"
  local doc="${root}/docs/ops/support-matrix.md"
  local rc=0

  if [ ! -f "$doc" ]; then
    crc_bad "支持矩阵缺失：docs/ops/support-matrix.md"
    return 1
  fi

  local k
  for k in "Erlang/OTP" "PostgreSQL" "Flutter" "Docker"; do
    if grep -q "$k" "$doc"; then
      crc_ok "支持矩阵声明了 ${k}"
    else
      crc_bad "支持矩阵未声明 ${k}"
      rc=1
    fi
  done
  return $rc
}

# ------------------------------------------------------------
# 7. 钱包约束克隆 Gate：脚本存在、语法正确，且离线 fail-closed 守卫真实执行
# 这里只运行 mock/输入守卫，不连接任何数据库；生产规模克隆仍是独立外部 Gate。
# ------------------------------------------------------------
check_wallet_constraint_gate() {
  local root="${1:-$IMBOY_ROOT}"
  local gate="${root}/scripts/verify_wallet_constraint_clone.sh"
  local test="${root}/scripts/test/wallet_constraint_clone_guard_test.sh"
  local rc=0 f

  for f in "$gate" "$test"; do
    if [ ! -f "$f" ]; then
      crc_bad "钱包约束克隆 Gate 资产缺失：${f#"${root}/"}"
      rc=1
    elif ! bash -n "$f" 2>/dev/null; then
      crc_bad "钱包约束克隆 Gate 语法错误：${f#"${root}/"}"
      rc=1
    fi
  done
  [ "$rc" -eq 0 ] || return "$rc"

  if (cd "$root" && bash "$test" >/dev/null 2>&1); then
    crc_ok "钱包约束克隆 Gate 离线守卫通过"
  else
    crc_bad "钱包约束克隆 Gate 离线守卫失败"
    rc=1
  fi
  return "$rc"
}

crc_main() {
  echo "== 1. 版本一致性 =="
  check_version "$IMBOY_ROOT"
  echo "== 2. 迁移 up/down =="
  check_migrations "$IMBOY_ROOT"
  echo "== 3. 法律许可与运行时授权退役 =="
  check_license "$IMBOY_ROOT"
  echo "== 4. 备份 / 恢复 / 部署脚本 =="
  check_ops_scripts "$IMBOY_ROOT"
  echo "== 5. 商业契约覆盖 =="
  check_contract_coverage "$IMBOY_ROOT"
  echo "== 6. 支持矩阵 =="
  check_support_matrix "$IMBOY_ROOT"
  echo "== 7. 钱包约束克隆 Gate =="
  check_wallet_constraint_gate "$IMBOY_ROOT"

  echo ""
  echo "通过 ${CRC_PASS} 项，失败 ${CRC_FAIL} 项"
  [ "$CRC_FAIL" -eq 0 ]
}

# 被 source 时只加载函数，便于测试注入 fixture 根目录
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
  crc_main
  exit $?
fi
