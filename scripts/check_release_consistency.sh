#!/usr/bin/env bash
# ============================================================
# 商业化发布一致性检查 / Release consistency gate (C0-CONTRACT-01)
# ------------------------------------------------------------
# 检查项（全部只读，不改任何文件）：
#   1. 版本    VERSION 与 relx.config 的 release 版本必须一致
#              （历史事故：两处双源，只 bump 一处导致发布出错版本）
#   2. 迁移    每个 *.up.sql 必须有同号 *.down.sql；序号不得重复
#   3. License LICENSE + gen_license.escript 存在；public_info/0 白名单
#              不得泄漏 license 原文 / 签名材料 / 内部失败原因
#   4. 运维    备份 / 恢复 / 恢复演练 / 部署脚本存在且语法可解析
#   5. 契约    商业路由（billing / adm finance billing / license / sso /
#              export_data / brand）必须都在 api/openapi.yaml 里有契约
#   6. 支持矩阵 docs/ops/support-matrix.md 存在且声明关键依赖版本
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
  for up in "$dir"/*.up.sql; do
    [ -e "$up" ] || continue
    base="${up%.up.sql}"
    if [ ! -f "${base}.down.sql" ]; then
      missing="${missing} $(basename "$up")"
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
# 3. License：文件在位 + 状态 API 不泄漏授权材料
# ------------------------------------------------------------
check_license() {
  local root="${1:-$IMBOY_ROOT}"
  local rc=0
  local f

  for f in "LICENSE" "scripts/gen_license.escript" "src/lib/imboy_license.erl"; do
    if [ -f "${root}/${f}" ]; then
      crc_ok "License 资产存在：${f}"
    else
      crc_bad "License 资产缺失：${f}"
      rc=1
    fi
  done

  local src="${root}/src/lib/imboy_license.erl"
  if [ -f "$src" ]; then
    # public_info/0 是对外白名单，出现下列任何键都意味着授权材料外泄
    local body leak=""
    body="$(sed -n '/^public_info() ->/,/^    }\./p' "$src")"
    if [ -z "$body" ]; then
      crc_bad "imboy_license:public_info/0 未找到（脱敏白名单无法校验）"
      rc=1
    else
      local k
      for k in license_text signature signed_by private_key reason raw; do
        if echo "$body" | grep -q "\b${k}\b"; then
          leak="${leak} ${k}"
        fi
      done
      if [ -n "$leak" ]; then
        crc_bad "public_info/0 泄漏授权材料字段：${leak}"
        rc=1
      else
        crc_ok "public_info/0 白名单未泄漏授权材料"
      fi
    fi
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
  for f in backup_pg.sh restore_pg.sh restore_smoke.sh deploy.sh backup_garage.sh; do
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
  "/api/adm/stats/license"
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

crc_main() {
  echo "== 1. 版本一致性 =="
  check_version "$IMBOY_ROOT"
  echo "== 2. 迁移 up/down =="
  check_migrations "$IMBOY_ROOT"
  echo "== 3. License 资产与脱敏 =="
  check_license "$IMBOY_ROOT"
  echo "== 4. 备份 / 恢复 / 部署脚本 =="
  check_ops_scripts "$IMBOY_ROOT"
  echo "== 5. 商业契约覆盖 =="
  check_contract_coverage "$IMBOY_ROOT"
  echo "== 6. 支持矩阵 =="
  check_support_matrix "$IMBOY_ROOT"

  echo ""
  echo "通过 ${CRC_PASS} 项，失败 ${CRC_FAIL} 项"
  [ "$CRC_FAIL" -eq 0 ]
}

# 被 source 时只加载函数，便于测试注入 fixture 根目录
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
  crc_main
  exit $?
fi
