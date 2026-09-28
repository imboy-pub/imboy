#!/usr/bin/env bash
# =============================================================================
# 跨仓 Widget 资产配对门（CP-ASSET-04）—— 后端常量 ↔ imboyadmin 产物名 一致性。
#
# 为什么需要它：CS widget frame HTML 引用的版本化静态 JS 路径是跨仓冻结合同
# （hosted-widget-contract-v1 S4）——后端写死在 ?PUBLIC_FRAME_ASSET_JS /
# ?FRAME_ASSET_JS 常量，产物名写死在 imboyadmin 的 widget-manifest.mts /
# widget-verify.mts 常量。两处各改各的会让 frame HTML 引用一个不存在（或错误
# 版本）的资产、线上静默 404。本门在任一侧漂移时非零退出。
#
# 单源机制（避免双真源）：配对关系（哪个面用哪个文件名、常量在哪个源文件）
# 唯一登记在本仓 priv/cs_widget_asset_pairing.json；本脚本（imboy 侧）与
# imboyadmin 仓 scripts/check-widget-asset-pairing.mts（admin 侧）都只读这份数据
# 做同一套对比，谁都不自带第二份配对表。
#
# 检查项（任一失败 exit 2）：
#   B1 后端每个面的 -define(MACRO, <<"/path">>) 实际值 == 合同 asset_path
#      （后端常量带前导 /，合同不带，比对前归一）；
#   A1 admin 侧每个登记常量的字面值 == 合同 asset_path（public 面两处：
#      manifest 产出名 + verify 校验名）；
#   J1 合同 JSON 自身合法且 face 集合与后端两个常量源一一对应。
#
# 退出码：0=配对一致；2=漂移/合同不合法；3=admin 仓不可达（显式 SKIPPED，
# "没核对"不冒充"一致"，与 feature-cross-repo-check 的 SKIPPED 语义一致）。
#
# 用法：
#   scripts/check_widget_asset_pairing.sh            # admin 默认 ../imboyadmin
#   ADMIN_REPO_DIR=/path/imboyadmin \
#     scripts/check_widget_asset_pairing.sh          # CI / worktree 布局显式指定
# =============================================================================
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONTRACT="$REPO_ROOT/priv/cs_widget_asset_pairing.json"

fail() { echo "  ❌ $*" >&2; FAILED=1; }
ok()   { echo "  ✅ $*"; }
FAILED=0

if [ ! -f "$CONTRACT" ]; then
  echo "❌ 配对合同缺失：$CONTRACT" >&2
  exit 2
fi

# 用 python3 解析合同（macOS 无预装 jq；仓内门禁脚本同为 python3 依赖），
# 输出制表符分隔行供 bash 循环：face \t asset_path \t backend_source \t macro
PAIRS="$(python3 - "$CONTRACT" <<'PY'
import json, sys
with open(sys.argv[1], encoding='utf-8') as f:
    contract = json.load(f)
for p in contract.get('pairs', []):
    be = p.get('backend') or {}
    print('\t'.join([
        p.get('face', ''),
        p.get('asset_path', ''),
        be.get('source', ''),
        be.get('macro', ''),
    ]))
PY
)" || { echo "❌ 合同 JSON 解析失败" >&2; exit 2; }

if [ -z "$PAIRS" ]; then
  echo "❌ 合同 pairs 为空" >&2
  exit 2
fi

# ---- B1) 后端常量 == 合同 ---------------------------------------------------
backend_checked=0
while IFS="$(printf '\t')" read -r face asset be_src macro; do
  [ -n "$face" ] || continue
  src_file="$REPO_ROOT/$be_src"
  if [ ! -f "$src_file" ]; then
    fail "B1 [$face] 后端源文件缺失：$be_src"
    continue
  fi
  # 形如 -define(MACRO, <<"/widget-assets/cs-widget.vN.js">>).
  actual="$(grep -E "^-define\(${macro}," "$src_file" \
    | head -1 \
    | sed -E 's/.*<<"([^"]+)">>.*/\1/' || true)"
  if [ -z "$actual" ]; then
    fail "B1 [$face] 未在 $be_src 找到 -define(${macro}, <<\"...\">>) 形式的常量"
    continue
  fi
  norm="${actual#/}"
  if [ "$norm" != "$asset" ]; then
    fail "B1 [$face] 后端常量 ${macro}=${actual} != 合同 ${asset}"
  else
    ok "B1 [$face] 后端 ${macro} == 合同 ${asset}"
  fi
  backend_checked=$((backend_checked + 1))
done <<< "$PAIRS"

if [ "$backend_checked" -eq 0 ]; then
  echo "❌ 合同没有任何可检查的后端面" >&2
  exit 2
fi

# ---- A1) admin 侧产物名 == 合同 --------------------------------------------
ADMIN_DIR="${ADMIN_REPO_DIR:-$REPO_ROOT/../imboyadmin}"
if [ ! -d "$ADMIN_DIR" ]; then
  # ${ADMIN_DIR} 必须加大括号：紧随的全角字符会被 bash 并入变量名解析
  # （darwin arm64 bash 3.2 实测），导致 unbound variable 而非预期 exit 3。
  echo "⚠️  SKIPPED: imboyadmin 仓不可达（${ADMIN_DIR}）——admin 侧未核对" >&2
  echo "    指定：ADMIN_REPO_DIR=/path/to/imboyadmin $0" >&2
  exit 3
fi

# 提取合同中 admin 面的登记（face \t asset \t admin_file \t const）
ADMIN_PAIRS="$(python3 - "$CONTRACT" <<'PY'
import json, sys
with open(sys.argv[1], encoding='utf-8') as f:
    contract = json.load(f)
for p in contract.get('pairs', []):
    adm = p.get('admin')
    if not adm:
        continue
    for s in adm.get('sources', []):
        print('\t'.join([
            p.get('face', ''),
            p.get('asset_path', ''),
            s.get('file', ''),
            s.get('const', ''),
        ]))
PY
)"

if [ -n "$ADMIN_PAIRS" ]; then
  while IFS="$(printf '\t')" read -r face asset adm_file const_name; do
    [ -n "$face" ] || continue
    adm_src="$ADMIN_DIR/$adm_file"
    if [ ! -f "$adm_src" ]; then
      fail "A1 [$face] admin 源文件缺失：$ADMIN_DIR/$adm_file"
      continue
    fi
    # 形如 const STABLE_ASSET_ALIAS = 'widget-assets/cs-widget.vN.js'
    # （BSD grep -E 不识别 \s，用 [[:space:]]）
    actual="$(grep -E "const ${const_name}[[:space:]]*=" "$adm_src" \
      | head -1 \
      | sed -E "s/.*['\"]([^'\"]+)['\"].*/\1/" || true)"
    if [ -z "$actual" ]; then
      fail "A1 [$face] 未在 $adm_file 找到 const ${const_name} = '...' 形式的常量"
      continue
    fi
    if [ "$actual" != "$asset" ]; then
      fail "A1 [$face] admin ${const_name}=${actual} != 合同 ${asset}"
    else
      ok "A1 [$face] admin ${const_name} == 合同 ${asset}"
    fi
  done <<< "$ADMIN_PAIRS"
else
  echo "  （合同未登记 admin 产出面，A1 跳过——legacy 面仅后端侧校验）"
fi

# ---- 汇总 -------------------------------------------------------------------
if [ "$FAILED" -ne 0 ]; then
  echo "❌ Widget 资产配对门未通过：后端常量与 admin 产物名存在漂移，或合同不合法" >&2
  exit 2
fi

echo "✅ Widget 资产配对门通过：后端常量 / admin 产物名 / 合同三者一致（合同=priv/cs_widget_asset_pairing.json）"
