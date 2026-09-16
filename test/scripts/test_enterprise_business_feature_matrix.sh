#!/usr/bin/env bash
# ============================================================================
# EB-10：scripts/enterprise_business_feature_matrix.sh 的**静态安全自检**
#
# 为什么要有它：三档矩阵只用 disposable worktree 隔离 —— 一旦隔离失效，它的
# 断言会退化成「重复测试调用方当前状态」，而不再证明任何东西，且可能静默改写
# 共享主树。本自检把这次真实踩到的四个失效模式做成机械判据（纯静态、秒级）：
#
#   S1 生成器必须用**副本自己的** scripts/generate_product_features.py 调用
#      （它用 Path(__file__).resolve().parents[1] 当 repo 根；用调用方路径会把
#       11 个产物写回调用方，副本根本没被生成）
#   S2 必须用 `git worktree add` 造副本并注册到清理 trap（不切换共享 manifest）
#   S3 deps **复制**而不是软链共享（make 会把绝对路径写进 deps/*/*.d，共享会
#       反向污染调用方并让后续构建找不到头文件）
#   S4 副本目录名必须是 imboy（relx 经 code:lib_dir(imboy) 解析本应用）
#
# 用法：bash test/scripts/test_enterprise_business_feature_matrix.sh
# 退出码：0 = 四项全过；1 = 有失效
# ============================================================================
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/scripts/enterprise_business_feature_matrix.sh"
FAILED=0

ok()   { printf '  [ok]   %s\n' "$*"; }
fail() { printf '  [FAIL] %s\n' "$*" >&2; FAILED=$((FAILED + 1)); }

[ -f "$SCRIPT" ] || { echo "missing $SCRIPT" >&2; exit 1; }

# 语法 + bash 3.2 兼容（本机 /bin/bash = 3.2）
bash -n "$SCRIPT" && ok "S0 bash -n 语法通过" || fail "S0 语法错误"

# S1：副本自己的生成器
if rg -q 'python3 "\$wt/scripts/generate_product_features\.py"' "$SCRIPT"; then
  ok "S1 用副本自己的 scripts/generate_product_features.py 调用生成器"
else
  fail "S1 未用副本自己的生成器路径（产物会写回调用方，隔离失效）"
fi
if rg -q '^PY_GENERATOR=' "$SCRIPT" && rg -q 'python3 "\$PY_GENERATOR"' "$SCRIPT"; then
  fail "S1 仍有 'python3 \$PY_GENERATOR' 调用点（= 写回调用方的老缺陷）"
else
  ok "S1 生成调用点不指向调用方脚本"
fi

# S2：一次性工作树 + 清理 trap；且改 manifest 只改副本外的临时 preset 文件
if rg -q 'git -C "\$ROOT" worktree add --detach' "$SCRIPT"; then
  ok "S2 用 git worktree add 造一次性副本"
else
  fail "S2 未用 git worktree add"
fi
if rg -q '^trap cleanup EXIT' "$SCRIPT" && rg -q 'worktree remove --force' "$SCRIPT"; then
  ok "S2 注册了 EXIT trap 并在退出时注销一次性工作树"
else
  fail "S2 缺 EXIT trap / worktree remove"
fi
if rg -q '"\$MANIFEST"' "$SCRIPT"; then
  ok "S2 只把调用方 manifest 当只读输入（无写回语句）"
else
  fail "S2 见不到对调用方 manifest 的只读引用"
fi

# S3：deps 复制
if rg -q 'cp -Rcp "\$ROOT/deps/\." "\$wt/deps/"' "$SCRIPT" &&
  rg -q 'rm -f "\$wt/deps"/\*/\*\.d' "$SCRIPT"; then
  ok "S3 deps 复制进副本并清掉带进来的 .d（不软链共享）"
else
  fail "S3 deps 不是「复制 + 清 .d」"
fi
if rg -q 'ln -s "\$ROOT/deps"' "$SCRIPT"; then
  fail "S3 仍软链共享 deps（会反向污染调用方 .d）"
else
  ok "S3 无 deps 软链共享"
fi

# S4：副本目录名 imboy
if rg -q 'wt="\$TMPBASE/wt-\$name/imboy"' "$SCRIPT"; then
  ok "S4 副本目录名为 imboy（relx code:lib_dir 需要）"
else
  fail "S4 副本目录名不是 imboy（relx 会 app_not_found）"
fi

# S5：调用方未变的三重断言（tree hash / manifest 字节 / deps 状态）
for needle in '调用方 tracked tree hash 复原' '字节未变' 'deps/ 状态逐字未变'; do
  if rg -q "$needle" "$SCRIPT"; then
    ok "S5 含调用方未变断言：$needle"
  else
    fail "S5 缺调用方未变断言：$needle"
  fi
done

echo
if [ "$FAILED" -eq 0 ]; then
  echo "enterprise_business_feature_matrix 静态自检 PASS"
  exit 0
fi
echo "enterprise_business_feature_matrix 静态自检 FAIL（$FAILED 条）" >&2
exit 1
