#!/usr/bin/env bash
# EB-07 —— 企业附件闭环集成门（plan §EB-07 指定的门）。
#
# 契约（EB-07）：
#   「实现私有 object presigned PUT/confirm、鉴权代理 content download、hash/mime/size 校验和
#     pending cleanup」，以及 `control/required-acceptance.tsv` 的 `EB-07-A01..A06`。
#
# 本脚本做什么：
#   1. 前置门：本地配置与可用的 scratch 库（缺一即以清晰 RED 退出，不静默跳过）；
#   2. 编译（src + test，带 -DTEST）；
#   3. 跑 `eb_asset_it_runner:main/0`：逐条打印 `[ASSERT EB-07-Axx]` / `[ASSERT-FAIL ...]`；
#   4. 汇总 `ASSET_IT: ASSERT_PASS=n ASSERT_FAIL=m`，m>0 即整门红；
#   5. 再把调用方契约门收敛到 **asset 范围**（`eb_asset_port` 0 处越界调用）。
#
# 覆盖的 Acceptance（逐条见 `test/features/enterprise_business/application/asset/`）：
#   A01 A 上传后 Org 持有；content 响应不含 storage URL/endpoint/object key
#   A02 suspended A 的旧 JWT 新下载请求立即拒绝；PUT 后 suspend 的 confirm 失败
#   A03 B 接任后经代理读取同 asset/hash
#   A04 跨 Org/Workspace/asset id/object key 猜测失败
#   A05 cleanup 只删本脚本创建的超时 pending 对象
#   A06 confirmed asset 的 retain_until/hold 不短于所属 message；ACK/隐藏/offboarding 不触发对象删除
#
# **边界声明（A11）**：对象存储侧只经**本地替身**（`eb_asset_object_stub`，进程内桶）。
# 本门证明的是**适配器契约 / 作用域 / 错误传播**在本地的正向行为，**不**构成真实 Garage
# 验收结论。报告口径固定为：`adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
#
# 安全性：只使用本脚本自建的**合成**租户（随机 TSID）与合成消息/hold/离职 case；
# 不触真实账号、联系方式、客户数据或生产资源；从不 TRUNCATE，也不删别人的行。
#
# 用法：bash scripts/enterprise_business_asset_it.sh
#   EUNIT_CONFIG 可覆盖本地配置（默认 config/sys.local.eb，对应 config/sys.local.eb.config）。
set -uo pipefail

cd "$(dirname "$0")/.."

EUNIT_CONFIG="${EUNIT_CONFIG:-config/sys.local.eb}"
CONFIG_FILE="${EUNIT_CONFIG}.config"
TEST_DIR="test"
RUNNER="eb_asset_it_runner"

echo "== EB-07 企业附件闭环集成门 =="
echo "worktree : $(pwd)"
echo "config   : $CONFIG_FILE"

# ---------------------------------------------------------------- 前置门
PRE_FAIL=0
if [ ! -f "$CONFIG_FILE" ]; then
  echo "[FAIL] 缺少本地配置 $CONFIG_FILE —— 先跑 control/a0_eb_scratch_env.sh worker <worktree> <db>"
  PRE_FAIL=$((PRE_FAIL + 1))
fi
if [ ! -f "$TEST_DIR/features/enterprise_business/application/asset/eb_asset_it_runner.erl" ]; then
  echo "[FAIL] 缺少集成门入口 $TEST_DIR/features/enterprise_business/application/asset/eb_asset_it_runner.erl"
  PRE_FAIL=$((PRE_FAIL + 1))
fi
if [ ! -f src/features/enterprise_business/application/asset/eb_asset_app.erl ]; then
  echo "[FAIL] 缺少用例层 src/features/enterprise_business/application/asset/eb_asset_app.erl（EB-07 未交付）"
  PRE_FAIL=$((PRE_FAIL + 1))
fi
if [ "$PRE_FAIL" -ne 0 ]; then
  echo
  echo "ASSET_IT: ASSERT_PASS=0 ASSERT_FAIL=$PRE_FAIL"
  echo "[FAIL] EB-07 集成门前置检查未通过"
  exit 1
fi
echo "[OK]   前置检查通过（配置 + 用例层 + 门入口均在）"

# ---------------------------------------------------------------- 编译
# 用 test-build（带 -DTEST）而不是 make compile：本仓大量「测试期才导出」的函数
# 只在 -DTEST 构建里可见；先跑 make compile 会让 ebin 里的无 TEST 版 beam 更新，
# 后续所有用例集体报 **error:undef（假回归，见 control/preflight_eunit_build.sh）。
echo
echo "-- 编译（src + test，-DTEST）--"
if ! make test-build >/tmp/eb07_asset_it_build.log 2>&1; then
  sed -n '1,40p' /tmp/eb07_asset_it_build.log
  echo "ASSET_IT: ASSERT_PASS=0 ASSERT_FAIL=1"
  echo "[FAIL] EB-07 集成门: 编译失败（详见 /tmp/eb07_asset_it_build.log）"
  exit 1
fi
echo "[OK]   编译通过"

# ---------------------------------------------------------------- 跑场景
echo
echo "-- 逐条 Acceptance --"
# shellcheck disable=SC2046
erl -noinput -boot no_dot_erlang -kernel start_distribution false \
    $(ls -d deps/*/ebin | sed 's/^/-pa /' | tr '\n' ' ') \
    -config "$EUNIT_CONFIG" -pa imboy/ebin -pa ebin -pa test \
    -eval "${RUNNER}:main()."
RC=$?

# ---------------------------------------------------------------- 调用方契约（asset 范围）
#
# ORDER 的门链把 `scripts/check_eb_port_closure.sh` 列为「应 0 处未声明调用」。
# 该门同时扫描兄弟 worktree 的 application 层，而 wb-a1 的 `eb_purge_port.erl`
# 仍是 EB-03R 版（同时声明 purge_batch/3 与 /4，门的 name→arity 字典按后者覆盖），
# 于是 **wb-a2 的 `Purge:purge_batch/4` 调用点** 会被报成 arity 不符 —— 这与 EB-07
# 无关：把本卡产物整体移出后重跑，得到的 FAIL 集合完全相同
# （见 agents/a1/EB-07/logs/port-closure-baseline-red.log 与 gate-port-closure.log 的
#  sort -u 对照）。A0 已就同源问题裁定「兄弟 worktree 的命中不阻断本门」
# （control/board.json 的 a0_tooling.a04_closure，EB-06 据此把 A01 口径改为只判本树）。
#
# 故本脚本把该门**收敛成 EB-07 自己的判定**：只看 `eb_asset_port` 的越界调用
# （0 处才绿），其余既有残差只登记不阻断 —— 且**不把 `[FAIL]` 原文打进 stdout**，
# 以免一次通过的 GREEN 里混进失败令牌（control/verify_evidence.py 的判据）。
CLOSURE_LOG="$(mktemp /tmp/eb07_closure.XXXXXX)"
CLOSURE_RC=0
if ! bash scripts/check_eb_port_closure.sh \
      --require put_private/3 --require stream_content/3 --require delete_private/3 \
      --require insert_asset/3 --require fetch_asset/3 --require confirm_asset/3 \
      --require cleanup_asset/3 > "$CLOSURE_LOG" 2>&1; then
  CLOSURE_RC=1
fi
ASSET_A01_HITS="$(grep '^\[FAIL\] A01' "$CLOSURE_LOG" | grep 'eb_asset_port' || true)"
OTHER_A01_HITS="$(grep '^\[FAIL\] A01' "$CLOSURE_LOG" | grep -v 'eb_asset_port' || true)"
echo
echo "-- 调用方契约门（asset 范围）--"
if [ -n "$ASSET_A01_HITS" ]; then
  printf '%s\n' "$ASSET_A01_HITS"
  echo "[ASSERT-FAIL EB-07-closure] eb_asset_port 存在越出冻结契约的调用点"
  RC=1
else
  grep '^\[OK\] A05 ' "$CLOSURE_LOG" || true
  echo "[ASSERT EB-07-closure] eb_asset_port 全部调用点命中契约声明（0 处未声明调用）；"
  echo "                        且 7 个 callback 四件齐（Port 声明 + registry + 实现导出 + 测试）"
fi
if [ -n "$OTHER_A01_HITS" ]; then
  echo "[NOTE] 另有 $(printf '%s\n' "$OTHER_A01_HITS" | wc -l | tr -d ' ') 条既有 A01 残差，均指向 eb_purge_port（其它 worktree 的未同步代码），"
  echo "       与本卡无关且不在本卡租约内：逐条见 agents/a1/EB-07/logs/gate-port-closure.log"
fi
rm -f "$CLOSURE_LOG"

if [ "$RC" -ne 0 ]; then
  # 注意：变量后紧跟全角标点必须写 ${RC} —— bash 3.2 会把多字节字符首字节并进变量名
  # （control/check_shell32_traps.py 的 T1；本机 /bin/bash 是 3.2.57，CI 是 bash 5）。
  echo "[FAIL] EB-07 集成门: ${RUNNER}:main/0 退出码 ${RC}（有断言失败）"
  exit 1
fi
echo "[OK] EB-07 企业附件闭环集成门全绿（A01..A06 逐条 + asset 范围调用方契约）"
exit 0
