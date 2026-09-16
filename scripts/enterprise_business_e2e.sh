#!/usr/bin/env bash
# EB-11 —— Foundation **独立 E2E 与安全审查**门（plan §8 EB-11 / §2.1 全链）。
#
# 依据：
#   * `control/plan.snapshot.md` §8 EB-11、§2.1（本地必须复现的 18 条）
#   * `control/required-acceptance.tsv` 的 `EB-11-A01..A06`
#   * `agents/a5/EB-11/ORDER.md` §1/§4/§5（已知阻断 F1/F2/F6 的处置口径与证据清单）
#
# ## 本脚本做什么
#
#   1. **前置门**：本地 scratch 配置在位、库名必须落在本 run 的 scratch 命名空间
#      （`imboy_eb_w2`，**绝不**碰共享 `imboy_v1`）、主机必须是 loopback、门入口在位；
#      并核对 `HEAD` 与作业书冻结的 Base 逐字一致。
#   2. **隔离端口**：为本次运行选一个空闲端口（`HTTP_PORT`），避免与其它 worker 的
#      监听器 `eaddrinuse` 冲突（企业套件与 E2E 必须**串行**）。
#   3. **测试构建**：`make test-build`（带 `-DTEST`；先跑 `make compile` 会让 ebin 变成
#      无 TEST 版并引发成批 `undef` 假失败 —— 见 `control/preflight_eunit_build.sh`）。
#   4. **跑 E2E**：`erl ... -eval 'eb_e2e_runner:main().'`，逐条打印 `[ASSERT EB-11-Axx.n]`；
#      全绿落 `logs/green.log`，有失败落 `logs/red.log`（**不**把失败混进 green）。
#   5. **residual 四项**（进程 / 端口 / 对象 / DB）与 **A03 机械扫描**（日志与证据目录里
#      不得出现金丝雀明文或存储能力串）的原始输出落 `logs/`。
#
# ## 边界声明（不得夸大）
#
#   * 对象存储侧是 EB-07 的**本地替身** adapter ⇒ 口径固定
#     `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
#   * 通过后**仅可**声明 `LOCAL_FOUNDATION_PASS`；不得声称 production ready /
#     真实客户验收 / 发布通过。
#   * F1/F2/F6 的测试侧补偿点与「因 F6 无法真实验证的端点」见 RESULT.json 与 RUN ORDER；
#     本脚本不做任何降级式跳过。
#
# 用法：
#   bash scripts/enterprise_business_e2e.sh
#   EB11_EVIDENCE_DIR=/tmp/eb11 bash scripts/enterprise_business_e2e.sh   # 覆盖证据目录
#   EB11_HTTP_PORT=19801 bash scripts/enterprise_business_e2e.sh          # 指定端口
set -uo pipefail

cd "$(dirname "$0")/.."
WT="$(pwd)"
RUN_ROOT="${EB11_RUN_ROOT:-$(cd "$WT/../.." && pwd)}"
EVID="${EB11_EVIDENCE_DIR:-$RUN_ROOT/agents/a5/EB-11/logs}"
EUNIT_CONFIG="${EUNIT_CONFIG:-config/sys.local.eb}"
CONFIG_FILE="${EUNIT_CONFIG}.config"
BASE_SHA="${EB11_BASE_SHA:-611d6752231499d31f0b8f282660b6113f585d2d}"
EXPECT_DB="${EB11_SCRATCH_DB:-imboy_eb_w2}"

mkdir -p "$EVID"
# 每次运行都从**干净**的证据目录开始：残留的旧扫描输出会让「金丝雀扫描」自命中
# （旧输出里带着上一次 run 的金丝雀作为检索键），从而产生不可解释的假命中。
rm -rf "$EVID/internal"
rm -f "$EVID"/*.txt
# 注意：**不删** red.log/green.log —— 正规顺序是「先取 RED、再取 GREEN」，
# 本脚本在末尾用 mv 覆盖 green.log（红绿顺序由 mtime 自然保证）。
TMPLOG="$(mktemp "/tmp/eb11_e2e.XXXXXX")"

echo "== EB-11 Foundation 独立 E2E 门 =="
echo "worktree : $WT"
echo "run_root : $RUN_ROOT"
echo "evidence : $EVID"
echo "config   : $CONFIG_FILE"

PRE_FAIL=0
if [ ! -f "$CONFIG_FILE" ]; then
  echo "[E2E-PRE] 缺少本地 scratch 配置 $CONFIG_FILE"
  echo "         先跑: bash $RUN_ROOT/control/a0_eb_scratch_env.sh worker <worktree> $EXPECT_DB"
  PRE_FAIL=$((PRE_FAIL + 1))
fi
if [ ! -f test/features/enterprise_business/e2e/eb_e2e_runner.erl ]; then
  echo "[E2E-PRE] 缺少门入口 test/features/enterprise_business/e2e/eb_e2e_runner.erl"
  PRE_FAIL=$((PRE_FAIL + 1))
fi
if [ ! -d src/features/enterprise_business ]; then
  echo "[E2E-PRE] 缺少企业特性源码 src/features/enterprise_business"
  PRE_FAIL=$((PRE_FAIL + 1))
fi

# 库名与主机：scratch 命名空间 + loopback（绝不触碰共享库/远端实例）
DB_IN_CONFIG="$(python3 - "$CONFIG_FILE" <<'PY' 2>/dev/null
import re, sys
try:
    t = open(sys.argv[1], encoding='utf-8', errors='replace').read()
except OSError:
    print(""); raise SystemExit(0)
m = re.findall(r'database\s*=>\s*"([^"]+)"', t)
print(m[0] if m else "")
PY
)"
HOST_IN_CONFIG="$(python3 - "$CONFIG_FILE" <<'PY' 2>/dev/null
import re, sys
try:
    t = open(sys.argv[1], encoding='utf-8', errors='replace').read()
except OSError:
    print(""); raise SystemExit(0)
m = re.findall(r'host\s*=>\s*"([^"]+)"', t)
print(m[0] if m else "")
PY
)"
echo "db       : ${DB_IN_CONFIG:-<unset>} @ ${HOST_IN_CONFIG:-<unset>}"
case "${DB_IN_CONFIG}" in
  "$EXPECT_DB") : ;;
  "") echo "[E2E-PRE] 未能从配置读出库名（文件缺失或格式变化）"; PRE_FAIL=$((PRE_FAIL + 1)) ;;
  *) echo "[E2E-PRE] 库名不是本 run 的 scratch 库 ${EXPECT_DB}（实测 ${DB_IN_CONFIG}）——拒绝运行"
     PRE_FAIL=$((PRE_FAIL + 1)) ;;
esac
case "${DB_IN_CONFIG}" in
  imboy_v1|postgres|template*) echo "[E2E-PRE] 拒绝操作共享/系统库: $DB_IN_CONFIG"; PRE_FAIL=$((PRE_FAIL + 1)) ;;
esac
case "${HOST_IN_CONFIG}" in
  127.0.0.1|localhost|::1) : ;;
  *) echo "[E2E-PRE] 主机不是 loopback（${HOST_IN_CONFIG}）——scratch 库必须在本地"; PRE_FAIL=$((PRE_FAIL + 1)) ;;
esac

HEAD_SHA="$(git rev-parse HEAD 2>/dev/null || echo unknown)"
echo "head     : $HEAD_SHA"
if [ "$HEAD_SHA" != "$BASE_SHA" ]; then
  echo "[E2E-PRE] HEAD 与冻结 Base 不一致（期望 ${BASE_SHA}）"
  PRE_FAIL=$((PRE_FAIL + 1))
fi

if [ "$PRE_FAIL" -ne 0 ]; then
  echo
  echo "E2E: ASSERT_PASS=0 ASSERT_FAIL=$PRE_FAIL"
  echo "[FAIL] EB-11 E2E 前置检查未通过（$PRE_FAIL 项）"
  exit 1
fi
echo "[OK]   前置检查通过（scratch 库 + loopback + 门入口 + HEAD=Base）"

# ---------------------------------------------------------------- 隔离端口
pick_port() {
  python3 - <<'PY'
import socket
s = socket.socket()
s.bind(("127.0.0.1", 0))
print(s.getsockname()[1])
s.close()
PY
}
PORT="${EB11_HTTP_PORT:-$(pick_port)}"
if ! [ "$PORT" -gt 0 ] 2>/dev/null; then
  echo "[FAIL] EB-11 E2E: 无法选择隔离端口"
  exit 1
fi
echo "port     : ${PORT}（隔离；企业套件与 E2E 必须串行）"

# ---------------------------------------------------------------- 基线快照（供 residual 比对）
sed -n '1,200p' /dev/null > "$EVID/a05-residual-process.txt"
pgrep -fl beam.smp > "$EVID/internal-pgrep-before.txt" 2>/dev/null || true
lsof -nP -iTCP -sTCP:LISTEN > "$EVID/internal-listen-before.txt" 2>/dev/null || true

# ---------------------------------------------------------------- 测试构建
echo
echo "-- 测试构建（-DTEST）--"
if ! make test-build > "$EVID/build.log" 2>&1; then
  sed -n '1,40p' "$EVID/build.log"
  echo "E2E: ASSERT_PASS=0 ASSERT_FAIL=1"
  echo "[FAIL] EB-11 E2E: 测试构建失败（详见 $EVID/build.log）"
  exit 1
fi
if ! bash "$RUN_ROOT/control/preflight_eunit_build.sh" "$WT" > "$EVID/preflight.log" 2>&1; then
  cat "$EVID/preflight.log"
  echo "[FAIL] EB-11 E2E: ebin 不是 -DTEST 构建"
  exit 1
fi
tail -1 "$EVID/preflight.log"

# --- A0 播种文件的新鲜度守卫 -----------------------------------------------
# 本 run 与播种的真实坑（ORDER §环境坑已警告、本次实撞）：A0 从上游卡复制进来的文件
# **mtime 是旧的**（如 organization_member_logic.erl 保持上游 19:48），而 erlang.mk 按
# mtime 判新鲜 ⇒ 既有的同模块 beam 更新，`make test-build` **跳过重编**，
# 运行时于是拿到「没有 suspend/3 的旧 beam」并报 undef —— 看起来像 EB-08 的缺陷，
# 其实是构建顺序伪影。判据用**导出金丝雀**（不依赖 mtime）：缺则 touch 源码重编一次。
CORE_CANARY="src/logic/organization_member_logic.erl"
if [ -f "$CORE_CANARY" ]; then
  CORE_OK="$(
    erl -noinput -boot no_dot_erlang -kernel start_distribution false -pa ebin \
      -eval 'io:format("~p", [lists:member({suspend,3}, organization_member_logic:module_info(exports))]), halt(0).' \
      2>/dev/null || echo false
  )"
  if [ "$CORE_OK" != "true" ]; then
    echo "[E2E-PRE] 检测到播种文件未被重编（$CORE_CANARY 的 suspend/3 不在 ebin 中）"
    echo "         ⇒ 按 ORDER §环境坑 的口径 touch 源码（**内容不变**）后重编一次"
    touch "$CORE_CANARY"
    make test-build >> "$EVID/build.log" 2>&1
  fi
fi
echo "[OK]   测试构建通过且 ebin 为 -DTEST 构建（含播种 Core 的新鲜度守卫）"

# ---------------------------------------------------------------- 跑 E2E
echo
echo "-- 运行 eb_e2e_runner:main/0 --"
HTTP_PORT="$PORT" \
EB11_EVIDENCE_DIR="$EVID" \
EB11_HEAD="$HEAD_SHA" \
EB11_BASE_SHA="$BASE_SHA" \
erl -noinput -boot no_dot_erlang -kernel start_distribution false \
    $(ls -d deps/*/ebin | sed 's/^/-pa /' | tr '\n' ' ') \
    -config "$EUNIT_CONFIG" -pa imboy/ebin -pa ebin -pa test \
    -eval 'eb_e2e_runner:main().' > "$TMPLOG" 2>&1
E2E_RC=$?

# ---------------------------------------------------------------- residual / 扫描
echo
echo "-- residual 与机械扫描 --"
RES_FAIL=0

# 进程：本次运行只允许一个 erl（脚本自身调用），退出后 beam.smp 集合应与基线一致
pgrep -fl beam.smp > "$EVID/internal-pgrep-after.txt" 2>/dev/null || true
{
  echo "# process residual（pgrep -fl beam.smp）"
  echo "## before"
  cat "$EVID/internal-pgrep-before.txt"
  echo "## after"
  cat "$EVID/internal-pgrep-after.txt"
} > "$EVID/a05-residual-process.txt"
if diff -q "$EVID/internal-pgrep-before.txt" "$EVID/internal-pgrep-after.txt" >/dev/null 2>&1; then
  echo "[OK]   process residual = 0（运行前后 beam.smp 集合一致）"
else
  echo "[WARN] process residual: 运行前后 beam.smp 集合不同（逐行见 $EVID/a05-residual-process.txt）"
  RES_FAIL=$((RES_FAIL + 1))
fi

# 端口：隔离端口在运行结束后必须无监听
{
  echo "# port residual（lsof -nP -iTCP:$PORT -sTCP:LISTEN）"
  lsof -nP -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || true
} > "$EVID/a05-residual-port.txt"
PORT_LINES="$(grep -c "LISTEN" "$EVID/a05-residual-port.txt" 2>/dev/null || true)"
if [ "${PORT_LINES:-0}" -gt 1 ]; then
  echo "[WARN] port residual：隔离端口 ${PORT} 仍有监听（见 $EVID/a05-residual-port.txt）"
  RES_FAIL=$((RES_FAIL + 1))
else
  echo "[OK]   port residual = 0（隔离端口 ${PORT} 无监听）"
fi

# 日志扫描：金丝雀明文 + 存储能力串（storage URL / object key）
CANARY_FILE="$EVID/internal/canaries.txt"
LOG_SCAN="$EVID/a03-log-scan.txt"
{
  echo "# 日志扫描（runner stdout/stderr：${TMPLOG}）"
  echo "## canary 明文命中"
  if [ -f "$CANARY_FILE" ]; then
    while IFS= read -r C; do
      [ -n "$C" ] || continue
      N="$(grep -c -F "$C" "$TMPLOG" 2>/dev/null || true)"
      echo "canary=$C hits=${N:-0}"
    done < "$CANARY_FILE"
  else
    echo "(canary 清单缺失：$CANARY_FILE)"
  fi
  echo "## 存储能力串命中（object_key/garage/X-Amz-/presign/bucket；仅扫**非 harness 叙述行**）"
  grep -v -E "^\[ASSERT|^--|^==" "$TMPLOG" \
    | grep -o -E "object_key|garage|X-Amz-|presign|bucket" | sort | uniq -c || true
} > "$LOG_SCAN" 2>&1
CANARY_HITS="$(grep -c -E "hits=[1-9]" "$LOG_SCAN" 2>/dev/null || true)"
STORAGE_HITS="$(grep -c -E "^ *[0-9]+ (object_key|garage|X-Amz-|presign|bucket)$" "$LOG_SCAN" 2>/dev/null || true)"
if [ "${CANARY_HITS:-0}" -eq 0 ] && [ "${STORAGE_HITS:-0}" -eq 0 ]; then
  echo "[OK]   日志无金丝雀明文、无存储能力串（见 ${LOG_SCAN}）"
else
  echo "[WARN] 日志扫描命中：canary=$CANARY_HITS storage=${STORAGE_HITS}（见 ${LOG_SCAN}）"
  RES_FAIL=$((RES_FAIL + 1))
fi

# 证据目录扫描：排除 internal/（金丝雀清单本身）与两份扫描输出（它们必然逐字含金丝雀作为检索键）
EV_SCAN="$EVID/a03-evidence-scan.txt"
{
  echo "# 证据目录扫描（${EVID}，排除 internal/）"
  echo "## canary 明文命中"
  if [ -f "$CANARY_FILE" ]; then
    while IFS= read -r C; do
      [ -n "$C" ] || continue
      HITS="$(grep -rl -F "$C" "$EVID" --exclude-dir=internal \
        --exclude=a03-log-scan.txt --exclude=a03-evidence-scan.txt 2>/dev/null || true)"
      echo "canary=$C files=${HITS:-none}"
    done < "$CANARY_FILE"
  else
    echo "(canary 清单缺失)"
  fi
  echo "## 存储能力串命中"
  grep -r -o -E "object_key|garage|X-Amz-|presign" "$EVID" --exclude-dir=internal \
    --exclude=a03-log-scan.txt --exclude=a03-evidence-scan.txt 2>/dev/null | sort | uniq -c || true
} > "$EV_SCAN" 2>&1
EV_HITS="$(grep -c -E "files=[^n]" "$EV_SCAN" 2>/dev/null || true)"
if [ "${EV_HITS:-0}" -eq 0 ]; then
  echo "[OK]   证据目录无金丝雀明文（见 ${EV_SCAN}）"
else
  echo "[WARN] 证据扫描命中：canary=${EV_HITS}（见 ${EV_SCAN}）"
  RES_FAIL=$((RES_FAIL + 1))
fi

# DB / 集群级 residual（只读清单：库与角色；不打印口令）
PG_DUMP="$EVID/a05-residual-db-cluster.txt"
{
  echo "# 集群级对象清单（只读）"
  python3 - "$CONFIG_FILE" <<'PY' 2>&1 || true
import re, subprocess, sys
try:
    t = open(sys.argv[1], encoding='utf-8', errors='replace').read()
except OSError:
    print("config unreadable")
    raise SystemExit(0)

def g(pat, dflt=None):
    m = re.search(pat, t)
    return m.group(1) if m else dflt

host = g(r'host\s*=>\s*"([^"]+)"', "127.0.0.1")
port = g(r'port\s*=>\s*(\d+)', "5432")
user = g(r'username\s*=>\s*"([^"]+)"')
pwd = g(r'password\s*=>\s*"([^"]+)"')
db = g(r'database\s*=>\s*"([^"]+)"')
env = dict(__import__("os").environ, PGPASSWORD=pwd or "")
for label, sql in (("databases", "SELECT datname FROM pg_database WHERE datistemplate=false ORDER BY datname"),
                   ("roles", "SELECT rolname FROM pg_roles WHERE rolname LIKE 'imboy%' ORDER BY rolname")):
    out = subprocess.run(
        ["psql", "-h", host, "-p", port, "-U", user, "-d", db, "-tAc", sql],
        env=env, capture_output=True, text=True)
    print("[%s]" % label)
    print(out.stdout.strip() or out.stderr.strip())
PY
} > "$PG_DUMP" 2>&1
echo "[OK]   DB/集群级清单落盘（见 ${PG_DUMP}）"

# ---------------------------------------------------------------- 结论
echo
if [ "$E2E_RC" -eq 0 ]; then
  mv "$TMPLOG" "$EVID/green.log"
  cat "$EVID/green.log"
  echo
  echo "[OK] EB-11 Foundation 独立 E2E 全绿（A01..A06 逐条 + residual 四项 + 机械扫描）"
  echo "口径: adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN; LOCAL_FOUNDATION_PASS"
  if [ "$RES_FAIL" -ne 0 ]; then
    echo "[WARN] residual/扫描阶段有 $RES_FAIL 项需要人工复核（见 $EVID/A05、A03 原始输出）"
    exit 1
  fi
  exit 0
else
  mv "$TMPLOG" "$EVID/red.log"
  cat "$EVID/red.log"
  echo
  echo "E2E: ASSERT_FAIL>0 或运行异常（详见 $EVID/red.log）"
  echo "[FAIL] EB-11 Foundation 独立 E2E: eb_e2e_runner:main/0 退出码 ${E2E_RC}"
  exit 1
fi
