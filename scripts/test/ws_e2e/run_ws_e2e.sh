#!/usr/bin/env bash
# run_ws_e2e.sh — Q04/AC07/AC08 真 WS 双连接端到端编排（BE-WS-E2E，2026-10-07）
#
# 职责：节点互斥锁 → 编译（eunit 锁保护编译窗口）→ 以 config/sys.p1-boot.config
#（phase1_delivery profile + 隔离库 imboy_p1_20261007t003716z@127.0.0.1:4323）
# boot 9800 节点 → 运行 ws_e2e_test.py 四链 → 停节点 → 释放锁。
# 证据落盘 RUN_ROOT/control/：q04-ws-e2e-evidence.log（协议帧+SQL+HTTP 原始记录）、
# q04-ws-e2e-summary.json、q04-ws-e2e-node.log（节点启动日志）。
#
# 约束：所有外部地址仅 127.0.0.1；不改 config/ 既有文件；绝不 push；
# 节点锁 /tmp/phase1-node.lock 排队等待（另一 agent 也在起节点，绝不换端口）。
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
RUN_ROOT="${RUN_ROOT:-$HOME/.zcode-runs/phase1-20261007T003716Z}"
CTRL="$RUN_ROOT/control"
NODE_LOCK=/tmp/phase1-node.lock
EUNIT_LOCK=/tmp/phase1-eunit.lock
HTTP_PORT=9800
BOOT_ENV=p1-boot   # config/sys.p1-boot.config：phase1_delivery + 隔离库 + 9800

mkdir -p "$CTRL"
EV="$CTRL/q04-ws-e2e-evidence.log"
NODE_LOG="$CTRL/q04-ws-e2e-node.log"

log() { printf '[%s] %s\n' "$(date '+%H:%M:%S')" "$*" | tee -a "$EV"; }

# ---------- 节点互斥锁（任务纪律：排队等待，绝不换端口） ----------
while ! mkdir "$NODE_LOCK" 2>/dev/null; do sleep 5; done
trap 'rmdir "$NODE_LOCK" 2>/dev/null; rmdir "$EUNIT_LOCK" 2>/dev/null' EXIT
log "acquired node lock ${NODE_LOCK}"

# 端口必须空闲（锁内若被占用 = 残留节点，不抢杀 foreign 进程，直接 BLOCKED）
if lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1; then
  log "BLOCKED: port $HTTP_PORT already in use（锁内残留 foreign 监听，不杀未知进程）"
  lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN | tee -a "$EV"
  exit 2
fi

# ---------- 环境指纹（编译时工作区状态如实入档） ----------
{
  echo "===== run_ws_e2e start $(date '+%Y-%m-%dT%H:%M:%S%z') ====="
  echo "== git HEAD =="; git -C "$REPO" rev-parse HEAD
  echo "== git dirty（含其他 agent WIP，如实记录） =="
  git -C "$REPO" status --porcelain
  echo "== boot config =="; git -C "$REPO" check-ignore -q "config/sys.$BOOT_ENV.config" \
    && echo "config/sys.$BOOT_ENV.config gitignored(本地)" || echo "WARN: config tracked"
  echo "== pg =="; pg_isready -h 127.0.0.1 -p 4323
} >> "$EV" 2>&1

# ---------- 编译窗口（与 eunit 串行：编译写 ebin 与他人 eunit VM 互斥） ----------
while ! mkdir "$EUNIT_LOCK" 2>/dev/null; do sleep 5; done
log "acquired eunit lock for compile window"
cd "$REPO" || exit 2
# 首次失败可能是与并行 agent 构建目录的瞬态竞态（曾实测一次
# "No rule to make target 'all'"，同环境复跑成功）——重试一次
MAKE_OK=0
for attempt in 1 2; do
  if IMBOYENV=$BOOT_ENV make -C "$REPO" all >> "$NODE_LOG" 2>&1; then
    MAKE_OK=1; break
  fi
  log "make all attempt ${attempt} failed（10s 后重试）"
  tail -5 "$NODE_LOG" | tee -a "$EV"
  sleep 10
done
if [ "$MAKE_OK" != "1" ]; then
  log "BLOCKED: IMBOYENV=${BOOT_ENV} make all 两次均失败（详见 ${NODE_LOG}）"
  exit 2
fi
rmdir "$EUNIT_LOCK" 2>/dev/null
log "make all OK（relx 组装完成，sys.runtime.config <- sys.${BOOT_ENV}.config）"

# ---------- boot 节点（daemon 模式：console+nohup 会因 stdin EOF 立即退出） ----------
REL_BIN="$REPO/_rel/imboy/bin/imboy"
"$REL_BIN" daemon >> "$NODE_LOG" 2>&1
log "node booting (daemon)；等待 :$HTTP_PORT 就绪…"

READY=0
CODE=000
for i in $(seq 1 90); do
  sleep 2
  CODE=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$HTTP_PORT/api/v1/conversation/mine" || true)
  if [ "$CODE" = "401" ] || [ "$CODE" = "200" ]; then READY=1; break; fi
  # daemon 模式下无 BOOT_PID 可查；靠 epmd 节点名兜底判断进程存活
  if ! (erl -noshell -sname "p1wse2e_probe_$$_$i" -setcookie imboy_local_dev_only \
        -eval "case net_adm:ping('imboy@127.0.0.1') of pong -> halt(0); pang -> halt(1) end" 2>/dev/null); then
    if [ "$i" -gt 5 ]; then
      log "WARN: 节点 ping 不通（第 ${i} 轮），继续等 HTTP 探活"
    fi
  fi
done
if [ "$READY" != "1" ]; then
  log "BLOCKED: 节点 $((i*2))s 内未就绪（最后 HTTP code=${CODE}）"
  tail -40 "$NODE_LOG" >> "$EV"
  "$REL_BIN" stop >/dev/null 2>&1 || true
  exit 2
fi
log "node ready on :$HTTP_PORT (probe code=${CODE})"

# ---------- 四链测试 ----------
python3 "$REPO/scripts/test/ws_e2e/ws_e2e_test.py" \
  --repo "$REPO" \
  --base-url "http://127.0.0.1:$HTTP_PORT" \
  --evidence "$EV"
RC=$?

# ---------- 停节点（release 脚本会连带 heart；stop 后确认端口释放） ----------
"$REL_BIN" stop >> "$NODE_LOG" 2>&1 || true
for i in $(seq 1 15); do
  lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1 || break
  sleep 2
done
if lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1; then
  log "WARN: 端口 $HTTP_PORT 仍被占用，kill boot 进程组兜底"
  PIDS=$(lsof -ti:$HTTP_PORT)
  # shellcheck disable=SC2086
  kill $PIDS 2>/dev/null || true
  sleep 3
fi
lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1 \
  && log "WARN: :$HTTP_PORT 仍 LISTEN" \
  || log "node stopped, port $HTTP_PORT released"

log "ws_e2e exit_code=$RC"
exit $RC
