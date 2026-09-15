#!/usr/bin/env bash
# 本地开发用的「对象存储公网入口」开关。
#
# 背景：elib_oss:presign_get_for_key/3 只认 elib_oss:public_endpoint()，
# 而本地 garage 是 docker 容器，对外地址是局域网明文 http（如 192.168.1.69:3900）。
# 第三方多模态模型抓不到这种地址 ⇒ 视频回课恒 provider_error（智谱 400/1210）。
# 生产 sys.pro.config 把 endpoint 直接指向 https://s3.imboy.pub（本身公网 https），
# 所以这条只在本地需要。
#
# 做法：起一个 cloudflared 快速隧道指向本机 garage，再把运行中节点的
# public_endpoint 热切到隧道域名（application:set_env，不改配置文件、不用重启）。
# SigV4 只签 host（X-Amz-SignedHeaders=host），隧道保留 Host，故签名照旧有效。
# 每次 presign 的有效期见 moya_ai_worker:?VIEW_URL_EXPIRES（600s）。
#
# 用法（在你自己的终端里跑；start 会阻塞占用终端）：
#   scripts/dev_garage_tunnel.sh start     # 起隧道 + 热切 public_endpoint + 三条自证
#   scripts/dev_garage_tunnel.sh status    # 看隧道域名 / 节点当前 public_endpoint
#   scripts/dev_garage_tunnel.sh revert    # 还原 public_endpoint 并关隧道（另一终端可用）
#
# start 阻塞是故意的：Ctrl-C / kill 会触发 trap 自动还原 public_endpoint 并关隧道，
# 不会留下「配置指向一个已死域名」的坑。若要让它在后台常驻，请用
# `nohup setsid ...` 之类的真守护化手段，别只加 &（会被会话回收）。
#
# ⚠️ 打开期间，任何拿到 presign URL 的人都能读到那一个对象（TTL 内）。
#    涉及未成年人作业媒体时，用完立即 revert，别让它过夜。
set -uo pipefail

cd "$(dirname "$0")/.." || exit 1
ROOT="$(pwd)"
LOG="$ROOT/tmp/garage_tunnel.log"
PIDFILE="$ROOT/tmp/garage_tunnel.pid"
STATEFILE="$ROOT/tmp/garage_tunnel.prev_endpoint"
GARAGE_TARGET="${GARAGE_LOCAL:-http://127.0.0.1:3900}"
CF_BIN="${CF_BIN:-/opt/homebrew/opt/cloudflared/bin/cloudflared}"

mkdir -p "$ROOT/tmp"
touch "$LOG"

# --- 节点名 / cookie 从真正生效的 vm_local.args 读，不硬编码 ---
# 注：BSD sed 不支持 BRE 的 \+，会静默取空 → 这里用 awk（ERE）。
ARGS_FILE="$ROOT/config/vm_local.args"
[ -f "$ARGS_FILE" ] || ARGS_FILE="$ROOT/config/vm.args"
NODE="$(awk '/^-name[ \t]+/{print $2; exit}' "$ARGS_FILE")"
COOKIE="$(awk '/^-setcookie[ \t]+/{print $2; exit}' "$ARGS_FILE")"
[ -n "${NODE}" ] && [ -n "${COOKIE}" ] || { echo "✗ 读不到节点名/cookie（源: ${ARGS_FILE}）"; exit 1; }

rpc() { # rpc <eval 串>；串内可用 N 指代目标节点
  erl -noshell -hidden -name imboytun@127.0.0.1 -setcookie "$COOKIE" -eval "
    N = list_to_atom(\"$NODE\"),
    pong = net_adm:ping(N),
    $1,
    halt(0)." 2>&1
}

get_public_endpoint() {
  rpc 'io:format("~ts", [maps:get(public_endpoint, element(2, rpc:call(N, application, get_env, [imboy, garage])), <<"">>)])'
}

set_public_endpoint() {
  rpc "ok = rpc:call(N, application, set_env, [imboy, garage, (element(2, rpc:call(N, application, get_env, [imboy, garage])))#{public_endpoint => <<\"$1\">>}]), io:format(\"ok\")" >/dev/null
}

# 配置文件里的真值（无备份时的兜底还原点）
config_public_endpoint() {
  awk '/public_endpoint =>/{s=$0; sub(/.*<<"/,"",s); sub(/">>.*/,"",s); print s; exit}' "$ROOT/config/sys.local.config"
}

restore_and_exit() {
  echo
  echo "→ 收到退出信号，正在还原…"
  do_revert
  exit 0
}

do_revert() {
  if [ -f "$STATEFILE" ]; then
    set_public_endpoint "$(cat "$STATEFILE")"
    echo "✓ public_endpoint 已还原为 $(cat "$STATEFILE")"
    rm -f "$STATEFILE"
  else
    echo "! 无备份文件，未改动 public_endpoint"
  fi
  if [ -f "$PIDFILE" ]; then
    kill "$(cat "$PIDFILE")" 2>/dev/null && echo "✓ 隧道已关闭"
    rm -f "$PIDFILE"
  fi
  echo "  现状 = $(get_public_endpoint)"
}

case "${1:-status}" in
  start)
    if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
      echo "✓ 隧道已在跑（pid $(cat "$PIDFILE")）"; exit 0
    fi
    [ -x "$CF_BIN" ] || { echo "✗ 找不到 cloudflared（CF_BIN=$CF_BIN）"; exit 1; }

    CUR="$(get_public_endpoint)"
    case "$CUR" in
      *trycloudflare.com)
        if [ -f "$STATEFILE" ]; then
          echo "  当前已是隧道值，保留既有备份：$(cat "$STATEFILE")"
        else
          FB="$(config_public_endpoint)"
          printf '%s' "$FB" > "$STATEFILE"
          echo "  当前已是隧道值且无备份 → 以配置文件值兜底作为还原点：${FB}"
        fi ;;
      *)
        printf '%s' "$CUR" > "$STATEFILE"
        echo "  原 public_endpoint = ${CUR}（已存 ${STATEFILE}）" ;;
    esac

    "$CF_BIN" tunnel --url "$GARAGE_TARGET" --no-autoupdate > "$LOG" 2>&1 &
    CF_PID=$!
    echo $CF_PID > "$PIDFILE"
    trap restore_and_exit INT TERM HUP

    TUN=""
    for _ in $(seq 1 30); do
      TUN="$(sed -n 's/.*\(https:\/\/[a-z0-9-]*\.trycloudflare\.com\).*/\1/p' "$LOG" | head -1)"
      [ -n "$TUN" ] && break
      sleep 1
    done
    [ -n "$TUN" ] || { echo "✗ 30s 内没拿到隧道域名，看 $LOG"; do_revert; exit 1; }
    echo "  隧道域名 = ${TUN}"

    set_public_endpoint "$TUN"
    echo "✓ public_endpoint 已热切（未改配置文件、未重启）"

    # 自证1：节点现在签出来的 host 必须是隧道域名（否则模型仍拿到局域网地址）
    SIGNED="$(rpc 'io:format("~ts", [rpc:call(N, elib_oss, presign_get_for_key, [<<"probe/key">>, 60])])')"
    case "$SIGNED" in
      "$TUN"/*) echo "  自证1 presign host ✓ = ${TUN}" ;;
      *) echo "  自证1 ✗ 签出来的还是 ${SIGNED%%/probe/*}" ;;
    esac

    # 自证2：新域名的 DNS 要几十秒才生效，必须轮询（否则会误判「隧道不通」）
    CODE=000
    for i in $(seq 1 12); do
      CODE="$(curl -s -o /dev/null -w '%{http_code}' --max-time 15 "${TUN}/")"
      [ "$CODE" != 000 ] && break
      sleep 5
    done
    case "$CODE" in
      403) echo "  自证2 GET / → 403 ✓（隧道已通到 Garage，Host 未被改写）" ;;
      000) echo "  自证2 ✗ 60s 内 DNS/连接仍不通（云侧尚未发布）" ;;
      *)   echo "  自证2 ? GET / → HTTP ${CODE}" ;;
    esac

    # 自证3（可选）：给真实对象键时，用隧道域名取一次，206 才说明模型也能抓到
    if [ -n "${PROBE_KEY:-}" ]; then
      PK_URL="$(PROBE_KEY="$PROBE_KEY" rpc 'io:format("~ts", [rpc:call(N, elib_oss, presign_get_for_key, [list_to_binary(os:getenv("PROBE_KEY")), 120])])')"
      RCODE="$(curl -s -o /dev/null -w '%{http_code}' --max-time 25 -H 'Range: bytes=0-0' "$PK_URL")"
      case "$RCODE" in
        206) echo "  自证3 取对象 → 206 ✓（签名+Host+对象全通，模型应能抓到）" ;;
        *)   echo "  自证3 ✗ 取对象 → HTTP ${RCODE}" ;;
      esac
    else
      echo "  自证3 跳过（未给 PROBE_KEY=<object_key>）"
    fi

    echo
    echo "隧道运行中（pid ${CF_PID}）。Ctrl-C 退出会自动还原；"
    echo "也可以在另一终端跑 scripts/dev_garage_tunnel.sh revert。"
    wait "$CF_PID"
    echo "隧道进程已退出，自动还原…"
    do_revert
    ;;

  status)
    echo "隧道域名      = $(sed -n 's/.*\(https:\/\/[a-z0-9-]*\.trycloudflare\.com\).*/\1/p' "$LOG" 2>/dev/null | tail -1)"
    echo "隧道进程      = $([ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null && echo "pid $(cat "$PIDFILE") 在跑" || echo '未运行')"
    echo "节点 public_endpoint = $(get_public_endpoint)"
    echo "备份的原始值  = $(cat "$STATEFILE" 2>/dev/null || echo '(无)')"
    ;;

  revert)
    do_revert
    ;;

  *)
    echo "用法: $0 {start|status|revert}"; exit 1 ;;
esac
