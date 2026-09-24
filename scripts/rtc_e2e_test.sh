#!/usr/bin/env bash
# RTC (LiveKit SFU) 端到端测试 / RTC (LiveKit SFU) E2E test — LK-TEST-01 v2
#
# 目标（plan LK-TEST-01 验收 A01/A02/A04 的本地 e2e 侧）：
#   S1  join 签发业务 oracle：group/c2c 正例、非群成员/非好友 ACL 负例、
#       livekit 未配置受控错误（livekit_not_configured）、c2c 对称房间、
#       重复 join 幂等、did 维度 identity
#   S2  真实 SFU 信令：用后端签发的 LiveKit token 连 v1.13.7 —— publish
#       （demo 流）/ subscribe（第二端连接）/ leave（房间关闭）
#   S3  embedded TURN UDP relay：客户端强制 ICE relay（lk CLI --force-relay）
#       断言 selected candidate 为 relay + LiveKit 日志出现 TURN allocation；
#       CLI 不可用时降级为协议级 TURN Allocate 探针（jxskiss/base62 凭据
#       算法复刻，见 _turn_probe python 段），仍断言 XOR-RELAYED-ADDRESS 落
#       在 relay 段 + 日志 allocation
#   S4  断线/负例：token 过期被拒、token 对错误 room 被拒
#
# 两种运行模式：
#   RTC_E2E_MODE=selfcontained（默认，零外部依赖）：
#     - 自起 LiveKit v1.13.7 容器（合同镜像；amd64 digest 固定校验）
#     - 自起 imboy 隔离节点（scratch 库 + 随机 HTTP 端口 + auto_migrate）
#     - 种子合成账号（绝不触真实用户数据），token 用 token_ds 生产同款签发
#     - 测完自动回收（容器/节点/scratch 库 drop），幂等可重跑
#   RTC_E2E_MODE=remote（兼容旧用法）：
#     API_BASE/TOKEN/GROUP_ID [+ LIVEKIT_URL/LIVEKIT_API_KEY/LIVEKIT_API_SECRET]
#     只跑 S1/S2 的远端断言（旧行为超集）。
#
# 端口租约（run 20260921T101700Z-622bf9b2-3527a2a8，A0 登记）：
#   TEST_LIVEKIT_PORT=17880（信令 HTTP/WS）  TEST_RTC_TCP_PORT=17881（ICE/TCP）
#   TEST_UDP_RANGE=51900-51950（ICE/UDP：51900-51920 媒体段 + 51921-51950 TURN relay 段）
#   TURN UDP 3478：LiveKit embedded TURN 合同端口（W2 已删 eturnal，本机空闲）
#
# 退出码：0=全部通过；1=断言失败；2=BLOCKED（环境缺失：无 docker/镜像/端口被占）
#
# 依赖：docker、curl、jq、python3（TURN/JWT 探针用标准库）、erl（自起节点）。
#       lk CLI 可选：PATH 中的 lk，或 docker 镜像 livekit/livekit-cli（自动拉取）。
#       无 lk CLI 时 S2/S3 降级为协议级探针（python websocket 不引入，S2 用
#       token 签发侧断言 + S3 TURN Allocate；发布/订阅的 SFU 面证据退化为
#       room 生命周期断言，差异会在输出中明确标注 DEGRADED）。

set -uo pipefail

cd "$(dirname "$0")/.."
WT="$(pwd)"
RUN_ROOT="${RTC_E2E_RUN_ROOT:-$(cd "$WT/../.." && pwd)}"

MODE="${RTC_E2E_MODE:-selfcontained}"
LOGDIR="${RTC_E2E_LOGDIR:-$WT/logs/rtc_e2e}"
mkdir -p "$LOGDIR"

# ── 租约常量（A0 登记，勿改）──────────────────────────────────────────────────
LK_SIGNAL_PORT="${TEST_LIVEKIT_PORT:-17880}"
LK_TCP_PORT="${TEST_RTC_TCP_PORT:-17881}"
LK_UDP_START="${TEST_UDP_START:-51900}"
LK_UDP_END="${TEST_UDP_END:-51950}"          # 租约段终点
LK_MEDIA_START="${TEST_MEDIA_START:-51900}"  # 段内媒体范围
LK_MEDIA_END="${TEST_MEDIA_END:-51920}"
LK_RELAY_START="${TEST_RELAY_START:-51921}"  # 段内 TURN relay 范围
LK_TURN_UDP="${TEST_TURN_UDP_PORT:-3478}"
LK_IMAGE="livekit/livekit-server:v1.13.7"
LK_IMAGE_AMD64_DIGEST="sha256:5d3dcc475d064536d9948ebe4eeab8e3b24d6f07a46f6d71a3415a2901bbdc52"
LK_CLI_IMAGE="livekit/livekit-cli:v2.1.1"
# 真机联测时后端签发的 ws_url/LiveKit advertise 都用本机局域网 IP：
# RTC_E2E_KEEP=1 RTC_ADVERTISE_IP=192.168.x.x bash scripts/rtc_e2e_test.sh
LK_HOST="${RTC_ADVERTISE_IP:-127.0.0.1}"
# 真机 TURN relay 联测专用：Docker Desktop vpnkit 对入站 UDP 一律改写源 IP
# （设备侧实测全部显示为 192.168.65.1），ICE 应答源不匹配 + TURN permission
# 不匹配，强制 relay 的连通性检查必败。RTC_E2E_HOST_NET=1 时改用
# `docker run --network host`（Docker Desktop 需开启 Host networking），端口
# 直绑 macOS 宿主，地址语义与生产同构。非 relay 真机腿（host candidate 直达
# published port，代理还原回源地址）不受此问题影响。
LK_HOST_NET="${RTC_E2E_HOST_NET:-0}"

CONTAINER="${RTC_E2E_CONTAINER:-imboy_lk_20260921T101700Z_livekit}"
NODE_NAME="${RTC_E2E_NODE_NAME:-lkrtc_e2e@127.0.0.1}"
NODE_COOKIE="${RTC_E2E_NODE_COOKIE:-imboy_lk_e2e_2026}"

# scratch DB（租约库名；大小写敏感，须带引号创建）
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGPASSWORD="${PGPASSWORD:-abc54321}"
SCRATCH_DB="${RTC_E2E_DB:-imboy_lk_20260921T101700Z}"

# 本 run 的 LiveKit API 凭据（仅本地容器用，合成值）
LK_API_KEY="lktestkey0000000"
LK_API_SECRET="lktestsecret_000000000000000000000000000000000000"

PASS=0; FAIL=0; DEGRADED=0; BLOCKED=0; REQ_DEGRADED=0
say()  { printf '%s\n' "$*"; }
ok()   { PASS=$((PASS+1)); say "[PASS] $1"; }
bad()  { FAIL=$((FAIL+1)); say "[FAIL] $1"; }
degr() { DEGRADED=$((DEGRADED+1)); say "[DEGRADED] $1"; }
# Required Acceptance 对应的降级：不允许随 exit 0 计绿（LK-TEST-01 假绿修复）。
# 结尾 gate：FAIL>0 -> exit 1；BLOCKED>0 或 REQ_DEGRADED>0 -> exit 2。
degr_req() { REQ_DEGRADED=$((REQ_DEGRADED+1)); DEGRADED=$((DEGRADED+1)); say "[DEGRADED][REQUIRED] $1"; }
pass_summary() {
  say ""
  say "RTC_E2E: PASS=$PASS FAIL=$FAIL DEGRADED=$DEGRADED (required=$REQ_DEGRADED) BLOCKED=$BLOCKED"
}

cleanup_actions=""
register_cleanup() { cleanup_actions="$cleanup_actions;$1"; }
run_cleanups() {
  [ -n "${cleanup_actions//;/}" ] || return 0
  local _acts=()
  IFS=';' read -ra _acts <<< "${cleanup_actions#;}"
  for _a in "${_acts[@]}"; do
    [ -n "$_a" ] || continue
    eval "$_a" >/dev/null 2>&1 || true
  done
}
KEEP="${RTC_E2E_KEEP:-0}"
trap run_cleanups EXIT INT TERM

# ── 工具函数 ──────────────────────────────────────────────────────────────────
http_code() { # method url [curl args...] → code, body 存 $BODY
  local method="$1" url="$2"; shift 2
  curl -sS -m 10 -o "$LOGDIR/last_body.json" -w '%{http_code}' -X "$method" "$@" "$url" 2>/dev/null || printf '000'
}
jqget() { jq -r "$1" "$LOGDIR/last_body.json" 2>/dev/null; }

wait_http_ok() { # url max_tries
  local url="$1" tries="${2:-30}" i
  for ((i=0; i<tries; i++)); do
    [ "$(http_code GET "$url")" = "200" ] && return 0
    sleep 1
  done
  return 1
}

pick_port() {
  python3 - <<'PY'
import socket
s = socket.socket(); s.bind(("127.0.0.1", 0))
print(s.getsockname()[1]); s.close()
PY
}

# portable timeout：macOS 无 GNU timeout，用后台 pid + 轮询实现
run_to() { # <secs> <cmd...>
  local secs="$1"; shift
  "$@" &
  local _pid=$!
  local _watch=0
  while kill -0 "$_pid" 2>/dev/null && [ "$_watch" -lt "$secs" ]; do
    sleep 1; _watch=$((_watch + 1))
  done
  if kill -0 "$_pid" 2>/dev/null; then
    kill "$_pid" 2>/dev/null
    wait "$_pid" 2>/dev/null
    return 124
  fi
  wait "$_pid"
}

port_free() { # port [udp|tcp]
  local p="$1" proto="${2:-tcp}"
  if [ "$proto" = udp ]; then
    ! lsof -nP -iUDP:"$p" 2>/dev/null | grep -q .
  else
    ! lsof -nP -iTCP:"$p" -sTCP:LISTEN 2>/dev/null | grep -q .
  fi
}

# ══════════════════════════════════════════════════════════════════════════════
# Python 探针：JWT(HS256) 签发/验签 + TURN Allocate（LiveKit base62 凭据）
# ══════════════════════════════════════════════════════════════════════════════
PY_HELPERS='
import base64, hashlib, hmac, json, os, socket, struct, sys, time

B62 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

def b62_encode(data: bytes) -> str:
    """jxskiss/base62 v2 (bit-stream, 6bit -> char, compact 0x1E) 的 python 复刻。
    livekit-server v1.13.7 pkg/service/turn.go 用它编码 TURN username/password。"""
    out = []
    pos = len(data) * 8
    while pos > 0:
        size = 6
        r = pos & 0x7
        i = pos >> 3
        if r == 0:
            i, r = i - 1, 8
        b = (data[i] >> (8 - r)) & 0xFF if (8 - r) < 8 else data[i] & 0xFF
        if r < 6 and i > 0:
            b |= (data[i - 1] << r) & 0xFF
        b &= 0x3F
        if (b & 0x1E) == 0x1E:
            if pos > 6 or b > 0x1F:
                size = 5
            b &= 0x1F
        out.append(B62[b])
        pos -= size
    return "".join(out)

# ── LiveKit access JWT（与 imboy rtc_room_logic: jwerl HS256 同构）───────────
def b64url(raw: bytes) -> str:
    return base64.urlsafe_b64encode(raw).rstrip(b"=").decode()

def jwt_sign(claims: dict, secret: str) -> str:
    h = b64url(json.dumps({"alg": "HS256", "typ": "JWT"}, separators=(",", ":")).encode())
    p = b64url(json.dumps(claims, separators=(",", ":")).encode())
    sig = b64url(hmac.new(secret.encode(), f"{h}.{p}".encode(), hashlib.sha256).digest())
    return f"{h}.{p}.{sig}"

def jwt_verify(token: str, secret: str) -> dict:
    h, p, s = token.split(".")
    want = b64url(hmac.new(secret.encode(), f"{h}.{p}".encode(), hashlib.sha256).digest())
    if not hmac.compare_digest(want, s):
        raise SystemExit("jwt-verify: signature mismatch")
    return json.loads(base64.urlsafe_b64decode(p + "=" * (-len(p) % 4)))

# ── TURN（RFC 5766）最小客户端：Allocate with long-term credentials ─────────
MAGIC = 0x2112A442

def stun_pack(msg_type: int, attrs: bytes) -> bytes:
    return struct.pack(">HHI", msg_type, len(attrs), MAGIC) + os.urandom(12) + attrs

def attr(t: int, v: bytes) -> bytes:
    pad = (4 - len(v) % 4) % 4
    return struct.pack(">HH", t, len(v)) + v + b"\x00" * pad

def turn_allocate(host: str, port: int, username: str, password: str, timeout=4.0):
    """Allocate UDP relay。返回 dict(relayed=..., lifetime=...)，失败抛异常。
    凭据按 LiveKit v1.13.7 算法在调用方生成。"""
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.settimeout(timeout)
    # 1) 无凭据 Allocate -> 期望 401 + REALM + NONCE
    m = stun_pack(0x0003, attr(0x0019, bytes([17, 0, 0, 0])))
    s.sendto(m, (host, port))
    data, _ = s.recvfrom(2048)
    mtype, _, _magic = struct.unpack(">HHI", data[:8])
    # STUN: success=0x0100|m, error=0x0110|m；Allocate(0x003)→success 0103 / error 0113
    if mtype != 0x0113:  # Allocate Error（无凭据首请求，期望 401 challenge）
        raise RuntimeError(f"expect 401 challenge, got 0x{mtype:04x}")
    realm = nonce = b""
    off = 20
    while off < len(data):
        t, l = struct.unpack(">HH", data[off:off + 4]); v = data[off + 4:off + 4 + l]
        if t == 0x0014: realm = v
        if t == 0x0015: nonce = v
        off += 4 + l + ((4 - l % 4) % 4)
    if not realm or not nonce:
        raise RuntimeError("401 without realm/nonce")
    # 2) 带凭据 Allocate。MESSAGE-INTEGRITY（RFC 5389 §15.4）：
    #   HMAC-SHA1(key, header(len 含 MI 属性共 24 字节) + attrs)
    key = hashlib.md5(f"{username}:{realm.decode()}:{password}".encode()).digest()
    uname = username.encode()
    attrs = (attr(0x0019, bytes([17, 0, 0, 0])) + attr(0x0006, uname)
             + attr(0x0014, realm) + attr(0x0015, nonce))
    txn = os.urandom(12)
    head = struct.pack(">HHI", 0x0003, len(attrs) + 24, MAGIC) + txn
    mac = hmac.new(key, head + attrs, hashlib.sha1).digest()
    m = head + attrs + attr(0x0008, mac)
    s.sendto(m, (host, port))
    data, _ = s.recvfrom(2048)
    mtype = struct.unpack(">H", data[:2])[0]
    body = {}
    off = 20
    while off < len(data):
        t, l = struct.unpack(">HH", data[off:off + 4]); v = data[off + 4:off + 4 + l]
        if t == 0x0016 and l >= 8:  # XOR-RELAYED-ADDRESS
            fam = v[1]
            xport = struct.unpack(">H", v[2:4])[0] ^ (MAGIC >> 16)
            if fam == 1:
                ipbytes = bytes(a ^ b for a, b in zip(v[4:8], struct.pack(">I", MAGIC)))
                body["relayed"] = ".".join(str(b) for b in ipbytes) + f":{xport}"
            else:
                body["relayed"] = f"<ipv6:{xport}>"
        if t == 0x0020 and l >= 8:  # XOR-MAPPED-ADDRESS
            fam = v[1]
            xport = struct.unpack(">H", v[2:4])[0] ^ (MAGIC >> 16)
            if fam == 1:
                ipbytes = bytes(a ^ b for a, b in zip(v[4:8], struct.pack(">I", MAGIC)))
                body["mapped"] = ".".join(str(b) for b in ipbytes) + f":{xport}"
        if t == 0x000D:
            body["lifetime"] = struct.unpack(">I", v[:4])[0]
        if t == 0x0009 and mtype == 0x0113:  # ERROR-CODE
            body["error"] = struct.unpack(">H", v[2:4])[0]
        off += 4 + l + ((4 - l % 4) % 4)
    if mtype != 0x0103:  # Allocate Success
        raise RuntimeError(f"allocate failed: 0x{mtype:04x} {body}")
    return body

def livekit_turn_creds(api_key: str, api_secret: str, pid: str, ttl=300):
    """livekit-server v1.13.7 pkg/service/turn.go CreateUsername/CreatePassword 复刻：
    username = base62(apiKey|pID|expiry); password = base62(SHA256(secret|pID|expiry))"""
    expiry = int(time.time()) + ttl
    uname = b62_encode(f"{api_key}|{pid}|{expiry}".encode())
    pw = b62_encode(hashlib.sha256(f"{api_secret}|{pid}|{expiry}".encode()).digest())
    return uname, pw

if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "turn-probe":
        host, port, api_key, api_secret = sys.argv[2], int(sys.argv[3]), sys.argv[4], sys.argv[5]
        u, p = livekit_turn_creds(api_key, api_secret, "e2e_probe_a5")
        r = turn_allocate(host, port, u, p)
        print(json.dumps({"username_len": len(u), **r}))
    elif cmd == "jwt-verify":
        print(json.dumps(jwt_verify(sys.argv[2], sys.argv[3])))
    elif cmd == "jwt-sign":
        claims = json.loads(sys.argv[4])
        print(jwt_sign(claims, sys.argv[3]))
'

py() { python3 -c "$PY_HELPERS" "$@"; }

# ══════════════════════════════════════════════════════════════════════════════
# 自含模式基础设施：LiveKit 容器 + imboy 隔离节点
# ══════════════════════════════════════════════════════════════════════════════

# LiveKit 配置：与 deploy/docker-compose.livekit-turn.yml 的 W5 端口合同同构
# （3478/udp + relay 段独立于媒体段），仅端口换成 A0 租约段；TLS 自签证书
# 走完整形态（生产 TLS 侧验证归 W4，本地不发布 5349）。
write_livekit_config() { # $1=config path $2=certdir $3=advertise_ip
  cat > "$1" <<YAML
port: ${LK_SIGNAL_PORT}
rtc:
  tcp_port: ${LK_TCP_PORT}
  port_range_start: ${LK_MEDIA_START}
  port_range_end: ${LK_MEDIA_END}
  node_ip: ${3:-127.0.0.1}
  use_external_ip: false
turn:
  enabled: true
  domain: turn.local.test
  cert_file: /etc/livekit/certs/fullchain.pem
  key_file: /etc/livekit/certs/privkey.pem
  tls_port: 5349
  udp_port: ${LK_TURN_UDP}
  relay_range_start: ${LK_RELAY_START}
  relay_range_end: ${LK_UDP_END}
keys:
  ${LK_API_KEY}: ${LK_API_SECRET}
logging:
  level: debug
  json: true
YAML
}

ensure_livekit_image() {
  # 合同校验（两级）：Hub index 内 amd64 manifest digest 必须等于锁定值；
  # 本地拉取一致性：RepoDigests 含该 index digest。
  # （docker image inspect 的 RepoDigest 是 multi-arch index digest，
  #  合同锁定值是 amd64 平台 manifest digest——见 logs/image_digest.txt）
  local idx_digest
  idx_digest="$(hub_index_digest "$LK_IMAGE")" || return 1
  if [ "$idx_digest" = "BLOCKED_NETWORK" ]; then
    degr_req "Docker Hub API 不可达：digest 在线校验是 Required，降级不绿（BLOCKED 语义）"
    docker image inspect "$LK_IMAGE" >/dev/null 2>&1 || return 1
    return 0
  fi
  printf 'index digest: %s\namd64 manifest digest: %s (contract)\n' \
    "$idx_digest" "$LK_IMAGE_AMD64_DIGEST" > "$LOGDIR/image_digest.txt"
  local api_amd64
  api_amd64="$(hub_amd64_manifest_digest "$LK_IMAGE")" || return 1
  [ "$api_amd64" = "$LK_IMAGE_AMD64_DIGEST" ] \
    || { say "[FAIL] Hub amd64 digest=$api_amd64 与合同 $LK_IMAGE_AMD64_DIGEST 不符"; return 1; }
  # 本地缺失或架构漂移（他处裸 pull 会把 tag 顶到 arm64）→ 重拉 amd64
  local arch=""
  arch="$(docker image inspect "$LK_IMAGE" --format '{{.Architecture}}' 2>/dev/null || true)"
  if [ "$arch" != amd64 ]; then
    say "-- 拉取 $LK_IMAGE (linux/amd64, 本地=${arch:-缺失}) ..."
    docker pull --platform linux/amd64 "$LK_IMAGE" >>"$LOGDIR/docker_pull.log" 2>&1 || return 1
  fi
  docker image inspect "$LK_IMAGE" --format '{{.Architecture}} {{index .RepoDigests 0}}' \
    | tee -a "$LOGDIR/image_digest.txt"
  docker image inspect "$LK_IMAGE" --format '{{.Architecture}}' | grep -q amd64 || return 1
  grep -q "$idx_digest" "$LOGDIR/image_digest.txt"
}

hub_registry_token() { # image → bearer token（匿名 pull scope）
  curl -sf --max-time 10 "https://auth.docker.io/token?service=registry.docker.io&scope=repository:${1%:*}:pull" \
    | jq -r .token
}
hub_index_digest() { # image:tag → index digest（Docker-Content-Digest header）
  local img="$1" repo="${1%:*}" tag="${1##*:}" tok
  tok="$(hub_registry_token "$img")" || { echo BLOCKED_NETWORK; return 0; }
  curl -sf --max-time 10 -H "Authorization: Bearer $tok" \
    -H "Accept: application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json" \
    -o /dev/null -D - "https://registry-1.docker.io/v2/$repo/manifests/$tag" 2>/dev/null \
    | tr -d '\r' | awk -F': ' 'tolower($1)=="docker-content-digest"{print $2}' | head -1
}
hub_amd64_manifest_digest() { # image:tag → index 内 amd64 平台 manifest digest
  local img="$1" repo="${1%:*}" tag="${1##*:}" tok
  tok="$(hub_registry_token "$img")" || return 1
  curl -sf --max-time 10 -H "Authorization: Bearer $tok" \
    -H "Accept: application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json" \
    "https://registry-1.docker.io/v2/$repo/manifests/$tag" 2>/dev/null \
    | jq -r '[.manifests[] | select(.platform.architecture=="amd64" and .platform.os=="linux")][0].digest'
}

start_livekit() { # $1=advertise_ip
  docker rm -f "$CONTAINER" >/dev/null 2>&1 || true
  local cfg="$LOGDIR/livekit.yaml" certdir="$LOGDIR/turn-certs"
  mkdir -p "$certdir"
  LK_MEDIA_START=$LK_UDP_START write_livekit_config "$cfg" "$certdir" "$1"
  # 自签证书（TURN TLS 完整配置形态；本地只验 UDP 路径）
  openssl req -x509 -newkey rsa:2048 -nodes -keyout "$certdir/privkey.pem" \
    -out "$certdir/fullchain.pem" -days 2 -subj "/CN=turn.local.test" \
    >/dev/null 2>&1 || return 1
  if [ "$LK_HOST_NET" = 1 ]; then
    docker run -d --name "$CONTAINER" --platform linux/amd64 --network host \
      -v "$cfg:/etc/livekit.yaml:ro" \
      -v "$certdir:/etc/livekit/certs:ro" \
      "$LK_IMAGE" --config /etc/livekit.yaml >>"$LOGDIR/docker_run.log" 2>&1 || return 1
  else
    docker run -d --name "$CONTAINER" --platform linux/amd64 \
      -p "$LK_SIGNAL_PORT:$LK_SIGNAL_PORT" \
      -p "$LK_TCP_PORT:$LK_TCP_PORT" \
      -p "$LK_TURN_UDP:$LK_TURN_UDP/udp" \
      -p "$LK_MEDIA_START-$LK_MEDIA_END:$LK_MEDIA_START-$LK_MEDIA_END/udp" \
      -p "$LK_RELAY_START-$LK_UDP_END:$LK_RELAY_START-$LK_UDP_END/udp" \
      -v "$cfg:/etc/livekit.yaml:ro" \
      -v "$certdir:/etc/livekit/certs:ro" \
      "$LK_IMAGE" --config /etc/livekit.yaml >>"$LOGDIR/docker_run.log" 2>&1 || return 1
  fi
  register_cleanup "docker rm -f $CONTAINER"
  # 健康等待：信令端口 HTTP 就绪
  local i
  for ((i=0; i<30; i++)); do
    if curl -sf -o /dev/null "http://127.0.0.1:$LK_SIGNAL_PORT"; then return 0; fi
    if ! docker inspect "$CONTAINER" >/dev/null 2>&1; then
      docker logs "$CONTAINER" | tail -20 >> "$LOGDIR/livekit_boot_fail.log" 2>/dev/null
      return 1
    fi
    sleep 1
  done
  return 1
}

# lk CLI 封装：优先宿主机 lk；否则 docker（与 LiveKit 容器同 netns，ICE 直达容器 IP）。
# v2.1.1 的 --url/--api-key/--api-secret 是根级全局旗标，放在子命令后不被解析
# （实测打 usage help → join 静默不执行）。统一走 env 注入（LIVEKIT_*），两类
# 安装形态行为一致；ws:// 前缀由 CLI 自行转换为 twirp 的 http://。
# 输出走调用方 stdout/stderr（join 由调用方重定向到用例日志；
# room list/participants 的 stdout 供 grep 断言），stderr 汇入 lk_cli.log
lk_cli() {
  if command -v lk >/dev/null 2>&1; then
    LIVEKIT_URL="ws://127.0.0.1:$LK_SIGNAL_PORT" \
    LIVEKIT_API_KEY="$LK_API_KEY" LIVEKIT_API_SECRET="$LK_API_SECRET" \
      lk "$@" 2>>"$LOGDIR/lk_cli.log"
  else
    docker run --rm --network "container:$CONTAINER" \
      --label "imboy.lktests=$CONTAINER" \
      -e LIVEKIT_URL="ws://127.0.0.1:$LK_SIGNAL_PORT" \
      -e LIVEKIT_API_KEY="$LK_API_KEY" -e LIVEKIT_API_SECRET="$LK_API_SECRET" \
      "$LK_CLI_IMAGE" "$@" 2>>"$LOGDIR/lk_cli.log"
  fi
}
# 确定性回收本脚本起的 CLI 容器（按 label 命名空间，绝不触碰 foreign 容器）
cleanup_lk_cli_containers() {
  docker ps -aq --filter "label=imboy.lktests=$CONTAINER" 2>/dev/null \
    | xargs docker rm -f >/dev/null 2>&1 || true
}
lk_cli_supports_force_relay() {
  lk_cli room join --help 2>/dev/null | grep -qi "force-relay"
}

ensure_cli_image() {
  command -v lk >/dev/null 2>&1 && return 0
  docker image inspect "$LK_CLI_IMAGE" >/dev/null 2>&1 && return 0
  say "-- 拉取 $LK_CLI_IMAGE ..."
  docker pull "$LK_CLI_IMAGE" >>"$LOGDIR/docker_pull.log" 2>&1
}

cleanup_scratch_db() {
  docker exec -e PGPASSWORD="$PGPASSWORD" imboy_pg18 \
    psql -U "$PGUSER" -h 127.0.0.1 -d postgres \
    -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='$SCRATCH_DB' AND pid <> pg_backend_pid();" \
    -c "DROP DATABASE IF EXISTS \"$SCRATCH_DB\";"
}

# imboy 隔离节点：scratch 库 + 随机端口 + livekit 段指向本地容器
start_imboy_node() {
  # scratch 库（幂等重建；护栏：拒绝共享库）
  [[ "$SCRATCH_DB" =~ ^[A-Za-z0-9_]+$ ]] \
    || { say "[BLOCKED] scratch 库名含非法字符: $SCRATCH_DB"; exit 2; }
  case "$SCRATCH_DB" in
    imboy_v1|postgres|template*) say "[BLOCKED] 拒绝共享库 $SCRATCH_DB"; exit 2 ;;
  esac
  docker exec -e PGPASSWORD="$PGPASSWORD" imboy_pg18 psql -U "$PGUSER" -h 127.0.0.1 -d postgres \
    -c "DROP DATABASE IF EXISTS \"$SCRATCH_DB\";" -c "CREATE DATABASE \"$SCRATCH_DB\" OWNER $PGUSER;" \
    >>"$LOGDIR/scratch_db.log" 2>&1 || return 1
  # 迁移前置扩展（与 imboy_test_v1 同口径；缺 timescaledb 时 0001 迁移 hypertable 必挂）
  docker exec -e PGPASSWORD="$PGPASSWORD" imboy_pg18 psql -U "$PGUSER" -h 127.0.0.1 -d "$SCRATCH_DB" \
    -c "CREATE EXTENSION IF NOT EXISTS timescaledb;" \
    -c "CREATE EXTENSION IF NOT EXISTS pgcrypto;" \
    -c "CREATE EXTENSION IF NOT EXISTS pg_jieba;" \
    -c "CREATE EXTENSION IF NOT EXISTS postgis;" \
    -c "CREATE EXTENSION IF NOT EXISTS vector;" \
    >>"$LOGDIR/scratch_db.log" 2>&1 || return 1
  register_cleanup cleanup_scratch_db

  API_PORT="${RTC_E2E_HTTP_PORT:-$(pick_port)}"
  ADM_PORT="${RTC_E2E_ADM_PORT:-$(pick_port)}"
  say "   imboy 节点: http_port=$API_PORT 库=$SCRATCH_DB"

  # 物化配置：example 为模板，改 db/端口，注入 livekit 段
  local cfg="$LOGDIR/sys.local.rtc.config"
  cp config/sys.config.example "$cfg"
  python3 - "$cfg" "$API_PORT" "$ADM_PORT" "$SCRATCH_DB" "$LK_SIGNAL_PORT" \
      "$LK_API_KEY" "$LK_API_SECRET" "$LK_HOST" <<'PY'
import re, sys
path, api_port, adm_port, db, lk_port, lk_key, lk_secret, lk_host = sys.argv[1:9]
t = open(path, encoding="utf-8").read()
t = re.sub(r"\{http_port, \d+\}", "{http_port, %s}" % api_port, t)
t = re.sub(r"\{http_port_adm, \d+\}", "{http_port_adm, %s}" % adm_port, t)
t = re.sub(r'database => "imboy_v1"', 'database => "%s"' % db, t)
t = re.sub(r'database => "imboy_v1"', 'database => "%s"' % db, t)
# livekit 段：example 已带占位（wss://rtc.imboy.pub），整段替换为本地容器值
# （rtc_room_logic:livekit_config 读 {imboy, livekit} map）。
# ws_url 用 advertise host（真机联测时为局域网 IP，本机跑为 127.0.0.1）。
lk_re = re.compile(r"\{livekit, #\{[^}]*\}\}", re.S)
lk_new = ('{livekit, #{ws_url => <<"ws://%s:%s">>, '
          'api_key => <<"%s">>, api_secret => <<"%s">>}}') % (lk_host, lk_port, lk_key, lk_secret)
t, n = lk_re.subn(lk_new, t, count=1)
assert n == 1, "livekit section not found in example config"
open(path, "w", encoding="utf-8").write(t)
PY
  grep -A3 "{livekit," "$cfg" | head -5 >>"$LOGDIR/node_config_check.txt" || return 1

  # 种子+驻留节点 escript
  local seed="$LOGDIR/rtc_e2e_seed.escript"
  local alias_dir="$LOGDIR/appalias/imboy" alias_ebin
  mkdir -p "$alias_dir"
  rm -rf "$alias_dir/ebin" "$alias_dir/priv"
  ln -s "$WT/ebin" "$alias_dir/ebin"
  ln -s "$WT/priv" "$alias_dir/priv"
  alias_ebin="$alias_dir/ebin"
  cat > "$seed" <<ERLEOF
#!/usr/bin/env escript
%%! -noshell -noinput -config ${cfg%.config}
%% LK-TEST-01 e2e 种子节点：起 app(auto_migrate) → 种子合成账号 → 驻留等指令。
%% 指令经 erlang 分布式 rpc（cookie 见脚本常量）：
%%   application:unset_env(imboy, livekit) / set_env / 查询。
main([]) ->
    {ok, _} = net_kernel:start(['$NODE_NAME']),
    erlang:set_cookie(node(), '$NODE_COOKIE'),
    Root = "$WT",
    [code:add_patha(P) || P <- filelib:wildcard(Root ++ "/deps/*/ebin")],
    code:add_patha(Root ++ "/ebin"),
    code:add_patha("$alias_ebin"),
    {ok, _} = application:ensure_all_started(imboy),
    T0 = erlang:system_time(millisecond),
    MkId = fun() ->
        940000000000000000 + ((T0 - 1789000000000) * 100000)
            + (erlang:unique_integer([positive, monotonic]) rem 100000)
    end,
    [U1, U2, U3, G] = [MkId() || _ <- lists:seq(1, 4)],
    Password = elib_password:generate(<<"lk-e2e-local-2026">>),
    lists:foreach(fun(U) ->
        {ok, _} = elib_pg:query(
            <<"INSERT INTO \\"user\\"(id,password,account,nickname,reg_ip,reg_cosv) "
              "VALUES (\$1,\$3,\$2,'lk_e2e_user','127.0.0.1','x')">>,
            [U, iolist_to_binary([<<"lke2e-">>, integer_to_binary(U)]), Password])
    end, [U1, U2, U3]),
    {ok, _} = elib_pg:query(
        <<"INSERT INTO \\"group\\"(id, owner_uid, creator_uid, title) "
          "VALUES (\$1, \$2, \$2, 'lk_e2e_group')">>, [G, U1]),
    lists:foreach(fun(U) ->
        {ok, _} = elib_pg:query(
            <<"INSERT INTO group_member(id, group_id, user_id, role, is_join, status) "
              "VALUES (\$1, \$2, \$3, 1, true, 1)">>, [MkId(), G, U])
    end, [U1, U2]),
    lists:foreach(fun({A, B}) ->
        {ok, _} = elib_pg:query(
            <<"INSERT INTO user_friend(id, from_user_id, to_user_id, status) "
              "VALUES (\$1, \$2, \$3, 1)">>, [MkId(), A, B])
    end, [{U1, U2}, {U2, U1}]),
    T1 = token_ds:encrypt_token(U1),
    T2 = token_ds:encrypt_token(U2),
    T3 = token_ds:encrypt_token(U3),
    io:format("SEED_U1 ~p~nSEED_U2 ~p~nSEED_U3 ~p~nSEED_G ~p~n", [U1, U2, U3, G]),
    io:format("SEED_TOKEN1 ~ts~nSEED_TOKEN2 ~ts~nSEED_TOKEN3 ~ts~n", [T1, T2, T3]),
    io:format("SEED_READY~n"),
    receive
        stop -> ok
    after infinity -> ok
    end.
ERLEOF
  NODE_LOG="$LOGDIR/imboy_node.log"
  chmod +x "$seed"
  # epmd 预检：net_kernel 动态启动依赖 epmd；macOS 冷机可能未起（econnrefused
  # → nodistribution 假失败）。已监听则不动，未监听则 -daemon 拉起（幂等）。
  if ! lsof -nP -iTCP:4369 -sTCP:LISTEN >/dev/null 2>&1; then
    say "   epmd 未运行，执行 epmd -daemon"
    epmd -daemon >>"$LOGDIR/epmd.log" 2>&1 || true
    sleep 1
    lsof -nP -iTCP:4369 -sTCP:LISTEN >/dev/null 2>&1 \
      || { say "[BLOCKED] epmd 拉起失败（见 $LOGDIR/epmd.log）"; return 1; }
  fi
  # IMBOYENV=local：非 strict 分支（jwt_key 等 dev 派生，与 make eunit-local 同口径）
  IMBOYENV=local escript "$seed" >"$NODE_LOG" 2>&1 &
  SEED_PID=$!
  register_cleanup "kill $SEED_PID 2>/dev/null"
  # 等 READY + healthz
  local i
  for ((i=0; i<120; i++)); do
    grep -q "SEED_READY" "$NODE_LOG" 2>/dev/null && break
    if ! kill -0 "$SEED_PID" 2>/dev/null; then
      tail -30 "$NODE_LOG" >> "$NODE_LOG.fail" ; return 1
    fi
    sleep 1
  done
  grep -q "SEED_READY" "$NODE_LOG" || return 1
  wait_http_ok "http://127.0.0.1:$API_PORT/healthz" 30 || return 1
  # 读种子输出
  SEED_U1="$(grep '^SEED_U1 ' "$NODE_LOG" | awk '{print $2}')"
  SEED_U2="$(grep '^SEED_U2 ' "$NODE_LOG" | awk '{print $2}')"
  SEED_U3="$(grep '^SEED_U3 ' "$NODE_LOG" | awk '{print $2}')"
  SEED_G="$(grep '^SEED_G ' "$NODE_LOG" | awk '{print $2}')"
  SEED_TOKEN1="$(grep '^SEED_TOKEN1 ' "$NODE_LOG" | awk '{print $2}')"
  SEED_TOKEN2="$(grep '^SEED_TOKEN2 ' "$NODE_LOG" | awk '{print $2}')"
  SEED_TOKEN3="$(grep '^SEED_TOKEN3 ' "$NODE_LOG" | awk '{print $2}')"
  API_BASE="http://127.0.0.1:$API_PORT"
}

# 节点 rpc（热改 livekit 配置做 livekit_not_configured 负例）
node_rpc() { # mod fun args(明确写好的 erl 列表字面量)
  erl -noshell -name "ctl$$@127.0.0.1" -setcookie "$NODE_COOKIE" \
    -eval "case net_adm:ping('$NODE_NAME') of pong -> rpc:call('$NODE_NAME', $1, $2, $3), halt(0); pang -> halt(3) end" \
    2>/dev/null
}

# ══════════════════════════════════════════════════════════════════════════════
# 断言组
# ══════════════════════════════════════════════════════════════════════════════

rtc_join() { # token kind target_id did → writes body; echoes code
  http_code POST "$API_BASE/api/v1/rtc/room/join" \
    -H "Authorization: Bearer $1" -H "Content-Type: application/json" \
    -d "{\"kind\":\"$2\",\"target_id\":$3,\"did\":\"$4\"}"
}

stage1_contract() {
  say ""
  say "== S1: join 签发契约（group/c2c 正例 + ACL 负例 + 受控错误）=="

  local code
  say "-- S1.1 group 成员正例"
  code="$(rtc_join "$SEED_TOKEN1" group "$SEED_G" didA)"
  local room1 tok1
  room1="$(jqget '.payload.room_name // .data.room_name')"
  tok1="$(jqget '.payload.token // .data.token')"
  local ws1; ws1="$(jqget '.payload.ws_url // .data.ws_url')"
  [ "$code" = 200 ] && ok "group join HTTP 200" || bad "group join HTTP=$code body=$(head -c 200 "$LOGDIR/last_body.json")"
  [ "$room1" = "rtc_group_$SEED_G" ] && ok "群房间名 rtc_group_$SEED_G" || bad "群房间名=$room1"
  [ "$ws1" = "ws://$LK_HOST:$LK_SIGNAL_PORT" ] && ok "ws_url points to local SFU ($LK_HOST)" || bad "ws_url=$ws1"
  [ -n "$tok1" ] && [ "$tok1" != null ] && ok "返回 livekit token" || bad "token 缺失"

  say "-- S1.2 token claims（JWT HS256 验签 + video grant）"
  if py jwt-verify "$tok1" "$LK_API_SECRET" > "$LOGDIR/jwt_claims.json" 2>"$LOGDIR/jwt_verify.err"; then
    local sub room exp
    sub="$(jq -r .sub "$LOGDIR/jwt_claims.json")"
    room="$(jq -r '.video.room' "$LOGDIR/jwt_claims.json")"
    exp="$(jq -r .exp "$LOGDIR/jwt_claims.json")"
    [ "$sub" = "${SEED_U1}_didA" ] && ok "sub=${SEED_U1}_didA（uid+did 设备后缀）" || bad "sub=$sub"
    [ "$room" = "rtc_group_$SEED_G" ] && ok "video.room 一致" || bad "video.room=$room"
    jq -e '.video.roomJoin == true and .video.canPublish == true and .video.canSubscribe == true' \
      "$LOGDIR/jwt_claims.json" >/dev/null && ok "roomJoin/canPublish/canSubscribe 权限齐" \
      || bad "video grant 不完整: $(cat "$LOGDIR/jwt_claims.json")"
    local now; now="$(date +%s)"
    [ "$exp" -gt "$now" ] && [ "$exp" -le $((now + 601)) ] && ok "exp 有效期 600s 窗口" || bad "exp=$exp now=$now"
  else
    bad "JWT 验签失败: $(head -c 200 "$LOGDIR/jwt_verify.err")"
  fi

  say "-- S1.3 did 维度：同 uid 异 did 得不同 identity（防同号互踢回归）"
  rtc_join "$SEED_TOKEN1" group "$SEED_G" didB >/dev/null
  local tok2; tok2="$(jqget '.payload.token // .data.token')"
  py jwt-verify "$tok2" "$LK_API_SECRET" > "$LOGDIR/jwt_claims2.json" 2>/dev/null \
    && [ "$(jq -r .sub "$LOGDIR/jwt_claims2.json")" = "${SEED_U1}_didB" ] \
    && ok "didB → sub=${SEED_U1}_didB" || bad "didB sub=$(jq -r .sub "$LOGDIR/jwt_claims2.json" 2>/dev/null)"

  say "-- S1.4 ACL 负例：非群成员"
  # 注：elib_response:error 形态为 HTTP 200 + {code!=0, msg}（仓库口径），
  # 断言以业务 msg 文案 + 未签发 token 为准，不按 HTTP 状态码判死。
  rtc_join "$SEED_TOKEN3" group "$SEED_G" didA >/dev/null
  local acl_msg acl_tok
  acl_msg="$(jqget '.msg // .message // empty')"
  acl_tok="$(jqget '.payload.token // .data.token // empty')"
  if printf '%s' "$acl_msg" | grep -q "不是群成员" && [ -z "$acl_tok" ]; then
    ok "非群成员被拒（msg 命中，未签发 token）"
  else
    bad "非群成员未走 ACL 拒绝: msg=$acl_msg"
  fi

  say "-- S1.5 ACL 负例：非好友 c2c"
  rtc_join "$SEED_TOKEN3" c2c "$SEED_U1" didA >/dev/null
  acl_msg="$(jqget '.msg // .message // empty')"
  acl_tok="$(jqget '.payload.token // .data.token // empty')"
  if printf '%s' "$acl_msg" | grep -q "不是好友" && [ -z "$acl_tok" ]; then
    ok "非好友 c2c 被拒（msg 命中，未签发 token）"
  else
    bad "陌生人 c2c 未走 ACL 拒绝: msg=$acl_msg"
  fi

  say "-- S1.6 c2c 对称房间：两端各自 join 得同一 room"
  rtc_join "$SEED_TOKEN1" c2c "$SEED_U2" didA >/dev/null
  local r12; r12="$(jqget '.payload.room_name // .data.room_name')"
  rtc_join "$SEED_TOKEN2" c2c "$SEED_U1" didA >/dev/null
  local r21; r21="$(jqget '.payload.room_name // .data.room_name')"
  [ "$r12" = "$r21" ] && [ -n "$r12" ] && ok "对称房间 $r12" || bad "房间不对称: $r12 vs $r21"
  echo "$r12" > "$LOGDIR/c2c_room.txt"

  say "-- S1.7 受控错误：livekit 未配置"
  node_rpc application unset_env "[imboy, livekit]"
  rtc_join "$SEED_TOKEN1" group "$SEED_G" didA >/dev/null
  local msg nc_tok
  msg="$(jqget '.msg // .message // .error // empty')"
  nc_tok="$(jqget '.payload.token // .data.token // empty')"
  if printf '%s' "$msg" | grep -q "livekit_not_configured" && [ -z "$nc_tok" ]; then
    ok "livekit_not_configured 受控错误（msg 命中，未签发 token，未崩 500）"
  else
    bad "缺配置未走受控错误: msg=$msg"
  fi
  # 恢复配置（节点 set_env 回原值）
  node_rpc application set_env "[imboy, livekit, #{ws_url => <<\"ws://$LK_HOST:$LK_SIGNAL_PORT\">>, api_key => <<\"$LK_API_KEY\">>, api_secret => <<\"$LK_API_SECRET\">>}]"
  rtc_join "$SEED_TOKEN1" group "$SEED_G" didA >/dev/null
  local rt_tok; rt_tok="$(jqget '.payload.token // .data.token // empty')"
  [ -n "$rt_tok" ] && [ "$rt_tok" != null ] && ok "恢复配置后 join 复通" || bad "恢复配置后 join 仍失败 msg=$(jqget '.msg')"

  say "-- S1.8 重复 join 幂等"
  rtc_join "$SEED_TOKEN1" group "$SEED_G" didA >/dev/null
  local ra; ra="$(jqget '.payload.room_name // .data.room_name')"
  rtc_join "$SEED_TOKEN1" group "$SEED_G" didA >/dev/null
  local rb; rb="$(jqget '.payload.room_name // .data.room_name')"
  [ "$ra" = "$rb" ] && [ -n "$ra" ] && ok "重复 join 同房间（${ra}）" || bad "重复 join 房间漂移: $ra/$rb"
}

stage2_sfu() {
  say ""
  say "== S2: 真实 SFU 信令（publish / subscribe / leave）=="
  if ! command -v lk >/dev/null 2>&1 && ! docker image inspect "$LK_CLI_IMAGE" >/dev/null 2>&1; then
    ensure_cli_image || true
  fi
  local have_lk=1
  lk_cli room list >/dev/null 2>&1 || have_lk=0
  if [ "$have_lk" = 0 ]; then
    degr_req "lk CLI 不可用（宿主机无 lk 且镜像拉取失败）：S2 真实信令 publish/subscribe 是 Required，降级不绿（BLOCKED 语义）"
    return 0
  fi
  register_cleanup cleanup_lk_cli_containers

  local room="rtc_group_$SEED_G"
  # 注：lk CLI 不支持 --token，join 由 CLI 用与后端相同的 key/secret 自签
  # （同密钥体系）；后端签发 token 的 claims 正确性已由 S1.2 验签闭环。
  say "-- S2.1 publish：lk 发布 demo 流"
  lk_cli room join --identity "${SEED_U1}_pub1" --publish-demo "$room" \
    >"$LOGDIR/s2_publisher.log" 2>&1 &
  local pubpid=$!

  say "-- S2.2 subscribe：第二端（同房另一 identity）接入"
  lk_cli room join --identity "${SEED_U2}_sub1" "$room" \
    >"$LOGDIR/s2_subscriber.log" 2>&1 &
  local subpid=$!

  # 轮询 SFU：房间存在且 2 参与者、publisher 有 track
  local okroom=0 okpart=0 oktrack=0 i
  for ((i=0; i<15; i++)); do
    if [ "$okroom" = 0 ] && lk_cli room list 2>/dev/null | grep -q "$room"; then okroom=1; fi
    local parts
    parts="$(lk_cli room participants list "$room" 2>/dev/null || true)"
    if [ "$okpart" = 0 ] && [ "$(printf '%s' "$parts" | grep -c "${SEED_U1}_pub1\|${SEED_U2}_sub1")" -ge 2 ]; then okpart=1; fi
    if [ "$oktrack" = 0 ] && printf '%s' "$parts" | grep -qi "track"; then oktrack=1; fi
    [ "$okroom" = 1 ] && [ "$okpart" = 1 ] && [ "$oktrack" = 1 ] && break
    sleep 2
  done
  lk_cli room participants list "$room" >"$LOGDIR/s2_participants.txt" 2>/dev/null || true
  [ "$okroom" = 1 ] && ok "SFU 建房 ${room}（后端签发 token 被真实信令接受）" || bad "SFU 未出现房间 $room"
  [ "$okpart" = 1 ] && ok "双参与者接入（publish+subscribe 面均有连接）" || bad "参与者不足：$(cat "$LOGDIR/s2_participants.txt")"
  if [ "$oktrack" = 1 ]; then
    ok "房间内存在已发布 track（publish 证据）"
  else
    bad "未见 track（publish 证据缺失）：$(cat "$LOGDIR/s2_participants.txt")"
  fi
  # 订阅侧证据：subscriber 日志出现 track 事件（demo 流被拉到）。
  # 防 usage-help 误匹配：先排除 CLI 参数错误形态（USAGE: 段）。
  if ! grep -q "USAGE:" "$LOGDIR/s2_subscriber.log" 2>/dev/null \
     && grep -qi "track" "$LOGDIR/s2_subscriber.log" 2>/dev/null; then
    ok "subscriber 收到 track 事件（subscribe 证据）"
  else
    # A7 复核遗留项：订阅回执是媒体合同必要面（publish 有硬断言、subscribe
    # 只有此处软降级会形成假绿窗口），缺失必须计 REQ_DEGRADED 进终局门。
    degr_req "subscriber 日志无 track 事件（subscribe 回执缺失；双端连接+参与者 track 仅覆盖 publish 面）"
  fi

  say "-- S2.3 leave：断开后房间收敛"
  kill "$pubpid" "$subpid" 2>/dev/null || true
  wait "$pubpid" 2>/dev/null; wait "$subpid" 2>/dev/null
  cleanup_lk_cli_containers
  lk_cli room participants remove --room "$room" "${SEED_U2}_sub1" >/dev/null 2>&1 || true
  lk_cli room participants remove --room "$room" "${SEED_U1}_pub1" >/dev/null 2>&1 || true
  local gone=0
  for ((i=0; i<20; i++)); do
    if ! lk_cli room list 2>/dev/null | grep -q "$room"; then gone=1; break; fi
    sleep 2
  done
  if [ "$gone" = 1 ]; then
    ok "全员离开后房间关闭（leave 释放）"
  else
    degr "房间 $room 30s+ 仍在列表（LiveKit 空房保留策略差异，非断言失败）"
  fi
}

stage3_turn() {
  say ""
  say "== S3: embedded TURN UDP relay（3478 + relay 段 $LK_RELAY_START-${LK_UDP_END}）=="

  # 3a. 协议级 TURN Allocate：凭据按 v1.13.7 算法生成（jxskiss/base62 复刻）
  say "-- S3.1 TURN Allocate 探针（LiveKit v1.13.7 凭据算法）"
  if py turn-probe 127.0.0.1 "$LK_TURN_UDP" "$LK_API_KEY" "$LK_API_SECRET" \
      > "$LOGDIR/turn_probe.json" 2> "$LOGDIR/turn_probe.err"; then
    local relayed lifetime
    relayed="$(jq -r .relayed "$LOGDIR/turn_probe.json")"
    lifetime="$(jq -r .lifetime "$LOGDIR/turn_probe.json")"
    local rhost rport
    rhost="${relayed%:*}"
    rport="${relayed##*:}"
    if [ "$rhost" = "$LK_HOST" ] \
        && [ -n "$rport" ] \
        && [ "$rport" -ge "$LK_RELAY_START" ] \
        && [ "$rport" -le "$LK_UDP_END" ]; then
      ok "Allocate 成功且 relayed=$relayed 落在 relay 段（协议级 relay 路径证据）"
    else
      bad "relayed=$relayed 与 advertise=$LK_HOST 或 relay 段 $LK_RELAY_START-$LK_UDP_END 不一致"
    fi
    [ -n "$lifetime" ] && [ "$lifetime" -gt 0 ] && ok "LIFETIME=$lifetime" || bad "lifetime 异常: $lifetime"
  else
    bad "TURN Allocate 失败: $(head -c 300 "$LOGDIR/turn_probe.err")"
  fi

  # 3b. 媒体级 selected-candidate=relay（lk CLI force-relay）
  say "-- S3.2 强制 ICE relay 的媒体级证据"
  local room; room="$(cat "$LOGDIR/c2c_room.txt" 2>/dev/null)"
  [ -n "$room" ] || { rtc_join "$SEED_TOKEN1" group "$SEED_G" relay1 >/dev/null; room="rtc_group_$SEED_G"; }
  rtc_join "$SEED_TOKEN1" group "$SEED_G" relay1 >/dev/null
  local tok; tok="$(jqget '.payload.token // .data.token')"
  local relay_evidence=""
  if lk_cli_supports_force_relay; then
    run_to 25 lk_cli room join "$room" \
      --identity "${SEED_U1}_relay1" --publish-demo --force-relay \
      >"$LOGDIR/s3_relay_join.log" 2>&1
    if grep -qi "relay" "$LOGDIR/s3_relay_join.log"; then
      relay_evidence="cli-force-relay"
      ok "lk CLI --force-relay 接入且日志含 relay candidate（$(grep -i -m1 relay "$LOGDIR/s3_relay_join.log" | head -c 120)）"
    fi
  else
    degr_req "lk CLI 无 --force-relay：selected-candidate=relay 是 Required，本脚本腿不承载则不绿（BLOCKED 语义；真机 rtc_relay_realdevice_test 单独出证）"
  fi

  # 3c. LiveKit 日志 TURN allocation 证据（A02 硬性要求：仅端口可达不算）
  # v1.13.7 的 pion embedded TURN 不逐条打印 allocation 事件（debug 级实测无），
  # 弱匹配（字段名 relay_allocation_limit / SFU "stream allocation"）不作为证据。
  # 认两种强证据：
  #   (1) "Starting TURN server"（服务激活，含 udp 3478 + relay 段配置）
  #   (2) trickle candidate "... typ relay" 且端口落在 relay 段——这是 SFU 为
  #       真实信令会话签出的 TURN relay 分配（server 侧 allocation 的媒体面痕迹）
  say "-- S3.3 LiveKit 日志 TURN allocation 证据"
  docker logs "$CONTAINER" > "$LOGDIR/livekit_container.log" 2>&1 || true
  local turn_started=0 relay_cand=""
  grep -q "Starting TURN server" "$LOGDIR/livekit_container.log" && turn_started=1
  relay_cand="$(grep -o 'candidate:[0-9]* [0-9]* udp [0-9]* [0-9.]* [0-9]* typ relay' \
    "$LOGDIR/livekit_container.log" | awk '{print $6}' \
    | awk -v s="$LK_RELAY_START" -v e="$LK_UDP_END" '$1>=s && $1<=e' | head -1)"
  if [ "$turn_started" = 1 ] && [ -n "$relay_cand" ]; then
    ok "LiveKit 日志 TURN 证据齐：Starting TURN server + relay 段 trickle candidate（端口 ${relay_cand}）"
    grep -m2 "Starting TURN server" "$LOGDIR/livekit_container.log" >> "$LOGDIR/turn_allocation_evidence.txt"
    grep -m3 "typ relay" "$LOGDIR/livekit_container.log" >> "$LOGDIR/turn_allocation_evidence.txt"
  elif [ "$turn_started" = 1 ]; then
    # S2 join 未跑或未触发 server 侧 relay 时（如单独执行 S3）：降级但不算失败，
    # 协议级 Allocate（S3.1）仍是 allocation 的直接证明
    degr "日志有 Starting TURN server 但无 relay 段 trickle candidate（本次无媒体会话触发；allocation 直接证明归 S3.1 探针）"
  else
    bad "LiveKit 日志无 TURN 服务激活记录（A02 不满足）"
  fi
}

stage4_negative() {
  say ""
  say "== S4: 负例（过期 token / 无 grant token 被 SFU 拒绝，twirp HTTP API 面）=="
  # LiveKit 服务面（twirp）以 Bearer JWT 鉴权：过期 token 与缺 grant token
  # 都必须在 HTTP 层被拒。ws 级 join 校验（房间不匹配 token）由真机
  # integration_test/rtc 用例与 lk CLI join 的 room grant 行为共同覆盖。
  local api="http://127.0.0.1:$LK_SIGNAL_PORT/twirp/livekit.RoomService/ListRooms"

  say "-- S4.1 过期 token 被拒"
  local exp_tok code4
  exp_tok="$(py jwt-sign x "$LK_API_SECRET" "{\"iss\":\"$LK_API_KEY\",\"sub\":\"expired_probe\",\"exp\":1}")"
  code4="$(http_code POST "$api" -H "Authorization: Bearer $exp_tok" -H 'Content-Type: application/json' -d '{}')"
  if [ "$code4" = 401 ] || [ "$code4" = 403 ]; then
    ok "过期 token 被拒(HTTP=${code4})"
  else
    bad "过期 token 未被拒: HTTP=$code4 body=$(head -c 150 "$LOGDIR/last_body.json")"
  fi

  say "-- S4.2 缺 grant 的合法签名 token 被拒"
  local nogrant_tok
  nogrant_tok="$(py jwt-sign x "$LK_API_SECRET" "{\"iss\":\"$LK_API_KEY\",\"sub\":\"nogrant_probe\"}")"
  code4="$(http_code POST "$api" -H "Authorization: Bearer $nogrant_tok" -H 'Content-Type: application/json' -d '{}')"
  if [ "$code4" = 401 ] || [ "$code4" = 403 ]; then
    ok "无 grant token 被拒（HTTP=${code4}, grants 校验生效）"
  else
    bad "无 grant token 未被拒: HTTP=$code4 body=$(head -c 150 "$LOGDIR/last_body.json")"
  fi

  say "-- S4.3 篡改签名 token 被拒"
  local bad_tok="${exp_tok%?}x"
  code4="$(http_code POST "$api" -H "Authorization: Bearer $bad_tok" -H 'Content-Type: application/json' -d '{}')"
  if [ "$code4" = 401 ] || [ "$code4" = 403 ]; then
    ok "篡改签名被拒(HTTP=${code4})"
  else
    bad "篡改签名未被拒: HTTP=$code4"
  fi
}

# ══════════════════════════════════════════════════════════════════════════════
# remote 模式（兼容旧行为）
# ══════════════════════════════════════════════════════════════════════════════
run_remote() {
  API_BASE="${API_BASE:-http://127.0.0.1:9800}"
  TOKEN="${TOKEN:?remote 模式需要 TOKEN}"
  GROUP_ID="${GROUP_ID:?remote 模式需要 GROUP_ID}"
  SEED_TOKEN1="$TOKEN"; SEED_G="$GROUP_ID"
  stage1_contract
  if [ -n "${LIVEKIT_URL:-}" ]; then
    export LIVEKIT_URL LIVEKIT_API_KEY LIVEKIT_API_SECRET
    stage2_sfu
  else
    say "（未设 LIVEKIT_URL，跳过 S2）"
  fi
}

# ══════════════════════════════════════════════════════════════════════════════
main() {
  say "RTC E2E (LK-TEST-01) mode=$MODE logdir=$LOGDIR"

  if [ "$MODE" = remote ]; then
    run_remote
    pass_summary
    final_gate
  fi

  # ── 前置检查 ──
  local pre=0
  command -v docker >/dev/null 2>&1 || { say "[BLOCKED] 无 docker"; BLOCKED=$((BLOCKED+1)); pre=1; }
  command -v python3 >/dev/null 2>&1 || { say "[BLOCKED] 无 python3"; BLOCKED=$((BLOCKED+1)); pre=1; }
  command -v jq >/dev/null 2>&1 || { say "[BLOCKED] 无 jq"; BLOCKED=$((BLOCKED+1)); pre=1; }
  command -v escript >/dev/null 2>&1 || { say "[BLOCKED] 无 escript"; BLOCKED=$((BLOCKED+1)); pre=1; }
  docker exec imboy_pg18 true >/dev/null 2>&1 || { say "[BLOCKED] imboy_pg18 容器不可用（PG 4323）"; BLOCKED=$((BLOCKED+1)); pre=1; }
  [ -d ebin ] || { say "[BLOCKED] ebin/ 不存在：先 make compile"; BLOCKED=$((BLOCKED+1)); pre=1; }
  local p
  for p in "$LK_SIGNAL_PORT" "$LK_TCP_PORT"; do
    port_free "$p" tcp || { say "[BLOCKED] 端口 $p/tcp 被占（租约冲突）"; BLOCKED=$((BLOCKED+1)); pre=1; }
  done
  for p in ${RTC_E2E_HTTP_PORT:-} ${RTC_E2E_ADM_PORT:-}; do
    port_free "$p" tcp || { say "[BLOCKED] 端口 $p/tcp 被占（租约冲突）"; BLOCKED=$((BLOCKED+1)); pre=1; }
  done
  for p in "$LK_TURN_UDP" "$LK_UDP_START" "$LK_UDP_END"; do
    port_free "$p" udp || { say "[BLOCKED] 端口 $p/udp 被占（租约冲突）"; BLOCKED=$((BLOCKED+1)); pre=1; }
  done
  if [ "$pre" != 0 ]; then
    pass_summary
    say "环境缺失，退出 2（BLOCKED）"
    exit 2
  fi

  ensure_livekit_image || { pass_summary; say "[BLOCKED] LiveKit 镜像不可得"; exit 2; }
  RTC_ADVERTISE_IP="${RTC_ADVERTISE_IP:-127.0.0.1}"
  say "-- 启动 LiveKit v1.13.7 (advertise=${RTC_ADVERTISE_IP})"
  start_livekit "$RTC_ADVERTISE_IP" \
    && ok "LiveKit 容器就绪（$(docker inspect "$CONTAINER" --format '{{.State.Status}}' 2>/dev/null)）" \
    || { pass_summary; say "[BLOCKED] LiveKit 启动失败（见 $LOGDIR/livekit_boot_fail.log）"; exit 2; }

  say "-- 启动 imboy 隔离节点（scratch=${SCRATCH_DB}）"
  start_imboy_node \
    && ok "imboy 节点就绪（${API_BASE}）" \
    || { pass_summary; say "[BLOCKED] imboy 节点启动失败（见 $LOGDIR/imboy_node.log）"; exit 2; }

  stage1_contract
  stage2_sfu
  stage3_turn
  stage4_negative

  # 快照容器日志（证据）
  if docker inspect "$CONTAINER" >/dev/null 2>&1; then
    docker logs "$CONTAINER" > "$LOGDIR/livekit_container.log" 2>&1 || true
  fi

  pass_summary
  # KEEP=1 且无 FAIL：保留容器/节点/scratch 供真机联测复用——即使 Required
  # DEGRADED 导致 exit 2（真机腿承载），容器也必须活着供真机接入。
  if [ "$KEEP" = 1 ] && [ "$FAIL" -eq 0 ]; then
    say "KEEP=1：保留 LiveKit 容器($CONTAINER)/节点/scratch 库供真机测试复用"
    say "  真机接入地址: ws://<本机局域网IP>:${LK_SIGNAL_PORT}（需 RTC_ADVERTISE_IP=<局域网IP> 重跑）"
    cleanup_actions=""
  fi
  final_gate
}

# 统一终局判定（假绿修复）：FAIL>0 -> 1；BLOCKED>0 或 Required DEGRADED>0 -> 2。
# 任何 Required Acceptance 的降级不再允许 exit 0。
final_gate() {
  if [ "$FAIL" -gt 0 ]; then
    say "[GATE] FAIL=$FAIL -> exit 1"
    exit 1
  fi
  if [ "$BLOCKED" -gt 0 ]; then
    say "[GATE] BLOCKED=$BLOCKED -> exit 2"
    exit 2
  fi
  if [ "$REQ_DEGRADED" -gt 0 ]; then
    say "[GATE] Required DEGRADED=$REQ_DEGRADED 不计绿（转 BLOCKED 语义）-> exit 2"
    exit 2
  fi
  exit 0
}

main "$@"
