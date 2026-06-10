#!/usr/bin/env bash
# Garage 二进制安装脚本（非 docker）/ Garage binary installer (no docker)
# 自动识别平台：macOS Apple Silicon → 本地开发模式；Linux → 生产 systemd 模式
# Auto-detects: macOS → local dev mode; Linux → production systemd mode
#
# 用法 / Usage:
#   bash scripts/garage-install.sh
#
# 说明 / Notes:
#   - 与 docker 版 scripts/garage-local-setup.sh 共存，本脚本走二进制安装。
#   - 幂等：重复运行不会重置已有密钥与数据。
#   - 安全：不开放整桶公开读，私有附件经后端 presigned GET 下载。
set -euo pipefail

# ============ 可调参数 / Tunables ============
GARAGE_VERSION="v2.3.0"
BUCKET="imboy"
REGION="garage"          # 必须与 Erlang sys.config / Flutter region 完全一致
KEY_NAME="imboy-key"
S3_PORT=3900
RPC_PORT=3901
ADMIN_PORT=3903
DOWNLOAD_BASE="https://garagehq.deuxfleurs.fr/api/v1/download"

# ============ 平台检测 / Platform detection ============
OS="$(uname -s)"
ARCH="$(uname -m)"
case "${OS}-${ARCH}" in
  Darwin-arm64)   PLATFORM="aarch64-apple-darwin";       MODE="dev"  ;;
  Darwin-x86_64)  PLATFORM="x86_64-apple-darwin";        MODE="dev"  ;;
  Linux-x86_64)   PLATFORM="x86_64-unknown-linux-musl";  MODE="prod" ;;
  Linux-aarch64|Linux-arm64) PLATFORM="aarch64-unknown-linux-musl"; MODE="prod" ;;
  *) echo "✗ 不支持的平台 / Unsupported platform: ${OS}-${ARCH}" >&2; exit 1 ;;
esac

# 生产模式可能需要 sudo；开发模式尽量不用 / Use sudo only when not root
if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

if [ "$MODE" = "dev" ]; then
  TOML="${HOME}/garage.toml"
  META_DIR="/tmp/garage/meta"
  DATA_DIR="/tmp/garage/data"
  API_BIND="127.0.0.1:${S3_PORT}"
  BIN_PATH="${HOME}/.local/bin/garage"   # 用户可写路径，无需 sudo
  GG_SUDO=""                              # 开发模式全程不用 sudo
else
  TOML="/etc/garage.toml"
  META_DIR="/var/lib/garage/meta"
  DATA_DIR="/var/lib/garage/data"
  API_BIND="0.0.0.0:${S3_PORT}"
  BIN_PATH="/usr/local/bin/garage"
  GG_SUDO="$SUDO"
fi

echo "==> 平台 / Platform : ${OS}-${ARCH} (${PLATFORM})"
echo "==> 模式 / Mode     : ${MODE}"
echo "==> 配置 / Config   : ${TOML}"

# CLI 封装：dev 免 sudo，prod 需 sudo 读取 root 配置；使用解析到的实际二进制
gg() { $GG_SUDO "$GARAGE_BIN" -c "$TOML" "$@"; }

# ============ 1. 安装二进制 / Install binary ============
GARAGE_BIN=""
if command -v garage >/dev/null 2>&1; then
  GARAGE_BIN="$(command -v garage)"
  cur="$(garage --version 2>/dev/null | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1 || true)"
  echo "==> 复用已安装 garage ${cur:-未知} / reusing: ${GARAGE_BIN}"
  if [ "$MODE" = "prod" ] && [ "$cur" != "$GARAGE_VERSION" ]; then
    echo "    (生产建议 ${GARAGE_VERSION}，当前 ${cur:-未知}，继续使用现有)"
  fi
else
  echo "==> 未检测到 garage，下载 ${GARAGE_VERSION} / downloading"
  mkdir -p "$(dirname "$BIN_PATH")"
  tmpbin="$(mktemp)"
  url="${DOWNLOAD_BASE}?version=${GARAGE_VERSION}&platform=${PLATFORM}"
  echo "    ${url}"
  curl -fsSL -o "$tmpbin" "$url"
  chmod +x "$tmpbin"
  if [ "$MODE" = "dev" ]; then mv "$tmpbin" "$BIN_PATH"; else $SUDO mv "$tmpbin" "$BIN_PATH"; fi
  GARAGE_BIN="$BIN_PATH"
  echo "    已安装到 ${BIN_PATH}"
fi
"$GARAGE_BIN" --version

# ============ 2. 生成配置 / Generate config ============
# 已存在则保留旧配置（复用 rpc_secret，避免与已有数据/密钥失配）
if [ -f "$TOML" ]; then
  echo "==> 配置已存在，保留 / Config exists, keeping: ${TOML}"
else
  echo "==> 生成配置 / Writing config: ${TOML}"
  RPC_SECRET="$(openssl rand -hex 32)"
  ADMIN_TOKEN="$(openssl rand -hex 32)"
  conf="$(cat <<EOF
metadata_dir       = "${META_DIR}"
data_dir           = "${DATA_DIR}"
db_engine          = "lmdb"
replication_factor = 1
rpc_bind_addr      = "127.0.0.1:${RPC_PORT}"
rpc_secret         = "${RPC_SECRET}"

[s3_api]
s3_region     = "${REGION}"
api_bind_addr = "${API_BIND}"

[admin]
api_bind_addr = "127.0.0.1:${ADMIN_PORT}"
admin_token   = "${ADMIN_TOKEN}"
EOF
)"
  if [ "$MODE" = "dev" ]; then
    printf '%s\n' "$conf" > "$TOML"
  else
    printf '%s\n' "$conf" | $SUDO tee "$TOML" >/dev/null
    $SUDO chmod 600 "$TOML"
  fi
fi

# ============ 3. 准备数据目录 / Prepare data dirs ============
if [ "$MODE" = "dev" ]; then
  mkdir -p "$META_DIR" "$DATA_DIR"
else
  $SUDO mkdir -p "$META_DIR" "$DATA_DIR"
  if ! id garage >/dev/null 2>&1; then
    $SUDO useradd -r -s /bin/false garage
  fi
  $SUDO chown -R garage:garage /var/lib/garage "$TOML"
fi

# ============ 4. 启动服务 / Start service ============
if [ "$MODE" = "dev" ]; then
  if pgrep -f "garage -c ${TOML} server" >/dev/null 2>&1; then
    echo "==> Garage 已在运行 / already running"
  else
    echo "==> 后台启动 Garage / Starting in background (nohup)"
    nohup "$GARAGE_BIN" -c "$TOML" server >/tmp/garage/garage.log 2>&1 &
    echo "    日志 / log: /tmp/garage/garage.log"
  fi
else
  SERVICE="/etc/systemd/system/garage.service"
  if [ ! -f "$SERVICE" ]; then
    echo "==> 写入 systemd 服务 / Writing systemd unit"
    $SUDO tee "$SERVICE" >/dev/null <<EOF
[Unit]
Description=Garage S3-compatible object store
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=${BIN_PATH} -c ${TOML} server
Restart=on-failure
RestartSec=5s
User=garage
Group=garage
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ReadWritePaths=/var/lib/garage

[Install]
WantedBy=multi-user.target
EOF
  fi
  $SUDO systemctl daemon-reload
  $SUDO systemctl enable --now garage
fi

# ============ 5. 等待就绪 / Wait until ready ============
echo "==> 等待 S3 端口就绪（最多 30s）/ Waiting for S3 port..."
ready=0
for _ in $(seq 1 30); do
  # garage 对匿名 GET / 返回 403 属正常；拿到任意 HTTP 状态码即说明端口已监听
  code="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${S3_PORT}/" 2>/dev/null || true)"
  if [ -n "$code" ] && [ "$code" != "000" ]; then ready=1; break; fi
  sleep 1
done
[ "$ready" -eq 1 ] && echo "    就绪 / ready" || { echo "✗ 启动超时，请检查日志 / startup timeout" >&2; exit 1; }

# ============ 6. 初始化布局 / Cluster layout（幂等）============
# 用 garage 原生命令取节点 ID（格式 <hex>@<addr>），比解析 status 表格稳健
NODE_ID="$(gg node id 2>/dev/null | head -1 | cut -d'@' -f1)"
if [ -z "${NODE_ID:-}" ]; then
  echo "✗ 无法获取 Node ID / cannot get node id" >&2; exit 1
fi
echo "==> Node ID: ${NODE_ID:0:16}..."
# 配置单节点布局（幂等）：assign 入 staging，再按 garage 提示的版本号 apply
gg layout assign -z dc1 -c 1G "$NODE_ID" 2>/dev/null || true
NEXT_VER="$(gg layout show 2>/dev/null | grep -oE 'apply --version [0-9]+' | grep -oE '[0-9]+' | head -1)"
if [ -n "${NEXT_VER:-}" ]; then
  gg layout apply --version "$NEXT_VER" && echo "    布局已应用 version ${NEXT_VER}"
else
  echo "    (布局无待应用变更 / layout up to date)"
fi
# 等待布局生效（bucket list 成功即就绪）
for _ in $(seq 1 10); do gg bucket list >/dev/null 2>&1 && break; sleep 1; done

# ============ 7. bucket 与密钥 / Bucket & key（幂等）============
echo "==> 创建 bucket: ${BUCKET}"
gg bucket create "$BUCKET" 2>/dev/null || echo "    (已存在 / exists)"

echo "==> 获取/创建访问密钥: ${KEY_NAME}"
KEY_OUT="$(gg key info "$KEY_NAME" --show-secret 2>/dev/null || gg key create "$KEY_NAME")"
ACCESS_KEY="$(printf '%s\n' "$KEY_OUT" | grep -i "Key ID"     | awk '{print $NF}' | head -1)"
SECRET_KEY="$(printf '%s\n' "$KEY_OUT" | grep -i "Secret key" | awk '{print $NF}' | head -1)"
if [ -z "$ACCESS_KEY" ]; then
  echo "✗ 解析 ACCESS_KEY 失败，请检查 garage key 输出格式" >&2; exit 1
fi

# 仅授权服务端密钥读写，绝不开放整桶公开读
# Authorize server key only; NEVER enable bucket-wide public-read.
echo "==> 授权密钥访问 bucket / Authorizing key"
gg bucket allow "$BUCKET" --read --write --owner --key "$ACCESS_KEY"

# ============ 8. 输出 Erlang 配置 / Emit Erlang config ============
cat <<EOF

╔════════════════════════════════════════════════════════════════╗
║  Garage 已就绪！将以下配置写入 imboy/config/sys.local.config    ║
║  Garage ready! Add the following to sys.local.config            ║
╚════════════════════════════════════════════════════════════════╝

, {garage, #{
    endpoint   => <<"http://127.0.0.1:${S3_PORT}">>,
    region     => <<"${REGION}">>,
    bucket     => <<"${BUCKET}">>,
    access_key => <<"${ACCESS_KEY}">>,
    secret_key => <<"${SECRET_KEY:-<已存在，请用 garage key info ${KEY_NAME} --show-secret 查看>}">>
}}

EOF

if [ "$MODE" = "dev" ]; then
  echo "管理命令 / Manage: garage -c ${TOML} status | bucket list | key list"
  echo "停止 / Stop:       pkill -f 'garage -c ${TOML} server'"
else
  echo "管理命令 / Manage: sudo systemctl status garage ; journalctl -u garage -f"
fi
