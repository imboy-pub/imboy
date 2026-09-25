#!/usr/bin/env bash
# Garage Linux 二进制安装脚本（非 docker）/ Garage Linux binary installer
# macOS 没有 v2.4.1 官方二进制，请改用 scripts/garage-local-setup.sh。
#
# 用法 / Usage:
#   bash scripts/garage-install.sh
#
# 说明 / Notes:
#   - 与 docker 版 scripts/garage-local-setup.sh 共存，本脚本走二进制安装。
#   - 幂等：重复运行不会重置已有密钥与数据。
#   - 安全：私有桶不开放匿名读；只有 imboy-public 通过 Website API 公开。
set -euo pipefail

# ============ 可调参数 / Tunables ============
GARAGE_VERSION="v2.4.1"
BUCKET="imboy"
PUBLIC_BUCKET="imboy-public"
REGION="garage"          # 必须与 Erlang sys.config / Flutter region 完全一致
KEY_NAME="imboy-key"
S3_PORT=3900
RPC_PORT=3901
WEB_PORT=3902
ADMIN_PORT=3903
DOWNLOAD_BASE="https://garagehq.deuxfleurs.fr/_releases/${GARAGE_VERSION}"

# ============ 平台检测 / Platform detection ============
OS="$(uname -s)"
ARCH="$(uname -m)"
case "${OS}-${ARCH}" in
  Darwin-*)
    echo "✗ Garage ${GARAGE_VERSION} 官方未提供 macOS 二进制；请运行 scripts/garage-local-setup.sh" >&2
    exit 1
    ;;
  Linux-x86_64)   PLATFORM="x86_64-unknown-linux-musl" ;;
  Linux-aarch64|Linux-arm64) PLATFORM="aarch64-unknown-linux-musl" ;;
  *) echo "✗ 不支持的平台 / Unsupported platform: ${OS}-${ARCH}" >&2; exit 1 ;;
esac

# 非 root 用户通过 sudo 安装系统服务。
if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

TOML="/etc/garage.toml"
META_DIR="/var/lib/garage/meta"
DATA_DIR="/var/lib/garage/data"
API_BIND="0.0.0.0:${S3_PORT}"
BIN_PATH="/usr/local/bin/garage"

echo "==> 平台 / Platform : ${OS}-${ARCH} (${PLATFORM})"
echo "==> 配置 / Config   : ${TOML}"

# CLI 使用解析到的实际二进制，并通过 sudo 读取 root 配置。
gg() { $SUDO "$GARAGE_BIN" -c "$TOML" "$@"; }

# ============ 1. 安装二进制 / Install binary ============
GARAGE_BIN=""
if command -v garage >/dev/null 2>&1; then
  GARAGE_BIN="$(command -v garage)"
  cur="$(garage --version 2>/dev/null | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1 || true)"
  echo "==> 复用已安装 garage ${cur:-未知} / reusing: ${GARAGE_BIN}"
  if [ "$cur" != "$GARAGE_VERSION" ]; then
    echo "    (生产建议 ${GARAGE_VERSION}，当前 ${cur:-未知}，继续使用现有)"
  fi
else
  echo "==> 未检测到 garage，下载 ${GARAGE_VERSION} / downloading"
  mkdir -p "$(dirname "$BIN_PATH")"
  tmpbin="$(mktemp)"
  url="${DOWNLOAD_BASE}/${PLATFORM}/garage"
  echo "    ${url}"
  curl -fsSL -o "$tmpbin" "$url"
  chmod +x "$tmpbin"
  $SUDO mv "$tmpbin" "$BIN_PATH"
  GARAGE_BIN="$BIN_PATH"
  echo "    已安装到 ${BIN_PATH}"
fi
"$GARAGE_BIN" --version

# ============ 2. 生成配置 / Generate config ============
# 已存在则保留旧配置（复用 rpc_secret，避免与已有数据/密钥失配）
if [ -f "$TOML" ]; then
  echo "==> 配置已存在，保留 / Config exists, keeping: ${TOML}"
  if ! $SUDO grep -q '^\[s3_web\]' "$TOML"; then
    echo "✗ 现有 ${TOML} 缺少 [s3_web]，为避免改坏已有配置，本脚本不会自动追加。" >&2
    echo "  请按 docs/guides/operations/garage-deployment.md 配置 Website API 后重跑。" >&2
    exit 1
  fi
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

[s3_web]
bind_addr   = "127.0.0.1:${WEB_PORT}"
root_domain = ".garage.localhost"
index       = "index.html"

[admin]
api_bind_addr = "127.0.0.1:${ADMIN_PORT}"
admin_token   = "${ADMIN_TOKEN}"
EOF
)"
  printf '%s\n' "$conf" | $SUDO tee "$TOML" >/dev/null
  $SUDO chmod 600 "$TOML"
fi

# ============ 3. 准备数据目录 / Prepare data dirs ============
$SUDO mkdir -p "$META_DIR" "$DATA_DIR"
if ! id garage >/dev/null 2>&1; then
  $SUDO useradd -r -s /bin/false garage
fi
$SUDO chown -R garage:garage /var/lib/garage "$TOML"

# ============ 4. 启动服务 / Start service ============
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
ExecStart=${GARAGE_BIN} -c ${TOML} server
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

# ============ 5. 等待就绪 / Wait until ready ============
echo "==> 等待 S3 端口就绪（最多 30s）/ Waiting for S3 port..."
ready=0
for _ in $(seq 1 30); do
  # garage 对匿名 GET / 返回 403 属正常；拿到任意 HTTP 状态码即说明端口已监听
  code="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${S3_PORT}/" 2>/dev/null || true)"
  if [ -n "$code" ] && [ "$code" != "000" ]; then ready=1; break; fi
  sleep 1
done
if [ "$ready" -eq 1 ]; then
  echo "    就绪 / ready"
else
  echo "✗ 启动超时，请检查日志 / startup timeout" >&2
  exit 1
fi

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
echo "==> 创建 buckets: ${BUCKET} / ${PUBLIC_BUCKET}"
gg bucket create "$BUCKET" 2>/dev/null || echo "    (已存在 / exists)"
gg bucket create "$PUBLIC_BUCKET" 2>/dev/null || echo "    (${PUBLIC_BUCKET} 已存在 / exists)"

echo "==> 获取/创建访问密钥: ${KEY_NAME}"
KEY_OUT="$(gg key info "$KEY_NAME" --show-secret 2>/dev/null || gg key create "$KEY_NAME")"
ACCESS_KEY="$(printf '%s\n' "$KEY_OUT" | grep -i "Key ID"     | awk '{print $NF}' | head -1)"
SECRET_KEY="$(printf '%s\n' "$KEY_OUT" | grep -i "Secret key" | awk '{print $NF}' | head -1)"
if [ -z "$ACCESS_KEY" ]; then
  echo "✗ 解析 ACCESS_KEY 失败，请检查 garage key 输出格式" >&2; exit 1
fi

# 服务端密钥可读写两个桶；只有独立 public 桶启用 Website 匿名读取。
echo "==> 授权密钥访问 bucket / Authorizing key"
gg bucket allow "$BUCKET" --read --write --owner --key "$ACCESS_KEY"
gg bucket allow "$PUBLIC_BUCKET" --read --write --owner --key "$ACCESS_KEY"
gg bucket website --allow "$PUBLIC_BUCKET"

# ============ 8. 输出 Erlang 配置 / Emit Erlang config ============
cat <<EOF

╔════════════════════════════════════════════════════════════════╗
║  Garage 已就绪！将以下配置写入 imboy/config/sys.local.config    ║
║  Garage ready! Add the following to sys.local.config            ║
╚════════════════════════════════════════════════════════════════╝

, {garage, #{
    endpoint         => <<"http://127.0.0.1:${S3_PORT}">>,
    public_endpoint  => <<"https://api.example.com/s3">>,
    region           => <<"${REGION}">>,
    bucket           => <<"${BUCKET}">>,
    public_bucket    => <<"${PUBLIC_BUCKET}">>,
    public_base_url  => <<"https://files.example.com">>,
    access_key => <<"${ACCESS_KEY}">>,
    secret_key => <<"${SECRET_KEY:-<已存在，请用 garage key info ${KEY_NAME} --show-secret 查看>}">>
}}

EOF

echo "管理命令 / Manage: sudo systemctl status garage ; journalctl -u garage -f"
