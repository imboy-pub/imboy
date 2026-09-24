#!/usr/bin/env bash
# Garage 本地 dev 环境一键脚本（配置文件挂载配方）
# 用法：bash scripts/garage-local-setup.sh
#
# 背景（2026-09-12 重建实录）：Garage v2 的容器入口不再支持
# GARAGE_METADATA_DIR / GARAGE_DATA_DIR / GARAGE_S3_API_BIND_ADDR 等
# env-only 启动配方（创建即退出）。必须挂载配置文件：
#   /tmp/garage/garage.toml  → /etc/garage.toml (ro)
#   /tmp/garage/meta         → /tmp/garage/meta   (bind)
#   /tmp/garage/data         → /tmp/garage/data   (bind)
# 端口：S3 API 3900 / RPC 3901 / 公共 Web 3902（+admin 3909 仅本地回环）。
#
# 幂等：容器已在跑则直接复用（不删数据不重建），并继续检查布局、桶、权限和 Website。
# 密钥 secret 只在创建时可见：重建前可导出
# GARAGE_ACCESS_KEY / GARAGE_SECRET_KEY / GARAGE_RPC_SECRET 复用旧值。
set -euo pipefail

CONTAINER=garage-local
IMAGE=dxflrs/garage:v2.4.1
GARAGE_DIR=/tmp/garage
CONF=$GARAGE_DIR/garage.toml
BUCKET=imboy
PUBLIC_BUCKET=imboy-public
S3_PORT=3900
RPC_PORT=3901
WEB_PORT=3902

garage() { docker exec "$CONTAINER" /garage "$@"; }

echo "==> 检查容器 $CONTAINER ..."
if docker ps --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  echo "    容器已运行 → 复用并幂等检查布局、桶、密钥和 Website 配置"
else
  echo "==> 清理旧容器（数据在 ${GARAGE_DIR}，不受影响）..."
  docker rm -f "$CONTAINER" 2>/dev/null || true

  echo "==> 准备配置与数据目录..."
  mkdir -p "$GARAGE_DIR/meta" "$GARAGE_DIR/data"

  # rpc_secret 优先级：环境变量 > 既有配置文件 > 新生成
  if [ -z "${GARAGE_RPC_SECRET:-}" ] && [ -f "$CONF" ]; then
    GARAGE_RPC_SECRET=$(sed -n 's/^rpc_secret *= *"\(.*\)"/\1/p' "$CONF" | head -1)
  fi
  RPC_SECRET="${GARAGE_RPC_SECRET:-$(openssl rand -hex 32)}"

  if [ -f "$CONF" ] && grep -q "rpc_secret *= *\"${RPC_SECRET}\"" "$CONF"; then
    echo "    复用既有 ${CONF}"
  else
    cat > "$CONF" <<EOF
# Garage 本地 dev 容器配置（scripts/garage-local-setup.sh 生成）
# S3 API 0.0.0.0:3900（真机经 LAN IP 直传）、RPC 3901、公共 Web 3902
metadata_dir       = "${GARAGE_DIR}/meta"
data_dir           = "${GARAGE_DIR}/data"
db_engine          = "lmdb"
replication_factor = 1
rpc_bind_addr      = "0.0.0.0:${RPC_PORT}"
rpc_secret         = "${RPC_SECRET}"

[s3_api]
s3_region     = "garage"
api_bind_addr = "0.0.0.0:${S3_PORT}"

[s3_web]
bind_addr   = "0.0.0.0:${WEB_PORT}"
root_domain = ".garage.localhost"
index       = "index.html"

[admin]
api_bind_addr = "127.0.0.1:3909"
EOF
    echo "    已写入 ${CONF}"
  fi

  echo "==> 启动 Garage（配置文件挂载配方）..."
  docker run -d --name "$CONTAINER" \
    -p ${S3_PORT}:3900 -p ${RPC_PORT}:3901 -p ${WEB_PORT}:3902 \
    -v "$CONF:/etc/garage.toml:ro" \
    -v "$GARAGE_DIR/meta:/tmp/garage/meta" \
    -v "$GARAGE_DIR/data:/tmp/garage/data" \
    "$IMAGE"
fi

echo "==> 等待 Garage 就绪（最多 30s）..."
ready=0
for _ in $(seq 1 30); do
  # 匿名 GET / 正常返回 403；任意非 000 HTTP 状态都表示端口已监听。
  code="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${S3_PORT}/" 2>/dev/null || true)"
  if [ -n "$code" ] && [ "$code" != "000" ]; then
    ready=1
    echo "    就绪！"
    break
  fi
  sleep 1
done
[ "$ready" -eq 1 ] || { echo "    ✗ Garage 启动超时，请检查 docker logs ${CONTAINER}" >&2; exit 1; }

echo "==> 获取节点 ID 并配置布局..."
NODE_ID=$(garage node id 2>/dev/null | awk '{print $1}' | head -1)
echo "    Node ID: ${NODE_ID:0:16}..."
garage layout assign -z dc1 -c 1G "$NODE_ID" 2>/dev/null \
  || echo "    (布局已分配，跳过)"
CUR_VER=$(garage layout show 2>/dev/null | awk '/[Cc]urrent cluster layout version/{print $NF}' | head -1)
if garage layout apply --version $((CUR_VER + 1)) 2>/dev/null; then
  echo "    布局已生效 (version $((CUR_VER + 1)))"
else
  echo "    (布局无需更新，跳过)"
fi

echo "==> 创建桶: ${BUCKET} / ${PUBLIC_BUCKET}..."
garage bucket create "$BUCKET" 2>/dev/null || echo "    (${BUCKET} 已存在)"
garage bucket create "$PUBLIC_BUCKET" 2>/dev/null || echo "    (${PUBLIC_BUCKET} 已存在)"

echo "==> 创建/获取访问密钥 imboy-key..."
garage key create imboy-key 2>/dev/null || echo "    (imboy-key 已存在)"

echo "==> 授权密钥访问桶..."
if [ -n "${GARAGE_ACCESS_KEY:-}" ]; then
  ACCESS_KEY="$GARAGE_ACCESS_KEY"
else
  ACCESS_KEY=$(garage key info imboy-key 2>/dev/null | awk '/Key ID|key id/{print $NF}' | head -1)
  [ -n "$ACCESS_KEY" ] || ACCESS_KEY=$(garage key list 2>/dev/null | awk '/imboy-key/{print $1}' | head -1)
fi
if [ -z "$ACCESS_KEY" ]; then
  echo "    ✗ 解析 ACCESS_KEY 失败" >&2
  exit 1
fi
# bucket allow 是幂等授权（重复授权返回成功），失败必为真故障，不静默吞错
garage bucket allow "$BUCKET" --read --write --owner --key "$ACCESS_KEY" \
  || { echo "    ✗ ${BUCKET} 授权失败" >&2; exit 1; }
garage bucket allow "$PUBLIC_BUCKET" --read --write --owner --key "$ACCESS_KEY" \
  || { echo "    ✗ ${PUBLIC_BUCKET} 授权失败" >&2; exit 1; }
garage bucket website --allow "$PUBLIC_BUCKET" \
  || { echo "    ✗ ${PUBLIC_BUCKET} Website 公开读取启用失败" >&2; exit 1; }

# 私有桶不开放匿名读；只有 scope=public 使用的独立桶通过 Website API 对外读取。

echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║  Garage 已就绪！将以下配置写入 config/sys.local.config     ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""
echo ", {garage, #{"
echo "    endpoint         => <<\"http://127.0.0.1:${S3_PORT}\">>,"
echo "    public_endpoint  => <<\"http://127.0.0.1:${S3_PORT}\">>,"
echo "    region           => <<\"garage\">>,"
echo "    bucket           => <<\"${BUCKET}\">>,"
echo "    public_bucket    => <<\"${PUBLIC_BUCKET}\">>,"
echo "    public_base_url  => <<\"http://${PUBLIC_BUCKET}.garage.localhost:${WEB_PORT}\">>,"
if [ -n "${GARAGE_ACCESS_KEY:-}" ] || [ -n "${GARAGE_SECRET_KEY:-}" ]; then
  echo "    access_key => <<\"${GARAGE_ACCESS_KEY:-}\">>,"
  echo "    secret_key => <<\"${GARAGE_SECRET_KEY:-}\">>"
else
  echo "    access_key => <<\"${ACCESS_KEY}\">>,"
  echo "    secret_key => <<\"见下方提示\">>"
fi
echo "}}"
echo "    真机调试时请把 127.0.0.1/localhost 地址替换为手机可达的 HTTPS 域名。"
echo ""
if [ -z "${GARAGE_SECRET_KEY:-}" ]; then
  echo "ℹ  secret_key 仅在密钥**首次创建**时展示。若本次是复用旧数据卷："
  echo "   从 config/sys.local.config 沿用旧值，或重建前导出 GARAGE_SECRET_KEY。"
  echo "   （新建密钥场景可用: garage key info imboy-key --show-secret）"
fi
echo ""
echo "==> 测试 S3 端口连通性:"
curl -si http://127.0.0.1:${S3_PORT}/ | head -2
echo ""
echo "==> 测试上传（需要 aws CLI 或 curl + SigV4）:"
echo "    curl -X PUT http://127.0.0.1:${S3_PORT}/${BUCKET}/test.txt \\"
echo "         --aws-sigv4 \"aws:amz:garage:s3\" \\"
echo "         --user \"${ACCESS_KEY}:${GARAGE_SECRET_KEY:-<secret>}\" \\"
echo "         -d 'hello garage'"
