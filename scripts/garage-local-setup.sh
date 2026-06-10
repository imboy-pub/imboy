#!/usr/bin/env bash
# Garage 本地测试环境一键启动脚本
# 用法：bash scripts/garage-local-setup.sh
set -euo pipefail

CONTAINER=garage-local
IMAGE=dxflrs/garage:v2.0.0
BUCKET=imboy
S3_PORT=3900
RPC_PORT=3901
# 复用已有数据卷时须传入原 GARAGE_RPC_SECRET，否则自动生成
# Pass original GARAGE_RPC_SECRET when reusing an existing data volume; otherwise auto-generated
RPC_SECRET="${GARAGE_RPC_SECRET:-$(openssl rand -hex 32)}"

echo "==> 停止并清理旧容器..."
docker rm -f "$CONTAINER" 2>/dev/null || true

echo "==> 启动 Garage..."
docker run -d --name "$CONTAINER" \
  -p ${S3_PORT}:3900 -p ${RPC_PORT}:3901 \
  -e GARAGE_METADATA_DIR=/tmp/garage/meta \
  -e GARAGE_DATA_DIR=/tmp/garage/data \
  -e GARAGE_RPC_BIND_ADDR=0.0.0.0:3901 \
  -e GARAGE_S3_API_BIND_ADDR=0.0.0.0:3900 \
  -e GARAGE_RPC_SECRET=$RPC_SECRET \
  "$IMAGE"

echo "==> 等待 Garage 就绪（最多 30s）..."
for i in $(seq 1 30); do
  if curl -sf http://127.0.0.1:${S3_PORT}/ >/dev/null 2>&1; then
    echo "    就绪！"
    break
  fi
  sleep 1
done

echo "==> 获取节点 ID..."
NODE_ID=$(docker exec "$CONTAINER" /garage node id 2>/dev/null | awk '{print $1}' | head -1)
echo "    Node ID: ${NODE_ID:0:16}..."

echo "==> 配置节点布局..."
docker exec "$CONTAINER" /garage layout assign -z dc1 -c 1G "$NODE_ID"
docker exec "$CONTAINER" /garage layout apply --version 1

echo "==> 创建 bucket: $BUCKET..."
docker exec "$CONTAINER" /garage bucket create "$BUCKET" 2>/dev/null || echo "    (已存在)"

echo "==> 创建访问密钥 imboy-key..."
KEY_OUTPUT=$(docker exec "$CONTAINER" /garage key create imboy-key 2>/dev/null \
  || docker exec "$CONTAINER" /garage key info imboy-key 2>/dev/null)

echo "==> 授权密钥访问 bucket..."
ACCESS_KEY=$(echo "$KEY_OUTPUT" | grep -i "key id" | awk '{print $NF}')
if [ -z "$ACCESS_KEY" ]; then
  echo "    ✗ 解析 ACCESS_KEY 失败，请检查 garage key create/info 输出格式" >&2
  exit 1
fi
# 标准语法：bucket 名作为位置参数置于最前；不再 || true 静默吞错，授权失败即退出
docker exec "$CONTAINER" /garage bucket allow "$BUCKET" \
  --read --write --owner --key "$ACCESS_KEY"

# 注意：不设置 bucket 公开读（--read --public）。
# 私有附件经后端 /v1/attachment/view_url 按需签发短时 presigned GET，避免整桶匿名可读。

SECRET_KEY=$(echo "$KEY_OUTPUT" | grep -i "secret" | awk '{print $NF}')

echo ""
echo "╔══════════════════════════════════════════════════════════╗"
echo "║  Garage 已就绪！将以下配置写入 config/sys.local.config  ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""
echo ", {garage, #{"
echo "    endpoint   => <<\"http://127.0.0.1:${S3_PORT}\">>"
echo "    region     => <<\"garage\">>"
echo "    bucket     => <<\"${BUCKET}\">>"
echo "    access_key => <<\"${ACCESS_KEY}\">>"
echo "    secret_key => <<\"${SECRET_KEY}\">>"
echo "}}"
echo ""
echo "==> 测试 S3 端口连通性:"
curl -si http://127.0.0.1:${S3_PORT}/ | head -2
echo ""
echo "==> 测试上传（需要 aws CLI 或 curl + SigV4）:"
echo "    curl -X PUT http://127.0.0.1:${S3_PORT}/${BUCKET}/test.txt \\"
echo "         --aws-sigv4 \"aws:amz:garage:s3\" \\"
echo "         --user \"${ACCESS_KEY}:${SECRET_KEY}\" \\"
echo "         -d 'hello garage'"
