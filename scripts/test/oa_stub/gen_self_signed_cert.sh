#!/usr/bin/env bash
# Q08 / AC20 — OA 页面替身用的自签 TLS 证书生成脚本【仅测试用途】。
#
# ⚠️⚠️⚠️ 仅限本地测试（Q08 OA 替身 / phase1 节点验证）使用 ⚠️⚠️⚠️
#   - 证书为自签、无 CA 链、CN/SAN 仅 localhost + 127.0.0.1，有效期 2 天；
#   - 绝不用于任何生产环境、绝不分发给第三方、绝不用于真实域名；
#   - 生成物（cert/key）只应写入临时 RUN_DIR（/tmp），不进 git、不进交付包。
#
# 用法: gen_self_signed_cert.sh <输出目录>
# 产物: <输出目录>/oa_stub.crt  <输出目录>/oa_stub.key
set -euo pipefail
OUT="${1:?usage: $0 <output-dir>}"
mkdir -p "$OUT"
openssl req -x509 -newkey rsa:2048 -nodes -days 2 \
  -subj "/C=CN/O=IMBoy-Q08-TEST-ONLY/CN=localhost" \
  -addext "subjectAltName=DNS:localhost,IP:127.0.0.1" \
  -addext "basicConstraints=critical,CA:FALSE" \
  -keyout "$OUT/oa_stub.key" -out "$OUT/oa_stub.crt" \
  >/dev/null 2>&1
chmod 600 "$OUT/oa_stub.key"
echo "TEST-ONLY self-signed cert: $OUT/oa_stub.crt (CN=localhost, SAN localhost/127.0.0.1, 2d)"
