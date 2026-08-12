#!/usr/bin/env bash
# ADR 07 §6.3 / ADR 08 §4 守护：服务端零密码学（server zero crypto）。
#
# 不变量：E2EE 私钥/明文永不经服务端。服务端代码不得对 E2EE payload 调用解密，
# 不得引用客户端密码学库（libolm/vodozemac/megolm）做解密。零命中才通过（exit 0）。
#
# CI 关键路径运行；本地可 `bash scripts/check_server_zero_crypto.sh`。
# olm_identity_* 是密钥「存储/转发」模块（只存公钥侧），非解密，故不触发。
set -euo pipefail
cd "$(dirname "$0")/.."

FAIL=0

# 1. 不得对 E2EE payload 调用任何解密（ADR 07 §6.1）
HITS1=$(grep -rnE "elib_cipher[a-z_]*:[a-z_]*decrypt[^,]*e2ee" src --include="*.erl" || true)
if [ -n "$HITS1" ]; then
  echo "[FAIL] 服务端对 E2EE payload 调用了解密（违反 ADR 07 §6.1）："
  echo "$HITS1"
  FAIL=1
fi

# 2. 不得引用客户端密码学库做解密（libolm/vodozemac/megolm decrypt）
# olm_identity_* 只负责公钥存储/转发与签名校验，不执行 E2EE 解密；这里
# 必须和文件顶部的边界说明保持一致，否则仅在注释中提到 vodozemac 也会误报。
HITS2=$(grep -rnE "\b(libolm|vodozemac)\b|\bmegolm[a-z_]*decrypt" src --include="*.erl" --exclude="olm_identity_*.erl" || true)
if [ -n "$HITS2" ]; then
  echo "[FAIL] 服务端引用了客户端密码学库解密（违反 ADR 07 §6.1）："
  echo "$HITS2"
  FAIL=1
fi

# 3. 服务端不得计算 Safety Number（ADR 06 §8.1 零算法 / §9.4 T-06-14）
HITS3=$(grep -rniE "safety_?number" src --include="*.erl" || true)
if [ -n "$HITS3" ]; then
  echo "[FAIL] 服务端出现 Safety Number 计算（违反 ADR 06 §8.1 零算法）："
  echo "$HITS3"
  FAIL=1
fi

if [ "$FAIL" -eq 0 ]; then
  echo "[OK] 服务端零密码学守护通过：无 E2EE 解密命中。"
fi
exit "$FAIL"
