#!/usr/bin/env bash
# 发版后附件链路冒烟（2026-10-20 prod 事故防复发第 3 层）：
#   [1] healthz            节点活着
#   [2] login              mock 登录拿 token
#   [3] direct register    direct-v1 会话能开（prod 频道上传路径；空 registry 在此 503）
#   [4] presign private    私有根就位（scope=private 走私桶）
#   [5] presign public     公开根就位（scope=public 走公桶；public_bucket 缺失在此失败）
#   [6] upload+confirm     multipart 全链上传落库（服务端中转 → Garage → HEAD 核实）
#   [7] view_url + GET     读取链路全通（authorize → presign → Garage 读回字节比对）
# 事故对照：2026-10-20 prod registry 随发版丢失后，本脚本 [3][4][5][7] 全部当场红。
# Usage:  ./post_release_attachment_smoke.sh [BASE_URL]   # 默认 http://127.0.0.1:9700
# 前置：节点已启动；mock jscode2session 在 127.0.0.1:9817（本地联调形态）；
#       prod 无 mock 登录时改用 TOKEN=<jwt> 直接跳过 [2]（后续脚本用该 token）。
# Exit 0 = PASS, non-zero = FAIL。

set -u
set -o pipefail

BASE="${1:-http://127.0.0.1:9700}"
TMPDIR_LOC="${TMPDIR:-/tmp}"
UPLOAD_FILE="${TMPDIR_LOC}/post_release_smoke_$$.bin"
MIME="image/png"
SIZE_BYTES=4096

fail() {
    echo "FAIL: $*"
    exit 1
}

cleanup() {
    rm -f "${UPLOAD_FILE}" "${UPLOAD_FILE}.download"
}
trap cleanup EXIT

echo "=== post-release attachment smoke ==="
echo "base=${BASE}"
echo

# [1] healthz
echo "--- [1] healthz ---"
curl -sS --max-time 5 "${BASE}/healthz" >/dev/null 2>&1 || fail "healthz 不可达（节点未启动/端口不对）"
echo "ok"
echo

# [2] login（TOKEN 环境变量优先：prod 无 mock 登录通道）
if [[ -n "${TOKEN:-}" ]]; then
    echo "--- [2] login：使用环境变量 TOKEN ---"
else
    echo "--- [2] login (wechat-mini mock) ---"
    LOGIN_RESP=$(curl -sS -X POST "${BASE}/api/v1/auth/wechat-mini/login" \
        -H 'Content-Type: application/json' \
        -d '{"code":"smoke-post-release"}') || fail "login curl 失败"
    TOKEN=$(printf '%s' "${LOGIN_RESP}" | grep -o '"token":"[^"]*"' | head -1 | cut -d'"' -f4)
    [[ -n "${TOKEN}" ]] || fail "login 无 token（本地需 9817 mock jscode2session；prod 用 TOKEN=<jwt>）"
fi
echo "token: ${TOKEN:0:24}..."
echo

# [3] direct register 探针：direct-v1 会话（空 registry → 503 upload_root_unavailable）
echo "--- [3] direct register（prod 频道上传路径） ---"
REG_RESP=$(curl -sS -X POST "${BASE}/api/v1/attachment/direct/register" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H 'Content-Type: application/json' \
    -d '{"scope":"private","filename":"smoke.png","client_upload_id":"00000000-0000-0000-0000-00000000dead"}') || fail "direct register curl 失败"
echo "${REG_RESP}" | head -c 300
echo
REG_CODE=$(printf '%s' "${REG_RESP}" | grep -o '"code":[0-9]*' | head -1 | cut -d: -f2)
[[ "${REG_CODE}" == "0" ]] || fail "direct register 业务失败 code=${REG_CODE}（503=registry 空根：检查 IMBOY_ATTACHMENT_REGISTRY / 启动日志）"
echo

# [4] presign private（私有根探针）
echo "--- [4] presign scope=private ---"
PRE_PRIV=$(curl -sS "${BASE}/api/v1/attachment/presign?filename=smoke.png&mime_type=${MIME}&scope=private&method=multipart" \
    -H "Authorization: Bearer ${TOKEN}") || fail "presign(private) curl 失败"
UP_URL=$(printf '%s' "${PRE_PRIV}" | grep -o '"upload_url":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
OBJ_KEY=$(printf '%s' "${PRE_PRIV}" | grep -o '"object_key":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
[[ -n "${UP_URL}" && -n "${OBJ_KEY}" ]] || {
    echo "${PRE_PRIV}" | head -c 300
    echo
    fail "presign(private) 缺 upload_url/object_key（503=registry 私有根缺失）"
}
echo "object_key: ${OBJ_KEY}"
echo

# [5] presign public（公开根探针；仅探签发，不真传——公桶对象清理成本高）
echo "--- [5] presign scope=public ---"
PRE_PUB=$(curl -sS "${BASE}/api/v1/attachment/presign?filename=smoke-pub.png&mime_type=${MIME}&scope=public&method=multipart" \
    -H "Authorization: Bearer ${TOKEN}") || fail "presign(public) curl 失败"
UP_URL_PUB=$(printf '%s' "${PRE_PUB}" | grep -o '"upload_url":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
[[ -n "${UP_URL_PUB}" ]] || {
    echo "${PRE_PUB}" | head -c 300
    echo
    fail "presign(public) 缺 upload_url（registry 公开根缺失 / IMBOY_GARAGE_PUBLIC_BUCKET 未配）"
}
echo "ok"
echo

# [6] upload + confirm（真实上传私有探针文件并落库）
echo "--- [6] multipart upload + confirm ---"
dd if=/dev/urandom of="${UPLOAD_FILE}" bs=${SIZE_BYTES} count=1 2>/dev/null
SRC_MD5=$(md5 -q "${UPLOAD_FILE}" 2>/dev/null || md5sum "${UPLOAD_FILE}" | cut -d' ' -f1)
UP_RESP=$(curl -sS -X POST "${UP_URL}" \
    -H "Authorization: Bearer ${TOKEN}" \
    -F "file=@${UPLOAD_FILE};type=${MIME}") || fail "upload curl 失败"
CONFIRM_RESP=$(curl -sS -X POST "${BASE}/api/v1/attachment/confirm" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H 'Content-Type: application/json' \
    -d "{\"object_key\":\"${OBJ_KEY}\",\"mime_type\":\"${MIME}\",\"size\":${SIZE_BYTES},\"file_hash256\":\"\"}") || fail "confirm curl 失败"
C_CODE=$(printf '%s' "${CONFIRM_RESP}" | grep -o '"code":[0-9]*' | head -1 | cut -d: -f2)
[[ "${C_CODE}" == "0" ]] || {
    echo "${CONFIRM_RESP}" | head -c 300
    echo
    fail "confirm 业务失败 code=${C_CODE}（Garage 对象 0 字节/HEAD 失败）"
}
echo "src md5=${SRC_MD5}"
echo

# [7] view_url + GET 读回比对（authorize → presign → Garage 读取全链）
echo "--- [7] view_url + GET 读回 ---"
VIEW_RESP=$(curl -sS "${BASE}/api/v1/attachment/view_url?object_key=${OBJ_KEY}" \
    -H "Authorization: Bearer ${TOKEN}") || fail "view_url curl 失败"
VIEW_URL=$(printf '%s' "${VIEW_RESP}" | grep -o '"url":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
[[ -n "${VIEW_URL}" ]] || {
    echo "${VIEW_RESP}" | head -c 300
    echo
    fail "view_url 未返回地址（『无权访问该附件』=scope/scope_ref 不符；500=存储配置故障）"
}
curl -sS --max-time 30 -o "${UPLOAD_FILE}.download" "${VIEW_URL}" || fail "GET view_url 失败"
GOT_MD5=$(md5 -q "${UPLOAD_FILE}.download" 2>/dev/null || md5sum "${UPLOAD_FILE}.download" | cut -d' ' -f1)
[[ "${GOT_MD5}" == "${SRC_MD5}" ]] || fail "读回字节不一致（src=${SRC_MD5} got=${GOT_MD5}）"
echo "读回 md5 一致：${GOT_MD5}"
echo

echo "=== PASS：上传（direct/multipart/双 scope）与读取链路全通 ==="
