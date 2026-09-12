#!/usr/bin/env bash
# multipart 直传端到端 smoke：POST /api/v1/attachment/upload 全链自证。
# 链路：wechat-mini mock 登录 → presign(&method=multipart) → multipart POST
#       → confirm 落库 → psql 核查（可选）。
# Usage:  ./multipart_upload_smoke.sh [BASE_URL]   # 默认 http://127.0.0.1:9700
# 前置：节点已启动；moya 联调 mock jscode2session 在 127.0.0.1:9817（登录用）。
# Exit 0 = PASS, non-zero = FAIL。

set -u
set -o pipefail

BASE="${1:-http://127.0.0.1:9700}"
TMPDIR_LOC="${TMPDIR:-/tmp}"
UPLOAD_FILE="${TMPDIR_LOC}/multipart_smoke_$$.bin"
SIZE_BYTES=$((3 * 1024 * 1024))
MIME="image/jpeg"

# Local PG (from config/sys.local.config)，SKIP_PSQL=1 跳过核查段
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

echo "=== multipart upload smoke ==="
echo "base=${BASE} size=${SIZE_BYTES} mime=${MIME}"
echo

cleanup() {
    rm -f "${UPLOAD_FILE}" "${TMPDIR_LOC}/multipart_smoke_$$.download"
}
trap cleanup EXIT

# 1) 登录（mock jscode2session 兑换任意 code）
echo "--- [1] login (wechat-mini mock) ---"
LOGIN_RESP=$(curl -sS -X POST "${BASE}/api/v1/auth/wechat-mini/login" \
    -H 'Content-Type: application/json' \
    -d '{"code":"smoke-multipart-code"}')
LOGIN_RC=$?
echo "${LOGIN_RESP}" | head -c 300
echo
if [[ ${LOGIN_RC} -ne 0 ]]; then
    echo "FAIL: login curl rc=${LOGIN_RC}"
    exit 1
fi
TOKEN=$(printf '%s' "${LOGIN_RESP}" | grep -o '"token":"[^"]*"' | head -1 | cut -d'"' -f4)
if [[ -z "${TOKEN}" ]]; then
    echo "FAIL: no token in login response（检查 9817 mock jscode2session 是否在跑）"
    exit 1
fi
echo "token: ${TOKEN:0:24}..."
echo

# 2) presign（method=multipart 签发 upload_url）
echo "--- [2] presign &method=multipart ---"
PRESIGN_RESP=$(curl -sS "${BASE}/api/v1/attachment/presign?filename=smoke.jpg&mime_type=${MIME}&scope=private&method=multipart" \
    -H "Authorization: Bearer ${TOKEN}")
PRESIGN_RC=$?
echo "${PRESIGN_RESP}" | head -c 400
echo
if [[ ${PRESIGN_RC} -ne 0 ]]; then
    echo "FAIL: presign curl rc=${PRESIGN_RC}"
    exit 1
fi
# jsone 响应把 / 转义为 \/，提取后需反转义（否则 curl 报 URL rejected）
UPLOAD_URL=$(printf '%s' "${PRESIGN_RESP}" | grep -o '"upload_url":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
OBJECT_KEY=$(printf '%s' "${PRESIGN_RESP}" | grep -o '"object_key":"[^"]*"' | head -1 | cut -d'"' -f4 | sed 's#\\/#/#g')
if [[ -z "${UPLOAD_URL}" || -z "${OBJECT_KEY}" ]]; then
    echo "FAIL: presign payload 缺 upload_url/object_key"
    exit 1
fi
echo "upload_url: ${UPLOAD_URL}"
echo "object_key: ${OBJECT_KEY}"
echo

# 3) 造测试文件并 multipart POST（服务端流式收 → Garage）
echo "--- [3] multipart upload ---"
dd if=/dev/urandom of="${UPLOAD_FILE}" bs=1048576 count=3 2>/dev/null
SRC_MD5=$(md5 -q "${UPLOAD_FILE}" 2>/dev/null || md5sum "${UPLOAD_FILE}" | cut -d' ' -f1)
echo "src md5=${SRC_MD5}"
UPLOAD_RESP=$(curl -sS -X POST "${UPLOAD_URL}" \
    -H "Authorization: Bearer ${TOKEN}" \
    -F "file=@${UPLOAD_FILE};type=${MIME}")
UPLOAD_RC=$?
echo "${UPLOAD_RESP}" | head -c 300
echo
if [[ ${UPLOAD_RC} -ne 0 ]]; then
    echo "FAIL: upload curl rc=${UPLOAD_RC}（exit 52/空响应多为节点在跑旧 beam，重启后复测）"
    exit 1
fi
echo

# 4) confirm（唯一落库真源；服务端 HEAD 核实真实 size/mime）
echo "--- [4] confirm ---"
CONFIRM_RESP=$(curl -sS -X POST "${BASE}/api/v1/attachment/confirm" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H 'Content-Type: application/json' \
    -d "{\"object_key\":\"${OBJECT_KEY}\",\"mime_type\":\"${MIME}\",\"size\":${SIZE_BYTES},\"file_hash256\":\"\"}")
CONFIRM_RC=$?
echo "${CONFIRM_RESP}" | head -c 400
echo
if [[ ${CONFIRM_RC} -ne 0 ]]; then
    echo "FAIL: confirm curl rc=${CONFIRM_RC}"
    exit 1
fi
CODE=$(printf '%s' "${CONFIRM_RESP}" | grep -o '"code":[0-9]*' | head -1 | cut -d: -f2)
if [[ "${CODE}" != "0" ]]; then
    echo "FAIL: confirm 业务失败 code=${CODE}（code=400 且消息含『落库失败』多为 Garage 对象 0 字节/HEAD 416）"
    exit 1
fi
ATTACHMENT_ID=$(printf '%s' "${CONFIRM_RESP}" | grep -o '"attachment_id":"[^"]*"' | head -1 | cut -d'"' -f4)
echo "attachment_id: ${ATTACHMENT_ID:-（未返回，字段名以响应为准）}"
echo

# 5) psql 核查落库行（只读）
if [[ "${SKIP_PSQL:-0}" != "1" ]]; then
    echo "--- [5] psql 核查 attachment 行 ---"
    PSQL_OUT=$(psql -h "${PGHOST}" -p "${PGPORT}" -U "${PGUSER}" -d "${PGDATABASE}" -tAc \
        "SELECT path, size, mime_type FROM attachment WHERE path = '${OBJECT_KEY}' LIMIT 1" 2>&1)
    echo "${PSQL_OUT}"
    ROW_COUNT=$(printf '%s' "${PSQL_OUT}" | grep -c "${OBJECT_KEY}")
    if [[ ${ROW_COUNT} -lt 1 ]]; then
        echo "FAIL: attachment 表未查到落库行"
        exit 1
    fi
    echo
fi

echo "=== multipart upload smoke PASS ==="
exit 0
