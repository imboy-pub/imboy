#!/usr/bin/env bash
set -Eeuo pipefail

BASE_URL="${IMBOY_FEATURE_BASE_URL:-}"
PUBLIC_PATH="${IMBOY_FEATURE_PUBLIC_PATH:-/api/v1/app/features}"
ADMIN_PATH="${IMBOY_FEATURE_ADMIN_PATH:-/api/adm/admin/config/features}"
TIMEOUT="${IMBOY_FEATURE_TIMEOUT:-15}"
INSECURE=0
SHOW_BODY=0
EXPECTS=()
ADMIN_HEADERS=()
FORBIDDEN_HEADERS=()

usage() {
  cat <<'USAGE'
用法:
  bash ./script/run_feature_flag_smoke.sh \
    --base-url https://<base_url> \
    --admin-header 'authorization: Bearer <admin_token>' \
    --forbidden-header 'authorization: Bearer <limited_token>' \
    --expect core=true \
    --expect moment=false

作用:
  1. 校验公共功能矩阵接口是否可用
  2. 校验后台功能矩阵接口是否可用（可选）
  3. 对比公共端 / 后台端 payload 是否一致（可选）
  4. 校验受限后台账号是否被正确拒绝（可选）
  5. 校验功能开关结果是否符合预期（可重复 --expect）

参数:
  --base-url <url>             必填；也可通过 IMBOY_FEATURE_BASE_URL 提供
  --public-path <path>         可选；也可通过 IMBOY_FEATURE_PUBLIC_PATH 覆盖
  --admin-path <path>          可选；也可通过 IMBOY_FEATURE_ADMIN_PATH 覆盖
  --admin-header <header>      可选，后台高权限请求头，可重复传入
  --forbidden-header <header>  可选，后台低权限请求头，可重复传入
  --expect <key=value>         可选，例如 moment=false，可重复传入
  --timeout <seconds>          可选；也可通过 IMBOY_FEATURE_TIMEOUT 覆盖，默认 15
  -k, --insecure               可选，跳过 TLS 证书校验
  --show-body                  可选，打印接口原始响应
  -h, --help                   查看帮助

示例:
  bash ./script/run_feature_flag_smoke.sh \
    --base-url https://<base_url> \
    --admin-header 'authorization: Bearer <admin_token>' \
    --forbidden-header 'authorization: Bearer limited-token' \
    --expect core=true \
    --expect channel=true \
    --expect moment=false \
    --expect group_task=false
USAGE
}

log()  { echo "[INFO] $*"; }
ok()   { echo "[ OK ] $*"; }
warn() { echo "[WARN] $*"; }
fail() { echo "[FAIL] $*"; exit 1; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --base-url)
      BASE_URL="${2:-}"
      shift 2
      ;;
    --public-path)
      PUBLIC_PATH="${2:-}"
      shift 2
      ;;
    --admin-path)
      ADMIN_PATH="${2:-}"
      shift 2
      ;;
    --admin-header)
      ADMIN_HEADERS+=("${2:-}")
      shift 2
      ;;
    --forbidden-header)
      FORBIDDEN_HEADERS+=("${2:-}")
      shift 2
      ;;
    --expect)
      EXPECTS+=("${2:-}")
      shift 2
      ;;
    --timeout)
      TIMEOUT="${2:-}"
      shift 2
      ;;
    -k|--insecure)
      INSECURE=1
      shift
      ;;
    --show-body)
      SHOW_BODY=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      fail "未知参数: $1"
      ;;
  esac
done

[[ -n "$BASE_URL" ]] || fail "缺少 --base-url，且 IMBOY_FEATURE_BASE_URL 未设置"
[[ "$TIMEOUT" =~ ^[0-9]+$ ]] || fail "--timeout 必须是正整数"

BASE_URL="${BASE_URL%/}"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

# bash 3.2 兼容（macOS 系统 bash 无 nameref）：结果写入全局 CURL_ARGS
# bash 3.2 compatible (macOS system bash lacks namerefs): writes global CURL_ARGS
build_curl_args() {
  CURL_ARGS=(
    -sS
    --connect-timeout "$TIMEOUT"
    --max-time "$TIMEOUT"
  )
  if [[ "$INSECURE" -eq 1 ]]; then
    CURL_ARGS+=(-k)
  fi
  for header in "$@"; do
    CURL_ARGS+=(-H "$header")
  done
}

request_endpoint() {
  local label="$1"
  local url="$2"
  local header_name="$3"
  local body_file="$TMP_DIR/${label}.json"
  local status_file="$TMP_DIR/${label}.status"
  # eval 间接展开数组名；${arr[@]+...} 防 set -u 空数组报错（bash 3.2）
  local headers_copy=()
  eval "headers_copy=(\${${header_name}[@]+\"\${${header_name}[@]}\"})"

  build_curl_args ${headers_copy[@]+"${headers_copy[@]}"}
  log "[$label] GET $url"

  local http_code
  http_code="$(curl "${CURL_ARGS[@]}" -o "$body_file" -w '%{http_code}' "$url")" || {
    fail "[$label] 请求失败: $url"
  }
  echo "$http_code" > "$status_file"
  ok "[$label] HTTP $http_code"

  if [[ "$SHOW_BODY" -eq 1 ]]; then
    echo "----- [$label] body -----"
    cat "$body_file"
    echo
  fi
}

validate_success_response() {
  local label="$1"
  local body_file="$TMP_DIR/${label}.json"
  local status_file="$TMP_DIR/${label}.status"

  python3 - "$label" "$body_file" "$status_file" <<'PY'
import json
import sys

label, body_path, status_path = sys.argv[1:4]
status = int(open(status_path, 'r', encoding='utf-8').read().strip())
if status < 200 or status >= 300:
    raise SystemExit(f'[{label}] unexpected http status: {status}')

with open(body_path, 'r', encoding='utf-8') as fh:
    data = json.load(fh)

if not isinstance(data, dict):
    raise SystemExit(f'[{label}] response is not a json object')
if data.get('code') != 0:
    raise SystemExit(f'[{label}] business code != 0: {data.get("code")} / {data.get("msg")}')
payload = data.get('payload')
if not isinstance(payload, dict):
    raise SystemExit(f'[{label}] payload is not an object')
summary = ', '.join(f'{k}={str(v).lower()}' for k, v in sorted(payload.items()))
print(f'[{label}] payload => {summary}')
PY
}

validate_forbidden_response() {
  local label="$1"
  local body_file="$TMP_DIR/${label}.json"
  local status_file="$TMP_DIR/${label}.status"

  python3 - "$label" "$body_file" "$status_file" <<'PY'
import json
import sys

label, body_path, status_path = sys.argv[1:4]
status = int(open(status_path, 'r', encoding='utf-8').read().strip())
raw = open(body_path, 'r', encoding='utf-8').read().strip()

if status == 403:
    print(f'[{label}] forbidden check passed by HTTP 403')
    raise SystemExit(0)

try:
    data = json.loads(raw) if raw else {}
except json.JSONDecodeError:
    raise SystemExit(f'[{label}] expected forbidden-like response, got http {status} and non-json body')

if isinstance(data, dict) and data.get('code') not in (0, None):
    print(f'[{label}] forbidden check passed by business code {data.get("code")} / {data.get("msg")}')
    raise SystemExit(0)

raise SystemExit(f'[{label}] expected forbidden response, got http {status}, code={data.get("code") if isinstance(data, dict) else None}')
PY
}

compare_payloads() {
  local left_label="$1"
  local right_label="$2"
  python3 - "$TMP_DIR/${left_label}.json" "$TMP_DIR/${right_label}.json" "$left_label" "$right_label" <<'PY'
import json
import sys

left_path, right_path, left_label, right_label = sys.argv[1:5]
left = json.load(open(left_path, 'r', encoding='utf-8')).get('payload')
right = json.load(open(right_path, 'r', encoding='utf-8')).get('payload')
if left != right:
    raise SystemExit(f'payload mismatch between {left_label} and {right_label}')
print(f'[{left_label}/{right_label}] payload matched')
PY
}

check_expectations() {
  local body_file="$TMP_DIR/public.json"
  if [[ ${#EXPECTS[@]} -eq 0 ]]; then
    return 0
  fi

  python3 - "$body_file" "${EXPECTS[@]}" <<'PY'
import json
import sys

body_path = sys.argv[1]
expects = sys.argv[2:]
payload = json.load(open(body_path, 'r', encoding='utf-8')).get('payload')
if not isinstance(payload, dict):
    raise SystemExit('public payload is not an object')

def to_bool(raw: str):
    normalized = raw.strip().lower()
    if normalized in ('1', 'true', 'yes', 'on'):
        return True
    if normalized in ('0', 'false', 'no', 'off'):
        return False
    raise ValueError(raw)

for item in expects:
    if '=' not in item:
        raise SystemExit(f'invalid expectation: {item}')
    key, raw_value = item.split('=', 1)
    key = key.strip()
    expected = to_bool(raw_value)
    actual = payload.get(key)
    if actual is None:
        raise SystemExit(f'missing feature key: {key}')
    if actual is not expected:
        raise SystemExit(f'feature mismatch: {key} expected {expected} got {actual}')
    print(f'[expect] {key}={str(actual).lower()}')
PY
}

request_endpoint public "$BASE_URL$PUBLIC_PATH" ADMIN_HEADERS
validate_success_response public
check_expectations

if [[ ${#ADMIN_HEADERS[@]} -gt 0 ]]; then
  request_endpoint admin "$BASE_URL$ADMIN_PATH" ADMIN_HEADERS
  validate_success_response admin
  compare_payloads public admin
else
  warn "未提供 --admin-header，跳过后台高权限对比"
fi

if [[ ${#FORBIDDEN_HEADERS[@]} -gt 0 ]]; then
  request_endpoint forbidden "$BASE_URL$ADMIN_PATH" FORBIDDEN_HEADERS
  validate_forbidden_response forbidden
else
  warn "未提供 --forbidden-header，跳过后台低权限校验"
fi

ok "feature flag smoke 检查通过"
