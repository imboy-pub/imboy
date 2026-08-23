#!/usr/bin/env bash
# 8 步应用层冒烟链 / 8-step application smoke chain
#
# Golden Gates 计划 §4.3（P3-C4）：Golden Install 与 CI smoke 共用的应用层冒烟链，
# 顺序钉死，不可调整：
#
#   Health → Register → Login → Admin Login → C2C → WebSocket
#         → Attachment Upload → Attachment Download
#
# 每步输出 PASS/FAIL + 耗时；任一步失败即 fail-fast（exit 1）并给出排查提示。
# 全绿 exit 0。
#
# 用法 / Usage（从仓库根目录或任意目录）:
#   bash scripts/smoke_8step.sh
#   make smoke-8step
#
# 环境变量 / Environment overrides（均为可选项，默认指向本机 local 栈）:
#   BASE_URL                    后端 HTTP 基址，默认 http://127.0.0.1:9800
#   WS_URL                      WebSocket 基址，默认 ws://127.0.0.1:9800/api/v1/ws
#                               （路由以 src/imboy_router.erl 的 /api/v1/ws 为准；
#                                旧脚本 c2c_ws_smoke.py 的 /api/ws 默认值已过时）
#   CURL_TIMEOUT                单请求 curl 超时秒数，默认 10
#   SMOKE_MASTER_CODE           注册验证码（local/dev/test 环境万能码，
#                               config/sys.local.config verification_master_code）
#   SMOKE_ADMIN_ACCOUNT         已有超管账号（adm setup 已初始化的环境必填，见步骤 4）
#   SMOKE_ADMIN_PASSWORD        已有超管明文密码（与上成对）
#   SMOKE_ADMIN_PHONE           首次初始化时 imboy_ctl adm create 用手机号
#   SMOKE_ADMIN_CREATE_PASSWORD 首次初始化时创建的超管明文密码（8-64 位含字母数字）
#   SMOKE_C2C_FROM / SMOKE_C2C_TO  C2C 收发双方 uid，默认 1000000051 → 1000000056
#   PGHOST/PGPORT/PGUSER/PGDATABASE/PGPASSWORD  收端可见断言用的本地 PG 连接
#   IMBOY_CTL_NODE / IMBOY_CTL_COOKIE  imboy_ctl RPC 连接（release 节点需显式指定）
#
# 依赖: curl jq openssl python3 psql（C2C/WS 断言）websockets（WS 步骤）
#
# 端点核实来源（禁止臆造，均已在代码中核实）:
#   /healthz                    src/api/healthz_handler.erl + imboy_router.erl:31
#   /api/v1/passport/signup     src/api/passport_handler.erl signup/4（code 校验走
#                               verification_code_ds 万能码，非生产环境生效）
#   /api/v1/passport/login      src/api/passport_handler.erl login/1（rsa_encrypt!=1
#                               时密码为明文，elib_cipher:safe_rsa_decrypt/2）
#   /api/adm/passport/meta      src/adm/adm_passport_handler.erl meta（csrf+RSA 公钥）
#   /api/adm/passport/do_login  同上 login POST（pwd=RSA-OAEP-SHA256(md5(明文))，
#                               captcha 在 local/dev/test 固定 1234 放行）
#   C2C 发送                    无 REST 端点，走 imboy_ctl msg send（RPC），
#                               收端可见以 msg_store 落库为准（同 c2c_smoke.sh）
#   WS                          scripts/smoke/c2c_ws_smoke.py（Bob 收帧断言）
#   /api/v1/attachment/*        src/api/attach_handler.erl presign/confirm/view_url
set -u
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CTL="${SCRIPT_DIR}/imboy_ctl"
WS_PY="${SCRIPT_DIR}/smoke/c2c_ws_smoke.py"
# imboy_ctl 是 escript（#!/usr/bin/env escript），不能以 bash 解释执行；
# 与 c2c_smoke.sh 一致：确保可执行权限后经 shebang 直接调用。
[ -x "$CTL" ] || chmod +x "$CTL" || true

BASE_URL="${BASE_URL:-http://127.0.0.1:9800}"
WS_URL="${WS_URL:-ws://127.0.0.1:9800/api/v1/ws}"
CURL_TIMEOUT="${CURL_TIMEOUT:-10}"
SMOKE_MASTER_CODE="${SMOKE_MASTER_CODE:-abc12345}"
SMOKE_C2C_FROM="${SMOKE_C2C_FROM:-1000000051}"
SMOKE_C2C_TO="${SMOKE_C2C_TO:-1000000056}"

# 本地 PG（同 scripts/smoke/c2c_smoke.sh，源自 config/sys.local.config）
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

BASE_URL="${BASE_URL%/}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; NC='\033[0m'

# ---------------------------------------------------------------- helpers --

now_ms() { python3 -c 'import time; print(int(time.time()*1000))'; }

STEP_NAME=""
STEP_T0=0

step_begin() {
    STEP_NAME="$1"
    STEP_T0="$(now_ms)"
    printf '\n[%s] %s\n' "$(date '+%H:%M:%S')" "$STEP_NAME"
}

step_end() { # step_end <ok:0|1> <detail...>
    local ok="$1"; shift
    local ms=$(( $(now_ms) - STEP_T0 ))
    if [ "$ok" -eq 0 ]; then
        printf "${GREEN}PASS${NC} %s (${ms}ms)\n" "$*"
    else
        printf "${RED}FAIL${NC} %s (${ms}ms)\n" "$*"
    fi
}

die() { # die <hint...> ：FAIL 后 fail-fast 退出
    printf "${RED}==== 8-step smoke FAILED at: %s ====${NC}\n" "$STEP_NAME"
    printf "排查提示 / Next steps:\n%s\n" "$1"
    exit 1
}

# http <label> <method> <url> [curl 额外参数...] → 输出 HTTP 码，body 落 $TMP/last_body
http() {
    local method="$1"; local url="$2"; shift 2
    local code
    code="$(curl -sS -m "$CURL_TIMEOUT" -o "$TMP/last_body" -w '%{http_code}' \
        -X "$method" "$@" "$url" 2>"$TMP/last_err")" || {
        printf '000'
        echo "curl: $(cat "$TMP/last_err")" >&2
        return 0
    }
    printf '%s' "$code"
}

body_snippet() { head -c 300 "$TMP/last_body"; echo; }

json_code() { jq -r '.code // empty' "$TMP/last_body" 2>/dev/null; }

md5_hex() { printf '%s' "$1" | openssl md5 -r | awk '{print $1}'; }

# -------------------------------------------------- 前置依赖检查（非步骤）--

for bin in curl jq openssl python3 psql; do
    command -v "$bin" >/dev/null 2>&1 || {
        echo "ERROR: 缺少依赖 $bin"; exit 1;
    }
done

echo "==== 8-step application smoke chain ===="
echo "base_url=${BASE_URL}  ws_url=${WS_URL}  ctl=${CTL}"
echo "c2c: ${SMOKE_C2C_FROM} -> ${SMOKE_C2C_TO}  pg=${PGHOST}:${PGPORT}/${PGDATABASE}"

# ------------------------------------------------------------- Step 1/8 ---

step_begin "Step 1/8 Health (GET /healthz)"
CODE="$(http GET "$BASE_URL/healthz")"
STATUS="$(jq -r '.status // empty' "$TMP/last_body" 2>/dev/null)"
if [ "$CODE" = "200" ] && [ "$STATUS" = "ok" ]; then
    step_end 0 "HTTP $CODE status=ok $(jq -r '"db=" + (.db // "?") + " version=" + (.version // "?")' "$TMP/last_body" 2>/dev/null)"
else
    step_end 1 "HTTP $CODE status=${STATUS:-none} body=$(body_snippet)"
    die "后端未起或 /healthz 异常。确认: IMBOYENV=local make run（或 release 节点），curl $BASE_URL/healthz 手工复现。"
fi

# ------------------------------------------------------------- Step 2/8 ---

step_begin "Step 2/8 Register (POST /api/v1/passport/signup)"
SMOKE_EMAIL="smoke8_$(now_ms)@smoke.local"
SMOKE_PWD="Smoke8-$(openssl rand -hex 6)"
CODE="$(http POST "$BASE_URL/api/v1/passport/signup" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    --data-urlencode "type=email" \
    --data-urlencode "account=$SMOKE_EMAIL" \
    --data-urlencode "pwd=$SMOKE_PWD" \
    --data-urlencode "code=$SMOKE_MASTER_CODE" \
    --data-urlencode "rsa_encrypt=0" \
    --data-urlencode "nickname=smoke8")"
BIZ="$(json_code)"
if [ "$CODE" = "200" ] && [ "$BIZ" = "0" ]; then
    step_end 0 "HTTP $CODE code=0 account=$SMOKE_EMAIL"
elif [ "$BIZ" = "402" ]; then
    step_end 1 "HTTP $CODE code=402 license 用户配额已满 body=$(body_snippet)"
    die "注册被 License 配额闸门拒绝（imboy_license:check_user_quota，非链路故障）。
清理测试用户、升级 License，或在配额未满的环境（cleanroom）运行。"
else
    step_end 1 "HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    die "注册失败。检查: (1) 目标环境 env ∈ local/dev/test 才有万能验证码
（verification_master_code，默认 abc12345，可用 SMOKE_MASTER_CODE 覆盖）；
(2) 参数 type/account/pwd/code/nickname 是否被网关/中间件改写。"
fi

# ------------------------------------------------------------- Step 3/8 ---

step_begin "Step 3/8 Login (POST /api/v1/passport/login)"
CODE="$(http POST "$BASE_URL/api/v1/passport/login" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    --data-urlencode "type=email" \
    --data-urlencode "account=$SMOKE_EMAIL" \
    --data-urlencode "pwd=$SMOKE_PWD" \
    --data-urlencode "rsa_encrypt=0")"
BIZ="$(json_code)"
TOKEN="$(jq -r '.payload.token // empty' "$TMP/last_body" 2>/dev/null)"
LOGIN_UID="$(jq -r '.payload.uid // empty' "$TMP/last_body" 2>/dev/null)"
if [ "$CODE" = "200" ] && [ "$BIZ" = "0" ] && [ -n "$TOKEN" ]; then
    step_end 0 "HTTP $CODE code=0 uid=${LOGIN_UID} token_len=${#TOKEN}"
else
    step_end 1 "HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    die "登录失败（注册成功但登录被拒）。检查 login_security_logic 锁定计数、
账号状态，或后端日志中 do_login_verify 的拒绝原因。"
fi

# ------------------------------------------------------------- Step 4/8 ---

# 管理后台登录：RSA-OAEP-SHA256(md5(明文)) + csrf + captcha（local 固定 1234）。
# 返回 0 且打印 admin uid；非 0 失败（细节已由调用方打印）。
admin_do_login() { # <account> <plain_password>
    local account="$1" plain="$2"
    local meta csrf pubkey md5 cipher
    if ! meta="$(curl -sS -m "$CURL_TIMEOUT" "$BASE_URL/api/adm/passport/meta" 2>/dev/null)"; then
        echo "admin meta 请求失败"; return 1
    fi
    csrf="$(printf '%s' "$meta" | jq -r '.payload.csrf_token // empty')"
    pubkey="$(printf '%s' "$meta" | jq -r '.payload.public_key // empty')"
    if [ -z "$csrf" ] || [ -z "$pubkey" ] || [ "$pubkey" = "-----BEGIN PUBLIC KEY----------END PUBLIC KEY-----" ]; then
        echo "admin meta 缺 csrf_token/public_key: $(printf '%s' "$meta" | head -c 200)"
        return 1
    fi
    # 单行 PEM 重构为 64 列标准格式（后端 re:replace 去掉了换行）
    printf '%s' "$pubkey" | python3 -c '
import sys
k = sys.stdin.read().strip().replace("\n", "")
k = k.replace("-----BEGIN PUBLIC KEY-----", "").replace("-----END PUBLIC KEY-----", "")
lines = [k[i:i+64] for i in range(0, len(k), 64)]
print("-----BEGIN PUBLIC KEY-----")
print("\n".join(lines))
print("-----END PUBLIC KEY-----")' > "$TMP/adm_pub.pem"
    md5="$(md5_hex "$plain")"
    if ! cipher="$(printf '%s' "$md5" | openssl pkeyutl -encrypt -pubin \
            -inkey "$TMP/adm_pub.pem" \
            -pkeyopt rsa_padding_mode:oaep \
            -pkeyopt rsa_oaep_md:sha256 \
            -pkeyopt rsa_mgf1_md:sha256 2>/dev/null | base64 | tr -d '\n')"; then
        echo "openssl RSA-OAEP-SHA256 加密失败"; return 1
    fi
    [ -n "$cipher" ] || { echo "RSA 加密结果为空"; return 1; }
    CODE="$(http POST "$BASE_URL/api/adm/passport/do_login" \
        -H "Content-Type: application/x-www-form-urlencoded" \
        --data-urlencode "account=$account" \
        --data-urlencode "pwd=$cipher" \
        --data-urlencode "captcha=1234" \
        --data-urlencode "csrf_token=$csrf")"
    BIZ="$(json_code)"
    if [ "$CODE" = "200" ] && [ "$BIZ" = "0" ] \
        && [ -n "$(jq -r '.payload.id // empty' "$TMP/last_body" 2>/dev/null)" ]; then
        return 0
    fi
    echo "do_login HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    return 1
}

step_begin "Step 4/8 Admin Login (POST /api/adm/passport/do_login)"
ADM_ACCOUNT="${SMOKE_ADMIN_ACCOUNT:-}"
ADM_PASSWORD="${SMOKE_ADMIN_PASSWORD:-}"
ADM_CREATED="false"
if [ -n "$ADM_ACCOUNT" ] && [ -n "$ADM_PASSWORD" ]; then
    # 路径 A：环境变量提供已有超管凭据（setup 已初始化的环境）
    :
else
    # 路径 B：尝试 imboy_ctl adm create（cleanroom 首装路径；911=已初始化幂等拒绝）
    ADM_ACCOUNT="${SMOKE_ADMIN_PHONE:-13900000001}"
    ADM_PASSWORD="${SMOKE_ADMIN_CREATE_PASSWORD:-Smoke8-Admin-$(openssl rand -hex 4)}"
    if [ ! -f "$CTL" ]; then
        step_end 1 "imboy_ctl 不存在: $CTL"
        die "需要 SMOKE_ADMIN_ACCOUNT/SMOKE_ADMIN_PASSWORD 或可用的 imboy_ctl。"
    fi
    ADM_OUT="$("$CTL" adm create --phone "$ADM_ACCOUNT" --password "$ADM_PASSWORD" 2>&1)"
    ADM_RC=$?
    if [ "$ADM_RC" -eq 0 ] && printf '%s' "$ADM_OUT" | grep -q "CREATED=true"; then
        ADM_CREATED="true"
    elif printf '%s' "$ADM_OUT" | grep -q "911\|已完成首启初始化"; then
        # 幂等拒绝：已有超管，但密码未知 → 尝试列出账号辅助排障
        EXISTING="$(psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" \
            -At -F',' -c "SELECT account FROM adm_user WHERE status = 1 ORDER BY id LIMIT 3" \
            2>/dev/null | paste -s -d' ' -)"
        step_end 1 "adm create 911（系统已初始化，已有超管密码未知）existing=[${EXISTING:-unknown}]"
        die "adm setup 已完成，无法自动创建新超管（幂等拒绝，行为正确）。
请提供已有超管凭据重跑:
  SMOKE_ADMIN_ACCOUNT=<account> SMOKE_ADMIN_PASSWORD=<明文密码> bash scripts/smoke_8step.sh
已存在的启用超管账号: ${EXISTING:-（psql 不可用，自查 adm_user 表）}"
    else
        step_end 1 "adm create rc=$ADM_RC out=$(printf '%s' "$ADM_OUT" | head -c 200)"
        die "imboy_ctl adm create 失败（非 911）。检查: IMBOY_CTL_NODE/IMBOY_CTL_COOKIE
是否指向运行中节点、密码强度（8-64 位含字母数字）。输出: $ADM_OUT"
    fi
fi
if admin_do_login "$ADM_ACCOUNT" "$ADM_PASSWORD"; then
    step_end 0 "HTTP 200 code=0 account=$ADM_ACCOUNT created=$ADM_CREATED"
else
    step_end 1 "admin_do_login 失败（详见上方输出）"
    die "管理后台登录失败。检查:
(1) SMOKE_ADMIN_PASSWORD 是否为明文（脚本自动做 md5+RSA，勿预哈希）；
(2) 目标环境 env ∈ local/dev/test 才有 captcha=1234 固定放行
    （adm_passport_handler ?ADM_TEST_CAPTCHA），其它环境需真实验证码；
(3) 后端 login_rsa_* 密钥与 /api/adm/passport/meta 返回公钥是否一致。"
fi

# ------------------------------------------------------------- Step 5/8 ---

step_begin "Step 5/8 C2C (imboy_ctl msg send + msg_store 落库)"
if [ ! -f "$CTL" ]; then
    step_end 1 "imboy_ctl 不存在: $CTL"
    die "C2C 步骤依赖 imboy_ctl RPC。"
fi
SEND_OUT="$("$CTL" msg send "$SMOKE_C2C_FROM" "$SMOKE_C2C_TO" 2>&1)"
SEND_RC=$?
if [ "$SEND_RC" -ne 0 ]; then
    step_end 1 "msg send rc=$SEND_RC out=$(printf '%s' "$SEND_OUT" | head -c 300)"
    if printf '%s' "$SEND_OUT" | grep -q "encrypted_message_required"; then
        die "C2C 明文发送被 E2EE 策略拒绝（imboy_policy message_encryption_required，
storage_mode/e2ee_mode 处于强制加密档位；make smoke-c2c 同样会红，非本脚本回归）。
cleanroom 默认 profile（community/enterprise preset 均 storage_mode=archived）可明文发送；
本环境需调整 policy 或等 Tier-0 脚本支持 E2EE 密文。"
    fi
    die "C2C 发送失败。检查 IMBOY_CTL_NODE/IMBOY_CTL_COOKIE 连接与后端日志。
输出: $SEND_OUT"
fi
MSG_ID="$(printf '%s' "$SEND_OUT" | awk -F= '/^MSG_ID=/ {print $2}' | tr -d '[:space:]')"
[ -n "$MSG_ID" ] || { step_end 1 "无法解析 MSG_ID: $SEND_OUT"; die "escript 输出异常。"; }

# 收端可见：msg_store 落库断言（写异步，轮询 10s；同 scripts/smoke/c2c_smoke.sh）
ROW=""
SQL="SELECT chat_type, from_id, to_id FROM msg_store WHERE msg_id = '${MSG_ID}' LIMIT 1;"
for _ in $(seq 1 20); do
    ROW="$(psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" \
        -At -F '|' -c "$SQL" 2>/dev/null)" || true
    [ -n "$ROW" ] && break
    sleep 0.5
done
if [ -z "$ROW" ]; then
    step_end 1 "msg_store 10s 内未见 msg_id=$MSG_ID"
    die "消息发送返回成功但 msg_store 无落库。检查 PG 连接参数（PGHOST/PGPORT/...）
与 msg_logic 异步落库日志。"
fi
CHAT_TYPE="$(printf '%s' "$ROW" | cut -d'|' -f1)"
FROM_ID="$(printf '%s' "$ROW" | cut -d'|' -f2)"
TO_ID="$(printf '%s' "$ROW" | cut -d'|' -f3)"
if [ "$CHAT_TYPE" = "c2c" ] && [ "$FROM_ID" = "$SMOKE_C2C_FROM" ] && [ "$TO_ID" = "$SMOKE_C2C_TO" ]; then
    step_end 0 "msg_id=$MSG_ID chat_type=c2c ${FROM_ID}→${TO_ID}（收端可见）"
else
    step_end 1 "row 异常: chat_type=$CHAT_TYPE from=$FROM_ID to=$TO_ID"
    die "msg_store 行字段与发送双方不一致。"
fi

# ------------------------------------------------------------- Step 6/8 ---

step_begin "Step 6/8 WebSocket (Bob WS 收到 C2C 帧)"
[ -f "$WS_PY" ] || { step_end 1 "缺少 $WS_PY"; die "c2c_ws_smoke.py 不存在。"; }
python3 -c "import websockets" 2>/dev/null || {
    step_end 1 "python3 websockets 包缺失"
    die "安装: pip3 install websockets"
}
BOB_TOKEN="$("$CTL" user token "$SMOKE_C2C_TO" 2>/dev/null)" || BOB_TOKEN=""
[ -n "$BOB_TOKEN" ] || { step_end 1 "mint Bob token 失败 uid=$SMOKE_C2C_TO"; die "imboy_ctl user token 失败（检查 RPC 连接）。"; }
if BOB_TOKEN="$BOB_TOKEN" BOB_UID="$SMOKE_C2C_TO" FROM_UID="$SMOKE_C2C_FROM" \
   ESCRIPT_PATH="$CTL" WS_URL="$WS_URL" python3 "$WS_PY"; then
    step_end 0 "WS 收帧匹配 MSG_ID（${WS_URL}）"
else
    WS_RC=$?
    step_end 1 "c2c_ws_smoke.py rc=$WS_RC"
    die "WS round-trip 失败。检查:
(1) WS_URL（默认 ${WS_URL}，与 imboy_router.erl 一致；旧 /api/ws 会 404）；
(2) Bob token 是否有效； (3) 后端 websocket_handler 日志。"
fi

# ------------------------------------------------------------- Step 7/8 ---

step_begin "Step 7/8 Attachment Upload (presign → PUT → confirm)"
printf 'imboy-smoke8-attachment-roundtrip' > "$TMP/smoke8.txt"
AUTH=(-H "Authorization: Bearer $TOKEN")
CODE="$(http GET "$BASE_URL/api/v1/attachment/presign?filename=smoke8.txt&mime_type=text/plain" "${AUTH[@]}")"
BIZ="$(json_code)"
PUT_URL="$(jq -r '.payload.put_url // .data.put_url // empty' "$TMP/last_body" 2>/dev/null)"
OBJ_KEY="$(jq -r '.payload.object_key // .data.object_key // empty' "$TMP/last_body" 2>/dev/null)"
if [ "$CODE" != "200" ] || [ "$BIZ" != "0" ] || [ -z "$PUT_URL" ] || [ -z "$OBJ_KEY" ]; then
    step_end 1 "presign HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    die "presign 失败。检查登录 token、mime_type 白名单（elib_oss ?ALLOWED_TYPES）、
对象存储（Garage）配置。"
fi
printf '%s' "$OBJ_KEY" | grep -qE "^u[0-9]+/" || {
    step_end 1 "object_key 未绑定 uid 前缀: $OBJ_KEY"
    die "presign 返回的 object_key 异常（归属守卫）。"
}
PUT_CODE="$(curl -sS -m "$CURL_TIMEOUT" -o /dev/null -w '%{http_code}' -X PUT \
    -H "Content-Type: text/plain" --data-binary @"$TMP/smoke8.txt" "$PUT_URL" 2>/dev/null || printf '000')"
if [ "$PUT_CODE" != "200" ] && [ "$PUT_CODE" != "204" ]; then
    step_end 1 "S3 PUT 直传 HTTP ${PUT_CODE}（put_url 指向 $(printf '%s' "$PUT_URL" | cut -c1-60)...）"
    die "Garage S3 直传失败。检查对象存储可达性（presign URL 的 host:port）与
签名有效期。"
fi
MD5="$(openssl md5 -r "$TMP/smoke8.txt" | awk '{print $1}')"
SIZE="$(wc -c < "$TMP/smoke8.txt" | tr -d ' ')"
CODE="$(http POST "$BASE_URL/api/v1/attachment/confirm" "${AUTH[@]}" \
    -H "Content-Type: application/json" \
    -d "{\"object_key\":\"$OBJ_KEY\",\"md5\":\"$MD5\",\"mime_type\":\"text/plain\",\"size\":$SIZE}")"
BIZ="$(json_code)"
if [ "$CODE" = "200" ] && [ "$BIZ" = "0" ]; then
    step_end 0 "PUT $PUT_CODE + confirm code=0 key=$OBJ_KEY"
else
    step_end 1 "confirm HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    die "confirm 落库失败（HEAD 核实/大小/类型任一被拒）。看后端 attach_logic:confirm 日志。"
fi

# ------------------------------------------------------------- Step 8/8 ---

step_begin "Step 8/8 Attachment Download (view_url → GET → 逐字节比对)"
CODE="$(http GET "$BASE_URL/api/v1/attachment/view_url?object_key=$OBJ_KEY" "${AUTH[@]}")"
BIZ="$(json_code)"
VURL="$(jq -r '.payload.url // .data.url // empty' "$TMP/last_body" 2>/dev/null)"
if [ "$CODE" != "200" ] || [ "$BIZ" != "0" ] || [ -z "$VURL" ]; then
    step_end 1 "view_url HTTP $CODE code=${BIZ:-none} body=$(body_snippet)"
    die "下载 URL 签发失败（归属校验被拒或对象元数据缺失）。"
fi
DL_CODE="$(curl -sS -m "$CURL_TIMEOUT" -o "$TMP/smoke8_got.txt" -w '%{http_code}' \
    "$VURL" 2>/dev/null || printf '000')"
if [ "$DL_CODE" = "200" ] && cmp -s "$TMP/smoke8.txt" "$TMP/smoke8_got.txt"; then
    step_end 0 "GET 200 + 内容逐字节一致（${SIZE}B round-trip）"
elif [ "$DL_CODE" != "200" ]; then
    step_end 1 "presigned GET HTTP $DL_CODE"
    die "签名下载 URL 读回失败。检查 Garage 可达与签名过期策略。"
else
    step_end 1 "内容不一致: 期望 $(wc -c < "$TMP/smoke8.txt" | tr -d ' ')B, 实得 $(wc -c < "$TMP/smoke8_got.txt" | tr -d ' ')B"
    die "上传/下载内容不一致（对象被覆盖或存储损坏）。"
fi

# ------------------------------------------------------------------ 汇总 --

printf '\n%s\n' "==== 8-step smoke chain ALL GREEN (Health → Register → Login → Admin Login → C2C → WebSocket → Upload → Download) ===="
printf '测试痕迹（local 可留档）: user=%s attachment=%s admin_acct=%s(created=%s)\n' \
    "$SMOKE_EMAIL" "$OBJ_KEY" "$ADM_ACCOUNT" "$ADM_CREATED"
exit 0
