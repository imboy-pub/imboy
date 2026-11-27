#!/usr/bin/env bash
# Q08 / AC21-W14（F08 运行层证据）+ AC20 OA 替身链 — phase1_delivery 隔离节点验证。
#
# 目的（Q08 调查 §3 W14 缺口 / §2 OA 替身方案）：
#   1. 以 config/sys.p1-boot.config（product_profile=phase1_delivery，端口 9800，
#      隔离库）boot 完整 release 节点；
#   2. F08 oracle：bot_webhook_delivery_worker 在监督树内常驻（whereis 非
#      undefined）；Bot 产品入口（/api/v1/bot/*）同时 5190 关闭；
#   3. W1：INT-12 配 loopback 回调 → INT-32 入箱 → 真 worker/sender 投递，
#      Python 接收端独立复算 HMAC 验签，500→200 两次到达、间隔 ≥4.9s，
#      bot_delivery 终态 success/attempt_count=2；
#   4. W2：恒 410 → 一次尝试即 dead（7s 后仍 dead，无重试）；
#   5. W3：挂起 1.6s（发送端超时 1000ms）→ attempt error/{recv,timeout} → retry；
#   6. AC20：自签 TLS OA 替身页服务 + /api/v1/workbench/entries →
#      /api/v1/oa/sso/code → 替身 /cb 落点（code/state 记录）；移除 identity
#      mapping 后再签发 → 信封 code 403（fail-early NEG-H04）。
#
# 纪律：整个 boot→验证→teardown 持有 /tmp/phase1-node.lock（mkdir 自旋锁）；
# `make rel` 编译窗口额外持有 /tmp/phase1-eunit.lock（与并行 eunit 会话互斥，
# 二者共享 ebin）。Webhook 只打本机替身（127.0.0.1），绝不外发。
# PG 凭据从 config/sys.local.config 运行时解析，只进进程环境变量，绝不落盘。
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO"
RUN_DIR="$(mktemp -d /tmp/imboy-w14.XXXXXX)"
NODE_LOCK=/tmp/phase1-node.lock
EUNIT_LOCK=/tmp/phase1-eunit.lock
NODE=imboy@127.0.0.1
COOKIE=imboy_local_dev_only
HTTP_PORT=9800
RECV_PORT=28443
OA_PORT=28444

# 996 段合成租户（与 eunit 995 段、既有数据互不冲突；再跑幂等）
ORG=996101
OWNER=996001
HUMAN=996011
APP_HW=996201          # Webhook 应用（W1-W3）
APP_SSO=996202         # OA 替身应用
EXT_USER="q08-ext-human"

psql_q() { PGPASSWORD="$PG_PASS" psql -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" -d "$PG_DB" -v ON_ERROR_STOP=1 -tAc "$1"; }
die() { echo "FATAL: $*" >&2; exit 1; }

cleanup() {
  echo "== teardown =="
  "$REPO/_rel/imboy/bin/imboy" stop >/dev/null 2>&1 || true
  for _ in $(seq 1 20); do
    lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1 || break
    sleep 1
  done
  if lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1; then
    echo "WARN: port $HTTP_PORT still listening, forcing kill"
    pkill -f "imboy@127.0.0.1" 2>/dev/null || true
    pkill -f "heart" >/dev/null 2>&1 || true
    sleep 2
  fi
  lsof -nP -iTCP:$HTTP_PORT -sTCP:LISTEN >/dev/null 2>&1 && echo "ERROR: port $HTTP_PORT NOT released"
  [[ -n "${RECV_PID:-}" ]] && kill "$RECV_PID" 2>/dev/null || true
  [[ -n "${OA_PID:-}" ]] && kill "$OA_PID" 2>/dev/null || true
  psql_q "UPDATE enterprise_external_identity SET status='removed' WHERE organization_id=$ORG AND status='active'" >/dev/null 2>&1 || true
  psql_q "UPDATE enterprise_application_credential SET status='revoked', revoked_at=CURRENT_TIMESTAMP WHERE organization_id=$ORG AND status='active'" >/dev/null 2>&1 || true
  rmdir "$NODE_LOCK" 2>/dev/null || true
  echo "TEARDOWN_DONE"
}
trap cleanup EXIT

# ---- 节点锁（全程）----
while ! mkdir "$NODE_LOCK" 2>/dev/null; do sleep 5; done
echo "== node lock acquired: $NODE_LOCK =="

# ---- PG 凭据（从节点实际使用的 sys.p1-boot.config 运行时解析，不落盘不打印）----
# 注意：必须解析 p1-boot 而非 sys.local——后者的 pg_conf 指向另一个本地库
# （2026-10-07 实测跨库误seed事故）。块内字段顺序固定：host/username/password/
# database/port，按文件序匹配防止 .*? 跨块串位。
eval "$(python3 - <<'PY'
import re
txt = open('config/sys.p1-boot.config').read()
m = re.search(r'host\s*=>\s*"([^"]+)"\s*,\s*username\s*=>\s*"([^"]+)"\s*,\s*password\s*=>\s*"([^"]+)"\s*,\s*database\s*=>\s*"([^"]+)"\s*,\s*port\s*=>\s*(\d+)', txt)
assert m, 'pg_conf not found in config/sys.p1-boot.config'
print('PG_HOST=%s PG_USER=%s PG_DB=%s PG_PORT=%s' % (m.group(1), m.group(2), m.group(4), m.group(5)))
print('export PG_PASS=%s' % m.group(3))
PY
)"

for p in $HTTP_PORT $RECV_PORT $OA_PORT; do
  lsof -nP -iTCP:$p -sTCP:LISTEN >/dev/null 2>&1 && die "port $p busy before start"
done

# ---- 构建 release（短窗口持 eunit 锁：共享 ebin）----
# 0.3s 快速自旋：并行 eunit 会话逐模块循环持锁，释放窗口常 <1s（2026-10-07
# 实测 5s 轮询持续抢不到），快速轮询才能插入 make rel 的长窗口。
while ! mkdir "$EUNIT_LOCK" 2>/dev/null; do sleep 0.3; done
echo "== building release (IMBOYENV=p1-boot) =="
IMBOYENV=p1-boot make -s rel > "$RUN_DIR/build.log" 2>&1 || { tail -20 "$RUN_DIR/build.log"; rmdir "$EUNIT_LOCK" 2>/dev/null || true; die "make rel failed"; }
rmdir "$EUNIT_LOCK" 2>/dev/null || true
grep -q 'phase1_delivery' config/sys.runtime.config || die "sys.runtime.config 不是 p1-boot 版本"

# ---- Python 接收端替身 ----
python3 scripts/test/webhook_receiver_stub.py "$RECV_PORT" "$RUN_DIR" > "$RUN_DIR/receiver.log" 2>&1 &
RECV_PID=$!
for _ in $(seq 1 20); do curl -sf "http://127.0.0.1:$RECV_PORT/healthz" >/dev/null && break; sleep 0.5; done
curl -sf "http://127.0.0.1:$RECV_PORT/healthz" >/dev/null || die "receiver not ready"

# ---- 种子数据（996 段；业务数据 psql 直插；重复执行幂等）----
psql_q "INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES ($OWNER,'x','t996_owner',0,1,'127.0.0.1','x'),($HUMAN,'x','t996_human',0,1,'127.0.0.1','x') ON CONFLICT (id) DO NOTHING"
psql_q "INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES ($ORG,'q08-w14-org',$OWNER,'active','{}'::jsonb,'{}'::jsonb,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP) ON CONFLICT (id) DO NOTHING"
psql_q "INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES ($ORG,$OWNER,'owner','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP),($ORG,$HUMAN,'member','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP) ON CONFLICT DO NOTHING"
psql_q "INSERT INTO enterprise_application (id, organization_id, principal_user_id, application_key, name, status, allowed_redirect_uris) VALUES ($APP_HW,$ORG,$OWNER,'q08-w14-webhook','Q08 W14 webhook app','active','{}') ON CONFLICT (id) DO NOTHING"
psql_q "INSERT INTO enterprise_application (id, organization_id, principal_user_id, application_key, name, status, allowed_redirect_uris) VALUES ($APP_SSO,$ORG,NULL,'q08-w14-sso','Q08 W14 sso app','active',ARRAY['https://127.0.0.1:$OA_PORT/cb']) ON CONFLICT (id) DO NOTHING"
psql_q "INSERT INTO enterprise_external_identity (id, organization_id, application_id, external_user_id, user_id, status) VALUES (996801,$ORG,$APP_SSO,'$EXT_USER',$HUMAN,'active') ON CONFLICT (id) DO UPDATE SET status='active'"

# ---- boot phase1_delivery 节点（IMBOYENV=local：SSRF 守卫开放 loopback http）----
# daemon 模式：console 模式下 stdin EOF 会让 Eshell 立即 init:stop（2026-10-07
# 实测：app started 同秒 "Terminating erlang"），daemon 与 heart 语义匹配且无需 stdin。
echo "== booting node (phase1_delivery, port $HTTP_PORT) =="
IMBOYENV=local ERL_CRASH_DUMP="$RUN_DIR/erl_crash.dump" \
  "$REPO/_rel/imboy/bin/imboy" daemon
ln -sf "$REPO/log/debug.log" "$RUN_DIR/node-debug.log" 2>/dev/null || true
for _ in $(seq 1 120); do
  curl -s -o /dev/null "http://127.0.0.1:$HTTP_PORT/" && break
  sleep 1
done
curl -s -o /dev/null "http://127.0.0.1:$HTTP_PORT/" || { tail -40 "$RUN_DIR/node.log"; die "node did not come up"; }
echo "NODE_HTTP_UP"

rpc_eval() {
  erl -noshell -noinput -hidden -name "q08p$RANDOM@127.0.0.1" -setcookie "$COOKIE" \
    -eval "$1" -s init stop 2>/dev/null
}

# 使用生产签发接口生成加密签名密钥；凭据只保留在当前 shell 变量中。
CRED_HW_FULL=$(rpc_eval "{ok,C}=rpc:call('$NODE',enterprise_internal_ops,issue_credential,[$ORG,$APP_HW,undefined]), io:format(\"~s\",[maps:get(credential,C)])")
[[ "$CRED_HW_FULL" == ib_int_*.* ]] || die "HMAC credential issue failed"

WORKER=$(rpc_eval "io:format(\"~p\", [rpc:call('$NODE', erlang, whereis, [bot_webhook_delivery_worker])])")
echo "W14_WORKER_WHEREIS=$WORKER"
[[ "$WORKER" == *"<"* ]] || die "bot_webhook_delivery_worker 未运行（F08 oracle 失败）"
PROFILE=$(rpc_eval "io:format(\"~p\", [rpc:call('$NODE', application, get_env, [imboy, product_profile])])")
echo "NODE_PRODUCT_PROFILE=$PROFILE"
[[ "$PROFILE" == *phase1_delivery* ]] || die "profile 不是 phase1_delivery"
rpc_eval "rpc:call('$NODE', application, set_env, [imboy, enterprise_internal_rate_limits, #{internal_read => 10000, internal_write => 10000, internal_sso => 10000}]), rpc:call('$NODE', application, set_env, [imboy, bot_webhook_timeout_ms, 1000])" >/dev/null
JWT=$(rpc_eval "io:format(\"~s\", [rpc:call('$NODE', token_ds, encrypt_token, [$HUMAN])])")
[[ -n "$JWT" && "$JWT" != *error* ]] || die "JWT 生成失败"

RESP="$RUN_DIR/resp.json"
request() { # method path credential idem body; Internal uses exact-wire HMAC.
  local method="$1" path="$2" cred="$3" idem="$4" body="$5"
  local hdrs=(-H 'Content-Type: application/json')
  [[ -n "$idem" ]] && hdrs+=(-H "Idempotency-Key: $idem")
  if [[ "$path" == /api/internal/v1/* ]]; then
    local signed=()
    while IFS= read -r value; do signed+=("$value"); done < <(
      IMBOY_TEST_CREDENTIAL="$cred" python3 - "$method" "$path" "$idem" "$body" <<'PY_SIGN'
import hashlib, hmac, os, secrets, sys, time
method,path,idem,body=sys.argv[1:]
locator,secret=os.environ['IMBOY_TEST_CREDENTIAL'].split('.',1)
ts=str(int(time.time()));nonce=secrets.token_hex(16)
canonical='\n'.join(['IMBOY-INTERNAL-V1','sha256',locator,'',method,path,'','application/json',idem,hashlib.sha256(body.encode()).hexdigest(),ts,nonce])
for name,value in [('Credential',locator),('Timestamp',ts),('Nonce',nonce),('Signature','sha256='+hmac.new(secret.encode(),canonical.encode(),hashlib.sha256).hexdigest())]:
 print('X-Imboy-'+name+': '+value)
PY_SIGN
    )
    [[ ${#signed[@]} == 4 ]] || die "HMAC signing failed"
    for value in "${signed[@]}"; do hdrs+=(-H "$value"); done
  else
    hdrs+=(-H "Authorization: Bearer $cred")
  fi
  curl -sS -o "$RESP" -w '%{http_code}' -X "$method" "http://127.0.0.1:$HTTP_PORT$path" "${hdrs[@]}" --data "$body"
}
post() { request POST "$1" "$2" "${3:-}" "${4:-\{\}}"; }
code_of() { python3 -c "import json;d=json.load(open('$RESP'));e=d.get('error');print(e.get('code') if isinstance(e,dict) else d.get('code',''))" 2>/dev/null || echo parse-fail; }
jget() { python3 -c "import json;d=json.load(open('$RESP'));print(d$1)" 2>/dev/null || echo parse-fail; }

# ---- 证据 A：Bot 产品入口 5190（F08：产品关、设施活）----
s=$(post /api/v1/bot/list_mine "$JWT" "" '{}')
c=$(code_of)
echo "W14_BOT_ENTRY status=$s code=$c"
[[ "$c" == "5190" ]] || die "Bot 产品入口未按 5190 关闭（got status=$s code=$c）"
echo "W14_BOT_ENTRY_5190=PASS"

wait_delivery_status() { # delivery_id expected timeout_s
  local st=""
  for _ in $(seq 1 "$3"); do
    st=$(psql_q "SELECT status FROM bot_delivery WHERE delivery_id='$1'")
    [[ "$st" == "$2" ]] && return 0
    sleep 1
  done
  echo "last_status=$st" >&2
  return 1
}

run_scenario() { # name mode → DID（全局变量）
  local name="$1" mode="$2" s
  echo "$mode" > "$RUN_DIR/receiver_mode"
  s=$(request PUT /api/internal/v1/webhook "$CRED_HW_FULL" "q08-w14-$name-config" "{\"url\":\"http://127.0.0.1:$RECV_PORT/hook\",\"events\":[\"file.confirmed\"]}")
  [[ "$s" == 200 ]] || die "$name: INT-12 config 失败 status=$s $(cat "$RESP")"
  jget "['secret']" > "$RUN_DIR/receiver_secret"
  [[ -s "$RUN_DIR/receiver_secret" ]] || die "$name: INT-12 未返回 secret"
  # emit 幂等键带运行时间戳：同键复用旧 delivery 会让断言查到上一次的
  # success 记录而 worker 不再投递（2026-10-07 实证：stub evidence 恒 0）。
  # 每次运行生成新 DID，重跑才有真实投递可断言。
  s=$(post /api/internal/v1/webhook/test-delivery "$CRED_HW_FULL" "q08-w14-$name-emit-$(date +%s)" '{}')
  [[ "$s" == 200 ]] || die "$name: INT-32 失败 status=$s $(cat "$RESP")"
  DID=$(jget "['delivery_id']")
  [[ -n "$DID" && "$DID" != parse-fail ]] || die "$name: delivery_id 解析失败"
  echo "== scenario $name → delivery $DID =="
}

echo "== W1: 500 → retry(5s) → 200 =="
run_scenario w1 retry
wait_delivery_status "$DID" success 40 || die "W1 未到 success"
ROW=$(psql_q "SELECT status || '/' || attempt_count FROM bot_delivery WHERE delivery_id='$DID'")
[[ "$ROW" == "success/2" ]] || die "W1 终态断言失败: $ROW"
ATTS=$(psql_q "SELECT string_agg(status_class || '/' || coalesce(http_status::text,'-'), ',' ORDER BY attempt_no) FROM bot_delivery_attempt WHERE delivery_id='$DID'")
[[ "$ATTS" == "5xx/500,2xx/200" ]] || die "W1 attempt 断言失败: $ATTS"
W1_CHECK=$(python3 - "$DID" "$RECV_PORT" <<'PY'
import json, subprocess, sys
did, port = sys.argv[1], sys.argv[2]
rows = json.loads(subprocess.check_output(["curl", "-s", f"http://127.0.0.1:{port}/evidence"]))
mine = [r for r in rows if r["delivery"] == did]
assert len(mine) == 2, "expect 2 receptions, got %d" % len(mine)
assert all(r["hmac_ok"] for r in mine), "HMAC 验签失败"
gap = mine[1]["ts_arrival"] - mine[0]["ts_arrival"]
assert gap >= 4.9, "两次到达间隔 %.2fs < 4.9s" % gap
assert mine[0]["body"] == mine[1]["body"], "body 逐字节不一致"
print("PASS gap=%.2fs hmac=verified-both body_len=%d" % (gap, len(mine[0]["body"])))
PY
) || die "W1 接收端断言失败"
echo "W14_W1=PASS $ROW attempts=[$ATTS] $W1_CHECK"

echo "== W2: 恒 410 → dead =="
run_scenario w2 dead
wait_delivery_status "$DID" dead 30 || die "W2 未到 dead"
ROW=$(psql_q "SELECT status || '/' || attempt_count FROM bot_delivery WHERE delivery_id='$DID'")
[[ "$ROW" == "dead/1" ]] || die "W2 终态断言失败: $ROW"
ATTS=$(psql_q "SELECT string_agg(status_class || '/' || coalesce(http_status::text,'-'), ',' ORDER BY attempt_no) FROM bot_delivery_attempt WHERE delivery_id='$DID'")
[[ "$ATTS" == "4xx/410" ]] || die "W2 attempt 断言失败: $ATTS"
sleep 7
ROW2=$(psql_q "SELECT status FROM bot_delivery WHERE delivery_id='$DID'")
[[ "$ROW2" == "dead" ]] || die "W2 死信不稳固: $ROW2"
echo "W14_W2=PASS $ROW attempts=[$ATTS] no-retry-after-dead"

echo "== W3: 挂起 1.6s（sender 超时 1s）→ {recv,timeout} → retry =="
run_scenario w3 hang
wait_delivery_status "$DID" retry 30 || die "W3 未到 retry"
ROW=$(psql_q "SELECT status || '/' || attempt_count FROM bot_delivery WHERE delivery_id='$DID'")
ATTS=$(psql_q "SELECT string_agg(status_class || '/' || coalesce(http_status::text,'-') || '/' || coalesce(error_trunc,''), ',' ORDER BY attempt_no) FROM bot_delivery_attempt WHERE delivery_id='$DID'")
[[ "$ROW" == "retry/1" && "$ATTS" == "error/-/{recv,timeout}" ]] || die "W3 断言失败: $ROW [$ATTS]"
echo "W14_W3=PASS $ROW attempts=[$ATTS]"

# ---- 证据 B：OA 页面替身链（AC20）----
echo "== OA stub: 自签 TLS + entries → code → /cb 落点 → 撤权 403 =="
bash scripts/test/oa_stub/gen_self_signed_cert.sh "$RUN_DIR"
python3 scripts/test/oa_stub/oa_stub_server.py "$OA_PORT" "$RUN_DIR/oa_stub.crt" "$RUN_DIR/oa_stub.key" "$RUN_DIR" > "$RUN_DIR/oa_stub.log" 2>&1 &
OA_PID=$!
for _ in $(seq 1 20); do curl -skf "https://127.0.0.1:$OA_PORT/healthz" >/dev/null && break; sleep 0.5; done
curl -skf "https://127.0.0.1:$OA_PORT/healthz" >/dev/null || die "oa stub not ready"

s=$(curl -sS -o "$RESP" -w '%{http_code}' "http://127.0.0.1:$HTTP_PORT/api/v1/workbench/entries?organization_id=$ORG" -H "Authorization: Bearer $JWT")
[[ "$s" == 200 ]] || die "entries 失败 status=$s $(cat "$RESP")"
# jget 模板是 print(d$1)——表达式必须以 d[' 下标形式接续；any(/len( 开头
# 会被拼成 dany(/dlen(（NameError→parse-fail）。取数组后管道二次过滤
# （jget 输出是 Python repr 单引号，二次解析用 ast 而非 json）。
KEY_IN=$(jget "['payload']['entries']" | python3 -c "import ast,sys;print(sum(1 for e in ast.literal_eval(sys.stdin.read()) if e['application_key']=='q08-w14-sso'))")
[[ "$KEY_IN" == "1" ]] || die "entries 未包含 SSO 应用（entries=$(cat "$RESP")）"
echo "OA_STUB_ENTRIES=PASS"

NONCE="q08stub$(date +%s)"
s=$(curl -sS -o "$RESP" -w '%{http_code}' -X POST "http://127.0.0.1:$HTTP_PORT/api/v1/oa/sso/code" \
  -H "Authorization: Bearer $JWT" -H 'Content-Type: application/json' \
  --data "{\"organization_id\":$ORG,\"application_key\":\"q08-w14-sso\",\"redirect_uri\":\"https://127.0.0.1:$OA_PORT/cb\",\"nonce\":\"$NONCE\"}")
[[ "$s" == 200 ]] || die "code 签发失败 status=$s $(cat "$RESP")"
SSO_CODE=$(jget "['payload']['code']")
[[ "$SSO_CODE" == oa_sso_* ]] || die "code 形态异常: $SSO_CODE"
LAND=$(curl -sk -o /dev/null -w '%{http_code}' "https://127.0.0.1:$OA_PORT/cb?code=$SSO_CODE&state=$NONCE")
[[ "$LAND" == 200 ]] || die "替身 /cb 落点失败: $LAND"
LAND_OK=$(python3 -c "
import json,subprocess
rows=json.loads(subprocess.check_output(['curl','-sk','https://127.0.0.1:$OA_PORT/evidence']))
m=[r for r in rows if r['code']=='$SSO_CODE']
assert m and m[0]['state']=='$NONCE', 'landing 未记录'
print('ok')" ) || die "落点证据断言失败"
[[ "$LAND_OK" == "ok" ]] || die "落点证据断言失败"
echo "OA_STUB_CODE_LANDING=PASS (code=${SSO_CODE:0:16}... state=nonce 已回放)"

psql_q "UPDATE enterprise_external_identity SET status='removed' WHERE organization_id=$ORG AND application_id=$APP_SSO" >/dev/null
s=$(curl -sS -o "$RESP" -w '%{http_code}' -X POST "http://127.0.0.1:$HTTP_PORT/api/v1/oa/sso/code" \
  -H "Authorization: Bearer $JWT" -H 'Content-Type: application/json' \
  --data "{\"organization_id\":$ORG,\"application_key\":\"q08-w14-sso\",\"redirect_uri\":\"https://127.0.0.1:$OA_PORT/cb\",\"nonce\":\"${NONCE}x\"}")
c=$(code_of)
[[ "$c" == "403" ]] || die "撤权后再签发应信封 code 403（got status=$s code=$c）"
echo "OA_STUB_REVOKED_REISSUE_403=PASS"

echo
echo "W14_RESULT=ok"
echo "EVIDENCE_DIR=${RUN_DIR}（含 node.log / webhook_receptions.jsonl / oa_stub_landings.jsonl）"
