#!/usr/bin/env bash
# Q08 / AC19-6 — internal API 独立本地调用方（curl）证据脚本。
#
# 目的（Q08 调查 §1 AC19-6 缺口）：既有 internal 矩阵全部由仓内 eunit 真 HTTP 壳
# 驱动；本脚本用 **curl**（第二个、独立于仓内 eunit HTTP client 的调用方）对
# intbe02_http_support 建立的真 Cowboy 监听 + 一次性 marker PG 复跑 6 条代表路由
# （INT-01/02/03/12/13/14 + INT-32）的正反例，产出可重放证据：
#   C01 INT-01 GET application（HMAC 签名凭证 A）→ 200
#   C02 INT-01 GET application（HMAC 签名凭证 RO，第二个应用凭证）→ 200
#   C03 INT-02 PUT identity-mappings（坏 secret 签名，验签拒绝）→ 401 invalid_credential
#   C04 INT-03 POST identity-mappings/resolve → 200 命中映射
#   C05 INT-12 PUT webhook → 200 返回 secret（loopback http 仅 test profile 可过 SSRF 守卫）
#   C06 INT-32 POST webhook/test-delivery → 200 enqueued=true
#   C07 INT-12 同 Idempotency-Key 异 body 重放 → 409 idempotency_conflict
#   C08 INT-13 未知 delivery_id 重放 → 404 resource_not_found
#   C09 INT-14 exchange 伪造 code（不带 Idempotency-Key，验证 single_use_code 豁免）→ 404
#   C10 撤销凭证后 INT-01 → 401 invalid_credential（无 oracle）
#
# 凭据全部为 intbe02 合成夹具（995 段 + intbe02_secret_*），只对一次性 marker 库有效；
# PG 连接凭据从 config/sys.local.config 的 pg_conf **运行时**解析，只进本进程环境变量，
# 绝不打印、绝不写入任何产物。
#
# 与其他 eunit 会话互斥：编译与 marker PG 使用 /tmp/phase1-eunit.lock（mkdir 自旋锁）。
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEPS_ROOT="${IMBOY_DEPS_ROOT:-$REPO}"
export IMBOY_DEPS_ROOT="$DEPS_ROOT"
RUN_DIR="$(mktemp -d /tmp/imboy-int-curl.XXXXXX)"
export Q08_RUN_DIR="$RUN_DIR"
LOCK=/tmp/phase1-eunit.lock

# ---- mkdir 自旋锁（macOS 无 flock；与并行 eunit/节点会话互斥）----
while ! mkdir "$LOCK" 2>/dev/null; do sleep 5; done
trap 'rmdir "$LOCK" 2>/dev/null' EXIT
echo "== lock acquired: $LOCK =="

cleanup() {
  # RUN_DIR 保留（证据：evidence.jsonl / driver.log / compile.log），不删除。
  rmdir "$LOCK" 2>/dev/null || true
}
trap cleanup EXIT

cd "$REPO"

# ---- PG 连接（从本地 pg_conf 运行时解析；密码只进环境变量）----
eval "$(python3 - <<'PY'
import re
txt = open('config/sys.local.config').read()
m = re.search(r'\{pg_conf, #\{.*?host\s*=>\s*"([^"]+)".*?username\s*=>\s*"([^"]+)".*?password\s*=>\s*"([^"]+)".*?port\s*=>\s*(\d+)', txt, re.S)
if not m:
    m2 = re.search(r'pg_conf.*?host\s*=>\s*"([^"]+)".*?username\s*=>\s*"([^"]+)".*?password\s*=>\s*"([^"]+)".*?port\s*=>\s*(\d+)', txt, re.S)
    m = m2
assert m, 'pg_conf not found in config/sys.local.config'
print('export INTBE02_INTTEST_PG_HOST=%s INTBE02_INTTEST_PG_USER=%s INTBE02_INTTEST_PG_PORT=%s' % (m.group(1), m.group(2), m.group(4)))
print('export INTBE02_INTTEST_PG_PASSWORD=%s' % m.group(3))
print('export INTBE02_INTTEST_PG_DBNAME=postgres')
PY
)"
export IMBOYENV=test   # test profile：SSRF 守卫显式开放 loopback http（W4/W6 已另有 prod 拒绝例）
export ERL_CRASH_DUMP="$RUN_DIR/erl_crash.dump"

# ---- 编译（gate 脚本同款配方：src 全量 + 最小测试支撑）----
# eunit-local 的 A1c 硬复位可能暂缺 ebin/imboy.app：锁内先补齐产品 ebin。
[[ -f ebin/imboy.app ]] || make -s compile >"$RUN_DIR/compile_app.log" 2>&1
# 偶发 erlc 瞬时失败（共享 ebin 重建窗口内取到中间态）→ 重试一次。
mkdir -p "$RUN_DIR/beams"
compile_once() { python3 - <<'PY'
import os, pathlib, subprocess, shutil
out = pathlib.Path(os.environ['Q08_RUN_DIR'])
deps = pathlib.Path(os.environ.get('IMBOY_DEPS_ROOT', '.'))
shutil.copyfile(deps / 'ebin' / 'imboy.app', out / 'beams' / 'imboy.app')
paths = [str(p) for p in (deps / 'deps').glob('*/ebin') if p.is_dir()]
if not paths:
    raise SystemExit('dependencies absent; build dependencies or set IMBOY_DEPS_ROOT')
paths.insert(0, str(deps / 'ebin'))
files = [str(p) for p in pathlib.Path('src').rglob('*.erl')]
files += ['test/common/eunit_runner.erl', 'test/common/meck_helper.erl',
          'test/api/intbe02_http_support.erl', 'test/common/inttest_marker_db.erl',
          'test/features/enterprise_business/infrastructure/eb_pg_test_fixture.erl']
cmd = ['erlc', '+debug_info', '+nowarn_unused_function', '+nowarn_unused_vars',
       '+{parse_transform,lager_transform}', '-o', str(out / 'beams'), '-I', 'include', '-I', 'src']
for path in paths:
    cmd += ['-pa', path]
with (out / 'compile.log').open('w') as log:
    r = subprocess.run(cmd + files, stdout=log, stderr=subprocess.STDOUT)
if r.returncode != 0:
    raise SystemExit('compile failed, see compile.log')
print('compiled', len(files), 'modules')
PY
}
compile_once || compile_once || { tail -30 "$RUN_DIR/compile.log"; exit 1; }

# ---- 内层 curl 用例（由 erl VM 在监听就绪后分两相调用）----
cat > "$RUN_DIR/inner.sh" <<'INNER'
#!/usr/bin/env bash
set -uo pipefail
PHASE="$1"
source "$Q08_RUN_DIR/fixtures.env"
EV="$Q08_RUN_DIR/evidence.jsonl"
log_case() { # id name status code result（code 为信封 error.code，成功例为空）
  printf 'CASE %s %-38s -> HTTP %s %s [%s]\n' "$1" "$2" "$3" "${4:-}" "${5:-}"
  printf '{"id":"%s","name":"%s","status":%s,"code":"%s","result":"%s"}\n' \
    "$1" "$2" "$3" "${4:-}" "${5:-}" >> "$EV"
}
fail() { log_case "$1" "$2" "$3" "$4" "FAIL"; echo "$1" > "$Q08_RUN_DIR/rc$PHASE"; exit 0; }
ok()   { log_case "$1" "$2" "$3" "$4" "PASS"; }

# get_body_code <bodyfile>  → 信封 error.code（internal 面）或顶层 code
code_of() {
  python3 -c "
import json,sys
try:
    d=json.load(open('$1'))
except Exception:
    print(''); raise SystemExit
e=d.get('error')
print(e.get('code') if isinstance(e,dict) else d.get('code',''))"
}
jget() { python3 -c "import json;d=json.load(open('$1'));print(d$2)"; }

req() { # method path cred extra-headers... ; body via stdin; 输出 status，body 落 $BODY_F
  # Internal 面合同：HMAC-SHA256 请求签名（四头），canonical 12 行与
  # enterprise_internal_signature:canonical/5 逐字节一致（org/query 空行）。
  # REQ_SECRET_OVERRIDE 可替换 HMAC key（坏签名负例 C03 用）。
  local method="$1" path="$2" cred="$3"; shift 3
  local prefix="${cred%%.*}" secret="${REQ_SECRET_OVERRIDE:-${cred#*.}}"
  local idem="" h
  for h in "$@"; do
    case "$h" in
      [Ii]dempotency-[Kk]ey:*) idem="${h#*:}"; idem="${idem# }";;
    esac
  done
  cat > "$REQ_BODY_F"   # body 落盘：摘要与发送同源
  local ts nonce bhash sig
  ts=$(date +%s)
  # nonce 用 openssl rand（确定终止）；勿用 tr </dev/urandom | head：Erlang
  # os:cmd 子进程继承被忽略的 SIGPIPE，head 退出后 tr 会在 urandom 上死读。
  nonce="q08curl$(openssl rand -hex 12)"
  bhash=$(openssl dgst -sha256 -r "$REQ_BODY_F" | cut -d' ' -f1)
  sig=$(printf 'IMBOY-INTERNAL-V1\nsha256\n%s\n\n%s\n%s\n\napplication/json\n%s\n%s\n%s\n%s' \
    "$prefix" "$method" "$path" "$idem" "$bhash" "$ts" "$nonce" \
    | openssl dgst -sha256 -hmac "$secret" -r | cut -d' ' -f1)
  local hdrs=(-H "Content-Type: application/json"
    -H "x-imboy-credential: $prefix"
    -H "x-imboy-timestamp: $ts"
    -H "x-imboy-nonce: $nonce"
    -H "x-imboy-signature: sha256=$sig")
  for h in "$@"; do hdrs+=(-H "$h"); done
  : > "$BODY_F" || true
  curl -sS -o "$BODY_F" -w '%{http_code}' -X "$method" \
    "http://127.0.0.1:${INT_PORT}${path}" "${hdrs[@]}" --data-binary "@$REQ_BODY_F" || true
}
BODY_F="$Q08_RUN_DIR/body.json"
REQ_BODY_F="$Q08_RUN_DIR/req_body.json"

if [[ "$PHASE" == "1" ]]; then
  s=$(echo '{}' | req GET /api/internal/v1/application "$INT_CRED_A")
  c=$(code_of "$BODY_F"); k=$(jget "$BODY_F" "['application_key']")
  [[ "$s" == 200 && "$k" == "$INT_APP_KEY_A" ]] && ok C01 "INT-01 signed cred GET application" "$s" "" || fail C01 "INT-01 signed cred GET application" "$s" "$c"

  s=$(echo '{}' | req GET /api/internal/v1/application "$INT_CRED_RO")
  c=$(code_of "$BODY_F")
  [[ "$s" == 200 ]] && ok C02 "INT-01 second signed cred GET application" "$s" "" || fail C02 "INT-01 second signed cred GET application" "$s" "$c"

  # 坏 secret 签名（格式合法四头、canonical 正确、HMAC key 错）→ 验签拒绝 401
  s=$(printf '{"external_user_id":"intbe02-ext-new","user_id":995013}' \
      | REQ_SECRET_OVERRIDE="wrong_secret_for_bad_signature_neg" \
        req PUT /api/internal/v1/identity-mappings "$INT_CRED_A" 'Idempotency-Key: q08-curl-C03')
  c=$(code_of "$BODY_F")
  [[ "$s" == 401 && "$c" == invalid_credential ]] && ok C03 "INT-02 bad-signature 401" "$s" "$c" || fail C03 "INT-02 bad-signature 401" "$s" "$c"

  s=$(printf '{"external_user_ids":["%s"]}' "$INT_EXT_H1" | req POST /api/internal/v1/identity-mappings/resolve "$INT_CRED_A")
  c=$(code_of "$BODY_F"); u=$(jget "$BODY_F" "['mappings'][0]['user_id']")
  [[ "$s" == 200 && "$u" == 995011 ]] && ok C04 "INT-03 resolve mapping" "$s" "" || fail C04 "INT-03 resolve mapping" "$s" "$c"

  s=$(printf '{"url":"http://127.0.0.1:9/hook","events":["file.confirmed"]}' | req PUT /api/internal/v1/webhook "$INT_CRED_A" 'Idempotency-Key: q08-curl-C05')
  c=$(code_of "$BODY_F"); sec=$(jget "$BODY_F" "['secret']")
  [[ "$s" == 200 && -n "$sec" ]] && ok C05 "INT-12 configure webhook (+secret)" "$s" "" || fail C05 "INT-12 configure webhook (+secret)" "$s" "$c"

  s=$(echo '{}' | req POST /api/internal/v1/webhook/test-delivery "$INT_CRED_A" 'Idempotency-Key: q08-curl-C06')
  c=$(code_of "$BODY_F"); eq=$(jget "$BODY_F" "['enqueued']"); ev=$(jget "$BODY_F" "['event_type']")
  [[ "$s" == 200 && "$eq" == True && "$ev" == webhook.ping ]] && ok C06 "INT-32 test-delivery enqueued" "$s" "" || fail C06 "INT-32 test-delivery enqueued" "$s" "$c"

  s=$(printf '{"url":"http://127.0.0.1:10/hook","events":["file.confirmed"]}' | req PUT /api/internal/v1/webhook "$INT_CRED_A" 'Idempotency-Key: q08-curl-C05')
  c=$(code_of "$BODY_F")
  [[ "$s" == 409 && "$c" == idempotency_conflict ]] && ok C07 "INT-12 same-key diff-body 409" "$s" "$c" || fail C07 "INT-12 same-key diff-body 409" "$s" "$c"

  s=$(echo '{}' | req POST /api/internal/v1/webhook/deliveries/9223372036854775807/replay "$INT_CRED_A" 'Idempotency-Key: q08-curl-C08')
  c=$(code_of "$BODY_F")
  [[ "$s" == 404 && "$c" == resource_not_found ]] && ok C08 "INT-13 unknown delivery replay 404" "$s" "$c" || fail C08 "INT-13 unknown delivery replay 404" "$s" "$c"

  # INT-14 single_use_code 幂等豁免：不带 Idempotency-Key 不得 400 invalid_request
  s=$(printf '{"code":"oa_sso_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA","redirect_uri":"%s","nonce":"%s"}' "$INT_REDIRECT" "$INT_NONCE" | req POST /api/internal/v1/oa/sso/exchange "$INT_CRED_SSO")
  c=$(code_of "$BODY_F")
  [[ "$s" == 404 && "$c" == resource_not_found ]] && ok C09 "INT-14 bogus code no-idem-key 404" "$s" "$c" || fail C09 "INT-14 bogus code no-idem-key 404" "$s" "$c"
else
  s=$(echo '{}' | req GET /api/internal/v1/application "$INT_CRED_D")
  c=$(code_of "$BODY_F")
  [[ "$s" == 401 && "$c" == invalid_credential ]] && ok C10 "INT-01 revoked credential 401" "$s" "$c" || fail C10 "INT-01 revoked credential 401" "$s" "$c"
fi
echo 0 > "$Q08_RUN_DIR/rc$PHASE"
INNER
chmod +x "$RUN_DIR/inner.sh"

# ---- erl 驱动器（escript；监听 + seed + 两相调用 + 撤销凭证）----
cat > "$RUN_DIR/driver.escript" <<'DRIVER'
#!/usr/bin/env escript
%%! -noshell
main(_) ->
    RunDir = os:getenv("Q08_RUN_DIR"),
    Deps = os:getenv("IMBOY_DEPS_ROOT"),
    Beams = filename:join(RunDir, "beams"),
    code:add_patha(Beams),
    [code:add_patha(P) || P <- filelib:wildcard(filename:join(Deps, "deps/*/ebin"))],
    application:set_env(lager, handlers, [{lager_console_backend, [{level, error}]}]),
    {ok, _} = application:ensure_all_started(lager),
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, postgre_aes_key, <<"SYNTHETIC-CONFORMANCE-AES-KEY">>),
    %% pg_conf 形状占位（gate 脚本同款）：真实 host/port/user/password 由
    %% INTBE02_INTTEST_PG_* 环境变量优先提供（inttest_marker_db:resolve_server）。
    application:set_env(imboy, pg_conf, #{start_mfa => {epgsql, connect, [#{}]}}),
    {ok, _} = imboy_cache:start_link([]),
    S = intbe02_http_support:setup_all(),
    Rcs = try
        Port = maps:get(port, S),
        Env = [
            {"INT_PORT", integer_to_list(Port)},
            {"INT_CRED_A", maps:get(cred_a, S)},
            {"INT_CRED_RO", maps:get(cred_ro, S)},
            {"INT_CRED_D", maps:get(cred_d, S)},
            {"INT_CRED_SSO", maps:get(cred_sso, S)},
            {"INT_APP_KEY_A", <<"intbe02-oa-a">>},
            {"INT_EXT_H1", <<"intbe02-ext-h1">>},
            {"INT_REDIRECT", <<"https://oa.customer.example.com/sso/cb">>},
            {"INT_NONCE", <<"nonce_intbe02_0123456789abcdef">>}
        ],
        ok = file:write_file(
            filename:join(RunDir, "fixtures.env"),
            [[ "export ", K, "='", V, "'\n" ] || {K, V} <- Env]
        ),
        %% teardown 的 ensure_default_pool 会按 imboy.pg_conf 重建默认池：
        %% 回填 marker 库真实连接参数（来自 provision State），避免空占位崩溃。
        Server = maps:get(server, S),
        application:set_env(
            imboy,
            pg_conf,
            #{
                name => pgsql,
                max_count => 4,
                init_count => 1,
                start_mfa =>
                    {epgsql, connect, [
                        #{
                            host => maps:get(host, Server),
                            port => maps:get(port, Server),
                            username => maps:get(username, Server),
                            password => maps:get(password, Server),
                            database => maps:get(db, S),
                            ssl => false,
                            timeout => 10000,
                            codecs => [{epgsql_codec_rfc3339_bin, []}]
                        }
                    ]}
            }
        ),
        %% 相1：正反例 C01..C09（输出留档 RUN_DIR/phase1.log 作为证据）
        _ = os:cmd("bash " ++ filename:join(RunDir, "inner.sh") ++ " 1 > " ++ filename:join(RunDir, "phase1.log") ++ " 2>&1"),
        %% 撤销 App D 凭证（revoked → invalid_credential，无 oracle 状态）
        [Prefix | _] = binary:split(maps:get(cred_d, S), <<".">>),
        ok = intbe02_http_support:sql_exec(
            maps:get(conn, S),
            [
                <<"UPDATE enterprise_application_credential SET status='revoked', revoked_at=CURRENT_TIMESTAMP WHERE credential_prefix='">>,
                Prefix, <<"';">>
            ]
        ),
        %% 相2：C10
        _ = os:cmd("bash " ++ filename:join(RunDir, "inner.sh") ++ " 2 > " ++ filename:join(RunDir, "phase2.log") ++ " 2>&1"),
        Rc1 = read_rc(RunDir, "1"),
        Rc2 = read_rc(RunDir, "2"),
        io:format("PHASE1_RC=~p PHASE2_RC=~p~n", [Rc1, Rc2]),
        {Rc1, Rc2}
    after
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end,
    case Rcs of
        {0, 0} ->
            io:format("Q08_CURL_EVIDENCE_RESULT=ok~n"),
            halt(0);
        _ ->
            io:format("Q08_CURL_EVIDENCE_RESULT=FAIL~n"),
            halt(1)
    end.

read_rc(RunDir, Phase) ->
    case file:read_file(filename:join(RunDir, "rc" ++ Phase)) of
        {ok, <<"0", _/binary>>} -> 0;
        _ -> 1
    end.
DRIVER

echo "== booting listener VM (disposable marker PG + real Cowboy) =="
escript "$RUN_DIR/driver.escript" 2>&1 | tee "$RUN_DIR/driver.log"
rc=${PIPESTATUS[0]}
echo "== evidence =="
cat "$RUN_DIR/evidence.jsonl" || true
# VM halt 竞态可能让 marker 库 DROP 报 55006（teardown 的默认池重建连接晚于
# release）→ 清理**无活动连接**的 intbe02 残留 marker 库（他人在用的跳过）。
PGPASSWORD="$INTBE02_INTTEST_PG_PASSWORD" psql -h "$INTBE02_INTTEST_PG_HOST" -p "$INTBE02_INTTEST_PG_PORT" \
  -U "$INTBE02_INTTEST_PG_USER" -d "$INTBE02_INTTEST_PG_DBNAME" -tAc \
  "SELECT datname FROM pg_database WHERE datname LIKE 'inttest_intbe02_%' AND NOT EXISTS (SELECT 1 FROM pg_stat_activity a WHERE a.datname = pg_database.datname AND a.pid <> pg_backend_pid())" 2>/dev/null \
  | while read -r db; do
      [[ -z "$db" ]] && continue
      PGPASSWORD="$INTBE02_INTTEST_PG_PASSWORD" psql -h "$INTBE02_INTTEST_PG_HOST" -p "$INTBE02_INTTEST_PG_PORT" \
        -U "$INTBE02_INTTEST_PG_USER" -d "$INTBE02_INTTEST_PG_DBNAME" -c "DROP DATABASE IF EXISTS \"$db\"" >/dev/null 2>&1 && echo "dropped leftover marker db: $db"
    done
rmdir "$LOCK" 2>/dev/null; trap - EXIT
exit "$rc"
