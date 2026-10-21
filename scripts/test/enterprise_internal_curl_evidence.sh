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

# curl 超时（本地测试默认；环境变量可覆盖）：连接 5s / 单请求总 30s。
# 必须 export：inner.sh 由 driver 的 os:cmd 起独立进程，普通变量传不进去。
export CURL_CONNECT_TIMEOUT="${CURL_CONNECT_TIMEOUT:-5}"
export CURL_MAX_TIME="${CURL_MAX_TIME:-30}"
# 锁等待上限：另一运行持锁超时则显式失败，不无限自旋。
LOCK_WAIT_TIMEOUT_S="${LOCK_WAIT_TIMEOUT:-600}"

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEPS_ROOT="${IMBOY_DEPS_ROOT:-$REPO}"
export IMBOY_DEPS_ROOT="$DEPS_ROOT"
RUN_DIR="$(mktemp -d /tmp/imboy-int-curl.XXXXXX)"
export Q08_RUN_DIR="$RUN_DIR"
LOCK=/tmp/phase1-eunit.lock

# ---- mkdir 自旋锁（macOS 无 flock；与并行 eunit/节点会话互斥）----
# 有限等待：超过上限视为环境被长期占用，显式失败退出（不持锁、不清理他人资源）。
_lock_waited=0
while ! mkdir "$LOCK" 2>/dev/null; do
  sleep 5
  _lock_waited=$((_lock_waited + 5))
  if (( _lock_waited >= LOCK_WAIT_TIMEOUT_S )); then
    echo "ERROR: lock $LOCK not acquired within ${LOCK_WAIT_TIMEOUT_S}s (another run holds it)" >&2
    exit 3
  fi
done
trap 'rmdir "$LOCK" 2>/dev/null' EXIT
echo "== lock acquired: $LOCK =="

cleanup() {
  # RUN_DIR 保留（证据：evidence.jsonl / driver.log / compile.log），不删除。
  rmdir "$LOCK" 2>/dev/null || true
}
trap cleanup EXIT

cd "$REPO"

# ---- 工具与测试支撑模块预检（缺失即有限失败，不跑到一半才崩）----
for _tool in curl openssl python3 psql erl escript make; do
  command -v "$_tool" >/dev/null 2>&1 || {
    echo "ERROR: required tool not found: $_tool" >&2
    exit 3
  }
done
FIXTURE_MOD=test/features/enterprise_business/infrastructure/eb_pg_test_fixture.erl
[[ -f "$FIXTURE_MOD" ]] || {
  echo "ERROR: missing test support module: $FIXTURE_MOD" >&2
  exit 3
}

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
die() { echo "FATAL($PHASE): $1" >&2; echo "$1" > "$Q08_RUN_DIR/rc$PHASE"; exit 3; }

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
  # 签名材料任一环节失败即停（die）：空值/截断值继续发请求只会产生误导性负例。
  local ts nonce bhash sig
  ts=$(date +%s) || die "timestamp generation failed"
  # nonce 用 openssl rand（确定终止）；勿用 tr </dev/urandom | head：Erlang
  # os:cmd 子进程继承被忽略的 SIGPIPE，head 退出后 tr 会在 urandom 上死读。
  nonce="q08curl$(openssl rand -hex 12)" || die "nonce generation failed (openssl rand)"
  bhash=$(openssl dgst -sha256 -r "$REQ_BODY_F" | cut -d' ' -f1) || die "body digest failed (openssl dgst)"
  sig=$(printf 'IMBOY-INTERNAL-V1\nsha256\n%s\n\n%s\n%s\n\napplication/json\n%s\n%s\n%s\n%s' \
    "$prefix" "$method" "$path" "$idem" "$bhash" "$ts" "$nonce" \
    | openssl dgst -sha256 -hmac "$secret" -r | cut -d' ' -f1) || die "HMAC signing failed (openssl dgst -hmac)"
  # 生成结果长度/格式校验：nonce = q08curl + 24 hex（31 字符，符合签名器
  # nonce 16..128 与 [0-9a-zA-Z_-] 字符集）；摘要/签名 = 64 hex。
  [[ ${#nonce} -eq 31 && "$nonce" =~ ^q08curl[0-9a-f]{24}$ ]] || die "bad nonce format: '$nonce'"
  [[ ${#bhash} -eq 64 && "$bhash" =~ ^[0-9a-f]{64}$ ]] || die "bad body digest format: '$bhash'"
  [[ ${#sig} -eq 64 && "$sig" =~ ^[0-9a-f]{64}$ ]] || die "bad signature format: '$sig'"
  local hdrs=(-H "Content-Type: application/json"
    -H "x-imboy-credential: $prefix"
    -H "x-imboy-timestamp: $ts"
    -H "x-imboy-nonce: $nonce"
    -H "x-imboy-signature: sha256=$sig")
  for h in "$@"; do hdrs+=(-H "$h"); done
  : > "$BODY_F" || true
  local http_code curl_rc
  http_code=$(curl -sS -o "$BODY_F" -w '%{http_code}' \
    --connect-timeout "${CURL_CONNECT_TIMEOUT:-5}" --max-time "${CURL_MAX_TIME:-30}" \
    -X "$method" "http://127.0.0.1:${INT_PORT}${path}" "${hdrs[@]}" \
    --data-binary "@$REQ_BODY_F")
  curl_rc=$?
  # 传输故障与预期 4xx 必须区分：curl 非 0（连接拒绝/超时等）时 HTTP 码无意义，
  # 输出 000 让调用方按 FAIL 记录（不吞错、也不必中止——后续用例的失败同样入证）。
  if (( curl_rc != 0 )); then
    echo "TRANSPORT-ERROR: curl exit=$curl_rc on $method $path (server down or timeout; not an expected 4xx)" >&2
    echo 000
    return 0
  fi
  echo "$http_code"
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
    %% 归属输出：MARKER_DB 是本次 provision State 的库名（唯一能证明归属的
    %% 标识）；bash 侧只允许对它做幂等补删，绝不按前缀扫描/删除其他库。
    %% 若 setup_all 本身 raise（S 未绑定），不会打印此行，bash 侧不删任何库。
    io:format("MARKER_DB=~ts~n", [maps:get(db, S)]),
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
# driver 非零是预期路径（用例失败 → FAIL），set -e 会在管道处提前退出并跳过
# evidence 校验与清理——这里临时关 -e，以 PIPESTATUS 保留真实退出码。
set +e
escript "$RUN_DIR/driver.escript" 2>&1 | tee "$RUN_DIR/driver.log"
rc=${PIPESTATUS[0]}
set -e
echo "== evidence =="
cat "$RUN_DIR/evidence.jsonl" || true

# 成功判定不看打印的 "ok"：C01..C10 恰好各一次（无缺失/重复）、全部 PASS。
_evidence_verdict="$(python3 - "$RUN_DIR/evidence.jsonl" <<'PY'
import json, sys
try:
    rows = [json.loads(line) for line in open(sys.argv[1]) if line.strip()]
except Exception as exc:
    print(f"BAD:{exc}")
    raise SystemExit
ids = [row["id"] for row in rows]
expected = [f"C{i:02d}" for i in range(1, 11)]
if sorted(ids) != expected or len(ids) != len(set(ids)):
    missing = sorted(set(expected) - set(ids))
    dupes = sorted({i for i in ids if ids.count(i) > 1})
    print(f"INCOMPLETE missing={missing} dupes={dupes}")
elif not all(row.get("result") == "PASS" for row in rows):
    failed = [row["id"] for row in rows if row.get("result") != "PASS"]
    print(f"FAILED:{','.join(failed)}")
else:
    print("PASS")
PY
)"
echo "evidence verdict: $_evidence_verdict"
[[ "$_evidence_verdict" == "PASS" ]] || rc=1

# ---- marker 库清理（归属收窄）----
# release(S) 已在 VM 内对本 State 的库做精确 DROP；VM halt 竞态可能让该 DROP
# 报 55006 残留。此处**只**对 driver 报告的 MARKER_DB（本次 provision 的库名）
# 做一次幂等补删；没有归属证明的名字一律不碰，更不做任何前缀扫描。
MARKER_DB="$(sed -n 's/^MARKER_DB=//p' "$RUN_DIR/driver.log" | tail -1)"
if [[ -n "$MARKER_DB" ]]; then
  if PGPASSWORD="$INTBE02_INTTEST_PG_PASSWORD" psql -h "$INTBE02_INTTEST_PG_HOST" -p "$INTBE02_INTTEST_PG_PORT" \
      -U "$INTBE02_INTTEST_PG_USER" -d "$INTBE02_INTTEST_PG_DBNAME" \
      -c "DROP DATABASE IF EXISTS \"$MARKER_DB\"" >/dev/null 2>&1; then
    echo "marker db cleaned: $MARKER_DB"
  else
    echo "ERROR: marker db cleanup FAILED (kept for inspection; run dir: $RUN_DIR): $MARKER_DB" >&2
    rc=1
  fi
else
  echo "NOTE: driver reported no MARKER_DB (setup failed before provision); nothing to clean"
fi
rmdir "$LOCK" 2>/dev/null; trap - EXIT
exit "$rc"
