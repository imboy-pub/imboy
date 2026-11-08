#!/usr/bin/env bash
# Golden smoke fixture regression: real shell functions, strict HTTP/CLI stubs.
# 离线合成测试，不构建镜像、不访问网络，不代表真实 Golden/WS/存储验收。
# shellcheck disable=SC2030,SC2031 # isolated subshells deliberately share fixture globals
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
umask 077
mkdir -p "$TMP/bin"
CHECKS=0
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
pass() { CHECKS=$((CHECKS + 1)); printf 'PASS %02d: %s\n' "$CHECKS" "$*"; }

# No HTTP request is sent. Unknown methods, options, endpoints, credentials or
# friend transitions fail instead of returning an unconditional successful stub.
cat > "$TMP/bin/curl" <<'PY'
#!/usr/bin/env python3
import hashlib
import json
import os
import pathlib
import sys
from urllib.parse import urlsplit

args = iter(sys.argv[1:])
method, output, url, headers, form, body = 'GET', None, None, {}, {}, None
for arg in args:
    if arg in ('-sS', '-s', '-S'):
        continue
    if arg in ('-m', '-w'):
        next(args)
    elif arg == '-o':
        output = pathlib.Path(next(args))
    elif arg == '-X':
        method = next(args)
    elif arg == '-H':
        key, value = next(args).split(':', 1)
        headers[key.lower()] = value.strip()
    elif arg == '--data-urlencode':
        key, value = next(args).split('=', 1)
        assert key not in form, 'duplicate form field'
        form[key] = value
    elif arg in ('-d', '--data-binary'):
        body = next(args)
    elif arg.startswith('http://'):
        assert url is None, 'multiple URLs'
        url = arg
    else:
        raise AssertionError('unexpected curl option')
assert output and url and urlsplit(url).hostname in ('golden.invalid', 'object.invalid')
state_path = pathlib.Path(os.environ['GOLDEN_TEST_STATE'])
state = json.loads(state_path.read_text())
mode = os.environ.get('GOLDEN_TEST_MODE', '')
events = pathlib.Path(os.environ['GOLDEN_TEST_EVENTS'])
path = urlsplit(url).path
status, response = '200', {'code': 0, 'payload': {}}
def event(name):
    with events.open('a') as stream:
        stream.write(name + '\n')
def reject(kind):
    global status, response
    if mode in (kind + '_http', kind + '_business'):
        status = '503' if mode.endswith('_http') else '200'
        response = {'code': 503, 'msg': 'PRIVATE_RESPONSE_CANARY',
                    'payload': {'token': 'PRIVATE_RESPONSE_TOKEN'}}
        return True
    return False
def authenticated():
    return next(user for user in state['users']
                if headers.get('authorization') == 'Bearer ' + user['token'])

if path == '/api/v1/passport/signup':
    assert method == 'POST' and form['type'] == 'email' and form['rsa_encrypt'] == '0'
    assert form['code'] == os.environ['GOLDEN_TEST_CODE'], 'registration code drift'
    assert form['account'].endswith('@smoke.local') and form['nickname']
    assert len(form['pwd']) >= 8 and not any(u['email'] == form['account'] for u in state['users'])
    event('signup')
    if mode == 'transport':
        sys.exit(7)
    if not reject('signup'):
        index = len(state['users'])
        state['users'].append({'email': form['account'], 'pwd': form['pwd'],
                               'uid': str(9223372036854775001 + index),
                               'token': 'fixture-private-token-' + str(index)})
elif path == '/api/v1/passport/login':
    assert method == 'POST' and form['type'] == 'email' and form['rsa_encrypt'] == '0'
    event('login')
    user = next(u for u in state['users'] if u['email'] == form['account'])
    assert user['pwd'] == form['pwd']
    rejected = reject('login')
    # Even rejected responses carry valid IDs/tokens: status/code must be checked.
    response['payload'] = {'uid': user['uid'] if mode == 'string_ids' else int(user['uid']),
                           'token': user['token']}
    if not rejected:
        if mode == 'login_missing_uid':
            del response['payload']['uid']
        elif mode == 'login_missing_token':
            del response['payload']['token']
        elif mode == 'login_float_uid':
            response['payload']['uid'] = float(user['uid'])
        elif mode == 'login_boolean_uid':
            response['payload']['uid'] = True
        elif mode == 'login_bad_uid':
            response['payload']['uid'] = '0'
        elif mode == 'duplicate_uid':
            response['payload']['uid'] = state['users'][0]['uid']
elif path in ('/api/v1/friend/add', '/api/v1/friend/confirm'):
    a, b = state['users']
    assert method == 'POST' and form['to'] == b['uid']
    assert json.loads(form['payload'])['from']['source'] == 'golden'
    if path.endswith('/add'):
        assert authenticated() == a and form['created_at'].isdigit()
        event('friend:add')
        if not reject('friend_add'):
            state['pending'] = True
    else:
        event('friend:confirm')
        assert authenticated() == b and form['from'] == a['uid'] and state.get('pending')
        if not reject('friend_confirm'):
            state['friends'] = True
elif path == '/api/v1/friend/list':
    a, b = state['users']
    user = authenticated()
    event('friend:list:A' if user == a else 'friend:list:B')
    assert method == 'GET' and state.get('friends')
    reject('friend_list')
    peer = (b if user == a else a)['uid']
    # friend[] contains peer user profiles, not user_friend relation rows.
    response['payload']['friend'] = [] if mode == 'reverse_missing' and user == b else [
        {'id': peer if mode == 'string_ids' else int(peer)}]
    if mode == 'legacy_friend_fields':
        response['payload']['friend'] = [{'from_user_id': user['uid'], 'to_user_id': peer}]
elif path == '/api/v1/attachment/presign':
    user = authenticated()
    assert method == 'GET'
    event('attachment:presign')
    state['object_key'] = 'u' + user['uid'] + '/golden.txt'
    state['object_token'] = user['token']
    response['payload'] = {'put_url': 'http://object.invalid/upload', 'object_key': state['object_key']}
elif path == '/upload':
    assert method == 'PUT' and body.startswith('@')
    data = pathlib.Path(body[1:]).read_bytes()
    state['md5'], state['size'] = hashlib.md5(data).hexdigest(), len(data)
    event('attachment:put')
    status = '204'
elif path == '/api/v1/attachment/confirm':
    assert method == 'POST' and authenticated()['token'] == state['object_token']
    data = json.loads(body)
    assert data['object_key'] == state['object_key'] and data['md5'] == state['md5']
    assert data['size'] == state['size'] and data['mime_type'] == 'text/plain'
    event('attachment:confirm')
elif path == '/api/v1/group/add':
    a, b = state['users']
    assert method == 'POST' and authenticated() == a and state.get('friends')
    assert json.loads(body)['member_uids'] == [b['uid']]
    event('group:add')
    reject('group')
    gid = '9223372036854775010'
    response['payload']['group'] = {'id': gid if mode == 'string_ids' else int(gid)}
    invalid = {'group_float': float(gid), 'group_boolean': True, 'group_zero': 0,
               'group_missing': None, 'group_invalid': 'not-a-group'}
    if mode in invalid:
        response['payload']['group']['id'] = invalid[mode]
else:
    raise AssertionError('unexpected endpoint')
state_path.write_text(json.dumps(state))
output.write_text(json.dumps(response))
sys.stdout.write(status)
PY
chmod +x "$TMP/bin/curl"
export PATH="$TMP/bin:$PATH"

# Load definitions, never either CLI's main (which can clone, start or clean up).
# Ask Bash to recognize a complete function instead of counting quoted braces.
python3 - "$ROOT" "$TMP/functions.sh" <<'PY'
import pathlib
import re
import subprocess
import sys
root, destination = map(pathlib.Path, sys.argv[1:])
selected = {
    'golden_install.sh': ['idem_http', 'idem_json', 'idem_upload'],
    'golden_upgrade.sh': ['up_http', 'up_jq', 'state_set_json', 'gu_register_login', 'gu_send_c2c', 'gu_group_create',
                          'gu_friend_make', 'gu_friend_has', 'gu_datagen'],
}
with destination.open('w') as output:
    for filename, names in selected.items():
        lines = (root / 'scripts' / filename).read_text().splitlines(True)
        for name in names:
            start = next(i for i, line in enumerate(lines) if re.match(r'^' + name + r'\(\)\s*\{', line))
            for end in range(start + 1, len(lines) + 1):
                snippet = ''.join(lines[start:end])
                if subprocess.run(['bash', '-n'], input=snippet, text=True, capture_output=True).returncode == 0:
                    output.write(snippet + '\n')
                    break
            else:
                raise AssertionError('incomplete function: ' + name)
install = (root / 'scripts/golden_install.sh').read_text().splitlines(True)
start = next(i for i, line in enumerate(install) if line.strip() == 'CURRENT_STAGE="smoke8"')
end = next(i for i in range(start, len(install)) if install[i].strip() == 'T1=$(now)')
destination.with_name('install-smoke-stage.sh').write_text(''.join(install[start:end + 1]))
PY
# shellcheck source=scripts/golden_common.sh
. "$ROOT/scripts/golden_common.sh"
# shellcheck disable=SC1091 # extracted real functions are in a private temporary file
. "$TMP/functions.sh"

# shellcheck disable=SC2034 # globals consumed by the extracted Install/Upgrade functions
new_case() {
  local name="$1"
  RUN_DIR="$TMP/$name"; CLONE_DIR="$RUN_DIR/clone"; DEPLOY_DIR="$CLONE_DIR/deploy"
  BIN_DIR="$TMP/bin"; OVERRIDE_YML="$DEPLOY_DIR/smoke.yml"
  STATE_FILE="$RUN_DIR/upgrade_state.json"; idem_body="$RUN_DIR/idem_body"
  UP_BODY="$RUN_DIR/up_body"
  IDEM_CONTENT='GOLDEN-IDEM-ROUNDTRIP'
  PROFILE=ci; BASE_URL=http://golden.invalid; WS_URL=ws://golden.invalid/api/v1/ws
  ADMIN_PHONE=13800138000; ADMIN_PASSWORD=Fixture_Admin_Only_1234
  COMPOSE_O='blocked-docker compose'; STAGE_HINT=''
  unset SMOKE_C2C_FROM SMOKE_C2C_TO GOLDEN_SMOKE_CODE
  mkdir -p "$DEPLOY_DIR" "$CLONE_DIR/scripts"
  printf 'POSTGRES_USER=fixture\nPOSTGRES_DB=fixture\nPOSTGRES_PASSWORD=fixture-only\n' > "$DEPLOY_DIR/.env"
  export GOLDEN_TEST_STATE="$RUN_DIR/server.json" GOLDEN_TEST_EVENTS="$RUN_DIR/events"
  export GOLDEN_TEST_MODE="${2:-}"
  printf '{"users":[]}' > "$GOLDEN_TEST_STATE"
  printf '{}' > "$STATE_FILE"
  : > "$GOLDEN_TEST_EVENTS"
  write_compose_override
  export GOLDEN_TEST_CODE="$GOLDEN_SMOKE_CODE"
  printf '%s' "$GOLDEN_SMOKE_CODE" > "$RUN_DIR/expected-code"
  cat > "$CLONE_DIR/scripts/smoke_8step.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[[ "$SMOKE_MASTER_CODE" == "$GOLDEN_TEST_CODE" && "$SMOKE_C2C_FROM" == 9223372036854775001 \
  && "$SMOKE_C2C_TO" == 9223372036854775002 && "$WS_URL" == ws://golden.invalid/api/v1/ws ]]
printf 'smoke8\n' >> "$GOLDEN_TEST_EVENTS"
EOF
  cat > "$CLONE_DIR/scripts/imboy_ctl" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[[ $# == 6 && "$1 $2" == 'msg send' && "$3" == 9223372036854775001 \
  && "$4" == 9223372036854775002 && "$5" == -T && -n "$6" ]]
python3 - "$GOLDEN_TEST_STATE" "$GOLDEN_TEST_EVENTS" <<'PY'
import json, pathlib, sys
assert json.loads(pathlib.Path(sys.argv[1]).read_text())['friends']
events = pathlib.Path(sys.argv[2]).read_text().splitlines()
assert 'friend:list:A' in events and 'friend:list:B' in events
PY
printf 'c2c\n' >> "$GOLDEN_TEST_EVENTS"
if [[ "$GOLDEN_TEST_MODE" == c2c_denied ]]; then
  printf 'encrypted_message_required\n' >&2; exit 23
fi
printf 'MSG_ID=fixture-c2c\n'
EOF
  chmod +x "$CLONE_DIR/scripts/imboy_ctl"
}

assert_private_output() { # No tokens, passwords, master code or failure bodies in logs.
  python3 - "$TMP/$1" "$TMP/$1.out" <<'PY'
import json, pathlib, sys
case, log = map(pathlib.Path, sys.argv[1:])
output = log.read_text() + (case / 'events').read_text()
secrets = [case.joinpath('expected-code').read_text(), 'PRIVATE_RESPONSE_CANARY', 'PRIVATE_RESPONSE_TOKEN']
for user in json.loads(case.joinpath('server.json').read_text())['users']:
    secrets.extend([user['pwd'], user['token']])
assert all(secret and secret not in output for secret in secrets), 'fixture secret appeared in output'
PY
}

(
  new_case install
  TOKEN=attachment-token-sentinel
  # shellcheck disable=SC2329 # invoked by COMPOSE_O in the extracted Install stage
  smoke_compose() {
    [[ "$*" == 'up -d' ]] || return 1
    printf 'compose:smoke\n' >> "$GOLDEN_TEST_EVENTS"
  }
  COMPOSE_O=smoke_compose
  # shellcheck disable=SC2329 # invoked by the extracted Install stage
  wait_healthz() { [[ "$1" == 180 ]]; }
  # shellcheck disable=SC2329 # invoked by the extracted Install stage
  hosts_set_garage() { :; }
  # Execute the actual Install smoke stage so omitting its preparation call fails.
  # shellcheck disable=SC1091 # extracted executable stage, all external surfaces stubbed
  . "$TMP/install-smoke-stage.sh"
  [[ "$TOKEN" == attachment-token-sentinel && "$SMOKE_C2C_FROM" == 9223372036854775001 \
    && "$SMOKE_C2C_TO" == 9223372036854775002 ]] || fail 'generated pair lost identity or caller token'
  [[ "$(cat "$GOLDEN_TEST_EVENTS")" == $'compose:smoke\nsignup\nlogin\nsignup\nlogin\nfriend:add\nfriend:confirm\nfriend:list:A\nfriend:list:B\nsmoke8' ]] \
    || fail 'Install fixture did not finish both friend directions before smoke8'
) > "$TMP/install.out" 2>&1 || fail 'Install smoke fixture failed'
assert_private_output install
pass 'Install preserves JSON bigint identities exactly and confirms both friends before smoke; caller token is preserved'

(
  new_case string-ids string_ids
  prepare_golden_smoke_users
  [[ "$SMOKE_C2C_FROM" == 9223372036854775001 && "$SMOKE_C2C_TO" == 9223372036854775002 ]]
  run_smoke8
) > "$TMP/string-ids.out" 2>&1 || fail 'string UID compatibility failed'
assert_private_output string-ids
pass 'decimal-string login and friend profile IDs preserve the same large identities'

for mode in signup_http signup_business transport login_http login_business login_missing_uid \
  login_missing_token login_float_uid login_boolean_uid login_bad_uid duplicate_uid friend_add_business \
  friend_confirm_business friend_list_business reverse_missing legacy_friend_fields; do
  if (
    new_case "$mode" "$mode"
    prepare_golden_smoke_users || { printf '%s\n' "$STAGE_HINT"; exit 1; }
  ) > "$TMP/$mode.out" 2>&1; then
    fail "$mode: incomplete fixture was accepted"
  fi
  assert_private_output "$mode"
  expected=$'signup\nlogin\nsignup\nlogin'
  case "$mode" in
    signup_*|transport) expected=signup ;;
    login_*) expected=$'signup\nlogin' ;;
    duplicate_uid) : ;;
    friend_add_business) expected+=$'\nfriend:add' ;;
    friend_confirm_business) expected+=$'\nfriend:add\nfriend:confirm' ;;
    friend_list_business|legacy_friend_fields) expected+=$'\nfriend:add\nfriend:confirm\nfriend:list:A' ;;
    reverse_missing) expected+=$'\nfriend:add\nfriend:confirm\nfriend:list:A\nfriend:list:B' ;;
  esac
  [[ "$(cat "$TMP/$mode/events")" == "$expected" ]] || fail "$mode: execution continued beyond the rejected transition"
done
pass 'HTTP, transport, business, identity and both friendship directions fail closed without exposing response bodies'

for mode in missing_code missing_from missing_to invalid_uid same_uid; do
  if (
    new_case "$mode"
    SMOKE_C2C_FROM=9223372036854775001; SMOKE_C2C_TO=9223372036854775002
    case "$mode" in
      missing_code) unset GOLDEN_SMOKE_CODE ;;
      missing_from) unset SMOKE_C2C_FROM ;;
      missing_to) unset SMOKE_C2C_TO ;;
      invalid_uid) SMOKE_C2C_FROM=invalid ;;
      same_uid) SMOKE_C2C_TO="$SMOKE_C2C_FROM" ;;
    esac
    run_smoke8
  ) > "$TMP/$mode.out" 2>&1; then fail "$mode: smoke child ran with invalid inputs"; fi
  [[ ! -s "$TMP/$mode/events" ]] || fail "$mode: invalid inputs reached child or HTTP"
done
pass 'smoke child requires the matching master code and a complete, distinct UID pair'

# The actual group creation helper must retain the exact ID used by later WS/history.
group_fixture() {
  new_case "$1" "${2:-}"
  golden_register_login A || return 1
  local token_a="$GOLDEN_USER_TOKEN" uid_a="$GOLDEN_USER_UID"
  golden_register_login B || return 1
  golden_friend_make "$token_a" "$uid_a" "$GOLDEN_USER_TOKEN" "$GOLDEN_USER_UID" || return 1
  gu_group_create "$token_a" "$GOLDEN_USER_UID" || { printf '%s\n' "$STAGE_HINT"; return 1; }
  [[ "$UP_GID" == 9223372036854775010 ]]
}
for mode in numeric_ids string_ids; do
  (group_fixture "group-$mode" "$mode") > "$TMP/group-$mode.out" 2>&1 || fail "$mode: group ID lost precision"
  assert_private_output "group-$mode"
done
for mode in group_http group_business group_float group_boolean group_zero group_missing group_invalid; do
  if (group_fixture "$mode" "$mode") > "$TMP/$mode.out" 2>&1; then fail "$mode: invalid group response was accepted"; fi
  assert_private_output "$mode"
done
pass 'actual group helper preserves bigint/string group IDs and rejects HTTP, business and invalid-ID responses'

# These stubs are only for unrelated datagen payloads. The real gu_datagen,
# shared friend helpers and gu_send_c2c still run and enforce their order.
require_friends() {
  grep -Fxq 'friend:list:A' "$GOLDEN_TEST_EVENTS" && grep -Fxq 'friend:list:B' "$GOLDEN_TEST_EVENTS" \
    || return 1
  printf '%s\n' "$1" >> "$GOLDEN_TEST_EVENTS"
}
gu_wait_msg() { require_friends history; }
# shellcheck disable=SC2034 # extracted gu_datagen reads the payload result globals
gu_upload_attachment() { require_friends attachment || return 1; UP_OBJ_KEY=fixture-key; UP_OBJ_MD5=fixture-md5; }
# shellcheck disable=SC2034 # extracted gu_datagen reads the group result global
gu_group_create() { require_friends group || return 1; UP_GID=9223372036854775010; }
gu_send_group_msg() { require_friends group-message; }
# shellcheck disable=SC2034,SC2153 # extracted functions consume MSG_COUNT and set UP_UID/UP_TOKEN
upgrade_fixture() {
  new_case "$1" "${2:-}"
  gu_register_login A
  local token_a="$UP_TOKEN" uid_a="$UP_UID"
  gu_register_login B
  local token_b="$UP_TOKEN" uid_b="$UP_UID"
  MSG_COUNT=1
  gu_datagen "$token_a" "$uid_a" "$token_b" "$uid_b"
  SMOKE_C2C_FROM="$uid_a" SMOKE_C2C_TO="$uid_b" run_smoke8
}
(upgrade_fixture upgrade) > "$TMP/upgrade.out" 2>&1 || fail 'Upgrade data fixture failed'
python3 - "$TMP/upgrade" <<'PY'
import json, pathlib, sys
case = pathlib.Path(sys.argv[1])
events = case.joinpath('events').read_text().splitlines()
assert events.index('friend:list:A') < events.index('friend:list:B') < events.index('c2c') < events.index('group') < events.index('smoke8')
state = json.loads(case.joinpath('upgrade_state.json').read_text())
assert state['user_A']['uid'] == '9223372036854775001' and state['user_B']['uid'] == '9223372036854775002'
assert state['c2c_msg_1']['id'] == 'fixture-c2c'
PY
assert_private_output upgrade
pass 'Upgrade records baseline users, establishes mutual friendship before C2C/group, and reuses that pair after upgrade'
for mode in friend_confirm_business reverse_missing c2c_denied; do
  name="upgrade-$mode"
  if (upgrade_fixture "$name" "$mode") > "$TMP/$name.out" 2>&1; then fail "$name: datagen continued after rejection"; fi
  if grep -Eq '^(attachment|group|smoke8)$' "$TMP/$name/events"; then fail "$name: rejected datagen reached later steps"; fi
  assert_private_output "$name"
done
pass 'Upgrade does not continue after incomplete friendship or an E2EE-required CLI rejection'

(
  new_case idempotency
  idem_upload
  [[ "$(cat "$GOLDEN_TEST_EVENTS")" == $'signup\nlogin\nattachment:presign\nattachment:put\nattachment:confirm' ]] \
    || fail 'idempotency upload skipped a real helper transition'
) > "$TMP/idempotency.out" 2>&1 || fail 'idempotency registration/upload fixture failed'
assert_private_output idempotency
pass 'Install idempotency registration uses the same generated code and its authenticated attachment round-trip'

printf 'All %s offline Golden smoke checks passed; no real container, WebSocket or Golden acceptance was executed.\n' "$CHECKS"
