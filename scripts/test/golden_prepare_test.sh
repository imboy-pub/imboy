#!/usr/bin/env bash
# Golden 准备与身份传递离线回归：真实 CLI/公共函数 + 严格 Git/Docker/安装器桩。
# No registry, daemon, Erlang node or release is used. The installer stub fails
# deliberately after checking its inputs; this is not a Golden acceptance run.
# shellcheck disable=SC2030,SC2031 # subshell fixtures deliberately isolate shared helper globals
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin" "$TMP/source/deploy/nginx" "$TMP/source/config" "$TMP/source/scripts"
cp "$ROOT/deploy/.env.example" "$ROOT/deploy/docker-compose.community.yml" "$TMP/source/deploy/"
cp "$ROOT/deploy/nginx/init-letsencrypt.sh" "$TMP/source/deploy/nginx/"
cp "$ROOT/config/vm.args" "$TMP/source/config/"
printf '#!/usr/bin/env bash\nexit 99\n' > "$TMP/source/scripts/imboy_ctl"
CHECKS=0
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
pass() { CHECKS=$((CHECKS + 1)); printf 'PASS %02d: %s\n' "$CHECKS" "$*"; }

# This validator runs at the first pull and again at the real CLI's phase 2.
# It checks values without writing credentials into the event/output logs.
cat > "$TMP/validate.sh" <<'EOF'
read_value() { sed -n "s/^$1=//p" "$GOLDEN_TEST_DEPLOY/.env" | head -1; }
for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
  value="$(read_value "$key")"
  [[ -n "$value" && "$value" != *example.com* ]] || exit 90
  if [ "$GOLDEN_TEST_PROFILE" = ci ]; then
    cert="$GOLDEN_TEST_DEPLOY/data/certbot/conf/live/$value"
    [[ -s "$cert/fullchain.pem" && -s "$cert/privkey.pem" && ! -e "$cert/.imboy-selfsigned" ]] || exit 90
  fi
done
[[ "$(read_value IMBOY_BASE_URL)" == "https://$(read_value API_DOMAIN)" ]] || exit 90
[[ "$(read_value BACKEND_IMAGE)" == "$GOLDEN_TEST_BACKEND" ]] || exit 90
[[ "$(read_value IMBOY_DIRECT_CLEANUP_PG_USER)" == "$IMBOY_DIRECT_CLEANUP_PG_USER" ]] || exit 90
[[ "$IMBOY_DIRECT_CLEANUP_PG_USER" != "$(read_value POSTGRES_USER)" ]] || exit 90
[[ "$(read_value IMBOY_DIRECT_CLEANUP_PG_PASSWORD)" == "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" ]] || exit 90
[[ ${#IMBOY_DIRECT_CLEANUP_PG_PASSWORD} -ge 32 ]] || exit 90
vmargs="$GOLDEN_TEST_DEPLOY/data/backend_priv/golden-vm.args"
[[ "$(awk '$1=="-name" {print $2}' "$vmargs")" == "$IMBOY_CTL_NODE" ]] || exit 90
[[ "$(awk '$1=="-setcookie" {print $2}' "$vmargs")" == "$IMBOY_CTL_COOKIE" ]] || exit 90
[[ ${#IMBOY_CTL_COOKIE} -ge 32 && "$(stat -c %a "$vmargs")" == 600 ]] || exit 90
runtime="$GOLDEN_TEST_DEPLOY/docker-compose.golden-runtime.yml"
grep -Fq 'VMARGS_PATH: /opt/imboy/priv_runtime/golden-vm.args' "$runtime" || exit 90
if grep -q 'IMBOYENV:' "$runtime"; then exit 90; fi
if grep -qE 'IMBOY_VERIFICATION_MASTER_CODE|GOLDEN_SMOKE_CODE' "$runtime" \
  "$GOLDEN_TEST_DEPLOY/.env" "$GOLDEN_TEST_DEPLOY/docker-compose.community.yml"; then exit 90; fi
smoke="$GOLDEN_TEST_DEPLOY/docker-compose.golden-override.yml"
[[ "$GOLDEN_SMOKE_CODE" =~ ^[0-9a-f]{32}$ && "$(stat -c %a "$smoke")" == 600 ]] || exit 90
grep -Fq 'IMBOYENV: dev' "$smoke" || exit 90
grep -Fq "IMBOY_VERIFICATION_MASTER_CODE: \"$GOLDEN_SMOKE_CODE\"" "$smoke" || exit 90
if grep -q 'GOLDEN_SMOKE_CODE' "$smoke"; then exit 90; fi
if [[ -e "$GOLDEN_TEST_EVENTS.code" ]]; then
  [[ "$(cat "$GOLDEN_TEST_EVENTS.code")" == "$GOLDEN_SMOKE_CODE" ]] || exit 90
else
  printf '%s' "$GOLDEN_SMOKE_CODE" > "$GOLDEN_TEST_EVENTS.code"
  chmod 600 "$GOLDEN_TEST_EVENTS.code"
fi
if [ "$GOLDEN_TEST_PROFILE" = ci ]; then
  grep -Fq 'network_mode: none' "$runtime" || exit 90
else
  if grep -q 'network_mode:' "$runtime"; then exit 90; fi
fi
EOF

cat > "$TMP/source/deploy/install.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
if [[ "$*" == --help ]]; then
  if [[ "${GOLDEN_TEST_OLD_INSTALLER:-0}" == 1 ]]; then printf '%s\n' 'old installer'; exit 0; fi
  printf '%s\n' '--compose-file <overlay> --init-cleanup-db'
  exit 0
fi
if [[ ! -f .env ]]; then
  [[ "$*" == '--edition community --yes' ]] || exit 91
  cp .env.example .env
  chmod 600 .env
  printf 'INIT\n' >> "$GOLDEN_TEST_EVENTS"
  exit 0
fi
[[ $# == 10 && "$1" == --edition && "$2" == community && "$3" == --yes \
  && "$4" == --compose-file && "$5" == "$PWD/docker-compose.golden-runtime.yml" \
  && "$6" == --init-cleanup-db && "$7" == --admin-phone && "$9" == --admin-password ]] || exit 91
GOLDEN_TEST_DEPLOY=$PWD
. "$GOLDEN_TEST_VALIDATOR"
printf 'INSTALL_READY\n' >> "$GOLDEN_TEST_EVENTS"
printf 'intentional offline installer stop\n' >&2
exit 73
EOF
cat > "$TMP/bin/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[[ "$1" == clone && "$2" == -q && "$3" == --depth && "$4" == 1 && "$5" == --branch ]] || exit 92
mkdir -p "${@: -1}"
cp -R "$GOLDEN_TEST_SOURCE/." "${@: -1}/"
printf 'CLONE\n' >> "$GOLDEN_TEST_EVENTS"
EOF
cat > "$TMP/source/deploy/init-cleanup-db.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
[[ $# == 0 ]] || exit 95
[[ "$COMPOSE_FILES" == "docker-compose.community.yml $PWD/docker-compose.golden-runtime.yml $PWD/docker-compose.golden-override.yml" ]] || exit 95
printf 'CLEANUP_VERIFY_ONLY\n' >> "$GOLDEN_TEST_EVENTS"
exit "${GOLDEN_TEST_CLEANUP_RC:-0}"
EOF
cat > "$TMP/bin/python3" <<'EOF'
#!/usr/bin/env bash
[[ "$*" == '-c import websockets' ]] || exit 93
EOF
cat > "$TMP/bin/docker" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
case "$*" in
  info|'ps -a --format {{.Names}}'|'volume ls -q'|'network ls --format {{.Name}}'|'compose version') exit 0 ;;
  'inspect -f '*|'inspect --format '*) printf 'offline mock\n'; exit 0 ;;
esac
[[ "$1" == compose ]] || exit 94
shift
files=()
while [[ "${1:-}" == -f ]]; do files+=("$2"); shift 2; done
[[ ${#files[@]} == 2 && "${files[0]}" == */docker-compose.community.yml \
  && "${files[1]}" == */docker-compose.golden-runtime.yml ]] || exit 94
GOLDEN_TEST_DEPLOY="$(dirname "${files[0]}")"
case "${1:-}" in
  pull)
    . "$GOLDEN_TEST_VALIDATOR"
    printf 'PULL_READY\n' >> "$GOLDEN_TEST_EVENTS" ;;
  ps|logs) : ;; # failure-log collection only
  up) [[ "$*" == 'up -d imboy_nginx' || "$*" == 'up -d imboy_certbot' ]] || exit 94 ;;
  exec)
    if [[ "${GOLDEN_TEST_WRAPPER:-}" == ctl ]]; then
      [[ $# == 13 && "$2" == -T && "$3" == -e && "$4" == IMBOY_CTL_NODE \
        && "$5" == -e && "$6" == IMBOY_CTL_COOKIE && "$7" == imboy_backend \
        && "$8" == sh && "$9" == -c && "${11}" == golden && "${12}" == node && "${13}" == status ]] || exit 94
      [[ "$IMBOY_CTL_COOKIE" == "$(awk '$1=="-setcookie" {print $2}' "$GOLDEN_TEST_DEPLOY/data/backend_priv/golden-vm.args")" ]] || exit 94
      printf 'CTL_READY\n' >> "$GOLDEN_TEST_EVENTS"
    elif [[ "${GOLDEN_TEST_WRAPPER:-}" == psql ]]; then
      [[ "$*" == 'exec -T -e PGPASSWORD -e PGUSER -e PGDATABASE imboy_pg18 psql -At -c SELECT 1' ]] || exit 94
      printf 'PSQL_READY\n' >> "$GOLDEN_TEST_EVENTS"
    elif [[ "$*" == 'exec -T imboy_nginx wget '* ]]; then
      domain=""; url=""
      for arg in "$@"; do
        case "$arg" in --header=Host:\ *) domain="${arg#--header=Host: }" ;; http://127.0.0.1/*) url="$arg" ;; esac
      done
      [[ -n "$domain" && -n "$url" ]] || exit 94
      printf 'HTTP01 %s\n' "$domain" >> "$GOLDEN_TEST_EVENTS"
      cat "$GOLDEN_TEST_DEPLOY/data/certbot/www/${url#http://127.0.0.1/}"
    elif [[ "$*" == 'exec -T imboy_nginx nginx -t' || "$*" == 'exec -T imboy_nginx nginx -s reload' ]]; then
      printf 'NGINX_CHECK\n' >> "$GOLDEN_TEST_EVENTS"
    else exit 94; fi ;;
  *) printf 'unexpected Docker operation (blocked)\n' >&2; exit 94 ;;
esac
EOF
chmod +x "$TMP/bin/"* "$TMP/source/deploy/install.sh"
export GOLDEN_TEST_SOURCE="$TMP/source" GOLDEN_TEST_VALIDATOR="$TMP/validate.sh"
export PATH="$TMP/bin:$PATH"
export GOLDEN_HOSTS_FILE="$TMP/hosts" GOLDEN_MIN_MEM_GB=0 GOLDEN_MIN_DISK_GB=0
touch "$GOLDEN_HOSTS_FILE"
unset IMBOY_CTL_NODE IMBOY_CTL_COOKIE IMBOY_DIRECT_CLEANUP_PG_USER IMBOY_DIRECT_CLEANUP_PG_PASSWORD
digest="sha256:$(printf '%064d' 1)"
backend="ghcr.io/imboy-pub/imboy-backend@$digest"
RUN_ENV=()

run_cli() { # <name> <install|upgrade> <profile> <expected stage> [CLI options]
  local name="$1" kind="$2" profile="$3" stage="$4" rc=0
  shift 4
  WORK="$TMP/$name"
  export GOLDEN_TEST_EVENTS="$WORK.events" GOLDEN_TEST_PROFILE="$profile" GOLDEN_TEST_BACKEND="$backend"
  : > "$GOLDEN_TEST_EVENTS"
  local args=(--image-ref "$backend" --git-ref v1.0.0-alpha.84)
  if [ "$kind" = upgrade ]; then
    args=(--to-image "$backend" --to-ref v1.0.0-alpha.84 --from v1.2.3)
    GOLDEN_TEST_BACKEND=ghcr.io/imboy-pub/imboy-backend:1.2.3
  fi
  env "${RUN_ENV[@]}" bash "$ROOT/scripts/golden_$kind.sh" "${args[@]}" \
    --profile "$profile" --admin-phone 13800138000 --admin-password CI_Only_Test_1234 \
    --workdir "$WORK" "$@" > "$WORK.out" 2>&1 || rc=$?
  [[ "$rc" == 1 ]] || fail "$name: expected deliberate failure, rc=$rc"
  grep -Fq "STAGE=$stage " "$WORK.out" || fail "$name: wrong failure stage"
  if [[ "$stage" == install || "$stage" == install-vn ]]; then
    [[ "$(cat "$GOLDEN_TEST_EVENTS")" == $'CLONE\nINIT\nPULL_READY\nINSTALL_READY' ]] \
      || fail "$name: preparation/pull/install order or inputs are wrong"
    local cookie password smoke_code
    cookie="$(awk '$1=="-setcookie" {print $2}' "$WORK/imboy/deploy/data/backend_priv/golden-vm.args")"
    password="$(sed -n 's/^IMBOY_DIRECT_CLEANUP_PG_PASSWORD=//p' "$WORK/imboy/deploy/.env")"
    smoke_code="$(cat "$GOLDEN_TEST_EVENTS.code")"
    if grep -Fq "$cookie" "$WORK.out" "$GOLDEN_TEST_EVENTS" "$WORK/imboy/scripts/imboy_ctl" \
      "$WORK/imboy/deploy/docker-compose.golden-runtime.yml"; then fail "$name: cookie leaked"; fi
    if grep -Fq "$password" "$WORK.out" "$GOLDEN_TEST_EVENTS"; then fail "$name: cleanup password leaked"; fi
    if grep -Fq "$smoke_code" "$WORK.out" "$GOLDEN_TEST_EVENTS"; then fail "$name: smoke code leaked"; fi
    run_dirs=("$WORK"/run-*)
    [[ "$(stat -c %a "${run_dirs[0]}")" == 700 ]] || fail "$name: response/state directory is not private"
  elif grep -Eq 'PULL_READY|INSTALL_READY' "$GOLDEN_TEST_EVENTS"; then
    fail "$name: invalid input reached pull/install"
  fi
}

run_cli install install ci install
INSTALL_WORK=$WORK
pass 'Install prepares all five domains, credentials and shared runtime identity before pull'
run_cli upgrade upgrade ci install-vn
pass 'Upgrade uses the same preparation and controlled account initialization'
first_cookie="$(awk '$1=="-setcookie" {print $2}' "$INSTALL_WORK/imboy/deploy/data/backend_priv/golden-vm.args")"
second_cookie="$(awk '$1=="-setcookie" {print $2}' "$WORK/imboy/deploy/data/backend_priv/golden-vm.args")"
[[ "$first_cookie" != "$second_cookie" ]] || fail 'independent runs reused a cookie'
pass 'separate cleanrooms get separate cookies without putting them in logs or wrappers'
[[ "$(cat "$INSTALL_WORK.events.code")" != "$(cat "$WORK.events.code")" ]] || fail 'independent runs reused a smoke code'
pass 'smoke code is unique per cleanroom, exported consistently, absent from production overlays and logs'

for kind in install upgrade; do
  run_cli "missing-host-$kind" "$kind" host arg \
    --api-domain api.test.invalid --admin-domain admin.test.invalid --certbot-email cert@test.invalid
  [[ ! -s "$GOLDEN_TEST_EVENTS" ]] || fail 'missing host fields reached clone'
done
pass 'both host CLIs reject missing Widget/RTC/TURN domains before clone'
run_cli duplicate install ci arg --rtc-domain api.golden-ci.imboy.internal
run_cli path-domain install ci arg --cs-widget-domain=../bad
pass 'duplicate or path-like domains fail before filesystem certificate preparation'

RUN_ENV=(IMBOY_DIRECT_CLEANUP_PG_USER=imboy_user)
run_cli bad-user install ci env-init
RUN_ENV=(IMBOY_DIRECT_CLEANUP_PG_PASSWORD=too_short)
run_cli bad-password install ci env-init
# shellcheck disable=SC2016 # literal dollar sign is invalid fixture input, never expanded
RUN_ENV=('IMBOY_DIRECT_CLEANUP_PG_PASSWORD=Fixture_Cleanup_12345678901234567890$literal')
run_cli nonliteral-password install ci env-init
RUN_ENV=(IMBOY_DIRECT_CLEANUP_PG_PASSWORD=CHANGE_ME_Cleanup_12345678901234567890)
run_cli placeholder-password install ci env-init
RUN_ENV=(IMBOY_CTL_COOKIE=too_short)
run_cli bad-cookie install ci env-init
pass 'invalid cleanup identity, weak credentials and invalid cookie fail before pull'
RUN_ENV=(GOLDEN_TEST_OLD_INSTALLER=1)
run_cli unsupported-baseline upgrade ci env-init
pass 'unsupported baseline installer is rejected without changing its source or skipping its gates'

RUN_ENV=(IMBOY_DIRECT_CLEANUP_PG_USER=preserved_worker
  IMBOY_DIRECT_CLEANUP_PG_PASSWORD=Fixture_Cleanup_Only_12345678901234567890
  IMBOY_CTL_NODE=preserved_node@127.0.0.1 IMBOY_CTL_COOKIE=Fixture_Cookie_Only_12345678901234567890)
run_cli supplied install host install --api-domain=api.test.invalid --admin-domain=admin.test.invalid \
  --cs-widget-domain=cs.test.invalid --rtc-domain rtc.test.invalid --turn-domain=turn.test.invalid \
  --certbot-email=cert@test.invalid
grep -Fxq 'IMBOY_DIRECT_CLEANUP_PG_USER=preserved_worker' "$WORK/imboy/deploy/.env" || fail 'supplied user lost'
grep -Fxq 'IMBOY_DIRECT_CLEANUP_PG_PASSWORD=Fixture_Cleanup_Only_12345678901234567890' "$WORK/imboy/deploy/.env" || fail 'supplied password lost'
grep -Fxq -- '-setcookie Fixture_Cookie_Only_12345678901234567890' "$WORK/imboy/deploy/data/backend_priv/golden-vm.args" || fail 'supplied cookie lost'
[[ ! -d "$WORK/imboy/deploy/data/certbot" ]] || fail 'host mode generated CI certificates'
RUN_ENV=()
pass 'both option forms preserve provided domains and credentials; host gets no CI certificate fixture'

# Execute generated wrappers: bash -n alone cannot catch a stray doubled slash.
(
  export IMBOY_CTL_NODE=imboy@127.0.0.1 IMBOY_CTL_COOKIE="$first_cookie"
  export GOLDEN_TEST_EVENTS="$TMP/wrappers.events" GOLDEN_TEST_WRAPPER=ctl
  : > "$GOLDEN_TEST_EVENTS"
  bash "$INSTALL_WORK/imboy/scripts/imboy_ctl" node status
  [[ "$(cat "$GOLDEN_TEST_EVENTS")" == CTL_READY ]] || fail 'CLI wrapper did not make exactly one correct Docker call'
  export GOLDEN_TEST_WRAPPER=psql
  psql_wrappers=("$INSTALL_WORK"/run-*/bin/psql)
  bash "${psql_wrappers[0]}" -At -c 'SELECT 1'
  [[ "$(tail -1 "$GOLDEN_TEST_EVENTS")" == PSQL_READY ]] || fail 'psql wrapper lost runtime overlay or arguments'
  : > "$GOLDEN_TEST_EVENTS"
  unset IMBOY_CTL_COOKIE
  if bash "$INSTALL_WORK/imboy/scripts/imboy_ctl" node status > "$TMP/wrapper-missing.out" 2>&1; then fail 'missing cookie accepted'; fi
  [[ ! -s "$GOLDEN_TEST_EVENTS" ]] || fail 'missing cookie reached Docker'
)
pass 'generated wrappers execute one correct command and missing identity stops before Docker'

(
  # shellcheck source=scripts/golden_common.sh
  . "$ROOT/scripts/golden_common.sh"
  CLONE_DIR="$INSTALL_WORK/imboy"; DEPLOY_DIR="$CLONE_DIR/deploy"
  RUNTIME_YML="$DEPLOY_DIR/docker-compose.golden-runtime.yml"
  OVERRIDE_YML="$DEPLOY_DIR/docker-compose.golden-override.yml"
  COMMUNITY_YML="$DEPLOY_DIR/docker-compose.community.yml"
  PROFILE=ci
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
    printf -v "$key" '%s' "$(env_get_var "$key")"
  done
  IMBOY_CTL_COOKIE="$first_cookie"
  # Model a different immutable target's VM flags; keep the same run's identity.
  printf '\n+S 4\n' >> "$CLONE_DIR/config/vm.args"
  prepare_golden_control
  [[ "$(awk '$1=="-setcookie" {print $2}' "$DEPLOY_DIR/data/backend_priv/golden-vm.args")" == "$first_cookie" ]] || fail 'upgrade rotated cookie'
  grep -Fxq '+S 4' "$DEPLOY_DIR/data/backend_priv/golden-vm.args" || fail 'target VM flags were not refreshed'
  env_before="$(sha256sum "$DEPLOY_DIR/.env")"
  prepare_golden_cleanup
  [[ "$(sha256sum "$DEPLOY_DIR/.env")" == "$env_before" ]] || fail 'repeated prep overwrote existing credentials'
  export GOLDEN_TEST_EVENTS="$TMP/cleanup-verify.events"
  : > "$GOLDEN_TEST_EVENTS"
  verify_golden_cleanup
  [[ "$(cat "$GOLDEN_TEST_EVENTS")" == CLEANUP_VERIFY_ONLY ]] || fail 'upgrade cleanup verification changed the helper contract'
  if GOLDEN_TEST_CLEANUP_RC=27 verify_golden_cleanup; then fail 'cleanup verification error was ignored'; fi

  env_before="$(sha256sum "$DEPLOY_DIR/.env")"
  if (env_set_var ADMIN_IMAGE $'one\ntwo') > "$TMP/env-newline.out" 2>&1; then fail 'newline assignment accepted'; fi
  if (env_set_var ADMIN_IMAGE 'unsupported;literal') > "$TMP/env-syntax.out" 2>&1; then fail 'shell syntax assignment accepted'; fi
  [[ "$(sha256sum "$DEPLOY_DIR/.env")" == "$env_before" ]] || fail 'rejected literal changed configuration'

  certificate="$DEPLOY_DIR/data/certbot/conf/live/$API_DOMAIN/fullchain.pem"
  before="$(sha256sum "$certificate")"
  preset_ci_certs >/dev/null
  [[ "$(sha256sum "$certificate")" == "$before" ]] || fail 'existing fixture replaced'
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
    openssl x509 -in "$DEPLOY_DIR/data/certbot/conf/live/${!key}/fullchain.pem" -checkend 0 -noout >/dev/null
  done
  export GOLDEN_TEST_EVENTS="$TMP/tls.events"
  : > "$GOLDEN_TEST_EVENTS"
  COMPOSE_FILES="$COMMUNITY_YML $RUNTIME_YML" bash "$DEPLOY_DIR/nginx/init-letsencrypt.sh" > "$TMP/tls.out" 2>&1
  [[ "$(grep -c '^HTTP01 ' "$GOLDEN_TEST_EVENTS")" == 5 ]] || fail 'real TLS bootstrap did not probe all five domains'
  [[ "$(grep -c '^NGINX_CHECK$' "$GOLDEN_TEST_EVENTS")" == 3 ]] || fail 'nginx validation/reload was lost'

  touch "$DEPLOY_DIR/data/certbot/conf/live/$API_DOMAIN/.imboy-selfsigned"
  if (preset_ci_certs) > "$TMP/cert-state.out" 2>&1; then fail 'production bootstrap marker overwritten'; fi
  rm "$DEPLOY_DIR/data/certbot/conf/live/$API_DOMAIN/.imboy-selfsigned"
  rm "$DEPLOY_DIR/data/certbot/conf/live/$API_DOMAIN/privkey.pem"
  if (preset_ci_certs) > "$TMP/cert-partial.out" 2>&1; then fail 'partial fixture overwritten'; fi
  [[ "$(sha256sum "$certificate")" == "$before" ]] || fail 'partial certificate failure modified original'
  rm "$certificate"
  env_set_var UPTRACE_ENABLED true
  env_set_var UPTRACE_DOMAIN trace.test.invalid
  preset_ci_certs >/dev/null
  [[ -s "$DEPLOY_DIR/data/certbot/conf/live/trace.test.invalid/privkey.pem" ]] || fail 'optional Uptrace certificate missing'
  PROFILE=host
  if (preset_ci_certs) > "$TMP/cert-host.out" 2>&1; then fail 'host accepted CI cert preparation'; fi
)
pass 'repeat prep preserves credentials; target VM flags refresh while the run cookie stays fixed'
pass 'upgrade cleanup recheck preserves the canonical base, both overlays and verify-only failure semantics'
pass 'unsupported .env bytes fail without modifying the prior configuration'
pass 'five valid CI certificates retain HTTP-01/nginx checks without invoking certonly'
pass 'partial/production certificate state fails closed; optional Uptrace gets its certificate'

(
  # shellcheck source=scripts/golden_common.sh
  . "$ROOT/scripts/golden_common.sh"
  INSTALL_LOG="$TMP/timing.log"; PROFILE=ci
  printf '%s\n' '100 创建网络并启动服务' '110 校验并签发 TLS 证书' '115 等待后端健康' > "$INSTALL_LOG"
  parse_install_stages 130
  [[ "$SEC_STACK" == 10 && "$SEC_TLS" == 5 && "$SEC_INITTAIL" == 15 && "$TLS_NOTE" == *未验证公网* ]] || fail 'current TLS timing anchors drifted'
)
pass 'current installer timing includes local TLS validation and does not claim public issuance'

printf 'All %s offline Golden preparation checks passed; no image build or Golden acceptance was executed.\n' "$CHECKS"
