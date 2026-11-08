#!/usr/bin/env bash
# Offline configuration contracts; no Docker, certificate issuance or real TLS.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
BASE="$ROOT/deploy/docker-compose.community.yml"
TURN="$ROOT/deploy/docker-compose.livekit-turn.yml"
for tool in awk envsubst mktemp cp cmp readlink; do
  command -v "$tool" >/dev/null || { printf '[FAIL] Missing tool: %s\n' "$tool" >&2; exit 1; }
done
TMP="$(mktemp -d)"
trap 'rm -rf -- "$TMP"' EXIT
passed=0
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
pass() { passed=$((passed + 1)); printf '[PASS] %s\n' "$*"; }

# Read the existing two-space service/four-space short-bind format, not comments.
service_block() {
  awk -v service="$1" '
    $0 == "  " service ":" { found=1; next }
    found && /^  [A-Za-z0-9_]+:/ { exit }
    found { print }
  ' "$2"
}
mounts() {
  service_block "$1" "$2" | awk '
    /^    volumes:/ { found=1; next }
    found && /^    [A-Za-z0-9_]+:/ { exit }
    found && /^      - / { sub(/^      - /, ""); print }
  '
}
backend="$(service_block imboy_backend "$BASE")"
mapfile -t backend_mounts < <(mounts imboy_backend "$BASE")
rsa_source= log_source=
for spec in "${backend_mounts[@]}"; do
  case "$spec" in
    *:/opt/imboy/priv_runtime:ro) rsa_source="${spec%:/opt/imboy/priv_runtime:ro}" ;;
    *:/opt/imboy/log|*:/opt/imboy/log:rw) log_source="${spec%:rw}"; log_source="${log_source%:/opt/imboy/log}" ;;
  esac
done
rsa_source="${rsa_source//'${DATA_DIR:-./data}'/$TMP}"
log_source="${log_source//'${DATA_DIR:-./data}'/$TMP}"
[[ "$rsa_source" = "$TMP/backend_priv" ]] || fail 'Installer RSA directory is not mounted read-only'
[[ "$backend" == *'/opt/imboy/priv_runtime/keys/login_rsa_priv.pem'* ]] || fail 'RSA config does not use the runtime mount'
pass 'Installer RSA directory backs the read-only backend runtime mount'
[[ "$log_source" = "$TMP/backend_log" ]] || fail 'Backend logs do not have a writable persistent directory'
pass 'Backend application logs have a writable persistent mount'
[[ "$backend" == *'IMBOY_GARAGE_PUBLIC_BUCKET:'*'${IMBOY_GARAGE_PUBLIC_BUCKET'* ]] || fail 'Public bucket override is not passed to backend'
pass 'Public bucket setting reaches the backend'

nginx="$(service_block imboy_nginx "$BASE")"
[[ "$nginx" == *'RTC_DOMAIN: ${RTC_DOMAIN}'* ]] || fail 'Nginx does not receive RTC_DOMAIN'
# Official hooks run only for nginx/nginx-debug; a shell wrapper must re-enter
# the entrypoint with nginx. The companion --docker test exercises that command.
nginx_command="$(printf '%s\n' "$nginx" | awk '/^    command:/ { print }')"
[[ "$nginx_command" == *'command: ["nginx"'* || "$nginx_command" == *'exec /docker-entrypoint.sh nginx '* ]] \
  || fail 'Nginx startup bypasses the official template-rendering entrypoint'
pass 'Nginx startup enters the official template-rendering entrypoint'
mapfile -t nginx_mounts < <(mounts imboy_nginx "$BASE")
declare -A templates=()
for spec in "${nginx_mounts[@]}"; do
  mode="${spec##*:}"; pair="${spec%:*}"
  target="${pair##*:}"; source="${pair%:*}"
  case "$target" in
    /etc/nginx/templates/*)
      [[ "$mode" = ro && -f "$ROOT/deploy/$source" ]] || fail 'Nginx template bind is not a read-only file'
      templates["${target##*/}"]="$ROOT/deploy/$source" ;;
    /etc/nginx/templates) fail 'Nginx must not load every file in the shared host templates directory' ;;
  esac
done
[[ "${#templates[@]}" -eq 3 && -n "${templates[imboy.conf.template]:-}" && -n "${templates[cs-widget.conf.template]:-}" && -n "${templates[rtc.conf.template]:-}" ]] \
  || fail 'Expected exactly the API/admin, Widget and RTC HTTP templates'
[[ "${templates[rtc.conf.template]}" != *'/nginx/templates/'* ]] || fail 'Community RTC template must stay outside the shared business template directory'
pass 'Community Nginx mounts exactly three HTTP templates without shared stream files'

export API_DOMAIN=api.example.test ADMIN_DOMAIN=admin.example.test CS_WIDGET_DOMAIN=widget.example.test
export RTC_DOMAIN=rtc.example.test S3_UPSTREAM=garage:3900 UPTRACE_DOMAIN=trace.example.test
# Sentinels detect accidentally substituting Nginx built-in variables.
export host=SHOULD_NOT_REPLACE remote_addr=SHOULD_NOT_REPLACE http_upgrade=SHOULD_NOT_REPLACE
export connection_upgrade=SHOULD_NOT_REPLACE scheme=SHOULD_NOT_REPLACE
export request_uri=SHOULD_NOT_REPLACE http_host=SHOULD_NOT_REPLACE proxy_add_x_forwarded_for=SHOULD_NOT_REPLACE
for flavor in base uptrace; do
  filter_file="$BASE"
  [[ "$flavor" != uptrace ]] || filter_file="$ROOT/deploy/docker-compose.uptrace.yml"
  filter="$(service_block imboy_nginx "$filter_file" | awk '/^[[:space:]]+NGINX_ENVSUBST_FILTER:/ { sub(/^[^:]+:[[:space:]]*/, ""); gsub(/"/, ""); print }')"
  filter="${filter//'$$'/'$'}" # Compose converts $$ to $ before container entrypoint.
  [[ -n "$filter" ]] || fail 'Nginx substitution filter is missing'
  substitutions=
  for name in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN S3_UPSTREAM UPTRACE_DOMAIN host remote_addr http_upgrade connection_upgrade scheme request_uri http_host proxy_add_x_forwarded_for; do
    [[ "$name" =~ $filter ]] && substitutions+="\${$name} "
  done
  for name in "${!templates[@]}"; do
    envsubst "$substitutions" < "${templates[$name]}" > "$TMP/$name"
    rendered="$(cat "$TMP/$name")"
    [[ "$rendered" != *'${'*'_DOMAIN}'* && "$rendered" != *'${S3_UPSTREAM}'* ]] || fail "$flavor filter left template variables unresolved: $name"
    [[ "$rendered" != *SHOULD_NOT_REPLACE* ]] || fail 'Nginx built-ins were substituted'
  done
  if [[ "$flavor" = uptrace ]]; then
    envsubst "$substitutions" < "$ROOT/deploy/uptrace/nginx.conf.template" > "$TMP/uptrace.conf"
    [[ "$(cat "$TMP/uptrace.conf")" != *'${UPTRACE_DOMAIN}'* ]] || fail 'Uptrace domain is not substituted'
  fi
  pass "$flavor filter renders all supplied HTTP templates while preserving Nginx variables"
done
rendered="$(cat "$TMP/rtc.conf.template")"
[[ "$rendered" == *'server_name rtc.example.test;'* && "$rendered" == *'/etc/letsencrypt/live/rtc.example.test/fullchain.pem;'* ]] || fail 'RTC host/certificate does not match the domain'
[[ "$rendered" == *'proxy_pass http://imboy_livekit:7880;'* ]] || fail 'RTC origin does not reach LiveKit'
[[ "$rendered" == *'Upgrade $http_upgrade;'* && "$rendered" == *'Connection $connection_upgrade;'* ]] || fail 'RTC proxy lacks WebSocket upgrade headers'
[[ "$rendered" == *'proxy_http_version 1.1;'* && "$rendered" == *'ssl_protocols TLSv1.2 TLSv1.3;'* ]] \
  || fail 'RTC signaling must use HTTP/1.1 upgrades over TLS 1.2/1.3'
pass 'RTC signaling uses its domain/certificate, LiveKit upstream and WebSocket upgrade headers'
livekit="$(service_block imboy_livekit "$BASE")"
[[ "$livekit" == *'"127.0.0.1:7880:7880"'* && "$livekit" != *'network_mode: host'* && "$nginx" != *'network_mode: host'* ]] \
  || fail 'LiveKit signaling must keep its Compose service route and loopback-only host publish'
pass 'Nginx reaches LiveKit inside Compose; host signaling stays on loopback'

domain=turn.example.test
data="$TMP/host data"
live="$data/certbot/conf/live/$domain"
archive="$data/certbot/conf/archive/$domain"
mkdir -p "$live" "$archive"
printf 'synthetic certificate generation 1\n' > "$archive/fullchain1.pem"
printf 'synthetic private fixture generation 1\n' > "$archive/privkey1.pem"
ln -s "../../archive/$domain/fullchain1.pem" "$live/fullchain.pem"
ln -s "../../archive/$domain/privkey1.pem" "$live/privkey.pem"
cert_path="$(awk '$1 == "cert_file:" { print $2 }' "$TURN")"
key_path="$(awk '$1 == "key_file:" { print $2 }' "$TURN")"
cert_path="${cert_path//'${TURN_DOMAIN}'/$domain}"
key_path="${key_path//'${TURN_DOMAIN}'/$domain}"
[[ "$cert_path" = /* && "$key_path" = /* ]] || fail 'TURN certificate paths are not absolute'
mapfile -t turn_mounts < <(mounts imboy_livekit "$TURN")
[[ "${#turn_mounts[@]}" -eq 2 ]] || fail 'Expected the TURN domain live and archive binds'

# Materialize only the actual declared mounts in a temporary filesystem. This
# verifies relative symlink resolution, not Docker mount execution or TLS parsing.
materialize_mounts() {
  local dest_root="$1" cert_source="$2" spec pair source target mode saw_custom=0
  for spec in "${turn_mounts[@]}"; do
    mode="${spec##*:}"; pair="${spec%:*}"
    target="${pair##*:}"; source="${pair%:*}"
    [[ "$mode" = ro ]] || fail 'TURN certificate mounts must be read-only'
    target="${target//'${TURN_DOMAIN}'/$domain}"
    case "$source" in
      '${LIVEKIT_TURN_CERT_DIR:'*'}') source="$cert_source"; saw_custom=1 ;;
      *) source="${source//'${DATA_DIR:-./data}'/$data}"
         source="${source//'${TURN_DOMAIN}'/$domain}" ;;
    esac
    [[ "$source" != *'$'* && "$target" != *'$'* && "$target" = /* ]] || fail 'Unresolved TURN mount path'
    [[ "$target" == "/etc/letsencrypt/live/$domain" || "$target" == "/etc/letsencrypt/archive/$domain" ]] \
      || fail 'TURN binds must be scoped to this domain'
    mkdir -p "$dest_root$target"
    [[ ! -d "$source" ]] || cp -a "$source/." "$dest_root$target/"
  done
  [[ "$saw_custom" -eq 1 ]] || fail 'LIVEKIT_TURN_CERT_DIR compatibility was removed'
}
materialize_mounts "$TMP/certbot" "$live"
cmp "$TMP/certbot$cert_path" "$archive/fullchain1.pem" || fail 'Certbot certificate link does not resolve'
cmp "$TMP/certbot$key_path" "$archive/privkey1.pem" || fail 'Certbot private-key link does not resolve'
[[ "$(readlink -f "$TMP/certbot$cert_path")" == "$TMP/certbot/etc/letsencrypt/archive/$domain/fullchain1.pem" ]] \
  || fail 'Certbot link resolved outside the TURN archive'
pass 'Declared TURN binds resolve Certbot live/archive links for one domain'

printf 'synthetic certificate generation 2\n' > "$archive/fullchain2.pem"
printf 'synthetic private fixture generation 2\n' > "$archive/privkey2.pem"
ln -sfn "../../archive/$domain/fullchain2.pem" "$live/fullchain.pem"
ln -sfn "../../archive/$domain/privkey2.pem" "$live/privkey.pem"
materialize_mounts "$TMP/renewed" "$live"
cmp "$TMP/renewed$cert_path" "$archive/fullchain2.pem" || fail 'Renewed certificate link does not resolve'
cmp "$TMP/renewed$key_path" "$archive/privkey2.pem" || fail 'Renewed key link does not resolve'
pass 'Renewed archive filenames resolve without changing mount/config paths'

mkdir -p "$TMP/custom flat"
printf 'synthetic external certificate\n' > "$TMP/custom flat/fullchain.pem"
printf 'synthetic external private fixture\n' > "$TMP/custom flat/privkey.pem"
data="$TMP/no certbot tree" # Flat files must not depend on a populated archive.
materialize_mounts "$TMP/flat" "$TMP/custom flat"
cmp "$TMP/flat$cert_path" "$TMP/custom flat/fullchain.pem" || fail 'Custom flat certificate path broke'
cmp "$TMP/flat$key_path" "$TMP/custom flat/privkey.pem" || fail 'Custom flat key path broke'
pass 'Existing external flat certificate directory remains usable'
printf '\n%s PASS, 0 FAIL (offline configuration fixtures only)\n' "$passed"
