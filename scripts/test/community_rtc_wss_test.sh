#!/usr/bin/env bash
# Real-container signaling regression; never contacts a deployed IMBoy service.
# LiveKit is real; API/Admin/Widget/S3 are HTTP fixtures. No media/TURN acceptance.
set -euo pipefail
usage() {
  cat <<'USAGE'
Usage: bash scripts/test/community_rtc_wss_test.sh --docker
Requires local Linux Docker/Compose v2, jq, curl and OpenSSL.
Uses disposable containers/network, loopback ports and synthetic TLS/JWTs.
Exit 0 = passed; 1 = failed; 77 = required test environment unavailable.
No public DNS, ACME issuance, production credentials or two-device media tests.
USAGE
}
case "${1:-}" in
  --docker) [[ $# -eq 1 ]] || { usage >&2; exit 1; } ;;
  -h|--help) usage; exit 0 ;;
  *) usage >&2; exit 1 ;;
esac
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
pass() { printf '[PASS] %s\n' "$*"; }
skip() { printf '[SKIP] %s; container/TLS/WSS results are NOT verified.\n' "$*"; exit 77; }
for tool in docker jq curl openssl mktemp awk sed tr grep cp chmod; do
  command -v "$tool" >/dev/null 2>&1 || skip "Missing test tool: $tool"
done
# Bind local fixtures only; never silently use an inherited remote Docker daemon.
DOCKER=(env -u DOCKER_HOST -u DOCKER_CONTEXT -u DOCKER_TLS -u DOCKER_TLS_VERIFY -u DOCKER_CERT_PATH
  docker --host=unix:///var/run/docker.sock)
"${DOCKER[@]}" info >/dev/null 2>&1 || skip 'Local Docker daemon is unavailable'
"${DOCKER[@]}" compose version >/dev/null 2>&1 || skip 'Docker Compose v2 is unavailable'
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
BASE="$ROOT/deploy/docker-compose.community.yml"
umask 077
TMP="$(mktemp -d "${TMPDIR:-/tmp}/imboy-rtc-wss.XXXXXXXX")"
NETWORK="imboy-rtc-wss-${TMP##*.}"
network_created=false
cleanup() {
  local status=$? id file
  if [[ "$status" -ne 0 ]]; then
    for file in "$TMP/nginx.cid" "$TMP/livekit.cid"; do
      if [[ -s "$file" ]]; then
        read -r id < "$file"
        "${DOCKER[@]}" logs --tail 30 "$id" >&2 || true
      fi
    done
  fi
  for file in "$TMP/nginx.cid" "$TMP/livekit.cid" "$TMP/upstream.cid"; do
    if [[ -s "$file" ]]; then
      read -r id < "$file"
      "${DOCKER[@]}" rm -f "$id" >/dev/null 2>&1 || true
    fi
  done
  if "$network_created"; then "${DOCKER[@]}" network rm "$NETWORK" >/dev/null 2>&1 || true; fi
  rm -rf -- "$TMP"
  return "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# Resolve the actual model using .env.example and a clean process environment.
# Customer .env files and exported product/secret overrides are not read.
secret="$(openssl rand -hex 32)"
COMPOSE_ENV=(env -i "PATH=$PATH")
for name in HOME DOCKER_CONFIG HTTP_PROXY HTTPS_PROXY NO_PROXY http_proxy https_proxy no_proxy SSL_CERT_FILE; do
  [[ ! -v "$name" ]] || COMPOSE_ENV+=("$name=${!name}")
done
COMPOSE_ENV+=("API_DOMAIN=api.test.invalid" "ADMIN_DOMAIN=admin.test.invalid"
  "CS_WIDGET_DOMAIN=cs.test.invalid" "RTC_DOMAIN=rtc.test.invalid" "TURN_DOMAIN=turn.test.invalid"
  "DATA_DIR=$TMP/data" "LIVEKIT_API_KEY=rtc_test_key" "LIVEKIT_API_SECRET=$secret"
  "IMBOY_DIRECT_CLEANUP_PG_USER=rtc_test_cleanup" "IMBOY_DIRECT_CLEANUP_PG_PASSWORD=rtc_test_unused"
  "S3_UPSTREAM=garage:3900" "COMPOSE_DISABLE_ENV_FILE=1")
"${COMPOSE_ENV[@]}" docker --host=unix:///var/run/docker.sock compose \
  --env-file "$ROOT/deploy/.env.example" -f "$BASE" config --format json > "$TMP/model.json"
nginx_image="$(jq -er '.services.imboy_nginx.image' "$TMP/model.json")"
livekit_image="$(jq -er '.services.imboy_livekit.image' "$TMP/model.json")"
[[ "$nginx_image" == *@sha256:* && "$livekit_image" == *@sha256:* ]] || fail 'Images must retain repository digest pins'
jq -e '.services.imboy_nginx.entrypoint == null' "$TMP/model.json" >/dev/null \
  || fail 'Nginx must retain the official image entrypoint'
jq -e '.services.imboy_nginx.command | type == "array" and length > 0' "$TMP/model.json" >/dev/null \
  || fail 'Expected the resolved Nginx command array'
mapfile -d '' -t nginx_command < <(jq -jr '.services.imboy_nginx.command[] | ., "\u0000"' "$TMP/model.json")
jq -r '.services.imboy_nginx.environment | to_entries[] | "\(.key)=\(.value)"' "$TMP/model.json" > "$TMP/nginx.env"
# Disable only external-IP discovery for this isolated LiveKit. No STUN, TURN
# or media ports are exposed. All production files and port contracts stay intact.
jq -er '.services.imboy_livekit.environment.LIVEKIT_CONFIG' "$TMP/model.json" \
  | sed 's/use_external_ip: true/use_external_ip: false/' > "$TMP/livekit.yaml"

mkdir -p "$TMP/certs/live" "$TMP/www/.well-known/acme-challenge" "$TMP/nginx_log"
openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
  -keyout "$TMP/test.key" -out "$TMP/test.crt" -subj '/CN=rtc.test.invalid' \
  -addext 'subjectAltName=DNS:api.test.invalid,DNS:admin.test.invalid,DNS:cs.test.invalid,DNS:rtc.test.invalid' \
  >/dev/null 2>&1
for domain in api.test.invalid admin.test.invalid cs.test.invalid rtc.test.invalid; do
  mkdir -p "$TMP/certs/live/$domain"
  cp "$TMP/test.crt" "$TMP/certs/live/$domain/fullchain.pem"
  cp "$TMP/test.key" "$TMP/certs/live/$domain/privkey.pem"
done
printf 'rtc-acme-fixture\n' > "$TMP/www/.well-known/acme-challenge/rtc-check"
chmod 755 "$TMP/www" "$TMP/www/.well-known" "$TMP/www/.well-known/acme-challenge"
chmod 644 "$TMP/www/.well-known/acme-challenge/rtc-check"
nginx_mounts=()
while IFS=$'\t' read -r source target read_only; do
  case "$target" in
    /etc/nginx/templates|/etc/nginx/templates/*|/docker-entrypoint.d/*)
      [[ "$source" == "$ROOT/deploy/"* && -e "$source" && "$read_only" == true ]] \
        || fail "Unexpected Nginx template/hook bind: $target"
      nginx_mounts+=(-v "$source:$target:ro") ;;
    /etc/letsencrypt) nginx_mounts+=(-v "$TMP/certs:$target:ro") ;;
    /var/www/certbot) nginx_mounts+=(-v "$TMP/www:$target:ro") ;;
    /var/log/nginx) nginx_mounts+=(-v "$TMP/nginx_log:$target") ;;
    *) fail "Unhandled Nginx bind (update fixture explicitly): $target" ;;
  esac
done < <(jq -r '.services.imboy_nginx.volumes[] | [.source, .target, (.read_only // false)] | @tsv' "$TMP/model.json")
cat > "$TMP/upstream.conf" <<'NGINX'
server {
    listen 80;
    listen 8080;
    listen 9800;
    listen 13990;
    listen 3900;
    default_type text/plain;
    access_log off;
    return 200 "$server_port $request_uri\n";
}
NGINX
"${DOCKER[@]}" network create --internal "$NETWORK" >/dev/null
network_created=true
"${DOCKER[@]}" run -d --cidfile "$TMP/upstream.cid" --network "$NETWORK" \
  --network-alias imboy_backend --network-alias imboy_edge --network-alias imboy_admin \
  --network-alias imboy_widget --network-alias garage \
  -v "$TMP/upstream.conf:/etc/nginx/conf.d/default.conf:ro" "$nginx_image" >/dev/null
"${DOCKER[@]}" run -d --cidfile "$TMP/livekit.cid" --network "$NETWORK" \
  --network-alias imboy_livekit -v "$TMP/livekit.yaml:/etc/livekit.yaml:ro" \
  "$livekit_image" --config /etc/livekit.yaml >/dev/null
"${DOCKER[@]}" run -d --cidfile "$TMP/nginx.cid" --network "$NETWORK" \
  --env-file "$TMP/nginx.env" "${nginx_mounts[@]}" \
  -p 127.0.0.1::80 -p 127.0.0.1::443 "$nginx_image" "${nginx_command[@]}" >/dev/null
read -r nginx_id < "$TMP/nginx.cid"
http_address="$("${DOCKER[@]}" port "$nginx_id" 80/tcp)"
https_address="$("${DOCKER[@]}" port "$nginx_id" 443/tcp)"
http_port="${http_address##*:}"; https_port="${https_address##*:}"

# These URLs resolve exclusively to published loopback ports. Keep TLS hostname
# verification enabled, and never follow redirects to external destinations.
local_http() {
  local domain=$1; shift
  curl --disable --noproxy '*' --silent --show-error --connect-timeout 2 --max-time 3 \
    --resolve "$domain:$http_port:127.0.0.1" "$@"
}
local_https() {
  local domain=$1; shift
  curl --disable --noproxy '*' --silent --show-error --http1.1 --tlsv1.2 \
    --cacert "$TMP/test.crt" --connect-timeout 2 --max-time 5 \
    --resolve "$domain:$https_port:127.0.0.1" "$@"
}
ready=false
for attempt in {1..30}; do
  if local_http rtc.test.invalid --fail \
      "http://rtc.test.invalid:$http_port/.well-known/acme-challenge/rtc-check" \
      > "$TMP/acme-body" 2>/dev/null && grep -qx 'rtc-acme-fixture' "$TMP/acme-body"; then
    ready=true; break
  fi
  sleep 1
done
"$ready" || fail 'Nginx did not serve the RTC ACME fixture through the actual entrypoint'
for domain in api.test.invalid admin.test.invalid cs.test.invalid rtc.test.invalid turn.test.invalid; do
  body="$(local_http "$domain" --fail "http://$domain:$http_port/.well-known/acme-challenge/rtc-check")"
  [[ "$body" == rtc-acme-fixture ]] || fail "HTTP-01 webroot is not served for $domain"
done
pass 'Five-domain HTTP-01 routing works, including the TURN default webroot'
"${DOCKER[@]}" exec "$nginx_id" nginx -t
"${DOCKER[@]}" exec "$nginx_id" nginx -T > "$TMP/nginx-config" 2> "$TMP/nginx-check.log"
grep -Fq 'server_name rtc.test.invalid;' "$TMP/nginx-config" || fail 'RTC server was not rendered/loaded'
! grep -Fq 'livekit-l4-sni-stream' "$TMP/nginx-config" || fail 'Optional stream template entered HTTP configuration'
pass 'Official entrypoint renders the real templates; nginx -t passes'
check_route() {
  local domain=$1 path=$2 expected=$3 body
  body="$(local_https "$domain" --fail "https://$domain:$https_port$path")"
  [[ "$body" == "$expected" ]] || fail "Unexpected upstream/path for $domain$path"
}
check_route api.test.invalid /api/v1/healthz '9800 /api/v1/healthz'
check_route api.test.invalid /s3/test-bucket/item '3900 /test-bucket/item'
check_route api.test.invalid /edge '13990 /edge'
check_route admin.test.invalid / '80 /'
check_route cs.test.invalid /v1/loader.js '8080 /v1/loader.js'
pass 'Existing API, S3, edge, Admin and Widget routes reach expected fixture upstreams'
for domain in api.test.invalid rtc.test.invalid; do
  status="$(local_https "$domain" -o /dev/null -w '%{http_code}' "https://$domain:$https_port/metrics")"
  [[ "$status" == 403 ]] || fail "Public metrics must remain denied on $domain"
done
pass 'API and RTC metrics remain denied over verified TLS'

# v1.13.7 accepts Authorization: Bearer (pkg/service/auth.go). Use a private
# header file: the synthetic short-lived token never appears in URLs or logs.
base64url() { openssl base64 -A | tr '+/' '-_' | tr -d '='; }
now="$(date +%s)"
header="$(printf '%s' '{"alg":"HS256","typ":"JWT"}' | base64url)"
payload="$(jq -cn --argjson now "$now" '{iss:"rtc_test_key",sub:"rtc-wss-test",nbf:($now-10),exp:($now+300),video:{roomJoin:true,room:"rtc-wss-regression",canPublish:false,canSubscribe:false}}' | base64url)"
unsigned="$header.$payload"
signature="$(printf '%s' "$unsigned" | openssl dgst -sha256 -hmac "$secret" -binary | base64url)"
printf 'Authorization: Bearer %s.%s\n' "$unsigned" "$signature" > "$TMP/auth-header"
ws_key="$(openssl rand -base64 16)"
expected_accept="$(printf '%s' "${ws_key}258EAFA5-E914-47DA-95CA-C5AB0DC85B11" | openssl dgst -sha1 -binary | openssl base64 -A)"
upgrade_headers=(-H 'Connection: Upgrade' -H 'Upgrade: websocket' -H 'Sec-WebSocket-Version: 13' -H "Sec-WebSocket-Key: $ws_key")
ready=false
for attempt in {1..30}; do
  status="$(local_https rtc.test.invalid "${upgrade_headers[@]}" -o /dev/null -w '%{http_code}' \
    "https://rtc.test.invalid:$https_port/rtc?protocol=17&auto_subscribe=0" 2>/dev/null)" || status=000
  if [[ "$status" == 401 ]]; then ready=true; break; fi
  sleep 1
done
"$ready" || fail 'Unauthenticated RTC upgrade did not reach the real LiveKit 401 response'
pass 'Dedicated RTC origin reaches LiveKit and rejects an unauthenticated upgrade'
for origin_path in 'rtc.test.invalid /rtc' 'api.test.invalid /livekit/rtc'; do
  read -r domain path <<< "$origin_path"
  rc=0
  local_https "$domain" "${upgrade_headers[@]}" -H "@$TMP/auth-header" \
    --dump-header "$TMP/ws-headers" -o /dev/null \
    "https://$domain:$https_port$path?protocol=17&auto_subscribe=0" \
    2> "$TMP/ws-stderr" || rc=$?
  # An upgraded socket stays open. Accept curl timeout only with a complete,
  # independently checked 101 handshake; a timeout alone can never pass.
  [[ "$rc" -eq 0 || "$rc" -eq 28 ]] || fail "WSS transport failed for $domain$path (curl=$rc)"
  tr -d '\r' < "$TMP/ws-headers" > "$TMP/ws-normalized"
  grep -Eq '^HTTP/1\.1 101([[:space:]]|$)' "$TMP/ws-normalized" || fail "No HTTP 101 from $domain$path"
  awk -F ': *' 'tolower($1)=="upgrade" && tolower($2)=="websocket" {ok=1} END{exit !ok}' "$TMP/ws-normalized" \
    || fail 'Missing websocket Upgrade response'
  awk -F ': *' 'tolower($1)=="connection" && tolower($2) ~ /(^|, *)upgrade(,|$)/ {ok=1} END{exit !ok}' "$TMP/ws-normalized" \
    || fail 'Missing Connection: upgrade response'
  awk -F ': *' -v expected="$expected_accept" 'tolower($1)=="sec-websocket-accept" && $2==expected {ok=1} END{exit !ok}' "$TMP/ws-normalized" \
    || fail 'Incorrect Sec-WebSocket-Accept response'
  pass "Verified TLS + authenticated LiveKit WSS handshake: $domain$path"
done
printf '\nPASS: isolated Nginx/LiveKit signaling only. NOT verified: public ACME/DNS, TURN, ICE/media, App authorization or two-device calls.\n'
