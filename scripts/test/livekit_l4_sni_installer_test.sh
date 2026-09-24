#!/usr/bin/env bash
# shellcheck disable=SC1091,SC2034
set -Eeuo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Source functions without running main; all external mutation commands used by
# render_configs are replaced with no-op validators below.
source "$ROOT/deploy/install-livekit-l4-sni.sh"

SCRIPT_DIR="$ROOT/deploy"
CURRENT_BACKUP="$TMP/backup"
NGINX_VHOST_DIR="$TMP/vhosts"
HTTPS_VHOST_FILES="api.conf dual.conf"
TURN_DOMAIN=turn.example.com
LIVEKIT_SERVICE=imboy_livekit
LIVEKIT_TURN_CERT_DIR=/etc/letsencrypt/live/turn.example.com
COMPOSE_DIR="$TMP/compose"
COMPOSE_ENV_FILE="$TMP/compose/.env"
COMPOSE_FILE="$TMP/compose/base.yml"
COMPOSE=(true)

mkdir -p "$CURRENT_BACKUP" "$NGINX_VHOST_DIR" "$COMPOSE_DIR"
: >"$COMPOSE_ENV_FILE"
: >"$COMPOSE_FILE"
cat >"$NGINX_VHOST_DIR/api.conf" <<'NGINX'
server {
    listen 443 ssl http2;
    server_name api.example.com;
}
NGINX
cat >"$NGINX_VHOST_DIR/dual.conf" <<'NGINX'
server {
    listen 443 ssl;
    listen [::]:443 ssl;
    server_name dual.example.com;
}
NGINX

docker_gateway_cidr() { printf '172.29.0.1/32\n'; }
haproxy() { [ "$1" = -c ] && [ "$2" = -f ]; }

render_configs >/dev/null

assert_contains() {
  grep -Fq "$2" "$1" || { printf 'FAIL: %s lacks %s\n' "$1" "$2" >&2; exit 1; }
}

assert_contains "$CURRENT_BACKUP/staged/stream.conf" "turn.example.com 127.0.0.1:15442"
assert_contains "$CURRENT_BACKUP/staged/compose.yml" "172.29.0.1/32"
assert_contains "$CURRENT_BACKUP/staged/compose.yml" "127.0.0.1:15443:443"
assert_contains "$CURRENT_BACKUP/staged/vhosts/api.conf" "127.0.0.1:10443 ssl http2 proxy_protocol"
assert_contains "$CURRENT_BACKUP/staged/vhosts/dual.conf" "[::1]:10443 ssl proxy_protocol"
if grep -qE '^\s*server livekit .* check\s*$' "$CURRENT_BACKUP/staged/haproxy.cfg"; then
  printf 'FAIL: HAProxy template contains a bare TCP health check\n' >&2
  exit 1
fi

printf 'PASS: LiveKit L4 SNI templates render with exact /32 trust and no TCP health-check noise\n'
