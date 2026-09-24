#!/usr/bin/env bash
# Switch an existing host-Nginx LiveKit deployment to TURN/TLS on shared :443.

set -Eeuo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODE=""
CONFIG_FILE="/etc/imboy/livekit-l4-sni.env"
ROLLBACK_BACKUP=""
MUTATION_STARTED=0
CURRENT_BACKUP=""

log() { printf '[livekit-l4-sni] %s\n' "$*"; }
die() { printf '[livekit-l4-sni] ERROR: %s\n' "$*" >&2; return 1; }

usage() {
  cat <<'USAGE'
Usage:
  sudo bash install-livekit-l4-sni.sh --check [--config FILE]
  sudo bash install-livekit-l4-sni.sh --apply [--config FILE]
  sudo bash install-livekit-l4-sni.sh --rollback [--config FILE] [--backup DIR]

Modes:
  --check       Read-only prerequisite and current-state checks.
  --apply       Install HAProxy, back up config, switch traffic, and verify.
  --rollback    Restore the latest successful backup (or --backup DIR).
USAGE
}

parse_args() {
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --check|--apply|--rollback)
        [ -z "$MODE" ] || die "choose exactly one mode"
        MODE="${1#--}"
        ;;
      --config)
        shift; [ "$#" -gt 0 ] || die "--config requires a file"
        CONFIG_FILE="$1"
        ;;
      --backup)
        shift; [ "$#" -gt 0 ] || die "--backup requires a directory"
        ROLLBACK_BACKUP="$1"
        ;;
      -h|--help) usage; exit 0 ;;
      *) die "unknown argument: $1" ;;
    esac
    shift
  done
  [ -n "$MODE" ] || { usage; exit 2; }
}

load_config() {
  [ -f "$CONFIG_FILE" ] || die "config not found: $CONFIG_FILE (copy livekit-l4-sni.env.example first)"
  [ "$(stat -c %u "$CONFIG_FILE")" = 0 ] || die "config must be owned by root: $CONFIG_FILE"
  if find "$CONFIG_FILE" -prune -perm /022 -print -quit | grep -q .; then
    die "config must not be writable by group/world: $CONFIG_FILE"
  fi
  # shellcheck source=/dev/null
  source "$CONFIG_FILE"
  : "${TURN_DOMAIN:?TURN_DOMAIN is required}"
  : "${NGINX_VHOST_DIR:?NGINX_VHOST_DIR is required}"
  : "${NGINX_STREAM_CONF:?NGINX_STREAM_CONF is required}"
  : "${NGINX_REALIP_CONF:?NGINX_REALIP_CONF is required}"
  : "${HTTPS_VHOST_FILES:?HTTPS_VHOST_FILES is required}"
  : "${COMPOSE_DIR:?COMPOSE_DIR is required}"
  : "${COMPOSE_FILE:?COMPOSE_FILE is required}"
  : "${COMPOSE_ENV_FILE:?COMPOSE_ENV_FILE is required}"
  : "${LIVEKIT_SERVICE:=imboy_livekit}"
  : "${LIVEKIT_CONTAINER:=imboy_livekit}"
  : "${LIVEKIT_HEALTH_URL:=http://127.0.0.1:7880/}"
  : "${BACKEND_HEALTH_URL:=}"
  : "${LIVEKIT_TURN_CERT_DIR:?LIVEKIT_TURN_CERT_DIR is required}"
  : "${CERTBOT_HOOK:=/etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh}"
  : "${ETURNAL_SERVICE:=eturnal}"
  : "${L4_STATE_DIR:=/var/lib/imboy-livekit-l4-sni}"
  : "${L4_BACKUP_ROOT:=/root/imboy-livekit-l4-sni-backups}"
  GENERATED_COMPOSE="$COMPOSE_DIR/livekit-turn-l4-sni.generated.yml"
  HAPROXY_CONF=/etc/haproxy/haproxy.cfg
}

require_root() {
  [ "$(id -u)" -eq 0 ] || die "run with sudo/root"
}

require_commands() {
  local command_name
  for command_name in nginx docker openssl curl tar ss systemctl python3 grep \
    stat find install sha256sum awk; do
    command -v "$command_name" >/dev/null 2>&1 || die "missing command: $command_name"
  done
  if docker compose version >/dev/null 2>&1; then
    COMPOSE=(docker compose)
  elif command -v docker-compose >/dev/null 2>&1; then
    COMPOSE=(docker-compose)
  else
    die "Docker Compose is not installed"
  fi
}

install_haproxy() {
  command -v haproxy >/dev/null 2>&1 && return
  command -v apt-get >/dev/null 2>&1 || die "automatic HAProxy install currently supports Debian/Ubuntu apt-get only"
  log "installing HAProxy (no new public listener)"
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends haproxy
}

validate_domain() {
  [[ "$TURN_DOMAIN" =~ ^[A-Za-z0-9.-]+$ ]] || die "invalid TURN_DOMAIN"
  [[ "$TURN_DOMAIN" == *.* ]] || die "TURN_DOMAIN must be a fully qualified domain"
  [[ "$LIVEKIT_SERVICE" =~ ^[A-Za-z0-9_.-]+$ ]] || die "invalid LIVEKIT_SERVICE"
}

validate_certificate() {
  local cert="$LIVEKIT_TURN_CERT_DIR/fullchain.pem"
  local key="$LIVEKIT_TURN_CERT_DIR/privkey.pem"
  [ -s "$cert" ] && [ -s "$key" ] || die "TURN certificate/key missing under $LIVEKIT_TURN_CERT_DIR"
  openssl x509 -noout -in "$cert" >/dev/null 2>&1 || die "TURN certificate cannot be parsed"
  openssl pkey -noout -in "$key" >/dev/null 2>&1 || die "TURN private key cannot be parsed"
  openssl x509 -noout -subject -ext subjectAltName -in "$cert" 2>/dev/null \
    | grep -Fq "$TURN_DOMAIN" || die "certificate does not cover $TURN_DOMAIN"
  local cert_pub key_pub
  cert_pub="$(openssl x509 -noout -pubkey -in "$cert" 2>/dev/null)"
  key_pub="$(openssl pkey -pubout -in "$key" 2>/dev/null)"
  [ "$cert_pub" = "$key_pub" ] || die "TURN certificate/private key mismatch"
}

validate_nginx() {
  nginx -V 2>&1 | grep -q -- '--with-stream' || die "Nginx lacks stream module"
  nginx -V 2>&1 | grep -q -- '--with-stream_ssl_preread_module' || die "Nginx lacks ssl_preread module"
  nginx -t
  systemctl is-active --quiet nginx || die "nginx is not active"
  ss -H -lntp 'sport = :443' | grep -q nginx || die "public TCP/443 is not currently owned by nginx"
  local file
  for file in $HTTPS_VHOST_FILES; do
    [ -f "$NGINX_VHOST_DIR/$file" ] || die "Nginx vhost not found: $NGINX_VHOST_DIR/$file"
    grep -qE '^\s*listen\s+(443|\[::\]:443)(\s|;)' "$NGINX_VHOST_DIR/$file" \
      || grep -qE '^\s*listen\s+(127\.0\.0\.1:10443|\[::1\]:10443)(\s|;)' "$NGINX_VHOST_DIR/$file" \
      || die "no HTTPS listener found in $file"
  done
}

validate_livekit() {
  [ -f "$COMPOSE_FILE" ] || die "Compose file not found: $COMPOSE_FILE"
  [ -f "$COMPOSE_ENV_FILE" ] || die "Compose env file not found: $COMPOSE_ENV_FILE"
  [ -d "$COMPOSE_DIR" ] || die "Compose directory not found: $COMPOSE_DIR"
  [ "$(docker inspect -f '{{.State.Status}}' "$LIVEKIT_CONTAINER" 2>/dev/null || true)" = running ] \
    || die "LiveKit container is not running: $LIVEKIT_CONTAINER"
  curl --noproxy '*' -fsS --connect-timeout 3 --max-time 6 "$LIVEKIT_HEALTH_URL" >/dev/null \
    || die "LiveKit health URL failed: $LIVEKIT_HEALTH_URL"
}

validate_old_turn() {
  if systemctl is-active --quiet coturn 2>/dev/null; then
    die "coturn is active; stop and investigate it before this switch"
  fi
  if systemctl is-active --quiet "$ETURNAL_SERVICE"; then
    log "$ETURNAL_SERVICE is active and will be stopped (not uninstalled) by --apply"
  else
    log "$ETURNAL_SERVICE is already inactive; it will remain installed"
  fi
}

preflight() {
  require_root
  require_commands
  validate_domain
  validate_certificate
  validate_nginx
  validate_livekit
  validate_old_turn
  log "CHECK_PASS: prerequisites, certificate, Nginx modules, Compose, and LiveKit are valid"
}

docker_gateway_cidr() {
  local gateways gateway count
  if [ -n "${LIVEKIT_PROXY_TRUSTED_IP:-}" ]; then
    gateway="$LIVEKIT_PROXY_TRUSTED_IP"
  else
    gateways="$(docker inspect -f '{{range .NetworkSettings.Networks}}{{println .Gateway}}{{end}}' "$LIVEKIT_CONTAINER" 2>/dev/null \
      | awk 'NF && !seen[$0]++')"
    count="$(printf '%s\n' "$gateways" | awk 'NF {count++} END {print count+0}')"
    [ "$count" -eq 1 ] \
      || die "LiveKit has $count Docker gateways; set one exact LIVEKIT_PROXY_TRUSTED_IP"
    gateway="$gateways"
  fi
  python3 - "$gateway" <<'PY' || die "invalid LiveKit Docker IPv4 gateway: $gateway"
import ipaddress
import sys
address = ipaddress.ip_address(sys.argv[1])
if address.version != 4:
    raise SystemExit(1)
PY
  printf '%s/32\n' "$gateway"
}

backup_configs() {
  local stamp path
  local -a paths rels
  stamp="$(date -u +%Y%m%dT%H%M%SZ)"
  CURRENT_BACKUP="$L4_BACKUP_ROOT/$stamp"
  mkdir -p "$CURRENT_BACKUP" "$L4_STATE_DIR"
  chmod 700 "$CURRENT_BACKUP" "$L4_STATE_DIR"
  paths=("$HAPROXY_CONF" "$COMPOSE_FILE" "$COMPOSE_ENV_FILE")
  for path in $HTTPS_VHOST_FILES; do paths+=("$NGINX_VHOST_DIR/$path"); done
  for path in "$NGINX_STREAM_CONF" "$NGINX_REALIP_CONF" "$GENERATED_COMPOSE" "$CERTBOT_HOOK"; do
    if [ -e "$path" ]; then paths+=("$path"); else printf '%s\n' "$path" >>"$CURRENT_BACKUP/created-paths"; fi
  done
  for path in "${paths[@]}"; do rels+=("${path#/}"); done
  tar --acls --xattrs -czf "$CURRENT_BACKUP/config.tgz" -C / "${rels[@]}"
  sha256sum "$CURRENT_BACKUP/config.tgz" >"$CURRENT_BACKUP/config.tgz.sha256"
  systemctl is-enabled "$ETURNAL_SERVICE" >"$CURRENT_BACKUP/eturnal-enabled" 2>/dev/null || true
  systemctl is-active "$ETURNAL_SERVICE" >"$CURRENT_BACKUP/eturnal-active" 2>/dev/null || true
  docker inspect "$LIVEKIT_CONTAINER" >"$CURRENT_BACKUP/livekit-inspect.json"
  nginx -T >"$CURRENT_BACKUP/nginx-T.txt" 2>&1
  chmod 600 "$CURRENT_BACKUP"/*
  log "backup created: $CURRENT_BACKUP"
}

render_configs() {
  local stage="$CURRENT_BACKUP/staged" gateway_cidr
  mkdir -p "$stage/vhosts"
  gateway_cidr="$(docker_gateway_cidr)"
  cp "$SCRIPT_DIR/haproxy/livekit-turn.cfg.template" "$stage/haproxy.cfg"
  python3 - "$TURN_DOMAIN" "$LIVEKIT_SERVICE" "$gateway_cidr" \
    "$LIVEKIT_TURN_CERT_DIR" "$SCRIPT_DIR" "$stage" <<'PY'
import pathlib
import sys

domain, service, gateway, cert_dir, script_dir, stage = sys.argv[1:]
script_dir = pathlib.Path(script_dir)
stage = pathlib.Path(stage)
stream = (script_dir / "nginx/templates/livekit-l4-sni-stream.conf.template").read_text()
(stage / "stream.conf").write_text(stream.replace("__TURN_DOMAIN__", domain))
compose = (script_dir / "docker-compose.livekit-turn-l4-sni.yml.template").read_text()
replacements = {
    "__TURN_DOMAIN__": domain,
    "__LIVEKIT_SERVICE__": service,
    "__DOCKER_GATEWAY_CIDR__": gateway,
    "__LIVEKIT_TURN_CERT_DIR__": cert_dir,
}
for source, target in replacements.items():
    compose = compose.replace(source, target)
(stage / "compose.yml").write_text(compose)
PY
  printf '%s\n' 'set_real_ip_from 127.0.0.1;' 'set_real_ip_from ::1;' \
    'real_ip_header proxy_protocol;' >"$stage/realip.conf"
  local file
  for file in $HTTPS_VHOST_FILES; do cp -a "$NGINX_VHOST_DIR/$file" "$stage/vhosts/$file"; done
  python3 - "$stage/vhosts" <<'PY'
import pathlib
import re
import sys

root = pathlib.Path(sys.argv[1])
pattern = re.compile(r"^(\s*)listen\s+(443|\[::\]:443)([^;]*);", re.M)
for path in sorted(root.iterdir()):
    text = path.read_text()
    def replace(match):
        address = "127.0.0.1:10443" if match.group(2) == "443" else "[::1]:10443"
        options = match.group(3).strip().split()
        if "proxy_protocol" not in options:
            options.append("proxy_protocol")
        suffix = " " + " ".join(options) if options else ""
        return f"{match.group(1)}listen {address}{suffix};"
    updated, count = pattern.subn(replace, text)
    if count == 0 and "10443" not in text:
        raise SystemExit(f"no :443 listener rewritten in {path.name}")
    path.write_text(updated)
PY
  haproxy -c -f "$stage/haproxy.cfg"
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$COMPOSE_ENV_FILE" \
    -f "$COMPOSE_FILE" -f "$stage/compose.yml" config --quiet)
  bash -n "$SCRIPT_DIR/nginx/livekit-turn-cert-deploy-hook.sh"
  log "configuration render and static validation passed; trusted Docker source is one /32"
}

wait_livekit() {
  local _
  for _ in {1..45}; do
    curl --noproxy '*' -fsS --connect-timeout 2 --max-time 3 "$LIVEKIT_HEALTH_URL" >/dev/null 2>&1 && return
    sleep 1
  done
  die "LiveKit did not become healthy within 45 seconds"
}

install_configs() {
  local stage="$CURRENT_BACKUP/staged" file
  systemctl stop "$ETURNAL_SERVICE"
  systemctl disable "$ETURNAL_SERVICE"
  systemctl is-active --quiet "$ETURNAL_SERVICE" && die "$ETURNAL_SERVICE is still active"

  install -m 0644 "$stage/compose.yml" "$GENERATED_COMPOSE"
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$COMPOSE_ENV_FILE" \
    -f "$COMPOSE_FILE" -f "$GENERATED_COMPOSE" up -d --force-recreate "$LIVEKIT_SERVICE")
  wait_livekit

  install -m 0644 "$stage/haproxy.cfg" "$HAPROXY_CONF"
  haproxy -c -f "$HAPROXY_CONF"
  systemctl restart haproxy
  systemctl is-active --quiet haproxy

  for file in $HTTPS_VHOST_FILES; do install -m 0644 "$stage/vhosts/$file" "$NGINX_VHOST_DIR/$file"; done
  install -D -m 0644 "$stage/stream.conf" "$NGINX_STREAM_CONF"
  install -D -m 0644 "$stage/realip.conf" "$NGINX_REALIP_CONF"
  nginx -t
  systemctl reload nginx

  install -D -m 0755 "$SCRIPT_DIR/nginx/livekit-turn-cert-deploy-hook.sh" "$CERTBOT_HOOK"
}

verify_https_contracts() {
  local contract domain expected actual
  for contract in ${HTTPS_HEALTH_CONTRACTS:-}; do
    domain="${contract%:*}"; expected="${contract##*:}"
    [[ "$expected" =~ ^[0-9]{3}$ ]] || die "invalid HTTPS contract: $contract"
    actual="$(curl --noproxy '*' -skS --resolve "$domain:443:127.0.0.1" \
      -o /dev/null -w '%{http_code}' --connect-timeout 4 --max-time 8 "https://$domain/")"
    [ "$actual" = "$expected" ] || die "$domain returned $actual, expected $expected"
  done
}

verify_switch() {
  systemctl is-active --quiet nginx
  systemctl is-active --quiet haproxy
  if systemctl is-active --quiet "$ETURNAL_SERVICE"; then
    die "$ETURNAL_SERVICE unexpectedly became active"
  fi
  ss -H -lntp 'sport = :443' | grep -q nginx
  ss -H -lntp 'sport = :10443' | grep -q nginx
  ss -H -lntp 'sport = :15442' | grep -q haproxy
  ss -H -lntp 'sport = :15443' | grep -q docker-proxy
  ss -H -lnup 'sport = :3478' | grep -q docker-proxy
  ss -H -lnup 'sport = :50201' | grep -q docker-proxy
  ss -H -lnup 'sport = :50500' | grep -q docker-proxy
  if grep -qE '^\s*server livekit .* check\s*$' "$HAPROXY_CONF"; then
    die "HAProxy bare TCP health check must remain disabled"
  fi
  verify_https_contracts
  if [ -n "$BACKEND_HEALTH_URL" ]; then
    curl --noproxy '*' -fsS --connect-timeout 3 --max-time 6 "$BACKEND_HEALTH_URL" | grep -q '"status":"ok"'
  fi
  openssl s_client -connect 127.0.0.1:443 -servername "$TURN_DOMAIN" \
    -verify_hostname "$TURN_DOMAIN" -CAfile /etc/ssl/certs/ca-certificates.crt \
    -brief </dev/null 2>&1 | grep -q 'Verification: OK'
  docker logs "$LIVEKIT_CONTAINER" 2>&1 | grep -q 'Starting TURN server'
  docker logs "$LIVEKIT_CONTAINER" 2>&1 | grep -q '"turn.proxyProtocol": true'
  log "VERIFY_PASS: HTTPS baselines, backend, TURN TLS, ports, and PROXY protocol"
}

restore_backup() {
  local backup="$1" path enabled active
  [ -d "$backup" ] || die "backup directory not found: $backup"
  (cd "$backup" && sha256sum -c config.tgz.sha256)
  if [ -f "$backup/created-paths" ]; then
    while IFS= read -r path; do [ -n "$path" ] && rm -f -- "$path"; done <"$backup/created-paths"
  fi
  tar --acls --xattrs -xzf "$backup/config.tgz" -C /
  nginx -t
  systemctl reload nginx
  systemctl restart haproxy || true
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$COMPOSE_ENV_FILE" \
    -f "$COMPOSE_FILE" up -d --force-recreate "$LIVEKIT_SERVICE")
  wait_livekit
  enabled="$(cat "$backup/eturnal-enabled" 2>/dev/null || true)"
  active="$(cat "$backup/eturnal-active" 2>/dev/null || true)"
  if [ "$enabled" = enabled ]; then
    systemctl enable "$ETURNAL_SERVICE"
  else
    systemctl disable "$ETURNAL_SERVICE" || true
  fi
  if [ "$active" = active ]; then
    systemctl start "$ETURNAL_SERVICE"
  else
    systemctl stop "$ETURNAL_SERVICE" || true
  fi
  rm -f "$L4_STATE_DIR/active-backup"
  log "ROLLBACK_COMPLETE: restored $backup; HAProxy package remains installed"
}

on_error() {
  local rc=$?
  trap - ERR
  if [ "$MUTATION_STARTED" -eq 1 ] && [ -n "$CURRENT_BACKUP" ]; then
    log "apply failed (rc=$rc); starting automatic rollback"
    log "if rollback is interrupted, run: $0 --rollback --config $CONFIG_FILE --backup $CURRENT_BACKUP"
    restore_backup "$CURRENT_BACKUP"
  fi
  exit "$rc"
}

apply_switch() {
  [ ! -f "$L4_STATE_DIR/active-backup" ] || die "an active scripted switch already exists; use --check or --rollback"
  preflight
  if ! systemctl is-active --quiet "$ETURNAL_SERVICE" \
    && ss -H -lntp 'sport = :15442' | grep -q haproxy \
    && ss -H -lntp 'sport = :15443' | grep -q docker-proxy; then
    die "L4 SNI appears already active outside this installer; refusing a duplicate apply"
  fi
  install_haproxy
  require_commands
  backup_configs
  render_configs
  MUTATION_STARTED=1
  install_configs
  verify_switch
  printf '%s\n' "$CURRENT_BACKUP" >"$L4_STATE_DIR/active-backup"
  chmod 600 "$L4_STATE_DIR/active-backup"
  MUTATION_STARTED=0
  log "APPLY_PASS: backup=$CURRENT_BACKUP rollback='sudo bash $0 --rollback --config $CONFIG_FILE'"
}

rollback_switch() {
  require_root
  require_commands
  local backup="$ROLLBACK_BACKUP"
  [ -n "$backup" ] || backup="$(cat "$L4_STATE_DIR/active-backup" 2>/dev/null || true)"
  [ -n "$backup" ] || die "no active backup pointer; pass --backup DIR"
  restore_backup "$backup"
}

main() {
  parse_args "$@"
  load_config
  trap on_error ERR
  case "$MODE" in
    check) preflight ;;
    apply) apply_switch ;;
    rollback) rollback_switch ;;
  esac
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  main "$@"
fi
