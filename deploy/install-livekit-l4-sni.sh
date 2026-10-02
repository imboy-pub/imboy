#!/usr/bin/env bash
# Switch an existing host-Nginx LiveKit deployment to TURN/TLS on shared :443.
#
# nginx instance pinning (BT/宝塔 compatible): exactly one nginx binary is
# selected (explicit NGINX_BIN, then common BT panel paths, then PATH) and
# pinned to the running master process (master PID + prefix + config path,
# cross-checked via `nginx -V` and `ps`). Every nginx operation below --
# baseline test, config export, reload, verification and rollback -- addresses
# that same instance. A systemd unit named "nginx" is never trusted for
# reload/verify: a green distro unit must never be read as "panel nginx
# reloaded". Unrecognizable master lines (-g, ambiguous) fail with BLOCKED_ENV.

# shellcheck disable=SC2046  # managed_vhost_paths output is deliberately split into file args
set -Eeuo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODE=""
CONFIG_FILE="/etc/imboy/livekit-l4-sni.env"
ROLLBACK_BACKUP=""
MUTATION_STARTED=0
CURRENT_BACKUP=""

# Pinned nginx instance state (filled by resolve_nginx_instance).
NGINX_BIN=""
NGINX_BIN_REAL=""
NGINX_MASTER_PID=""
NGINX_CONF_PATH=""
NGINX_PREFIX_ARGS=""
NGINX_MASTER_CMDLINE=""
NGINX_DEFAULT_PREFIX=""
NGINX_IDENTITY=""

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

Extra env (see livekit-l4-sni.env.example): NGINX_BIN, COMPOSE_OVERLAY_FILES,
L4_SNI_CHECK_SCRIPT, HAPROXY_CONF.
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
  : "${NGINX_BIN:=}"
  : "${HAPROXY_CONF:=/etc/haproxy/haproxy.cfg}"
  : "${COMPOSE_OVERLAY_FILES:=}"
  : "${L4_SNI_CHECK_SCRIPT:=$SCRIPT_DIR/../scripts/check_l4_sni_listen.sh}"
  GENERATED_COMPOSE="$COMPOSE_DIR/livekit-turn-l4-sni.generated.yml"
  # Operator overlays must exist now: they are part of the running stack.
  COMPOSE_OVERLAYS=()
  local overlay
  for overlay in $COMPOSE_OVERLAY_FILES; do
    case "$overlay" in /*) ;; *) overlay="$COMPOSE_DIR/$overlay" ;; esac
    [ -f "$overlay" ] || die "COMPOSE_OVERLAY_FILES entry not found: $overlay"
    COMPOSE_OVERLAYS+=("$overlay")
  done
  COMPOSE_PRE_FILES=("$COMPOSE_FILE" ${COMPOSE_OVERLAYS[@]+"${COMPOSE_OVERLAYS[@]}"})
  COMPOSE_SWITCH_FILES=("$COMPOSE_FILE" ${COMPOSE_OVERLAYS[@]+"${COMPOSE_OVERLAYS[@]}"} "$GENERATED_COMPOSE")
}

require_root() { [ "$(id -u)" -eq 0 ] || die "run with sudo/root"; }

require_commands() {
  local command_name
  for command_name in docker openssl curl ss systemctl python3 grep \
    stat find install sha256sum awk ps; do
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
  local cert="$LIVEKIT_TURN_CERT_DIR/fullchain.pem" key="$LIVEKIT_TURN_CERT_DIR/privkey.pem"
  [ -s "$cert" ] && [ -s "$key" ] || die "TURN certificate/key missing under $LIVEKIT_TURN_CERT_DIR"
  openssl x509 -noout -in "$cert" >/dev/null 2>&1 || die "TURN certificate cannot be parsed"
  openssl pkey -noout -in "$key" >/dev/null 2>&1 || die "TURN private key cannot be parsed"
  openssl x509 -noout -subject -ext subjectAltName -in "$cert" 2>/dev/null \
    | grep -Fq "$TURN_DOMAIN" || die "certificate does not cover $TURN_DOMAIN"
  local cert_pub key_pub
  cert_pub="$(openssl x509 -noout -pubkey -in "$cert" 2>/dev/null || true)"
  key_pub="$(openssl pkey -pubout -in "$key" 2>/dev/null || true)"
  [ "$cert_pub" = "$key_pub" ] || die "TURN certificate/private key mismatch"
}

# Pinned nginx instance: selection, identity, consistent -t/-T/reload/verify.

real_path() { python3 -c 'import os,sys; print(os.path.realpath(sys.argv[1]))' "$1"; }

nginx_token_realpath() {
  local token="$1" resolved
  case "$token" in
    */*) ;;
    *) resolved="$(command -v "$token" 2>/dev/null || true)"
       [ -n "$resolved" ] || return 1
       token="$resolved" ;;
  esac
  real_path "$token"
}

# Print running masters as "pid|master-command-line" (prefix stripped).
nginx_running_masters() {
  ps -eo pid=,args= | awk '!/awk/ && /nginx: master process/ {
    line=$0; sub(/^[ \t]+/, "", line)
    pid=line; sub(/[ \t].*/, "", pid)
    rest=line; sub(/^[^ \t]*[ \t]+/, "", rest)
    sub(/^nginx: master process[ \t]+/, "", rest)
    print pid "|" rest
  }'
}

resolve_nginx_instance() {
  local masters mline pid rest bin bin_real cand found=""
  local -a cands
  masters="$(nginx_running_masters)"
  if [ -z "$masters" ]; then
    die "BLOCKED_ENV: no running 'nginx: master process' found; cannot pin the nginx instance (start nginx first or investigate)"
  fi
  if [ -n "$NGINX_BIN" ]; then
    [ -x "$NGINX_BIN" ] || die "NGINX_BIN is not executable: $NGINX_BIN (explicit selection never falls back)"
    cands=("$NGINX_BIN")
  else
    cands=(/www/server/nginx/sbin/nginx /www/server/openresty/nginx/sbin/nginx \
      /usr/local/nginx/sbin/nginx /usr/local/openresty/nginx/sbin/nginx)
    if command -v nginx >/dev/null 2>&1; then cands+=("$(command -v nginx)"); fi
  fi
  for cand in "${cands[@]}"; do
    [ -n "$cand" ] && [ -x "$cand" ] || continue
    bin_real="$(real_path "$cand")"
    while IFS= read -r mline; do
      [ -n "$mline" ] || continue
      pid="${mline%%|*}"
      rest="${mline#*|}"
      bin="$(printf '%s\n' "$rest" | awk '{print $1}')"
      if [ "$(nginx_token_realpath "$bin" 2>/dev/null || true)" = "$bin_real" ]; then
        [ -z "$found" ] || die "BLOCKED_ENV: multiple nginx masters match $cand; ambiguous instance"
        found="$cand"
        NGINX_MASTER_PID="$pid"
        NGINX_MASTER_CMDLINE="$rest"
      fi
    done <<<"$masters"
    [ -n "$found" ] && break
  done
  if [ -z "$found" ]; then
    printf '[livekit-l4-sni] ERROR: BLOCKED_ENV: no candidate nginx binary matches a running master.\n[livekit-l4-sni] candidates tried: %s\n[livekit-l4-sni] set NGINX_BIN explicitly to the binary serving :443. Running masters:\n' \
      "${cands[*]}" >&2
    nginx_running_masters | sed 's/^/[livekit-l4-sni]   /' >&2
    return 1
  fi
  NGINX_BIN="$found"
  NGINX_BIN_REAL="$(real_path "$NGINX_BIN")"

  # Reproduce how the master was started: honor its -c/-p, refuse -g.
  local -a toks
  local i arg conf_flag="" prefix_flag=""
  # shellcheck disable=SC2206
  toks=($NGINX_MASTER_CMDLINE)
  i=1
  while [ "$i" -lt "${#toks[@]}" ]; do
    arg="${toks[$i]}"
    case "$arg" in
      -c) conf_flag="${toks[$((i+1))]}"; i=$((i+2)) ;;
      -p) prefix_flag="${toks[$((i+1))]}"; i=$((i+2)) ;;
      -g) die "BLOCKED_ENV: nginx master runs with -g custom directives; this installer cannot reproduce that instance" ;;
      *) i=$((i+1)) ;;
    esac
  done

  local v_output conf_default prefix_default
  v_output="$("$NGINX_BIN" -V 2>&1 || true)"
  printf '%s\n' "$v_output" | grep -q -- '--with-stream' || die "pinned nginx lacks stream module: $NGINX_BIN"
  printf '%s\n' "$v_output" | grep -q -- '--with-stream_ssl_preread_module' || die "pinned nginx lacks ssl_preread module: $NGINX_BIN"
  conf_default="$(printf '%s\n' "$v_output" | sed -n 's/.*--conf-path=\([^ ]*\).*/\1/p' | head -1)"
  prefix_default="$(printf '%s\n' "$v_output" | sed -n 's/.*--prefix=\([^ ]*\).*/\1/p' | head -1)"
  [ -n "$conf_default" ] || die "BLOCKED_ENV: cannot parse --conf-path from nginx -V output"
  NGINX_CONF_PATH="${conf_flag:-$conf_default}"
  NGINX_PREFIX_ARGS=""
  [ -n "$prefix_flag" ] && NGINX_PREFIX_ARGS="-p $prefix_flag"
  NGINX_DEFAULT_PREFIX="$prefix_default"

  # Informational cross-check only: a systemd unit never decides reload/verify.
  local unit_pid
  unit_pid="$(systemctl show -p MainPID --value nginx 2>/dev/null | tr -d '[:space:]' || true)"
  if printf '%s' "$unit_pid" | grep -Eq '^[1-9][0-9]*$'; then
    if [ "$unit_pid" = "$NGINX_MASTER_PID" ]; then
      log "nginx systemd unit 'nginx' manages the pinned master (pid $unit_pid)"
    else
      log "NOTE: systemd unit 'nginx' manages a DIFFERENT nginx (MainPID=$unit_pid); this switch ignores that unit entirely"
    fi
  else
    log "nginx is not managed by a systemd unit 'nginx'; reload will signal the pinned master directly"
  fi

  NGINX_IDENTITY="bin=$NGINX_BIN real=$NGINX_BIN_REAL master_pid=$NGINX_MASTER_PID prefix=${prefix_flag:-$prefix_default} conf=$NGINX_CONF_PATH"
  log "nginx instance pinned: $NGINX_IDENTITY"
}

# Run the pinned binary against the pinned config: nginx_ctl -t / -T / -s reload
nginx_ctl() {
  local -a pre=()
  # shellcheck disable=SC2206
  [ -n "$NGINX_PREFIX_ARGS" ] && pre=($NGINX_PREFIX_ARGS)
  "$NGINX_BIN" ${pre[@]+"${pre[@]}"} -c "$NGINX_CONF_PATH" "$@"
}

nginx_master_args() {
  ps -eo pid=,args= | awk -v p="$NGINX_MASTER_PID" '$1 == p' \
    | sed -n '1s/^[0-9 \t]*nginx: master process //p'
}

nginx_master_check() { [ -n "$(nginx_master_args)" ]; }

nginx_instance_verify() {
  local margs bin real
  nginx_master_check || die "pinned nginx master (pid $NGINX_MASTER_PID) is not running; instance drift"
  margs="$(nginx_master_args)"
  bin="$(printf '%s\n' "$margs" | awk '{print $1}')"
  real="$(nginx_token_realpath "$bin" 2>/dev/null || true)"
  [ "$real" = "$NGINX_BIN_REAL" ] || die "nginx master pid $NGINX_MASTER_PID now runs '$real' but the pinned instance is $NGINX_BIN_REAL; instance drift"
}

# True when a listener on port/proto is owned by the pinned instance (the
# master itself or a descendant worker, via the ppid chain). Never lets an
# empty ss answer or an unknown pid trip ERR inside command substitutions.
nginx_owns_port() {
  local port="$1" proto="$2" line pids pid cur depth
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    pids="$(printf '%s\n' "$line" | grep -o 'pid=[0-9]*' | cut -d= -f2 || true)"
    for pid in $pids; do
      cur="$pid"; depth=0
      while [ "$depth" -lt 8 ] && [ -n "$cur" ] && [ "$cur" != "1" ] && [ "$cur" != "0" ]; do
        [ "$cur" = "$NGINX_MASTER_PID" ] && return 0
        cur="$(ps -o ppid= -p "$cur" 2>/dev/null | tr -d '[:space:]' || true)"
        depth=$((depth+1))
      done
    done
  done < <(ss -H -l${proto}p "sport = :$port")
  return 1
}

# Reload the pinned instance only: prefer `nginx -s reload` when the pid file
# named by the pinned config resolves to the pinned master; otherwise fall
# back to an explicit SIGHUP to the pinned master PID. systemd is never used.
nginx_reload() {
  local dump pidfile pidval pidpath
  nginx_ctl -t || die "nginx -t failed on pinned instance ($NGINX_BIN -c $NGINX_CONF_PATH)"
  dump="$(mktemp)"
  if ! nginx_ctl -T >"$dump" 2>/dev/null; then
    rm -f "$dump"
    die "nginx -T failed on pinned instance; refusing to reload"
  fi
  pidfile="$(sed -n 's/^[[:space:]]*pid[[:space:]][[:space:]]*\([^;]*\);.*/\1/p' "$dump" | head -1)"
  rm -f "$dump"
  pidpath="$pidfile"
  case "$pidpath" in /*) ;; *) pidpath="$NGINX_DEFAULT_PREFIX/$pidpath" ;; esac
  if [ -n "$pidpath" ]; then
    if [ -r "$pidpath" ]; then
      pidval="$(cat "$pidpath" 2>/dev/null || true)"
      pidval="${pidval%%[!0-9]*}"
      if [ "$pidval" = "$NGINX_MASTER_PID" ]; then
        nginx_ctl -s reload \
          || die "nginx -s reload failed on pinned instance (master pid $NGINX_MASTER_PID)"
        log "nginx reloaded via pinned binary -s reload (pidfile pid=$pidval == pinned master)"
        nginx_master_check || die "pinned nginx master disappeared after reload"
        return 0
      fi
    fi
  fi
  nginx_master_check || die "pinned nginx master disappeared before reload"
  kill -HUP "$NGINX_MASTER_PID" \
    || die "failed to signal pinned nginx master (SIGHUP pid $NGINX_MASTER_PID)"
  log "nginx reloaded via direct SIGHUP to pinned master pid $NGINX_MASTER_PID (pidfile '$pidfile' missing or points elsewhere)"
  nginx_master_check || die "pinned nginx master disappeared after reload"
}

# After reload: the config the pinned instance would load must contain the
# managed stream entry and rewritten listeners.
nginx_effective_check() {
  local dump
  dump="$(mktemp)"
  if ! nginx_ctl -T >"$dump" 2>/dev/null; then
    rm -f "$dump"
    die "nginx -T failed during effective-config verification (pinned instance)"
  fi
  grep -Fq "# configuration file $NGINX_STREAM_CONF:" "$dump" \
    || { rm -f "$dump"; die "effective nginx config does not include stream conf $NGINX_STREAM_CONF (instance drift?)"; }
  grep -Fq "127.0.0.1:10443" "$dump" \
    || { rm -f "$dump"; die "effective nginx config has no 127.0.0.1:10443 listener"; }
  grep -Fq "$TURN_DOMAIN" "$dump" \
    || { rm -f "$dump"; die "effective nginx config does not route $TURN_DOMAIN"; }
  rm -f "$dump"
}

# Task-1 listen-drift checker (frozen contract): check_l4_sni_listen.sh
# [--strict|--pre-switch] [config_file...]; exit 0 healthy, nonzero = violation
# or undecidable -- both block the switch.
run_l4_check() {
  local rc=0
  [ -n "$L4_SNI_CHECK_SCRIPT" ] || die "internal error: L4_SNI_CHECK_SCRIPT unset"
  if [ ! -f "$L4_SNI_CHECK_SCRIPT" ]; then
    die "BLOCKED_ENV: listen-drift checker not found at $L4_SNI_CHECK_SCRIPT; refusing to proceed without the Task-1 check"
  fi
  [ -x "$L4_SNI_CHECK_SCRIPT" ] || die "listen-drift checker is not executable: $L4_SNI_CHECK_SCRIPT"
  "$L4_SNI_CHECK_SCRIPT" "$@" || rc=$?
  [ "$rc" -eq 0 ] || die "listen-drift check failed (mode=${1:-?} rc=$rc): $L4_SNI_CHECK_SCRIPT $*"
}

managed_vhost_paths() {
  local file
  for file in $HTTPS_VHOST_FILES; do printf '%s\n' "$NGINX_VHOST_DIR/$file"; done
}

compose_config_to() {
  local out="$1" envf="$2"; shift 2
  local -a f=() p
  for p in "$@"; do f+=(-f "$p"); done
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$envf" ${f[@]+"${f[@]}"} config) >"$out"
}

compose_up() {
  local envf="$1"; shift
  local -a f=() p
  for p in "$@"; do f+=(-f "$p"); done
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$envf" ${f[@]+"${f[@]}"} \
    up -d --force-recreate "$LIVEKIT_SERVICE")
}

validate_nginx() {
  local file
  nginx_ctl -t || die "pinned nginx config fails -t ($NGINX_CONF_PATH)"
  nginx_master_check || die "pinned nginx master is not running"
  nginx_owns_port 443 t || die "public TCP/443 is not currently owned by the pinned nginx instance (pid $NGINX_MASTER_PID)"
  for file in $HTTPS_VHOST_FILES; do
    [ -f "$NGINX_VHOST_DIR/$file" ] || die "Nginx vhost not found: $NGINX_VHOST_DIR/$file"
    grep -qE '^\s*listen\s+(443|\[::\]:443)(\s|;)' "$NGINX_VHOST_DIR/$file" || \
      grep -qE '^\s*listen\s+(127\.0\.0\.1:10443|\[::1\]:10443)(\s|;)' "$NGINX_VHOST_DIR/$file" || \
      die "no HTTPS listener found in $file"
  done
  # Read-only Task-1 gate on the current (pre-switch) config.
  run_l4_check --pre-switch $(managed_vhost_paths)
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
  resolve_nginx_instance
  validate_domain
  validate_certificate
  validate_nginx
  validate_livekit
  validate_old_turn
  log "CHECK_PASS: prerequisites, certificate, pinned nginx instance ($NGINX_BIN_REAL pid $NGINX_MASTER_PID), Compose, LiveKit and pre-switch listen check are valid"
}

docker_gateway_cidr() {
  local gateways gateway count
  if [ -n "${LIVEKIT_PROXY_TRUSTED_IP:-}" ]; then
    gateway="$LIVEKIT_PROXY_TRUSTED_IP"
  else
    gateways="$(docker inspect -f '{{range .NetworkSettings.Networks}}{{println .Gateway}}{{end}}' "$LIVEKIT_CONTAINER" 2>/dev/null \
      | awk 'NF && !seen[$0]++' || true)"
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

# Backup: manifest + per-file sha256 + service-state snapshot (restore contract).

backup_configs() {
  local stamp path sha mode rel
  local -a paths
  stamp="$(date -u +%Y%m%dT%H%M%SZ)"
  CURRENT_BACKUP="$L4_BACKUP_ROOT/$stamp"
  mkdir -p "$CURRENT_BACKUP/files" "$L4_STATE_DIR"
  chmod 700 "$CURRENT_BACKUP" "$L4_STATE_DIR"
  paths=("$HAPROXY_CONF" "$COMPOSE_FILE" "$COMPOSE_ENV_FILE")
  for path in $HTTPS_VHOST_FILES; do paths+=("$NGINX_VHOST_DIR/$path"); done
  for path in "$NGINX_STREAM_CONF" "$NGINX_REALIP_CONF" "$CERTBOT_HOOK" "$GENERATED_COMPOSE"; do
    if [ -e "$path" ]; then paths+=("$path"); else printf '%s\n' "$path" >>"$CURRENT_BACKUP/created-paths"; fi
  done
  : >"$CURRENT_BACKUP/manifest.txt"
  for path in "${paths[@]}"; do
    [ -f "$path" ] || die "backup source is not a regular file: $path"
    rel="${path#/}"
    mkdir -p "$CURRENT_BACKUP/files/$(dirname "$rel")"
    cp -p "$path" "$CURRENT_BACKUP/files/$rel"
    sha="$(sha256sum "$path" | awk '{print $1}')"
    mode="$(stat -c %a "$path")"
    printf 'file %s %s %s\n' "$sha" "$mode" "$path" >>"$CURRENT_BACKUP/manifest.txt"
  done
  {
    printf 'format=2\ncreated=%s\nnginx.bin=%s\nnginx.master_pid=%s\nnginx.conf=%s\n' \
      "$stamp" "$NGINX_BIN_REAL" "$NGINX_MASTER_PID" "$NGINX_CONF_PATH"
    printf 'eturnal.enabled='; systemctl is-enabled "$ETURNAL_SERVICE" 2>/dev/null || true
    printf 'eturnal.active='; systemctl is-active "$ETURNAL_SERVICE" 2>/dev/null || true
    printf 'haproxy.active='; systemctl is-active haproxy 2>/dev/null || true
    printf 'compose.base=%s\n' "$COMPOSE_FILE"
    for path in ${COMPOSE_OVERLAYS[@]+"${COMPOSE_OVERLAYS[@]}"}; do
      printf 'compose.overlay=%s\n' "$path"
    done
    if [ -e "$GENERATED_COMPOSE" ]; then printf 'compose.generated_pre_existing=1\n'
    else printf 'compose.generated_pre_existing=0\n'; fi
  } >"$CURRENT_BACKUP/service-state.txt"
  docker inspect "$LIVEKIT_CONTAINER" >"$CURRENT_BACKUP/livekit-inspect.json" 2>/dev/null || printf '{}\n' >"$CURRENT_BACKUP/livekit-inspect.json"
  nginx_ctl -T >"$CURRENT_BACKUP/nginx-T.txt" 2>&1
  local -a pre_combo=("${COMPOSE_PRE_FILES[@]}")
  [ -e "$GENERATED_COMPOSE" ] && pre_combo+=("$GENERATED_COMPOSE")
  compose_config_to "$CURRENT_BACKUP/compose-effective-pre.txt" "$COMPOSE_ENV_FILE" ${pre_combo[@]+"${pre_combo[@]}"}
  chmod -R go-rwx "$CURRENT_BACKUP"
  log "backup created (manifest + per-file sha256 + service-state): $CURRENT_BACKUP"
}

render_configs() {
  local stage="$CURRENT_BACKUP/staged" gateway_cidr
  local -a f=() p
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
  # Validate the FULL switch combination (base + operator overlays + staged overlay).
  for p in ${COMPOSE_PRE_FILES[@]+"${COMPOSE_PRE_FILES[@]}"} "$stage/compose.yml"; do f+=(-f "$p"); done
  (cd "$COMPOSE_DIR" && "${COMPOSE[@]}" --env-file "$COMPOSE_ENV_FILE" ${f[@]+"${f[@]}"} config --quiet)
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
  compose_up "$COMPOSE_ENV_FILE" ${COMPOSE_SWITCH_FILES[@]+"${COMPOSE_SWITCH_FILES[@]}"}
  wait_livekit

  install -m 0644 "$stage/haproxy.cfg" "$HAPROXY_CONF"
  haproxy -c -f "$HAPROXY_CONF"
  systemctl restart haproxy && systemctl is-active --quiet haproxy

  for file in $HTTPS_VHOST_FILES; do install -m 0644 "$stage/vhosts/$file" "$NGINX_VHOST_DIR/$file"; done
  install -D -m 0644 "$stage/stream.conf" "$NGINX_STREAM_CONF"
  install -D -m 0644 "$stage/realip.conf" "$NGINX_REALIP_CONF"

  # Reload closed loop on the pinned instance only: candidate syntax check,
  # Task-1 strict listen check, reload (verify_switch re-checks afterwards).
  nginx_ctl -t || die "candidate nginx config failed -t (pinned $NGINX_BIN)"
  run_l4_check --strict $(managed_vhost_paths)
  nginx_reload

  install -D -m 0755 "$SCRIPT_DIR/nginx/livekit-turn-cert-deploy-hook.sh" "$CERTBOT_HOOK"
}

verify_https_contracts() {
  local contract domain expected actual
  for contract in ${HTTPS_HEALTH_CONTRACTS:-}; do
    domain="${contract%:*}"; expected="${contract##*:}"
    [[ "$expected" =~ ^[0-9]{3}$ ]] || die "invalid HTTPS contract: $contract"
    actual="$(curl --noproxy '*' -skS --resolve "$domain:443:127.0.0.1" \
      -o /dev/null -w '%{http_code}' --connect-timeout 4 --max-time 8 "https://$domain/" || true)"
    [ "$actual" = "$expected" ] || die "$domain returned $actual, expected $expected"
  done
}

verify_switch() {
  # Instance consistency first: never infer nginx health from a systemd unit.
  nginx_instance_verify
  systemctl is-active --quiet haproxy || die "haproxy is not active"
  if systemctl is-active --quiet "$ETURNAL_SERVICE"; then
    die "$ETURNAL_SERVICE unexpectedly became active"
  fi
  nginx_owns_port 443 t || die "TCP/443 is not owned by the pinned nginx instance (pid $NGINX_MASTER_PID)"
  nginx_owns_port 10443 t || die "TCP/10443 is not owned by the pinned nginx instance (pid $NGINX_MASTER_PID)"
  ss -H -lntp 'sport = :15442' | grep -q haproxy || die "TCP/15442 is not owned by haproxy"
  ss -H -lntp 'sport = :15443' | grep -q docker-proxy || die "TCP/15443 is not owned by docker-proxy"
  ss -H -lnup 'sport = :3478' | grep -q docker-proxy || die "UDP/3478 is not owned by docker-proxy"
  ss -H -lnup 'sport = :50201' | grep -q docker-proxy || die "UDP/50201 is not owned by docker-proxy"
  ss -H -lnup 'sport = :50500' | grep -q docker-proxy || die "UDP/50500 is not owned by docker-proxy"
  if grep -qE '^\s*server livekit .* check\s*$' "$HAPROXY_CONF"; then
    die "HAProxy bare TCP health check must remain disabled"
  fi
  # Post-reload: effective config of the pinned instance + strict listen check.
  nginx_effective_check
  run_l4_check --strict $(managed_vhost_paths)
  verify_https_contracts
  if [ -n "$BACKEND_HEALTH_URL" ]; then
    curl --noproxy '*' -fsS --connect-timeout 3 --max-time 6 "$BACKEND_HEALTH_URL" | grep -q '"status":"ok"'
  fi
  openssl s_client -connect 127.0.0.1:443 -servername "$TURN_DOMAIN" \
    -verify_hostname "$TURN_DOMAIN" -CAfile /etc/ssl/certs/ca-certificates.crt \
    -brief </dev/null 2>&1 | grep -q 'Verification: OK'
  docker logs "$LIVEKIT_CONTAINER" 2>&1 | grep -q 'Starting TURN server'
  docker logs "$LIVEKIT_CONTAINER" 2>&1 | grep -q '"turn.proxyProtocol": true'
  log "VERIFY_PASS: pinned nginx instance, ports owner, effective config, HTTPS baselines, backend, TURN TLS and PROXY protocol"
}

restore_backup() {
  local backup="$1" tag sha mode path rel value base ovl gen_pre enabled active
  local haproxy_was hrc harc degraded
  local -a combo=()
  [ -d "$backup" ] || die "backup directory not found: $backup"
  [ -f "$backup/manifest.txt" ] || die "backup manifest missing: $backup/manifest.txt"
  # Verify every stored file against its recorded per-file sha256 first.
  while read -r tag sha mode path; do
    [ "$tag" = file ] || continue
    rel="${path#/}"
    [ -f "$backup/files/$rel" ] || die "backup store is missing stored copy of $path"
    [ "$(sha256sum "$backup/files/$rel" | awk '{print $1}')" = "$sha" ] \
      || die "backup per-file checksum mismatch for $path (stored copy is corrupted)"
  done <"$backup/manifest.txt"
  # Instance consistency: refuse cross-instance restore.
  if [ -f "$backup/service-state.txt" ]; then
    value="$(sed -n 's/^nginx\.bin=//p' "$backup/service-state.txt" | head -1)"
    if [ -n "$value" ] && [ "$value" != "$NGINX_BIN_REAL" ]; then
      die "BLOCKED_ENV: backup was taken on nginx binary $value but the pinned instance is $NGINX_BIN_REAL; refusing a cross-instance restore"
    fi
    value="$(sed -n 's/^nginx\.conf=//p' "$backup/service-state.txt" | head -1)"
    if [ -n "$value" ] && [ "$value" != "$NGINX_CONF_PATH" ]; then
      die "BLOCKED_ENV: backup was taken on nginx conf $value but the pinned instance uses $NGINX_CONF_PATH; refusing a cross-instance restore"
    fi
  fi
  # Remove paths this installer created, restore every backed-up file.
  if [ -f "$backup/created-paths" ]; then
    while IFS= read -r path; do [ -n "$path" ] && rm -f -- "$path"; done <"$backup/created-paths"
  fi
  while read -r tag sha mode path; do
    [ "$tag" = file ] || continue
    mkdir -p "$(dirname "$path")"
    cp "$backup/files/${path#/}" "$path"
    chmod "$mode" "$path"
    [ "$(sha256sum "$path" | awk '{print $1}')" = "$sha" ] \
      || die "restored file does not match its recorded checksum: $path"
  done <"$backup/manifest.txt"
  log "restored $(grep -c '^file ' "$backup/manifest.txt") backed-up files (per-file checksums verified)"

  nginx_ctl -t || die "restored nginx config fails -t on the pinned instance"
  run_l4_check --pre-switch $(managed_vhost_paths)
  nginx_reload
  nginx_instance_verify
  # HAProxy: honor the pre-switch service-state snapshot instead of a blind
  # restart. A failure here degrades the rollback (explicit diagnostics with
  # the failing command and exit code + ROLLBACK_DEGRADED + nonzero result)
  # but never aborts the remaining restore steps.
  haproxy_was="$(sed -n 's/^haproxy\.active=//p' "$backup/service-state.txt" 2>/dev/null | head -1)"
  degraded=0
  case "$haproxy_was" in
    active)
      if systemctl restart haproxy; then
        systemctl is-active --quiet haproxy || {
          harc=$?
          printf '[livekit-l4-sni] ERROR: ROLLBACK_DEGRADED: systemctl is-active --quiet haproxy (rc=%d): haproxy is not active while the snapshot recorded haproxy.active=active\n' "$harc" >&2
          degraded=1
        }
      else
        hrc=$?
        printf '[livekit-l4-sni] ERROR: ROLLBACK_DEGRADED: systemctl restart haproxy failed (rc=%d); snapshot recorded haproxy.active=active\n' "$hrc" >&2
        degraded=1
      fi
      ;;
    "")
      # Early format=2 backups carry no haproxy.active record: keep the old
      # best-effort restart (diagnosed, never silent) but skip verification.
      systemctl restart haproxy || {
        hrc=$?
        printf '[livekit-l4-sni] ERROR: ROLLBACK_DEGRADED: systemctl restart haproxy failed (rc=%d); snapshot has no haproxy.active record (state verification skipped)\n' "$hrc" >&2
        degraded=1
      }
      ;;
    *)
      systemctl stop haproxy || true
      if systemctl is-active --quiet haproxy; then
        printf '[livekit-l4-sni] ERROR: ROLLBACK_DEGRADED: haproxy is still active after rollback; snapshot recorded haproxy.active=%s\n' "$haproxy_was" >&2
        degraded=1
      fi
      ;;
  esac

  # Rebuild the recorded Compose combination -- never base-only when the
  # pre-switch stack included overlays.
  base="$(sed -n 's/^compose\.base=//p' "$backup/service-state.txt" 2>/dev/null | head -1)"
  [ -n "$base" ] || base="$COMPOSE_FILE"
  while IFS= read -r ovl; do [ -n "$ovl" ] && [ -f "$ovl" ] && combo+=("$ovl"); done \
    < <(sed -n 's/^compose\.overlay=//p' "$backup/service-state.txt" 2>/dev/null)
  gen_pre="$(sed -n 's/^compose\.generated_pre_existing=//p' "$backup/service-state.txt" 2>/dev/null | head -1)"
  if [ "$gen_pre" = 1 ] && [ -f "$GENERATED_COMPOSE" ]; then combo+=("$GENERATED_COMPOSE"); fi
  compose_up "$COMPOSE_ENV_FILE" "$base" ${combo[@]+"${combo[@]}"}
  if [ -f "$backup/compose-effective-pre.txt" ]; then
    compose_config_to "$backup/compose-effective-post.txt" "$COMPOSE_ENV_FILE" "$base" ${combo[@]+"${combo[@]}"}
    [ "$(sha256sum "$backup/compose-effective-pre.txt" | awk '{print $1}')" = \
      "$(sha256sum "$backup/compose-effective-post.txt" | awk '{print $1}')" ] \
      || die "restored Compose effective configuration differs from the pre-switch snapshot; combination mismatch (base=$base overlays=${combo[*]:-none})"
    log "restore verified: rebuilt Compose combination (base + ${#combo[@]} overlay file(s)) matches the pre-switch effective configuration"
  fi
  wait_livekit
  enabled="$(sed -n 's/^eturnal\.enabled=//p' "$backup/service-state.txt" 2>/dev/null | head -1)"
  active="$(sed -n 's/^eturnal\.active=//p' "$backup/service-state.txt" 2>/dev/null | head -1)"
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
  if [ "$degraded" = 1 ]; then
    log "ROLLBACK_DEGRADED: restored $backup but haproxy did not return to its snapshot state; see ERROR diagnostics above and recover haproxy manually (HAProxy package remains installed)"
    return 1
  fi
  log "ROLLBACK_COMPLETE: restored $backup; HAProxy package remains installed"
}

on_error() {
  local rc=$?
  trap - ERR
  # A failure inside a command-substitution subshell must not run a nested rollback.
  if [ -n "${BASHPID:-}" ] && [ "$BASHPID" != "$$" ]; then
    exit "$rc"
  fi
  if [ "$MUTATION_STARTED" -eq 1 ] && [ -n "$CURRENT_BACKUP" ]; then
    log "apply failed (rc=$rc); starting automatic rollback"
    log "if rollback is interrupted, run: $0 --rollback --config $CONFIG_FILE --backup $CURRENT_BACKUP"
    if ! restore_backup "$CURRENT_BACKUP"; then
      log "AUTOMATIC ROLLBACK FAILED; run manually: $0 --rollback --config $CONFIG_FILE --backup $CURRENT_BACKUP"
    fi
  fi
  exit "$rc"
}

apply_switch() {
  # Duplicate-apply protection: intentionally retained (Task 3 keeps it; the
  # installer never takes over an SNI switch enabled outside of it).
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
  resolve_nginx_instance
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
