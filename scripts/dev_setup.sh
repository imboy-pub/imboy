#!/usr/bin/env bash
# 根 Compose 本地开发入口 / Local Compose setup; preserves existing credentials/data.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

usage() {
  cat <<'HELP'
Usage: bash scripts/dev_setup.sh [options]
  --infra-only       PostgreSQL + Garage + gateway; route API to the host
  --with-livekit     Add local LiveKit signaling/media
  --with-widget      Build and start the hosted Widget/Seat static service
  --with-garage      Compatibility alias (Garage is always included)
  --profile NAME     Add dev|pro|api|admin|livekit|widget
  --edition VALUE    community|professional|enterprise (license still required)
  --no-build         Use prepared/cached images; do not build application images
  -h, --help         Show this help

Default: dev + api + admin. First run copies .env.example and asks for both PG
passwords. Later runs preserve .env, keys and volumes. No production deployment.
HELP
}
die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }
step() { printf '\n==> %s\n' "$*"; }
INFRA_ONLY=0
BUILD=1
EXTRA_PROFILES=()
while (($#)); do
  case "$1" in
    --infra-only) INFRA_ONLY=1; shift ;;
    --no-build) BUILD=0; shift ;;
    --with-garage) shift ;;
    --with-livekit) EXTRA_PROFILES+=(livekit); shift ;;
    --with-widget) EXTRA_PROFILES+=(widget); shift ;;
    --profile)
      [[ $# -ge 2 ]] || die '--profile needs a value'
      case "$2" in dev|pro|api|admin|livekit|widget) EXTRA_PROFILES+=("$2");; *) die "Unknown profile: $2";; esac
      shift 2 ;;
    --edition)
      [[ $# -ge 2 ]] || die '--edition needs a value'
      case "$2" in community|professional|enterprise) export IMBOY_EDITION="$2";; *) die "Unknown edition: $2";; esac
      shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) die "Unknown option: $1 (see --help)" ;;
  esac
done
command -v docker >/dev/null || die 'Install Docker with Compose 2.23.1+ first'
command -v openssl >/dev/null || die 'openssl is required for local internal keys'
version="$(docker compose version --short)"
version="${version#v}"
IFS=. read -r major minor patch <<< "${version%%-*}"
[[ "$major" =~ ^[0-9]+$ && "$minor" =~ ^[0-9]+$ && "$patch" =~ ^[0-9]+$ ]] || die "Unknown Compose version: $version"
((major > 2 || (major == 2 && (minor > 23 || (minor == 23 && patch >= 1))))) || die 'Compose 2.23.1+ is required'
docker info >/dev/null 2>&1 || die 'Docker daemon is unavailable'

if [[ ! -f .env ]]; then
  (umask 077; cp .env.example .env)
  printf 'Created .env. Set IMBOY_PG_PASSWORD and IMBOY_PG_PRO_PASSWORD, then rerun.\n'
  exit 1
fi

# Only detect absent/empty assignments; never execute .env as shell code. Compose
# owns interpolation (including shell overrides, quotes and dollar signs).
# Existing nonempty values are not rewritten, even if they look like placeholders.
ensure_secret() {
  local key="$1" value prefix="${2:-}" generated secret_file
  [[ -n "${!key:-}" ]] && return 0
  value="$(awk -v key="$key" '
    $0 ~ "^[[:space:]]*" key "[[:space:]]*=" {
      v=$0; sub(/^[^=]*=/,"",v); sub(/\r$/,"",v)
      gsub(/^[[:space:]]+|[[:space:]]+$/,"",v)
      sub(/[[:space:]]+#.*$/,"",v)
    } END {print v}' .env)"
  case "$value" in ''|'""'|"''"|\#*)
    generated="${prefix}$(openssl rand -hex 32)"
    secret_file="$(mktemp "$ROOT/.env.XXXXXX")"
    # Fill the last empty assignment where it occurs, so later ${KEY} aliases
    # also work when start_node.sh sources this trusted local file in order.
    # If the key is absent, define it before any possible references.
    awk -v key="$key" -v generated="$generated" '
      { lines[NR]=$0 }
      $0 ~ "^[[:space:]]*" key "[[:space:]]*=" { last=NR }
      END {
        if (!last) print key "=" generated
        for (i=1; i<=NR; i++) print (i==last ? key "=" generated : lines[i])
      }' .env > "$secret_file"
    chmod 600 "$secret_file"
    mv "$secret_file" .env ;;
  esac
}
chmod 600 .env
ensure_secret GARAGE_RPC_SECRET
ensure_secret IMBOY_GARAGE_ACCESS_KEY GK
ensure_secret IMBOY_GARAGE_SECRET_KEY
ensure_secret IMBOY_EDGE_INTERNAL_TOKEN
ensure_secret LIVEKIT_API_SECRET

# Never let an ambient COMPOSE_PROFILES silently start the separate pro database.
export COMPOSE_PROFILES=
PROFILES=(dev)
if ((INFRA_ONLY)); then
  export API_UPSTREAM="${API_UPSTREAM:-host.docker.internal:9800}"
else
  PROFILES+=(api admin)
fi
PROFILES+=("${EXTRA_PROFILES[@]}")
COMPOSE=(docker compose --project-directory "$ROOT" --env-file "$ROOT/.env" -f "$ROOT/docker-compose.yml")
for profile in "${PROFILES[@]}"; do COMPOSE+=(--profile "$profile"); done
"${COMPOSE[@]}" config --quiet
# Check resolved values without sourcing .env. Compose 2.23.1 has no
# config --environment, so inspect only empty fields in its rendered YAML.
# Include api for this read-only validation even when starting infrastructure only.
missing_keys="$("${COMPOSE[@]}" --profile api config | awk '
  $1 ~ /^(POSTGRES_PASSWORD|GARAGE_DEFAULT_ACCESS_KEY|GARAGE_DEFAULT_SECRET_KEY|IMBOY_EDGE_INTERNAL_TOKEN|IMBOY_LIVEKIT_API_SECRET):$/ &&
      (NF == 1 || $2 == "\"\"" || $2 == "\047\047") { sub(/:$/, "", $1); print $1 }
  $1 == "rpc_secret" && $2 == "=" && $3 == "\"\"" { print "GARAGE_RPC_SECRET" }
')"
[[ -z "$missing_keys" ]] || die "Local internal keys resolve empty: $missing_keys; check .env references and rerun"
if [[ " ${PROFILES[*]} " == *' pro '* ]]; then
  docker network inspect imboy-network >/dev/null 2>&1 || docker network create imboy-network >/dev/null
fi

step 'Starting PostgreSQL, Garage and the local gateway'
"${COMPOSE[@]}" up -d --wait --wait-timeout 180 imboy_pg18 garage imboy_nginx

# Read actual container settings after Compose interpolation; do not reparse secrets.
container_env() {
  local id
  id="$("${COMPOSE[@]}" ps -q "$1")"
  [[ -n "$id" ]] || die "No running container for $1"
  docker inspect --format '{{range .Config.Env}}{{println .}}{{end}}' "$id" |
    awk -v key="$2" 'index($0,key "=")==1 {print substr($0,length(key)+2); exit}'
}
step 'Initializing private/public Garage buckets (existing data is retained)'
access_key="$(container_env garage GARAGE_DEFAULT_ACCESS_KEY)"
private_bucket="$(container_env garage GARAGE_DEFAULT_BUCKET)"
public_bucket="$(container_env garage GARAGE_PUBLIC_BUCKET)"
[[ -n "$access_key" && -n "$private_bucket" && -n "$public_bucket" ]] || die 'Garage initialization settings are missing'
[[ "$private_bucket" != "$public_bucket" ]] || die 'Garage private and public buckets must differ'
"${COMPOSE[@]}" exec -T garage /garage bucket info "$public_bucket" >/dev/null 2>&1 ||
  "${COMPOSE[@]}" exec -T garage /garage bucket create "$public_bucket"
"${COMPOSE[@]}" exec -T garage /garage bucket allow "$public_bucket" --read --write --owner --key "$access_key"
"${COMPOSE[@]}" exec -T garage /garage bucket website --deny "$private_bucket"
"${COMPOSE[@]}" exec -T garage /garage bucket website --allow "$public_bucket"

if ((INFRA_ONLY)); then
  if [[ ! -f config/sys.local.config ]]; then
    cp config/sys.config.example config/sys.local.config
  fi
fi

step 'Starting selected services and waiting for health checks'
if ((BUILD)); then
  for profile in "${PROFILES[@]}"; do
    case "$profile" in api|admin|widget) "${COMPOSE[@]}" build; break ;; esac
  done
fi
"${COMPOSE[@]}" up -d --no-build --wait --wait-timeout 300

# LiveKit has no image healthcheck; retry HTTP readiness after it enters running.
wait_gateway() {
  local origin_var="$1" path="$2" attempt
  for attempt in {1..20}; do
    if "${COMPOSE[@]}" exec -T imboy_nginx sh -ec \
      'origin=$(printenv "$1"); wget -q -T 5 -O /dev/null --header="Host: $origin" "http://127.0.0.1$2"' \
      sh "$origin_var" "$path"; then return 0; fi
    sleep 2
  done
  die "Gateway readiness failed: $origin_var $path; inspect compose logs"
}
[[ " ${PROFILES[*]} " != *' api '* ]] || wait_gateway API_DOMAIN /readyz
[[ " ${PROFILES[*]} " != *' admin '* ]] || wait_gateway ADMIN_DOMAIN /health
[[ " ${PROFILES[*]} " != *' widget '* ]] || wait_gateway CS_WIDGET_DOMAIN /health.txt
[[ " ${PROFILES[*]} " != *' livekit '* ]] || wait_gateway RTC_DOMAIN /

step 'Selected services are ready'
"${COMPOSE[@]}" ps
if ((INFRA_ONLY)); then
  printf '\nHost backend: check config/sys.local.config, then IMBOYENV=local make rel && IMBOYENV=local bash scripts/start_node.sh imboy_local\n'
fi
printf '\nURLs, optional services and safe stop commands: docs/guides/operations/deployment/local-development.md\n'
