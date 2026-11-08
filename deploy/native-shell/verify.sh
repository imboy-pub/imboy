#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
    printf 'Usage: sudo bash verify.sh [--s3-write]\nDefault: read-only local readiness/TLS/schema/admin checks. --s3-write writes, reads, compares and deletes a unique synthetic object.\n'; exit 0
fi
write=no
if [[ ${1:-} == --s3-write && $# == 1 ]]; then write=yes; elif (($#)); then die 2 ARG 'Unknown verify option.'; fi
require_root; lock_operation
load_site "$CONF_DIR/site.env"
read_kv "$CONF_DIR/release.env" META "$META_KEYS"
source "$HERE/lib/services.sh"
failures=0
pass() { printf 'PASS\t%s\n' "$1"; }
fail() { printf 'FAIL\t%s\n' "$1"; failures=$((failures+1)); }
for unit in imboy-postgresql imboy-garage imboy-backend nginx; do
    if systemctl is-active --quiet "$unit"; then pass "service:$unit"; else fail "service:$unit"; fi
done
body=$(curl --noproxy '*' -fsS --max-time 8 "http://127.0.0.1:$HTTP_PORT/healthz" 2>/dev/null || true)
if jq -e --arg v "${META[BACKEND_VERSION]}" '.status=="ok" and .db=="up" and .tsid=="ready" and .version==$v' <<< "$body" >/dev/null 2>&1; then pass 'backend:readiness-and-version'; else fail 'backend:readiness-and-version'; fi
if psql_admin imboy -Atqc 'SELECT 1' >/dev/null 2>&1; then pass 'postgres:sql'; else fail 'postgres:sql'; fi
extensions=$(psql_admin imboy -Atqc 'SELECT extname FROM pg_extension' 2>/dev/null || true)
for ext in pgcrypto pg_jieba timescaledb vector postgis pg_trgm btree_gin btree_gist citext unaccent intarray pg_stat_statements; do
    if grep -qx "$ext" <<< "$extensions"; then pass "extension:$ext"; else fail "extension:$ext"; fi
done
head=$(psql_admin imboy -Atqc 'SELECT max(version) FROM schema_migrations_history' 2>/dev/null || true)
if [[ $head == "${META[MIGRATION_HEAD]}" ]]; then pass 'schema:migration-head'; else fail 'schema:migration-head'; fi
super=$(psql_admin postgres -Atqc "SELECT rolsuper FROM pg_roles WHERE rolname='imboy'" 2>/dev/null || true)
if [[ $super == f ]]; then pass 'postgres:runtime-role-not-superuser'; else fail 'postgres:runtime-role-not-superuser'; fi
admins=$(psql_admin imboy -Atqc 'SELECT count(*) FROM public.adm_user' 2>/dev/null || true)
if [[ $admins =~ ^[0-9]+$ ]] && ((admins>0)); then pass 'admin:initialized'; else fail 'admin:initialized'; fi
for role in api s3 files; do
    case $role in api) domain=$API_DOMAIN; path=/healthz; expected=200;; s3) domain=$S3_DOMAIN; path=/; expected=403;; files) domain=$FILES_DOMAIN; path=/__imboy_absent_probe__; expected=404;; esac
    code=$(curl --noproxy '*' --resolve "$domain:443:127.0.0.1" -sS --max-time 10 -o /dev/null -w '%{http_code}' "https://$domain$path" 2>/dev/null || true)
    if [[ $code == "$expected" ]]; then pass "tls-vhost:$role"; else fail "tls-vhost:$role"; fi
    if getent ahosts "$domain" >/dev/null 2>&1; then pass "dns:resolves:$role"; else fail "dns:resolves:$role"; fi
done
code=$(curl --noproxy '*' --resolve "$API_DOMAIN:443:127.0.0.1" -sS --max-time 10 -o /dev/null -w '%{http_code}' "https://$API_DOMAIN/api/adm/setup/status" 2>/dev/null || true)
if [[ $code == 403 ]]; then pass 'admin:public-bootstrap-blocked'; else fail 'admin:public-bootstrap-blocked'; fi
if [[ $write == yes ]]; then
    load_secrets
    testdir=$(mktemp -d "$STATE_DIR/.s3-probe.XXXXXX")
    object="_delivery-probe/$(date -u +%Y%m%dT%H%M%SZ)-$(openssl rand -hex 8).txt"
    printf 'IMBoy synthetic deployment acceptance object\n' > "$testdir/input"
    cleanup() { s3_request DELETE "$PRIVATE_BUCKET/$object" '' /dev/null "https://$S3_DOMAIN" >/dev/null 2>&1 || printf 'WARN: probe object cleanup needs retry: %s\n' "$object" >&2; rm -f "$testdir/input" "$testdir/output"; rmdir "$testdir"; }
    trap cleanup EXIT
    if s3_request PUT "$PRIVATE_BUCKET/$object" "$testdir/input" /dev/null "https://$S3_DOMAIN" && s3_request GET "$PRIVATE_BUCKET/$object" '' "$testdir/output" "https://$S3_DOMAIN" && cmp -s "$testdir/input" "$testdir/output"; then pass 's3:tls-signed-put-get-compare'; else fail 's3:tls-signed-put-get-compare'; fi
    if s3_request DELETE "$PRIVATE_BUCKET/$object" '' /dev/null "https://$S3_DOMAIN"; then pass 's3:tls-signed-delete'; else fail 's3:tls-signed-delete'; fi
else printf 'NOT_RUN\ts3:write-read-delete (run --s3-write)\n'; fi
printf 'NOT_RUN\tbusiness:login-two-clients-message-attachment-restart-restore-upgrade\n'
printf 'NOT_RUN\tnetwork:independent-external-client-and-firewall-check\n'
((failures==0)) || exit 5
