#!/usr/bin/env bash
# TLS bootstrap control-flow regression. Docker/ACME/OpenSSL are local stubs;
# these checks do not prove real Nginx startup, certificate trust or WSS/media.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf -- "$TMP"' EXIT
BIN="$TMP/bin"
mkdir -p "$BIN"
passed=0
fail() { printf '[FAIL] %s\n' "$*" >&2; cat "$OUTPUT" >&2; exit 1; }
pass() { passed=$((passed + 1)); printf '[PASS] %s\n' "$*"; }

cat > "$BIN/openssl" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  rand) printf '0123456789abcdef01234567\n' ;;
  req)
    key= cert= domain=
    while [[ $# -gt 0 ]]; do
      case "$1" in
        -keyout) key="$2"; shift ;;
        -out) cert="$2"; shift ;;
        -subj) domain="${2#/CN=}"; shift ;;
      esac
      shift
    done
    [[ -n "$key" && -n "$cert" && -n "$domain" ]]
    printf 'temporary key for %s\n' "$domain" > "$key"
    printf 'temporary cert for %s\n' "$domain" > "$cert"
    printf 'self-signed %s\n' "$domain" >> "$MOCK_LOG"
    ;;
  *) printf 'Unexpected OpenSSL invocation\n' >&2; exit 2 ;;
esac
MOCK
cat > "$BIN/sleep" <<'MOCK'
#!/bin/sh
# Exercise the bounded retry loop without waiting in an offline test.
exit 0
MOCK
cat > "$BIN/docker" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
printf 'docker %s\n' "$*" >> "$MOCK_LOG"
case " $* " in
  *' up -d '*) printf 'start\n' >> "$MOCK_LOG" ;;
  *' exec -T imboy_nginx wget '*)
    domain= url=
    for arg in "$@"; do
      case "$arg" in
        '--header=Host: '*) domain="${arg#--header=Host: }" ;;
        http://127.0.0.1/*) url="$arg" ;;
      esac
    done
    [[ -n "$domain" && -n "$url" ]] || exit 2
    printf 'probe %s\n' "$domain" >> "$MOCK_LOG"
    [[ "${MOCK_PROBE_FAIL:-}" != "$domain" ]] || exit 1
    if [[ "${MOCK_PROBE_WRONG_BODY:-}" = "$domain" ]]; then
      printf 'an unrelated default server\n'
      exit 0
    fi
    cat "${DATA_DIR:-./data}/certbot/www/${url#http://127.0.0.1/}"
    ;;
  *' exec -T imboy_nginx nginx -t '*)
    printf 'nginx-test\n' >> "$MOCK_LOG"
    count=0; [[ ! -f "$MOCK_LOG.test-count" ]] || read -r count < "$MOCK_LOG.test-count"
    count=$((count + 1)); printf '%s\n' "$count" > "$MOCK_LOG.test-count"
    [[ "${MOCK_NGINX_TEST_FAIL_AT:-0}" != "$count" ]]
    ;;
  *' exec -T imboy_nginx nginx -s reload '*) printf 'reload\n' >> "$MOCK_LOG" ;;
  *' run '*" imboy_certbot "*)
    domain= previous=
    for arg in "$@"; do
      [[ "$previous" != -d ]] || domain="$arg"
      previous="$arg"
    done
    [[ -n "$domain" ]] || exit 2
    printf 'certbot %s\n' "$domain" >> "$MOCK_LOG"
    conf="${DATA_DIR:-./data}/certbot/conf"
    if [[ "${MOCK_CERTBOT_PARTIAL:-}" = "$domain" ]]; then
      mkdir -p "$conf/archive/$domain" "$conf/renewal"
      printf 'partial Certbot state\n' > "$conf/archive/$domain/fullchain1.pem"
      printf 'partial renewal state\n' > "$conf/renewal/$domain.conf"
      exit 1
    fi
    [[ "${MOCK_CERTBOT_FAIL:-}" != "$domain" ]] || exit 1
    [[ "${MOCK_CERTBOT_NO_FILES:-}" != "$domain" ]] || exit 0
    mkdir -p "$conf/live/$domain" "$conf/archive/$domain" "$conf/renewal"
    printf 'issued cert for %s\n' "$domain" > "$conf/archive/$domain/fullchain1.pem"
    printf 'issued key for %s\n' "$domain" > "$conf/archive/$domain/privkey1.pem"
    ln -s "../../archive/$domain/fullchain1.pem" "$conf/live/$domain/fullchain.pem"
    ln -s "../../archive/$domain/privkey1.pem" "$conf/live/$domain/privkey.pem"
    printf 'renewal config for %s\n' "$domain" > "$conf/renewal/$domain.conf"
    ;;
  *) printf 'Unexpected Docker invocation: %s\n' "$*" >&2; exit 2 ;;
esac
MOCK
chmod +x "$BIN/openssl" "$BIN/sleep" "$BIN/docker"

DOMAINS=(api.test.invalid admin.test.invalid widget.test.invalid rtc.test.invalid turn.test.invalid trace.test.invalid)
new_case() {
  FIXTURE="$TMP/$1 with spaces/deploy"
  LOG="$FIXTURE/events.log"
  OUTPUT="$FIXTURE/output.log"
  CONF="$FIXTURE/data/certbot/conf"
  mkdir -p "$FIXTURE/nginx" "$FIXTURE/data"
  cp "$ROOT/deploy/nginx/init-letsencrypt.sh" "$FIXTURE/nginx/"
  : > "$FIXTURE/docker-compose.community.yml"
  : > "$FIXTURE/docker-compose.uptrace.yml"
  : > "$LOG"
  printf 'existing customer data\n' > "$FIXTURE/data/keep-data"
  cat > "$FIXTURE/.env" <<'ENV'
API_DOMAIN=api.test.invalid
ADMIN_DOMAIN=admin.test.invalid
CS_WIDGET_DOMAIN=widget.test.invalid
RTC_DOMAIN=rtc.test.invalid
TURN_DOMAIN=turn.test.invalid
CERTBOT_EMAIL=certbot@test.invalid
UPTRACE_ENABLED=true
UPTRACE_DOMAIN=trace.test.invalid
ENV
}
run_bootstrap() {
  : > "$LOG"
  rm -f -- "$LOG.test-count"
  env -i PATH="$BIN:$PATH" MOCK_LOG="$LOG" "$@" \
    /bin/bash "$FIXTURE/nginx/init-letsencrypt.sh" > "$OUTPUT" 2>&1
}
has_event() { grep -Fxq "$1" "$LOG"; }
no_event() { ! has_event "$1"; }
temporary_cert() {
  [[ -s "$CONF/live/$1/fullchain.pem" && -s "$CONF/live/$1/privkey.pem" \
     && -f "$CONF/live/$1/.imboy-selfsigned" ]]
}
all_temporary() {
  local domain
  for domain in "${DOMAINS[@]}"; do temporary_cert "$domain" || return 1; done
}

printf '== TLS bootstrap control flow (offline stubs, no real TLS) ==\n'
new_case first
run_bootstrap || fail 'First bootstrap did not complete'
grep -Fq 'docker compose -f docker-compose.community.yml ' "$LOG" \
  || fail 'Standalone bootstrap did not select the shipped community Compose file'
pass 'Standalone bootstrap selects community Compose when prod is absent'
for domain in "${DOMAINS[@]}"; do
  [[ -L "$CONF/live/$domain/fullchain.pem" && -s "$CONF/live/$domain/privkey.pem" \
     && ! -e "$CONF/live/$domain/.imboy-selfsigned" ]] \
    || fail "Issued certificate was not retained for $domain"
done
awk '$1 == "probe" { seen[$2]=1 }
     $1 == "certbot" { if (length(seen) != 6) exit 1; issued++ }
     END { if (issued != 6) exit 1 }' "$LOG" \
  || fail 'Issuance started before all six HTTP routes were ready'
pass 'All six HTTP-01 routes are checked before the first issuance'
awk '$1 == "certbot" { tested=0 }
     $0 == "nginx-test" { tested=1 }
     $0 == "reload" { if (!tested) exit 1; reloaded=1 }
     END { if (!reloaded) exit 1 }' "$LOG" \
  || fail 'Reload was not preceded by nginx -t after the final issuance'
pass 'Successful issuance validates Nginx after certificate changes and before reload'
grep -Fxq 'existing customer data' "$FIXTURE/data/keep-data" || fail 'Customer data changed'
pass 'Bootstrap leaves unrelated data untouched'
cp -a "$CONF" "$FIXTURE/certificates.before"
run_bootstrap || fail 'Existing certificates did not permit an idempotent rerun'
diff -r "$FIXTURE/certificates.before" "$CONF" >/dev/null \
  || fail 'A rerun modified existing Certbot state'
! grep -q '^certbot ' "$LOG" || fail 'A rerun reissued existing certificates'
has_event nginx-test || fail 'Existing-cert path skipped nginx -t'
has_event 'probe rtc.test.invalid' || fail 'Existing-cert path skipped HTTP route readiness'
pass 'Existing Certbot links/content are preserved while startup is checked again'
if run_bootstrap MOCK_NGINX_TEST_FAIL_AT=1; then fail 'Existing-cert path accepted failed nginx -t'; fi
no_event reload || fail 'Existing-cert path reloaded after failed nginx -t'
diff -r "$FIXTURE/certificates.before" "$CONF" >/dev/null \
  || fail 'Nginx validation failure modified existing certificates'
pass 'Existing certificates remain unchanged when validation prevents reload'

new_case not_ready
if run_bootstrap MOCK_PROBE_FAIL=rtc.test.invalid; then fail 'Unready RTC route was accepted'; fi
! grep -q '^certbot ' "$LOG" || fail 'Certbot ran before RTC was ready'
all_temporary || fail 'An unready route caused temporary certificates to be removed'
no_event reload || fail 'Failed readiness attempted reload'
pass 'Unready RTC route stops issuance and preserves every temporary certificate'

new_case wrong_body
if run_bootstrap MOCK_PROBE_WRONG_BODY=rtc.test.invalid; then fail 'Unrelated HTTP response was accepted'; fi
! grep -q '^certbot ' "$LOG" || fail 'Certbot ran for an incorrect HTTP-01 response'
all_temporary || fail 'Incorrect HTTP response caused certificate removal'
pass 'HTTP success with the wrong challenge body cannot pass readiness'

new_case certbot_failure
if run_bootstrap MOCK_CERTBOT_FAIL=rtc.test.invalid; then fail 'Certbot failure was accepted'; fi
temporary_cert rtc.test.invalid || fail 'Failed RTC issuance did not restore temporary certificate and marker'
[[ -L "$CONF/live/api.test.invalid/fullchain.pem" ]] || fail 'Earlier issued certificate was lost'
no_event reload || fail 'Failed issuance attempted reload'
cp -a "$CONF/archive/api.test.invalid" "$FIXTURE/api.before"
run_bootstrap || fail 'Retry after a clean Certbot failure did not recover'
diff -r "$FIXTURE/api.before" "$CONF/archive/api.test.invalid" >/dev/null \
  || fail 'Retry overwrote a previously issued certificate'
no_event 'certbot api.test.invalid' || fail 'Retry reissued a previously completed domain'
has_event 'certbot rtc.test.invalid' || fail 'Retry skipped the failed RTC domain'
pass 'Clean Certbot failure restores a marked temporary certificate and recovers on rerun'

new_case certbot_partial
if run_bootstrap MOCK_CERTBOT_PARTIAL=rtc.test.invalid; then fail 'Partial Certbot failure was accepted'; fi
grep -Fxq 'partial Certbot state' "$CONF/archive/rtc.test.invalid/fullchain1.pem" \
  || fail 'Partial archive state was overwritten'
grep -Fxq 'partial renewal state' "$CONF/renewal/rtc.test.invalid.conf" \
  || fail 'Partial renewal state was overwritten'
if run_bootstrap; then fail 'Partial Certbot state should require manual recovery'; fi
! grep -q '^certbot ' "$LOG" || fail 'Retry issued over partial archive/renewal state'
pass 'Partial Certbot state is retained and a subsequent run stops for manual repair'

new_case missing_output
if run_bootstrap MOCK_CERTBOT_NO_FILES=rtc.test.invalid; then fail 'Certbot success without files was accepted'; fi
no_event reload || fail 'Missing issued files triggered reload'
temporary_cert rtc.test.invalid || fail 'Missing output did not restore a marked temporary certificate'
pass 'Certbot success without files fails and restores a marked temporary certificate'

new_case invalid_nginx
if run_bootstrap MOCK_NGINX_TEST_FAIL_AT=1; then fail 'Failed initial nginx -t was accepted'; fi
! grep -q '^certbot ' "$LOG" || fail 'Issuance began after initial nginx -t failed'
no_event reload || fail 'Failed initial nginx -t was followed by reload'
pass 'Initial configuration validation failure prevents issuance and reload'

new_case invalid_final_nginx
if run_bootstrap MOCK_NGINX_TEST_FAIL_AT=2; then fail 'Failed final nginx -t was accepted'; fi
has_event 'certbot trace.test.invalid' || fail 'Final validation test did not reach complete issuance'
no_event reload || fail 'Failed final nginx -t was followed by reload'
pass 'Final configuration validation failure prevents reload and success'

new_case explicit_files
run_bootstrap COMPOSE_FILES='docker-compose.community.yml docker-compose.uptrace.yml' \
  || fail 'Explicit Compose file set was rejected'
grep -Fq 'docker compose -f docker-compose.community.yml -f docker-compose.uptrace.yml ' "$LOG" \
  || fail 'Explicit overlay was omitted'
pass 'Explicit Compose file collection remains intact through bootstrap'

new_case existing_prod
: > "$FIXTURE/docker-compose.prod.yml"
run_bootstrap || fail 'Existing business Compose default was rejected'
grep -Fq 'docker compose -f docker-compose.prod.yml ' "$LOG" \
  || fail 'Existing business Compose default was changed'
pass 'Standalone bootstrap retains the existing business Compose default when present'

printf '\nTLS bootstrap regression: %s PASS; scope: offline control flow only.\n' "$passed"
