#!/usr/bin/env bash
# Local contract tests. No real service, database, network or production data.
set -euo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
if [[ ${1:-} != --case ]]; then
    failed=0; count=0
    cases=(syntax help unassembled kv_literal kv_duplicate kv_unknown site_valid site_invalid secrets_lengths secrets_repeat secrets_permissions secrets_missing_resume install_resume_phase render_repeat render_contract render_reject render_changed_contract manifest_ok manifest_tamper manifest_extra_file tls_valid tls_wrong_host rsa_roundtrip service_templates backup_snapshot backup_restart_failure backup_bad_hash backup_symlink archive_traversal)
    printf 'test\tstatus\n'
    for name in "${cases[@]}"; do
        count=$((count+1))
        if bash "$0" --case "$name"; then printf '%s\tPASS\n' "$name"; else printf '%s\tFAIL\n' "$name"; failed=$((failed+1)); fi
    done
    printf 'TOTAL=%s PASS=%s FAIL=%s\n' "$count" "$((count-failed))" "$failed"
    ((failed==0)); exit
fi
name=${2:?}
T=$(mktemp -d)
trap 'rm -rf -- "$T"' EXIT
source "$ROOT/lib/common.sh"
source "$ROOT/lib/services.sh"
source "$ROOT/lib/maintenance.sh"
source "$ROOT/lib/render.sh"
# Explicit fixture paths set after sourcing; CLI scripts never accept these overrides.
CONF_DIR=$T/config; DATA_DIR=$T/data; STATE_DIR=$T/state; BACKUP_DIR=$T/backup
mkdir -p "$CONF_DIR" "$DATA_DIR" "$STATE_DIR" "$BACKUP_DIR"
setup_site() {
    cat > "$T/site.env" <<EOF
SITE_NAME=synthetic
API_DOMAIN=api.delivery.test
S3_DOMAIN=s3.delivery.test
FILES_DOMAIN=files.delivery.test
TLS_DIR=$T/tls
GARAGE_CAPACITY=10G
CORS_ORIGINS=https://web.delivery.test
EOF
    load_site "$T/site.env"
}
setup_render() {
    setup_site; generate_secrets "$CONF_DIR"
    mkdir -p "$T/release/releases"
    printf '%s' '17.1.2 1.0.0-alpha.83' > "$T/release/releases/start_erl.data"
    mkdir -p "$T/release/releases/1.0.0-alpha.83"
    imboy_native_base_config > "$T/release/releases/1.0.0-alpha.83/sys.config"
}
setup_manifest() {
    mkdir -p "$T/package/lib" "$T/package/metadata"
    local f
    for f in install.sh lib/common.sh lib/render.sh lib/services.sh metadata/release.env; do printf 'fixture\n' > "$T/package/$f"; done
    (cd "$T/package" && find . -type f ! -name CHECKSUMS.sha256 -print0 | sort -z | xargs -0 sha256sum) > "$T/package/CHECKSUMS.sha256"
}
setup_tls() {
    setup_site
    local role domain
    for role in api s3 files; do
        domain=$role.delivery.test
        mkdir -p "$TLS_DIR/$role"
        openssl req -x509 -newkey rsa:2048 -nodes -days 30 -subj "/CN=$domain" -addext "subjectAltName=DNS:$domain" -keyout "$TLS_DIR/$role/privkey.pem" -out "$TLS_DIR/$role/fullchain.pem" >/dev/null 2>&1
    done
}
fixture_systemctl() {
    local operation=${1:-}; shift || true
    case $operation in
        is-active) [[ ${1:-} == --quiet ]] && shift; [[ -f $T/active/${1%.service} ]];;
        stop) local u; for u in "$@"; do printf 'stop:%s\n' "$u" >> "$T/events"; rm -f "$T/active/${u%.service}"; done;;
        start) local u; for u in "$@"; do printf 'start:%s\n' "$u" >> "$T/events"; touch "$T/active/${u%.service}"; done;;
        *) return 0;;
    esac
}
case $name in
syntax)
    while IFS= read -r -d '' f; do bash -n "$f"; done < <(find "$ROOT" -name '*.sh' -print0)
    [[ -z $(find "$ROOT" -name '*.py' -print -quit) ]]
    ;;
help)
    for script in install verify admin-init backup upgrade restore renew-tls; do bash "$ROOT/$script.sh" --help >/dev/null; done
    for script in build-backend build-native-dependencies prepare-payload prepare-host; do bash "$ROOT/tools/$script.sh" --help >/dev/null; done
    ;;
unassembled)
    mkdir -p "$T/unassembled/lib" "$T/unassembled/metadata"
    cp "$ROOT/install.sh" "$T/unassembled/install.sh"
    cp "$ROOT/lib/"*.sh "$T/unassembled/lib/"
    sed 's/^PAYLOAD_READY=.*/PAYLOAD_READY=no/' "$ROOT/metadata/release.env" > "$T/unassembled/metadata/release.env"
    code=0; bash "$T/unassembled/install.sh" --check > "$T/output" 2>&1 || code=$?
    [[ $code == 3 ]]; grep -q 'PAYLOAD' "$T/output"
    ;;
kv_literal)
    printf 'SITE_NAME=$(touch %s/pwned)\n' "$T" > "$T/literal"
    read_kv "$T/literal" SITE "$SITE_KEYS"
    [[ ! -e $T/pwned && ${SITE[SITE_NAME]} == *'$(touch '* ]]
    ;;
kv_duplicate)
    printf 'SITE_NAME=a\nSITE_NAME=b\n' > "$T/invalid"
    if (read_kv "$T/invalid" SITE "$SITE_KEYS") >/dev/null 2>&1; then exit 1; fi
    ;;
kv_unknown)
    printf 'PATH=/invalid\n' > "$T/invalid"
    if (read_kv "$T/invalid" SITE "$SITE_KEYS") >/dev/null 2>&1; then exit 1; fi
    ;;
site_valid) setup_site; [[ $S3_DOMAIN == s3.delivery.test ]];;
site_invalid)
    setup_site
    for domain in api.example.com '-bad.domain.test' 'bad..domain.test' 'https://api.delivery.test' '127.0.0.1'; do
        if valid_domain "$domain"; then exit 1; fi
    done
    ;;
secrets_lengths)
    generate_secrets "$CONF_DIR"
    [[ ${#JWT_SECRET} == 32 && ${#POSTGRE_AES_KEY} == 32 && ${#SOLIDIFIED_KEY} == 32 && ${#SOLIDIFIED_KEY_IV} == 16 && ${#GARAGE_RPC_SECRET} == 64 ]]
    [[ $JWT_SECRET != "$POSTGRE_AES_KEY" && $JWT_SECRET != "$ADM_COOKIE_SECRET" ]]
    [[ $(stat -c %a "$CONF_DIR/secrets.env") == 600 ]]
    ;;
secrets_repeat)
    generate_secrets "$CONF_DIR"; before=$(sha "$CONF_DIR/secrets.env")
    generate_secrets "$CONF_DIR"; [[ $before == "$(sha "$CONF_DIR/secrets.env")" ]]
    ;;
secrets_permissions)
    generate_secrets "$CONF_DIR"; chmod 644 "$CONF_DIR/secrets.env"
    if (load_secrets) >/dev/null 2>&1; then exit 1; fi
    ;;
secrets_missing_resume)
    generate_secrets "$CONF_DIR"; rm "$CONF_DIR/secrets.env"
    if (generate_secrets "$CONF_DIR") >/dev/null 2>&1; then exit 1; fi
    [[ ! -e $CONF_DIR/secrets.env ]]
    ;;
install_resume_phase)
    printf 'same-identity\n' > "$STATE_DIR/install.identity"
    for status in '' FAILED SERVICE_READY INITIALIZED; do
        printf '%s\n' "$status" > "$STATE_DIR/status"
        check_install_resume_state same-identity
    done
    for status in UPGRADING UPGRADE_FAILED UPGRADED_AWAITING_UAT RESTORE_FAILED RESTORED_AWAITING_UAT; do
        printf '%s\n' "$status" > "$STATE_DIR/status"
        if (check_install_resume_state same-identity) >/dev/null 2>&1; then exit 1; fi
    done
    ;;
render_repeat)
    setup_render; render_backend_config "$T/release" "$T/a"; render_backend_config "$T/release" "$T/b"
    for f in sys.config vm.args backend.env; do cmp "$T/a/$f" "$T/b/$f"; [[ $(stat -c %a "$T/a/$f") == 600 ]]; done
    ;;
render_contract)
    setup_render; render_backend_config "$T/release" "$T/out"
    grep -q 'IMBOY_WS_URL=wss://api.delivery.test/api/v1/ws' "$T/out/backend.env"
    grep -q '^HTTP_PORT=9800$' "$T/out/backend.env"
    grep -q '^IMBOY_GARAGE_PUBLIC_ENDPOINT=https://s3.delivery.test$' "$T/out/backend.env"
    grep -q 'public_base_url => <<"https://files.delivery.test">>' "$T/out/sys.config"
    grep -q '{listen_ip, {127, 0, 0, 1}}' "$T/out/sys.config"
    grep -q 'ecron' "$T/out/sys.config"
    grep -q '<<"https://web.delivery.test">>' "$T/out/sys.config"
    ! grep -q 'abc54321' "$T/out/sys.config"
    ;;
render_reject)
    setup_render; SOLIDIFIED_KEY_IV=short
    if (render_backend_config "$T/release" "$T/out") >/dev/null 2>&1; then exit 1; fi
    ;;
render_changed_contract)
    setup_render; printf '%% changed contract\n' >> "$T/release/releases/1.0.0-alpha.83/sys.config"
    if (render_backend_config "$T/release" "$T/out") >/dev/null 2>&1; then exit 1; fi
    ;;
manifest_ok) setup_manifest; verify_manifest "$T/package";;
manifest_tamper)
    setup_manifest; printf 'changed\n' >> "$T/package/install.sh"
    if (verify_manifest "$T/package") >/dev/null 2>&1; then exit 1; fi
    ;;
manifest_extra_file)
    setup_manifest; printf extra > "$T/package/unlisted"
    if (verify_manifest "$T/package") >/dev/null 2>&1; then exit 1; fi
    ;;
tls_valid) setup_tls; validate_tls;;
tls_wrong_host)
    setup_tls; API_DOMAIN=wrong.delivery.test
    if (validate_tls) >/dev/null 2>&1; then exit 1; fi
    ;;
rsa_roundtrip)
    openssl genrsa -traditional -out "$T/key" 2048 >/dev/null 2>&1
    openssl rsa -in "$T/key" -pubout -out "$T/pub" >/dev/null 2>&1
    printf 'synthetic-password-123' | openssl dgst -md5 | awk '{printf "%s",$NF}' > "$T/digest"
    openssl pkeyutl -encrypt -pubin -inkey "$T/pub" -in "$T/digest" -out "$T/encrypted" -pkeyopt rsa_padding_mode:oaep -pkeyopt rsa_oaep_md:sha256 -pkeyopt rsa_mgf1_md:sha256
    openssl pkeyutl -decrypt -inkey "$T/key" -in "$T/encrypted" -out "$T/decrypted" -pkeyopt rsa_padding_mode:oaep -pkeyopt rsa_oaep_md:sha256 -pkeyopt rsa_mgf1_md:sha256
    cmp "$T/digest" "$T/decrypted"
    ;;
service_templates)
    setup_site; mkdir "$T/units"
    systemctl() { return 0; }; nginx() { return 0; }
    write_units "$T/units"; write_nginx "$T/nginx.conf"
    grep -q 'location \^~ /api/adm/setup/' "$T/nginx.conf"
    grep -q 'proxy_pass http://127.0.0.1:3900;' "$T/nginx.conf"
    grep -Fq 'proxy_set_header Host $http_host;' "$T/nginx.conf"
    grep -q 'ExecStart=/opt/imboy/current/bin/imboy foreground' "$T/units/imboy-backend.service"
    grep -q '^Environment=VMARGS_PATH=' "$T/units/imboy-backend.service"
    grep -q '^Environment=RELX_REPLACE_OS_VARS=$' "$T/units/imboy-backend.service"
    ! grep -q '^Environment=RELX_VM_ARGS=' "$T/units/imboy-backend.service"
    ;;
backup_snapshot)
    systemctl() { fixture_systemctl "$@"; }
    wait_postgres() { return 0; }; wait_garage() { return 0; }; wait_health() { return 0; }
    mkdir "$T/active"
    for u in imboy-postgresql imboy-garage imboy-backend nginx; do touch "$T/active/$u"; done
    printf 'synthetic\n' > "$CONF_DIR/release.env"; printf 'synthetic\n' > "$CONF_DIR/secrets.env"
    printf 'preserve me\n' > "$DATA_DIR/data.txt"
    remember_services; create_backup
    [[ $(stat -c %a "$BACKUP_RESULT") == 600 && ! -e $T/active/nginx ]]
    validate_backup_archive "$BACKUP_RESULT" "$(sha "$BACKUP_RESULT")"
    mkdir "$T/extracted"; tar -xzf "$BACKUP_RESULT" -C "$T/extracted"
    (cd "$T/extracted" && sha256sum --check --status CONTENTS.sha256)
    cmp "$DATA_DIR/data.txt" "$T/extracted/data/data.txt"
    restore_service_state; [[ -f $T/active/nginx && -f $T/active/imboy-backend ]]
    ;;
backup_restart_failure)
    systemctl() { fixture_systemctl "$@"; }
    wait_postgres() { return 1; }; wait_garage() { return 0; }; wait_health() { return 0; }
    mkdir "$T/active"
    PREVIOUSLY_ACTIVE=(imboy-postgresql imboy-garage imboy-backend nginx)
    if restore_service_state >/dev/null 2>&1; then exit 1; fi
    [[ ! -f $T/active/nginx && ! -f $T/active/imboy-backend ]]
    ;;
backup_bad_hash)
    printf content > "$T/archive"
    if (validate_backup_archive "$T/archive" "$(printf '0%.0s' {1..64})") >/dev/null 2>&1; then exit 1; fi
    ;;
backup_symlink)
    mkdir "$T/config2"; ln -s /etc/passwd "$T/config2/link"
    tar -czf "$T/archive" -C "$T" config2
    if (validate_backup_archive "$T/archive" "$(sha "$T/archive")") >/dev/null 2>&1; then exit 1; fi
    ;;
archive_traversal)
    mkdir "$T/config3"; printf synthetic > "$T/config3/file"
    tar -czf "$T/archive" --transform='s|config3|config/../outside|' -C "$T" config3
    if (validate_backup_archive "$T/archive" "$(sha "$T/archive")") >/dev/null 2>&1; then exit 1; fi
    ;;
*) exit 2;;
esac
