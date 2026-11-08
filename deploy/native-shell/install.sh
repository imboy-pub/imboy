#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
source "$HERE/lib/services.sh"
source "$HERE/lib/render.sh"
usage() {
    cat <<'EOF'
Usage: bash install.sh --check [--site /absolute/site.env]
       sudo bash install.sh --apply [--prepare-host] [--resume] [--site /absolute/site.env]
--check   Read-only readiness checks; no downloads, services or data are changed.
--apply   Install a prepared native payload on a fresh dedicated systemd host.
--resume  Continue this same package/configuration; preserve data and secrets.
--prepare-host Install pinned host prerequisites from already approved OS repositories.
An interactive terminal is needed to create the first administrator. No Python.
EOF
}
mode=check; resume=no; prepare_host=no; site=$HERE/site.env
while (($#)); do
    case $1 in
        --check) mode=check; shift;; --apply) mode=apply; shift;; --resume) resume=yes; shift;; --prepare-host) prepare_host=yes; shift;;
        --site) [[ $# -ge 2 ]] || die 2 ARG '--site requires a path'; site=$2; shift 2;;
        -h|--help) usage; exit 0;; *) die 2 ARG "Unknown argument: $1";;
    esac
done
[[ $resume == no || $mode == apply ]] || die 2 ARG '--resume requires --apply.'
[[ $prepare_host == no || $mode == apply ]] || die 2 ARG '--prepare-host requires --apply.'
load_metadata "$HERE"
verify_manifest "$HERE"
load_site "$site"
identity="${META[RELEASE_ID]}:${META[SOURCE_SHA]}:$(sha "$HERE/CHECKSUMS.sha256"):$(sha "$site")"
# Check ownership of this installation before optional host package mutations.
if [[ $mode == apply ]]; then
    require_root
    if [[ $resume == yes ]]; then
        check_install_resume_state "$identity"
    else
        for path in "$INSTALL_DIR" "$CONF_DIR" "$DATA_DIR" "$STATE_DIR/install.identity" /etc/nginx/conf.d/imboy-native.conf; do
            [[ ! -e $path && ! -L $path ]] || die 4 EXISTING "Existing path: $path . Use --resume only for a matching interrupted installation."
        done
        for u in imboy imboy-pg imboy-garage; do ! id "$u" >/dev/null 2>&1 || die 4 EXISTING "User $u already exists; this installer cannot take over an existing installation."; done
        for unit in imboy-backend imboy-postgresql imboy-garage; do [[ ! -f /etc/systemd/system/$unit.service ]] || die 4 EXISTING "Existing unit: $unit"; done
    fi
fi
if [[ $prepare_host == yes ]]; then require_root; bash "$HERE/tools/prepare-host.sh" --apply; fi
check_host "$([[ $mode == apply ]] && printf yes || printf no)"
validate_payload "$HERE"
validate_tls
if [[ $mode == check ]]; then
    info 'PASS: package, platform, required executables, shared libraries, site data and TLS pairs.'
    info 'NOT_RUN: runtime startup, migrations, DNS reachability and business acceptance.'
    exit 0
fi
require_root
# Refuse implicit takeover before making directories or installing users.
if [[ $resume == no ]]; then
    need numfmt
    advertised=$(numfmt --from=si "$GARAGE_CAPACITY")
    free_data=$(df -PB1 /var/lib | awk 'END{print $4}')
    payload_size=$(du -sb "$HERE/payload" | awk '{print $1}')
    ((free_data > advertised+payload_size+2147483648)) || die 3 SPACE 'Insufficient capacity for the advertised Garage size, payload and 2 GiB reserve.'
    free_program=$(df -PB1 /opt | awk 'END{print $4}')
    ((free_program > payload_size+1073741824)) || die 3 SPACE 'Insufficient space for native programs plus 1 GiB reserve.'
    [[ -z $(ss -H -lnt '( sport = :5432 or sport = :9800 or sport = :3900 or sport = :3901 or sport = :3902 or sport = :3903 or sport = :9100 or sport = :4369 )') ]] || die 4 PORT 'A required private port is already in use.'
    # A distribution-default Nginx may already listen on 80/443. Other web stacks are unsupported.
fi
lock_operation
if [[ $resume == yes ]]; then
    check_install_resume_state "$identity"
    previous_status=$(cat "$STATE_DIR/status" 2>/dev/null || true)
    if [[ $previous_status == INITIALIZED || $previous_status == SERVICE_READY ]]; then
        load_secrets
        flock -u 9
        if [[ $previous_status == SERVICE_READY ]]; then bash "$HERE/admin-init.sh"; fi
        bash "$HERE/verify.sh"
        printf 'INITIALIZED\n' > "$STATE_DIR/status"
        info 'Existing initialized service verified; configuration, data and secrets preserved.'
        exit 0
    fi
    # Reconcile interrupted work with application traffic stopped, never with
    # a live application role temporarily elevated for a migration.
    [[ ! -f /etc/nginx/conf.d/imboy-native.conf ]] || systemctl stop nginx
    [[ ! -f /etc/systemd/system/imboy-backend.service ]] || systemctl stop imboy-backend.service
else
    printf '%s\n' "$identity" > "$STATE_DIR/install.identity"
fi
MIGRATION_PRIVILEGE=no
on_error() {
    local rc=$?
    trap - EXIT
    ((rc!=0)) || return 0
    if [[ $MIGRATION_PRIVILEGE == yes ]]; then
        systemctl stop nginx >/dev/null 2>&1 || true
        systemctl stop imboy-backend.service >/dev/null 2>&1 || true
        psql_admin postgres -qc 'ALTER ROLE imboy NOSUPERUSER NOCREATEDB NOCREATEROLE' >/dev/null 2>&1 || true
    fi
    printf 'FAILED\n' > "$STATE_DIR/status"
    printf 'ERROR [INSTALL] Initialization interrupted (exit %s). Preserve state; review local service logs and resume the same package.\n' "$rc" >&2
    exit "$rc"
}
trap on_error EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
info '1/7 Prepare dedicated service users and native release.'
prepare_accounts
stage_payload "$HERE"
install -m 600 "$site" "$CONF_DIR/site.env"
install -m 600 "$HERE/metadata/release.env" "$CONF_DIR/release.env"
sha "$HERE/CHECKSUMS.sha256" > "$CONF_DIR/package.sha256"
(cd "$HERE/payload" && find postgresql garage -type f -print0 | sort -z | xargs -0 sha256sum) > "$CONF_DIR/runtime.sha256"
info '2/7 Generate and retain host-local secrets, RSA and TLS configuration.'
generate_secrets "$CONF_DIR"
chmod 755 "$CONF_DIR"
generate_rsa
copy_tls
write_garage_config
write_units
info '3/7 Initialize PostgreSQL database and required extensions.'
init_postgres
info '4/7 Initialize Garage layout, private/public buckets and restricted credentials.'
init_garage
configure_s3_cors
info '5/7 Render runtime configuration and apply migrations.'
render_backend_config "$INSTALL_DIR/current" "$CONF_DIR"
chown root:imboy "$CONF_DIR/sys.config" "$CONF_DIR/vm.args"
chmod 640 "$CONF_DIR/sys.config" "$CONF_DIR/vm.args"
chmod 600 "$CONF_DIR/backend.env"
start_backend_migrate
info '6/7 Activate TLS entry points and verify service readiness.'
write_nginx
systemctl enable nginx >/dev/null
if systemctl is-active --quiet nginx; then systemctl reload nginx; else systemctl start nginx; fi
printf 'SERVICE_READY\n' > "$STATE_DIR/status"
trap - EXIT
flock -u 9
info '7/7 Initialize first administrator through the loopback-only setup endpoint.'
bash "$HERE/admin-init.sh"
bash "$HERE/verify.sh"
printf 'INITIALIZED\n' > "$STATE_DIR/status"
info 'Initialization checks passed. Complete the separate business/restart/restore/upgrade UAT before production sign-off.'
