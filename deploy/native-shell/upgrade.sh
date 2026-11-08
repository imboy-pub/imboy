#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
source "$HERE/lib/services.sh"
source "$HERE/lib/render.sh"
source "$HERE/lib/maintenance.sh"
usage() { printf 'Usage: sudo bash upgrade.sh --package /absolute/new-candidate [--check|--apply]\nOnly backend upgrades with byte-identical PostgreSQL/Garage payloads are supported. Stops writes, snapshots all state, then migrates. No automatic database downgrade.\n'; }
next=; mode=check
while (($#)); do
    case $1 in --package) [[ $# -ge 2 ]] || die 2 ARG 'Missing package path'; next=$2; shift 2;; --check) mode=check; shift;; --apply) mode=apply; shift;; -h|--help) usage; exit 0;; *) die 2 ARG "Unknown argument: $1";; esac
done
[[ -n $next ]] || die 2 ARG '--package is required.'
safe_path "$next"; next=$(cd "$next" && pwd -P)
require_root
load_metadata "$next"; verify_manifest "$next"
# A verified target package owns its renderer, units and upgrade behavior.
if [[ $next != "$HERE" ]]; then exec bash "$next/upgrade.sh" --package "$next" "--$mode"; fi
load_site "$CONF_DIR/site.env"
installed_release_hash=$(sha "$CONF_DIR/release.env")
read_kv "$CONF_DIR/release.env" META "$META_KEYS"
old_release=${META[RELEASE_ID]}; old_sha=${META[SOURCE_SHA]}; old_head=${META[MIGRATION_HEAD]}
current_status=$(cat "$STATE_DIR/status" 2>/dev/null || true)
case $current_status in
    INITIALIZED|UPGRADED_AWAITING_UAT|RESTORED_AWAITING_UAT) ;;
    *) die 4 UPGRADE_STATE 'Finish or recover the existing installation before starting a new upgrade.' ;;
esac
[[ -f $CONF_DIR/runtime.sha256 ]] || die 3 UPGRADE 'Installed runtime checksum record is missing.'
load_metadata "$next"; verify_manifest "$next"; check_host yes; validate_payload "$next"
[[ ${META[RELEASE_ID]} != "$old_release" && ${META[SOURCE_SHA]} != "$old_sha" ]] || die 4 SAME_RELEASE 'Upgrade requires a different release ID and source SHA.'
(( ${META[MIGRATION_HEAD]} >= old_head )) || die 4 DOWNGRADE 'Schema downgrade is not supported.'
(cd "$next/payload" && sha256sum --check --status "$CONF_DIR/runtime.sha256") || die 3 RUNTIME_CHANGE 'PG/Garage runtime differs. A separately rehearsed database/storage upgrade is required.'
[[ ! -e $INSTALL_DIR/releases/${META[RELEASE_ID]} ]] || die 4 EXISTING_RELEASE 'New release directory already exists; reconcile an interrupted upgrade before retrying.'
if [[ $mode == check ]]; then info 'Upgrade preflight passed. NOT_RUN: backup, migrations and business compatibility.'; exit 0; fi
lock_operation
[[ $(cat "$STATE_DIR/status" 2>/dev/null || true) == "$current_status" && $(sha "$CONF_DIR/release.env") == "$installed_release_hash" ]] || die 4 UPGRADE_STATE 'Installed state changed during preflight; run the preflight again.'
remember_services
MIGRATION_STARTED=no; MIGRATION_PRIVILEGE=no
finish() {
    local rc=$?
    trap - EXIT
    if ((rc!=0)); then
        if [[ $MIGRATION_STARTED == no ]]; then restore_service_state || true
        else
            systemctl stop nginx imboy-backend.service >/dev/null 2>&1 || true
            psql_admin postgres -qc 'ALTER ROLE imboy NOSUPERUSER NOCREATEDB NOCREATEROLE' >/dev/null 2>&1 || true
            printf 'UPGRADE_FAILED\n' > "$STATE_DIR/status"
            printf 'ERROR [UPGRADE] Writes remain stopped. Preserve the failed state and backup; do not switch only the old binary onto a migrated database.\n' >&2
        fi
    fi
    exit "$rc"
}
trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
create_backup
printf '%s\n' "$BACKUP_RESULT" > "$STATE_DIR/pre-upgrade-backup.path"
info "Consistent pre-upgrade backup: $BACKUP_RESULT"
printf 'UPGRADING\n' > "$STATE_DIR/status"
MIGRATION_STARTED=yes
stage_payload "$next"
load_secrets
render_backend_config "$INSTALL_DIR/current" "$CONF_DIR"
chown root:imboy "$CONF_DIR/sys.config" "$CONF_DIR/vm.args"; chmod 640 "$CONF_DIR/sys.config" "$CONF_DIR/vm.args"; chmod 600 "$CONF_DIR/backend.env"
write_units
systemctl start imboy-postgresql imboy-garage
start_backend_migrate
cp "$next/metadata/release.env" "$CONF_DIR/release.env"
sha "$next/CHECKSUMS.sha256" > "$CONF_DIR/package.sha256"
write_nginx
systemctl start nginx
flock -u 9
bash "$next/verify.sh"
printf 'UPGRADED_AWAITING_UAT\n' > "$STATE_DIR/status"
info 'Technical upgrade checks passed; complete the release-specific business acceptance checklist.'
