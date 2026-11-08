#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
source "$HERE/lib/services.sh"
source "$HERE/lib/maintenance.sh"
usage() { printf 'Usage: sudo bash restore.sh --from /absolute/backup.tar.gz --sha256 HASH --isolated-target [--check|--apply] [--resume]\nUses the exact original prepared package on a fresh isolated host. Never overwrites an existing installation. All original secrets and data are restored.\n'; }
archive=; expected=; isolated=no; mode=check; resume=no
while (($#)); do
    case $1 in
        --from) [[ $# -ge 2 ]] || die 2 ARG 'Missing --from'; archive=$2; shift 2;;
        --sha256) [[ $# -ge 2 ]] || die 2 ARG 'Missing --sha256'; expected=$2; shift 2;;
        --isolated-target) isolated=yes; shift;; --check) mode=check; shift;; --apply) mode=apply; shift;; --resume) resume=yes; shift;;
        -h|--help) usage; exit 0;; *) die 2 ARG "Unknown argument: $1";;
    esac
done
[[ -n $archive && -n $expected && $isolated == yes ]] || die 2 ARG 'Backup path, trusted checksum and --isolated-target are required.'
require_root; safe_path "$archive"
load_metadata "$HERE"; verify_manifest "$HERE"; check_host yes; validate_payload "$HERE"
validate_backup_archive "$archive" "$expected"
package_hash=$(tar -xOzf "$archive" config/package.sha256)
[[ $package_hash == "$(sha "$HERE/CHECKSUMS.sha256")" ]] || die 3 RESTORE_PACKAGE 'Restore requires the exact original package; a matching version label is insufficient.'
if [[ $mode == check ]]; then info 'PASS: archive format/checksum, original package and host. NOT_RUN: restore and data oracles.'; exit 0; fi
if [[ $resume == no ]]; then
    for path in "$INSTALL_DIR" "$CONF_DIR" "$DATA_DIR" "$STATE_DIR/install.identity" "$STATE_DIR/restore.identity"; do [[ ! -e $path && ! -L $path ]] || die 4 EXISTING 'Restore is limited to a fresh host. Use a separate machine for recovery.'; done
    for u in imboy imboy-pg imboy-garage; do ! id "$u" >/dev/null 2>&1 || die 4 EXISTING 'Dedicated account already exists.'; done
fi
lock_operation
restore_exit() {
    local rc=$?
    if ((rc!=0)); then printf 'RESTORE_FAILED\n' > "$STATE_DIR/status"; printf 'ERROR [RESTORE] Preserve the isolated target and original backup; inspect the failed stage before --resume.\n' >&2; fi
}
trap restore_exit EXIT
if [[ $resume == yes ]]; then
    [[ -f $STATE_DIR/restore.identity && $(cat "$STATE_DIR/restore.identity") == "$expected:$package_hash" ]] || die 4 RESUME 'Restore resume identity mismatch.'
else printf '%s\n' "$expected:$package_hash" > "$STATE_DIR/restore.identity"; fi
prepare_accounts
stage_payload "$HERE"
if [[ ! -f $STATE_DIR/restore.files-copied ]]; then
    staging=$(mktemp -d "$STATE_DIR/.restore.XXXXXX")
    tar --no-same-owner -xzpf "$archive" -C "$staging"
    (cd "$staging" && sha256sum --check --status CONTENTS.sha256) || die 3 RESTORE_CONTENT 'Backup internal checksum mismatch; nothing copied into live data paths.'
    cp -a "$staging/config/." "$CONF_DIR/"
    cp -a "$staging/data/." "$DATA_DIR/"
    chown -R imboy-pg:imboy-pg "$DATA_DIR/postgres"
    chown -R imboy-garage:imboy-garage "$DATA_DIR/garage"
    chown -R imboy:imboy "$DATA_DIR/backend"
    chown -R root:root "$CONF_DIR"
    chown root:imboy "$CONF_DIR/sys.config" "$CONF_DIR/vm.args" "$CONF_DIR/keys" "$CONF_DIR/keys/"*
    chown root:imboy-garage "$CONF_DIR/garage.toml"
    chmod 755 "$CONF_DIR"
    touch "$STATE_DIR/restore.files-copied"
    rm -rf --one-file-system -- "$staging"
fi
load_site "$CONF_DIR/site.env"
write_units
systemctl enable --now imboy-postgresql imboy-garage >/dev/null
wait_postgres || die 5 PG_READY 'Restored PostgreSQL is not ready.'
wait_garage || die 5 GARAGE_READY 'Restored Garage is not ready.'
systemctl enable --now imboy-backend >/dev/null
wait_health
write_nginx
systemctl enable --now nginx >/dev/null
flock -u 9
bash "$HERE/verify.sh"
printf 'RESTORED_AWAITING_UAT\n' > "$STATE_DIR/status"
info 'Snapshot restored. Before DNS cutover, compare real message/attachment counts and sample hashes on the isolated host; retain the original system.'
