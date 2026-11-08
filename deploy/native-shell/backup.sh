#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
source "$HERE/lib/maintenance.sh"
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
    printf 'Usage: sudo bash backup.sh\nStops Nginx/backend/Garage/PostgreSQL, creates one consistent cold snapshot, then restores prior service state. Backup contains secrets and customer data; keep private.\n'; exit 0
fi
[[ $# == 0 ]] || die 2 ARG 'No options supported.'
require_root; lock_operation
read_kv "$CONF_DIR/release.env" META "$META_KEYS"
remember_services
finish() {
    local rc=$?
    trap - EXIT
    restore_service_state || rc=5
    [[ -z ${BACKUP_WORK:-} ]] || printf 'NOTICE: private partial snapshot retained at %s\n' "$BACKUP_WORK" >&2
    exit "$rc"
}
trap finish EXIT
info 'Starting a consistent cold backup; service downtime begins now.'
create_backup
info "Backup created: $BACKUP_RESULT"
info 'Contains secrets and customer data. Encrypt before external transfer; do not attach it to support tickets.'
