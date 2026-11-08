#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
source "$HERE/lib/services.sh"
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
    printf 'Usage: sudo bash renew-tls.sh [--check|--apply]\nRead fresh PEM pairs from TLS_DIR in /etc/imboy/site.env, validate, copy and reload Nginx. No ACME client or Python is used.\n'; exit 0
fi
mode=${1:---check}
[[ $# -le 1 && ( $mode == --check || $mode == --apply ) ]] || die 2 ARG 'Use --check or --apply.'
require_root; lock_operation; load_site "$CONF_DIR/site.env"; validate_tls
if [[ $mode == --check ]]; then info 'TLS hostname, validity and key-pair checks passed.'; exit 0; fi
staging=$(mktemp -d "$CONF_DIR/.tls-next.XXXXXX")
previous=$(mktemp -d "$CONF_DIR/.tls-previous.XXXXXX")
tls_switch=no
tls_exit() {
    local rc=$?
    trap - EXIT
    if ((rc!=0)) && [[ $tls_switch == yes && -d $previous/tls ]]; then
        [[ ! -d $CONF_DIR/tls ]] || mv "$CONF_DIR/tls" "$previous/failed-tls"
        mv "$previous/tls" "$CONF_DIR/tls"
        nginx -t && systemctl reload nginx || true
        printf 'ERROR [TLS_RELOAD] Previous TLS tree restored; verify Nginx service state. Private diagnostic copies are retained.\n' >&2
    fi
    exit "$rc"
}
trap tls_exit EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
# Complete all reads and copies before replacing the live TLS tree.
copy_tls "$staging"
TLS_DIR=$staging
validate_tls
tls_switch=yes
mv "$CONF_DIR/tls" "$previous/tls"
mv "$staging" "$CONF_DIR/tls"
nginx -t
systemctl reload nginx
tls_switch=no
rm -rf --one-file-system -- "$previous"
info 'TLS certificates replaced and Nginx reloaded.'
