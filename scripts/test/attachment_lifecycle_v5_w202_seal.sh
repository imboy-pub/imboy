#!/usr/bin/env bash
# Compatibility entry point; all evidence uses the shared measured-leaf validator.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ "${4:?suite exit required}" -eq 0 ] || exit "$4"
exec bash "$SCRIPT_DIR/attachment_lifecycle_v5_seal.sh" "$1" "$2" "$3" "${5:?command required}"
