#!/usr/bin/env bash
# Compatibility entry point; all evidence uses the shared measured-leaf validator.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec bash "$SCRIPT_DIR/attachment_lifecycle_v5_seal.sh" "$@"
