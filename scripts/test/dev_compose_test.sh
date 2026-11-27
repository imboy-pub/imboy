#!/usr/bin/env bash
# Compose models and setup failure paths; --docker also exercises the real stack.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
exec python3 "$ROOT/scripts/test/test_dev_compose.py" "$@"
