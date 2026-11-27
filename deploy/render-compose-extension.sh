#!/usr/bin/env bash
# Render a literal Compose extension from the root model for private compatibility.
set -euo pipefail
case "${1:-}" in imboy-business-policy|imboy-business-widget) ;; *) exit 2 ;; esac
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
awk -v key="x-$1: |" '
  $0 == key { active=1; next }
  active && /^[^ ]/ && NF { exit }
  active { line=substr($0, 3); gsub(/[$][$]/, "$", line); print line; if (line == "services:") found=1 }
  END { if (!found) exit 1 }
' "$root/docker-compose.yml"
