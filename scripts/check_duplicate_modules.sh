#!/usr/bin/env bash
# 同一 -module 名禁止多个源文件：后编译覆盖先编译，
# 曾导致 e2ee_shard_validator（lib stub vs logic 实现）审计链全灭。
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

dups="$(find src -name '*.erl' -exec basename {} \; | sort | uniq -d)"

if [[ -n "$dups" ]]; then
  echo "duplicate module source files detected (same -module compiled twice, later wins):" >&2
  while IFS= read -r f; do
    find src -name "$f" | sed 's/^/  /' >&2
  done <<<"$dups"
  exit 1
fi

echo "duplicate module check passed"
