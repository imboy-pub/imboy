#!/usr/bin/env bash
# Promote four already-verified digests without rebuilding or overwriting tags.
# 默认只检查；只有 --apply 才写 Registry。Default is read-only.
set -euo pipefail
apply=false
if [[ "${1:-}" = --apply ]]; then apply=true; shift; fi
[[ $# = 5 ]] || { echo 'Usage: release_promote.sh [--apply] VERSION BACKEND@sha256:... ADMIN@sha256:... WIDGET@sha256:... PG18@sha256:...' >&2; exit 2; }
version="$1"; shift
bash "$(dirname "${BASH_SOURCE[0]}")/release_version.sh" "v$version" >/dev/null
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
refs=("$@")
components=(backend admin widget pg18)
targets=()
missing=()
approved_owner="${IMBOY_RELEASE_OWNER:-imboy-pub}"
approved_owner="${approved_owner,,}"

target_state() {
  local target="$1" expected="$2" actual
  if actual="$(docker buildx imagetools inspect "$target" --format '{{.Manifest.Digest}}' 2>"$tmp/error")"; then
    [[ "$actual" = "$expected" ]] || { echo "Refusing to replace existing release tag: $target ($actual != $expected)" >&2; return 1; }
    printf 'same\n'
  elif grep -Eqi 'unauthorized|denied|forbidden|timeout|timed out|TLS|connection' "$tmp/error"; then
    cat "$tmp/error" >&2
    return 1
  elif grep -Eqi 'manifest unknown|MANIFEST_UNKNOWN|: not found' "$tmp/error"; then
    printf 'missing\n'
  else
    cat "$tmp/error" >&2
    echo "Cannot establish target absence: $target" >&2
    return 1
  fi
}

# Check ALL targets before the first write. Authentication/network failures
# are not evidence that a tag is absent. Source lookup establishes repository
# read access before accepting a MANIFEST_UNKNOWN response for a target tag.
for i in "${!refs[@]}"; do
  ref="${refs[$i]}"
  [[ "$ref" =~ ^ghcr\.io/([a-z0-9][a-z0-9-]*)/imboy-${components[$i]}@sha256:[0-9a-f]{64}$ ]] \
    || { echo "Unexpected image source: $ref" >&2; exit 1; }
  current_owner="${BASH_REMATCH[1]}"
  [[ "$current_owner" = "$approved_owner" ]] || { echo 'Image owner does not match IMBOY_RELEASE_OWNER' >&2; exit 1; }
  expected="${ref##*@}"
  source_digest="$(docker buildx imagetools inspect "$ref" --format '{{.Manifest.Digest}}')"
  [[ "$source_digest" = "$expected" ]] || { echo "Source digest mismatch: $ref" >&2; exit 1; }
  target="${ref%@*}:$version"
  targets+=("$target")
  missing+=("$(target_state "$target" "$expected")")
  printf '%s <- %s (%s)\n' "$target" "$ref" "${missing[$i]}"
done

[[ "$apply" = true ]] || exit 0
for i in "${!refs[@]}"; do
  # Registry tagging has no compare-and-swap. External writers must obey the
  # same release-version serialization policy; recheck each target before write.
  state="$(target_state "${targets[$i]}" "${refs[$i]##*@}")"
  if [[ "$state" = missing ]]; then
    docker buildx imagetools create --prefer-index=false \
      --tag "${targets[$i]}" "${refs[$i]}"
  fi
  actual="$(docker buildx imagetools inspect "${targets[$i]}" --format '{{.Manifest.Digest}}')"
  [[ "$actual" = "${refs[$i]##*@}" ]] || { echo "Promotion digest mismatch: ${targets[$i]}" >&2; exit 1; }
done
