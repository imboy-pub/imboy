#!/usr/bin/env bash
# PR DCO：只检查事件中的 base..head，保留所有真实提交（含 merge）。
# Check the event's exact base..head range, including real merge commits.
set -euo pipefail

fail() { echo "::error::$*" >&2; exit 1; }

validate_commit() {
  local name="$1" sha="$2" type
  [[ "$sha" =~ ^([0-9a-f]{40}|[0-9a-f]{64})$ ]] ||
    fail "$name must be a full commit SHA"
  type="$(git cat-file -t "$sha")" || fail "$name commit object is unavailable"
  [[ "$type" == commit ]] || fail "$name must identify a commit object"
}

base="${PR_BASE_SHA:-}"
head="${PR_HEAD_SHA:-}"
validate_commit PR_BASE_SHA "$base"
validate_commit PR_HEAD_SHA "$head"

# Do not hide rev-list failure in process substitution: missing history must fail.
commits="$(git rev-list "$base..$head" --)" || fail "Cannot enumerate PR commits"
[[ -n "$commits" ]] || fail "PR range contains no commits"

rc=0
total=0
while IFS= read -r sha; do
  total=$((total + 1))
  email="$(git log -1 --format='%ae' "$sha")" || fail "Cannot read author for $sha"
  body="$(git log -1 --format='%b' "$sha")" || fail "Cannot read message for $sha"
  # Keep email matching literal: bot addresses may contain [ ] + . characters.
  if ! signoffs="$(grep -i '^signed-off-by:' <<< "$body")" ||
     ! grep -qiF -- "<${email}>" <<< "$signoffs"; then
    echo "::error::commit ${sha:0:12}: author <${email}> lacks a matching Signed-off-by"
    rc=1
  fi
done <<< "$commits"

[[ "$rc" -eq 0 ]] || exit "$rc"
echo "DCO passed for $total PR commits ($base..$head)"
