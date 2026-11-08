#!/usr/bin/env bash
# Git tag v<VERSION> -> image <VERSION>; preserve prerelease identity.
# 仅解析/校验，不创建 tag 或发布镜像。Read-only release contract validation.
set -euo pipefail

root="${IMBOY_RELEASE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
tag="${1:?Usage: release_version.sh vMAJOR.MINOR.PATCH[-prerelease]}"
version="${tag#v}"
die() { printf 'Release contract: %s\n' "$*" >&2; exit 1; }

# Docker tags cannot represent '+' losslessly. Reject build metadata rather
# than silently collapsing different source versions onto the same image tag.
[[ "$tag" = v* && "$version" =~ ^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?$ ]] \
  || die 'expected vMAJOR.MINOR.PATCH[-prerelease]; build metadata is unsupported'
prerelease=false
if [[ "$version" = *-* ]]; then
  prerelease=true
  IFS=. read -r -a identifiers <<< "${version#*-}"
  for identifier in "${identifiers[@]}"; do
    [[ ! "$identifier" =~ ^0[0-9]+$ ]] || die 'numeric prerelease identifiers must not have leading zeroes'
  done
fi
[[ "$version" = "$(cat "$root/VERSION")" ]] || die 'Git tag does not match VERSION'
env_version="$(sed -n 's/^IMBOY_VERSION=//p' "$root/deploy/.env.example")"
[[ "$version" = "$env_version" ]] || die 'deploy/.env.example does not match VERSION'
sha="$(git -C "$root" rev-parse HEAD)"
[[ "$sha" =~ ^[0-9a-f]{40}$ ]] || die 'invalid source commit'
run_id="${GITHUB_RUN_ID:-local}"
attempt="${GITHUB_RUN_ATTEMPT:-1}"
[[ "$run_id" =~ ^[0-9]+$ || "$run_id" = local ]] || die 'invalid run id'
[[ "$attempt" =~ ^[1-9][0-9]*$ ]] || die 'invalid run attempt'
candidate="${version}-candidate-${sha:0:12}-${run_id}-${attempt}"
[[ ${#candidate} -le 128 ]] || die 'candidate exceeds Docker tag length limit'
printf 'RELEASE_TAG=%s\nIMBOY_VERSION=%s\nIMBOY_GIT_SHA=%s\nCANDIDATE_TAG=%s\nPRERELEASE=%s\n' \
  "$tag" "$version" "$sha" "$candidate" "$prerelease"
