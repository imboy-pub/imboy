#!/usr/bin/env bash
# 四镜像候选构建与 Docker Hub 分发 / Four-image candidate build and distribution.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GHCR=ghcr.io/imboy-pub
HUB=docker.io/imboy
FRONTEND_URL=https://github.com/imboy-pub/imboy-admin-frontend.git
components=(backend pg18 admin widget)

die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }
require() { command -v "$1" >/dev/null || die "Required tool: $1"; }
usage() {
  cat <<'EOF'
Usage:
  ADMIN_REPO_REF=<approved-full-SHA> bash docker/build_images.sh build [--platform linux/amd64] [--push]
  bash docker/build_images.sh publish [--apply] VERSION BACKEND_REF PG18_REF ADMIN_REF WIDGET_REF

build: default local --load; --push builds once and pushes unique candidate tags
       to GHCR and Docker Hub. Multiple platforms require --push.
publish: copy accepted, promoted GHCR digests to Docker Hub without rebuilding.
         Read-only unless --apply; existing conflicting version tags are refused.
         REF format: ghcr.io/imboy-pub/imboy-<component>@sha256:<64 hex>.
         Run only after the four-image acceptance gate passes.
EOF
}
digest_of() {
  docker buildx imagetools inspect "$1" --format '{{.Manifest.Digest}}'
}
release_contract() {
  IMBOY_RELEASE_ROOT="$ROOT" bash "$ROOT/scripts/release_version.sh" "v$1"
}

build_images() {
  local platform=linux/amd64 push=false
  while (( $# )); do
    case "$1" in
      --platform) (( $# >= 2 )) || die '--platform needs a value'; platform="$2"; shift 2 ;;
      --push) push=true; shift ;;
      *) die "Unknown build option: $1" ;;
    esac
  done
  case "$platform" in
    linux/amd64|linux/arm64|linux/amd64,linux/arm64|linux/arm64,linux/amd64) ;;
    *) die 'Supported platforms: linux/amd64, linux/arm64, or both' ;;
  esac
  if [[ "$push" = false && "$platform" = *,* ]]; then
    die 'Multi-platform build requires --push; --load supports one platform'
  fi
  local frontend_sha="${ADMIN_REPO_REF:-}"
  [[ "$frontend_sha" =~ ^[0-9a-f]{40}$ ]] || die 'ADMIN_REPO_REF must be the approved full frontend commit SHA'
  require git; require docker; require python3; require bun; require tar
  [[ "$(bun --version)" = 1.3.14 ]] || die 'Bun 1.3.14 required (same as release CI)'
  [[ -z "$(git -C "$ROOT" status --porcelain --untracked-files=normal)" ]] || die 'Backend checkout is dirty'
  local version backend_sha candidate contract
  version="$(cat "$ROOT/VERSION")"
  contract="$(GITHUB_RUN_ID="$(date +%s)$$$RANDOM" GITHUB_RUN_ATTEMPT=1 release_contract "$version")"
  backend_sha="$(printf '%s\n' "$contract" | sed -n 's/^IMBOY_GIT_SHA=//p')"
  candidate="$(printf '%s\n' "$contract" | sed -n 's/^CANDIDATE_TAG=//p')"
  [[ "$backend_sha" =~ ^[0-9a-f]{40}$ && -n "$candidate" ]] || die 'Invalid release identity'
  docker buildx version >/dev/null

  # Only committed snapshots enter contexts; ignored local secrets cannot leak.
  local tmp
  tmp="$(mktemp -d)"
  trap "rm -rf -- $(printf '%q' "$tmp")" EXIT
  mkdir "$tmp/backend" "$tmp/admin" "$tmp/widget"
  git -C "$ROOT" archive "$backend_sha" | tar -xf - -C "$tmp/backend"
  git clone --quiet --filter=blob:none --no-checkout --single-branch --branch main "$FRONTEND_URL" "$tmp/frontend"
  git -C "$tmp/frontend" merge-base --is-ancestor "$frontend_sha" refs/remotes/origin/main \
    || die 'Approved frontend SHA is not in frontend main history'
  git -C "$tmp/frontend" checkout --quiet --detach "$frontend_sha"
  [[ "$(git -C "$tmp/frontend" rev-parse HEAD)" = "$frontend_sha" ]] || die 'Frontend checkout mismatch'

  # Keep the local recipe mirrors identical to the approved frontend source.
  cmp -s "$tmp/backend/docker/admin_Dockerfile" "$tmp/frontend/Dockerfile" \
    || die 'admin_Dockerfile differs from approved frontend Dockerfile; synchronize the recipe'
  cmp -s "$tmp/backend/docker/widget_Dockerfile" "$tmp/frontend/Dockerfile.widget" \
    || die 'widget_Dockerfile differs from approved frontend Dockerfile.widget; synchronize the recipe'
  ADMIN_REPO_DIR="$tmp/frontend" bash "$tmp/backend/scripts/check_widget_asset_pairing.sh"
  git -C "$tmp/frontend" archive "$frontend_sha" | tar -xf - -C "$tmp/admin"

  # Same verified asset build as release CI; Git HEAD is real, not an injected label.
  (cd "$tmp/frontend" && bun install --frozen-lockfile && bun run build:widget && bun run verify:widget)
  python3 - "$tmp/frontend/dist-widget/manifest.json" "$frontend_sha" <<'PY'
import json, sys
with open(sys.argv[1], encoding="utf-8") as f:
    manifest = json.load(f)
if manifest.get("source_head") != sys.argv[2]:
    raise SystemExit("Widget manifest source_head mismatch")
PY
  mkdir -p "$tmp/widget/docker/widget"
  cp "$tmp/frontend/docker/widget/nginx.conf" "$tmp/widget/docker/widget/"
  cp -R "$tmp/frontend/dist-widget" "$tmp/widget/"
  local frontend_version
  frontend_version="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["version"])' "$tmp/frontend/package.json")"

  local component context recipe revision source gh hub expected target metadata
  local output=(--load)
  if [[ "$push" = true ]]; then output=(--push); fi
  for component in "${components[@]}"; do
    local extra_labels=()
    revision="$backend_sha"
    source=https://github.com/imboy-pub/imboy
    case "$component" in
      backend) context="$tmp/backend"; recipe="$context/docker/backend_Dockerfile" ;;
      pg18) context="$tmp/backend"; recipe="$context/docker/pg18_Dockerfile" ;;
      admin|widget)
        context="$tmp/$component"
        recipe="$tmp/backend/docker/${component}_Dockerfile"
        revision="$frontend_sha"
        source=https://github.com/imboy-pub/imboy-admin-frontend
        extra_labels=(--label "pub.imboy.frontend.version=$frontend_version")
        ;;
    esac
    gh="$GHCR/imboy-$component:$candidate"
    hub="$HUB/$component:$candidate"
    metadata="$tmp/$component.metadata.json"
    printf '\n==> %s (%s) %s\n' "$component" "$platform" "$revision"
    docker buildx build --platform "$platform" --file "$recipe" \
      --label "org.opencontainers.image.source=$source" \
      --label "org.opencontainers.image.revision=$revision" \
      --label "org.opencontainers.image.version=$version" \
      ${extra_labels[@]+"${extra_labels[@]}"} \
      --tag "$gh" --tag "$hub" --metadata-file "$metadata" "${output[@]}" "$context"
    if [[ "$push" = true ]]; then
      expected="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["containerimage.digest"])' "$metadata")"
      [[ "$expected" =~ ^sha256:[0-9a-f]{64}$ ]] || die "Invalid build digest: $component"
      for target in "$gh" "$hub"; do
        [[ "$(digest_of "$target")" = "$expected" ]] || die "Registry digest mismatch: $target"
      done
      printf 'CANDIDATE %s/imboy-%s@%s %s/%s@%s\n' "$GHCR" "$component" "$expected" "$HUB" "$component" "$expected"
    fi
  done
  printf '\nFour candidate builds completed: %s (%s). NOT a release acceptance receipt.\n' "$candidate" "$platform"
}

# Only genuine manifest absence permits a new tag; network/auth errors fail closed.
hub_state() {
  local target="$1" expected="$2" actual error
  error="$(mktemp)"
  if actual="$(digest_of "$target" 2>"$error")"; then
    rm -f "$error"
    [[ "$actual" = "$expected" ]] || die "Refusing to replace existing tag: $target ($actual != $expected)"
    printf 'same\n'
  elif grep -Eqi 'unauthorized|denied|forbidden|timeout|timed out|TLS|connection' "$error"; then
    cat "$error" >&2; rm -f "$error"; return 1
  elif grep -Eqi 'manifest unknown|MANIFEST_UNKNOWN|: not found' "$error"; then
    rm -f "$error"; printf 'missing\n'
  else
    cat "$error" >&2; rm -f "$error"; return 1
  fi
}

publish_images() {
  local apply=false
  if [[ "${1:-}" = --apply ]]; then apply=true; shift; fi
  [[ $# = 5 ]] || die 'publish requires VERSION and four digest refs in backend pg18 admin widget order'
  local version="$1"; shift
  release_contract "$version" >/dev/null
  require docker
  if [[ "$apply" = true ]]; then require skopeo; fi
  local refs=("$@") targets=() expected_digests=()
  local i component ref expected target source_tag state
  for i in 0 1 2 3; do
    component="${components[$i]}"
    ref="${refs[$i]}"
    expected="${ref##*@}"
    [[ "$ref" = "$GHCR/imboy-$component@$expected" && "$expected" =~ ^sha256:[0-9a-f]{64}$ ]] \
      || die "Invalid $component digest ref: $ref"
    expected_digests+=("$expected")
    targets+=("$HUB/$component:$version")
  done

  # Preflight every source and target before the first write.
  for i in 0 1 2 3; do
    ref="${refs[$i]}"; expected="${expected_digests[$i]}"; target="${targets[$i]}"
    source_tag="${ref%@*}:$version"
    [[ "$(digest_of "$ref")" = "$expected" ]] || die "Source digest mismatch: $ref"
    [[ "$(digest_of "$source_tag")" = "$expected" ]] || die "GHCR release tag is missing or not the approved digest: $source_tag"
    state="$(hub_state "$target" "$expected")"
    printf '%s <- %s (%s)\n' "$target" "$ref" "$state"
  done
  [[ "$apply" = true ]] || return 0

  for i in 0 1 2 3; do
    ref="${refs[$i]}"; expected="${expected_digests[$i]}"; target="${targets[$i]}"
    # No Registry CAS: serialize release writers and recheck before each copy.
    state="$(hub_state "$target" "$expected")"
    if [[ "$state" = missing ]]; then
      skopeo copy --all --preserve-digests "docker://$ref" "docker://$target"
    fi
    [[ "$(digest_of "$target")" = "$expected" ]] || die "Docker Hub digest mismatch: $target"
  done
  printf 'Four Docker Hub release tags verified against the supplied GHCR digests.\n'
}

case "${1:-}" in
  build) shift; build_images "$@" ;;
  publish) shift; publish_images "$@" ;;
  -h|--help|'') usage ;;
  *) usage >&2; exit 2 ;;
esac
