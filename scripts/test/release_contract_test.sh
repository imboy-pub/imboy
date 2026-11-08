#!/usr/bin/env bash
# 版本映射和四镜像发布门禁的离线行为回归。Offline release-contract regressions.
# Docker and Git are strict mocks; this test cannot publish or inspect a real registry.
set -euo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
VERSION_SCRIPT="$ROOT/scripts/release_version.sh"
PROMOTE_SCRIPT="$ROOT/scripts/release_promote.sh"
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT
mkdir -p "$TMP/bin" "$TMP/fixture/deploy"
export IMBOY_RELEASE_ROOT="$TMP/fixture" IMBOY_RELEASE_OWNER=imboy-pub
export GITHUB_RUN_ID=741 GITHUB_RUN_ATTEMPT=2
export MOCK_HEAD=0123456789abcdef0123456789abcdef01234567
COUNT=0
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
pass() { COUNT=$((COUNT+1)); printf 'PASS %02d: %s\n' "$COUNT" "$*"; }
assert_line() { printf '%s\n' "$1" | grep -Fxq -- "$2" || fail "Missing output: $2"; }
fixture() {
  printf '%s\n' "$1" > "$IMBOY_RELEASE_ROOT/VERSION"
  printf 'IMBOY_VERSION=%s\n' "${2:-$1}" > "$IMBOY_RELEASE_ROOT/deploy/.env.example"
}

cat > "$TMP/bin/git" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ $# == 4 && "$1" == -C && "$2" == "$IMBOY_RELEASE_ROOT" && "$3" == rev-parse && "$4" == HEAD ]] || exit 90
printf '%s\n' "$MOCK_HEAD"
MOCK
cat > "$TMP/bin/docker" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ $# -ge 4 && "$1" == buildx && "$2" == imagetools ]] || exit 91
operation=$3
case "$operation" in
  inspect)
    [[ $# == 6 && "$5" == --format && "$6" == '{{.Manifest.Digest}}' ]] || exit 92
    subject=$4
    printf 'inspect %s\n' "$subject" >> "$MOCK_LOG" ;;
  create)
    [[ $# == 7 && "$4" == --prefer-index=false && "$5" == --tag ]] || exit 93
    subject=$6
    source=$7
    printf 'create %s %s\n' "$subject" "$source" >> "$MOCK_LOG" ;;
  *) exit 94 ;;
esac
component=${subject##*/}; component=${component%%@*}; component=${component%%:*}
case "$component" in imboy-backend|imboy-admin|imboy-widget|imboy-pg18) ;; *) exit 95 ;; esac
state="$MOCK_STATE/$component.digest"
other="sha256:$(printf '%064d' 999)"
if [[ "$operation" == create ]]; then
  [[ "$source" == ghcr.io/*/"$component"@sha256:* ]] || exit 96
  digest=${source##*@}
  if [[ "$MOCK_SCENARIO" == post_mismatch && "$component" == imboy-backend ]]; then digest=$other; fi
  printf '%s\n' "$digest" > "$state"
  exit 0
fi
if [[ "$subject" == *@* ]]; then
  if [[ "$MOCK_SCENARIO" == source_mismatch && "$component" == imboy-widget ]]; then
    printf '%s\n' "$other"
  else printf '%s\n' "${subject##*@}"; fi
  exit 0
fi
counter="$MOCK_STATE/$component.count"
n=0
if [[ -f "$counter" ]]; then read -r n < "$counter"; fi
n=$((n+1)); printf '%s\n' "$n" > "$counter"
if [[ "$component" == imboy-widget ]]; then
  case "$MOCK_SCENARIO" in
    auth) printf 'HTTP 401 Unauthorized: not found\n' >&2; exit 1 ;;
    timeout) printf 'net/http: TLS handshake timeout\n' >&2; exit 1 ;;
    unknown_error) printf 'upstream unavailable (HTTP 503)\n' >&2; exit 1 ;;
  esac
fi
if [[ "$MOCK_SCENARIO" == race && "$component" == imboy-backend && "$n" == 2 ]]; then
  printf '%s\n' "$other" > "$state"
fi
if [[ -f "$state" ]]; then cat "$state"; exit 0; fi
printf 'ERROR: manifest unknown: MANIFEST_UNKNOWN\n' >&2
exit 1
MOCK
chmod +x "$TMP/bin/git" "$TMP/bin/docker"
export PATH="$TMP/bin:$PATH"

fixture 1.0.0-alpha.84
out=$(bash "$VERSION_SCRIPT" v1.0.0-alpha.84)
assert_line "$out" 'RELEASE_TAG=v1.0.0-alpha.84'
assert_line "$out" 'IMBOY_VERSION=1.0.0-alpha.84'
assert_line "$out" 'PRERELEASE=true'
pass 'alpha version and prerelease identity are preserved'

fixture 1.2.3
out=$(bash "$VERSION_SCRIPT" v1.2.3)
assert_line "$out" 'RELEASE_TAG=v1.2.3'
assert_line "$out" 'IMBOY_VERSION=1.2.3'
assert_line "$out" 'PRERELEASE=false'
pass 'stable version retains its exact identity'

reject_version() {
  local name=$1 tag=$2 file_version=$3 env_version=${4:-$3}
  fixture "$file_version" "$env_version"
  if bash "$VERSION_SCRIPT" "$tag" > "$TMP/version.out" 2> "$TMP/version.err"; then fail "$name was accepted"; fi
  pass "$name is rejected"
}
reject_version 'tag without v prefix' 1.2.3 1.2.3
reject_version 'leading zero in core version' v01.2.3 01.2.3
reject_version 'leading zero in numeric prerelease' v1.2.3-alpha.01 1.2.3-alpha.01
reject_version 'build metadata that cannot map losslessly to Docker tags' v1.2.3+build.7 1.2.3+build.7
reject_version 'empty prerelease identifier' v1.2.3-alpha..7 1.2.3-alpha..7
reject_version 'empty prerelease suffix' v1.2.3- 1.2.3-
reject_version 'Git tag and VERSION mismatch' v1.2.3 1.2.4
reject_version 'VERSION and environment-template mismatch' v1.2.3 1.2.3 1.2.4

fixture 1.2.3-alpha.7
out=$(bash "$VERSION_SCRIPT" v1.2.3-alpha.7)
assert_line "$out" 'CANDIDATE_TAG=1.2.3-alpha.7-candidate-0123456789ab-741-2'
out_attempt=$(GITHUB_RUN_ATTEMPT=3 bash "$VERSION_SCRIPT" v1.2.3-alpha.7)
assert_line "$out_attempt" 'CANDIDATE_TAG=1.2.3-alpha.7-candidate-0123456789ab-741-3'
out_run=$(GITHUB_RUN_ID=742 bash "$VERSION_SCRIPT" v1.2.3-alpha.7)
assert_line "$out_run" 'CANDIDATE_TAG=1.2.3-alpha.7-candidate-0123456789ab-742-2'
[[ "$out" != "$out_attempt" && "$out" != "$out_run" ]] || fail 'candidate identity collapsed across runs or attempts'
pass 'candidate includes source SHA, run ID and retry attempt'

VERSION=1.2.3-alpha.7
COMPONENTS=(backend admin widget pg18)
REFS=()
for i in 0 1 2 3; do
  REFS+=("ghcr.io/imboy-pub/imboy-${COMPONENTS[$i]}@sha256:$(printf '%064d' "$((i+1))")")
done
prepare_promotion() {
  local name=$1
  export MOCK_SCENARIO=$2 MOCK_STATE="$TMP/$name.state" MOCK_LOG="$TMP/$name.calls"
  mkdir "$MOCK_STATE"
  : > "$MOCK_LOG"
  fixture "$VERSION"
}
promote_pass() {
  if ! bash "$PROMOTE_SCRIPT" "$@" "$VERSION" "${REFS[@]}" > "$TMP/promote.out" 2> "$TMP/promote.err"; then
    cat "$TMP/promote.err" >&2; fail "promotion unexpectedly failed: $MOCK_SCENARIO"
  fi
}
promote_reject() {
  if bash "$PROMOTE_SCRIPT" --apply "$VERSION" "${REFS[@]}" > "$TMP/promote.out" 2> "$TMP/promote.err"; then
    fail "promotion unexpectedly passed: $MOCK_SCENARIO"
  fi
}
assert_no_write() { if grep -q '^create ' "$MOCK_LOG"; then fail 'a rejected/read-only operation wrote a registry tag'; fi; }
assert_all_preflight_before_write() {
  local line ref target
  : > "$TMP/before-first-write"
  while IFS= read -r line; do
    [[ "$line" != create\ * ]] || break
    printf '%s\n' "$line" >> "$TMP/before-first-write"
  done < "$MOCK_LOG"
  for ref in "${REFS[@]}"; do
    target="${ref%@*}:$VERSION"
    grep -Fxq "inspect $ref" "$TMP/before-first-write" || fail "source was not checked before first write: $ref"
    grep -Fxq "inspect $target" "$TMP/before-first-write" || fail "target was not checked before first write: $target"
  done
}

prepare_promotion readonly missing
promote_pass
assert_no_write
[[ $(grep -c '^inspect ' "$MOCK_LOG") == 8 ]] || fail 'read-only check did not inspect all four sources and targets'
pass 'default invocation is read-only and checks all four images'

prepare_promotion create missing
promote_pass --apply
[[ $(grep -c '^create ' "$MOCK_LOG") == 4 ]] || fail 'four missing targets were not created exactly once'
assert_all_preflight_before_write
for i in 0 1 2 3; do
  [[ $(cat "$MOCK_STATE/imboy-${COMPONENTS[$i]}.digest") == "${REFS[$i]##*@}" ]] || fail 'promoted digest changed'
done
pass 'all four missing tags are created only after every preflight passes'

promote_pass --apply
[[ $(grep -c '^create ' "$MOCK_LOG") == 4 ]] || fail 'repeated promotion recreated an already matching tag'
pass 'matching digests make a repeated apply idempotent'

prepare_promotion conflict missing
printf 'sha256:%064d\n' 999 > "$MOCK_STATE/imboy-widget.digest"
promote_reject
assert_no_write
pass 'one conflicting target blocks every write and is never overwritten'

prepare_promotion auth auth
promote_reject
assert_no_write
pass '401 takes priority over not-found wording and blocks promotion'

prepare_promotion timeout timeout
promote_reject
assert_no_write
pass 'lookup timeout is not treated as a missing image'

prepare_promotion unknown unknown_error
promote_reject
assert_no_write
pass 'unclassified registry failure cannot establish target absence'

prepare_promotion race race
promote_reject
assert_no_write
[[ $(cat "$MOCK_STATE/imboy-backend.count") == 2 ]] || fail 'target was not rechecked immediately before write'
pass 'a conflicting digest appearing after preflight blocks the first write'

prepare_promotion post_mismatch post_mismatch
promote_reject
[[ $(grep -c '^create ' "$MOCK_LOG") == 1 ]] || fail 'promotion continued after a post-write digest mismatch'
pass 'post-write digest mismatch stops before the remaining three writes'

prepare_promotion owner missing
original_backend=${REFS[0]}
REFS[0]=${REFS[0]/ghcr.io\/imboy-pub\//ghcr.io\/unapproved-owner\/}
promote_reject
REFS[0]=$original_backend
[[ ! -s "$MOCK_LOG" ]] || fail 'unapproved owner reached a registry operation'
pass 'an owner outside IMBOY_RELEASE_OWNER is rejected before registry access'

prepare_promotion source source_mismatch
promote_reject
assert_no_write
pass 'source digest mismatch blocks promotion before any write'

printf 'All %s offline release-contract behavior checks passed; no real Git or Docker operations ran.\n' "$COUNT"
