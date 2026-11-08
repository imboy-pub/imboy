#!/usr/bin/env bash
# 离线失败分支回归；不会访问真实 Registry。Offline fail-closed verifier tests.
set -euo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
SCRIPT="$ROOT/scripts/verify_community_images.sh"
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT
mkdir "$TMP/bin" "$TMP/fixtures" "$TMP/results"
export VERIFY_FIXTURES="$TMP/fixtures" VERIFY_CALLS="$TMP/calls.log"
export VERIFY_REAL_JQ="$(command -v jq)" VERIFY_REAL_AWK="$(command -v awk)" VERIFY_REAL_CP="$(command -v cp)"
# Deliberately hostile inherited auth: the verifier must not export TOKEN or pass
# DOCKER_AUTH_CONFIG through to its isolated Docker child.
export TOKEN=DO_NOT_RETAIN_EXPORTED_TOKEN
export DOCKER_AUTH_CONFIG='{"auths":{"ghcr.io":{"auth":"DO_NOT_RETAIN_AUTH_OVERRIDE"}}}'

jq -n '{os:"linux",architecture:"amd64",rootfs:{type:"layers",diff_ids:[]},config:{Env:["DO_NOT_RETAIN_CONFIG_ENV=fixture"],Labels:{"org.opencontainers.image.source":"https://github.com/imboy-pub/imboy","org.opencontainers.image.revision":"0123456789012345678901234567890123456789","secret.internal":"DO_NOT_RETAIN_OTHER_LABEL"}}}' > "$VERIFY_FIXTURES/config-amd64.json"
jq '.architecture="arm64" | .variant="v8"' "$VERIFY_FIXTURES/config-amd64.json" > "$VERIFY_FIXTURES/config-arm64.json"
export VERIFY_AMD_CONFIG_DIGEST="sha256:$(sha256sum < "$VERIFY_FIXTURES/config-amd64.json" | cut -d' ' -f1)"
export VERIFY_ARM_CONFIG_DIGEST="sha256:$(sha256sum < "$VERIFY_FIXTURES/config-arm64.json" | cut -d' ' -f1)"
for arch in amd64 arm64; do
  config_digest="sha256:$(sha256sum < "$VERIFY_FIXTURES/config-$arch.json" | cut -d' ' -f1)"
  config_size=$(wc -c < "$VERIFY_FIXTURES/config-$arch.json")
  jq -n --arg digest "$config_digest" --argjson size "$config_size" '{schemaVersion:2,mediaType:"application/vnd.oci.image.manifest.v1+json",config:{mediaType:"application/vnd.oci.image.config.v1+json",digest:$digest,size:$size},layers:[]}' > "$VERIFY_FIXTURES/manifest-$arch.json"
done
export VERIFY_AMD_MANIFEST_DIGEST="sha256:$(sha256sum < "$VERIFY_FIXTURES/manifest-amd64.json" | cut -d' ' -f1)"
export VERIFY_ARM_MANIFEST_DIGEST="sha256:$(sha256sum < "$VERIFY_FIXTURES/manifest-arm64.json" | cut -d' ' -f1)"
single_digest=$VERIFY_AMD_MANIFEST_DIGEST
[[ "$VERIFY_AMD_MANIFEST_DIGEST" != "$VERIFY_ARM_MANIFEST_DIGEST" ]]
amd_size=$(wc -c < "$VERIFY_FIXTURES/manifest-amd64.json")
arm_size=$(wc -c < "$VERIFY_FIXTURES/manifest-arm64.json")
jq -n --arg amd "$VERIFY_AMD_MANIFEST_DIGEST" --arg arm "$VERIFY_ARM_MANIFEST_DIGEST" --argjson amd_size "$amd_size" --argjson arm_size "$arm_size" \
  '{schemaVersion:2,mediaType:"application/vnd.oci.image.index.v1+json",manifests:[{digest:$amd,size:$amd_size,mediaType:"application/vnd.oci.image.manifest.v1+json",platform:{os:"linux",architecture:"amd64"}},{digest:$arm,size:$arm_size,mediaType:"application/vnd.oci.image.manifest.v1+json",platform:{os:"linux",architecture:"arm64",variant:"v8"}},{digest:$amd,size:$amd_size,mediaType:"application/vnd.oci.image.manifest.v1+json",platform:{os:"unknown",architecture:"unknown"},annotations:{"vnd.docker.reference.type":"attestation-manifest"}}]}' > "$VERIFY_FIXTURES/index.json"
jq --arg amd "$VERIFY_AMD_MANIFEST_DIGEST" --argjson size "$amd_size" '.manifests[1].digest=$amd | .manifests[1].size=$size' "$VERIFY_FIXTURES/index.json" > "$VERIFY_FIXTURES/index-mislabel.json"
jq '.manifests[1].platform.variant="v9"' "$VERIFY_FIXTURES/index.json" > "$VERIFY_FIXTURES/index-variant.json"

cat > "$TMP/bin/curl" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ ${TOKEN+x} != x ]] || exit 98
out='' headers='' url='' auth_stdin=false
printf 'curl %s\n' "$*" >> "$VERIFY_CALLS"
while (($#)); do
  case "$1" in
    --output) out=$2; shift 2 ;;
    --dump-header) headers=$2; shift 2 ;;
    --header) [[ "$2" != '@-' ]] || auth_stdin=true; shift 2 ;;
    --connect-timeout|--max-time|--proto|--proto-redir|--write-out) shift 2 ;;
    https://*) url=$1; shift ;;
    *) shift ;;
  esac
done
if [[ "$url" == *'/token?'* ]]; then
  case "$VERIFY_SCENARIO" in
    network) printf '\n000'; exit 7 ;;
    auth) printf '{"errors":[{"code":"DENIED"}]}\n403'; exit 0 ;;
    *) printf '{"token":"DO_NOT_RETAIN_ANONYMOUS_TOKEN"}\n200'; exit 0 ;;
  esac
fi
"$auth_stdin" || exit 93
IFS= read -r authorization
[[ "$authorization" == 'Authorization: Bearer DO_NOT_RETAIN_ANONYMOUS_TOKEN' ]] || exit 94
status=200
if [[ "$url" == *'/tags/list?'* ]]; then
  repository=${url#https://ghcr.io/v2/}; repository=${repository%/tags/list*}
  if [[ "$VERIFY_SCENARIO" == hidden ]]; then
    printf '{"errors":[{"code":"DENIED"}]}' > "$out"; status=403
  elif [[ "$VERIFY_SCENARIO" == tags_name ]]; then
    printf '{"name":"wrong/repository","tags":[]}' > "$out"
  elif [[ "$VERIFY_SCENARIO" == tags_type ]]; then
    jq -n --arg repository "$repository" '{name:$repository,tags:{}}' > "$out"
  else jq -n --arg repository "$repository" '{name:$repository,tags:[]}' > "$out"; fi
elif [[ "$url" == *'/blobs/'* ]]; then
  case "${url##*/}" in
    "$VERIFY_AMD_CONFIG_DIGEST") cp "$VERIFY_FIXTURES/config-amd64.json" "$out" ;;
    "$VERIFY_ARM_CONFIG_DIGEST") cp "$VERIFY_FIXTURES/config-arm64.json" "$out" ;;
    *) printf '{"errors":[{"code":"BLOB_UNKNOWN"}]}' > "$out"; status=404 ;;
  esac
  [[ "$VERIFY_SCENARIO" != config_corrupt ]] || printf '\ncorrupt' >> "$out"
elif [[ "${url##*/}" == "$VERIFY_ARM_MANIFEST_DIGEST" ]]; then
  if [[ "$VERIFY_SCENARIO" == child_missing ]]; then
    printf '{"errors":[{"code":"MANIFEST_UNKNOWN"}]}' > "$out"; status=404
  elif [[ "$VERIFY_SCENARIO" == child_digest ]]; then cp "$VERIFY_FIXTURES/manifest-amd64.json" "$out"
  else cp "$VERIFY_FIXTURES/manifest-arm64.json" "$out"; fi
elif [[ "${url##*/}" == "$VERIFY_AMD_MANIFEST_DIGEST" ]]; then cp "$VERIFY_FIXTURES/manifest-amd64.json" "$out"
elif [[ "$VERIFY_SCENARIO" == missing || "$VERIFY_SCENARIO" == hidden || "$VERIFY_SCENARIO" == tags_name || "$VERIFY_SCENARIO" == tags_type ]]; then
  printf '{"errors":[{"code":"MANIFEST_UNKNOWN"}]}' > "$out"; status=404
elif [[ "$VERIFY_SCENARIO" == mislabel ]]; then cp "$VERIFY_FIXTURES/index-mislabel.json" "$out"
elif [[ "$VERIFY_SCENARIO" == variant_mismatch ]]; then cp "$VERIFY_FIXTURES/index-variant.json" "$out"
elif [[ "$VERIFY_SCENARIO" == index || "$VERIFY_SCENARIO" == child_missing || "$VERIFY_SCENARIO" == child_digest ]]; then cp "$VERIFY_FIXTURES/index.json" "$out"
else cp "$VERIFY_FIXTURES/manifest-amd64.json" "$out"; fi
printf 'HTTP/2 %s\r\nContent-Type: application/json\r\n' "$status" > "$headers"
if [[ "$url" == *'/manifests/'* && "$status" == 200 ]]; then
  digest="sha256:$(sha256sum < "$out" | cut -d' ' -f1)"
  [[ "$VERIFY_SCENARIO" != digest ]] || digest="sha256:$(printf '%064d' 0)"
  printf 'Docker-Content-Digest: %s\r\n' "$digest" >> "$headers"
fi
# A signed redirect must never survive in evidence headers.
printf 'Location: https://fixture.invalid/config?signature=DO_NOT_RETAIN_SIGNED_URL\r\n\r\n' >> "$headers"
printf '%s' "$status"
MOCK

cat > "$TMP/bin/docker" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ ${TOKEN+x} != x && ${DOCKER_AUTH_CONFIG-unset} == '' ]] || exit 98
printf 'docker %s\n' "$*" >> "$VERIFY_CALLS"
[[ "$1" == --config && "$2" == "$DOCKER_CONFIG" && "$3" == pull && "$4" == --platform ]] || exit 95
jq -e 'keys==["auths"] and (.auths|keys)==["ghcr.io"] and .auths["ghcr.io"]=={} and (has("credsStore")|not) and (has("credHelpers")|not)' "$DOCKER_CONFIG/config.json" >/dev/null || exit 96
[[ "$6" == ghcr.io/*@sha256:* ]] || exit 97
if [[ "$VERIFY_SCENARIO" == pull_fail && "$6" == *imboy-widget@* ]]; then exit 1; fi
printf 'Digest: %s\n' "${6##*@}"
MOCK
cat > "$TMP/bin/jq" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
if [[ ${VERIFY_SCENARIO:-} == jq_failure && "$*" == *'registry_error_codes:$errors}'* ]]; then exit 71; fi
exec "$VERIFY_REAL_JQ" "$@"
MOCK
cat > "$TMP/bin/awk" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
if [[ ${VERIFY_SCENARIO:-} == awk_failure && "$1" == 'tolower($0)'* ]]; then exit 72; fi
exec "$VERIFY_REAL_AWK" "$@"
MOCK
cat > "$TMP/bin/cp" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
if [[ ${VERIFY_SCENARIO:-} == cp_failure && "$2" == */config.request.json ]]; then exit 73; fi
exec "$VERIFY_REAL_CP" "$@"
MOCK
chmod +x "$TMP/bin/curl" "$TMP/bin/docker" "$TMP/bin/jq" "$TMP/bin/awk" "$TMP/bin/cp"
export PATH="$TMP/bin:$PATH"

run_case() {
  local name=$1 scenario=$2 expected=$3 rc
  shift 3
  if VERIFY_SCENARIO="$scenario" bash "$SCRIPT" --output "$TMP/results/$name" "$@" > "$TMP/$name.log" 2>&1; then rc=0; else rc=$?; fi
  if [[ "$expected" == PASS ]]; then
    [[ "$rc" == 0 ]] || { cat "$TMP/$name.log"; printf 'FAIL: %s unexpected exit %s\n' "$name" "$rc" >&2; exit 1; }
    jq -e '.ok and (.images|length==4)' "$TMP/results/$name/summary.json" >/dev/null
  else
    [[ "$rc" != 0 ]] || { printf 'FAIL: %s incorrectly passed\n' "$name" >&2; exit 1; }
    jq -e --arg expected "$expected" '(.ok|not) and (.images|length==4) and all(.images[]; .status==$expected)' "$TMP/results/$name/summary.json" >/dev/null \
      || { cat "$TMP/$name.log"; printf 'FAIL: %s did not report %s\n' "$name" "$expected" >&2; exit 1; }
  fi
  printf 'PASS: %s\n' "$name"
}

run_case single single PASS --version 1.0.0-alpha.84
jq -e 'all(.images[]; .status=="verified_metadata" and (.pull_verified|not) and .platforms==["linux/amd64"])' "$TMP/results/single/summary.json" >/dev/null
run_case multiarch index PASS --version 1.0.0-alpha.84 --platform linux/amd64,linux/arm64 --pull
jq -e 'all(.images[]; .pull_verified and .platforms==["linux/amd64","linux/arm64/v8"] and (.child_manifests|length)==2 and (.child_manifests|map(.manifest_digest)|unique|length)==2 and (.child_manifests|map(.config_digest)|unique|length)==2)' "$TMP/results/multiarch/summary.json" >/dev/null
run_case forged_arm64 mislabel platform_mismatch --version 1.0.0-alpha.84 --platform linux/amd64,linux/arm64
run_case variant_mismatch variant_mismatch platform_mismatch --version 1.0.0-alpha.84 --platform linux/arm64
run_case missing_child child_missing child_missing --version 1.0.0-alpha.84 --platform linux/amd64,linux/arm64
run_case child_digest child_digest digest_mismatch --version 1.0.0-alpha.84 --platform linux/arm64
run_case explicit single PASS \
  --backend "ghcr.io/imboy-pub/imboy-backend@$single_digest" \
  --admin "ghcr.io/imboy-pub/imboy-admin@$single_digest" \
  --widget "ghcr.io/imboy-pub/imboy-widget@$single_digest" \
  --pg "ghcr.io/imboy-pub/imboy-pg18@$single_digest"
run_case digest_mismatch digest digest_mismatch --version 1.0.0-alpha.84
run_case config_corrupt config_corrupt digest_mismatch --version 1.0.0-alpha.84
run_case permissions auth access_unconfirmed --version 1.0.0-alpha.84
run_case missing missing missing --version 1.0.0-alpha.84
run_case hidden hidden access_unconfirmed --version 1.0.0-alpha.84
run_case tags_repository_identity tags_name access_unconfirmed --version 1.0.0-alpha.84
run_case tags_response_type tags_type access_unconfirmed --version 1.0.0-alpha.84
run_case network network network_error --version 1.0.0-alpha.84
run_case architecture single platform_missing --version 1.0.0-alpha.84 --platform linux/arm64
run_case jq_evidence_failure jq_failure evidence_error --version 1.0.0-alpha.84
run_case awk_evidence_failure awk_failure evidence_error --version 1.0.0-alpha.84
run_case cp_evidence_failure cp_failure evidence_error --version 1.0.0-alpha.84

if VERIFY_SCENARIO=pull_fail bash "$SCRIPT" --version 1.0.0-alpha.84 --pull --output "$TMP/results/pull_fail" > "$TMP/pull_fail.log" 2>&1; then
  printf 'FAIL: failed digest pull incorrectly passed\n' >&2; exit 1
fi
jq -e '(.ok|not) and (.images|length==4) and any(.images[]; .component=="widget" and .status=="pull_failed") and all(.images[]|select(.component!="widget"); .pull_verified)' "$TMP/results/pull_fail/summary.json" >/dev/null
printf 'PASS: failed digest pull blocks the gate without skipping other images\n'

if grep -R -E 'DO_NOT_RETAIN_(ANONYMOUS_TOKEN|CONFIG_ENV|OTHER_LABEL|SIGNED_URL|AUTH_OVERRIDE|EXPORTED_TOKEN)' "$TMP/results" "$VERIFY_CALLS"; then
  printf 'FAIL: sensitive fixture retained in evidence or argv\n' >&2; exit 1
fi
printf 'PASS: bearer, config secrets, and signed redirects are absent from evidence and argv\n'

if VERIFY_SCENARIO=single bash "$SCRIPT" --version latest --output "$TMP/results/latest" >/dev/null 2>&1; then
  printf 'FAIL: latest was accepted\n' >&2; exit 1
fi
if VERIFY_SCENARIO=single bash "$SCRIPT" --version 1.0.0-alpha.84 --output "$TMP/results/single" >/dev/null 2>&1; then
  printf 'FAIL: prior evidence was overwritten\n' >&2; exit 1
fi
printf 'PASS: latest and evidence overwrite are rejected\n'
printf 'All community image verifier offline regressions passed.\n'
