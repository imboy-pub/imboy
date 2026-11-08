#!/usr/bin/env bash
# 社区镜像匿名核验；只读 Registry，不登录、不推送。Anonymous, read-only GHCR checks.
set +x
set -euo pipefail
umask 077

usage() {
  cat <<'USAGE'
Usage:
  verify_community_images.sh --version 1.0.0-alpha.84 [options]
  verify_community_images.sh --backend REF --admin REF --widget REF --pg REF [options]
Options:
  --platform linux/amd64[,linux/arm64]  Required platforms (default: linux/amd64)
  --output DIR                        New evidence directory (must not exist)
  --pull                              Also pull every required platform by digest
REF must be an explicit ghcr.io/OWNER/IMAGE:TAG or ghcr.io/OWNER/IMAGE@sha256:HASH.
--version uses ghcr.io/imboy-pub/imboy-{backend,admin,widget,pg18}:VERSION.
Anonymous only. No login or publication. Metadata success does not prove layer pull.
Requires Bash, curl, jq, sha256sum; --pull additionally requires a working Docker daemon.
USAGE
}
die() { printf 'ERROR: %s\n' "$*" >&2; exit 2; }
VERSION='' OUTPUT='' PLATFORMS='linux/amd64' PULL=false
BACKEND='' ADMIN='' WIDGET='' PG=''
while (($#)); do
  case "$1" in
    --version|--output|--platform|--backend|--admin|--widget|--pg)
      (($# >= 2)) || die "$1 requires a value"
      case "$1" in
        --version) VERSION=$2 ;; --output) OUTPUT=$2 ;; --platform) PLATFORMS=$2 ;;
        --backend) BACKEND=$2 ;; --admin) ADMIN=$2 ;; --widget) WIDGET=$2 ;; --pg) PG=$2 ;;
      esac
      shift 2 ;;
    --pull) PULL=true; shift ;;
    -h|--help) usage; exit 0 ;;
    *) die "Unknown argument: $1" ;;
  esac
done
if [[ -n "$VERSION" ]]; then
  [[ -z "$BACKEND$ADMIN$WIDGET$PG" ]] || die '--version and explicit refs are mutually exclusive'
  [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z][0-9A-Za-z.-]*)?$ ]] || die 'Expected unprefixed version, preserving its prerelease suffix'
  BACKEND="ghcr.io/imboy-pub/imboy-backend:$VERSION"
  ADMIN="ghcr.io/imboy-pub/imboy-admin:$VERSION"
  WIDGET="ghcr.io/imboy-pub/imboy-widget:$VERSION"
  PG="ghcr.io/imboy-pub/imboy-pg18:$VERSION"
fi
[[ -n "$BACKEND" && -n "$ADMIN" && -n "$WIDGET" && -n "$PG" ]] || die 'Supply --version or all four image refs'
REFS=("$BACKEND" "$ADMIN" "$WIDGET" "$PG")
COMPONENTS=(backend admin widget pg)
for ref in "${REFS[@]}"; do
  [[ "$ref" =~ ^ghcr\.io/[a-z0-9][a-z0-9._-]*/[a-z0-9][a-z0-9._/-]*(:[A-Za-z0-9_][A-Za-z0-9_.-]*|@sha256:[0-9a-f]{64})$ ]] || die "Unsupported explicit GHCR ref: $ref"
  case "$ref" in */../*|*/./*|*//*|*:latest) die "Unsafe or mutable-default ref: $ref" ;; esac
done
[[ -n "$PLATFORMS" && "$PLATFORMS" != ,* && "$PLATFORMS" != *, && "$PLATFORMS" != *,,* ]] || die 'Invalid platform list'
IFS=',' read -r -a REQUIRED <<< "$PLATFORMS"
for platform in "${REQUIRED[@]}"; do
  [[ "$platform" =~ ^linux/[a-z0-9_]+(/v[0-9]+)?$ ]] || die "Invalid Linux platform: $platform"
done
for dep in curl jq sha256sum; do command -v "$dep" >/dev/null || die "Required tool unavailable: $dep"; done
if "$PULL"; then command -v docker >/dev/null || die 'Docker is required for --pull'; fi
OUTPUT=${OUTPUT:-"image-evidence-$(date -u +%Y%m%dT%H%M%SZ)-$$"}
[[ ! -e "$OUTPUT" ]] || die "Evidence directory already exists: $OUTPUT"
mkdir -p -- "$OUTPUT"
TMP=$(mktemp -d)
TOKEN=''
export -n TOKEN
cleanup() { unset TOKEN; rm -rf -- "$TMP"; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
mkdir "$TMP/docker"
# A nonempty auths map suppresses Docker's default credential-helper discovery.
printf '{"auths":{"ghcr.io":{}}}\n' > "$TMP/docker/config.json"
REQUIRED_JSON=$(printf '%s\n' "${REQUIRED[@]}" | jq -R . | jq -s .)
ACCEPT='application/vnd.oci.image.index.v1+json, application/vnd.docker.distribution.manifest.list.v2+json, application/vnd.oci.image.manifest.v1+json, application/vnd.docker.distribution.manifest.v2+json'

fail() { STATE=$1; DETAIL=$2; return 1; }
file_digest() { local line; line=$(sha256sum < "$1") || return; printf 'sha256:%s\n' "${line%% *}"; }
header_digest() {
  awk 'tolower($1)=="docker-content-digest:" {gsub("\r", "", $2); value=$2} END {print value}' "$1"
}

# token 响应只留在内存；凭据经 stdin 送入 curl，不进 argv 或证据文件。
# Keep the token in memory, passing the Authorization header via stdin only.
get_token() {
  local response payload curl_rc error_codes
  if response=$(curl -q --silent --show-error --connect-timeout 10 --max-time 30 \
      --proto '=https' --write-out $'\n%{http_code}' \
      "https://ghcr.io/token?service=ghcr.io&scope=repository:${REPOSITORY}:pull" 2>"$DIR/token.stderr"); then
    curl_rc=0
  else curl_rc=$?; fi
  TOKEN_HTTP=${response##*$'\n'}
  payload=${response%$'\n'*}
  error_codes=$(printf '%s' "$payload" | jq -c '[.errors[]?.code | select(type=="string")]' 2>/dev/null) || error_codes='[]'
  [[ -n "$error_codes" ]] || error_codes='[]'
  jq -n --arg status "$TOKEN_HTTP" --argjson curl_exit "$curl_rc" --argjson errors "$error_codes" \
    '{http_status:$status,curl_exit:$curl_exit,registry_error_codes:$errors}' > "$DIR/token-result.json" \
    || { fail evidence_error 'Unable to write token result'; return 1; }
  ((curl_rc == 0)) || { fail network_error 'Anonymous token request did not complete'; return 1; }
  if [[ "$TOKEN_HTTP" != 200 ]]; then
    case "$TOKEN_HTTP" in
      401|403) fail access_unconfirmed "Anonymous token HTTP $TOKEN_HTTP; existence and visibility remain unconfirmed" ;;
      *) fail registry_error "Anonymous token HTTP $TOKEN_HTTP; not classified as missing" ;;
    esac
    return 1
  fi
  TOKEN=$(printf '%s' "$payload" | jq -er '.token // .access_token | select(type=="string" and length>0)' 2>/dev/null) || { fail invalid_token_response 'No anonymous bearer returned'; return 1; }
  [[ "$TOKEN" != *$'\r'* && "$TOKEN" != *$'\n'* ]] || { fail invalid_token_response 'Invalid bearer header'; return 1; }
}

# 仅保留安全响应头；blob 重定向可能带短期签名 URL，不能写入交付证据。
# Retain selected headers only; blob redirects may contain signed URLs.
get_object() {
  local url=$1 stem=$2 kind=$3 curl_rc
  : > "$TMP/headers" || { fail evidence_error 'Unable to prepare response headers'; return 1; }
  : > "$stem.stderr" || { fail evidence_error 'Unable to prepare request diagnostics'; return 1; }
  : > "$stem.body" || { fail evidence_error 'Unable to prepare response body'; return 1; }
  if HTTP_STATUS=$(printf 'Authorization: Bearer %s\n' "$TOKEN" | \
      curl -q --silent --show-error --connect-timeout 10 --max-time 60 \
        --proto '=https' --proto-redir '=https' --location --header @- \
        --header "Accept: $ACCEPT" --dump-header "$TMP/headers" \
        --output "$stem.body" --write-out '%{http_code}' "$url" 2>"$stem.stderr"); then
    curl_rc=0
  else curl_rc=$?; fi
  if [[ -f "$TMP/headers" ]]; then
    awk 'tolower($0) ~ /^(http\/|docker-content-digest:|content-type:|date:|x-github-request-id:|www-authenticate:)/' "$TMP/headers" > "$stem.headers" \
      || { fail evidence_error 'Unable to retain safe response headers'; return 1; }
  else : > "$stem.headers" || { fail evidence_error 'Unable to write response headers'; return 1; }; fi
  jq -n --arg url "$url" --arg http "$HTTP_STATUS" --argjson curl_exit "$curl_rc" \
    '{url:$url,http_status:$http,curl_exit:$curl_exit}' > "$stem.request.json" \
    || { fail evidence_error 'Unable to write request result'; return 1; }
  ((curl_rc != 23)) || { fail evidence_error 'curl could not write its response'; return 1; }
  ((curl_rc == 0)) || { fail network_error "Registry $kind request did not complete"; return 1; }
  [[ "$HTTP_STATUS" == 200 ]] && return 0
  if [[ "$HTTP_STATUS" == 404 && ( "$kind" == manifest || "$kind" == child_manifest ) ]] && \
      jq -e 'any(.errors[]?; .code=="MANIFEST_UNKNOWN")' "$stem.body" >/dev/null 2>&1; then
    if [[ "$kind" == child_manifest ]]; then
      fail child_missing 'Verified index references an unavailable child manifest'; return 1
    fi
    # A hidden repository must not be mistaken for an absent tag.
    get_object "https://ghcr.io/v2/$REPOSITORY/tags/list?n=1" "$DIR/visibility" tags || return 1
    jq -e --arg repository "$REPOSITORY" '.name==$repository and (.tags|type)=="array" and all(.tags[]; type=="string")' \
      "$DIR/visibility.body" >/dev/null 2>&1 || { fail access_unconfirmed 'tags/list did not establish readable repository identity'; return 1; }
    fail missing 'MANIFEST_UNKNOWN in an anonymously readable repository'
  elif [[ "$HTTP_STATUS" == 401 || "$HTTP_STATUS" == 403 ]]; then
    fail access_unconfirmed "Registry HTTP $HTTP_STATUS; existence and visibility remain unconfirmed"
  else
    fail registry_error "Registry $kind HTTP $HTTP_STATUS; not classified as missing"
  fi
}

verify_manifest() {
  local stem=$1 expected=$2 calculated
  CHECKED_DIGEST=$(header_digest "$stem.headers") || { fail evidence_error 'Unable to read manifest digest header'; return 1; }
  calculated=$(file_digest "$stem.body") || { fail digest_unconfirmed 'Unable to hash manifest'; return 1; }
  [[ "$CHECKED_DIGEST" =~ ^sha256:[0-9a-f]{64}$ && "$CHECKED_DIGEST" == "$calculated" ]] || { fail digest_mismatch 'Manifest bytes and Docker-Content-Digest do not agree'; return 1; }
  [[ -z "$expected" || "$CHECKED_DIGEST" == "$expected" ]] || { fail digest_mismatch 'Manifest digest differs from the requested descriptor'; return 1; }
  jq -e '.schemaVersion==2' "$stem.body" >/dev/null 2>&1 || { fail invalid_manifest 'Unsupported manifest schema'; return 1; }
  CHECKED_MEDIA_TYPE=$(jq -er '.mediaType' "$stem.body") || { fail invalid_manifest 'Missing manifest media type'; return 1; }
  case "$CHECKED_MEDIA_TYPE" in
    application/vnd.oci.image.index.v1+json|application/vnd.docker.distribution.manifest.list.v2+json|application/vnd.oci.image.manifest.v1+json|application/vnd.docker.distribution.manifest.v2+json) ;;
    *) fail invalid_manifest 'Unsupported manifest media type'; return 1 ;;
  esac
}

verify_config() {
  local stem=$1 evidence_dir=$2 requested=$3 descriptor=$4 actual fetched=false ext
  CONFIG_DIGEST=$(jq -er '.config.digest' "$stem.body" 2>/dev/null) || { fail invalid_manifest 'No image configuration descriptor'; return 1; }
  [[ "$CONFIG_DIGEST" =~ ^sha256:[0-9a-f]{64}$ ]] || { fail invalid_manifest 'Invalid configuration digest'; return 1; }
  if get_object "https://ghcr.io/v2/$REPOSITORY/blobs/$CONFIG_DIGEST" "$TMP/config" config; then fetched=true; fi
  if ! "$fetched" && [[ "$STATE" == evidence_error ]]; then return 1; fi
  for ext in headers request.json stderr; do
    cp "$TMP/config.$ext" "$evidence_dir/config.$ext" || { fail evidence_error 'Unable to retain configuration request evidence'; return 1; }
  done
  "$fetched" || return 1
  actual=$(file_digest "$TMP/config.body") || { fail digest_unconfirmed 'Unable to hash configuration'; return 1; }
  [[ "$actual" == "$CONFIG_DIGEST" ]] || { fail digest_mismatch 'Configuration bytes differ from manifest descriptor'; return 1; }
  # Do not persist image Env or unrelated labels: they may contain publisher secrets.
  jq -e 'select((.os|type)=="string" and (.architecture|type)=="string" and ((.variant // "")|type)=="string") | {os,architecture,variant:(.variant // ""),labels:((.config.Labels // {}) | with_entries(select(.key=="org.opencontainers.image.source" or .key=="org.opencontainers.image.revision" or .key=="org.opencontainers.image.version")))}' \
    "$TMP/config.body" > "$evidence_dir/config-summary.json" || { fail evidence_error 'Unable to write a valid configuration summary'; return 1; }
  CONFIG_PLATFORM=$(jq -er '.os+"/"+.architecture+(if .variant=="" then "" else "/"+.variant end)' "$evidence_dir/config-summary.json") || { fail invalid_config 'Configuration lacks OS/architecture'; return 1; }
  if [[ "$descriptor" != null ]]; then
    jq -e --argjson descriptor "$descriptor" '.os==$descriptor.os and .architecture==$descriptor.architecture and .variant==($descriptor.variant // "")' \
      "$evidence_dir/config-summary.json" >/dev/null || { fail platform_mismatch 'Child configuration does not match its index platform descriptor'; return 1; }
  fi
  [[ -z "$requested" || "$CONFIG_PLATFORM" == "$requested" || "$CONFIG_PLATFORM" == "$requested/"* ]] \
    || { fail platform_mismatch "Configuration does not satisfy required platform: $requested"; return 1; }
}

record_platform() {
  local requested=$1 manifest_digest=$2 children actual
  children=$(printf '%s' "$CHILD_MANIFESTS" | jq -c --arg requested "$requested" --arg platform "$CONFIG_PLATFORM" \
    --arg manifest "$manifest_digest" --arg config "$CONFIG_DIGEST" \
    '.+[{requested_platform:$requested,platform:$platform,manifest_digest:$manifest,config_digest:$config}]') \
    || { fail evidence_error 'Unable to record verified child manifest'; return 1; }
  actual=$(printf '%s' "$ACTUAL_PLATFORMS" | jq -c --arg platform "$CONFIG_PLATFORM" '.+[$platform] | unique') \
    || { fail evidence_error 'Unable to record verified platforms'; return 1; }
  CHILD_MANIFESTS=$children; ACTUAL_PLATFORMS=$actual
}

verify_one() {
  local ref=$1 selector expected='' platform pull_log candidates count descriptor child_digest child_dir descriptor_platform advertised
  REPOSITORY=${ref#ghcr.io/}
  if [[ "$REPOSITORY" == *@* ]]; then
    selector=${REPOSITORY##*@}; REPOSITORY=${REPOSITORY%@*}; expected=$selector
  else selector=${REPOSITORY##*:}; REPOSITORY=${REPOSITORY%:*}; fi
  get_token || return 1
  get_object "https://ghcr.io/v2/$REPOSITORY/manifests/$selector" "$DIR/manifest" manifest || return 1
  verify_manifest "$DIR/manifest" "$expected" || return 1
  DIGEST=$CHECKED_DIGEST
  if [[ "$CHECKED_MEDIA_TYPE" == *index* || "$CHECKED_MEDIA_TYPE" == *manifest.list* ]]; then
    advertised=$(jq -c '[.manifests[] | select(.annotations["vnd.docker.reference.type"] != "attestation-manifest") | .platform | select((.os|type)=="string" and (.architecture|type)=="string" and .os!="unknown" and .architecture!="unknown") | .os+"/"+.architecture+(if (.variant // "")=="" then "" else "/"+.variant end)] | unique' "$DIR/manifest.body") || { fail invalid_manifest 'Invalid index platforms'; return 1; }
    ADVERTISED_PLATFORMS=$advertised
    for platform in "${REQUIRED[@]}"; do
      candidates=$(jq -c --arg wanted "$platform" '[.manifests[] | select(.annotations["vnd.docker.reference.type"] != "attestation-manifest") | select(.platform.os!="unknown" and .platform.architecture!="unknown") | select((.platform.os+"/"+.platform.architecture+(if (.platform.variant // "")=="" then "" else "/"+.platform.variant end)) as $p | $p==$wanted or ($p|startswith($wanted+"/")))]' "$DIR/manifest.body") || { fail invalid_manifest 'Invalid child platform descriptors'; return 1; }
      count=$(printf '%s' "$candidates" | jq -er 'length') || { fail invalid_manifest 'Unable to select child platform'; return 1; }
      [[ "$count" != 0 ]] || { fail platform_missing "Required platform absent: $platform"; return 1; }
      [[ "$count" == 1 ]] || { fail platform_ambiguous "Multiple descriptors match $platform; request an exact variant"; return 1; }
      descriptor=$(printf '%s' "$candidates" | jq -ce '.[0]') || { fail invalid_manifest 'Invalid child descriptor'; return 1; }
      child_digest=$(printf '%s' "$descriptor" | jq -er '.digest') || { fail invalid_manifest 'Child descriptor lacks digest'; return 1; }
      [[ "$child_digest" =~ ^sha256:[0-9a-f]{64}$ ]] || { fail invalid_manifest 'Invalid child manifest digest'; return 1; }
      descriptor_platform=$(printf '%s' "$descriptor" | jq -ce '.platform') || { fail invalid_manifest 'Invalid child platform'; return 1; }
      child_dir="$DIR/platform-${platform//\//_}"
      mkdir "$child_dir" || { fail evidence_error 'Unable to create child evidence directory'; return 1; }
      get_object "https://ghcr.io/v2/$REPOSITORY/manifests/$child_digest" "$child_dir/manifest" child_manifest || return 1
      verify_manifest "$child_dir/manifest" "$child_digest" || return 1
      [[ "$CHECKED_MEDIA_TYPE" == application/vnd.oci.image.manifest.v1+json || "$CHECKED_MEDIA_TYPE" == application/vnd.docker.distribution.manifest.v2+json ]] \
        || { fail invalid_manifest 'Child descriptor must resolve to an image manifest'; return 1; }
      verify_config "$child_dir/manifest" "$child_dir" "$platform" "$descriptor_platform" || return 1
      record_platform "$platform" "$child_digest" || return 1
    done
  else
    verify_config "$DIR/manifest" "$DIR" '' null || return 1
    record_platform "$CONFIG_PLATFORM" "$DIGEST" || return 1
  fi
  for platform in "${REQUIRED[@]}"; do
    printf '%s' "$ACTUAL_PLATFORMS" | jq -e --arg wanted "$platform" 'any(.[]; .==$wanted or startswith($wanted+"/"))' >/dev/null || { fail platform_missing "Required platform absent: $platform"; return 1; }
  done
  METADATA_OK=true
  if "$PULL"; then
    for platform in "${REQUIRED[@]}"; do
      pull_log="$DIR/pull-${platform//\//_}.log"
      if ! DOCKER_AUTH_CONFIG= DOCKER_CONFIG="$TMP/docker" docker --config "$TMP/docker" pull --platform "$platform" \
          "ghcr.io/$REPOSITORY@$DIGEST" > "$pull_log" 2>&1; then
        fail pull_failed "Anonymous digest pull failed for $platform (see pull log)"; return 1
      fi
    done
    PULL_OK=true; STATE=verified_pull; DETAIL='Anonymous metadata and digest pull commands passed; daemon cache may be reused'
  else STATE=verified_metadata; DETAIL='Anonymous metadata passed; digest pull was not requested'; fi
}

FAILURES=0
for index in 0 1 2 3; do
  component=${COMPONENTS[$index]}; ref=${REFS[$index]}; DIR="$OUTPUT/$component"
  mkdir "$DIR"
  STARTED=$(date -u +%FT%TZ)
  STATE=unconfirmed DETAIL='' TOKEN='' TOKEN_HTTP='' HTTP_STATUS='' DIGEST='' REPOSITORY=''
  ACTUAL_PLATFORMS='[]' ADVERTISED_PLATFORMS='[]' CHILD_MANIFESTS='[]' METADATA_OK=false PULL_OK=false
  if verify_one "$ref"; then :; else FAILURES=$((FAILURES+1)); fi
  TOKEN=''
  jq -n --arg component "$component" --arg ref "$ref" --arg repository "ghcr.io/$REPOSITORY" \
    --arg started "$STARTED" --arg finished "$(date -u +%FT%TZ)" --arg status "$STATE" --arg detail "$DETAIL" \
    --arg digest "$DIGEST" --argjson platforms "$ACTUAL_PLATFORMS" --argjson advertised "$ADVERTISED_PLATFORMS" \
    --argjson children "$CHILD_MANIFESTS" --argjson required "$REQUIRED_JSON" \
    --argjson metadata_verified "$METADATA_OK" --argjson pull_requested "$PULL" --argjson pull_verified "$PULL_OK" \
    '{component:$component,ref:$ref,repository:$repository,started_utc:$started,finished_utc:$finished,anonymous:true,status:$status,detail:$detail,digest:($digest|if .=="" then null else . end),platforms:$platforms,advertised_platforms:$advertised,child_manifests:$children,required_platforms:$required,metadata_verified:$metadata_verified,pull_requested:$pull_requested,pull_verified:$pull_verified}' > "$DIR/result.json"
  printf '%s: %s — %s\n' "$component" "$STATE" "$DETAIL"
done
jq -s --argjson pull "$PULL" --argjson required "$REQUIRED_JSON" \
  '{schema_version:1,anonymous:true,pull_requested:$pull,required_platforms:$required,images:.,ok:all(.[]; .metadata_verified and (if $pull then .pull_verified else true end))}' \
  "$OUTPUT/backend/result.json" "$OUTPUT/admin/result.json" "$OUTPUT/widget/result.json" "$OUTPUT/pg/result.json" > "$OUTPUT/summary.json"
printf 'Evidence: %s/summary.json\n' "$OUTPUT"
((FAILURES == 0))
