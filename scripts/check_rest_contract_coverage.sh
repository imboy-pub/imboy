#!/usr/bin/env bash
# REST contract coverage gate (RTF-04).
#
# Gate mode (default): every row in test/rest/contracts.tsv must have a
# real route, an OpenAPI operation, a spec, a suite and the exact Case IDs.
# Any miss => NO-GO with a non-zero exit.
#
# Inventory mode (--inventory FILE): generate the full route inventory as
# review evidence. Facts and their sources:
#   * route patterns / handlers ........... src/imboy_router.erl (get_routes/0
#     composition incl. feature-gated helper lists; plugin routes are runtime
#     registry entries and are therefore NOT statically enumerable).
#   * openapi_methods ..................... the referenced OpenAPI operation
#     file only. Methods are NEVER inferred from the path shape; when no
#     OpenAPI evidence exists the row is flagged UNKNOWN_REVIEW_REQUIRED.
#   * classified_auth ..................... a transcription of the auth
#     middleware's own registration facts:
#       - src/api/auth_middleware_api_v1.erl: open()/option() membership,
#         verify_sign on /api/v1/passport/* + /api/v1/ws + /api/v1/init +
#         /api/v1/refreshtoken (open+device-sign), IsPaymentCallback
#         (/api/v1/payment/callback/:gateway) and IsChannelWebhook
#         (/api/v1/webhook/channel/:token) -> callback-secret (credential in
#         URL, handler fail-closed), IsMcpPath (/api/v1/mcp) ->
#         mcp-credential, /api/adm/* -> admin-session (adm_auth_middleware).
#       - cs_http:is_credential_surface_path/1 +
#         imboy_route_shape:is_cs_widget_frame_path/1: frozen path shapes ->
#         cs-credential. These are transcribed verbatim below (single-segment
#         wildcards as "*"); cs_http.erl stays the source of truth.
#       - everything else -> jwt (default: JWT + device-sign gate).
#     Items that cannot be determined statically are marked
#     UNKNOWN_REVIEW_REQUIRED, never guessed.
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
INDEX="$ROOT/test/rest/contracts.tsv"
ROUTER="$ROOT/src/imboy_router.erl"
FAIL=0

# Router source with comment-only lines removed: a commented-out route tuple
# must NOT satisfy the route-presence check.
ROUTER_BODY=$(grep -vE '^[[:space:]]*%' "$ROUTER" || true)

# Body extraction helper: print every line from after the function head up
# to (exclusive) the next column-1 top-level construct (a -attribute or a
# function head). A plain "] ." range end does not work: open() closes with
# "] ++ test_open_routes()." and every inner list is indented, so the old
# extraction leaked to EOF (RTF-04 defect fix).
fn_body() {
  awk -v fn="$1" '$0 ~ "^[[:space:]]*" fn "\\(\\) ->" { f = 1; next } f && /^(-|[a-z_])/ { exit } f { print }' "$ROUTER"
}

open_list() {
  # Runtime open() = the literal list ++ test_open_routes() (the latter is
  # dev/test-only; included because the gate and the inventory run in a dev
  # context and the middleware consults imboy_router:open() verbatim).
  {
    fn_body open
    fn_body test_open_routes
  } | grep -oE '<<"[^"]+">>' | tr -d '<>"' | sort -u
}

option_list() {
  fn_body option | grep -oE '<<"[^"]+">>' | tr -d '<>"'
}

OPEN_CACHE=
OPTION_CACHE=

ensure_lists() {
  [[ -n "$OPEN_CACHE" ]] || OPEN_CACHE=$(open_list)
  [[ -n "$OPTION_CACHE" ]] || OPTION_CACHE=$(option_list)
}

in_open() {
  ensure_lists
  grep -qx "$1" <<<"$OPEN_CACHE"
}

in_option() {
  ensure_lists
  grep -qx "$1" <<<"$OPTION_CACHE"
}

# Extract (path, handler) pairs from the router. Handles both the common
# one-line form `{"/path", handler, ...` and the multi-line form where the
# handler atom starts on the next line, plus the root route `{"/", ...}`.
# Comment-only lines are skipped so commented tuples stay invisible.
route_tuples() {
  awk '
    /^[[:space:]]*%/ { next }
    match($0, /\{"\/[^"]*",/) {
      path = substr($0, RSTART + 2, RLENGTH - 4)
      rest = substr($0, RSTART + RLENGTH)
      if (match(rest, /^[[:space:]]+[a-z_][a-z0-9_@]*/)) {
        handler = substr(rest, RSTART, RLENGTH)
        gsub(/^[[:space:]]+/, "", handler)
        print path "\t" handler
        next
      }
      pending = path
      need = 1
      next
    }
    need {
      if ($0 ~ /^[[:space:]]*$/ || $0 ~ /^[[:space:]]*%/) next
      if (match($0, /^[[:space:]]*[a-z_][a-z0-9_@]*/)) {
        handler = substr($0, RSTART, RLENGTH)
        gsub(/^[[:space:]]+/, "", handler)
        print pending "\t" handler
        need = 0
      }
      next
    }
  ' "$ROUTER"
}

# --- cs-credential surface (verbatim transcription of the frozen shapes in
# --- cs_http:is_credential_surface_path/1 + imboy_route_shape) --------------
# segs[] holds the segments of the candidate path; match_shape compares them
# against a shape where "*" stands for one arbitrary (variable) segment.
match_shape() {
  local -a want=("$@")
  [[ ${#segs[@]} -eq ${#want[@]} ]] || return 1
  local i
  for i in "${!want[@]}"; do
    [[ ${want[i]} == "*" || ${want[i]} == "${segs[i]}" ]] || return 1
  done
  return 0
}

is_cs_credential_route() {
  local IFS='/'
  read -ra segs <<< "${1#/}"
  match_shape api v1 cs widget frame '*' && return 0
  match_shape api v1 cs sessions && return 0
  match_shape api v1 cs organizations '*' sessions queue && return 0
  match_shape api v1 cs sessions '*' messages && return 0
  match_shape api v1 cs sessions '*' rating && return 0
  match_shape api v1 cs widget bootstrap && return 0
  match_shape api v1 cs widget identity exchange && return 0
  match_shape api v1 cs widget sessions && return 0
  match_shape api v1 cs widget sessions '*' messages && return 0
  match_shape api v1 cs widget sessions '*' events && return 0
  match_shape api v1 cs widget sessions '*' rating && return 0
  match_shape api v1 cs widget sessions '*' assets '*' && return 0
  match_shape api v1 cs widget sessions '*' assets '*' content && return 0
  return 1
}

# auth_middleware_api_v1:is_single_segment_route/2: a registered route
# pattern like "/api/v1/payment/callback/:gateway" satisfies it because its
# variable segment contains no "/".
is_single_segment_suffix() {
  case "$1" in
    "$2"?*) {
      local rest="${1#"$2"}"
      [[ -n "$rest" && "$rest" != */* ]]
    } ;;
    *) return 1 ;;
  esac
}

# classified_auth per middleware facts (see file header). Order mirrors the
# middleware: credential surfaces first, then open/option membership.
classify_auth() {
  local path=$1
  if is_cs_credential_route "$path"; then
    echo "cs-credential"
  elif [[ "$path" == "/api/v1/mcp" ]]; then
    echo "mcp-credential"
  elif is_single_segment_suffix "$path" "/api/v1/payment/callback/" ||
    is_single_segment_suffix "$path" "/api/v1/webhook/channel/"; then
    echo "callback-secret"
  elif in_open "$path"; then
    case "$path" in
      /api/v1/passport/* | /api/v1/ws | /api/v1/init | /api/v1/refreshtoken)
        echo "open+device-sign"
        ;;
      *)
        echo "open"
        ;;
    esac
  elif in_option "$path"; then
    echo "optional-jwt"
  elif [[ "$path" == /api/adm/* ]]; then
    echo "admin-session"
  else
    echo "jwt"
  fi
}

inventory() {
  local out=$1
  ensure_lists
  {
    printf 'path\thandler\tin_open\tin_option\topenapi_methods\tclassified_auth\treview\n'
    route_tuples | sort -u |
      while IFS=$'\t' read -r path handler; do
        # OpenAPI evidence: resolve the $ref behind the path key in
        # api/openapi.yaml, then read the operation methods from the
        # referenced file. No path-shape guessing; guard the pipeline so a
        # route without an OpenAPI entry degrades to UNKNOWN_REVIEW_REQUIRED
        # instead of killing the run under pipefail. The $ref line is the one
        # AFTER the path key, and its './x' value is relative to api/.
        methods="UNKNOWN_REVIEW_REQUIRED"
        refline=$(grep -A1 -F "  ${path}:" "$ROOT/api/openapi.yaml" 2>/dev/null | sed -n '2p' || true)
        ref=$(printf '%s' "$refline" | sed -nE "s/.*'([^']+)'.*/\1/p" || true)
        if [[ -n "${ref:-}" ]]; then
          target="$ROOT/api/${ref#./}"
          if [[ -f "$target" ]]; then
            found=$(grep -oE '^(get|post|put|delete|patch|head|options):' "$target" | tr -d ':' | tr '\n' ',' | sed 's/,$//' || true)
            [[ -n "${found:-}" ]] && methods=$found
          fi
        fi
        auth=$(classify_auth "$path")
        review="OK"
        if [[ "$methods" == "UNKNOWN_REVIEW_REQUIRED" ]]; then
          review="UNKNOWN_REVIEW_REQUIRED"
        fi
        ino=$(in_open "$path" && echo yes || echo no)
        ino2=$(in_option "$path" && echo yes || echo no)
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$path" "$handler" "$ino" "$ino2" "$methods" "$auth" "$review"
      done
  } >"$out"
  local total review_required
  total=$(($(wc -l <"$out") - 1))
  review_required=$(awk -F'\t' 'NR > 1 && $7 != "OK"' "$out" | wc -l | tr -d ' ')
  echo "inventory: $total routes -> $out ($review_required UNKNOWN_REVIEW_REQUIRED)"
}

if [[ "${1:-}" == "--inventory" ]]; then
  [[ -n "${2:-}" ]] || {
    echo "usage: $0 --inventory OUTPUT_FILE" >&2
    exit 2
  }
  inventory "$2"
  exit 0
fi

while IFS=$'\t' read -r method path handler auth spec suite case_ids operation; do
  [[ -z "${method}" || "${method}" == \#* ]] && continue

  status=GO
  if ! grep -Fq "{\"${path}\", ${handler}," <<<"$ROUTER_BODY"; then
    echo "NO-GO ${method} ${path}: route/handler missing"
    status=NO-GO
  fi
  # Full AUTH reconciliation (reverdict 2026-09-23, finding 5): every row's
  # registered auth must equal the middleware-derived classification — not
  # only open* rows. A route drifting jwt->open (or any other direction), or
  # a mis-filled TSV auth column, is a NO-GO.
  computed_auth=$(classify_auth "${path}")
  if [[ "${computed_auth}" != "${auth}" ]]; then
    echo "NO-GO ${method} ${path}: auth drift (tsv=${auth} computed=${computed_auth})"
    status=NO-GO
  fi
  if ! grep -Fq "  ${path}:" "$ROOT/api/openapi.yaml"; then
    echo "NO-GO ${method} ${path}: OpenAPI path missing"
    status=NO-GO
  fi
  method_lower=$(printf '%s' "$method" | tr '[:upper:]' '[:lower:]')
  if [[ ! -f "$ROOT/${operation}" ]] || ! grep -Eq "^${method_lower}:" "$ROOT/${operation}"; then
    echo "NO-GO ${method} ${path}: OpenAPI operation missing"
    status=NO-GO
  fi
  if [[ ! -f "$ROOT/${spec}" || ! -f "$ROOT/${suite}" ]]; then
    echo "NO-GO ${method} ${path}: spec or suite missing"
    status=NO-GO
  else
    IFS=',' read -ra cases <<< "$case_ids"
    for case_id in "${cases[@]}"; do
      if ! grep -Fq "### ${case_id}" "$ROOT/${spec}" || ! grep -Fq "<<\"${case_id}\">>" "$ROOT/${suite}"; then
        echo "NO-GO ${method} ${path}: ${case_id} missing from spec or suite"
        status=NO-GO
      fi
    done
  fi

  if [[ "$status" == "GO" ]]; then
    echo "GO ${method} ${path} auth=${auth} cases=${case_ids}"
  else
    FAIL=1
  fi
done < "$INDEX"

exit "$FAIL"
