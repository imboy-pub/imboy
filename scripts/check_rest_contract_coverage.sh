#!/usr/bin/env bash
# REST contract coverage gate (RTF-04).
#
# Gate mode (default): every row in test/rest/contracts.tsv must have a
# real route, an OpenAPI operation, a spec, a suite and the exact Case IDs.
# Any miss => NO-GO with a non-zero exit.
#
# Inventory mode (--inventory FILE): generate the full route inventory as
# review evidence. Methods are taken from the OpenAPI operation files; when
# no OpenAPI entry exists the method/auth classification is marked
# UNKNOWN_REVIEW_REQUIRED instead of being guessed (plan stop condition).
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
INDEX="$ROOT/test/rest/contracts.tsv"
FAIL=0

open_list() {
  awk '/^open\(\) ->/,/^\]\./' "$ROOT/src/imboy_router.erl" |
    grep -oE '<<"[^"]+">>' | tr -d '<>"'
}

option_list() {
  awk '/^option\(\) ->/,/^\]\./' "$ROOT/src/imboy_router.erl" |
    grep -oE '<<"[^"]+">>' | tr -d '<>"'
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

inventory() {
  local out=$1
  ensure_lists
  {
    echo -e "path\thandler\tin_open\tin_option\topenapi_methods\tclassified_auth\treview"
    grep -oE '\{"/[^"]+", *[a-z_][a-z0-9_@]*' "$ROOT/src/imboy_router.erl" |
      sed -E 's/^\{"([^"]+)", */\1\t/' |
      sort -u |
      while IFS=$'\t' read -r path handler; do
        methods="UNKNOWN_REVIEW_REQUIRED"
        ref=$(grep -A1 -F "  ${path}:" "$ROOT/api/openapi.yaml" 2>/dev/null |
          head -1 | sed -nE "s/.*'\.\/([^']+)'.*/\1/p")
        if [[ -n "${ref:-}" && -f "$ROOT/$ref" ]]; then
          found=$(grep -oE '^(get|post|put|delete|patch|head|options):' "$ROOT/$ref" | tr -d ':' | tr '\n' ',' | sed 's/,$//')
          [[ -n "$found" ]] && methods=$found
        fi
        review="OK"
        auth="jwt"
        if in_open "$path"; then
          auth="open"
          case "$path" in
            /api/v1/passport/*|/api/v1/ws|/api/v1/init|/api/v1/refreshtoken)
              auth="open+device-sign"
              ;;
          esac
        elif in_option "$path"; then
          auth="optional-jwt"
        elif [[ "$path" == /api/adm/* ]]; then
          auth="admin-session"
        fi
        if [[ "$methods" == "UNKNOWN_REVIEW_REQUIRED" ]]; then
          review="UNKNOWN_REVIEW_REQUIRED"
          auth="UNKNOWN_REVIEW_REQUIRED"
        fi
        echo -e "${path}\t${handler}\t$(in_open "$path" && echo yes || echo no)\t$(in_option "$path" && echo yes || echo no)\t${methods}\t${auth}\t${review}"
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
  if ! grep -Fq "{\"${path}\", ${handler}," "$ROOT/src/imboy_router.erl"; then
    echo "NO-GO ${method} ${path}: route/handler missing"
    status=NO-GO
  fi
  if [[ "${auth}" == open* ]] && ! in_open "${path}"; then
    echo "NO-GO ${method} ${path}: open auth registration missing"
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
