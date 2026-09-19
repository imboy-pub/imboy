#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
INDEX="$ROOT/test/rest/contracts.tsv"
FAIL=0

while IFS=$'\t' read -r method path handler auth spec suite case_ids operation; do
  [[ -z "${method}" || "${method}" == \#* ]] && continue

  status=GO
  if ! grep -Fq "{\"${path}\", ${handler}," "$ROOT/src/imboy_router.erl"; then
    echo "NO-GO ${method} ${path}: route/handler missing"
    status=NO-GO
  fi
  if [[ "${auth}" == "open" ]] && ! grep -Fq "<<\"${path}\">>" "$ROOT/src/imboy_router.erl"; then
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
