#!/usr/bin/env bash
# Tool-boundary regression; this is not an actual Erlang Xref execution.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir "$work/bin"
cat > "$work/bin/make" <<'STUB'
#!/usr/bin/env bash
[[ "$*" == xref ]] || exit 90
case "$XREF_FIXTURE" in
  success) echo 'XREF completed'; exit 0 ;;
  tool_failure) echo 'compiler failed before Xref'; exit 2 ;;
  undefined) echo 'Undefined function missing:call/0'; exit 0 ;;
  both) echo 'Undefined function missing:call/0'; exit 2 ;;
  empty) exit 0 ;;
  *) exit 91 ;;
esac
STUB
chmod +x "$work/bin/make"
check() {
  local fixture=$1 expected=$2 log=$3 rc=0
  PATH="$work/bin:$PATH" XREF_FIXTURE="$fixture" \
    bash "$root/scripts/ci/xref-gate.sh" "$log" > "$work/output" 2>&1 || rc=$?
  if (( rc != expected )); then
    cat "$work/output" >&2
    echo "ERROR: $fixture expected $expected, got $rc" >&2
    exit 1
  fi
}
check success 0 "$work/xref.log"
check tool_failure 2 "$work/xref.log"
check undefined 1 "$work/xref.log"
check both 2 "$work/xref.log"
check empty 1 "$work/xref.log"
# A successful make must not hide an evidence-write failure.
check success 1 "$work/nonexistent/xref.log"
echo 'xref gate shell regressions: 6 passed'
