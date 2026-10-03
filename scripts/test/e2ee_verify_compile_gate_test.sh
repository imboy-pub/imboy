#!/usr/bin/env bash
# Offline regression of the actual Makefile recipe; no Erlang or PG is started.
set -euo pipefail
cd "$(dirname "$0")/../.."
fixture="$(mktemp -d /tmp/imboy-e2ee-compile-gate.XXXXXX)"
trap 'rm -rf -- "$fixture"' EXIT
mkdir -p "$fixture/bin" "$fixture/deps/mock/ebin" "$fixture/test" "$fixture/ebin"
awk '/^e2ee-verify:/ { active=1; print "e2ee-verify:"; next }
     /^\.PHONY: clear_beam/ { active=0 }
     active { print }' Makefile > "$fixture/Makefile"
touch "$fixture/test/e2ee_fixture.erl" "$fixture/test/stale_test.beam"
cat > "$fixture/bin/erlc" <<'STUB'
#!/usr/bin/env bash
count=0
[[ ! -f compile-count ]] || count="$(cat compile-count)"
count=$((count + 1))
printf '%s' "$count" > compile-count
if [[ "$GATE_MODE" == "first" && "$count" == 1 ]] ||
   [[ "$GATE_MODE" == "second" && "$count" == 2 ]]; then
  echo 'synthetic compiler failure'
  exit 42
fi
STUB
cat > "$fixture/bin/erl" <<'STUB'
#!/usr/bin/env bash
touch runtime-started
[[ "$GATE_MODE" != "runtime" ]] || exit 43
STUB
chmod +x "$fixture/bin/erlc" "$fixture/bin/erl"
passed=0
for mode in first second runtime success; do
  rm -f "$fixture/compile-count" "$fixture/runtime-started"
  rc=0
  (cd "$fixture" && PATH="$fixture/bin:$PATH" GATE_MODE="$mode" make e2ee-verify) > "$fixture/$mode.log" 2>&1 || rc=$?
  echo "CHECK mode=$mode rc=$rc runtime_started=$([[ -f "$fixture/runtime-started" ]] && echo yes || echo no)"
  if [[ "$mode" == "first" || "$mode" == "second" ]]; then
    [[ "$rc" != 0 && ! -f "$fixture/runtime-started" ]] || exit 1
    [[ "$(cat "$fixture/compile-count")" == "$([[ "$mode" == first ]] && echo 1 || echo 2)" ]] || exit 1
    [[ -f "$fixture/test/stale_test.beam" ]] || exit 1
  elif [[ "$mode" == runtime ]]; then
    [[ "$rc" != 0 && -f "$fixture/runtime-started" ]] || exit 1
  else
    [[ "$rc" == 0 && -f "$fixture/runtime-started" ]] || exit 1
  fi
  passed=$((passed + 1))
  echo "PASS $mode"
done
echo "RESULT PASS=$passed FAIL=0 (offline recipe control flow only)"
