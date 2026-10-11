#!/usr/bin/env bash
# Behavioral regression: producer -> classifier -> nightly retry -> job outputs.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
export DIALYZER_BASELINE="$TMP/baseline"
printf 'old.erl|Old warning\n' > "$DIALYZER_BASELINE"
producer="$ROOT/scripts/check_dialyzer_baseline.sh"
classifier="$ROOT/.qa/classify.sh"
n=0
check() {
  local expected_rc="$1" pattern="$2"; shift 2
  local rc=0
  "$@" > "$TMP/output" 2>&1 || rc=$?
  if [ "$rc" != "$expected_rc" ] || ! grep -qE "$pattern" "$TMP/output"; then
    cat "$TMP/output"; echo "FAIL: expected rc=$expected_rc pattern=$pattern actual=$rc"; exit 1
  fi
  n=$((n+1)); echo "ok $n: $pattern (rc=$rc)"
}
raw() { printf 'Proceeding with analysis...\n%s\n done in 0m1s\n' "$1" > "$TMP/raw"; }
produce() { bash "$producer" "$TMP/raw" > "$TMP/gate" || [ "$?" = 1 ]; }
raw $'old.erl:1: Old warning\nnew.erl:2: New warning'; produce
cp "$TMP/gate" "$TMP/red"
check 1 'NEW=1 KNOWN=1 FIXED=0 .*VERDICT=NEW_FAILURE' bash "$classifier" dialyze "$TMP/gate" --gate-rc 2
check 1 'VERDICT=NEW_FAILURE' bash "$classifier" dialyze "$TMP/gate" --gate-rc 2 --gh-output "$TMP/direct-output"
check 0 '^1$' awk 'END {print NR}' "$TMP/direct-output"
raw 'old.erl:99: Old warning'; produce
cp "$TMP/gate" "$TMP/green"
check 0 'NEW=0 KNOWN=1 FIXED=0 .*VERDICT=PASS' bash "$classifier" dialyze "$TMP/gate" --known "$TMP/no-registry"
raw ''; produce
check 0 'NEW=0 KNOWN=0 FIXED=1 .*VERDICT=RATCHET' bash "$classifier" dialyze "$TMP/gate"
printf 'Proceeding with analysis...\nold.erl:1: Old warning\n' > "$TMP/raw"
check 3 'ERROR.*PARSE_ERROR' bash "$producer" "$TMP/raw"
cp "$TMP/output" "$TMP/gate"
check 3 'FIXED=0 .*VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/gate" --gate-rc 2
raw 'old.erl(99): Changed header'
check 3 'ERROR.*PARSE_ERROR' bash "$producer" "$TMP/raw"
sed '/DIALYZER_RESULT.*END/d' "$TMP/red" > "$TMP/truncated"
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/truncated" --gate-rc 2
sed $'s/RESULT\t1/RESULT\t2/g' "$TMP/red" > "$TMP/version"
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/version" --gate-rc 2
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/red" --gate-rc 0
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/green" --gate-rc 2
printf 'No space left on device\n' > "$TMP/raw"
check 3 'ERROR.*INFRA_FAILURE' bash "$producer" "$TMP/raw"
cp "$TMP/output" "$TMP/infra"
check 0 'FIXED=0 FLAKY=0 INFRA=1 VERDICT=INFRA_FAILURE' bash "$classifier" dialyze "$TMP/infra" --gate-rc 2
check 0 'VERDICT=INFRA_FAILURE' bash "$classifier" dialyze "$TMP/raw" --gate-rc 2
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/infra" --gate-rc 0
cat "$TMP/red" "$TMP/red" > "$TMP/duplicate"
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/duplicate" --gate-rc 2
sed '/DIALYZER_RESULT.*KNOWN/d' "$TMP/red" > "$TMP/missing"
check 3 'VERDICT=PARSE_ERROR' bash "$classifier" dialyze "$TMP/missing" --gate-rc 2
# Human previews truncate at 50; machine records must retain all fingerprints.
{ echo 'Proceeding with analysis...'; for i in $(seq 1 60); do echo "new$i.erl:1: New warning"; done; echo ' done in 0m1s'; } > "$TMP/raw"
produce
check 1 'NEW=60 KNOWN=0 FIXED=1 .*VERDICT=NEW_FAILURE' bash "$classifier" dialyze "$TMP/gate" --gate-rc 2
cp "$TMP/output" "$TMP/sixty"
check 0 'new60.erl' cat "$TMP/sixty"
# Two deterministic attempts, no Erlang/PG/global caches touched.
cat > "$TMP/attempt" <<'RUN'
#!/usr/bin/env bash
if [ ! -f "$STATE" ]; then touch "$STATE"; echo first-raw > dialyze-last.log; cat "$FIRST"; exit "$FIRST_RC"; fi
echo second-raw > dialyze-last.log
cat "$SECOND"; exit "$SECOND_RC"
RUN
export STATE="$TMP/state" FIRST="$TMP/red" FIRST_RC=2 SECOND_RC=0 SECOND="$TMP/green"
export GITHUB_OUTPUT="$TMP/github-output"
cd "$TMP"
nightly() { rm -f "$STATE" "$GITHUB_OUTPUT"; bash "$ROOT/.qa/nightly-gate.sh" dialyze "$TMP/nightly.log" -- bash "$TMP/attempt"; }
check 0 'NEW=0 KNOWN=1 FIXED=0 FLAKY=1 .*VERDICT=FLAKY' nightly
check 0 '^first-raw$' cat "$TMP/nightly.log.raw"
check 0 '^second-raw$' cat "$TMP/nightly.log.retry.raw"
SECOND_RC=2 SECOND="$TMP/red"
check 1 'NEW=1 KNOWN=1 FIXED=0 .*VERDICT=NEW_FAILURE' nightly
check 0 'new.erl\|New warning' cat "$GITHUB_OUTPUT"
SECOND_RC=0 SECOND="$TMP/truncated"
check 3 'VERDICT=PARSE_ERROR' nightly
FIRST="$TMP/truncated" SECOND="$TMP/green"
check 3 'VERDICT=PARSE_ERROR' nightly
FIRST="$TMP/infra" SECOND="$TMP/infra" SECOND_RC=2
check 2 'VERDICT=INFRA_FAILURE' nightly
FIRST="$TMP/infra" SECOND="$TMP/green" SECOND_RC=0
check 0 'FLAKY=0 INFRA=1 VERDICT=INFRA_FAILURE' nightly
# Non-Dialyzer classification still uses existing registry semantics.
printf 'new_module: test...*failed*\nFailed: 1\n' > "$TMP/eunit"
check 1 'VERDICT=NEW_FAILURE' bash "$classifier" full-eunit "$TMP/eunit" --gate-rc 1
echo "PASS: $n regression checks"
