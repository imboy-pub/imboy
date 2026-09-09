#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/scripts/agent_hub_golden_flow.sh"
TEST_TMPDIR="${TEST_TMPDIR:?TEST_TMPDIR is required}"
STUB_BIN="$TEST_TMPDIR/bin"
mkdir -p "$STUB_BIN"

cat > "$STUB_BIN/psql" <<'EOF'
#!/usr/bin/env bash
touch "$PSQL_CALLED_MARKER"
exit 99
EOF
chmod +x "$STUB_BIN/psql"

grep -Fq 'PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up' "$SCRIPT" || {
  echo "strict migration driver does not target the scratch DB" >&2
  exit 1
}

awk '
  /^export IMBOY_PG_DATABASE="\$DB"$/ { bound = 1 }
  bound && /make -C "\$ROOT" eunit-local/ { eunit = 1 }
  END { exit !(bound && eunit) }
' "$SCRIPT" || {
  echo "eunit-local is not bound to the scratch DB" >&2
  exit 1
}

assert_rejected_before_psql() {
  name="$1"
  shift
  marker="$TEST_TMPDIR/$name.psql-called"
  evidence="$TEST_TMPDIR/$name-evidence"
  output="$TEST_TMPDIR/$name.log"
  set +e
  env -u PGHOSTADDR -u PGSERVICE -u PGSERVICEFILE \
    PATH="$STUB_BIN:$PATH" PSQL_CALLED_MARKER="$marker" \
    PGHOST=127.0.0.1 "$@" \
    bash "$SCRIPT" --evidence-dir "$evidence" > "$output" 2>&1
  code=$?
  set -e
  [ "$code" -eq 2 ] || {
    echo "$name did not fail closed with exit 2 (got $code)" >&2
    exit 1
  }
  [ ! -e "$marker" ] || {
    echo "$name reached psql" >&2
    exit 1
  }
  [ ! -e "$evidence" ] || {
    echo "$name created evidence before validation" >&2
    exit 1
  }
}

assert_rejected_before_psql remote-hostaddr PGHOSTADDR=192.0.2.10
assert_rejected_before_psql service PGSERVICE=remote-service
assert_rejected_before_psql service-file PGSERVICEFILE="$TEST_TMPDIR/pg_service.conf"
assert_rejected_before_psql localhost PGHOST=localhost

echo "golden flow DB isolation contract PASS"
