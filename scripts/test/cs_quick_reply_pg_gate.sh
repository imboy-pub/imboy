#!/usr/bin/env bash
# Run the existing CRUD suite on an owned native PostgreSQL 18 cluster.
# This is the quick-reply business schema, not a full application migration gate.
set -euo pipefail
[[ $(id -u) -ne 0 ]] || { echo 'Native PostgreSQL requires a non-root user' >&2; exit 2; }
root=$(git rev-parse --show-toplevel)
cd "$root"
pg_bin=$(pg_config --bindir)
version=$("$pg_bin/postgres" --version)
[[ "$version" =~ PostgreSQL\)\ 18\. ]] || { echo "PostgreSQL 18 required: $version" >&2; exit 2; }

umask 077
work=$(mktemp -d /tmp/imboy-cs-quick-reply.XXXXXXXX)
pgdata="$work/data"
cleanup() {
  local rc=$?
  trap - EXIT
  if [[ -f "$pgdata/postmaster.pid" ]]; then
    if ! "$pg_bin/pg_ctl" -D "$pgdata" -m fast -w stop; then
      echo "Failed to stop owned PostgreSQL; retained $work" >&2
      exit 1
    fi
  fi
  if [[ $rc -ne 0 && -f "$work/postgres.log" ]]; then
    cat "$work/postgres.log" >&2
  fi
  rm -rf -- "$work"
  exit "$rc"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# Never inherit a developer's database or service selection.
unset PGSERVICE PGSERVICEFILE PGHOSTADDR PGOPTIONS
export PGHOST=127.0.0.1 PGUSER=imboy_contract PGDATABASE=cs_quick_reply_contract PGSSLMODE=disable
PGPORT=$(python3 - <<'PY'
import socket
with socket.socket() as sock:
    sock.bind(('127.0.0.1', 0))
    print(sock.getsockname()[1])
PY
)
PGPASSWORD=$(python3 -c 'import secrets; print(secrets.token_hex(24))')
export PGPORT PGPASSWORD
printf '%s\n' "$PGPASSWORD" > "$work/password"
"$pg_bin/initdb" -D "$pgdata" --username="$PGUSER" --pwfile="$work/password" \
  --auth-local=trust --auth-host=scram-sha-256 --encoding=UTF8 --locale=C
mkdir "$work/socket"
cat >> "$pgdata/postgresql.conf" <<EOF
listen_addresses = '127.0.0.1'
port = $PGPORT
unix_socket_directories = '$work/socket'
max_connections = 12
EOF
"$pg_bin/pg_ctl" -D "$pgdata" -l "$work/postgres.log" -w start
"$pg_bin/createdb" "$PGDATABASE"
psql=("$pg_bin/psql" -X --no-password --set=ON_ERROR_STOP=1)
[[ $("${psql[@]}" -Atc 'SHOW data_directory') == "$pgdata" ]] || {
  echo 'Connected database is not the cluster created by this gate' >&2; exit 2;
}
"${psql[@]}" -c "COMMENT ON DATABASE cs_quick_reply_contract IS 'imboy disposable quick-reply contract gate'"
printf 'Candidate: %s\n%s\n' "$(git rev-parse HEAD)" "$version"
"${psql[@]}" -c 'SELECT current_database(), system_identifier FROM pg_control_system()'
echo 'Schema scope: foundation user DDL plus migrations 76, 95, 113 and 240'
sha256sum priv/migrations/00000001_foundation.up.sql

# Keep the real user columns, constraints and indexes needed by the FK migrations.
# The rest of foundation requires extensions unrelated to quick replies.
python3 - "$work/user.sql" <<'PY'
from pathlib import Path
import re
import sys

source = Path('priv/migrations/00000001_foundation.up.sql').read_text()
patterns = [
    r'CREATE TABLE public\."user" \(\n.*?\n\);',
    r'ALTER TABLE ONLY public\."user"\s+ADD CONSTRAINT user_pkey PRIMARY KEY \(id\);',
    *(rf'CREATE UNIQUE INDEX {name} ON public\."user"[^;]*;'
      for name in ('uk_account', 'uk_email', 'uk_mobile')),
]
statements = []
for pattern in patterns:
    matches = re.findall(pattern, source, re.DOTALL)
    if len(matches) != 1:
        raise SystemExit('Expected exactly one foundation user DDL statement: ' + pattern)
    statements.extend(matches)
Path(sys.argv[1]).write_text('\n\n'.join(statements) + '\n')
PY
"${psql[@]}" -f "$work/user.sql"
for migration in \
  00000076_workspace_foundation \
  00000095_organization_foundation \
  00000113_organization_member \
  00000240_cs_quick_reply; do
  file="priv/migrations/$migration.up.sql"
  sha256sum "$file"
  "${psql[@]}" -f "$file"
done

cat > "$work/owned.config" <<EOF
[{imboy, [
  {env, test}, {sql_driver, pgsql},
  {pg_conf, #{name => pgsql, init_count => 1, max_count => 2,
    start_mfa => {epgsql, connect, [#{host => "$PGHOST", port => $PGPORT,
      username => "$PGUSER", password => "$PGPASSWORD", database => "$PGDATABASE",
      ssl => false, timeout => 4000, codecs => [{epgsql_codec_rfc3339_bin, []}]}]}}}
]}].
EOF
IMBOYENV=local IMBOY_TEST_CONFIG="$work/owned.config" \
  make eunit-local t=cs_quick_reply_tests EUNIT_CONFIG="$work/owned" \
    EUNIT_USE_RELAY=0 EUNIT_RELAY_TARGET="$PGPORT" EUNIT_OPTS=verbose 2>&1 \
  | python3 -u -c '
import os
import sys
for line in sys.stdin:
    sys.stdout.write(line.replace(os.environ["PGPASSWORD"], "[REDACTED_TEST_PASSWORD]"))
' | tee "$work/eunit.log"

# The peer must report a real nonzero pass, with no cancelled/skipped cases.
python3 - "$work/eunit.log" <<'PY'
from pathlib import Path
import re
import sys

log = Path(sys.argv[1]).read_text()
reports = re.findall(
    r'\[isolated_pg_fixture\] cs_quick_reply_tests:.* result=ok counts=(\[.*\])', log
)
if len(reports) != 1:
    raise SystemExit('Missing unique successful quick-reply peer report')
counts = dict((name, int(value)) for name, value in re.findall(r'\{(\w+),(\d+)\}', reports[0]))
if counts.get('pass', 0) < 1 or any(counts.get(name, -1) != 0 for name in ('fail', 'skip', 'cancel')):
    raise SystemExit('Quick-reply suite did not fully execute: ' + str(counts))
print('QUICK_REPLY_PEER_COUNTS=' + str(counts))
PY
remaining=$("${psql[@]}" -Atc '
  SELECT (SELECT count(*) FROM cs_quick_reply)
       + (SELECT count(*) FROM organization_member)
       + (SELECT count(*) FROM workspace)
       + (SELECT count(*) FROM organization)
       + (SELECT count(*) FROM "user")')
[[ "$remaining" == 0 ]] || { echo "Fixture rows remaining: $remaining" >&2; exit 1; }
echo 'QUICK_REPLY_NATIVE_PG_PASS fixture_rows_remaining=0'
