#!/usr/bin/env bash
# Real backup_garage entry + G19 functions; all external resources are local doubles.
set -euo pipefail
ROOT="${G19_LAYOUT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)}"
TASK="$(mktemp -d)"
TASK="$(cd "$TASK" && pwd -P)"
completed=0
trap 'rc=$?; rm -rf "$TASK"; if [[ "$completed" != 1 && "$rc" == 0 ]]; then rc=99; fi; exit "$rc"' EXIT
mkdir -p "$TASK/repo/scripts/lib" "$TASK/repo/priv/migrations" "$TASK/bin" "$TASK/source/a" "$TASK/backups" "$TASK/remote"
cp "$ROOT/scripts/backup_garage.sh" "$TASK/repo/scripts/"
cp "$ROOT/scripts/lib/metrics_push.sh" "$TASK/repo/scripts/lib/"
printf 'nested synthetic bytes\n' > "$TASK/source/a/b.bin"
: > "$TASK/source/empty.bin"
: > "$TASK/repo/priv/migrations/00000225_fixture.up.sql"
cat > "$TASK/repo/scripts/backup_pg.sh" <<'EOF'
#!/usr/bin/env bash
set -eu
printf 'MOCK_BACKUP_CALLED\n' >> "$BACKUP_CALLS"
printf 'synthetic dump\n' > "$BACKUP_DIR/${POSTGRES_DB}_fixture.dump"
EOF
cat > "$TASK/repo/scripts/restore_pg.sh" <<'EOF'
#!/usr/bin/env bash
set -eu
printf 'MOCK_RESTORE_ONLY\n'
EOF
cat > "$TASK/bin/rclone" <<'PY'
#!/usr/bin/env python3
import json,os,sys,shutil
from pathlib import Path
args=sys.argv[1:];cmd=args[0]
def path(value):
 if ':' not in value:return Path(value)
 remote,key=value.split(':',1)
 if remote!='owned':raise SystemExit('foreign remote refused')
 return Path(os.environ['FAKE_OBJECT_ROOT'])/key
if cmd=='listremotes':print('owned:')
elif cmd=='copy':
 src,dst=path(args[1]),path(args[2]);dst.mkdir(parents=True,exist_ok=True)
 for item in src.iterdir():
  target=dst/item.name
  if item.is_dir():shutil.copytree(item,target,dirs_exist_ok=True)
  else:shutil.copyfile(item,target)
elif cmd=='size':
 root=path(args[1]);files=[p for p in root.rglob('*') if p.is_file()]
 print(json.dumps({'count':len(files),'bytes':sum(p.stat().st_size for p in files)}))
elif cmd=='cat':sys.stdout.buffer.write(path(args[1]).read_bytes())
elif cmd=='lsf':
 root=path(args[1]);print('\n'.join(sorted(str(p.relative_to(root)) for p in root.rglob('*') if p.is_file())))
else:raise SystemExit('unexpected fake rclone command')
PY
for name in docker curl; do
  printf '#!/bin/sh\necho "REAL_RESOURCE_COMMAND_REFUSED" >&2\nexit 91\n' > "$TASK/bin/$name"
done
chmod +x "$TASK/bin/"*
mkdir -p "$TASK/remote/synthetic-bucket"
cp -R "$TASK/source/." "$TASK/remote/synthetic-bucket/"
export PATH="$TASK/bin:$PATH" FAKE_OBJECT_ROOT="$TASK/remote" BACKUP_CALLS="$TASK/backup-calls.log"
export IMBOY_ROOT="$TASK/repo" DRILL_BACKUP_DIR="$TASK/backups" DRILL_BUCKET=synthetic-bucket GARAGE_REMOTE=owned
export DRILL_TARGET_DB=p1drill_synthetic DRILL_RESTORE_DB=p1drill_synthetic_restore
export PUSHGATEWAY_URL='' METRICS_PUSH_STRICT=0
# The sourceable guard prevents seed/drop/main. SQL below is an explicit local double.
source "$ROOT/scripts/test/phase1_gates/g19_drill_backup_restore_oracle.sh"
psql_exec() {
  case "$2" in
    *'SELECT chat_type'*) printf 'c2c|synthetic-conv|synthetic-message\n' ;;
    *'SELECT path'*)
      printf 'a/b.bin|%s\n' "$(shasum -a 256 "$TASK/source/a/b.bin" | awk '{print $1}')"
      printf 'empty.bin|%s\n' "$(shasum -a 256 "$TASK/source/empty.bin" | awk '{print $1}')" ;;
    *'SELECT version'*) printf '225\n' ;;
    *'pg_terminate_backend'*|*'DROP DATABASE'*) printf 'MOCK_SQL_NO_DATABASE\n' >> "$TASK/mock-sql.log" ;;
    *) echo 'UNEXPECTED_SQL_DOUBLE' >&2; return 92 ;;
  esac
}
step2_backup
step3_drop_and_restore
# Old code restores bucket_timestamp/a/b.bin and must fail these exact assertions.
[[ -f "$TASK/remote/p1drill-restore/a/b.bin" && -f "$TASK/remote/p1drill-restore/empty.bin" ]] || {
  echo 'FAIL: restored object keys gained snapshot-directory prefix' >&2; exit 1;
}
cmp "$TASK/source/a/b.bin" "$TASK/remote/p1drill-restore/a/b.bin"
cmp "$TASK/source/empty.bin" "$TASK/remote/p1drill-restore/empty.bin"
step4_oracle
printf 'PASS: exact nested and zero-byte keys via real backup entry\n'
snapshot="$(g19_snapshot_dir)"
expect_reject() { if "$@"; then echo 'FAIL: malformed snapshot accepted' >&2; exit 1; fi; }
mkdir "$DRILL_BACKUP_DIR/garage/${DRILL_BUCKET}_extra"
expect_reject g19_snapshot_dir
rmdir "$DRILL_BACKUP_DIR/garage/${DRILL_BUCKET}_extra"
printf 'PASS: multiple snapshots rejected\n'
mv "$snapshot" "$TASK/held-snapshot"
expect_reject g19_snapshot_dir
ln -s "$TASK/held-snapshot" "$snapshot"
expect_reject g19_snapshot_dir
rm "$snapshot"
mv "$TASK/held-snapshot" "$snapshot"
printf 'PASS: empty parent and symlink rejected\n'
printf '%s\n' "$TASK/escape" > "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path"
expect_reject restore_garage_snapshot
printf '%s\n' "$snapshot" > "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path"
printf 'PASS: pinned snapshot escape rejected\n'
printf 'corrupt bytes\n' > "$TASK/remote/p1drill-restore/a/b.bin"
expect_reject step4_oracle
cp "$TASK/source/a/b.bin" "$TASK/remote/p1drill-restore/a/b.bin"
printf 'PASS: same-key wrong bytes rejected\n'
printf 'extra\n' > "$TASK/remote/p1drill-restore/extra.bin"
expect_reject step4_oracle
rm "$TASK/remote/p1drill-restore/extra.bin" "$TASK/remote/p1drill-restore/empty.bin"
expect_reject step4_oracle
printf 'PASS: extra and missing object keys rejected\n'

# Relative paths remain supported; source G19 changed cwd to the synthetic repo.
for relative in ./relative-backups ./existing/../parent-backups; do
  mkdir -p "$TASK/repo/existing" "$TASK/repo/${relative%/*}" "$relative"
  DRILL_BACKUP_DIR="$relative"
  step2_backup
  [[ "$DRILL_BACKUP_DIR" == /* ]]
  [[ -f "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path" ]]
done
printf 'PASS: relative and legitimate parent traversal preserve backup behavior\n'
# A link hidden behind '..' must also fail before invoking backup_pg/backup_garage.
ln -s "$TASK/repo/existing" "$TASK/repo/link-ancestor"
called_before="$(wc -l < "$BACKUP_CALLS")"
DRILL_BACKUP_DIR=./link-ancestor/../forbidden-backups
expect_reject step2_backup
called_after="$(wc -l < "$BACKUP_CALLS")"
[[ "$called_before" == "$called_after" && ! -e "$TASK/repo/forbidden-backups" ]]
printf 'PASS: symlink ancestor rejected before either backup entry\n'
printf 'COMPONENT_ONLY: no real PG, object service, network or installation acceptance\n'
completed=1
