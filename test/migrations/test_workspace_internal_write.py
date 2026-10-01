"""Workspace write migration roundtrip and refusal without deleting new evidence."""
import contextlib
import pathlib
import subprocess
import tempfile
import unittest

REPO = pathlib.Path(__file__).resolve().parents[2]
PG = pathlib.Path('/opt/homebrew/opt/postgresql@18/bin')


@contextlib.contextmanager
def synthetic_cluster():
    with tempfile.TemporaryDirectory(prefix='gz-workspace-write-') as directory:
        root = pathlib.Path(directory)
        socket = root / 'socket'
        socket.mkdir()
        data = root / 'data'
        subprocess.run([str(PG / 'initdb'), '-D', str(data), '-U', 'synthetic',
                        '-A', 'trust', '--no-locale', '-E', 'UTF8'],
                       check=True, stdout=subprocess.DEVNULL)
        subprocess.run([str(PG / 'pg_ctl'), '-D', str(data), '-l', str(root / 'pg.log'),
                        '-o', f"-k {socket} -p 62660 -c listen_addresses=''", '-w', 'start'],
                       check=True, stdout=subprocess.DEVNULL)
        try:
            def sql(text, success=True):
                result = subprocess.run([str(PG / 'psql'), '-X', '-h', str(socket),
                                         '-p', '62660', '-U', 'synthetic', '-d', 'postgres',
                                         '-v', 'ON_ERROR_STOP=1', '-At'],
                                        input=text, capture_output=True, text=True)
                assert (result.returncode == 0) == success, result.stderr
                return result.stdout.strip()
            yield sql
        finally:
            subprocess.run([str(PG / 'pg_ctl'), '-D', str(data), '-m', 'fast', '-w', 'stop'],
                           check=True, stdout=subprocess.DEVNULL)


class MigrationTest(unittest.TestCase):
    def test_roundtrip_and_preservation(self):
        with synthetic_cluster() as sql:
            sql("CREATE TABLE enterprise_application_grant_scope(scope text "
                "CONSTRAINT ck_eags_scope_fixed CHECK(scope='workspaces:read'));"
                "CREATE TABLE enterprise_application(allowed_scopes jsonb);"
                "CREATE TABLE workspace(id bigint PRIMARY KEY,name text);")
            up = (REPO / 'priv/migrations/00000160_workspace_internal_write.up.sql').read_text()
            down = (REPO / 'priv/migrations/00000160_workspace_internal_write.down.sql').read_text()
            sql('BEGIN;' + up + 'COMMIT;')
            sql("INSERT INTO workspace(id,name) VALUES(1,'synthetic');")
            self.assertEqual(sql('SELECT version FROM workspace;'), '1')
            sql("INSERT INTO enterprise_application_grant_scope VALUES('*');", False)
            for statement, table in [
                ("INSERT INTO enterprise_application_grant_scope VALUES('workspaces:write');",
                 'enterprise_application_grant_scope'),
                ("INSERT INTO enterprise_application VALUES('[\"workspaces:write\"]');",
                 'enterprise_application')]:
                sql(statement)
                before = sql(f'SELECT row_to_json(t) FROM {table} t;')
                sql('BEGIN;' + down + 'COMMIT;', False)
                self.assertEqual(sql(f'SELECT row_to_json(t) FROM {table} t;'), before)
                sql(f'DELETE FROM {table};')  # Synthetic fixture only.
            sql('BEGIN;' + down + 'COMMIT;')
            self.assertEqual(sql('SELECT id,name FROM workspace;'), '1|synthetic')
            sql('BEGIN;' + up + 'COMMIT;')
            sql("UPDATE workspace SET name='updated',version=1 WHERE id=1;")
            self.assertEqual(sql('SELECT version FROM workspace;'), '2')
            before = sql('SELECT row_to_json(w) FROM workspace w;')
            sql('BEGIN;' + down + 'COMMIT;', False)
            self.assertEqual(sql('SELECT row_to_json(w) FROM workspace w;'), before)


if __name__ == '__main__':
    unittest.main()
