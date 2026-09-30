"""Run migration 159 against an isolated synthetic PostgreSQL cluster."""
import pathlib
import subprocess
import tempfile
import unittest

REPO = pathlib.Path(__file__).resolve().parents[2]
PG = pathlib.Path('/opt/homebrew/opt/postgresql@18/bin')


class MigrationTest(unittest.TestCase):
    def test_roundtrip_and_preservation(self):
        with tempfile.TemporaryDirectory(prefix='gz-seat-api-migration-') as root:
            root = pathlib.Path(root)
            socket = root / 'socket'
            socket.mkdir()
            data = root / 'data'
            subprocess.run([str(PG / 'initdb'), '-D', str(data), '-U', 'synthetic',
                            '-A', 'trust', '--no-locale', '-E', 'UTF8'],
                           check=True, stdout=subprocess.DEVNULL)
            subprocess.run([str(PG / 'pg_ctl'), '-D', str(data), '-l', str(root / 'pg.log'),
                            '-o', f"-k {socket} -p 62659 -c listen_addresses=''", '-w', 'start'],
                           check=True, stdout=subprocess.DEVNULL)
            try:
                def sql(text, success=True):
                    result = subprocess.run([str(PG / 'psql'), '-X', '-h', str(socket),
                                             '-p', '62659', '-U', 'synthetic', '-d', 'postgres',
                                             '-v', 'ON_ERROR_STOP=1', '-At'], input=text,
                                            capture_output=True, text=True)
                    self.assertEqual(result.returncode == 0, success, result.stderr)
                    return result.stdout.strip()

                sql('CREATE TABLE enterprise_application_grant_scope(scope text);'
                    'CREATE TABLE enterprise_application(allowed_scopes jsonb);'
                    'CREATE TABLE enterprise_application_usage(metric text);'
                    'CREATE TABLE customer_service_event(actor_kind text);')
                up = (REPO / 'priv/migrations/00000159_customer_service_internal_api.up.sql').read_text()
                down = (REPO / 'priv/migrations/00000159_customer_service_internal_api.down.sql').read_text()
                sql('BEGIN;'+up+'COMMIT;')
                sql('BEGIN;'+up+'COMMIT;')
                sql("INSERT INTO enterprise_application_grant_scope VALUES ('*');", False)
                sql("INSERT INTO enterprise_application_usage VALUES ('arbitrary');", False)
                for table, column, value in [
                    ('enterprise_application_grant_scope', 'scope', "'customer_service:read'"),
                    ('enterprise_application_grant_scope', 'scope', "'customer_service:write'"),
                    ('enterprise_application', 'allowed_scopes', "'[\"customer_service:read\"]'"),
                    ('enterprise_application', 'allowed_scopes', "'[\"customer_service:write\"]'"),
                    ('enterprise_application_usage', 'metric', "'seat.read'"),
                ]:
                    sql(f'INSERT INTO {table}({column}) VALUES ({value});')
                    before = sql(f'SELECT row_to_json(t) FROM {table} t;')
                    sql('BEGIN;'+down+'COMMIT;', False)
                    self.assertEqual(sql(f'SELECT row_to_json(t) FROM {table} t;'), before)
                    sql(f'DELETE FROM {table};')  # Owned synthetic fixture only.
                sql('BEGIN;'+down+'COMMIT;')
                sql("INSERT INTO enterprise_application_grant_scope VALUES ('customer_service:read');", False)
                sql("INSERT INTO enterprise_application_usage VALUES ('seat.read');", False)
                sql("INSERT INTO enterprise_application_grant_scope VALUES ('groups:read');")
                sql("INSERT INTO enterprise_application_usage VALUES ('directory.page');")
                sql('BEGIN;'+up+'COMMIT;')
                self.assertEqual(sql('SELECT scope FROM enterprise_application_grant_scope;'), 'groups:read')
                self.assertEqual(sql('SELECT metric FROM enterprise_application_usage;'), 'directory.page')
            finally:
                subprocess.run([str(PG / 'pg_ctl'), '-D', str(data), '-m', 'fast', '-w', 'stop'],
                               check=True, stdout=subprocess.DEVNULL)


if __name__ == '__main__':
    unittest.main()
