"""Channel governance versions and refusal to discard grants or version history."""
import pathlib
import unittest
from test_workspace_internal_write import synthetic_cluster

REPO = pathlib.Path(__file__).resolve().parents[2]


class MigrationTest(unittest.TestCase):
    def test_roundtrip_and_preservation(self):
        with synthetic_cluster() as sql:
            sql("CREATE TABLE enterprise_application_grant_scope(scope text "
                "CONSTRAINT ck_eags_scope_fixed CHECK(scope='channels:read'));"
                "CREATE TABLE enterprise_application(allowed_scopes jsonb);"
                "CREATE TABLE channel(id bigint PRIMARY KEY,name text,description text,avatar text,"
                "custom_id text,tags jsonb,visibility int,access_type int,join_policy int,status int,"
                "scope text,workspace_id bigint,creator_uid bigint,is_verified bool,subscriber_count bigint);")
            up = (REPO / 'priv/migrations/00000161_channel_internal_write.up.sql').read_text()
            down = (REPO / 'priv/migrations/00000161_channel_internal_write.down.sql').read_text()
            sql('BEGIN;' + up + 'COMMIT;')
            sql("INSERT INTO channel(id,name,subscriber_count) VALUES(1,'synthetic',0);")
            self.assertEqual(sql('SELECT version FROM channel;'), '1')
            sql("UPDATE channel SET subscriber_count=1,version=999 WHERE id=1;")
            self.assertEqual(sql('SELECT version FROM channel;'), '1')
            sql("INSERT INTO enterprise_application_grant_scope VALUES('*');", False)
            for statement, table in [
                ("INSERT INTO enterprise_application_grant_scope VALUES('channels:write');",
                 'enterprise_application_grant_scope'),
                ("INSERT INTO enterprise_application VALUES('[\"channels:write\"]');",
                 'enterprise_application')]:
                sql(statement)
                before = sql(f'SELECT row_to_json(t) FROM {table} t;')
                sql('BEGIN;' + down + 'COMMIT;', False)
                self.assertEqual(sql(f'SELECT row_to_json(t) FROM {table} t;'), before)
                sql(f'DELETE FROM {table};')  # Synthetic fixture only.
            sql('BEGIN;' + down + 'COMMIT;')
            self.assertEqual(sql('SELECT id,name FROM channel;'), '1|synthetic')
            sql('BEGIN;' + up + 'COMMIT;')
            sql("UPDATE channel SET avatar='changed',version=1 WHERE id=1;")
            self.assertEqual(sql('SELECT version FROM channel;'), '2')
            sql("UPDATE channel SET version=1 WHERE id=1;")
            self.assertEqual(sql('SELECT version FROM channel;'), '2')
            before = sql('SELECT row_to_json(c) FROM channel c;')
            sql('BEGIN;' + down + 'COMMIT;', False)
            self.assertEqual(sql('SELECT row_to_json(c) FROM channel c;'), before)


if __name__ == '__main__':
    unittest.main()
