"""HMAC-only schema roundtrip and refusal to fabricate legacy authorization data."""
import unittest

from test_workspace_internal_write import REPO, synthetic_cluster


UP = (REPO / 'priv/migrations/00000210_internal_hmac_only.up.sql').read_text()
DOWN = (REPO / 'priv/migrations/00000210_internal_hmac_only.down.sql').read_text()


def schema(sql):
    sql("CREATE TABLE enterprise_application(id bigint PRIMARY KEY, allowed_scopes jsonb);"
        "CREATE TABLE enterprise_application_credential(id bigint PRIMARY KEY, "
        "secret_digest text NOT NULL, signing_secret_enc bytea);"
        "CREATE TABLE enterprise_oa_login_ticket(id bigint PRIMARY KEY, "
        "issuer_authentication text NOT NULL DEFAULT 'bearer' "
        "CHECK(issuer_authentication IN ('bearer','hmac')));")
    sql('BEGIN;' + UP + 'COMMIT;')


class MigrationTest(unittest.TestCase):
    def test_empty_schema_roundtrip(self):
        with synthetic_cluster() as sql:
            schema(sql)
            sql('BEGIN;' + DOWN + 'COMMIT;')
            sql('BEGIN;' + UP + 'COMMIT;')
            self.assertEqual('0', sql("SELECT count(*) FROM information_schema.columns "
                                      "WHERE table_name='enterprise_application' AND column_name='allowed_scopes';"))

    def blocked(self, table, insert):
        with synthetic_cluster() as sql:
            schema(sql)
            sql(insert)
            before = sql(f'SELECT row_to_json(t) FROM {table} t;')
            sql('BEGIN;' + DOWN + 'COMMIT;', False)
            self.assertEqual(before, sql(f'SELECT row_to_json(t) FROM {table} t;'))
            self.assertEqual('0', sql("SELECT count(*) FROM information_schema.columns "
                                      "WHERE table_name='enterprise_application' AND column_name='allowed_scopes';"))

    def test_application_blocks_lossy_downgrade(self):
        self.blocked('enterprise_application', 'INSERT INTO enterprise_application VALUES(1);')

    def test_ticket_blocks_fabricated_authentication(self):
        self.blocked('enterprise_oa_login_ticket', 'INSERT INTO enterprise_oa_login_ticket VALUES(3);')

    def test_credential_blocks_fabricated_digest(self):
        self.blocked('enterprise_application_credential',
                     "INSERT INTO enterprise_application_credential VALUES(2,NULL,'synthetic-encrypted-key');")


if __name__ == '__main__':
    unittest.main()
