"""The TG subset follows the renumbered production migration, not unrelated CS DDL."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('verify_textguard_business', ROOT / 'scripts/verify_textguard_business.py')
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class TextGuardBusinessSchemaTests(unittest.TestCase):
    def test_subset_uses_current_textguard_migration(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            schema = MODULE.schema_subset(ROOT, output, lambda p, value: p.write_text(json.dumps(value)))
            ddl = schema.read_text()
            proof = json.loads((output / 'business-schema-subset-proof.json').read_text())
            self.assertIn('00000197_textguard_review_metadata.up.sql', proof['production_migrations_sha256'])
            self.assertNotIn('00000195_cs_first_reply_facts.up.sql', proof['production_migrations_sha256'])
            self.assertIn('CREATE OR REPLACE FUNCTION public.textguard_profile_version()', ddl)
            self.assertNotIn('ALTER TABLE customer_service_session', ddl)
            self.assertFalse(proof['full_migration_chain_tested'])


if __name__ == '__main__':
    unittest.main()
