#!/usr/bin/env python3
"""The 204 rollback must restore the complete pre-204 function, not redeclare it."""
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2]


class RegisterRollbackTests(unittest.TestCase):
    def test_restores_the_frozen_201_body_in_place(self):
        migrations = ROOT / 'priv/migrations'
        before = (migrations / '00000201_attachment_direct_v1_sessions.up.sql').read_text()
        rollback = (migrations / '00000204_attachment_direct_v1_register_race.down.sql').read_text()
        expression = r'CREATE (?:OR REPLACE )?FUNCTION fn_dv1_register\(.*?\$\$;'
        expected = re.search(expression, before, re.S).group().replace(
            'CREATE FUNCTION', 'CREATE OR REPLACE FUNCTION', 1)
        actual = re.search(expression, rollback, re.S).group()
        self.assertEqual(actual, expected)
        self.assertEqual(rollback.count('$$'), 2)
        self.assertNotRegex(rollback, r'CREATE\s+FUNCTION\s+fn_dv1_')
        self.assertNotIn('fn_dv1_declare', rollback)
        self.assertIn('DROP INDEX IF EXISTS uk_attach_pending_client_upload_id;', rollback)


if __name__ == '__main__':
    unittest.main()
