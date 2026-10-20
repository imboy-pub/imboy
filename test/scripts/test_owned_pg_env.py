"""Pure runner contract: explicit source, no daily config inference or value logging."""
import json
import os
from pathlib import Path
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / 'scripts/ci/with-owned-pg-env.sh'


class OwnedPgEnvTests(unittest.TestCase):
    def run_command(self, changes=None, remove=()):
        env = dict(os.environ, IMBOY_TEST_OWNED_PG_SOURCE='1', PGHOST='127.0.0.1',
                   PGPORT='55432', PGUSER='synthetic-user', PGDATABASE='synthetic-db', PGPASSWORD='')
        env.update(changes or {})
        for key in remove:
            env.pop(key, None)
        code = 'import json,os; print(json.dumps({n:os.environ[n] for n in ["HIRDPOC_PG_HOST","HIRDPOC_PG_PASSWORD","DIRECTORY_SYNC_INTTEST_PG_PASSWORD","TG16_PG_PASSWORD"]}))'
        return subprocess.run([str(SCRIPT), sys.executable, '-c', code], env=env,
                              text=True, capture_output=True)

    def test_explicit_empty_password_is_preserved(self):
        result = self.run_command()
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual({'HIRDPOC_PG_HOST': '127.0.0.1', 'HIRDPOC_PG_PASSWORD': '',
                          'DIRECTORY_SYNC_INTTEST_PG_PASSWORD': '', 'TG16_PG_PASSWORD': ''}, json.loads(result.stdout))

    def test_missing_inputs_fail_without_values(self):
        for name in ['IMBOY_TEST_OWNED_PG_SOURCE', 'PGHOST', 'PGPORT', 'PGUSER', 'PGDATABASE', 'PGPASSWORD']:
            with self.subTest(name=name):
                result = self.run_command(remove=[name])
                self.assertEqual(2, result.returncode)
                self.assertEqual('', result.stdout)
                self.assertNotIn('synthetic-user', result.stderr)
                self.assertNotIn('synthetic-db', result.stderr)

    def test_nonloopback_and_invalid_port_are_rejected(self):
        for changes in [{'PGHOST': 'example.invalid'}, {'PGPORT': '0'}, {'PGPORT': '65536'}, {'PGPORT': 'x'}]:
            self.assertEqual(2, self.run_command(changes).returncode)


if __name__ == '__main__':
    unittest.main()
