#!/usr/bin/env python3
"""Small offline contract tests; never install software, start services or run BEAM."""
import ast
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent

class NativePlanTests(unittest.TestCase):
    def test_syntax_and_pins(self):
        for path in HERE.glob('*.py'):
            ast.parse(path.read_text(), filename=str(path))
        for path in HERE.glob('*.sh'):
            subprocess.run(['bash', '-n', str(path)], check=True)
        pins = json.loads((HERE / 'native-eunit-pins.json').read_text())
        self.assertEqual(len(pins['extensions']), 12)
        self.assertEqual(len(pins['excluded_module_entries']), 9)
        for source in pins['sources'] + pins['rust'] + [pins['garage']]:
            key = 'commit' if source.get('kind') == 'git' else 'sha256'
            self.assertRegex(source[key], r'^[a-f0-9]{%d}$' % (40 if key == 'commit' else 64))

    def test_final_command_and_scope(self):
        workflow = (ROOT / '.github/workflows/native-eunit-acceptance.yml').read_text()
        self.assertIn('branches: [ci/native-eunit-acceptance-20261018]', workflow)
        self.assertIn('runs-on: ubuntu-24.04', workflow)
        self.assertIn("NATIVE_EPHEMERAL_KEYS_APPROVED: 'true'", workflow)
        self.assertNotIn('secrets.', workflow)
        self.assertNotIn('workflow_dispatch:', workflow)
        self.assertNotIn('services:', workflow)
        run = (HERE / 'native-eunit-run.sh').read_text()
        self.assertIn("bash -c 'IMBOYENV=local make eunit'", run)
        self.assertNotIn('EUNIT_MODS=', run)
        self.assertIn('prepare_database uid', run)
        self.assertIn('prepare_database full', run)
        self.assertIn('DIRECTORY_SYNC_INTTEST_PG_PASSWORD=fixture-local-trust', run)
        self.assertIn('set +e # Every disposal step', run)
        self.assertIn('sudo -n rm -rf --one-file-system -- "$PRIVATE"', run)
        self.assertIn('.native-ci-owner', run)
        self.assertIn('private_directory_deleted=%s', run)
        self.assertNotIn('docker ', run.lower())
        bootstrap=(HERE/'native-eunit-bootstrap.sh').read_text()
        self.assertIn('$(pg_config --sharedir)/tsearch_data/jieba_user.dict',bootstrap)

    def test_strict_raw_exit_and_framework(self):
        cases = [('All 2 tests passed.',0,True),
                 ('All 2 tests passed.',2,False),
                 ('All 2 tests passed.\ncontext setup failed',0,False),
                 ('All 2 tests passed.\ncontext cleanup failed',0,False),
                 ('All 2 tests passed.\ninstantiation of subtests failed',0,False),
                 ('All 2 tests passed.\nresult from instantiator x is not a test',0,False),
                 ('All 2 tests passed.\nOne or more tests were cancelled.',0,False),
                 ('All 2 tests passed.\nSkipped: 1.',0,False),
                 ('All 2 tests passed.\nFailed: 1.',0,False),
                 ('All 2 tests passed.\nsuite:case...*timed out*',0,False),
                 ('All 2 tests passed.\nSIGTERM received - shutting down',0,False),
                 ('All 2 tests passed.\ngmake: *** [eunit] Terminated: 15',0,False),
                 ('All 2 tests passed.\nThere were no tests to run.',0,False),
                 ('expected request timed out\nAll 2 tests passed.',0,True),
                 ('',0,False)]
        with tempfile.TemporaryDirectory() as temp:
            path=Path(temp)/'raw.log'; summary=Path(temp)/'summary.json'
            for raw,rc,good in cases:
                with self.subTest(raw=raw,rc=rc):
                    path.write_text(raw); Path(str(path)+'.exit').write_text(str(rc))
                    result=subprocess.run([sys.executable,str(HERE/'native-eunit-validate.py'),str(path),str(summary)],capture_output=True)
                    self.assertEqual(result.returncode==0,good)
                    self.assertEqual(json.loads(summary.read_text())['clean'],good)

    def test_capture_redacts_and_preserves_exit(self):
        with tempfile.TemporaryDirectory() as temp:
            raw=Path(temp)/'raw.log'; public=Path(temp)/'public.log'
            env=dict(os.environ,W202_GARAGE_SECRET_KEY='synthetic-secret-check')
            result=subprocess.run([sys.executable,str(HERE/'native-eunit-capture.py'),str(raw),str(public),sys.executable,'-c',"print('synthetic-secret-check');raise SystemExit(7)"],env=env,capture_output=True)
            self.assertEqual(result.returncode,7)
            self.assertIn('synthetic-secret-check',raw.read_text())
            self.assertNotIn('synthetic-secret-check',public.read_text())
            self.assertNotIn(b'synthetic-secret-check',result.stdout)
            self.assertEqual(Path(str(raw)+'.exit').read_text(),'7\n')

if __name__ == '__main__':
    unittest.main()
