#!/usr/bin/env python3
"""Small offline contract tests; never install software, start services or run BEAM."""
import ast
import json
import os
import re
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent

class NativePlanTests(unittest.TestCase):
    def test_uid_proof_exact_report(self):
        surfaces = ('ordinary', 'enterprise_internal', 'enterprise_seat', 'cs_widget')
        leaves = [s+'/disk/provider_bypass_writer_denied' for s in surfaces]

        def report():
            root = ET.Element('testsuite', name='native_uid_preflight', tests='4', failures='0', errors='0', skipped='0')
            for leaf in leaves:
                case = ET.SubElement(root, 'testcase', name='fixture:1 test ('+leaf+')')
                row = dict(leaf=leaf, owner_uid=0, writer_uid=65534, unprivileged_writer_exit=2)
                ET.SubElement(case, 'system-out').text = 'before <&>\nW403_PROVIDER_BYPASS '+json.dumps(row)+'\nafter\n'
            return root

        def row_change(root, key, value):
            node = root[0][0]
            row = json.loads(node.text.splitlines()[1].split(' ', 1)[1])
            row[key] = value
            node.text = 'W403_PROVIDER_BYPASS '+json.dumps(row)+'\n'

        mutations = [
            ('wrong suite', lambda r: r.set('name', 'other')),
            ('wrong count', lambda r: r.set('tests', '3')),
            ('failed counter', lambda r: r.set('failures', '1')),
            ('error counter', lambda r: r.set('errors', '1')),
            ('skip counter', lambda r: r.set('skipped', '1')),
            ('missing case', lambda r: r.remove(r[0])),
            ('extra case', lambda r: ET.SubElement(r, 'testcase')),
            ('failed case', lambda r: ET.SubElement(r[0], 'failure')),
            ('error case', lambda r: ET.SubElement(r[0], 'error')),
            ('skipped case', lambda r: ET.SubElement(r[0], 'skipped')),
            ('missing output', lambda r: r[0].remove(r[0][0])),
            ('missing proof', lambda r: setattr(r[0][0], 'text', 'ordinary output')),
            ('duplicate proof', lambda r: setattr(r[0][0], 'text', r[0][0].text*2)),
            ('malformed json', lambda r: setattr(r[0][0], 'text', 'W403_PROVIDER_BYPASS {bad')),
            ('nonobject json', lambda r: setattr(r[0][0], 'text', 'W403_PROVIDER_BYPASS []')),
            ('unknown leaf', lambda r: row_change(r, 'leaf', 'unexpected')),
            ('duplicate leaf', lambda r: row_change(r, 'leaf', leaves[1])),
            ('case mismatch', lambda r: r[0].set('name', 'wrong descriptor')),
            ('nonroot owner', lambda r: row_change(r, 'owner_uid', 1000)),
            ('boolean owner', lambda r: row_change(r, 'owner_uid', False)),
            ('root writer', lambda r: row_change(r, 'writer_uid', 0)),
            ('negative writer', lambda r: row_change(r, 'writer_uid', -1)),
            ('boolean writer', lambda r: row_change(r, 'writer_uid', True)),
            ('successful write', lambda r: row_change(r, 'unprivileged_writer_exit', 0)),
            ('invalid exit', lambda r: row_change(r, 'unprivileged_writer_exit', -1)),
            ('string exit', lambda r: row_change(r, 'unprivileged_writer_exit', '2')),
            ('unapproved field', lambda r: row_change(r, 'secret', 'do-not-export')),
        ]
        with tempfile.TemporaryDirectory() as temp:
            directory = Path(temp)/'xml'; directory.mkdir()
            xml = directory/'TEST-native_uid_preflight.xml'
            out = Path(temp)/'proof.json'

            def check(good):
                if out.exists(): out.unlink()
                result = subprocess.run([sys.executable, str(HERE/'native-eunit-uid-proof.py'), str(directory), str(out)], capture_output=True)
                self.assertEqual(result.returncode == 0, good)
                self.assertEqual(out.exists(), good)
                if good:
                    self.assertEqual({p['leaf'] for p in json.loads(out.read_text())}, set(leaves))
                self.assertNotIn(b'do-not-export', result.stdout+result.stderr)

            ET.ElementTree(report()).write(xml, encoding='utf-8'); check(True)
            for name, mutate in mutations:
                with self.subTest(name=name):
                    root = report(); mutate(root)
                    ET.ElementTree(root).write(xml, encoding='utf-8'); check(False)
            xml.write_text('<testsuite>'); check(False)
            xml.unlink(); check(False)
            ET.ElementTree(report()).write(xml, encoding='utf-8')
            extra = directory/'TEST-extra.xml'; extra.write_bytes(xml.read_bytes()); check(False)

    def test_job_env_context_and_runner_paths(self):
        workflow = (ROOT / '.github/workflows/native-eunit-acceptance.yml').read_text()
        job_env = re.search(r'^    env:\n(.*?)^    steps:', workflow, re.M | re.S).group(1)
        # Job-level env cannot reference runner, job, steps or env contexts.
        allowed = {'github', 'needs', 'strategy', 'matrix', 'vars', 'secrets', 'inputs'}
        for context in re.findall(r'\$\{\{\s*([A-Za-z_]+)', job_env):
            self.assertIn(context, allowed)
        self.assertNotIn('NATIVE_CI_ROOT:', job_env)
        self.assertNotIn('NATIVE_CI_ARTIFACTS:', job_env)
        self.assertIn("if: always() && env.NATIVE_CI_ARTIFACTS != ''", workflow)
        first_run = re.search(r'^        run: \|\n(.*?)(?=^      - name:)', workflow, re.M | re.S).group(1)
        prefix = '\n'.join(line[10:] for line in first_run.splitlines()).split('if [[ "$NATIVE_EPHEMERAL_KEYS_APPROVED"')[0]
        self.assertIn('export NATIVE_CI_ROOT=', prefix)
        self.assertIn('export NATIVE_CI_ARTIFACTS=', prefix)
        self.assertIn('>> "$GITHUB_ENV"', prefix)
        with tempfile.TemporaryDirectory() as temp:
            env_path = Path(temp) / 'github-env'
            env = dict(os.environ, RUNNER_TEMP=temp+'/', GITHUB_RUN_ID='123', GITHUB_RUN_ATTEMPT='2', GITHUB_ENV=str(env_path))
            result = subprocess.run(['bash', '-c', prefix + '\nprintf "%s\\n%s\\n" "$NATIVE_CI_ROOT" "$NATIVE_CI_ARTIFACTS"'], env=env, text=True, capture_output=True, check=True)
            root = temp + '/imboy-native-123-2'
            artifacts = temp + '/imboy-native-evidence-123-2'
            self.assertEqual(result.stdout, root+'\n'+artifacts+'\n')
            self.assertEqual(env_path.read_text(), 'NATIVE_CI_ROOT='+root+'\nNATIVE_CI_ARTIFACTS='+artifacts+'\n')

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
        self.assertIn('branches: [ci/native-eunit-acceptance-20261018, ci/eunit-followup-20261018]', workflow)
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
