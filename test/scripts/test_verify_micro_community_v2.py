"""V2 fixture self-tests prove verifier rejection, never product acceptance."""
import copy
import hashlib
import json
from pathlib import Path
import subprocess
from test.scripts import test_verify_micro_community_acceptance as legacy

V, ROOT = legacy.V, legacy.ROOT


class V2Test(legacy.VerifierTest):
    # Do not inherit V1's assertions: they intentionally prohibit full acceptance.
    def setUp(self):
        super().setUp()
        self.policy.write_bytes((ROOT / 'docs/reference/contracts/micro-community-v2-policy.json').read_bytes())
        repo = Path(self.manifest['sources']['backend']['repo_path'])
        contract = repo / 'docs/reference/contracts/micro-community-v2-policy.json'
        contract.parent.mkdir(parents=True)
        contract.write_bytes(self.policy.read_bytes())
        subprocess.run(['git', '-C', str(repo), 'add', '.'], check=True, capture_output=True)
        subprocess.run(['git', '-C', str(repo), '-c', 'user.name=leeyi', '-c', 'user.email=leeyisoft@qq.com',
                        'commit', '-qm', 'synthetic frozen V2 contract'], check=True)
        source = self.manifest['sources']['backend']
        source['head'] = subprocess.check_output(['git', '-C', str(repo), 'rev-parse', 'HEAD']).decode().strip()
        source['tree'] = subprocess.check_output(['git', '-C', str(repo), 'rev-parse', 'HEAD^{tree}']).decode().strip()
        self.json('environment.json', {'fixture': True})
        self.manifest.update(execution_policy='MICRO_COMMUNITY_V2', policy_sha256=V.digest(self.policy),
                             environment_path='environment.json', environment_sha256=V.digest(self.run / 'environment.json'),
                             executor='synthetic executor')
        for record in self.commands:
            record.update(environment_sha256=self.manifest['environment_sha256'], environment_overrides={})
            if record['id'].startswith('round'):
                record['argv'] = ['make', 'eunit']
                self.write_log(record, 'All 1 tests passed.\nFailed: 0. Skipped: 0.')
        for row in self.rows:
            row.update(status='PASS', policy_sha256=V.digest(self.policy))
        self.quality.update(backend_full_eunit='PASS', backend_full_rounds=['round1', 'round2'])
        self.json('quality-gates.json', self.quality)
        self.rebind_manifest()
        self.leaves = []
        command = next(c for c in self.commands if c['id'] == 'admin_test')
        for parent, items in json.loads(self.policy.read_text())['leaves'].items():
            for key, description in items.items():
                oracle = {'leaf_id': key, 'command_id': command['id'], 'candidate_manifest_sha256': self.candidate,
                          'log_sha256': command['log_sha256'], 'device_id': 'fixture', 'platform': 'android',
                          'assertions': [{'name': 'synthetic', 'expected': 1, 'actual': 1, 'passed': True}]}
                self.json(key + '.json', oracle)
                self.leaves.append(dict(id=key, parent=parent, requirement=description, status='PASS',
                                        candidate_manifest_sha256=self.candidate, command_id=command['id'],
                                        log_sha256=command['log_sha256'], evidence_path=key + '.json',
                                        evidence_sha256=V.digest(self.run / (key + '.json'))))
        self.json('state.json', {'status': 'MICRO_COMMUNITY_CANDIDATE_PASS',
                                'acceptance_ids': {r['id']: 'PASS' for r in self.rows}})
        self.seal()

    def seal(self):
        self.json('acceptance-leaves.json', self.leaves)
        self.json('independent-review.json', {'reviewer': 'independent fixture', 'status': 'PASS',
                    'candidate_manifest_sha256': self.candidate,
                    'artifacts': {name: V.digest(self.run / name) for name in
                                  ('acceptance-leaves.json', 'acceptance.tsv', 'quality-gates.json', 'commands.jsonl')}})

    def test_v2_normal(self):
        self.assertEqual(self.verify(), 'MICRO_COMMUNITY_CANDIDATE_PASS')

    def test_v2_leaf_set(self):
        for mode in ('missing', 'extra', 'duplicate'):
            old = copy.deepcopy(self.leaves)
            if mode == 'missing':
                self.leaves.pop()
            elif mode == 'extra':
                self.leaves.append(dict(self.leaves[0], id='MC-AC-27.01'))
            else:
                self.leaves[-1] = dict(self.leaves[0])
            self.seal()
            with self.assertRaisesRegex(ValueError, 'leaf ID set'):
                self.verify()
            self.leaves = old

    def test_v2_leaf_binding(self):
        for field, value in [('candidate_manifest_sha256', '0'*64), ('status', 'PARTIAL'),
                             ('log_sha256', '0'*64), ('requirement', 'reduced contract'),
                             ('evidence_path', '../outside.json')]:
            old = dict(self.leaves[0])
            self.leaves[0][field] = value
            self.seal()
            with self.assertRaises((ValueError, OSError)):
                self.verify()
            self.leaves[0] = old

    def test_v2_raw_failures(self):
        record = self.commands[-1]
        for raw in ['All 1 tests passed.\nFailed: 1.', 'All 1 tests passed.\nSkipped: 1.',
                    'All 1 tests passed.\nOne or more tests were cancelled.',
                    'All 1 tests passed.\nSIGTERM received - shutting down',
                    'All 1 tests passed.\nABORTED_BY_RESOURCE_GUARD=1', 'fake PASS']:
            self.write_log(record, raw)
            self.write_commands()
            self.seal()
            with self.assertRaises(ValueError):
                self.verify()

    def test_v2_expected_business_error_log(self):
        record = next(c for c in self.commands if c['id'] == 'round1')
        self.write_log(record, '[organization_admin_invitation_cancelled]\nexpected request timed out\nAll 1 tests passed.')
        self.write_commands()
        self.seal()
        self.assertEqual(self.verify(), 'MICRO_COMMUNITY_CANDIDATE_PASS')

    def test_v2_filter_rejected(self):
        self.commands[-1]['environment_overrides'] = {'EUNIT_MODS': 'only_one_test'}
        self.write_commands()
        self.seal()
        with self.assertRaisesRegex(ValueError, 'filtered EUnit environment'):
            self.verify()

    def test_v2_two_rounds_required(self):
        self.quality['backend_full_rounds'] = ['round1', 'round1']
        self.json('quality-gates.json', self.quality)
        self.seal()
        with self.assertRaisesRegex(ValueError, 'distinct full EUnit'):
            self.verify()

    def test_v2_same_executor_rejected(self):
        review = json.loads((self.run / 'independent-review.json').read_text())
        review['reviewer'] = self.manifest['executor']
        self.json('independent-review.json', review)
        with self.assertRaisesRegex(ValueError, 'independent review'):
            self.verify()


    def test_v2_device_oracle(self):
        leaf = next(r for r in self.leaves if r['parent'] == 'MC-AC-22')
        path = self.run / leaf['evidence_path']
        oracle = json.loads(path.read_text())
        oracle['device_id'] = 'emulator'
        self.json(leaf['evidence_path'], oracle)
        leaf['evidence_sha256'] = V.digest(path)
        self.seal()
        with self.assertRaisesRegex(ValueError, 'device/platform'):
            self.verify()

    def test_v2_hidden_index(self):
        repo = Path(self.manifest['sources']['backend']['repo_path'])
        path = repo / 'package.json'
        original = path.read_bytes()
        path.write_bytes(original + b' ')
        subprocess.run(['git', '-C', str(repo), 'add', 'package.json'], check=True)
        path.write_bytes(original)
        with self.assertRaisesRegex(ValueError, 'hidden index'):
            self.verify()

    def test_v2_symlink_escape(self):
        outside = self.run.parent / (self.run.name + '-outside')
        outside.write_text('{}')
        self.addCleanup(outside.unlink)
        (self.run / 'escape').symlink_to(outside)
        self.leaves[0].update(evidence_path='escape', evidence_sha256=V.digest(outside))
        self.seal()
        with self.assertRaisesRegex(ValueError, 'escapes run root'):
            self.verify()

    def test_v2_bad_oracle(self):
        leaf = self.leaves[0]
        path = self.run / leaf['evidence_path']
        original = json.loads(path.read_text())
        for field, value in [('command_id', 'wrong'), ('assertions', []), ('log_sha256', '0'*64)]:
            oracle = dict(original, **{field: value})
            self.json(leaf['evidence_path'], oracle)
            leaf['evidence_sha256'] = V.digest(path)
            self.seal()
            with self.assertRaises(ValueError):
                self.verify()


def load_tests(loader, _tests, _pattern):
    # Legacy tests run under their original policy in their own test module.
    import unittest
    return unittest.TestSuite(V2Test(name) for name in loader.getTestCaseNames(V2Test)
                              if name.startswith('test_v2_'))
