"""Synthetic verifier tests are tool self-tests, never business acceptance."""
import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
SPEC = importlib.util.spec_from_file_location('micro_verifier', ROOT / 'scripts/verify_micro_community_acceptance.py')
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)


class VerifierTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.run = Path(self.temp.name)
        self.plan = self.run / 'plan'
        self.policy = self.run / 'policy'
        self.plan.write_text('synthetic plan')
        self.policy.write_text('NO_FULL_EUNIT')
        sources = self.create_repositories()
        (self.run / 'protected-wip.sha256').write_text('synthetic frozen WIP')
        (self.run / 'current-protected-wip.sha256').write_text('synthetic frozen WIP')
        baseline = self.run / 'baseline'
        baseline.mkdir()
        (baseline / 'protected').write_text('foreign baseline bytes')
        for source in sources.values():
            source['baseline_repo_path'] = str(baseline)
            source['baseline_head'] = source['head']
        self.json('protected-wip.json', [{'path': str(baseline / 'protected'),
                                         'sha256': V.digest(baseline / 'protected'), 'repo': 'backend'}])
        self.json('devices.json', [{'id': 'fixture', 'targetPlatform': 'android-arm64',
                                   'isSupported': True, 'emulator': False}])
        self.manifest = {'plan_sha256': V.digest(self.plan), 'policy_sha256': V.digest(self.policy),
                         'execution_policy': 'NO_FULL_EUNIT', 'protected_wip_sha256': V.digest(self.run / 'protected-wip.sha256'),
                         'protected_wip_manifest_sha256': V.digest(self.run / 'protected-wip.json'),
                         'device': {'targetPlatform': 'android', 'isPhysicalDevice': True, 'resource_id': 'fixture',
                                    'discovery_path': 'devices.json', 'discovery_sha256': V.digest(self.run / 'devices.json'),
                                    'discovery_command_id': 'discovery'},
                         'sources': sources}
        self.json('candidate-manifest.json', self.manifest)
        self.candidate = V.digest(self.run / 'candidate-manifest.json')
        self.commands = self.create_commands(sources)
        self.write_commands()
        (self.run / 'focused-suites.txt').write_text('fixture_tests\n')
        self.quality = {'backend_full_eunit': 'NOT_EXECUTED_BY_USER_POLICY',
                        'gates': {name: name for name in V.GATES}, 'backend_scoped_rounds': [['round1'], ['round2']]}
        self.json('quality-gates.json', self.quality)
        self.rows = [{'id': name, 'status': 'PARTIAL' if name == 'MC-AC-25' else 'PASS',
                      'candidate_manifest_sha256': self.candidate, 'plan_sha256': V.digest(self.plan),
                      'policy_sha256': V.digest(self.policy), 'command_id': 'admin_test', 'oracle': 'synthetic oracle',
                      'evidence_path': 'admin_test.log', 'evidence_sha256': V.digest(self.run / 'admin_test.log'), 'reviewer': 'fixture',
                      'blocker': 'NO_FULL_EUNIT'} for name in sorted(V.IDS)]
        self.write_rows()
        self.json('state.json', {'status': 'SCOPED_IMPLEMENTATION_PASS', 'acceptance_ids': {r['id']: r['status'] for r in self.rows}})

    def create_repositories(self):
        sources = {}
        scripts = {'test': 'bun test --isolate', 'lint': 'eslint .',
                   'typecheck': 'tsc --noEmit -p tsconfig.app.json', 'deadcode': 'knip',
                   'build': 'tsc -b && vite build'}
        for name in ('backend', 'admin', 'app'):
            repo = self.run / name
            repo.mkdir()
            subprocess.run(['git', 'init', '-q', str(repo)], check=True)
            (repo / 'package.json').write_text(json.dumps({'scripts': scripts}))
            subprocess.run(['git', '-C', str(repo), 'add', 'package.json'], check=True)
            subprocess.run(['git', '-C', str(repo), '-c', 'user.name=leeyi', '-c', 'user.email=leeyisoft@qq.com', 'commit', '-qm', 'fixture'], check=True)
            def git(*args):
                return subprocess.check_output(['git', '-C', str(repo), *args]).decode().strip()
            sources[name] = {'repo_path': str(repo), 'head': git('rev-parse', 'HEAD'),
                             'tree': git('rev-parse', 'HEAD^{tree}'),
                             'diff_sha256': hashlib.sha256(b'').hexdigest(), 'untracked': {}}
        return sources

    def create_commands(self, sources):
        results = {'backend_compile': 'ERLC imboy.erl', 'admin_test': '1 pass\n0 fail',
                   'admin_lint': '$ eslint .', 'admin_typecheck': '$ tsc --noEmit -p tsconfig.app.json',
                   'admin_deadcode': '$ knip', 'admin_build': '1 modules transformed.\nbuilt in 1s',
                   'app_analyze': 'No issues found!', 'app_test': '00:01 +1: All tests passed!',
                   'round1': 'All 1 tests passed.', 'round2': 'All 1 tests passed.',
                   'discovery': (self.run / 'devices.json').read_text()}
        commands = []
        for index, name in enumerate(sorted(V.GATES) + ['discovery', 'round1', 'round2']):
            owner = 'backend' if name.startswith(('backend', 'round')) else ('admin' if name.startswith('admin') else 'app')
            argv = ['make', 'compile'] if name == 'backend_compile' else (['bun', 'run', name[6:]] if owner == 'admin' else ['flutter', name[4:]])
            record = {'id': name, 'argv': argv, 'cwd': sources[owner]['repo_path'], 'exit_code': 0,
                      'test_count': 1, 'log_path': name + '.log', 'candidate_manifest_sha256': self.candidate,
                      'started_at': f'2026-10-08T00:00:{index * 2:02}+00:00',
                      'ended_at': f'2026-10-08T00:00:{index * 2 + 1:02}+00:00'}
            if name == 'discovery':
                record.update(argv=['flutter', 'devices', '--machine'], cwd=sources['app']['repo_path'])
            if name.startswith('round'):
                record.update(argv=['make', 'eunit', 't=fixture_tests'], suite='fixture_tests')
            self.write_log(record, results[name])
            commands.append(record)
        return commands

    def write_log(self, command, raw):
        start = {key: command[key] for key in ('id', 'argv', 'cwd', 'started_at')}
        end = {key: command[key] for key in ('id', 'exit_code', 'ended_at')}
        log = self.run / command['log_path']
        log.write_text('MC_COMMAND_START ' + json.dumps(start) + '\n' + raw + '\nMC_COMMAND_END ' + json.dumps(end) + '\n')
        command['log_sha256'] = V.digest(log)

    def json(self, name, value):
        (self.run / name).write_text(json.dumps(value))

    def write_rows(self):
        with (self.run / 'acceptance.tsv').open('w') as handle:
            writer = csv.DictWriter(handle, fieldnames=list(self.rows[0]), delimiter='\t')
            writer.writeheader()
            writer.writerows(self.rows)

    def write_commands(self):
        (self.run / 'commands.jsonl').write_text('\n'.join(json.dumps(c) for c in self.commands))

    def verify(self):
        return V.verify(self.run, self.plan, self.policy, self.candidate)

    def test_normal_synthetic_bundle(self):
        self.assertEqual(self.verify(), 'SCOPED_IMPLEMENTATION_PASS')

    def test_id_missing_extra_duplicate(self):
        for mode in ('missing', 'extra', 'duplicate'):
            original = list(self.rows)
            if mode == 'missing':
                self.rows.pop()
            elif mode == 'extra':
                self.rows.append(dict(self.rows[0], id='MC-AC-27'))
            else:
                self.rows[-1] = dict(self.rows[0])
            self.write_rows()
            with self.assertRaisesRegex(ValueError, 'ID set'):
                self.verify()
            self.rows = original

    def test_row_false_pass_and_hash(self):
        for field, value in [('oracle', ''), ('evidence_path', 'absent'), ('evidence_sha256', '0' * 64),
                             ('candidate_manifest_sha256', '0' * 64), ('plan_sha256', '0' * 64),
                             ('policy_sha256', '0' * 64)]:
            old = self.rows[0][field]
            self.rows[0][field] = value
            self.write_rows()
            with self.assertRaises(ValueError):
                self.verify()
            self.rows[0][field] = old

    def test_nonzero_empty_tests_full_eunit(self):
        for changes in ({'exit_code': 1}, {'test_count': 0}, {'argv': ['make', 'eunit']},
                        {'argv': ['make', 'eunit', 't=*']}, {'argv': ['make', 'check']}):
            old = dict(self.commands[-1])
            self.commands[-1].update(changes)
            self.write_commands()
            with self.assertRaises(ValueError):
                self.verify()
            self.commands[-1] = old

    def test_device_and_wip(self):
        original_device = dict(self.manifest['device'])
        for changes in ({'targetPlatform': 'ios'}, {'isPhysicalDevice': False}):
            self.manifest['device'] = dict(original_device, **changes)
            self.rebind_manifest()
            with self.assertRaisesRegex(ValueError, 'Android physical device'):
                self.verify()
        self.manifest['device'] = original_device
        self.rebind_manifest()
        (self.run / 'current-protected-wip.sha256').write_text('foreign change')
        with self.assertRaisesRegex(ValueError, 'protected WIP drift'):
            self.verify()

    def rebind_manifest(self):
        self.json('candidate-manifest.json', self.manifest)
        self.candidate = V.digest(self.run / 'candidate-manifest.json')
        for command in self.commands:
            command['candidate_manifest_sha256'] = self.candidate
        for row in self.rows:
            row['candidate_manifest_sha256'] = self.candidate
        self.write_commands()
        self.write_rows()

    def test_quality_gate_rejects_fixture_argv(self):
        command = next(c for c in self.commands if c['id'] == 'backend_compile')
        command['argv'] = ['fixture', 'backend_compile']
        self.write_log(command, 'ERLC imboy.erl')
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'quality gate argv'):
            self.verify()

    def test_quality_gate_rejects_wrong_repo(self):
        command = next(c for c in self.commands if c['id'] == 'backend_compile')
        command['cwd'] = self.manifest['sources']['app']['repo_path']
        self.write_log(command, 'ERLC imboy.erl')
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'quality repository cwd'):
            self.verify()

    def test_failed_log_cannot_be_rehashed_to_pass(self):
        command = self.commands[-1]
        self.write_log(command, 'FAILED: 0 tests passed; tests skipped; exit 1')
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'runner failure'):
            self.verify()

    def test_self_declared_count_without_runner_summary_fails(self):
        command = self.commands[-1]
        self.write_log(command, 'synthetic no test summary')
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'EUnit runner result'):
            self.verify()

    def test_two_rounds_reject_log_reuse_and_overlap(self):
        command, first = self.commands[-1], self.commands[-2]
        old = dict(command)
        command.update(log_path=first['log_path'], log_sha256=first['log_sha256'])
        # Identity-binding fails before the independent round log-reuse gate.
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'envelope identity'):
            self.verify()
        command.update(old, started_at=first['started_at'], ended_at=first['ended_at'])
        self.write_log(command, 'All 1 tests passed.')
        self.write_commands()
        with self.assertRaisesRegex(ValueError, 'overlap/reuse time'):
            self.verify()

    def test_actual_protected_file_bytes_are_checked(self):
        (self.run / 'baseline' / 'protected').write_text('altered foreign bytes')
        with self.assertRaisesRegex(ValueError, 'actual protected WIP drift'):
            self.verify()

    def test_protected_path_mixed_into_candidate_is_rejected(self):
        path = self.run / 'backend' / 'protected'
        path.write_text('foreign baseline bytes')
        self.manifest['sources']['backend']['untracked'] = {'protected': V.digest(path)}
        self.rebind_manifest()
        with self.assertRaisesRegex(ValueError, 'protected WIP mixed'):
            self.verify()

    def test_physical_declaration_cannot_override_discovery(self):
        devices = [{'id': 'fixture', 'targetPlatform': 'android-arm64', 'isSupported': True, 'emulator': True}]
        self.json('devices.json', devices)
        self.manifest['device']['discovery_sha256'] = V.digest(self.run / 'devices.json')
        command = next(c for c in self.commands if c['id'] == 'discovery')
        self.write_log(command, json.dumps(devices))
        self.rebind_manifest()
        with self.assertRaisesRegex(ValueError, 'supported physical Android'):
            self.verify()

    def test_source_wip_is_recomputed(self):
        (self.run / 'backend' / 'foreign').write_text('untracked WIP')
        with self.assertRaisesRegex(ValueError, 'untracked/WIP'):
            self.verify()

    def test_mc25_and_quality_policy_cannot_pass(self):
        row = next(r for r in self.rows if r['id'] == 'MC-AC-25')
        row['status'] = 'PASS'
        self.write_rows()
        with self.assertRaisesRegex(ValueError, 'MC-AC-25'):
            self.verify()
        row['status'] = 'PARTIAL'
        self.write_rows()
        self.quality['backend_full_eunit'] = 'PASS'
        self.json('quality-gates.json', self.quality)
        with self.assertRaisesRegex(ValueError, 'full EUnit'):
            self.verify()

    def test_source_patch_is_recomputed(self):
        repo = self.run / 'backend'
        (repo / 'tracked').write_text('baseline')
        subprocess.run(['git', '-C', str(repo), 'add', 'tracked'], check=True)
        with self.assertRaisesRegex(ValueError, 'patch drift'):
            self.verify()

    def test_no_full_acceptance_claim(self):
        self.json('state.json', {'status': 'LOCAL_CANDIDATE_PASS', 'acceptance_ids': {r['id']: r['status'] for r in self.rows}})
        with self.assertRaisesRegex(ValueError, 'completion'):
            self.verify()

    def test_plan_policy_and_candidate_drift(self):
        for path in (self.plan, self.policy, self.run / 'candidate-manifest.json'):
            old = path.read_bytes()
            path.write_bytes(old + b' ')
            with self.assertRaises(ValueError):
                self.verify()
            path.write_bytes(old)


if __name__ == '__main__':
    unittest.main()
