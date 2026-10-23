import copy
import contextlib
import importlib.util
import io
import json
import tempfile
import unittest
from unittest.mock import patch
from pathlib import Path
R = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('validator', R / 'validate_plan.py')
v = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v)
S = json.loads((R / 'fixture-execution-state.json').read_text())
C = json.loads((R / 'fixture-execution-contract.json').read_text())
M = {'revision': C['revision'], 'files': {n: '0' * 64 for n in C['manifest_required_files']}}

class Checks(unittest.TestCase):
    def test_revision9_state_cannot_silently_use_revision10_contract(self):
        state = copy.deepcopy(S)
        state['plan_revision'] = '9'
        self.assertIn('contract revision mismatch', v.validate(state, C, M, R, False))

    def test_revision10_preserves_task_and_final_acceptance_scope(self):
        self.assertEqual(C['required_tasks'], [f'T{i:02d}' for i in range(1, 26)])
        self.assertTrue({'backend_full', 'app_full', 'device_journeys',
                         'visual_acceptance', 'resource_preemption'} <= set(C['required_checks']['T24']))
        self.assertIn('attachment_bind_gc', C['required_checks']['T06'])
        self.assertIn('scope_authorization', C['required_checks']['T05'])

    def test_revision10_async_additions_do_not_create_fake_runner_bindings(self):
        for name in ('DOUBAO-API.md', 'design-preview-doubao.html',
                     'fixtures/doubao-profile-v1.json'):
            self.assertIn(name, C['manifest_required_files'])
        self.assertIn('paid_dispatch_fence', C['required_checks']['T12'])
        self.assertNotIn('paid_dispatch_fence', C['frozen_test_bindings'].get('T12', {}))

    def test_external_state_argument_is_explicit(self):
        args = v.parse_arguments(['--directory', '/tmp/design', '--state', '/tmp/run/state.json'])
        self.assertEqual(args.directory, Path('/tmp/design'))
        self.assertEqual(args.state, Path('/tmp/run/state.json'))

    def test_default_state_remains_unspecified(self):
        self.assertIsNone(v.parse_arguments([]).state)

    def test_cli_reads_external_state_without_creating_local_state(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'design'
            root.mkdir()
            state = Path(directory) / 'run-state.json'
            state.write_text('{"origin": "external"}')
            for name in ('execution-contract.json', 'plan-manifest.json'):
                (root / name).write_text('{}')
            before = {p: p.read_bytes() for p in Path(directory).rglob('*') if p.is_file()}
            with patch('sys.argv', ['validate_plan.py', '--directory', str(root), '--state', str(state)]), \
                    patch.object(v, 'validate', return_value=[]) as validate, \
                    contextlib.redirect_stdout(io.StringIO()):
                v.main()
            self.assertEqual(validate.call_args.args[0], {'origin': 'external'})
            self.assertEqual(before, {p: p.read_bytes() for p in Path(directory).rglob('*') if p.is_file()})

    def check(self, mutate, expected):
        s = copy.deepcopy(S)
        mutate(s)
        errors = v.validate(s, C, M, R, check_files=False)
        self.assertTrue(any(expected in e for e in errors), errors)

    def test_permission_escalation(self):
        self.check(lambda s: s['permissions'].update(push=True), 'lacks trusted grant')

    def test_wildcard_claim(self):
        self.check(lambda s: s['tasks'][0].update(status='running', allowed_paths=['/tmp/src/*']), 'concrete paths')

    def test_component_can_unblock_development(self):
        state = copy.deepcopy(S)
        by_id = {t['id']: t for t in state['tasks']}
        by_id['T16']['status'] = 'component_ready'
        task = by_id['T18']
        task.update(status='running', controller_epoch=0, owner='worker-U',
                    allowed_paths=['/tmp/task/app.dart'], worktree='/tmp/task',
                    lease_until='2030-01-01T00:00:00Z')
        self.assertEqual(v.validate_task(task, by_id, state, C, False), [])

    def test_component_cannot_replace_final_acceptance(self):
        state = copy.deepcopy(S)
        by_id = {t['id']: t for t in state['tasks']}
        by_id['T16']['status'] = 'component_ready'
        by_id['T18']['status'] = 'accepted'
        errors = v.validate_task(by_id['T18'], by_id, state, C, False)
        self.assertTrue(any('dependency not ready: T16' in e for e in errors))

    def test_old_candidate_pass_rejected(self):
        state = copy.deepcopy(S)
        task = state['tasks'][0]
        task.update(status='accepted', candidate='new')
        task['evidence'] = [dict(path='/tmp/fixture', sha256='unused', command='test',
                                exit_code=0, candidate='old', created_at='2026-10-20',
                                result='PASS', scope='unit', check_id='baseline', runner_sha256='0'*64, cwd='/tmp')]
        self.assertTrue(any('no current' in e for e in v.validate_evidence(task, C, False)))

    def test_nonzero_pass_rejected(self):
        task = copy.deepcopy(S['tasks'][0])
        task['candidate'] = 'candidate'
        task['evidence'] = [dict(path='/tmp/fixture', sha256='unused', command='test',
                                exit_code=1, candidate='candidate', created_at='2026-10-20',
                                result='PASS', scope='unit', check_id='baseline', runner_sha256='0'*64, cwd='/tmp')]
        errors = v.validate_evidence(task, C, False)
        self.assertTrue(any('required evidence not PASS' in e for e in errors))

    def test_empty_manifest_rejected(self):
        errors = v.validate(S, C, {'revision': C['revision'], 'files': {}}, R, False)
        self.assertTrue(any('file set' in e for e in errors))

    def test_bad_manifest_hash(self):
        manifest = copy.deepcopy(M)
        manifest['files']['PLAN.md'] = 'not-a-hash'
        errors = v.validate(S, C, manifest, R, False)
        self.assertTrue(any('invalid manifest hash' in e for e in errors))

    def test_expired_claim(self):
        self.check(lambda s: s['tasks'][0].update(status='running',
                   lease_until='2000-01-01T00:00:00Z'), 'expired')

    def test_bad_lease(self):
        self.check(lambda s: s['tasks'][0].update(status='running',
                   lease_until='anything'), 'malformed lease')

    def test_empty_component_rejected(self):
        self.check(lambda s: s['tasks'][15].update(status='component_ready'),
                   'component candidate/evidence incomplete')

    def test_missing_required_check_coverage(self):
        task = copy.deepcopy(S['tasks'][0])
        task.update(status='accepted', candidate='candidate')
        self.assertTrue(any('coverage missing' in e for e in v.validate_evidence(task, C, False)))

    def test_bound_complete_evidence_accepted(self):
        task = copy.deepcopy(S['tasks'][0])
        task.update(status='accepted', candidate='candidate')
        for check in C['required_checks']['T01']:
            binding = dict(command='test '+check, runner_sha256='0'*64,
                           cwd='/tmp/owned', expected_exit=0)
            task['test_bindings'][check] = binding
            task['evidence'].append(dict(path='/tmp/fixture', sha256='0'*64,
                command=binding['command'], runner_sha256=binding['runner_sha256'],
                cwd=binding['cwd'], exit_code=0, candidate='candidate',
                created_at='2026-10-20', result='PASS', scope='integration', check_id=check))
        frozen_contract = copy.deepcopy(C)
        frozen_contract['frozen_test_bindings']['T01'] = copy.deepcopy(task['test_bindings'])
        self.assertEqual(v.validate_evidence(task, frozen_contract, False), [])
        task['evidence'][0]['command'] = 'different runner'
        self.assertTrue(any('binding' in e for e in v.validate_evidence(task, frozen_contract, False)))

    def test_state_and_evidence_cannot_rewrite_contract(self):
        task = copy.deepcopy(S['tasks'][0])
        task.update(status='accepted', candidate='candidate')
        binding = dict(command='unrelated', runner_sha256='0'*64, cwd='/tmp', expected_exit=0)
        task['test_bindings']['baseline'] = binding
        task['evidence'] = [dict(path='/tmp/f', sha256='0'*64, command='unrelated',
            runner_sha256='0'*64, cwd='/tmp', exit_code=0, candidate='candidate',
            created_at='2026-10-20', result='PASS', scope='integration', check_id='baseline')]
        errors = v.validate_evidence(task, C, False)
        self.assertTrue(any('frozen contract' in e for e in errors))

    def test_initial(self):
        self.assertEqual(v.validate(S, C, M, R, check_files=False), [])

    def test_cycle(self):
        self.check(lambda s: s['tasks'][0]['dependencies'].append('T25'), 'cycle')

    def test_dependency_drift(self):
        self.check(lambda s: s['tasks'][17]['dependencies'].clear(), 'definition drift')

    def test_unapproved(self):
        self.check(lambda s: s['tasks'][0].update(status='running'), 'lacks trusted')

    def test_stale_epoch(self):
        self.check(lambda s: s['tasks'][0].update(status='running', controller_epoch=9), 'stale')

    def test_illegal_component(self):
        self.check(lambda s: s['tasks'][0].update(status='component_ready'), 'not allowed')

    def test_forged_acceptance(self):
        self.check(lambda s: s['tasks'][0].update(status='accepted'), 'incomplete')

    def test_current_evidence_missing(self):
        self.check(lambda s: s['tasks'][0].update(status='accepted'), 'no current')

    def test_decision_gate(self):
        self.check(lambda s: s['tasks'][2].update(status='accepted'), 'unresolved decision')

    def test_fake_complete(self):
        self.check(lambda s: s.update(status='COMPLETE'), 'unaccepted')

    def test_duplicate_task(self):
        self.check(lambda s: s['tasks'].append(copy.deepcopy(s['tasks'][0])), 'duplicated')

    def test_not_ready(self):
        self.check(lambda s: s['tasks'][20].update(status='running'), 'not ready')

    def test_integration_skipped(self):
        self.check(lambda s: s['tasks'][15].update(status='accepted'), 'integration prerequisite')

    def test_over_capacity(self):
        def mutate(s):
            s['runtime_capacity'] = 1
            for t in s['tasks'][:2]:
                t['status'] = 'running'
        self.check(mutate, 'exceed capacity')

    def test_shared_lock(self):
        def mutate(s):
            for t in s['tasks'][:2]:
                t.update(status='running', resource_locks=['db-migration'])
        self.check(mutate, 'share exclusive')

    def test_overlapping_paths(self):
        def mutate(s):
            for t, path in zip(s['tasks'][:2], ['/tmp/owned', '/tmp/owned/a']):
                t.update(status='running', allowed_paths=[path])
        self.check(mutate, 'overlap directory')

if __name__ == '__main__':
    unittest.main()
