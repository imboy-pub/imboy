import copy
import json
from pathlib import Path
import tempfile
import time
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_runner as runner


class ReadmissionEventTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        plan = self.root / 'old-plan'
        plan.mkdir()
        contract = {'tasks': [{'task_id': 'W6-01', 'wave': 'W6'}],
                    'timeouts': {'task_seconds': 5, 'wave_seconds': 10, 'run_seconds': 20}}
        contract_file = plan / 'execution-contract.json'
        contract_file.write_text(json.dumps(contract))
        manifest = plan / 'manifest.sha256'
        manifest.write_text(runner.sha256_file(contract_file) + '  execution-contract.json\n')
        old_hash = runner.sha256_file(manifest)
        now = time.time()
        self.store = runner.StateStore(str(self.root))
        self.old = self.store.write_snapshot({
            'revision': 0, 'plan_dir': str(plan), 'plan_manifest_sha256': old_hash,
            'authorization_path': str(self.root / 'old-auth'), 'authorization_sha256': 'a' * 64,
            'verification_candidate': {'plan_manifest_sha256': old_hash},
            'tasks': {'W6-01': {'state': 'RUNNING', 'attempt': 5,
                                'task_started_at': now - 100, 'task_deadline': now - 95}},
            'time_budgets': {'run': {'started_at': now - 100, 'deadline': now - 80},
                             'waves': {'W6': {'started_at': now - 100, 'deadline': now - 90}}},
            'gates': {'G5': 'PASS'}, 'case_manifest_digests': {'old': 'preserved'},
        }, 'admit')
        self.proposed = copy.deepcopy(self.old)
        self.proposed.update(plan_dir=str(self.root / 'new-plan'), plan_manifest_sha256='b' * 64,
                             authorization_path=str(self.root / 'new-auth'),
                             authorization_sha256='c' * 64, reviewed_readmission='c' * 64)
        self.proposed['verification_candidate']['plan_manifest_sha256'] = 'b' * 64
        self.proposed['tasks']['W6-01']['task_deadline'] = now + 100
        self.proposed['time_budgets']['waves']['W6']['deadline'] = now + 200
        self.proposed['time_budgets']['run']['deadline'] = now + 300
        self.evidence = {'old_manifest_sha256': old_hash, 'new_manifest_sha256': 'b' * 64,
                         'old_receipt_sha256': 'a' * 64, 'new_receipt_sha256': 'c' * 64,
                         'review_sha256': 'd' * 64, 'old_archive_path': str(self.root / 'old-archive'),
                         'new_archive_path': str(self.root / 'new-archive'),
                         'review_path': str(self.root / 'review')}

    def test_real_verifier_unavailable_blocks_without_event(self):
        state, events = Path(self.store.state_path).read_bytes(), Path(self.store.events_path).read_bytes()
        with self.assertRaises(SystemExit) as err:
            runner._write_reviewed_readmission(str(self.root), self.proposed, self.evidence)
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)
        self.assertEqual(state, Path(self.store.state_path).read_bytes())
        self.assertEqual(events, Path(self.store.events_path).read_bytes())

    @patch.object(runner, 'candidate_errors', return_value=[])
    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_crash_after_event_recovers_once_without_rewriting_history(self, verifier, _candidate):
        original_events = Path(self.store.events_path).read_bytes()
        with patch.object(runner.StateStore, 'persist_snapshot', side_effect=OSError('simulated crash')):
            with self.assertRaises(OSError):
                runner._write_reviewed_readmission(str(self.root), self.proposed, self.evidence)
        self.assertEqual(self.old, self.store.load_state())
        events = Path(self.store.events_path).read_bytes()
        self.assertTrue(events.startswith(original_events))
        self.assertEqual('reviewed_readmission', self.store.load_events()[-1]['kind'])
        recovered = runner._resume_state(str(self.root))
        self.assertEqual(5, recovered['tasks']['W6-01']['attempt'])
        self.assertEqual(self.old['gates'], recovered['gates'])
        self.assertEqual(self.old['case_manifest_digests'], recovered['case_manifest_digests'])
        self.assertEqual(events, Path(self.store.events_path).read_bytes())
        self.assertEqual(2, verifier.call_count)

    @patch.object(runner, 'candidate_errors', return_value=[])
    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_snapshot_anchors_cannot_be_supplied_or_removed(self, _verifier, _candidate):
        changes = (
            lambda p: p.update(revision=999),
            lambda p: p.pop('revision'),
            lambda p: p.update(events_head_hash='e' * 64),
            lambda p: p.pop('events_head_hash'),
            lambda p: p['clock_anchor'].update(wall=p['clock_anchor']['wall'] - 1),
            lambda p: p.pop('clock_anchor'),
        )
        for change in changes:
            with self.subTest(change=change):
                proposed = copy.deepcopy(self.proposed)
                change(proposed)
                state, events = Path(self.store.state_path).read_bytes(), Path(self.store.events_path).read_bytes()
                with self.assertRaises(SystemExit) as err:
                    runner._write_reviewed_readmission(str(self.root), proposed, self.evidence)
                self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)
                self.assertEqual(state, Path(self.store.state_path).read_bytes())
                self.assertEqual(events, Path(self.store.events_path).read_bytes())

    @patch.object(runner, 'candidate_errors', return_value=[])
    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_unrelated_state_change_is_rejected_before_event(self, _verifier, _candidate):
        for mutate in (lambda p: p['tasks']['W6-01'].update(attempt=0),
                       lambda p: p['gates'].update(G5='NOT_STARTED'),
                       lambda p: p['case_manifest_digests'].clear(),
                       lambda p: p['verification_candidate'].update(heads={'x': 'changed'})):
            with self.subTest(mutate=mutate):
                proposed = copy.deepcopy(self.proposed)
                mutate(proposed)
                before = Path(self.store.events_path).read_bytes()
                with self.assertRaises(SystemExit) as err:
                    runner._write_reviewed_readmission(str(self.root), proposed, self.evidence)
                self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)
                self.assertEqual(before, Path(self.store.events_path).read_bytes())

    @patch.object(runner, 'candidate_errors', return_value=[])
    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_recovery_rechecks_verifier_and_never_writes_on_refusal(self, _verifier, _candidate):
        with patch.object(runner.StateStore, 'persist_snapshot', side_effect=OSError('simulated crash')):
            with self.assertRaises(OSError):
                runner._write_reviewed_readmission(str(self.root), self.proposed, self.evidence)
        state, events = Path(self.store.state_path).read_bytes(), Path(self.store.events_path).read_bytes()
        with patch.object(runner, '_verify_readmission_transition', return_value=False):
            with self.assertRaises(SystemExit) as err:
                runner._resume_state(str(self.root))
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)
        self.assertEqual(state, Path(self.store.state_path).read_bytes())
        self.assertEqual(events, Path(self.store.events_path).read_bytes())

    @patch.object(runner, 'candidate_errors', return_value=[])
    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_recovery_rejects_forged_but_hash_valid_state(self, _verifier, _candidate):
        forged = copy.deepcopy(self.proposed)
        forged['tasks']['W6-01']['attempt'] = 0
        with patch.object(runner.StateStore, 'persist_snapshot', side_effect=OSError('simulated crash')):
            with self.assertRaises(OSError):
                self.store.write_snapshot(forged, 'reviewed_readmission', {'evidence': self.evidence})
        state, events = Path(self.store.state_path).read_bytes(), Path(self.store.events_path).read_bytes()
        with self.assertRaises(SystemExit) as err:
            runner._resume_state(str(self.root))
        self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)
        self.assertEqual(state, Path(self.store.state_path).read_bytes())
        self.assertEqual(events, Path(self.store.events_path).read_bytes())

    @patch.object(runner, '_verify_readmission_transition', return_value=True)
    def test_prior_readmission_is_not_repeated(self, _verifier):
        previous = copy.deepcopy(self.old)
        previous['reviewed_readmission'] = 'c' * 64
        with self.assertRaises(SystemExit) as err:
            runner._check_readmission_transition(previous, self.proposed, self.evidence, time.time())
        self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)


if __name__ == '__main__':
    unittest.main()
