import copy
import datetime
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_runner as runner


class ReviewedBudgetTest(unittest.TestCase):
    """Verifier mocks isolate budget logic; they do not prove source authentication."""

    def setUp(self):
        self.contract = {
            'tasks': [{'task_id': 'W6-01', 'wave': 'W6'}, {'task_id': 'W6-02', 'wave': 'W6'}],
            'timeouts': {'run_seconds': 20, 'wave_seconds': 10,
                         'task_seconds': 5, 'task_overrides': {'W6-01': 5}},
            'retry_policy': {'task_attempts_total_max': 5, 'task_attempts_overrides': {'W6-01': 8}},
            'reviewed_readmission': {'task_id': 'W6-01', 'attempt_limit': 8,
                                     'deadlines': {'task': 110, 'wave': 120, 'run': 130}},
        }
        self.state = {
            'reviewed_readmission': 'a' * 64,
            'tasks': {'W6-01': {'task_started_at': 1, 'task_deadline': 110, 'attempt': 5},
                      'W6-02': {'task_started_at': 1, 'task_deadline': 110, 'attempt': 5}},
            'time_budgets': {'run': {'started_at': 1, 'deadline': 130},
                             'waves': {'W6': {'started_at': 1, 'deadline': 120},
                                       'W5': {'started_at': 1, 'deadline': 11}}},
        }

    def test_missing_provenance_fails_closed_even_for_non_enforcing_check(self):
        with self.assertRaises(SystemExit) as err:
            runner._budget_check(self.state, self.contract, enforce=False)
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)
        with self.assertRaises(SystemExit) as err:
            runner._task_attempt_limit(self.state, self.contract, 'W6-01')
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)

    def _self_authored_receipt(self, reference):
        now = datetime.datetime.now(datetime.timezone.utc)
        scope = self.contract['reviewed_readmission']
        receipt = {
            'receipt_id': 'synthetic-self-authored',
            'trusted_user_message_reference': reference,
            'w6_02_decisions': {'seed_accounts': 'synthetic'},
            'plan_manifest_sha256': 'b' * 64,
            'issued_at': (now - datetime.timedelta(minutes=1)).isoformat(),
            'expires_at': (now + datetime.timedelta(minutes=1)).isoformat(),
            'readmission': {
                'old_manifest_sha256': 'c' * 64,
                'new_manifest_sha256': 'b' * 64,
                'task_id': scope['task_id'], 'attempt_limit': scope['attempt_limit'],
                'new_deadlines': copy.deepcopy(scope['deadlines']),
            },
        }
        review = ('READMISSION-REVIEW-VERDICT: APPROVED\n'
                  'receipt_id: synthetic-self-authored\n'
                  'old_manifest_sha256: ' + 'c' * 64 + '\n'
                  'new_manifest_sha256: ' + 'b' * 64 + '\n').encode()
        receipt['readmission']['review_sha256'] = runner.sha256_bytes(review)
        return receipt, review

    def _bind_self_authored_receipt(self, root):
        receipt, _review = self._self_authored_receipt('继续收口任务计划，所有任务都pass')
        raw = runner.canonical_json(receipt).encode('utf-8')
        path = root / 'synthetic-receipt.json'
        path.write_bytes(raw)
        digest = runner.sha256_bytes(raw)
        self.state.update(authorization_path=str(path), authorization_sha256=digest,
                          reviewed_readmission=digest,
                          plan_manifest_sha256=receipt['plan_manifest_sha256'])
        return raw

    def test_fixed_user_sentence_and_negated_quote_are_not_trusted_sources(self):
        for reference in ('继续收口任务计划，所有任务都pass',
                          '我不同意授权；引用“继续收口任务计划，所有任务都pass”仅用于说明，不代表授权。'):
            with self.subTest(reference=reference):
                receipt, _review = self._self_authored_receipt(reference)
                raw = runner.canonical_json(receipt).encode('utf-8')
                self.assertIs(False, runner._verify_readmission_user(raw))

    def test_approved_template_and_self_claimed_review_are_not_independent_sources(self):
        receipt, template = self._self_authored_receipt('继续收口任务计划，所有任务都pass')
        for review in (template, template + b'independent_reviewer: self-claimed\n'):
            with self.subTest(review=review):
                receipt['readmission']['review_sha256'] = runner.sha256_bytes(review)
                raw = runner.canonical_json(receipt).encode('utf-8')
                self.assertIs(False, runner._verify_readmission_review(review, raw))

    def test_digest_consistent_state_and_receipt_do_not_authenticate_provenance(self):
        with tempfile.TemporaryDirectory() as tmp:
            raw = self._bind_self_authored_receipt(Path(tmp))
            self.assertEqual(runner.sha256_bytes(raw), self.state['authorization_sha256'])
            self.assertEqual(self.state['authorization_sha256'], self.state['reviewed_readmission'])
            self.assertIs(False, runner._verify_reviewed_readmission(self.state, self.contract))

    def test_digest_consistent_receipt_still_blocks_non_enforcing_budget_check(self):
        with tempfile.TemporaryDirectory() as tmp:
            self._bind_self_authored_receipt(Path(tmp))
            with self.assertRaises(SystemExit) as err:
                runner._budget_check(self.state, self.contract, enforce=False)
            self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)

    def test_transition_rejects_fake_paths_without_reading_receipts_or_archives(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            old = {'authorization_path': str(root / 'missing-old-receipt')}
            new = {'authorization_path': str(root / 'missing-new-receipt')}
            evidence = {'old_archive_path': str(root / 'missing-old-archive'),
                        'new_archive_path': str(root / 'missing-new-archive'),
                        'review_path': str(root / 'missing-review')}
            with patch.object(runner, '_receipt_bytes', side_effect=AssertionError('receipt read')) as receipt_read, \
                 patch.object(runner, '_load_archive_dir', side_effect=AssertionError('archive read')) as archive_read:
                self.assertIs(False, runner._verify_readmission_transition(old, new, evidence, 100))
                receipt_read.assert_not_called()
                archive_read.assert_not_called()

    def test_archive_reads_regular_nested_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'nested').mkdir()
            files = {'execution-contract.json': b'{}', 'nested/proof': b'synthetic'}
            for name, data in files.items():
                (root / name).write_bytes(data)
            manifest = ''.join(runner.sha256_bytes(data) + '  ' + name + '\n'
                               for name, data in files.items()).encode()
            (root / 'manifest.sha256').write_bytes(manifest)
            self.assertEqual({'manifest': manifest, 'files': files},
                             runner._load_archive_dir(str(root)))

    def test_archive_validates_all_entries_before_opening_members(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            digest = runner.sha256_bytes(b'{}')
            valid = digest + '  execution-contract.json\n'
            for name in ('../outside', '/outside', 'nested/../outside',
                         './outside', 'nested//outside', 'execution-contract.json'):
                with self.subTest(name=name):
                    (root / 'manifest.sha256').write_text(valid + digest + '  ' + name + '\n')
                    with patch.object(runner, '_read_run_artifact',
                                      wraps=runner._read_run_artifact) as read:
                        with self.assertRaises(ValueError):
                            runner._load_archive_dir(str(root))
                        self.assertEqual(1, read.call_count)

    def test_archive_rejects_symlinks_and_fifo_without_reading_content(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'regular').write_bytes(b'{}')
            member = root / 'execution-contract.json'
            manifest = root / 'manifest.sha256'
            manifest.write_text(runner.sha256_bytes(b'{}') + '  execution-contract.json\n')
            member.symlink_to(root / 'regular')
            with self.assertRaises((OSError, ValueError)):
                runner._load_archive_dir(str(root))
            member.unlink()
            os.mkfifo(member)
            with patch.object(runner.os, 'fdopen', wraps=os.fdopen) as read:
                # The ordinary manifest is read first, then FIFO content
                # must be rejected by fstat without entering fdopen.
                with self.assertRaises((OSError, ValueError)):
                    runner._load_archive_dir(str(root))
                self.assertEqual(1, read.call_count)
            member.unlink()
            (root / 'target').mkdir()
            # The link targets contain a fully valid manifest and members so
            # that a wrongly-followed link would parse and read successfully;
            # rejection must come from link refusal, not from parse errors.
            valid_manifest = (runner.sha256_bytes(b'{}') + '  execution-contract.json\n'
                              + runner.sha256_bytes(b'p') + '  linked/proof\n')
            (root / 'target/manifest.sha256').write_text(valid_manifest)
            (root / 'target/execution-contract.json').write_bytes(b'{}')
            (root / 'target/linked').mkdir()
            (root / 'target/linked/proof').write_bytes(b'p')
            (root / 'linked').symlink_to(root / 'target', target_is_directory=True)
            member.write_bytes(b'{}')
            (root / 'proof').write_bytes(b'p')
            # Directory symlink 'linked' (manifest names linked/proof).
            manifest.write_text(valid_manifest)
            with self.assertRaises((OSError, ValueError)):
                runner._load_archive_dir(str(root))
            manifest.unlink()
            # Manifest symlink whose target holds the fully valid manifest:
            # a wrongly-followed link would read members successfully, so
            # zero fdopen calls proves refusal happened at open time.
            manifest.symlink_to(root / 'target/manifest.sha256')
            with patch.object(runner.os, 'fdopen', wraps=os.fdopen) as read:
                with self.assertRaises((OSError, ValueError)):
                    runner._load_archive_dir(str(root))
                read.assert_not_called()
            manifest.unlink()
            os.mkfifo(manifest)
            with patch.object(runner.os, 'fdopen', wraps=os.fdopen) as read:
                with self.assertRaises((OSError, ValueError)):
                    runner._load_archive_dir(str(root))
                read.assert_not_called()

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_reviewed_limits_are_task_specific_and_exhaustion_is_preserved(self, _verify):
        with patch.object(runner.time, 'time', return_value=100):
            runner._budget_check(self.state, self.contract, 'W6-01')
            self.assertEqual(5, runner._task_attempt_limit(self.state, self.contract, 'W6-02'))
            for attempt in (5, 6, 7, 8):
                self.state['tasks']['W6-01']['attempt'] = attempt
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.state['tasks']['W6-01']['attempt'] = 9
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)
            self.state['tasks']['W6-02']['attempt'] = 6
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-02')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_deadline_and_scope_tampering_fails(self, _verify):
        for mutation in (
                lambda s, c: s['time_budgets']['run'].update(deadline=131),
                lambda s, c: s['time_budgets']['waves']['W6'].update(deadline=121),
                lambda s, c: s['time_budgets']['waves']['W5'].update(deadline=12),
                lambda s, c: s['tasks']['W6-01'].update(task_deadline=111),
                lambda s, c: c['retry_policy']['task_attempts_overrides'].update({'W6-02': 8}),
                lambda s, c: c['reviewed_readmission'].update(attempt_limit=9),
                lambda s, c: s.update(reviewed_readmission='invalid')):
            with self.subTest(mutation=mutation):
                state, contract = copy.deepcopy(self.state), copy.deepcopy(self.contract)
                mutation(state, contract)
                with self.assertRaises(SystemExit) as err:
                    runner._budget_check(state, contract, enforce=False)
                self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_expired_reviewed_deadlines_remain_enforced(self, _verify):
        with patch.object(runner.time, 'time', return_value=120):
            with self.assertRaises(SystemExit) as err:
                runner._budget_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)
        with patch.object(runner.time, 'time', return_value=110):
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)


if __name__ == '__main__':
    unittest.main()
