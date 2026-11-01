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

    def test_transition_rejects_missing_paths(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            old = {'authorization_path': str(root / 'missing-old-receipt')}
            new = {'authorization_path': str(root / 'missing-new-receipt')}
            evidence = {'old_archive_path': str(root / 'missing-old-archive'),
                        'new_archive_path': str(root / 'missing-new-archive'),
                        'review_path': str(root / 'missing-review')}
            self.assertIs(False, runner._verify_readmission_transition(old, new, evidence, 100))
            self.assertEqual([], list(root.iterdir()))

    def _protocol_receipt(self, reference=None, **overrides):
        now = datetime.datetime.now(datetime.timezone.utc)
        receipt = {
            'receipt_id': 'admission-008-canonical-probe',
            'trusted_user_message_reference': (
                '用户①原话逐字引用：「' + runner.READMISSION_USER_SENTENCE + '」'),
            'reviewer_mode': 'user-self',
            'trust_model': runner.READMISSION_TRUST_MODEL,
            'user_sentence_sha256': runner._readmission_sentence_digest(),
            'w6_02_decisions': {'backend_env': 'local', 'backend_port': 9810,
                                'seed_accounts': 'synthetic',
                                'device_id': 'XWE6R19916004085', 'browser': 'chromium'},
            'plan_manifest_sha256': 'b' * 64,
            'issued_at': (now - datetime.timedelta(minutes=1)).isoformat(),
            'expires_at': (now + datetime.timedelta(days=2)).isoformat(),
            'readmission': {
                'old_manifest_sha256': 'b' * 64, 'new_manifest_sha256': 'b' * 64,
                'task_id': 'W6-01', 'attempt_limit': 8,
                'new_deadlines': {'task': 1793701563, 'wave': 1793960763,
                                  'run': 1794047163},
            },
        }
        receipt.update(overrides)
        if reference is not None:
            receipt['trusted_user_message_reference'] = reference
        raw = runner.canonical_json(receipt).encode('utf-8')
        return receipt, raw

    def test_canonical_authorization_sentence_verifies_user_binding(self):
        _receipt, raw = self._protocol_receipt()
        self.assertIs(True, runner._verify_readmission_user(raw))

    def test_mutated_canonical_sentence_fails_user_binding(self):
        # containment 语义下引用只需完整包含规范句：尾部补空格等"原句+上下文"
        # 属合法引用（verbatim-quote + post-hoc audit 信任模型），不是负例。
        # 真负例是破坏规范句完整性的变异（改数字/截断首字符）。
        for mutated in (runner.READMISSION_USER_SENTENCE.replace('5→8', '5→9'),
                        runner.READMISSION_USER_SENTENCE[:-1]):
            with self.subTest(mutated=mutated):
                _receipt, raw = self._protocol_receipt(reference='「' + mutated + '」')
                self.assertIs(False, runner._verify_readmission_user(raw))

    def test_missing_protocol_fields_fail_user_binding(self):
        for field in ('reviewer_mode', 'trust_model'):
            with self.subTest(field=field):
                receipt, _raw = self._protocol_receipt()
                del receipt[field]
                raw = runner.canonical_json(receipt).encode('utf-8')
                self.assertIs(False, runner._verify_readmission_user(raw))

    def test_review_must_bind_user_sentence_and_mode(self):
        receipt, raw = self._protocol_receipt()
        digest = runner._readmission_sentence_digest()
        scope = receipt['readmission']
        base = ('READMISSION-REVIEW-VERDICT: APPROVED\n'
                'receipt_id: ' + receipt['receipt_id'] + '\n'
                'old_manifest_sha256: ' + scope['old_manifest_sha256'] + '\n'
                'new_manifest_sha256: ' + scope['new_manifest_sha256'] + '\n')
        full = (base + 'reviewer_mode: user-self\n'
                + 'user_sentence_sha256: ' + digest + '\n')
        self.assertIs(True, runner._verify_readmission_review(full.encode(), raw))
        for missing in ('reviewer_mode: user-self\n',
                        'user_sentence_sha256: ' + digest + '\n'):
            with self.subTest(missing=missing.strip()):
                self.assertIs(False, runner._verify_readmission_review(
                    base.encode(), raw))
        wrong_sentence = (base + 'reviewer_mode: user-self\n'
                          + 'user_sentence_sha256: ' + '0' * 64 + '\n')
        self.assertIs(False, runner._verify_readmission_review(
            wrong_sentence.encode(), raw))

    def test_steady_state_accepts_protocol_receipt_from_disk(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            receipt, raw = self._protocol_receipt()
            path = root / 'authorization-admission-008.json'
            path.write_bytes(raw)
            state = {
                'authorization_path': str(path),
                'authorization_sha256': runner.sha256_bytes(raw),
                'reviewed_readmission': runner.sha256_bytes(raw),
                'plan_manifest_sha256': 'b' * 64,
            }
            contract = {'reviewed_readmission': {
                'task_id': 'W6-01', 'attempt_limit': 8,
                'deadlines': receipt['readmission']['new_deadlines']}}
            self.assertIs(True, runner._verify_reviewed_readmission(state, contract))
            path.write_bytes(raw + b' ')
            self.assertIs(False, runner._verify_reviewed_readmission(state, contract))

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
