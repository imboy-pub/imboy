import copy
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_readmission as admission
from scripts import attachment_lifecycle_v5_runner as runner
from test.scripts import test_attachment_lifecycle_v5_readmission as fixtures


def logical(state):
    return {key: value for key, value in state.items()
            if key not in ('revision', 'events_head_hash', 'clock_anchor')}


class ReadmissionIntegrationTest(unittest.TestCase):
    def test_one_proposal_validates_then_recovers_its_durable_event(self):
        fixture = fixtures.ReadmissionTest('test_valid_proposal_does_not_modify_inputs')
        fixture.setUp()
        with tempfile.TemporaryDirectory() as tmp, patch.object(runner.time, 'time', return_value=100):
            root = Path(tmp)
            for name, archive in (('old-plan', fixture.old_archive), ('new-plan', fixture.new_archive)):
                plan = root / name
                plan.mkdir()
                (plan / 'manifest.sha256').write_bytes(archive['manifest'])
                for relative, data in archive['files'].items():
                    target = plan / relative
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(data)
            fixture.old['plan_dir'] = str(root / 'old-plan')
            fixture.old['authorization_path'] = str(root / 'old-receipt')
            fixture.new['plan_dir'] = str(root / 'new-plan')
            fixture.new['authorization_path'] = str(root / 'new-receipt')
            store = runner.StateStore(tmp)
            old = store.write_snapshot(fixture.old, 'admit')
            proposed = copy.deepcopy(old)
            proposed.update(copy.deepcopy(fixture.new))
            evidence = {'old_manifest_sha256': fixture.old['plan_manifest_sha256'],
                        'new_manifest_sha256': fixture.new['plan_manifest_sha256'],
                        'old_receipt_sha256': admission._sha(fixture.old_receipt),
                        'new_receipt_sha256': admission._sha(fixture.new_receipt),
                        'review_sha256': admission._sha(fixture.review),
                        'old_archive_path': str(root / 'old-plan'),
                        'new_archive_path': str(root / 'new-plan'),
                        'review_path': str(root / 'review')}

            def validate(old_state, new_state, _evidence, at):
                return admission.validate_readmission(
                    logical(old_state), logical(new_state), fixture.old_archive, fixture.new_archive,
                    fixture.old_receipt, fixture.new_receipt, fixture.review, at,
                    lambda _: True, lambda *_: True)[0]

            self.assertTrue(validate(old, proposed, evidence, 100))
            history = Path(store.events_path).read_bytes()
            with patch.object(runner, '_verify_readmission_transition', side_effect=validate) as verifier, \
                 patch.object(runner, 'candidate_errors', return_value=[]):
                with patch.object(runner.StateStore, 'persist_snapshot', side_effect=OSError('crash')):
                    with self.assertRaises(OSError):
                        runner._write_reviewed_readmission(tmp, proposed, evidence)
                event_history = Path(store.events_path).read_bytes()
                self.assertTrue(event_history.startswith(history))
                recovered = runner._resume_state(tmp)
                self.assertEqual(2, verifier.call_count)
            self.assertEqual(logical(proposed), logical(recovered))
            self.assertEqual(fixture.old['verification_candidate']['heads'], recovered['verification_candidate']['heads'])
            self.assertEqual(fixture.old['gate_candidates'], recovered['gate_candidates'])
            self.assertEqual(fixture.old['case_manifest_digests'], recovered['case_manifest_digests'])
            self.assertEqual(5, recovered['tasks']['W6-01']['attempt'])
            self.assertEqual(event_history, Path(store.events_path).read_bytes())


if __name__ == '__main__':
    unittest.main()
