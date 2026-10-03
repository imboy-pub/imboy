"""Negative tests for the L4 SNI completion gate."""
import csv
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SCRIPT = Path(__file__).resolve().parents[1] / 'verify_l4_sni_acceptance.py'
SPEC = importlib.util.spec_from_file_location('gate', SCRIPT)
gate = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(gate)


class CompletionGateTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / 'control').mkdir()
        self.plan = self.root / 'plan.md'
        self.plan.write_text('frozen plan')
        self.contract = {'plan_sha256': gate.digest(self.plan),
                         'acceptance_ids': sorted(gate.EXPECTED_IDS),
                         'backend_base_sha': 'a' * 40, 'app_base_sha': 'a' * 40}
        self.rows = []
        for ident in sorted(gate.EXPECTED_IDS):
            row = dict.fromkeys(gate.FIELDS, '')
            row.update(acceptance_id=ident, required='no' if ident == 'L4-T7-PREP'
                       else 'yes', status='PENDING', note='not executed')
            self.rows.append(row)

    def save(self):
        with (self.root / 'control/acceptance.tsv').open('w') as stream:
            writer = csv.DictWriter(stream, gate.FIELDS, delimiter='\t')
            writer.writeheader()
            writer.writerows(self.rows)
        (self.root / 'control/acceptance-contract.json').write_text(
            json.dumps(self.contract))
        pending = [r['acceptance_id'] for r in self.rows
                   if r['required'] == 'yes' and r['status'] != 'PASS']
        (self.root / 'control/state.json').write_text(json.dumps({
            'plan_status': 'PARTIAL' if pending else 'PASS', 'pending': pending,
            'acceptance_statuses': {r['acceptance_id']: r['status'] for r in self.rows}}))

    def verify(self):
        self.save()
        with patch.object(gate, 'git', return_value='b' * 40):
            return gate.verify(self.root, self.root, self.root, self.plan)

    def test_pending_is_integral_but_incomplete(self):
        self.assertEqual(self.verify()['plan_status'], 'PARTIAL')

    def test_required_cannot_be_demoted(self):
        self.rows[0]['required'] = 'no'
        with self.assertRaisesRegex(ValueError, 'required contract changed'):
            self.verify()

    def test_removing_id_from_contract_and_ledger_fails(self):
        removed = self.rows.pop()['acceptance_id']
        self.contract['acceptance_ids'].remove(removed)
        with self.assertRaisesRegex(ValueError, 'acceptance IDs'):
            self.verify()

    def test_duplicate_id_fails(self):
        self.rows.append(self.rows[0].copy())
        with self.assertRaisesRegex(ValueError, 'acceptance IDs'):
            self.verify()

    def test_old_candidate_fails(self):
        self.rows[0].update(status='PASS', backend_candidate_sha='a' * 40)
        with self.assertRaisesRegex(ValueError, 'not current HEAD'):
            self.verify()

    def test_ledger_width_fails(self):
        self.save()
        with (self.root / 'control/acceptance.tsv').open('a') as stream:
            stream.write('short\trow\n')
        with self.assertRaisesRegex(ValueError, '13 columns'):
            gate.read_rows(self.root / 'control/acceptance.tsv')

    def pass_fixture(self):
        row = self.rows[0]
        row.update(status='PASS', backend_candidate_sha='b' * 40,
                   app_candidate_sha='b' * 40, command='check', exit_code='0',
                   oracle='all passed', timestamp='2026-10-03T00:00:00Z',
                   evidence_path='result.json')
        log = self.root / 'raw.log'
        log.write_text('all passed')
        record = {k: row[k] for k in ('acceptance_id', 'backend_candidate_sha',
                  'app_candidate_sha', 'command', 'oracle', 'timestamp')}
        record.update(exit_code=0, oracle_pass=True, skipped=False,
                      plan_sha256=self.contract['plan_sha256'], log_path='raw.log',
                      log_sha256=gate.digest(log))
        return row, record

    def save_record(self, row, record):
        path = self.root / 'result.json'
        path.write_text(json.dumps(record))
        row['evidence_sha256'] = gate.digest(path)

    def test_valid_pass_record(self):
        row, record = self.pass_fixture()
        self.save_record(row, record)
        with patch.object(gate.subprocess, 'run'):
            self.assertNotIn(row['acceptance_id'], self.verify()['pending'])

    def test_record_mismatch_and_failed_results(self):
        row, original = self.pass_fixture()
        mutations = [('command', 'other', 'binding mismatch'),
                     ('oracle_pass', False, 'not PASS'),
                     ('skipped', True, 'not PASS'),
                     ('exit_code', 1, 'not PASS'),
                     ('plan_sha256', 'c' * 64, 'not PASS'),
                     ('log_sha256', 'c' * 64, 'log hash mismatch'),
                     ('log_path', '../escape', 'log missing')]
        for field, value, diagnostic in mutations:
            with self.subTest(field=field):
                record = dict(original, **{field: value})
                self.save_record(row, record)
                with patch.object(gate.subprocess, 'run'):
                    with self.assertRaisesRegex(ValueError, diagnostic):
                        self.verify()

    def test_tampered_record_rejected(self):
        row, record = self.pass_fixture()
        self.save_record(row, record)
        (self.root / 'result.json').write_text('{}')
        with patch.object(gate.subprocess, 'run'):
            with self.assertRaisesRegex(ValueError, 'evidence hash mismatch'):
                self.verify()


if __name__ == '__main__':
    unittest.main()
