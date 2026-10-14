"""Exercise the real static gate against malformed manifest structure and drift."""
import copy
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import yaml

ROOT = Path(__file__).resolve().parents[2]


class ManifestTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / 'manifest.yaml'
        self.document = yaml.safe_load((ROOT / 'api/internal/v1/manifest.yaml').read_text())

    def run_gate(self, expected, raw=None):
        self.path.write_text(raw if raw is not None else yaml.safe_dump(self.document, sort_keys=False))
        result = subprocess.run(
            [sys.executable, '-B', str(ROOT / 'scripts/check_enterprise_release_manifest.py'),
             '--repo', str(ROOT), '--manifest', str(self.path)],
            capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        self.assertIn('PASS 8)' if expected == 0 else 'FAIL 8)', result.stdout)
        self.assertNotIn('Traceback', result.stderr)

    def test_current_manifest(self):
        self.run_gate(0)

    def test_routes_misplaced_under_assertions(self):
        moved = self.document['routes'][15:]
        self.document['routes'] = self.document['routes'][:15]
        self.document['mechanical_assertions'].extend(moved)
        self.assertEqual(len(moved), 29)
        self.run_gate(1)

    def test_missing_route(self):
        self.document['routes'].pop()
        self.run_gate(1)

    def test_duplicate_id(self):
        self.document['routes'].append(copy.deepcopy(self.document['routes'][0]))
        self.run_gate(1)

    def test_method_drift_with_same_ids(self):
        self.document['routes'][0]['method'] = 'DELETE'
        self.run_gate(1)

    def test_path_drift_with_same_ids(self):
        self.document['routes'][0]['path'] += '/wrong'
        self.run_gate(1)

    def test_routes_must_be_array(self):
        self.document['routes'] = {'INT-01': self.document['routes'][0]}
        self.run_gate(1)

    def test_non_string_route_field(self):
        self.document['routes'][0]['method'] = 123
        self.run_gate(1)

    def test_human_counterpart_cannot_hide_internal_route(self):
        self.document['routes'][0]['id'] = 'HUMAN-SSO-01'
        self.run_gate(1)

    def test_duplicate_mapping_field_rejected(self):
        raw = yaml.safe_dump(self.document, sort_keys=False)
        self.run_gate(1, raw.replace('  method: GET', '  method: DELETE\n  method: GET', 1))

    def test_duplicate_top_level_routes_rejected(self):
        raw = yaml.safe_dump(self.document, sort_keys=False)
        self.run_gate(1, 'routes: []\n' + raw)


if __name__ == '__main__':
    unittest.main()
