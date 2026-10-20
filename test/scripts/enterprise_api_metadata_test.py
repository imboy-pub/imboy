"""Regress semantic drift that previously passed the route-count gate."""
import importlib.util
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("manifest_gate", ROOT / "scripts/check_enterprise_release_manifest.py")
GATE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GATE)


class MetadataGateTest(unittest.TestCase):
    def setUp(self):
        self.inputs = GATE.contract_metadata(ROOT, ROOT / "api/internal/v1/manifest.yaml")

    def test_current_candidate(self):
        self.assertEqual([], GATE.metadata_errors(*self.inputs))

    def test_scope_drift_rejected(self):
        registry, declared, *rest = self.inputs
        row = declared["INT-18"]
        declared["INT-18"] = (*row[:2], "groups:write", *row[3:])
        self.assertTrue(GATE.metadata_errors(registry, declared, *rest))

    def test_missing_sync_request_rejected(self):
        *metadata, requests, schemas = self.inputs
        requests.remove(("POST", "/api/internal/v1/directory/sync"))
        self.assertTrue(GATE.metadata_errors(*metadata, requests, schemas))

    def test_legacy_nullable_rejected(self):
        *metadata, schemas = self.inputs
        schemas["sso"]["properties"]["organization_id"] = {
            "type": "integer", "format": "int64", "nullable": True}
        self.assertTrue(GATE.metadata_errors(*metadata, schemas))


if __name__ == "__main__":
    unittest.main()
