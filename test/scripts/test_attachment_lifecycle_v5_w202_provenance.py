"""W2-02 logic-only EUnit cannot prove an HTTP entrypoint was exercised."""
import json
from pathlib import Path
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/test"))
import attachment_lifecycle_v5_adapter_w2_02_confirmed_content_immutable as adapter
import attachment_lifecycle_v5_adapter_w2_support as support


class W202ProvenanceTests(unittest.TestCase):
    def test_static_sidecar_cannot_prove_http_request(self):
        leaf = {"entrypoint": "ordinary", "ordering_or_fault": "hash_mismatch_no_confirm"}
        fake = {"entrypoint": "ordinary", "scenario": "hash_mismatch_no_confirm",
                "route": "/api/v1/attachment/confirm", "method": "POST",
                "handler": "attach_handler:confirm",
                "request_id": "w202-ordinary-hash_mismatch_no_confirm"}
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "entrypoint_provenance.jsonl"
            path.write_text(json.dumps(fake) + "\n")
            gaps = support.Gaps()
            self.assertEqual({}, adapter._provenance_summary(gaps, tmp, [leaf]))
            with self.assertRaises(support.AdapterGap):
                gaps.check()
            self.assertEqual(json.dumps(fake) + "\n", path.read_text())

    def test_case_without_http_observation_is_not_accepted(self):
        gaps = support.Gaps()
        self.assertEqual({}, adapter._provenance_summary(gaps, "/nonexistent", []))
        with self.assertRaises(support.AdapterGap):
            gaps.check()


if __name__ == "__main__":
    unittest.main()
