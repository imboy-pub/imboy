"""SQL pin probes cannot establish that an HTTP request was exercised."""
import json
from pathlib import Path
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/test"))
import attachment_lifecycle_v5_adapter_w2_01_upload_content_pin as adapter
import attachment_lifecycle_v5_adapter_w2_support as support


class W201ProvenanceTests(unittest.TestCase):
    def test_fake_sidecars_cannot_prove_surface_requests(self):
        leaves = [{"entrypoint": entry, "ordering_or_fault": "first_content_pin"}
                  for entry in ("ordinary", "enterprise_internal", "enterprise_seat", "cs_widget")]
        fake = [{"entrypoint": leaf["entrypoint"], "scenario": leaf["ordering_or_fault"],
                 "route": "/plausible/upload", "method": "POST", "handler": "handler",
                 "request_id": "synthetic-" + leaf["entrypoint"]} for leaf in leaves]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "entrypoint_provenance.jsonl"
            original = "".join(json.dumps(row) + "\n" for row in fake)
            path.write_text(original)
            (Path(tmp) / "raw.log").write_text("ENTRYPOINT_PROVENANCE " + json.dumps(fake[0]) + "\n")
            gaps = support.Gaps()
            self.assertEqual({}, adapter._provenance_summary(gaps, tmp, leaves))
            with self.assertRaises(support.AdapterGap):
                gaps.check()
            self.assertEqual(original, path.read_text())

    def test_missing_observation_remains_a_gap(self):
        gaps = support.Gaps()
        self.assertEqual({}, adapter._provenance_summary(gaps, "/nonexistent", []))
        with self.assertRaises(support.AdapterGap):
            gaps.check()


if __name__ == "__main__":
    unittest.main()
