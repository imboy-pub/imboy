import json
from pathlib import Path
import tempfile
import unittest

from scripts.test import attachment_lifecycle_v5_adapter_w2_support as support


class HistoricalProbeEvidenceTest(unittest.TestCase):
    def test_fabricated_http_metadata_and_green_summary_cannot_pass(self):
        with tempfile.TemporaryDirectory() as temp:
            row = {"name": "failclosed_insert_error", "entrypoint": "ordinary",
                   "scenario": "insert_error", "ac": "AC01", "pass": True,
                   "entrypoint_provenance": {"route": "/api/v1/attachment/presign",
                                             "method": "GET", "handler": "attach_handler",
                                             "request_id": "w201-ordinary-failclosed_insert_error"}}
            lines = [row, {"phase": "ordinary", "ok": 1, "bad": 0}]
            Path(temp, "ordinary.jsonl").write_text(
                "".join(json.dumps(line) + "\n" for line in lines))
            with self.assertRaisesRegex(support.AdapterGap, "cannot establish real HTTP"):
                support.load_probe_section(temp, "ordinary")


if __name__ == "__main__":
    unittest.main()
