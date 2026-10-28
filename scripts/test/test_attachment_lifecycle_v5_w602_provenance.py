import json
from pathlib import Path
import subprocess
import tempfile
import unittest

from scripts.test import attachment_lifecycle_v5_adapter_w6_02_support as support


ROOT = Path(__file__).resolve().parents[2]


class W602ProvenanceTest(unittest.TestCase):
    def test_producer_copies_real_fields_and_missing_producer_fails_closed(self):
        fields = ["device_provenance", "browser_provenance"]
        producer = ROOT / "scripts/attachment_lifecycle_v5_producer.py"
        self.assertEqual([], support.producer_drops(str(producer), fields, "W6-02/probe"))
        gaps = support.provenance_pipeline_gaps(str(ROOT), "W6-02/probe", True, True)
        self.assertTrue(any("per-driver session and time binding" in gap for gap in gaps))
        with tempfile.TemporaryDirectory() as tmp:
            fake = Path(tmp) / "fake.py"
            fake.write_text("raise SystemExit(0)\n")
            self.assertEqual(fields, support.producer_drops(str(fake), fields, "W6-02/probe"))

    def test_no_host_pid_fallback_and_no_stale_provenance(self):
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        functions = source[source.index("w602_device_pid() {"):source.index("w602_case() {")]
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "node_modules/.bin").mkdir(parents=True)
            playwright = root / "node_modules/.bin/playwright"
            playwright.write_text("#!/bin/sh\nexit 0\n")
            playwright.chmod(0o755)
            flutter = root / "flutter"
            flutter.write_text("#!/bin/sh\nexit 0\n")
            flutter.chmod(0o755)
            for name in ("device-provenance.json", "browser-provenance.json"):
                (root / name).write_text(json.dumps({"old": True}))
            script = f'''set -u
PATH="$1:$PATH"
{functions}
w602_run_flutter "$1" "$1" disk no-such-device; a=$?
w602_run_admin "$1" "$1" disk; b=$?
[ "$a" -eq 75 ] && [ "$b" -eq 75 ]
'''
            result = subprocess.run(["bash", "-c", script, "probe", tmp],
                                    capture_output=True, text=True, timeout=15)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertFalse((root / "device-provenance.json").exists())
            self.assertFalse((root / "browser-provenance.json").exists())

    def test_gate_refuses_unowned_backend_before_external_calls(self):
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        case = source[source.index("w602_case() {"):source.index("# ---------------- W6-03 executors")]
        with tempfile.TemporaryDirectory() as tmp:
            script = case + '\nw602_case W6-02/ac08 "$1"; rc=$?; [ "$rc" -eq 75 ]\n'
            result = subprocess.run(["bash", "-c", script, "probe", tmp],
                                    capture_output=True, text=True, timeout=5)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertEqual([], list(Path(tmp).iterdir()))


if __name__ == "__main__":
    unittest.main()
