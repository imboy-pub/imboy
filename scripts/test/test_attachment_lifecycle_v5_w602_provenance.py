import datetime
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

from scripts.test import attachment_lifecycle_v5_adapter_w6_02_support as support
from scripts.test import attachment_lifecycle_v5_adapter_w6_support as w6_support


ROOT = Path(__file__).resolve().parents[2]


class W602ProvenanceTest(unittest.TestCase):
    def test_producer_copies_real_fields_and_missing_producer_fails_closed(self):
        fields = ["entrypoint_provenance", "device_provenance",
                  "browser_provenance", "backend_provenance"]
        producer = ROOT / "scripts/attachment_lifecycle_v5_producer.py"
        self.assertEqual([], support.producer_drops(str(producer), fields, "W6-02/probe"))
        # The former unconditional gap ("per-driver session and time
        # binding") is gone: a healthy pipeline reports no gap, and the
        # binding itself is verified from observed_at (tests below).
        self.assertEqual([], support.provenance_pipeline_gaps(str(ROOT),
                                                              "W6-02/probe",
                                                              True, True))
        with tempfile.TemporaryDirectory() as tmp:
            fake = Path(tmp) / "fake.py"
            fake.write_text("raise SystemExit(0)\n")
            self.assertEqual(fields, support.producer_drops(str(fake), fields, "W6-02/probe"))
            gaps = support.provenance_pipeline_gaps(str(tmp), "W6-02/probe",
                                                    True, True)
            self.assertEqual(1, len(gaps))
            self.assertIn("never copies", gaps[0])

    def test_provenance_window_binding_accepts_in_session_rejects_outside(self):
        started = datetime.datetime(2026, 11, 1, 10, 0, 0,
                                    tzinfo=datetime.timezone.utc)
        finished = datetime.datetime(2026, 11, 1, 10, 8, 0,
                                     tzinfo=datetime.timezone.utc)
        windows = {"flutter": [(started, finished)]}
        device = {"kind": "physical", "resource_id": "XWE6R19916004085",
                  "process_id": 4242,
                  "observed_at": int(started.timestamp()) + 60}
        # An observation captured during the measured run is bound: no gap.
        self.assertEqual([], support.provenance_window_gaps(device, None, windows))
        # Within the tolerance margin around the window is still a binding.
        late = dict(device, observed_at=int(finished.timestamp()) + 60)
        self.assertEqual([], support.provenance_window_gaps(late, None, windows))
        # A record captured before/after the whole session (another attempt,
        # a stale file) is rejected fail-closed.
        stale = dict(device, observed_at=int(started.timestamp())
                     - support.PROVENANCE_WINDOW_TOLERANCE_S - 1)
        gaps = support.provenance_window_gaps(stale, None, windows)
        self.assertEqual(1, len(gaps))
        self.assertIn("outside this case's measured flutter suite window",
                      gaps[0])
        future = dict(device, observed_at=int(finished.timestamp())
                      + support.PROVENANCE_WINDOW_TOLERANCE_S + 1)
        gaps = support.provenance_window_gaps(future, None, windows)
        self.assertEqual(1, len(gaps))
        # A record without a usable observed_at never passes.
        naked = {key: value for key, value in device.items()
                 if key != "observed_at"}
        gaps = support.provenance_window_gaps(naked, None, windows)
        self.assertEqual(1, len(gaps))
        self.assertIn("no usable observed_at", gaps[0])
        # Browser provenance binds to the playwright windows of the same
        # case; device provenance to the flutter windows.
        browser = {"kind": "real-browser", "resource_id": "playwright-chromium",
                   "process_id": 4243,
                   "observed_at": int(started.timestamp()) + 60}
        self.assertEqual([], support.provenance_window_gaps(
            None, browser, {"playwright": [(started, finished)]}))
        gaps = support.provenance_window_gaps(
            None, browser, {"flutter": [(started, finished)]})
        self.assertEqual(1, len(gaps))
        self.assertIn("no measured playwright suite window", gaps[0])

    def test_w6_support_requires_and_carries_observed_at(self):
        allowlist = {"device_provenance": ["XWE6R19916004085"],
                     "browser_provenance": ["playwright-chromium"]}
        with tempfile.TemporaryDirectory() as tmp:
            case_dir = Path(tmp)
            good = {"kind": "physical", "resource_id": "XWE6R19916004085",
                    "process_id": 4242, "observed_at": 1793480000,
                    "source": "probe"}
            (case_dir / "device-provenance.json").write_text(json.dumps(good))
            proof = w6_support.device_provenance(str(case_dir), allowlist,
                                                 "W6-02/probe")
            self.assertEqual({"kind": "physical",
                              "resource_id": "XWE6R19916004085",
                              "process_id": 4242,
                              "observed_at": 1793480000}, proof)
            for bad in ({k: v for k, v in good.items() if k != "observed_at"},
                        dict(good, observed_at=0), dict(good, observed_at=-1),
                        dict(good, observed_at="1793480000")):
                (case_dir / "device-provenance.json").write_text(json.dumps(bad))
                with self.subTest(observed_at=bad.get("observed_at")):
                    with self.assertRaises(SystemExit) as caught:
                        w6_support.device_provenance(str(case_dir), allowlist,
                                                     "W6-02/probe")
                    self.assertEqual(30, caught.exception.code)
            bgood = {"kind": "real-browser",
                     "resource_id": "playwright-chromium",
                     "process_id": 4243, "observed_at": 1793480001}
            (case_dir / "browser-provenance.json").write_text(json.dumps(bgood))
            bproof = w6_support.browser_provenance(str(case_dir), allowlist,
                                                   "W6-02/probe")
            self.assertEqual(1793480001, bproof["observed_at"])
            (case_dir / "browser-provenance.json").write_text(json.dumps(
                {k: v for k, v in bgood.items() if k != "observed_at"}))
            with self.assertRaises(SystemExit) as caught:
                w6_support.browser_provenance(str(case_dir), allowlist,
                                              "W6-02/probe")
            self.assertEqual(30, caught.exception.code)

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
            for name in ("device-provenance.json", "device-provenance-put.json",
                         "browser-provenance.json"):
                (root / name).write_text(json.dumps({"old": True}))
            script = f'''set -u
PATH="$1:$PATH"
{functions}
w602_run_flutter "$1" "$1" disk no-such-device put; a=$?
w602_run_admin "$1" "$1" disk; b=$?
[ "$a" -eq 75 ] && [ "$b" -eq 75 ]
'''
            result = subprocess.run(["bash", "-c", script, "probe", tmp],
                                    capture_output=True, text=True, timeout=15)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertFalse((root / "device-provenance.json").exists())
            self.assertFalse((root / "device-provenance-put.json").exists())
            self.assertFalse((root / "browser-provenance.json").exists())

    def test_gate_refuses_unowned_backend_before_external_calls(self):
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        case = source[source.index("w602_preflight_write() {"):
                      source.index("# ---------------- W6-03 executors")]
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            app = root / "imboyapp-stub"
            (app / "integration_test").mkdir(parents=True)
            (app / "integration_test" / "attachment_lifecycle_v5_test.dart").write_text("")
            admin = root / "imboyadmin-stub"
            admin.mkdir()
            bin_dir = root / "stub-bin"
            bin_dir.mkdir()
            flutter = bin_dir / "flutter"
            flutter.write_text('#!/bin/sh\necho \'[{"id": "XWE6R19916004085"}]\'\n')
            flutter.chmod(0o755)
            script = f'''set -u
PATH="{bin_dir}:$PATH"
worktree_sibling() {{ case "$1" in
  imboyapp) echo "{app}" ;; imboyadmin) echo "{admin}" ;; esac; }}
{case}
w602_case W6-02/ac08 "$1"; rc=$?; [ "$rc" -eq 75 ]
'''
            result = subprocess.run(["bash", "-c", script, "probe", tmp],
                                    capture_output=True, text=True, timeout=15)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertIn("W602_BACKEND_URL not set", result.stderr)
            names = [entry.name for entry in root.iterdir()]
            self.assertNotIn("backend-provenance.json", names)
            self.assertFalse(any(name.startswith("flutter-integration-") for name in names),
                             names)
            self.assertFalse(any(name.startswith("device-provenance") for name in names),
                             names)


if __name__ == "__main__":
    unittest.main()
