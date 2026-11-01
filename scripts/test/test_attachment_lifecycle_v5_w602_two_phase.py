import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]

# Isolates the W6-02 two-phase kill/restore orchestration from gate.sh behind
# stub flutter/adb binaries: phase A (put) prints V5_KILL_READY pid=4242, the
# stub adb reports it dead after `kill -9` and relaunches as pid 4243, and
# phase B (restore) is the final verification session. Real devices, real
# backends and docker/PG are never touched.
ADB_STUB = '''#!/bin/sh
state="$V5_ADB_STATE"
case " $* " in
  *" shell pidof pub.imboy.app"*)
    if [ -f "$state.killed" ]; then echo 4243; else echo 4242; fi
    exit 0 ;;
esac
case " $* " in
  *" shell kill -9 4242"*) : > "$state.killed"; exit 0 ;;
esac
case " $* " in
  *" shell run-as"*) exit "${V5_ADB_RUNAS_RC:-0}" ;;
  *" push "*) exit 0 ;;
esac
exit 0
'''

FLUTTER_STUB = '''#!/usr/bin/env python3
import os, sys, time
time.sleep(3)
phase = ""
for arg in sys.argv[1:]:
    if arg.startswith("--dart-define=V5_E2E_PHASE="):
        phase = arg.rsplit("=", 1)[1]
run = os.environ["W602_E2E_RUN_ID"]
task = os.environ["W602_E2E_UPLOAD_ID"]
if phase == "put" and os.environ.get("V5_STUB_EMIT_KILL_READY", "1") == "1":
    print('{"type":"print","message":"V5_KILL_READY run=%s task=%s pid=4242"}' % (run, task))
print('{"type":"testDone","successful":1}')
'''


class W602TwoPhaseTest(unittest.TestCase):
    def gate_functions(self):
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        return source[source.index("w602_device_pid() {"):
                      source.index("w602_case() {")]

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        self.cdir = root / "case"
        self.cdir.mkdir()
        bin_dir = root / "bin"
        bin_dir.mkdir()
        adb = bin_dir / "adb"
        adb.write_text(ADB_STUB)
        adb.chmod(0o755)
        flutter = bin_dir / "flutter"
        flutter.write_text(FLUTTER_STUB)
        flutter.chmod(0o755)
        self.state = str(root / "adb-state")
        self.env = dict(os.environ,
                        PATH=f"{bin_dir}:{os.environ['PATH']}",
                        V5_ADB_STATE=self.state,
                        W602_BACKEND_URL="http://127.0.0.1:9801",
                        W602_E2E_UID="synthetic-user",
                        W602_E2E_RUN_ID="v5-e2e-two-phase-1",
                        W602_E2E_UPLOAD_ID="a1234567-1234-4123-8123-123456789012")
        self.script_base = (
            'set -u\n'
            'source "%s"\n' % (ROOT / "scripts/test/attachment_lifecycle_v5_flutter_command.sh")
            + self.gate_functions())

    def run_two_phase(self, extra=""):
        script = self.script_base + extra + (
            '\nw602_flutter_two_phase "%s" "%s" garage stub-device '
            '"$W602_E2E_RUN_ID" "$W602_E2E_UPLOAD_ID"; rc=$?\nexit "$rc"\n'
            % (ROOT, self.cdir))
        return subprocess.run(["bash", "-c", script], env=self.env,
                              capture_output=True, text=True, timeout=60)

    def test_two_phase_happy_path_writes_kill_evidence_and_phase_logs(self):
        result = self.run_two_phase()
        self.assertEqual(0, result.returncode, result.stderr)
        raw_log = (self.cdir / "raw.log").read_text()
        self.assertIn("CMD[flutter-garage-put]", raw_log)
        self.assertIn("CMD[flutter-garage-restore]", raw_log)
        self.assertIn("KILL -9 pid=4242", raw_log)
        self.assertIn("KILLED_MARKER v5-killed-v5-e2e-two-phase-1-"
                      "a1234567-1234-4123-8123-123456789012.mark", raw_log)
        # Phase A keeps its own suffixed artifacts; phase B owns the final ones.
        self.assertTrue((self.cdir / "flutter-integration-garage-put.jsonl").exists())
        self.assertTrue((self.cdir / "flutter-integration-garage.jsonl").exists())
        put_proof = json.loads((self.cdir / "device-provenance-put.json").read_text())
        restore_proof = json.loads((self.cdir / "device-provenance.json").read_text())
        self.assertEqual(4242, put_proof["process_id"])
        self.assertEqual(4243, restore_proof["process_id"])
        marker = (self.cdir / "v5-killed-v5-e2e-two-phase-1-"
                  "a1234567-1234-4123-8123-123456789012.mark").read_text()
        self.assertIn("pid=4242", marker)

    def test_missing_kill_ready_fails_case_without_device_mutation(self):
        result = self.run_two_phase('V5_STUB_EMIT_KILL_READY=0\nexport V5_STUB_EMIT_KILL_READY\n')
        self.assertEqual(30, result.returncode, result.stderr)
        self.assertIn("V5_KILL_READY pid missing", result.stderr)
        raw_log = (self.cdir / "raw.log").read_text()
        self.assertIn("CMD[flutter-garage-put]", raw_log)
        self.assertNotIn("KILL -9", raw_log)
        self.assertNotIn("CMD[flutter-garage-restore]", raw_log)

    def test_kill_ready_pid_parses_wrapped_and_bare_lines(self):
        jsonl = self.cdir / "sample.jsonl"
        jsonl.write_text("\n".join([
            '{"type":"start"}',
            '{"type":"print","message":"V5_KILL_READY run=v5-e2e-x task=t pid=1723"}',
            '{"type":"print","message":"noise pid=99"}',
            "V5_KILL_READY run=v5-e2e-x task=t pid=8116",
        ]))
        script = self.script_base + (
            '\nw602_kill_ready_pid "%s";\n' % jsonl)
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        # The last observed pid wins (host kills the freshest announcement).
        self.assertEqual("8116\n", result.stdout)

    def test_runas_marker_copy_failure_is_recorded_not_fatal(self):
        self.env["V5_ADB_RUNAS_RC"] = "1"
        result = self.run_two_phase()
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("run-as copy of the killed marker",
                      (self.cdir / "raw.log").read_text())


if __name__ == "__main__":
    unittest.main()
