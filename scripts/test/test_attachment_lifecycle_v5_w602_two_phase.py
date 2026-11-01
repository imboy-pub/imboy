import json
import os
from pathlib import Path
import subprocess
import sys
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
phase = scenario = ""
for arg in sys.argv[1:]:
    if arg.startswith("--dart-define=V5_E2E_PHASE="):
        phase = arg.rsplit("=", 1)[1]
    if arg.startswith("--dart-define=V5_E2E_SCENARIO="):
        scenario = arg.rsplit("=", 1)[1]
run = os.environ["W602_E2E_RUN_ID"]
task = os.environ["W602_E2E_UPLOAD_ID"]
name = scenario or "kill_after_put"
print('{"type":"start","pid":%d,"time":0}' % os.getpid())
print('{"type":"suite","time":0,"suite":{"id":0,"platform":"android",'
      '"path":"integration_test/attachment_lifecycle_v5_test.dart"}}')
print('{"type":"testStart","time":1,"test":{"id":3,"name":"%s",'
      '"suiteID":0,"groupIDs":[]}}' % name)
if phase == "put" and os.environ.get("V5_STUB_EMIT_KILL_READY", "1") == "1":
    tail = " scenario=%s" % name if scenario else ""
    print('{"type":"print","testID":3,"message":"V5_KILL_READY run=%s '
          'task=%s%s pid=4242"}' % (run, task, tail))
print('{"type":"testDone","testID":3,"result":"success","skipped":false,'
      '"hidden":false,"time":5}')
print('{"type":"done","success":true,"time":6}')
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

    # ---------------- leaf-scenario wiring (flutter-recovery-real) ----------

    def test_scenario_define_folds_kill_after_put_to_frozen_argv(self):
        script = self.script_base + (
            '\nfor leaf in kill_after_registered kill_after_put '
            'restart_before_put same_key_same_ciphertext_after_restart ""; do '
            'printf "[%s]" "$(w602_scenario_define "$leaf")"; done; printf "\n"\n')
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual("[kill_after_registered][][restart_before_put]"
                         "[same_key_same_ciphertext_after_restart][]\n",
                         result.stdout)

    def test_scenario_two_phase_classification_covers_the_eight_kill_leaves(self):
        script = self.script_base + (
            '\nfor leaf in kill_after_put restart_before_put '
            'same_key_same_ciphertext_after_restart user_switch '
            'lost_cipher_material_requires_new_task; do '
            'w602_scenario_two_phase "$leaf" && printf "two:%s " "$leaf" '
            '|| printf "single:%s " "$leaf"; done; printf "\n"\n')
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(
            "two:kill_after_put two:restart_before_put "
            "two:same_key_same_ciphertext_after_restart "
            "single:user_switch single:lost_cipher_material_requires_new_task \n",
            result.stdout)

    def test_tagged_two_phase_writes_leaf_named_artifacts_and_marker(self):
        result = self.run_two_phase(
            'TAG=restart_before_put\n'
            'w602_flutter_two_phase "%s" "%s" garage stub-device '
            '"$W602_E2E_RUN_ID" "$W602_E2E_UPLOAD_ID" "$TAG"; rc=$?\nexit "$rc"\n'
            % (ROOT, self.cdir))
        self.assertEqual(0, result.returncode, result.stderr)
        raw_log = (self.cdir / "raw.log").read_text()
        self.assertIn("CMD[flutter-garage-restart_before_put-put]", raw_log)
        self.assertIn("CMD[flutter-garage-restart_before_put-restore]", raw_log)
        # ... while the restore phase owns the leaf's final (phase-less)
        # report file, exactly like the untagged flow owns the driver name.
        self.assertTrue(
            (self.cdir / "flutter-integration-garage-restart_before_put.jsonl")
            .exists())
        self.assertIn("KILL -9 pid=4242 device=stub-device driver=garage "
                      "tag=restart_before_put", raw_log)
        self.assertIn("KILLED_MARKER v5-killed-%s-%s-restart_before_put.mark"
                      % ("v5-e2e-two-phase-1",
                         "a1234567-1234-4123-8123-123456789012"), raw_log)
        self.assertTrue(
            (self.cdir / "flutter-integration-garage-restart_before_put-put.jsonl")
            .exists())
        leaf_report = (
            self.cdir / "flutter-integration-garage-restart_before_put.jsonl"
        ).read_text()
        self.assertIn('"name":"restart_before_put"', leaf_report)
        # The put-phase announcement carries the leaf name for the host
        # parser, and the leaf-named report registers the leaf-named test.
        put_report = (
            self.cdir
            / "flutter-integration-garage-restart_before_put-put.jsonl"
        ).read_text()
        self.assertIn("V5_KILL_READY run=v5-e2e-two-phase-1 "
                      "task=a1234567-1234-4123-8123-123456789012 "
                      "scenario=restart_before_put pid=4242", put_report)
        # The frozen (untagged) artifact names must not be produced by a
        # tagged run; only the merge step may write them.
        self.assertFalse((self.cdir / "flutter-integration-garage.jsonl").exists())

    def test_kill_ready_pid_parses_scenario_tagged_announcement(self):
        jsonl = self.cdir / "tagged.jsonl"
        jsonl.write_text("\n".join([
            '{"type":"start","pid":4242}',
            '{"type":"print","message":"V5_KILL_READY run=v5-e2e-x '
            'task=t scenario=restart_before_put pid=1723"}',
        ]))
        script = self.script_base + (
            '\nw602_kill_ready_pid "%s";\n' % jsonl)
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual("1723\n", result.stdout)

    def test_merge_renumbers_ids_and_adapter_suite_binds_each_leaf_once(self):
        sys.path.insert(0, str(ROOT / "scripts" / "test"))
        import attachment_lifecycle_v5_adapter_w6_02_support as S2

        leaves = ["kill_after_registered", "user_switch", "restart_before_put"]
        for index, leaf in enumerate(leaves):
            events = [
                {"type": "start", "pid": 100 + index, "time": 0},
                {"type": "suite", "time": 0, "suite": {
                    "id": 0, "platform": "android",
                    "path": "integration_test/attachment_lifecycle_v5_test.dart"}},
                {"type": "testStart", "time": 1, "test": {
                    "id": 3, "name": leaf, "suiteID": 0, "groupIDs": []}},
                {"type": "testDone", "testID": 3, "result": "success",
                 "skipped": False, "hidden": False, "time": 10 + index},
                {"type": "done", "success": True, "time": 12 + index},
            ]
            path = self.cdir / ("flutter-integration-garage-%s.jsonl" % leaf)
            path.write_text("\n".join(json.dumps(event) for event in events))
        script = self.script_base + (
            '\nw602_merge_flutter_reports "%s" garage kill_after_registered '
            'user_switch restart_before_put; rc=$?; echo "rc=$rc"\n'
            % self.cdir)
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertIn("rc=0", result.stdout, result.stderr)
        merged = self.cdir / "flutter-integration-garage.jsonl"
        self.assertTrue(merged.exists())
        suite = S2.flutter_suite(str(self.cdir), "flutter-integration-garage.jsonl",
                                 "W6-02/flutter-recovery-real", "probe")
        names = sorted(test["name"] for test in suite["tests"])
        self.assertEqual(["kill_after_registered", "restart_before_put",
                          "user_switch"], names)
        for leaf in leaves:
            hits = S2.find_flutter_test(suite, leaf)
            self.assertEqual(1, len(hits), leaf)

    def test_merge_fails_closed_on_unsuccessful_segment(self):
        good = self.cdir / "flutter-integration-disk-user_switch.jsonl"
        good.write_text("\n".join(json.dumps(event) for event in [
            {"type": "start", "pid": 7, "time": 0},
            {"type": "done", "success": True, "time": 5},
        ]))
        bad = self.cdir / "flutter-integration-disk-kill_after_put.jsonl"
        bad.write_text("\n".join(json.dumps(event) for event in [
            {"type": "start", "pid": 8, "time": 0},
            {"type": "done", "success": False, "time": 6},
        ]))
        script = self.script_base + (
            '\nw602_merge_flutter_reports "%s" disk user_switch kill_after_put; '
            'rc=$?; echo "rc=$rc"\n' % self.cdir)
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertIn("rc=30", result.stdout)
        self.assertIn("finished unsuccessfully", result.stderr)
        # The adapter-visible merged artifact must not exist after a refusal.
        self.assertFalse(
            (self.cdir / "flutter-integration-disk.jsonl").exists())

    # ---------------- ac08 / e2e-compat scenario wiring ---------------------

    def test_scenario_classification_covers_ac08_and_e2e_compat_leaves(self):
        script = self.script_base + (
            '\nfor leaf in physical_device_restart legacy_acl_cipher_bytes '
            'unknown_history_retained physical_device kill_after_put; do '
            'w602_scenario_two_phase "$leaf" && printf "two:%s " "$leaf" '
            '|| printf "single:%s " "$leaf"; done; printf "\\n"\n')
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(
            "two:physical_device_restart two:legacy_acl_cipher_bytes "
            "two:unknown_history_retained two:physical_device "
            "two:kill_after_put \n",
            result.stdout)

    def test_run_scenario_leaves_merges_e2e_compat_case_list(self):
        # e2e-compat's frozen list: the empty-scenario two-phase flow
        # (kill_after_put tag) plus its own physical_device leaf; every
        # e2e-compat leaf runs the two-phase put/kill/restore protocol
        # (the dart suite expects V5_E2E_PHASE for each injected scenario
        # name). After the merge, the adapter-visible per-driver artifact
        # binds each scenario name exactly once.
        script = self.script_base + (
            '\nw602_run_scenario_leaves "%s" "%s" garage stub-device '
            '"$W602_E2E_RUN_ID" "$W602_E2E_UPLOAD_ID" kill_after_put '
            'physical_device; rc=$?\nexit "$rc"\n' % (ROOT, self.cdir))
        result = subprocess.run(["bash", "-c", script], env=self.env,
                                capture_output=True, text=True, timeout=120)
        self.assertEqual(0, result.returncode, result.stderr)
        sys.path.insert(0, str(ROOT / "scripts" / "test"))
        import attachment_lifecycle_v5_adapter_w6_02_support as S2
        suite = S2.flutter_suite(str(self.cdir),
                                 "flutter-integration-garage.jsonl",
                                 "W6-02/e2e-compat", "probe")
        names = sorted(test["name"] for test in suite["tests"])
        self.assertEqual(["kill_after_put", "physical_device"], names)
        for leaf in ("kill_after_put", "physical_device"):
            self.assertEqual(1, len(S2.find_flutter_test(suite, leaf)), leaf)
        raw_log = (self.cdir / "raw.log").read_text()
        self.assertIn("CMD[flutter-garage-kill_after_put-put]", raw_log)
        self.assertIn("CMD[flutter-garage-physical_device-put]", raw_log)
        self.assertIn("CMD[flutter-garage-physical_device-restore]", raw_log)

    def test_flutter_command_pins_standard_flavor(self):
        # The app declares a brand flavor dimension; only --flavor standard
        # keeps applicationId=pub.imboy.app so the executor's
        # `adb shell pidof pub.imboy.app` liveness observation can succeed.
        root = Path(self.temp.name)
        fl_bin = root / "fl-bin"
        fl_bin.mkdir()
        flutter = fl_bin / "flutter"
        flutter.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$ARGV_FILE"\n')
        flutter.chmod(0o755)
        env = dict(self.env, PATH=f"{fl_bin}:{self.env['PATH']}",
                   ARGV_FILE=str(root / "argv.txt"))
        script = ('set -u\nsource "%s"\nv5_w602_flutter_command garage '
                  'stub-device\n'
                  % (ROOT / "scripts/test/attachment_lifecycle_v5_flutter_command.sh"))
        result = subprocess.run(["bash", "-c", script], env=env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(0, result.returncode, result.stderr)
        argv = (root / "argv.txt").read_text().splitlines()
        self.assertIn("--flavor", argv)
        self.assertEqual("standard", argv[argv.index("--flavor") + 1])
        self.assertIn("--dart-define=V5_E2E_DRIVER=garage", argv)
        self.assertNotIn("--dart-define=V5_E2E_SCENARIO=", argv)



class W602BackendProvenanceWindowTest(unittest.TestCase):
    """R3 hardening: healthz_checked_at is consumed, not decoration.

    A W6-02 receipt's backend liveness claim must carry a positive epoch
    healthz timestamp that does not postdate the leaf's completion; a
    missing, malformed, zero or future timestamp fails the mandatory
    backend_provenance check fail-closed.
    """

    FINISHED = "2026-11-01T00:15:26Z"

    @staticmethod
    def receipt(healthz):
        leaf = {
            "required": True, "task_id": "W6-02", "case_id": "W6-02/ac08",
            "finished_at": W602BackendProvenanceWindowTest.FINISHED,
            "backend_provenance": {
                "kind": "owned-backend", "resource_id": "127.0.0.1:9810",
                "base_url": "http://127.0.0.1:9810", "process_id": 4242,
                "healthz_checked_at": healthz,
            },
        }
        return leaf, dict(leaf, source_case_ref="W2-01/upload-content-pin")

    def errors(self, healthz):
        sys.path.insert(0, str(ROOT / "scripts"))
        import attachment_lifecycle_v5_runner as runner
        with tempfile.TemporaryDirectory() as tmp:
            errs = runner.receipt_errors(
                *self.receipt(healthz), "W6-02", "W6-02/ac08", {},
                tmp, {"backend_provenance": ["127.0.0.1:9810"]}, {})
        return [e for e in errs
                if e == "missing registered live backend_provenance"]

    def test_healthz_within_window_is_accepted(self):
        # 1793480000 < 2026-11-01T00:15:26Z epoch: an honest past healthz.
        self.assertEqual([], self.errors(1793480000))

    def test_missing_malformed_zero_and_future_healthz_fail(self):
        for healthz in (None, "not-a-number", 0, -5, 1793600000):
            with self.subTest(healthz=healthz):
                self.assertEqual(1, len(self.errors(healthz)))


if __name__ == "__main__":
    unittest.main()
