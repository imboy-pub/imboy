"""W2 expand evidence must not be produced from the integrated migration chain."""
from pathlib import Path
import contextlib
import io
import json
import subprocess
import tempfile
import time
import types
import unittest

import test_attachment_lifecycle_v5_runner as runner_tests

runner = runner_tests.runner


ROOT = Path(__file__).resolve().parents[2]


class W201SchemaPhaseTests(unittest.TestCase):
    def test_unsealed_tampered_phase_is_rejected_by_real_runner_before_side_effects(self):
        fixture = runner_tests.VerificationTests("runTest")
        fixture.setUp()
        self.addCleanup(fixture.doCleanups)
        contract_path = fixture.plan / "execution-contract.json"
        fixture.contract["expected_leaf_matrix"]["W4-02/worker"][0]["schema_phase"] = "expand-queue"
        contract_path.write_text(json.dumps(fixture.contract))
        (fixture.plan / "manifest.sha256").write_text(
            runner.sha256_file(contract_path) + "  execution-contract.json\n")
        digest = runner.sha256_file(fixture.plan / "manifest.sha256")
        fixture.candidate["plan_manifest_sha256"] = digest
        fixture.state["plan_manifest_sha256"] = digest
        auth_path = Path(fixture.state["authorization_path"])
        auth = json.loads(auth_path.read_text())
        auth["plan_manifest_sha256"] = digest
        auth_path.write_text(json.dumps(auth))
        fixture.state["authorization_sha256"] = runner.sha256_file(auth_path)
        fixture.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1,
                                             "task_deadline": time.time() + 120}
        runner.StateStore(str(fixture.run)).write_snapshot(fixture.state)
        # Change only the frozen contract, keeping the manifest and receipt:
        # direct contract parsing would now incorrectly allow integrated.
        fixture.contract["expected_leaf_matrix"]["W4-02/worker"][0]["schema_phase"] = "integrated"
        contract_path.write_text(json.dumps(fixture.contract))
        script = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        def extract(name):
            start = script.index(name + "() {")
            return script[start:script.index("\n}\n", start) + 2]
        marker = fixture.run / "side-effect"
        shell = r'''set -u; RUN_DIR="$1"; STATE="$1/state.json"; RUNNER="$2"; MARKER="$1/side-effect"
mkdir() { touch "$MARKER"; }
ensure_pg() { touch "$MARKER"; }
''' + "\n".join(extract(name) for name in ("inspect_task_phase", "require_replay_phase_support", "phase_dispatch_preflight")) + '\nphase_dispatch_preflight W4-02 && mkdir && ensure_pg\n'
        result = subprocess.run(["bash", "-c", shell, "test", str(fixture.run),
                                 str(ROOT / "scripts/attachment_lifecycle_v5_runner.py")],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(20, result.returncode, result.stderr)
        self.assertIn("frozen contract binding invalid", result.stderr)
        self.assertFalse(marker.exists())

    def test_sealed_case_is_verified_before_unsealed_phase_rejection(self):
        fixture = runner_tests.VerificationTests("runTest")
        fixture.setUp()
        self.addCleanup(fixture.doCleanups)
        task_id = "W4-02"
        first, second = "W4-02/worker", "W4-02/second"
        fixture.leaf["schema_phase"] = "expand-queue"
        fixture.receipt["schema_phase"] = "expand-queue"
        # The unsealed case comes first: all sealed cases must still be
        # verified before the unsealed phase is rejected.
        fixture.contract["tasks"][0]["case_ids"] = [second, first]
        fixture.contract["expected_leaf_matrix"][second] = [dict(fixture.leaf)]
        contract_path = fixture.plan / "execution-contract.json"
        contract_path.write_text(json.dumps(fixture.contract))
        (fixture.plan / "manifest.sha256").write_text(
            runner.sha256_file(contract_path) + "  execution-contract.json\n")
        plan_hash = runner.sha256_file(fixture.plan / "manifest.sha256")
        fixture.candidate["plan_manifest_sha256"] = plan_hash
        fixture.state["plan_manifest_sha256"] = plan_hash
        auth_path = Path(fixture.state["authorization_path"])
        auth = json.loads(auth_path.read_text())
        auth["plan_manifest_sha256"] = plan_hash
        auth_path.write_text(json.dumps(auth))
        fixture.state["authorization_sha256"] = runner.sha256_file(auth_path)
        row = fixture.bound_receipt()
        fixture.state["tasks"][task_id] = {"state": "RUNNING", "attempt": 1,
                                            "task_deadline": time.time() + 120}
        runner.StateStore(str(fixture.run)).write_snapshot(fixture.state)
        attempt_dir = fixture.case_dir / "attempt-1"
        attempt_dir.mkdir()
        raw = attempt_dir / "raw.json"
        raw.write_bytes(fixture.raw.read_bytes())
        row["raw_output_path"] = str(raw)
        row["raw_output_sha256"] = runner.sha256_file(raw)
        receipts = attempt_dir / "measured-receipts.json"
        receipts.write_text(json.dumps([row]))
        args = types.SimpleNamespace(run_dir=str(fixture.run), task_id=task_id,
            case_id=first, case_dir=str(attempt_dir), receipts=str(receipts), command=row["command"])
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, runner.cmd_seal_case(args))
        script = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        def extract(name):
            start = script.index(name + "() {")
            return script[start:script.index("\n}\n", start) + 2]
        shell = r'''set -u; CONTRACT_JSON="$1"; RUN_DIR="$2"; STATE="$2/state.json"; RUNNER="$3"
''' + extract("inspect_task_phase") + "\n" + extract("require_replay_phase_support") + "\n" + extract("phase_dispatch_preflight") + '\nphase_dispatch_preflight W4-02\n'
        command = ["bash", "-c", shell, "test", str(contract_path), str(fixture.run),
                   str(ROOT / "scripts/attachment_lifecycle_v5_runner.py")]
        result = subprocess.run(command, capture_output=True, text=True, timeout=10)
        self.assertEqual(20, result.returncode, result.stderr)
        self.assertIn('"result": "PASS"', result.stdout)
        self.assertIn("fixture is not attested", result.stderr)
        original_auth = auth_path.read_bytes()
        auth_path.write_text("{}")
        result = subprocess.run(command, capture_output=True, text=True, timeout=10)
        self.assertEqual(20, result.returncode)
        self.assertNotIn('"result": "PASS"', result.stdout)
        self.assertNotIn("fixture is not attested", result.stderr)
        auth_path.write_bytes(original_auth)
        (fixture.plan / "manifest.sha256").write_text("invalid manifest\n")
        result = subprocess.run(command, capture_output=True, text=True, timeout=10)
        self.assertNotEqual(0, result.returncode)
        self.assertNotIn('"result": "PASS"', result.stdout)
        self.assertNotIn("fixture is not attested", result.stderr)

    def test_real_dispatch_checks_frozen_leaf_phase_before_fixture_side_effects(self):
        script = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        def extract(name):
            start = script.index(name + "() {")
            return script[start:script.index("\n}\n", start) + 2]
        functions = extract("require_replay_phase_support") + "\n" + extract("run_case")
        contract = ROOT / "docs/architecture/attachment-lifecycle-v5/execution-contract.json"
        with tempfile.TemporaryDirectory() as tmp:
            marker = Path(tmp) / "called"
            shell = r'''CONTRACT_JSON="$1"; EVID="$2/evidence"; MARKER="$2/called"
V5_TASK_ATTEMPT=1
mkdir() { printf 'mkdir\n' >> "$MARKER"; }
ensure_pg() { printf 'ensure_pg\n' >> "$MARKER"; return 77; }
inspect_task_phase() { python3 - "$CONTRACT_JSON" "$2" <<'PYC'
import json, sys
c = json.load(open(sys.argv[1])); leaves = c["expected_leaf_matrix"][sys.argv[2]]
print(json.dumps({"cases":[{"integrated_only":all(x["schema_phase"] == "integrated" for x in leaves)}]}))
PYC
}
''' + functions + '\nrun_case "$3"\n'
            for cid in ("W2-01/upload-content-pin", "W2-02/confirm-idempotent",
                        "W2-03/confirm-gc-race", "W3-01/history-unknown",
                        "W3-02/hold-gc-race", "W4-01/delete-entrypoints",
                        "W4-02/worker-lease", "W4-03/storage-disk"):
                with self.subTest(case=cid):
                    result = subprocess.run(["bash", "-c", shell, "test", str(contract), tmp, cid],
                                            text=True, capture_output=True, timeout=5)
                    self.assertEqual(20, result.returncode, result.stderr)
                    self.assertIn("fixture is not attested", result.stderr)
                    self.assertFalse(marker.exists())
            for cid in ("W4-04/reconcile", "W6-01/real-pg-garage-disk"):
                with self.subTest(case=cid):
                    result = subprocess.run(["bash", "-c", shell, "test", str(contract), tmp, cid],
                                            text=True, capture_output=True, timeout=5)
                    self.assertEqual(77, result.returncode, result.stderr)
                    self.assertEqual("mkdir\nensure_pg\n", marker.read_text())
                    marker.unlink()

    def test_w2_refuses_before_build_or_database_setup(self):
        script = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        start = script.index("w201_case() {")
        end = script.index("\n}\n", start) + 2
        function = script[start:end]
        with tempfile.TemporaryDirectory() as tmp:
            marker = Path(tmp) / "unexpected-side-effect"
            shell = r'''TASK=W2-01; ROOT="$1"; SIDE_EFFECT="$2"; CUR_CASE=W2-01/upload-content-pin
make() { touch "$SIDE_EFFECT"; }
w201_db_setup() { touch "$SIDE_EFFECT"; }
w201_run_all_sections() { touch "$SIDE_EFFECT"; }
seal_w201_leafs() { touch "$SIDE_EFFECT"; }
''' + function + '\nw201_case "$2"\n'
            result = subprocess.run(["bash", "-c", shell, "test", str(ROOT), str(marker)],
                                    text=True, capture_output=True, timeout=5)
            self.assertEqual(20, result.returncode, result.stderr)
            self.assertIn("attested expand-queue fixture", result.stderr)
            self.assertFalse(marker.exists())


if __name__ == "__main__":
    unittest.main()
