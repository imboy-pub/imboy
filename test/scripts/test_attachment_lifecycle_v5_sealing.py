"""Synthetic verifier fixtures; no business services or acceptance claims."""
import copy
import json
import os
import subprocess
import types
import unittest
from unittest import mock

import test_attachment_lifecycle_v5_runner as verifier

ROOT, runner = verifier.ROOT, verifier.runner


class SealingTests(unittest.TestCase):
    def setUp(self):
        self.f = verifier.VerificationTests()
        self.f.setUp()
        self.addCleanup(self.f.doCleanups)
        self.row = self.f.bound_receipt()
        self.f.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1,
                                          "task_deadline": __import__("time").time() + 600}
        # Raw snapshot rewrites break the ledger binding (_require_state
        # rejects them); transitions must go through StateStore.write_snapshot.
        self.f.state = runner.StateStore(str(self.f.run)).write_snapshot(self.f.state)
        # Evidence layout isolates each attempt under case_dir/attempt-N; the
        # verifier fixture builds the case root, so descend one level here.
        self.f.case_dir = self.f.case_dir / "attempt-1"
        self.f.case_dir.mkdir()
        self.input = self.f.case_dir / "measured-receipts.json"
        self.args = types.SimpleNamespace(run_dir=str(self.f.run), task_id="W4-02",
            case_id="W4-02/worker", case_dir=str(self.f.case_dir),
            receipts=str(self.input), command=self.row["command"])

    def seal(self, rows):
        self.input.write_text(json.dumps(rows))
        try:
            return runner.cmd_seal_case(self.args)
        except SystemExit as exc:
            return exc.code

    def sealed_leaf(self):
        matches = sorted(self.f.case_dir.glob("sealed-*/leaf-001.json"))
        self.assertEqual(1, len(matches), "expected exactly one sealed leaf")
        return json.loads(matches[0].read_text())

    def test_exact_measured_case_seals_and_binds_only_the_manifest(self):
        self.assertEqual(0, self.seal([self.row]))
        self.assertEqual(self.row, self.sealed_leaf())
        # Sealing binds the case manifest digest into the snapshot; no other
        # task/gate state may change.
        after = json.loads(self.f.state_file.read_text())
        self.assertEqual({self.args.case_id}, set(after.get("case_manifest_digests", {})))
        self.assertEqual("RUNNING", after["tasks"]["W4-02"]["state"])

    def test_reject_missing_duplicate_or_stale_without_leaf_output(self):
        stale = copy.deepcopy(self.row)
        stale["candidate_heads"]["imboy"] = "f" * 40
        for rows in ([], [self.row, self.row], [stale]):
            self.assertNotEqual(0, self.seal(rows))
            self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_reject_skip_fake_actor_and_changed_raw(self):
        for row in (dict(self.row, skipped=1), dict(self.row, actor_provenance={"kind": "mock"})):
            self.assertNotEqual(0, self.seal([row]))
        self.f.raw.write_text("all tests passed\n")
        self.assertNotEqual(0, self.seal([self.row]))
        self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_legacy_suite_flag_cannot_create_success_receipts(self):
        backup = self.f.run / "backup/premerge-untracked-plan/attachment-lifecycle-v5"
        backup.mkdir(parents=True)
        (backup / "execution-contract.json").write_text(json.dumps({
            "expected_leaf_matrix": {"W4-03/worker": [self.f.leaf]}}))
        script = ROOT / "scripts/test/attachment_lifecycle_v5_w403_seal.sh"
        env = dict(os.environ, V5_RUN_DIR=str(self.f.run), W403S_PG_OK="1")
        result = subprocess.run(["bash", str(script), str(ROOT), "W4-03/worker",
            str(self.f.case_dir), "synthetic suite"], env=env, capture_output=True, timeout=10)
        self.assertNotEqual(0, result.returncode)
        self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_shared_shell_entry_accepts_exact_measured_leaves(self):
        self.input.write_text(json.dumps([self.row]))
        result = subprocess.run(["bash", str(ROOT / "scripts/test/attachment_lifecycle_v5_seal.sh"),
            str(ROOT), self.args.case_id, str(self.f.case_dir), self.row["command"]],
            env=dict(os.environ, V5_RUN_DIR=str(self.f.run)), capture_output=True, timeout=10)
        self.assertEqual(0, result.returncode, result.stderr.decode())
        self.assertEqual(self.row, self.sealed_leaf())

    def test_all_compatibility_entries_refuse_suite_flags_without_measurements(self):
        env = dict(os.environ, V5_RUN_DIR=str(self.f.run))
        for flag in ["W202_CONFIRM_PG_OK", "W202_EB_ASSET_OK", "W202_GATEWAY_OK", "W202_ATTACH_LOGIC_OK",
                     "W203_PG_OK", "W302_PG_OK", "W303_PG_OK", "W401_PG_OK", "W402_PG_OK", "W403S_PG_OK", "W404S_PG_OK"]:
            env[flag] = "1"
        for wave in ["w202", "w203", "w301", "w302", "w303", "w401", "w402", "w403", "w404"]:
            with self.subTest(wave=wave):
                script = ROOT / f"scripts/test/attachment_lifecycle_v5_{wave}_seal.sh"
                args = ["bash", str(script), str(ROOT), self.args.case_id, str(self.f.case_dir)]
                if wave == "w202": args.append("0")
                args.append("synthetic suite")
                result = subprocess.run(args, env=env, capture_output=True, timeout=10)
                self.assertNotEqual(0, result.returncode)
                self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_gate_final_seal_does_not_modify_measured_raw_log(self):
        self.input.write_text(json.dumps([self.row]))
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        start = source.index("execute_case() {")
        function = source[start:source.index("\n}", start) + 2]
        script = 'run_case() { return 0; }\nproduce_measured() { return 0; }\n' + function + '\nexecute_case "$CASE_ID"'
        before = self.f.raw.read_bytes()
        env = dict(os.environ, ROOT=str(ROOT), SCRIPT_DIR=str(ROOT / "scripts/test"),
            RUN_DIR=str(self.f.run), V5_RUN_DIR=str(self.f.run),
            EVID=str(self.f.case_dir.parent.parent), V5_TASK_ATTEMPT="1",
            CASE_ID=self.args.case_id, V5_GATE_COMMAND=self.row["command"])
        result = subprocess.run(["bash", "-c", script], env=env, capture_output=True, timeout=10)
        self.assertEqual(0, result.returncode, result.stderr.decode())
        self.assertEqual(before, self.f.raw.read_bytes())
        self.assertTrue(list(self.f.case_dir.glob("sealed-*/leaf-001.json")))

    def test_reject_external_receipt_file(self):
        self.args.receipts = str(self.f.run / "external.json")
        self.assertNotEqual(0, self.seal([self.row]))
        self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_candidate_cannot_bind_a_different_plan(self):
        self.f.state["verification_candidate"]["plan_manifest_sha256"] = "f" * 64
        self.f.state = runner.StateStore(str(self.f.run)).write_snapshot(self.f.state)
        row = dict(self.row, plan_manifest_sha256="f" * 64)
        self.assertNotEqual(0, self.seal([row]))
        self.assertFalse(list(self.f.case_dir.glob("leaf-*.json")))

    def test_cross_case_symlink_never_writes_other_case(self):
        foreign = self.f.run / "evidence/W5-01/foreign-case"
        foreign.parent.mkdir()
        self.f.case_dir.rename(foreign)
        self.f.case_dir.symlink_to(foreign, target_is_directory=True)
        self.assertNotEqual(0, self.seal([self.row]))
        self.assertFalse(list(foreign.glob("leaf-*.json")))

    def test_directory_replacement_after_validation_never_writes_replacement(self):
        moved = self.f.case_dir.with_name("moved-original")
        foreign = self.f.case_dir.with_name("foreign-case")
        foreign.mkdir()
        publish = runner.case_store.publish
        def swap(fd, rows):
            self.f.case_dir.rename(moved)
            self.f.case_dir.symlink_to(foreign, target_is_directory=True)
            return publish(fd, rows)
        with mock.patch.object(runner.case_store, "publish", side_effect=swap):
            self.assertNotEqual(0, self.seal([self.row]))
        self.assertFalse(list(foreign.glob("sealed-*/leaf-*.json")))
        self.assertEqual(1, len(list(moved.glob("sealed-*/leaf-*.json"))))

    def test_directory_replacement_before_open_preserves_foreign_receipt(self):
        moved = self.f.case_dir.with_name("moved-original")
        foreign = self.f.case_dir.with_name("foreign-case")
        foreign.mkdir()
        (foreign / "leaf-001.json").write_text("FOREIGN")
        write = runner._write_case_rows
        def swap(*args, **kwargs):
            self.f.case_dir.rename(moved)
            foreign.rename(self.f.case_dir)
            return write(*args, **kwargs)
        with mock.patch.object(runner, "_write_case_rows", side_effect=swap):
            self.assertNotEqual(0, self.seal([self.row]))
        self.assertEqual("FOREIGN", (self.f.case_dir / "leaf-001.json").read_text())
        self.assertFalse(list(moved.glob("leaf-*.json")))


if __name__ == "__main__":
    unittest.main()
