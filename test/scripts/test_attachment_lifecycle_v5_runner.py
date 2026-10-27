"""The real verify command must reject fabricated or stale leaf receipts."""
import contextlib
import copy
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import tempfile
import time
import types
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "attachment_runner", ROOT / "scripts/attachment_lifecycle_v5_runner.py")
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class VerificationTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.run = Path(self.tmp.name)
        self.plan = self.run / "plan"
        self.plan.mkdir()
        self.leaf = dict(ac_id="AC07", domain="ordinary", driver="garage",
                         entrypoint="ordinary", ordering_or_fault="delete_ack_lost",
                         schema_phase="integrated")
        self.contract = {
            "wave_gates": {"G4": {"requires_tasks": ["W4-02"], "previous_gate": None}},
            "tasks": [{"task_id": "W4-02", "case_ids": ["W4-02/worker"]}],
            "case_registry": {"W4-02/worker": {"source_kind": "real-required"}},
            "expected_leaf_matrix": {"W4-02/worker": [self.leaf]},
        }
        contract_file = self.plan / "execution-contract.json"
        contract_file.write_text(json.dumps(self.contract))
        (self.plan / "manifest.sha256").write_text(
            runner.sha256_file(contract_file) + "  execution-contract.json\n")
        self.candidate = dict(heads={r: "a" * 40 for r in
                                    ("imboy", "imboyapp", "imboyadmin")},
                              owned_tree_digest="b" * 64, migration_digest="c" * 64,
                              plan_manifest_sha256=runner.sha256_file(self.plan / "manifest.sha256"))
        self.candidate["repository_paths"] = {}
        files = {}
        for repo in runner.REPO_NAMES:
            root = self.run / "repos" / repo
            root.mkdir(parents=True)
            (root / "fixture.txt").write_text("synthetic source\n")
            if repo == "imboy":
                migrations = root / "priv/migrations"
                migrations.mkdir(parents=True)
                (migrations / "00000001_fixture.up.sql").write_text("SELECT 1;\n")
            for command in (["git", "init", "-q"], ["git", "add", "."],
                            ["git", "-c", "user.name=leeyi", "-c",
                             "user.email=leeyisoft@qq.com", "commit", "-q", "-m", "synthetic fixture"]):
                subprocess.run(command, cwd=root, check=True, capture_output=True)
            self.candidate["repository_paths"][repo] = str(root)
            self.candidate["heads"][repo] = subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=root, text=True).strip()
            files[repo + "/fixture.txt"] = runner.sha256_file(root / "fixture.txt")
        self.candidate["owned_paths_sha256"] = files
        self.candidate["owned_tree_digest"] = runner.sha256_bytes(runner.canonical_json(files).encode())
        migrations = {"00000001_fixture.up.sql": runner.sha256_file(
            self.run / "repos/imboy/priv/migrations/00000001_fixture.up.sql")}
        self.candidate["migration_digest"] = runner.sha256_bytes(runner.canonical_json(migrations).encode())
        auth = self.run / "authorization.json"
        auth.write_text(json.dumps({"implementation_allowed": True, "synthetic_fixture": True}))
        self.state = dict(plan_dir=str(self.plan), revision=1,
                          plan_manifest_sha256=self.candidate["plan_manifest_sha256"],
                          gates={"G4": "NOT_STARTED"},
                          verification_candidate=self.candidate,
                          authorization_path=str(auth), authorization_sha256=runner.sha256_file(auth),
                          tasks={"W4-02": {"state": "PASS"}})
        self.state_file = self.run / "state.json"
        self.state_file.write_text(json.dumps(self.state))
        self.case_dir = self.run / "evidence/W4-02/worker"
        self.case_dir.mkdir(parents=True)
        self.raw = self.case_dir / "raw.log"
        self.raw.write_text("independent scenario observation\n")
        self.receipt = dict(self.leaf, required=True, task_id="W4-02", case_id="W4-02/worker",
                            executed=1, passed=1, failed=0, skipped=0, exit_code=0,
                            raw_output_path=str(self.raw),
                            raw_output_sha256=runner.sha256_file(self.raw))

    def verify(self, receipt=None):
        (self.case_dir / "leaf-001.json").write_text(json.dumps(receipt or self.receipt))
        with contextlib.redirect_stdout(io.StringIO()):
            return runner.cmd_verify(types.SimpleNamespace(run_dir=str(self.run), gate="G4"))

    def freeze_fixture(self):
        self.state.pop("verification_candidate")
        self.state["tasks"]["W4-02"]["state"] = "NOT_STARTED"
        auth = Path(self.state["authorization_path"])
        auth.write_text(json.dumps({"implementation_allowed": True, "expires_at": "2999-01-01T00:00:00Z",
            "plan_manifest_sha256": self.state["plan_manifest_sha256"],
            "repository_paths": self.candidate["repository_paths"],
            "resource_allowlist": {"postgres_container": ["attv5-synthetic"]}}))
        self.state["authorization_sha256"] = runner.sha256_file(auth)
        self.state_file.write_text(json.dumps(self.state))
        return types.SimpleNamespace(run_dir=str(self.run))

    def test_freeze_clean_candidate_is_idempotent_and_does_not_pass_tasks(self):
        args = self.freeze_fixture()
        self.assertEqual(0, runner.cmd_freeze_candidate(args))
        frozen = json.loads(self.state_file.read_text())
        self.assertEqual([], runner.candidate_errors(frozen["verification_candidate"]))
        self.assertEqual("NOT_STARTED", frozen["tasks"]["W4-02"]["state"])
        self.assertEqual({"postgres_container": ["attv5-synthetic"]}, frozen["resource_allowlist"])
        before = self.state_file.read_bytes()
        events = (self.run / "events.jsonl").read_bytes()
        self.assertEqual(0, runner.cmd_freeze_candidate(args))
        self.assertEqual(before, self.state_file.read_bytes())
        self.assertEqual(events, (self.run / "events.jsonl").read_bytes())

    def test_freeze_refuses_dirty_worktree_without_mutating_state(self):
        args = self.freeze_fixture()
        (Path(self.candidate["repository_paths"]["imboy"]) / "foreign.txt").write_text("foreign WIP")
        before = self.state_file.read_bytes()
        with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
        self.assertEqual(before, self.state_file.read_bytes())

    def test_freeze_refuses_binding_old_pass_to_new_candidate(self):
        args = self.freeze_fixture()
        self.state["tasks"]["W4-02"]["state"] = "PASS"
        self.state_file.write_text(json.dumps(self.state))
        before = self.state_file.read_bytes()
        with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
        self.assertEqual(before, self.state_file.read_bytes())

    def test_freeze_refuses_verifying_or_previously_attempted_evidence(self):
        args = self.freeze_fixture()
        for status, attempt in [("VERIFYING", 1), ("RETRYABLE", 1), ("READY", 1), ("BLOCKED_EXTERNAL", 0)]:
            with self.subTest(status=status, attempt=attempt):
                self.state["tasks"]["W4-02"].update(state=status, attempt=attempt)
                self.state_file.write_text(json.dumps(self.state))
                before = self.state_file.read_bytes()
                with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
                self.assertEqual(before, self.state_file.read_bytes())
                self.assertFalse((self.run / "events.jsonl").exists())

    def test_freeze_recovers_same_event_after_snapshot_failure(self):
        args = self.freeze_fixture()
        with mock.patch.object(runner.StateStore, "write_snapshot", side_effect=OSError("synthetic crash")):
            with self.assertRaises(OSError): runner.cmd_freeze_candidate(args)
        events = (self.run / "events.jsonl").read_bytes()
        self.assertNotIn("verification_candidate", json.loads(self.state_file.read_text()))
        self.assertEqual(0, runner.cmd_freeze_candidate(args))
        self.assertEqual(events, (self.run / "events.jsonl").read_bytes())
        self.assertIn("verification_candidate", json.loads(self.state_file.read_text()))

    def test_freeze_cannot_replace_crashed_event_with_new_clean_head(self):
        args = self.freeze_fixture()
        with mock.patch.object(runner.StateStore, "write_snapshot", side_effect=OSError("synthetic crash")):
            with self.assertRaises(OSError): runner.cmd_freeze_candidate(args)
        before = self.state_file.read_bytes()
        events = (self.run / "events.jsonl").read_bytes()
        root = Path(self.candidate["repository_paths"]["imboy"])
        (root / "fixture.txt").write_text("changed clean candidate")
        subprocess.run(["git", "add", "fixture.txt"], cwd=root, check=True, capture_output=True)
        subprocess.run(["git", "-c", "user.name=leeyi", "-c", "user.email=leeyisoft@qq.com", "commit", "-qm", "synthetic changed candidate"], cwd=root, check=True, capture_output=True)
        with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
        self.assertEqual(before, self.state_file.read_bytes())
        self.assertEqual(events, (self.run / "events.jsonl").read_bytes())

    def test_old_suite_flag_receipt_cannot_pass(self):
        before = self.state_file.read_bytes()
        self.assertNotEqual(0, self.verify())
        self.assertEqual(before, self.state_file.read_bytes())

    def test_duplicate_identity_is_not_exact_set(self):
        self.assertFalse(runner.v_leaf_set(["one"], ["one", "one"])[0])

    def test_counts_reject_boolean_and_fraction(self):
        for value in (True, 1.0, "1", -1):
            row = dict(self.receipt, executed=value, passed=value)
            self.assertFalse(runner.v_execution_counts(row)[0], value)

    def bound_receipt(self):
        row = copy.deepcopy(self.receipt)
        row.update(candidate_heads=self.candidate["heads"],
                   owned_tree_digest=self.candidate["owned_tree_digest"],
                   migration_digest=self.candidate["migration_digest"],
                   plan_manifest_sha256=self.candidate["plan_manifest_sha256"],
                   command="owned scenario harness", cwd=str(self.run),
                   started_at="2026-10-15T08:00:00+08:00",
                   finished_at="2026-10-15T08:00:01+08:00",
                   observation_deadline_seconds=1800,
                   oracle_result="PASS", request_ids=["synthetic-request"],
                   resource_ids=["owned-garage"],
                   actor_provenance={"kind": "real", "driver": "garage", "resource_kind": "storage",
                                     "resource_id": "owned-garage"},
                   oracle_observations={"pre_db_state": "leased",
                       "pre_object_hash": "d" * 64, "action": "delete then drop ACK",
                       "expected_http_semantics": "no HTTP request in worker",
                       "expected_db_delta": "retry then done",
                       "expected_object_state": "not_found", "proof_actor": "worker"})
        raw = dict(row)
        raw["assertions"] = [{"id": "delete_ack_lost", "passed": True}]
        self.raw.write_text(json.dumps(raw))
        row["raw_output_sha256"] = runner.sha256_file(self.raw)
        self.state["resource_allowlist"] = {"storage": ["owned-garage"]}
        self.state_file.write_text(json.dumps(self.state))
        return row

    def test_valid_bound_scenario_receipt_is_read_only(self):
        row = self.bound_receipt()
        before = self.state_file.read_bytes()
        self.assertEqual(0, self.verify(row))
        self.assertEqual(before, self.state_file.read_bytes())
        self.assertFalse((self.run / "events.jsonl").exists())
        for field, value in (("raw_output_sha256", "0" * 64),
                             ("candidate_heads", {"imboy": "f" * 40}),
                             ("actor_provenance", {"kind": "mock"}),
                             ("oracle_observations", {}),
                             ("resource_ids", ["unowned-object-store"]),
                             ("case_id", "W4-02/renamed")):
            bad = dict(row, **{field: value})
            self.assertNotEqual(0, self.verify(bad), field)

    def test_required_and_unstructured_observations_fail_closed(self):
        row = self.bound_receipt()
        for value in (None, False, "true"):
            self.assertNotEqual(0, self.verify(dict(row, required=value)))
        self.raw.write_text("not measured JSON")
        row["raw_output_sha256"] = runner.sha256_file(self.raw)
        self.assertNotEqual(0, self.verify(row))

    def test_unlisted_dirty_source_is_rejected(self):
        row = self.bound_receipt()
        (self.run / "repos/imboyapp/unlisted.py").write_text("changed")
        self.assertNotEqual(0, self.verify(row))

    def test_malformed_candidate_and_vector_fail_closed(self):
        self.assertTrue(runner.candidate_errors([]))
        self.assertTrue(runner._accepted_vector_errors(
            {repo: None for repo in runner.REPO_NAMES}, self.candidate))

    def test_accept_gate_seals_candidate(self):
        self.verify(self.bound_receipt())
        args = types.SimpleNamespace(run_dir=str(self.run), gate="G4")
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, runner.cmd_accept_gate(args))
        state = json.loads(self.state_file.read_text())
        self.assertEqual("PASS", state["gates"]["G4"])
        self.assertEqual(self.candidate, state["gate_candidates"]["G4"])

    def test_storage_resource_cannot_impersonate_physical_device(self):
        row = self.bound_receipt()
        row.update(task_id="W6-02", case_id="W6-02/flutter-recovery-real",
                   device_provenance={"kind": "physical", "resource_id": "owned-garage",
                                      "process_id": 99999999})
        errors = runner.receipt_errors(row, self.leaf, "W6-02", row["case_id"],
            self.candidate, str(self.run), self.state["resource_allowlist"])
        self.assertTrue(any("device_provenance" in reason for reason in errors))

    def test_incomplete_vector_is_rejected(self):
        self.assertTrue(runner._accepted_vector_errors({}, self.candidate))

    def test_ready_rejects_dirty_candidate_and_unfrozen_nonbootstrap(self):
        contract = {"tasks": [{"task_id": "W5-01", "wave": "W5", "dependencies": []}],
                    "wave_gates": {}}
        state = dict(tasks={"W5-01": {"state": "NOT_STARTED"}}, gates={},
                     verification_candidate=self.candidate)
        self.assertEqual(1, len(runner._compute_ready(state, contract)))
        (self.run / "repos/imboyapp/unlisted.py").write_text("dirty")
        self.assertEqual([], runner._compute_ready(state, contract))
        del state["verification_candidate"]
        self.assertEqual([], runner._compute_ready(state, contract))

    def test_accept_task_rejects_missing_expired_deadline_and_invalid_attempt(self):
        for fields in ({"attempt": 1}, {"attempt": 1, "task_deadline": time.time() - 1},
                       {"attempt": 6, "task_deadline": time.time() + 60}):
            self.state["tasks"]["W4-02"] = dict(state="COMMITTING", **fields)
            self.state_file.write_text(json.dumps(self.state))
            args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", commit_vector="unused")
            with self.assertRaises(SystemExit) as result:
                runner.cmd_accept_task(args)
            self.assertEqual(30, result.exception.code)

    def task_args(self, to):
        return types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", to=to,
                                     case_results=None, commit_vector=None, blocked_reason=None)

    def test_manual_pass_without_proofs_is_rejected(self):
        self.state["tasks"]["W4-02"] = {"state": "COMMITTING"}
        self.state_file.write_text(json.dumps(self.state))
        with self.assertRaises(SystemExit) as result:
            runner.cmd_task(self.task_args("PASS"))
        self.assertEqual(20, result.exception.code)

    def test_attempt_budget_and_deadline_cannot_reset(self):
        for fields in ({"attempt": 5}, {"attempt": 1, "task_started_at": time.time() - 40000}):
            self.state["tasks"]["W4-02"] = dict(state="READY", **fields)
            self.state_file.write_text(json.dumps(self.state))
            with self.assertRaises(SystemExit) as result:
                runner.cmd_task(self.task_args("RUNNING"))
            self.assertEqual(30, result.exception.code)

    def test_candidate_bytes_drift_even_without_commit_is_rejected(self):
        row = self.bound_receipt()
        (self.run / "repos/imboyapp/fixture.txt").write_text("changed source\n")
        self.assertNotEqual(0, self.verify(row))

    def test_real_storage_actor_cannot_replace_physical_device_or_browser(self):
        row = self.bound_receipt()
        for cid in ("W6-02/flutter-recovery-real", "W6-02/admin-recovery-real"):
            row["task_id"], row["case_id"] = "W6-02", cid
            errors = runner.receipt_errors(row, self.leaf, "W6-02", cid,
                self.candidate, str(self.run), self.state["resource_allowlist"])
            self.assertTrue(any("provenance" in reason for reason in errors))

    def test_accept_task_requires_real_artifacts_and_complete_vector(self):
        row = self.bound_receipt()
        self.state["tasks"]["W4-02"] = {"state": "COMMITTING", "attempt": 5, "task_deadline": time.time() + 60}
        self.state_file.write_text(json.dumps(self.state))
        (self.case_dir / "leaf-001.json").write_text(json.dumps(row))
        vector = {repo: {"commit": head, "parent": head, "unchanged": True}
                  for repo, head in self.candidate["heads"].items()}
        path = self.run / "vector.json"
        path.write_text(json.dumps(vector))
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", commit_vector=str(path))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, runner.cmd_accept_task(args))
        self.assertEqual("PASS", json.loads(self.state_file.read_text())["tasks"]["W4-02"]["state"])


if __name__ == "__main__":
    unittest.main()
