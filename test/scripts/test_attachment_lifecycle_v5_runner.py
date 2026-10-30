"""The real verify command must reject fabricated or stale leaf receipts."""
import contextlib
import builtins
import copy
import datetime
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
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
        self.run = Path(self.tmp.name).resolve()
        self.plan = self.run / "plan"
        self.plan.mkdir()
        self.leaf = dict(ac_id="AC07", domain="ordinary", driver="garage",
                         entrypoint="ordinary", ordering_or_fault="delete_ack_lost",
                         schema_phase="integrated")
        self.contract = {
            "wave_gates": {"G3": {"wave": "W3", "requires_tasks": [], "previous_gate": None},
                           "G4": {"wave": "W4", "requires_tasks": ["W4-02"], "previous_gate": "G3"}},
            "tasks": [{"task_id": "W4-02", "wave": "W4", "case_ids": ["W4-02/worker"]}],
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
        now = datetime.datetime.now(datetime.timezone.utc)
        auth.write_text(json.dumps({"implementation_allowed": True, "local_commit_allowed": True,
                                    "allowed_tasks": ["W4-02"],
                                    "plan_manifest_sha256": self.candidate["plan_manifest_sha256"],
                                    "issued_at": (now - datetime.timedelta(minutes=1)).isoformat(),
                                    "expires_at": (now + datetime.timedelta(days=1)).isoformat(),
                                    "synthetic_fixture": True}))
        self.state = dict(plan_dir=str(self.plan), revision=1,
                          plan_manifest_sha256=self.candidate["plan_manifest_sha256"],
                          gates={"G3": "PASS", "G4": "NOT_STARTED"},
                          gate_candidates={"G3": self.candidate},
                          verification_candidate=self.candidate,
                          authorization_path=str(auth), authorization_sha256=runner.sha256_file(auth),
                          tasks={"W4-02": {"state": "PASS"}})
        self.state_file = self.run / "state.json"
        store = runner.StateStore(str(self.run))
        store.append_event("admit", {"synthetic": True})
        self.state = store.write_snapshot(self.state)
        self.case_dir = self.run / "evidence/W4-02/worker"
        self.case_dir.mkdir(parents=True)
        self.raw = self.case_dir / "raw.log"
        self.raw.write_text("independent scenario observation\n")
        self.receipt = dict(self.leaf, required=True, task_id="W4-02", case_id="W4-02/worker",
                            task_attempt=1, executed=1, passed=1, failed=0, skipped=0, exit_code=0,
                            raw_output_path=str(self.raw),
                            raw_output_sha256=runner.sha256_file(self.raw))

    def prepare_synthetic_launch(self, task_id="W4-02", case_id="W4-02/worker"):
        # Isolated fixture reproduces the controller's durable launch intent.
        now = time.time()
        budgets = self.state.setdefault("time_budgets", {"run": None, "waves": {}})
        if budgets.get("run") is None:
            budgets["run"] = {"started_at": now, "deadline": now + 604800}
        budgets.setdefault("waves", {}).setdefault(task_id[:2],
            {"started_at": now, "deadline": now + 86400})
        self.state["clock_anchor"] = {"boot": runner._boot_id(), "wall": now,
                                      "monotonic": time.clock_gettime(time.CLOCK_MONOTONIC)}
        launch = {"task_id": task_id, "case_id": case_id, "attempt": self.state["tasks"][task_id]["attempt"],
                  "candidate_digest": runner.sha256_bytes(runner.canonical_json(self.candidate).encode()),
                  "command_started_at": now, "command_deadline": now + 60}
        self.state["active_launch"] = dict(launch, status="PREPARED")
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state, "launch_intent", {"launch": launch})
        return launch

    def seal_as_controller(self, args, launch, guard=lambda: None):
        lock = runner.RunLock(str(self.run))
        self.assertTrue(lock.acquire_nb())
        try:
            return runner.cmd_seal_case(args, held_lock=True, deadline_guard=guard, launch_binding=launch)
        finally:
            lock.release()

    def synthetic_legacy_anchor(self):
        # Test-only anchor, computed after the synthetic historical event exists.
        # It is deliberately not written to the frozen contract or authorization.
        event = list(runner.StateStore(str(self.run)).iter_events())[-1]
        original = runner._load_bound_contract
        def load(state):
            contract = copy.deepcopy(original(state))
            contract["legacy_case_event_hashes"] = {event["payload"]["case_id"]: event["entry_hash"]}
            return contract
        return mock.patch.object(runner, "_load_bound_contract", side_effect=load)

    def prepare_case(self, receipt=None):
        (self.case_dir / "case-commit.json").unlink(missing_ok=True)
        fd = os.open(str(self.case_dir), os.O_RDONLY | os.O_DIRECTORY)
        try:
            runner.case_store.publish(fd, [receipt or self.receipt])
            self.state.setdefault("case_manifest_digests", {})["W4-02/worker"] = runner.case_store.manifest_digest(fd)
            self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        finally:
            os.close(fd)
    def verify(self, receipt=None, prepared=False):
        if not prepared:
            self.prepare_case(receipt)
        with contextlib.redirect_stdout(io.StringIO()):
            return runner.cmd_verify(types.SimpleNamespace(run_dir=str(self.run), gate="G4"))

    def test_inspect_phase_rechecks_deadlines_after_candidate_hashing(self):
        now = time.time()
        self.state["tasks"]["W4-02"] = {
            "state": "RUNNING", "attempt": 1, "task_deadline": now + 5}
        self.state["time_budgets"] = {"run": None, "waves": {}}
        clock = [now]
        calls = []

        def hashing(_candidate):
            calls.append(True)
            if len(calls) == 2:
                clock[0] = now + 6
            return []

        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id=None)
        output = io.StringIO()
        with mock.patch.object(runner, "_require_state", return_value=self.state), \
             mock.patch.object(runner, "candidate_errors", side_effect=hashing), \
             mock.patch.object(runner.time, "time", side_effect=lambda: clock[0]), \
             contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()), \
             self.assertRaises(SystemExit) as exc:
            runner.cmd_inspect_phase(args)
        self.assertEqual(2, len(calls))
        self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
        self.assertEqual("", output.getvalue())

    def test_inspect_phase_rechecks_authorization_after_candidate_hashing(self):
        self.state["tasks"]["W4-02"] = {
            "state": "RUNNING", "attempt": 1, "task_deadline": time.time() + 120}
        self.state["time_budgets"] = {"run": None, "waves": {}}
        calls = []
        auth = Path(self.state["authorization_path"])

        def hashing(_candidate):
            calls.append(True)
            if len(calls) == 2:
                auth.write_text(auth.read_text() + " ")
            return []

        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id=None)
        output = io.StringIO()
        with mock.patch.object(runner, "_require_state", return_value=self.state), \
             mock.patch.object(runner, "candidate_errors", side_effect=hashing), \
             contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()), \
             self.assertRaises(SystemExit) as exc:
            runner.cmd_inspect_phase(args)
        self.assertEqual(2, len(calls))
        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
        self.assertEqual("", output.getvalue())

    def test_inspect_phase_rejects_clock_or_boot_change_during_hashing(self):
        now = time.time()
        self.state["tasks"]["W4-02"] = {
            "state": "RUNNING", "attempt": 1, "task_deadline": now + 120}
        self.state["time_budgets"] = {"run": None, "waves": {}}
        self.state["clock_anchor"] = {"boot": "boot-a", "wall": now - 1, "monotonic": 100}
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id=None)
        for failure in ("rollback", "reboot"):
            with self.subTest(failure=failure):
                clock = {"wall": now, "mono": 101, "boot": "boot-a", "calls": 0}

                def hashing(_candidate):
                    clock["calls"] += 1
                    if clock["calls"] == 2:
                        clock["mono"] = 102
                        if failure == "rollback":
                            clock["wall"] = now - 5
                        else:
                            clock["boot"] = "boot-b"
                    return []

                output = io.StringIO()
                with mock.patch.object(runner, "_require_state", return_value=self.state), \
                     mock.patch.object(runner, "candidate_errors", side_effect=hashing), \
                     mock.patch.object(runner, "_boot_id", side_effect=lambda: clock["boot"]), \
                     mock.patch.object(runner.time, "time", side_effect=lambda: clock["wall"]), \
                     mock.patch.object(runner.time, "clock_gettime", side_effect=lambda _kind: clock["mono"]), \
                     contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()), \
                     self.assertRaises(SystemExit) as exc:
                    runner.cmd_inspect_phase(args)
                self.assertEqual(2, clock["calls"])
                self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)
                self.assertEqual("", output.getvalue())

    def test_inspect_phase_rejects_transient_contract_substitution(self):
        self.state["tasks"]["W4-02"] = {
            "state": "RUNNING", "attempt": 1, "task_deadline": time.time() + 120}
        self.state["time_budgets"] = {"run": None, "waves": {}}
        original_open = builtins.open
        contract_path = self.plan / "execution-contract.json"
        altered = copy.deepcopy(self.contract)
        altered["expected_leaf_matrix"]["W4-02/worker"][0]["schema_phase"] = "base"
        substituted = json.dumps(altered).encode()
        substituted_reads = []

        def open_once(path, mode="r", *args, **kwargs):
            if Path(path) == contract_path and mode == "rb" and not substituted_reads:
                substituted_reads.append(True)
                return io.BytesIO(substituted)
            return original_open(path, mode, *args, **kwargs)

        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id=None)
        output = io.StringIO()
        with mock.patch.object(runner, "_require_state", return_value=self.state), \
             mock.patch("builtins.open", side_effect=open_once), \
             contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()), \
             self.assertRaises(SystemExit) as exc:
            runner.cmd_inspect_phase(args)
        self.assertEqual([True], substituted_reads)
        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
        self.assertEqual("", output.getvalue())
        self.assertEqual(self.contract, json.loads(contract_path.read_text()))

    def freeze_fixture(self):
        self.state.pop("verification_candidate")
        self.state["gates"]["G3"] = "NOT_STARTED"
        self.state.pop("gate_candidates", None)
        self.state["tasks"]["W4-02"]["state"] = "NOT_STARTED"
        auth = Path(self.state["authorization_path"])
        now = datetime.datetime.now(datetime.timezone.utc)
        auth.write_text(json.dumps({"implementation_allowed": True, "local_commit_allowed": True,
            "allowed_tasks": ["W4-02"],
            "issued_at": (now - datetime.timedelta(minutes=1)).isoformat(),
            "expires_at": (now + datetime.timedelta(days=1)).isoformat(),
            "plan_manifest_sha256": self.state["plan_manifest_sha256"],
            "repository_paths": self.candidate["repository_paths"],
            "resource_allowlist": {"postgres_container": ["attv5-synthetic"]}}))
        self.state["authorization_sha256"] = runner.sha256_file(auth)
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
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

    def test_candidate_checks_each_git_index_once_and_detects_file_drift(self):
        args = self.freeze_fixture()
        self.assertEqual(0, runner.cmd_freeze_candidate(args))
        candidate = json.loads(self.state_file.read_text())["verification_candidate"]
        with mock.patch.object(runner, "_ls_files_modes", wraps=runner._ls_files_modes) as modes:
            self.assertEqual([], runner.candidate_errors(candidate))
            self.assertLessEqual(modes.call_count, 3)
        name = next(iter(candidate["owned_paths_sha256"]))
        repo, path = name.split("/", 1)
        target = Path(candidate["repository_paths"][repo]) / path
        target.write_bytes(target.read_bytes() + b"drift")
        self.assertTrue(any("drift" in e for e in runner.candidate_errors(candidate)))

    def test_freeze_refuses_dirty_worktree_without_mutating_state(self):
        args = self.freeze_fixture()
        (Path(self.candidate["repository_paths"]["imboy"]) / "foreign.txt").write_text("foreign WIP")
        before = self.state_file.read_bytes()
        with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
        self.assertEqual(before, self.state_file.read_bytes())

    def test_freeze_refuses_binding_old_pass_to_new_candidate(self):
        args = self.freeze_fixture()
        self.state["tasks"]["W4-02"]["state"] = "PASS"
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        before = self.state_file.read_bytes()
        with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
        self.assertEqual(before, self.state_file.read_bytes())

    def test_freeze_refuses_verifying_or_previously_attempted_evidence(self):
        args = self.freeze_fixture()
        for status, attempt in [("VERIFYING", 1), ("RETRYABLE", 1), ("READY", 1), ("BLOCKED_EXTERNAL", 0)]:
            with self.subTest(status=status, attempt=attempt):
                self.state["tasks"]["W4-02"].update(state=status, attempt=attempt)
                self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
                before = self.state_file.read_bytes()
                events_before = (self.run / "events.jsonl").read_bytes()
                with self.assertRaises(SystemExit): runner.cmd_freeze_candidate(args)
                self.assertEqual(before, self.state_file.read_bytes())
                self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())

    def test_freeze_blocks_after_snapshot_failure_until_reconciled(self):
        args = self.freeze_fixture()
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("synthetic crash")):
            with self.assertRaises(OSError): runner.cmd_freeze_candidate(args)
        events = (self.run / "events.jsonl").read_bytes()
        self.assertNotIn("verification_candidate", json.loads(self.state_file.read_text()))
        with self.assertRaises(SystemExit) as exc:
            runner.cmd_freeze_candidate(args)
        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
        self.assertEqual(events, (self.run / "events.jsonl").read_bytes())
        self.assertNotIn("verification_candidate", json.loads(self.state_file.read_text()))

    def test_freeze_cannot_replace_crashed_event_with_new_clean_head(self):
        args = self.freeze_fixture()
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("synthetic crash")):
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
        self.prepare_case()
        before = self.state_file.read_bytes()
        self.assertNotEqual(0, self.verify(prepared=True))
        self.assertEqual(before, self.state_file.read_bytes())

    def test_resource_allowlist_rejects_invalid_types(self):
        resource = {"kind": "worker", "id": "worker-1"}
        for invalid in (17, True, {}, None, [], "", ["worker-1", 1]):
            with self.subTest(invalid=invalid):
                self.assertFalse(runner.v_resource(resource, {"worker": invalid})[0])
        self.assertFalse(runner.v_resource({"kind": "worker", "id": None}, {"worker": ["worker-1"]})[0])
        self.assertTrue(runner.v_resource(resource, {"worker": ["worker-1"]})[0])

    def test_resource_categories_require_exact_lists_except_disk_root(self):
        for kind in ("scratch_postgres", "garage_bucket", "device_provenance", "worker"):
            with self.subTest(kind=kind):
                resource = {"kind": kind, "id": "owned-1"}
                self.assertFalse(runner.v_resource(resource, {kind: "owned"})[0])
                self.assertFalse(runner.v_resource(resource, {kind: ["owned"]})[0])
                self.assertTrue(runner.v_resource(resource, {kind: ["owned-1"]})[0])
        disk = {"kind": "disk_root", "id": "/tmp/owned/run"}
        self.assertTrue(runner.v_resource(disk, {"disk_root": "/tmp/owned"})[0])
        self.assertFalse(runner.v_resource(disk, {"disk_root": ["/tmp/owned/run"]})[0])

    def test_disk_resource_rejects_neighbors_relative_paths_and_escaping_symlinks(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "owned"
            root.mkdir()
            inside = root / "run"
            inside.mkdir()
            outside = Path(tmp) / "owned-foreign"
            outside.mkdir()
            (root / "link").symlink_to(outside, target_is_directory=True)
            allow = {"disk_root": str(root)}
            for path in (outside, root / "link", "owned/run"):
                with self.subTest(path=path):
                    self.assertFalse(runner.v_resource({"kind": "disk_root", "id": str(path)}, allow)[0])
            self.assertFalse(runner.v_resource({"kind": "disk_root", "id": str(inside)},
                                               {"disk_root": "owned"})[0])
            self.assertTrue(runner.v_resource({"kind": "disk_root", "id": str(inside)}, allow)[0])

    def test_duplicate_identity_is_not_exact_set(self):
        self.assertFalse(runner.v_leaf_set(["one"], ["one", "one"])[0])

    def test_w403_self_asserted_http_provenance_cannot_pass_verifier(self):
        row = self.bound_receipt()
        row.update(task_id="W6-01", case_id="W6-01/real-pg-garage-disk",
                   entrypoint_provenance={"entrypoint": self.leaf["entrypoint"],
                                          "route": "/invented", "method": "GET",
                                          "handler": "fake", "request_id": "synthetic-request"})
        expected = dict(self.leaf, source_case_ref="W4-03/driver-content-immutable")
        for legacy in (False, True):
            errors = runner.receipt_errors(row, expected, "W6-01", "W6-01/real-pg-garage-disk",
                                           self.candidate, str(self.run), self.state["resource_allowlist"],
                                           self.contract, 1, legacy=legacy)
            self.assertIn("W4-03 HTTP and object provenance is not independently bound to the server", errors)
            w602 = runner.receipt_errors(row, self.leaf, "W6-02", "W6-02/e2e-compat",
                                         self.candidate, str(self.run), self.state["resource_allowlist"],
                                         self.contract, 1, legacy=legacy)
            self.assertIn("W6-02 provenance is not bound to its driver session and owned backend", w602)
            mismatch = runner.receipt_errors(row, expected, "W4-03", "W6-01/real-pg-garage-disk",
                                            self.candidate, str(self.run), self.state["resource_allowlist"],
                                            self.contract, 1, legacy=legacy)
            self.assertEqual(["case does not belong to task"], mismatch)

    def test_counts_reject_boolean_and_fraction(self):
        for value in (True, 1.0, "1", -1):
            row = dict(self.receipt, executed=value, passed=value)
            self.assertFalse(runner.v_execution_counts(row)[0], value)

    def bound_receipt(self):
        self.state["tasks"]["W4-02"].setdefault("attempt", 1)
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
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        return row

    def test_valid_bound_scenario_receipt_is_read_only(self):
        row = self.bound_receipt()
        self.prepare_case(row)
        before = self.state_file.read_bytes()
        events_before = (self.run / "events.jsonl").read_bytes()
        self.assertEqual(0, self.verify(row, prepared=True))
        self.assertEqual(before, self.state_file.read_bytes())
        self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())
        for field, value in (("raw_output_sha256", "0" * 64),
                             ("candidate_heads", {"imboy": "f" * 40}),
                             ("actor_provenance", {"kind": "mock"}),
                             ("oracle_observations", {}),
                             ("resource_ids", ["unowned-object-store"]),
                             ("case_id", "W4-02/renamed")):
            bad = dict(row, **{field: value})
            self.assertNotEqual(0, self.verify(bad), field)

    def test_case_binding_crash_recovers_only_unchanged_complete_evidence(self):
        row = self.bound_receipt()
        deadline = time.time() + 120
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": deadline}
        wave_start = time.time() - 86400 + 60
        self.state["time_budgets"] = {"run": None, "waves": {"W4": {"started_at": wave_start, "deadline": wave_start + 86400}}}
        run_start = time.time()
        self.state["time_budgets"]["run"] = {"started_at": run_start, "deadline": run_start + 604800}
        self.state["run_id"] = self.run.name
        store = runner.StateStore(str(self.run))
        self.state = store.write_snapshot(self.state)
        fd = os.open(str(self.case_dir), os.O_RDONLY | os.O_DIRECTORY)
        try:
            runner.case_store.publish(fd, [row])
            digest = runner.case_store.manifest_digest(fd)
        finally:
            os.close(fd)
        new = copy.deepcopy(self.state)
        new["case_manifest_digests"] = {"W4-02/worker": digest}
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
            with self.assertRaises(OSError):
                store.write_snapshot(new, "case_manifest_bound", {
                    "case_id": "W4-02/worker", "manifest_sha256": digest,
                    "candidate": self.candidate})
        anchor = self.synthetic_legacy_anchor()
        anchor.start()
        self.addCleanup(anchor.stop)
        snapshot_before = self.state_file.read_bytes()
        events_before = (self.run / "events.jsonl").read_bytes()
        with mock.patch.object(runner.time, "time", return_value=deadline + 1):
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run)))
        self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
        self.assertEqual(snapshot_before, self.state_file.read_bytes())
        self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())
        with mock.patch.object(runner.time, "time", return_value=wave_start + 86401):
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run)))
        self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
        self.assertEqual(snapshot_before, self.state_file.read_bytes())
        self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())
        original_raw = self.raw.read_bytes()
        self.raw.write_text("changed after crash")
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
            runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run)))
        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
        self.assertEqual(snapshot_before, self.state_file.read_bytes())
        self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())
        self.raw.write_bytes(original_raw)
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_OK, runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run))))
        recovered = json.loads(self.state_file.read_text())
        self.assertEqual(digest, recovered["case_manifest_digests"]["W4-02/worker"])
        self.assertEqual(1, recovered["tasks"]["W4-02"]["attempt"])

    def test_seal_binds_attempt_and_never_falls_back_to_legacy_directory(self):
        row = self.bound_receipt()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1,
                                          "task_deadline": time.time() + 120}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        case_dir = self.case_dir / "attempt-1"
        case_dir.mkdir()
        raw = case_dir / "raw.json"
        raw.write_bytes(self.raw.read_bytes())
        row["raw_output_path"] = str(raw)
        row["raw_output_sha256"] = runner.sha256_file(raw)
        receipts = case_dir / "measured-receipts.json"
        receipts.write_text(json.dumps([row]))
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id="W4-02/worker",
                                     case_dir=str(case_dir), receipts=str(receipts), command=row["command"])
        launch = self.prepare_synthetic_launch()
        def reject_deadline():
            raise RuntimeError("command deadline")
        with self.assertRaisesRegex(RuntimeError, "command deadline"):
            self.seal_as_controller(args, launch, reject_deadline)
        self.assertFalse((case_dir / "case-commit.json").exists())
        calls = []
        def expire_before_binding():
            calls.append(1)
            if len(calls) == 2:
                raise RuntimeError("command deadline")
        with self.assertRaisesRegex(RuntimeError, "command deadline"):
            self.seal_as_controller(args, launch, expire_before_binding)
        self.assertTrue((case_dir / "case-commit.json").exists())
        self.assertNotIn("W4-02/worker", json.loads(self.state_file.read_text()).get("case_manifest_digests", {}))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_OK, self.seal_as_controller(args, launch))
        state = json.loads(self.state_file.read_text())
        self.assertEqual(1, state["case_manifest_attempts"]["W4-02/worker"])
        self.assertEqual(str(case_dir), runner._bound_case_dir(str(self.run), state, "W4-02", "W4-02/worker"))
        state["case_manifest_attempts"]["W4-02/worker"] = 2
        missing = runner._bound_case_dir(str(self.run), state, "W4-02", "W4-02/worker")
        with self.assertRaises(OSError):
            runner._read_case_rows(str(self.run), missing, state["case_manifest_digests"]["W4-02/worker"])

    def test_attempt_case_binding_recovers_after_snapshot_crash(self):
        row = self.bound_receipt()
        now = time.time()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": now + 120}
        self.state["time_budgets"] = {"run": {"started_at": now, "deadline": now + 604800},
                                      "waves": {"W4": {"started_at": now, "deadline": now + 86400}}}
        self.state["run_id"] = self.run.name
        store = runner.StateStore(str(self.run))
        self.state = store.write_snapshot(self.state)
        case_dir = self.case_dir / "attempt-1"
        case_dir.mkdir()
        fd = os.open(str(case_dir), os.O_RDONLY | os.O_DIRECTORY)
        try:
            runner.case_store.publish(fd, [row])
            digest = runner.case_store.manifest_digest(fd)
        finally:
            os.close(fd)
        new = copy.deepcopy(self.state)
        new["case_manifest_digests"] = {"W4-02/worker": digest}
        new["case_manifest_attempts"] = {"W4-02/worker": 1}
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
            with self.assertRaises(OSError):
                store.write_snapshot(new, "case_manifest_bound", {
                    "case_id": "W4-02/worker", "manifest_sha256": digest,
                    "attempt": 1, "candidate": self.candidate})
        # Without an authorized historical anchor the real validator must fail closed.
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
            runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run)))
        self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)
        anchor = self.synthetic_legacy_anchor()
        anchor.start()
        self.addCleanup(anchor.stop)
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_OK, runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run))))
        state = json.loads(self.state_file.read_text())
        self.assertEqual(1, state["case_manifest_attempts"]["W4-02/worker"])
        self.assertEqual(digest, state["case_manifest_digests"]["W4-02/worker"])

    def test_launch_intent_recovers_without_starting_worker_or_resetting_budget(self):
        now = time.time()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": now + 120}
        self.state["time_budgets"] = {"run": {"started_at": now, "deadline": now + 604800},
                                      "waves": {"W4": {"started_at": now, "deadline": now + 86400}}}
        self.state["run_id"] = self.run.name
        store = runner.StateStore(str(self.run))
        self.state = store.write_snapshot(self.state)
        launch = {"launch_id": str(__import__("uuid").uuid4()), "task_id": "W4-02", "attempt": 1,
                  "candidate_digest": runner.sha256_bytes(runner.canonical_json(self.candidate).encode()),
                  "command_digest": "a" * 64, "prior_revision": self.state["revision"],
                  "owner_token": str(__import__("uuid").uuid4()),
                  "controller_boot": self.state["clock_anchor"]["boot"], "controller_pid": os.getpid(),
                  "controller_start": "synthetic", "status": "INTENT"}
        new = copy.deepcopy(self.state)
        new["active_launch"] = launch
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
            with self.assertRaises(OSError):
                store.write_snapshot(new, "launch_intent", {"launch": launch})
        state = runner._resume_state(str(self.run))
        self.assertEqual(launch, state["active_launch"])
        self.assertEqual(self.state["time_budgets"], state["time_budgets"])
        self.assertEqual([], runner._compute_ready(state, self.contract))
        report = io.StringIO()
        with contextlib.redirect_stdout(report):
            self.assertEqual(runner.EXIT_BLOCKED, runner.cmd_resume(types.SimpleNamespace(run_dir=str(self.run))))
        self.assertEqual("BLOCKED_EXTERNAL", json.loads(report.getvalue())["result"])
        self.assertEqual(launch["launch_id"], json.loads(report.getvalue())["active_launch"]["launch_id"])
        next_report = io.StringIO()
        with contextlib.redirect_stdout(next_report):
            self.assertEqual(runner.EXIT_BLOCKED, runner.cmd_next(types.SimpleNamespace(run_dir=str(self.run))))
        self.assertEqual(launch["launch_id"], json.loads(next_report.getvalue())["active_launch"]["launch_id"])
        before = self.state_file.read_bytes()
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as blocked:
            runner.cmd_task(types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", to="RETRYABLE"))
        self.assertEqual(runner.EXIT_BLOCKED, blocked.exception.code)
        self.assertEqual(before, self.state_file.read_bytes())
        for operation, args in (
                (runner.cmd_accept_gate, types.SimpleNamespace(run_dir=str(self.run), gate="G4")),
                (runner.cmd_accept_task, types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02")),
                (runner.cmd_seal_case, types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02"))):
            with self.subTest(operation=operation.__name__):
                with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    operation(args)
                self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)
                self.assertEqual(before, self.state_file.read_bytes())
        for field, value in (("attempt", 2), ("status", "FINISHED"),
                             ("candidate_digest", "b" * 64), ("controller_pid", 0)):
            bad = dict(launch, **{field: value})
            altered = copy.deepcopy(new)
            altered["revision"] = self.state["revision"] + 1
            altered["active_launch"] = bad
            event = {"payload": {"launch": bad}, "ts": time.time(), "entry_hash": "f" * 64}
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                runner._resume_launch_intent(str(self.run), store, self.state, altered, event)
        changed = copy.deepcopy(state)
        changed["tasks"]["W4-02"]["state"] = "RETRYABLE"
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
            with self.assertRaises(OSError):
                store.write_snapshot(changed, "task_state", {"task": "W4-02", "to": "RETRYABLE", "attempt": 1})
        state_before = self.state_file.read_bytes()
        events_before = (self.run / "events.jsonl").read_bytes()
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
            runner._resume_state(str(self.run))
        self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)
        self.assertEqual(state_before, self.state_file.read_bytes())
        self.assertEqual(events_before, (self.run / "events.jsonl").read_bytes())

    def test_launch_intent_command_deadline_is_bound_to_persisted_budgets(self):
        now = time.time()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": now + 120}
        self.state["time_budgets"] = {"run": {"started_at": now, "deadline": now + 604800},
                                      "waves": {"W4": {"started_at": now, "deadline": now + 86400}}}
        store = runner.StateStore(str(self.run))
        old = store.write_snapshot(self.state)
        launch = {"launch_id": str(__import__("uuid").uuid4()), "task_id": "W4-02", "attempt": 1,
                  "candidate_digest": runner.sha256_bytes(runner.canonical_json(self.candidate).encode()),
                  "command_digest": "a" * 64, "prior_revision": old["revision"],
                  "owner_token": str(__import__("uuid").uuid4()), "controller_boot": old["clock_anchor"]["boot"],
                  "controller_pid": os.getpid(), "controller_start": "synthetic", "status": "INTENT",
                  "case_id": "W4-02/worker", "command_started_at": now, "command_deadline": now + 60}
        new = copy.deepcopy(old)
        new["active_launch"] = launch
        with mock.patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
            with self.assertRaises(OSError):
                store.write_snapshot(new, "launch_intent", {"launch": launch})
        state = runner._resume_state(str(self.run))
        self.assertEqual(launch, state["active_launch"])
        self.assertEqual(old["time_budgets"], state["time_budgets"])
        for field, bad in (("command_deadline", now + 121), ("command_deadline", float("nan")),
                           ("case_id", "W4-02/other")):
            invalid = dict(launch, **{field: bad})
            changed = copy.deepcopy(new)
            changed.update(revision=old["revision"] + 1, active_launch=invalid)
            event = {"payload": {"launch": invalid}, "ts": time.time(), "entry_hash": "f" * 64}
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                runner._resume_launch_intent(str(self.run), store, old, changed, event)
        partial = dict(launch)
        partial.pop("command_deadline")
        changed = copy.deepcopy(new)
        changed.update(revision=old["revision"] + 1, active_launch=partial)
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            runner._resume_launch_intent(str(self.run), store, old, changed,
                                         {"payload": {"launch": partial}, "ts": time.time(), "entry_hash": "f" * 64})

    def test_verify_sealed_case_uses_bound_attempt_and_rejects_tampering(self):
        row = self.bound_receipt()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1,
                                          "task_deadline": time.time() + 120}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        case_dir = self.case_dir / "attempt-1"
        case_dir.mkdir()
        raw = case_dir / "raw.json"
        raw.write_bytes(self.raw.read_bytes())
        row["raw_output_path"] = str(raw)
        row["raw_output_sha256"] = runner.sha256_file(raw)
        receipts = case_dir / "measured-receipts.json"
        receipts.write_text(json.dumps([row]))
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id="W4-02/worker",
                                     case_dir=str(case_dir), receipts=str(receipts), command=row["command"])
        launch = self.prepare_synthetic_launch()
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_OK, self.seal_as_controller(args, launch))
        # The controller launch reconciles (releases its active entry) before
        # the timed case becomes verifiable.
        sealed_state = json.loads(self.state_file.read_text())
        sealed_state.pop("active_launch", None)
        self.state = runner.StateStore(str(self.run)).write_snapshot(
            sealed_state, "launch_finished", {"launch": launch})
        state = json.loads(self.state_file.read_text())
        state["tasks"]["W4-02"]["attempt"] = 2
        runner.StateStore(str(self.run)).write_snapshot(state)
        check = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id="W4-02/worker")
        before_state = self.state_file.read_bytes()
        before_events = (self.run / "events.jsonl").read_bytes()
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_OK, runner.cmd_verify_case(check))
        self.assertEqual(before_state, self.state_file.read_bytes())
        self.assertEqual(before_events, (self.run / "events.jsonl").read_bytes())
        event = runner._sealed_case_binding(str(self.run), "W4-02/worker")
        for bad in (dict(event, attempt=2), dict(event, candidate={}),
                    dict(event, manifest_sha256="0" * 64), None):
            with mock.patch.object(runner, "_sealed_case_binding", return_value=bad), \
                 contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(runner.EXIT_CONTRACT, runner.cmd_verify_case(check))
        with mock.patch.object(runner, "candidate_errors", return_value=["candidate drift"]), \
             contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_CONTRACT, runner.cmd_verify_case(check))
        with mock.patch.object(runner.time, "time", return_value=state["tasks"]["W4-02"]["task_deadline"] + 1), \
             contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
            runner.cmd_verify_case(check)
        self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
        raw.write_text("tampered")
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(runner.EXIT_CONTRACT, runner.cmd_verify_case(check))
        self.assertEqual(before_state, self.state_file.read_bytes())
        self.assertEqual(before_events, (self.run / "events.jsonl").read_bytes())

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

    def test_w602_receipts_cannot_bypass_unbound_session_provenance(self):
        row = self.bound_receipt()
        allowlist = dict(self.state["resource_allowlist"],
                         device_provenance=["synthetic-device"],
                         browser_provenance=["playwright-chromium"])
        row.update(task_id="W6-02",
                   device_provenance={"kind": "physical", "resource_id": "synthetic-device", "process_id": 1},
                   browser_provenance={"kind": "real-browser", "resource_id": "playwright-chromium", "process_id": 1})
        for case in ("flutter-recovery-real", "admin-recovery-real", "e2e-compat", "ac08"):
            with self.subTest(case=case):
                cid = "W6-02/" + case
                row["case_id"] = cid
                errors = runner.receipt_errors(row, self.leaf, "W6-02", cid,
                    self.candidate, str(self.run), allowlist)
                self.assertIn("W6-02 provenance is not bound to its driver session and owned backend", errors)

    def test_ac08_requires_device_but_not_browser(self):
        row = self.bound_receipt()
        row.update(task_id="W6-02", case_id="W6-02/ac08",
                   device_provenance={"kind": "physical", "resource_id": "synthetic-device", "process_id": 1})
        allowlist = dict(self.state["resource_allowlist"], device_provenance=["synthetic-device"])
        errors = runner.receipt_errors(row, self.leaf, "W6-02", row["case_id"],
                                       self.candidate, str(self.run), allowlist)
        self.assertNotIn("missing registered live browser_provenance", errors)
        self.assertIn("W6-02 provenance is not bound to its driver session and owned backend", errors)
        invalid_allowlist = dict(allowlist, device_provenance="synthetic-device")
        invalid_errors = runner.receipt_errors(row, self.leaf, "W6-02", row["case_id"],
                                               self.candidate, str(self.run), invalid_allowlist)
        self.assertIn("missing registered live device_provenance", invalid_errors)
        for case_id in ("W6-02/admin-recovery-real", "W6-02/e2e-compat"):
            row["case_id"] = case_id
            errors = runner.receipt_errors(row, self.leaf, "W6-02", case_id,
                                           self.candidate, str(self.run), allowlist)
            self.assertIn("missing registered live browser_provenance", errors)

    def test_w602_producer_to_seal_rejects_forged_process_provenance(self):
        cid = "W6-02/e2e-compat"
        self.contract["tasks"].append({"task_id": "W6-02", "case_ids": [cid]})
        self.contract["case_registry"][cid] = {"source_kind": "real-required"}
        self.contract["expected_leaf_matrix"][cid] = [self.leaf]
        contract_file = self.plan / "execution-contract.json"
        contract_file.write_text(json.dumps(self.contract))
        (self.plan / "manifest.sha256").write_text(
            runner.sha256_file(contract_file) + "  execution-contract.json\n")
        digest = runner.sha256_file(self.plan / "manifest.sha256")
        self.candidate["plan_manifest_sha256"] = digest
        self.state["plan_manifest_sha256"] = digest
        auth_path = Path(self.state["authorization_path"])
        auth = json.loads(auth_path.read_text())
        auth.update(plan_manifest_sha256=digest, allowed_tasks=["W4-02", "W6-02"])
        auth_path.write_text(json.dumps(auth))
        self.state["authorization_sha256"] = runner.sha256_file(auth_path)
        self.state["tasks"]["W6-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": time.time() + 120}
        self.state["resource_allowlist"] = {"garage_bucket": ["synthetic-bucket"],
            "device_provenance": ["synthetic-device"], "browser_provenance": ["playwright-chromium"]}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        case_dir = self.run / "evidence/W6-02/e2e-compat/attempt-1"
        case_dir.mkdir(parents=True)
        observations = {name: "synthetic" for name in (
            "pre_db_state", "pre_object_hash", "action", "expected_http_semantics",
            "expected_db_delta", "expected_object_state", "proof_actor")}
        row = {"leaf": self.leaf, "exit_code": 0, "executed": 1, "passed": 1,
               "failed": 0, "skipped": 0, "started_at": "2026-01-01T00:00:00Z",
               "finished_at": "2026-01-01T00:00:01Z", "oracle_observations": observations,
               "assertions": [{"id": "synthetic", "passed": True}], "request_ids": ["synthetic"],
               "actor": {"kind": "real", "driver": "garage", "resource_kind": "garage_bucket",
                         "resource_id": "synthetic-bucket"}, "resource_ids": ["synthetic-bucket"],
               "device_provenance": {"kind": "physical", "resource_id": "synthetic-device", "process_id": 1},
               "browser_provenance": {"kind": "real-browser", "resource_id": "playwright-chromium", "process_id": 1}}
        path = case_dir / "observations.json"
        path.write_text(json.dumps({"leaves": [row]}))
        command = "synthetic proof"
        sentinel = self.run / "producer-sentinel"
        sentinel.write_text("unchanged")
        (case_dir / "raw-001.json").symlink_to(sentinel)
        producer_args = [sys.executable, str(ROOT / "scripts/attachment_lifecycle_v5_producer.py"),
            "produce", "--run-dir", str(self.run), "--task-id", "W6-02", "--case-id", cid,
            "--case-dir", str(case_dir), "--command", command, "--cwd", str(self.run),
            "--observations", str(path)]
        receipts = case_dir / "measured-receipts.json"
        receipts.symlink_to(sentinel)
        conflict = subprocess.run(producer_args, capture_output=True, text=True)
        self.assertNotEqual(0, conflict.returncode)
        self.assertTrue(receipts.is_symlink())
        self.assertEqual("unchanged", sentinel.read_text())
        receipts.unlink()
        # Simulate termination after atomic publication but before directory fsync.
        crash_script = ("import os,sys; sys.path.insert(0,sys.argv.pop(1)); "
            "import attachment_lifecycle_v5_producer as p; orig=p._publish_noreplace; "
            "p._publish_noreplace=lambda *a: (orig(*a),os._exit(91)); p.main()")
        crash = subprocess.run([sys.executable, "-c", crash_script, str(ROOT / "scripts")]
                               + producer_args[2:], capture_output=True, text=True)
        self.assertEqual(91, crash.returncode, crash.stderr)
        self.assertEqual(1, receipts.stat().st_nlink)
        published = receipts.read_bytes()
        retry = subprocess.run(producer_args, capture_output=True, text=True)
        self.assertEqual(0, retry.returncode, retry.stderr)
        self.assertEqual(published, receipts.read_bytes())
        self.assertEqual("unchanged", sentinel.read_text())
        path.unlink()
        path.symlink_to(sentinel)
        linked = subprocess.run(producer_args, capture_output=True, text=True, timeout=5)
        self.assertNotEqual(0, linked.returncode)
        path.unlink()
        os.mkfifo(path)
        fifo = subprocess.run(producer_args, capture_output=True, text=True, timeout=5)
        self.assertNotEqual(0, fifo.returncode)
        path.unlink()
        path.write_text(json.dumps({"leaves": [row]}))
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W6-02", case_id=cid,
            case_dir=str(case_dir), receipts=str(receipts), command=command)
        launch = self.prepare_synthetic_launch("W6-02", cid)
        stale = json.loads(json.dumps(self.state))
        stale["tasks"]["W6-02"]["attempt"] = 2
        _, stale_errors, _, _ = runner._measured_case_rows(args, stale, self.contract, str(self.run))
        self.assertIn("case directory does not match task/case", stale_errors)
        original_state = self.state_file.read_bytes()
        self.state_file.write_text(json.dumps(stale))
        stale_retry = subprocess.run(producer_args, capture_output=True, text=True, timeout=5)
        self.assertNotEqual(0, stale_retry.returncode)
        self.assertEqual(published, receipts.read_bytes())
        self.state_file.write_bytes(original_state)
        before = self.state_file.read_bytes()
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            self.assertEqual(runner.EXIT_CONTRACT, self.seal_as_controller(args, launch))
        self.assertEqual(["W6-02 provenance is not bound to its driver session and owned backend"],
                         json.loads(output.getvalue())["errors"])
        self.assertFalse((case_dir / "leaf-001.json").exists())
        self.assertEqual(before, self.state_file.read_bytes())

    def test_seal_case_rejects_expired_or_invalid_task_attempt_before_writing(self):
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id="W4-02/worker")
        for fields in ({"attempt": 1, "task_deadline": time.time() - 1},
                       {"attempt": 0, "task_deadline": time.time() + 120},
                       {"attempt": 6, "task_deadline": time.time() + 120}):
            self.state["tasks"]["W4-02"] = dict(state="RUNNING", **fields)
            self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
            launch = self.prepare_synthetic_launch()
            before = self.state_file.read_bytes()
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                self.seal_as_controller(args, launch)
            self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
            self.assertEqual(before, self.state_file.read_bytes())
            self.assertFalse((self.case_dir / "manifest.json").exists())

    def test_seal_case_rejects_expired_wave_before_writing(self):
        now = time.time()
        self.state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1, "task_deadline": now + 120}
        self.state["time_budgets"] = {"run": {"started_at": now - 100, "deadline": now - 100 + 604800},
                                     "waves": {"W4": {"started_at": now - 86401, "deadline": now - 1}}}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        launch = self.prepare_synthetic_launch()
        before = self.state_file.read_bytes()
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", case_id="W4-02/worker")
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
            self.seal_as_controller(args, launch)
        self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
        self.assertEqual(before, self.state_file.read_bytes())

    def test_incomplete_vector_is_rejected(self):
        self.assertTrue(runner._accepted_vector_errors({}, self.candidate))

    def test_ready_rejects_dirty_candidate_and_unfrozen_nonbootstrap(self):
        contract = {"tasks": [{"task_id": "W5-01", "wave": "W5", "dependencies": []}],
                    "wave_gates": {"G5": {"wave": "W5", "previous_gate": "G4"}}}
        state = dict(tasks={"W5-01": {"state": "NOT_STARTED"}}, gates={"G4": "PASS"},
                     plan_manifest_sha256=self.candidate["plan_manifest_sha256"],
                     gate_candidates={"G4": self.candidate},
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
            self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
            args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", commit_vector="unused")
            with self.assertRaises(SystemExit) as result:
                runner.cmd_accept_task(args)
            self.assertEqual(30, result.exception.code)

    def task_args(self, to):
        return types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", to=to,
                                     case_results=None, commit_vector=None, blocked_reason=None)

    def test_manual_pass_without_proofs_is_rejected(self):
        self.state["tasks"]["W4-02"] = {"state": "COMMITTING"}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        with self.assertRaises(SystemExit) as result:
            runner.cmd_task(self.task_args("PASS"))
        self.assertEqual(20, result.exception.code)

    def test_attempt_budget_and_deadline_cannot_reset(self):
        for fields in ({"attempt": 5}, {"attempt": 1, "task_started_at": time.time() - 40000}):
            self.state["tasks"]["W4-02"] = dict(state="READY", **fields)
            self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
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
        row["task_attempt"] = 5
        raw = json.loads(self.raw.read_text())
        raw["task_attempt"] = 5
        self.raw.write_text(json.dumps(raw))
        row["raw_output_sha256"] = runner.sha256_file(self.raw)
        self.state["tasks"]["W4-02"] = {"state": "COMMITTING", "attempt": 5, "task_deadline": time.time() + 60}
        self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        fd = os.open(str(self.case_dir), os.O_RDONLY | os.O_DIRECTORY)
        try:
            runner.case_store.publish(fd, [row])
            self.state.setdefault("case_manifest_digests", {})["W4-02/worker"] = runner.case_store.manifest_digest(fd)
            self.state = runner.StateStore(str(self.run)).write_snapshot(self.state)
        finally:
            os.close(fd)
        vector = {repo: {"commit": head, "parent": head, "unchanged": True}
                  for repo, head in self.candidate["heads"].items()}
        path = self.run / "vector.json"
        path.write_text(json.dumps(vector))
        args = types.SimpleNamespace(run_dir=str(self.run), task_id="W4-02", commit_vector=str(path))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, runner.cmd_accept_task(args))
        self.assertEqual("PASS", json.loads(self.state_file.read_text())["tasks"]["W4-02"]["state"])


    def test_readmission_rejects_tampered_snapshot(self):
        run = self.run / "readmission"
        run.mkdir()
        args = types.SimpleNamespace(plan_dir=str(self.plan), run_dir=str(run),
                                     authorization=str(self.run / "authorization.json"))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, runner.cmd_admit(args))
            self.assertEqual(0, runner.cmd_admit(args))
            store = runner.StateStore(str(run))
            state = store.load_state()
            event = store.append_event("plain", {})
            store.persist_snapshot(state, event["entry_hash"])
            self.assertEqual(0, runner.cmd_admit(args))
        state_file = run / "state.json"
        state = json.loads(state_file.read_text())
        state["tasks"]["W4-02"]["state"] = "PASS"
        state_file.write_text(json.dumps(state))
        events_before = (run / "events.jsonl").read_bytes()
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as result:
            runner.cmd_admit(args)
        self.assertEqual(runner.EXIT_CONTRACT, result.exception.code)
        self.assertEqual(events_before, (run / "events.jsonl").read_bytes())
        self.assertEqual("PASS", json.loads(state_file.read_text())["tasks"]["W4-02"]["state"])

    def test_streamed_chain_preserves_binding_and_detects_corruption(self):
        run = self.run / "streamed"
        run.mkdir()
        store = runner.StateStore(str(run))
        self.assertEqual(0, store.scan_chain()["count"])
        store.append_event("admit", {})
        first_state = store.write_snapshot({"revision": 0, "tasks": {}})
        store.append_event("plain", {})
        scan = store.scan_chain()
        self.assertTrue(scan["ok"])
        self.assertTrue(store.snapshot_bound(first_state, [scan["bound"]]))
        self.assertEqual(scan["bound"], scan["previous_bound"])
        second_state = store.write_snapshot(first_state)
        scan = store.scan_chain()
        self.assertTrue(store.snapshot_bound(second_state, [scan["bound"]]))
        self.assertTrue(store.snapshot_bound(first_state, [scan["previous_bound"]]))
        events = (run / "events.jsonl").read_text().splitlines()
        altered = json.loads(events[1])
        altered["payload"]["state_digest"] = "0" * 64
        events[1] = json.dumps(altered)
        (run / "events.jsonl").write_text("\n".join(events) + "\n")
        self.assertFalse(store.scan_chain()["ok"])
        (run / "events.jsonl").write_text("{broken\n")
        with self.assertRaises(ValueError):
            store.scan_chain()


if __name__ == "__main__":
    unittest.main()
