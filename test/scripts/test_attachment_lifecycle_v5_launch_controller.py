"""Synthetic subprocess tests; do not access PostgreSQL or shared services."""
import json
import os
from pathlib import Path
import sys
import time
import unittest
from unittest import mock
import socket
import subprocess
import signal
import fcntl
import threading

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/test"))
sys.path.insert(0, str(ROOT / "scripts"))
import attachment_lifecycle_v5_launch_controller as controller  # noqa: E402
import test_attachment_lifecycle_v5_runner as runner_tests  # noqa: E402


class LaunchTests(unittest.TestCase):
    def setUp(self):
        self.fixture = runner_tests.VerificationTests("runTest")
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        self.root = self.fixture.run
        self.runner = controller.runner
        self.controller = controller.LaunchController(str(self.root))
        self.addCleanup(self.controller.close)
        state = self.fixture.state
        now = time.time()
        state["tasks"]["W4-02"] = {"state": "RUNNING", "attempt": 1,
                                    "task_deadline": now + 120}
        state["time_budgets"] = {"run": {"started_at": now, "deadline": now + 604800},
                                 "waves": {"W4": {"started_at": now, "deadline": now + 86400}}}
        self.store = self.runner.StateStore(str(self.root))
        self.state = self.store.write_snapshot(state)
        self.marker = self.root / "worker-ran"
        self.argv = [sys.executable, "-c", "from pathlib import Path; Path(%r).write_text('ok')" % str(self.marker)]

    def _prepared(self, argv=None, pass_fds=(), command_seconds=None):
        argv = self.argv if argv is None else argv
        intent = self.controller.prepare(self.state, "W4-02", argv, command_seconds=command_seconds)
        state, proc, pipe = self.controller.launch_prepared(self.store, self.state, intent, argv,
                                                       pass_fds=pass_fds)
        return intent, state, proc, pipe

    def test_worker_cannot_run_before_durable_prepared_and_go(self):
        intent, state, proc, pipe = self._prepared()
        self.addCleanup(lambda: proc.poll() is None and proc.kill())
        self.assertEqual("PREPARED", state["active_launch"]["status"])
        self.assertEqual(intent["launch_id"], state["active_launch"]["launch_id"])
        self.assertFalse(self.marker.exists())
        self.controller.release_prepared(pipe, intent, self.argv, case_deadline=intent["command_deadline"])
        deadline = time.monotonic() + 5
        while not self.marker.exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        self.assertEqual("ok", self.marker.read_text())
        self.assertIsNone(proc.poll())
        self.controller.close()
        self.assertIn(proc.wait(timeout=5), (20, 75))

    def test_parent_crash_before_go_closes_pipe_without_worker_action(self):
        _, _, proc, pipe = self._prepared()
        os.close(pipe)
        self.assertEqual(75, proc.wait(timeout=5))
        self.assertFalse(self.marker.exists())

    def test_close_before_go_releases_prepared_worker_without_running_payload(self):
        _, _, proc, _ = self._prepared(command_seconds=30)
        os.kill(proc.pid, signal.SIGSTOP)
        try:
            self.controller.close()
            probe = os.open(self.controller._lock.path, os.O_RDWR)
            try:
                with self.assertRaises(BlockingIOError):
                    fcntl.flock(probe, fcntl.LOCK_EX | fcntl.LOCK_NB)
            finally:
                os.close(probe)
        finally:
            os.kill(proc.pid, signal.SIGCONT)
        self.assertEqual(75, proc.wait(timeout=5))
        self.assertFalse(self.marker.exists())
        probe = os.open(self.controller._lock.path, os.O_RDWR)
        try:
            fcntl.flock(probe, fcntl.LOCK_EX | fcntl.LOCK_NB)
        finally:
            os.close(probe)

    def test_close_waits_for_release_validation_and_go(self):
        intent, _, proc, pipe = self._prepared(command_seconds=30)
        entered, resume, closed = threading.Event(), threading.Event(), threading.Event()
        errors = []
        real_check = self.runner.candidate_errors

        def paused(candidate):
            entered.set()
            if not resume.wait(5):
                raise RuntimeError("test release timeout")
            return real_check(candidate)

        def release():
            try:
                self.controller.release_prepared(pipe, intent, self.argv,
                                                 case_deadline=intent["command_deadline"])
            except BaseException as exc:
                errors.append(exc)

        def close():
            self.controller.close()
            closed.set()

        with mock.patch.object(self.runner, "candidate_errors", side_effect=paused):
            releaser = threading.Thread(target=release)
            closer = threading.Thread(target=close)
            releaser.start()
            try:
                self.assertTrue(entered.wait(5))
                closer.start()
                self.assertFalse(closed.wait(0.2))
            finally:
                resume.set()
                releaser.join(10)
                closer.join(10)
        self.assertFalse(releaser.is_alive())
        self.assertFalse(closer.is_alive())
        self.assertEqual([], errors)
        self.assertTrue(closed.is_set())
        self.assertIn(proc.wait(timeout=5), (20, 75))

    def test_forked_child_rejects_controller_before_inherited_mutex(self):
        acquired, resume = threading.Event(), threading.Event()

        def hold():
            with self.controller._operation_lock:
                acquired.set()
                resume.wait(5)

        thread = threading.Thread(target=hold)
        thread.start()
        self.assertTrue(acquired.wait(5))
        pid = os.fork()
        if pid == 0:
            try:
                self.controller.close()
            except ValueError:
                os._exit(0)
            os._exit(1)
        try:
            deadline = time.monotonic() + 3
            while time.monotonic() < deadline:
                done, status = os.waitpid(pid, os.WNOHANG)
                if done:
                    self.assertEqual(0, os.waitstatus_to_exitcode(status))
                    break
                time.sleep(0.02)
            else:
                os.kill(pid, signal.SIGKILL)
                os.waitpid(pid, 0)
                self.fail("forked child waited on inherited mutex")
        finally:
            resume.set()
            thread.join(5)

    def test_rpc_uses_real_runner_and_rejects_drift_or_unknown_operation(self):
        intent, _, proc, pipe = self._prepared()
        try:
            req = {"op": "inspect-phase", "task_id": "W4-02", "case_id": "W4-02/worker",
                   "attempt": 1, "candidate_digest": intent["candidate_digest"]}
            result = self.controller.dispatch(intent, req, "synthetic command")
            self.assertEqual(0, result["exit_code"], result["stderr"])
            for changed in ({"attempt": 2}, {"candidate_digest": "0" * 64},
                            {"op": "task"}, {"case_id": "W4-02/other"}):
                with self.assertRaises(ValueError):
                    self.controller.dispatch(intent, dict(req, **changed), "synthetic")
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_gated_real_child_calls_runner_over_socket(self):
        parent, child = socket.socketpair()
        self.addCleanup(parent.close)
        self.addCleanup(child.close)
        req = {"op": "inspect-phase", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": controller.digest(self.state["verification_candidate"])}
        code = ("import json,sys;from pathlib import Path;sys.path.insert(0,sys.argv[1]);"
                "import attachment_lifecycle_v5_launch_controller as c;"
                "r=c.client_call(int(sys.argv[2]),json.loads(sys.argv[3]));"
                "Path(sys.argv[4]).write_text(json.dumps(r));sys.exit(r['exit_code'])")
        argv = [sys.executable, "-c", code, str(ROOT / "scripts/test"), str(child.fileno()),
                json.dumps(req), str(self.marker)]
        intent, _, proc, pipe = self._prepared(argv, (child.fileno(),))
        child.close()
        try:
            self.assertFalse(self.marker.exists())
            self.controller.release_prepared(pipe, intent, argv, case_deadline=intent["command_deadline"])
            self.assertEqual(0, self.controller.serve_once(parent, intent, "synthetic"))
            deadline = time.monotonic() + 5
            while not self.marker.exists() and time.monotonic() < deadline:
                time.sleep(0.02)
            response = json.loads(self.marker.read_text())
            self.assertEqual(0, response["exit_code"])
            self.assertIn("W4-02/worker", response["stdout"])
            self.controller.close()
            self.assertIn(proc.wait(timeout=5), (20, 75))
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)

    def test_expired_case_deadline_never_releases_real_child(self):
        intent, _, proc, pipe = self._prepared(command_seconds=1.0)
        deadline = intent["command_deadline"]
        time.sleep(1.1)
        with self.assertRaises(ValueError):
            self.controller.release_prepared(pipe, intent, self.argv, case_deadline=deadline)
        self.assertEqual(75, proc.wait(timeout=5))
        self.assertFalse(self.marker.exists())

    def test_deadline_is_durable_and_cannot_be_extended_at_go(self):
        intent, state, proc, pipe = self._prepared(command_seconds=5)
        self.assertEqual(intent["command_deadline"], state["active_launch"]["command_deadline"])
        self.assertEqual("W4-02/worker", state["active_launch"]["case_id"])
        with self.assertRaises(ValueError):
            self.controller.release_prepared(pipe, intent, self.argv,
                                             case_deadline=intent["command_deadline"] + 1)
        self.assertEqual(75, proc.wait(timeout=5))
        self.assertFalse(self.marker.exists())

    def test_missing_persisted_budget_rejects_before_launch(self):
        for missing in ("run", "wave"):
            state = json.loads(json.dumps(self.state))
            if missing == "run":
                state["time_budgets"]["run"] = None
            else:
                state["time_budgets"]["waves"] = {}
            with self.subTest(missing=missing), self.assertRaises(ValueError):
                self.controller.prepare(state, "W4-02", self.argv)
        self.assertFalse(self.marker.exists())

    def test_command_timeout_stops_delayed_child(self):
        argv = [sys.executable, "-c", "import time;from pathlib import Path;time.sleep(2);Path(%r).write_text('bad')" % str(self.marker)]
        intent, _, proc, pipe = self._prepared(argv, command_seconds=0.8)
        self.controller.release_prepared(pipe, intent, argv, case_deadline=intent["command_deadline"])
        self.assertIn(proc.wait(timeout=5), (20, 75))
        time.sleep(1.4)
        self.assertFalse(self.marker.exists())

    def test_prepare_rejects_deadline_consumed_by_candidate_hash_before_persist(self):
        intent = self.controller.prepare(self.state, "W4-02", self.argv, command_seconds=0.3)
        before_state = (self.root / "state.json").read_bytes()
        before_events = (self.root / "events.jsonl").read_bytes()
        def slow_candidate(_candidate):
            time.sleep(0.4)
            return []
        with mock.patch.object(self.runner, "candidate_errors", side_effect=slow_candidate):
            with self.assertRaises(ValueError):
                self.controller.launch_prepared(self.store, self.state, intent, self.argv)
        self.assertEqual(before_state, (self.root / "state.json").read_bytes())
        self.assertEqual(before_events, (self.root / "events.jsonl").read_bytes())
        self.assertFalse(self.marker.exists())

    def test_rpc_rejected_after_persisted_command_deadline(self):
        intent, _, proc, pipe = self._prepared(command_seconds=1)
        try:
            time.sleep(1.1)
            req = {"op": "inspect-phase", "task_id": "W4-02", "case_id": "W4-02/worker",
                   "attempt": 1, "candidate_digest": intent["candidate_digest"]}
            before = (self.root / "state.json").read_bytes()
            with self.assertRaises(ValueError):
                self.controller.dispatch(intent, req, "synthetic")
            self.assertEqual(before, (self.root / "state.json").read_bytes())
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def _seal_fixture(self):
        self.fixture.state = self.state
        row = self.fixture.bound_receipt()
        self.state = self.fixture.state
        case_dir = self.root / "evidence" / "W4-02" / "worker" / "attempt-1"
        case_dir.mkdir(parents=True, exist_ok=True)
        raw = case_dir / "raw.json"
        raw.write_bytes(self.fixture.raw.read_bytes())
        row["raw_output_path"] = str(raw)
        row["raw_output_sha256"] = self.runner.sha256_file(raw)
        (case_dir / "measured-receipts.json").write_text(json.dumps([row]))
        return row

    def test_seal_pending_fsync_crosses_deadline_never_binds_case(self):
        row = self._seal_fixture()
        intent, _, proc, pipe = self._prepared(command_seconds=30)
        req = {"op": "seal-case", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": intent["candidate_digest"]}
        original = self.runner.StateStore.write_snapshot
        real_time = time.time
        clock = {"offset": 0}
        reached_pending = []
        def delayed(store, state, kind="snapshot_bound", payload=None):
            if kind == "case_manifest_pending":
                reached_pending.append(True)
                clock["offset"] = max(0, intent["command_deadline"] - real_time() + 1)
            return original(store, state, kind, payload)
        try:
            with mock.patch.object(self.runner.StateStore, "write_snapshot", delayed), \
                    mock.patch.object(self.runner.time, "time", side_effect=lambda: real_time() + clock["offset"]):
                with self.assertRaises((ValueError, SystemExit)):
                    self.controller.dispatch(intent, req, row["command"])
            self.assertTrue(reached_pending)
            state = self.store.load_state()
            self.assertNotIn("W4-02/worker", state.get("case_manifest_digests", {}))
            events = self.store.load_events()
            self.assertTrue(any(e["kind"] == "case_manifest_pending" for e in events))
            self.assertFalse(any(e["kind"] == "case_manifest_bound" for e in events))
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_seal_persists_and_validates_post_fsync_proof(self):
        row = self._seal_fixture()
        intent, _, proc, pipe = self._prepared(command_seconds=10)
        req = {"op": "seal-case", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": intent["candidate_digest"]}
        try:
            result = self.controller.dispatch(intent, req, row["command"])
            self.assertEqual(0, result["exit_code"], result)
            events = self.store.load_events()
            self.runner._validate_timed_case_events(events)
            with mock.patch.object(self.runner.StateStore, "load_events", side_effect=AssertionError("bulk load")):
                self.runner._require_state(str(self.root))
            bound = events[-1]
            self.assertEqual("case_manifest_bound", bound["kind"])
            self.assertEqual(bound["prev_hash"], bound["payload"]["pending_hash"])
            bad = json.loads(json.dumps(events))
            bad[-1]["payload"]["persisted_observed_clock"]["wall"] = intent["command_deadline"]
            with self.assertRaises(ValueError):
                self.runner._validate_timed_case_events(bad)
            bad = json.loads(json.dumps(events))
            del bad[-1]["payload"]["pending_hash"]
            with self.assertRaises(ValueError):
                self.runner._validate_timed_case_events(bad)
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_binding_snapshot_crash_recovers_only_with_timed_proof(self):
        row = self._seal_fixture()
        intent, _, proc, pipe = self._prepared(command_seconds=10)
        req = {"op": "seal-case", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": intent["candidate_digest"]}
        original = self.runner.StateStore.persist_snapshot
        def crash(store, state, event_hash):
            if store.load_events()[-1]["kind"] == "case_manifest_bound":
                raise OSError("simulated crash after bound event fsync")
            return original(store, state, event_hash)
        try:
            with mock.patch.object(self.runner.StateStore, "persist_snapshot", crash):
                with self.assertRaises(OSError):
                    self.controller.dispatch(intent, req, row["command"])
            self.assertNotIn("W4-02/worker", self.store.load_state().get("case_manifest_digests", {}))
            state = self.runner._resume_state(str(self.root))
            self.assertIn("W4-02/worker", state["case_manifest_digests"])
            self.runner._validate_timed_case_events(self.store.load_events())
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_pending_snapshot_crash_blocks_without_replay(self):
        row = self._seal_fixture()
        intent, _, proc, pipe = self._prepared(command_seconds=30)
        req = {"op": "seal-case", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": intent["candidate_digest"]}
        original = self.runner.StateStore.persist_snapshot
        def crash(store, state, event_hash):
            if store.load_events()[-1]["kind"] == "case_manifest_pending":
                raise OSError("simulated crash after pending fsync")
            return original(store, state, event_hash)
        try:
            with mock.patch.object(self.runner.StateStore, "persist_snapshot", crash):
                with self.assertRaises(OSError):
                    self.controller.dispatch(intent, req, row["command"])
            before_state = (self.root / "state.json").read_bytes()
            before_events = (self.root / "events.jsonl").read_bytes()
            with self.assertRaises(SystemExit) as exc:
                self.runner._resume_state(str(self.root))
            self.assertEqual(self.runner.EXIT_BLOCKED, exc.exception.code)
            self.assertEqual(before_state, (self.root / "state.json").read_bytes())
            self.assertEqual(before_events, (self.root / "events.jsonl").read_bytes())
            self.assertFalse(self.marker.exists())
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_direct_seal_cannot_create_unproven_binding(self):
        row = self._seal_fixture()
        case_dir = self.root / "evidence" / "W4-02" / "worker" / "attempt-1"
        args = controller.types.SimpleNamespace(run_dir=str(self.root), task_id="W4-02",
                    case_id="W4-02/worker", case_dir=str(case_dir),
                    receipts=str(case_dir / "measured-receipts.json"), command=row["command"])
        before = (self.root / "events.jsonl").read_bytes()
        with self.assertRaises(SystemExit) as exc:
            self.runner.cmd_seal_case(args)
        self.assertEqual(self.runner.EXIT_BLOCKED, exc.exception.code)
        self.assertEqual(before, (self.root / "events.jsonl").read_bytes())

    def test_legacy_requires_explicit_matching_frozen_event_hash(self):
        event = {"kind": "case_manifest_bound", "entry_hash": "a" * 64,
                 "payload": {"case_id": "W4-02/worker", "state_payload": {}}}
        # No anchor map declared by the frozen contract: contract-legal direct
        # seals pass; an explicitly declared map must match exactly.
        self.runner._validate_timed_case_events(iter([event]))
        with self.assertRaises(ValueError):
            self.runner._validate_timed_case_events(iter([event]), {"W4-02/worker": "b" * 64})
        self.runner._validate_timed_case_events(iter([event]), {"W4-02/worker": "a" * 64})

    def test_proof_event_may_finish_after_deadline_only_when_pending_was_durable_before_it(self):
        row = self._seal_fixture()
        intent, _, proc, pipe = self._prepared(command_seconds=30)
        req = {"op": "seal-case", "task_id": "W4-02", "case_id": "W4-02/worker",
               "attempt": 1, "candidate_digest": intent["candidate_digest"]}
        original = self.runner.StateStore.write_snapshot
        real_time = time.time
        clock = {"offset": 0}
        def delayed(store, state, kind="snapshot_bound", payload=None):
            if kind == "case_manifest_bound":
                clock["offset"] = max(0, intent["command_deadline"] - real_time() + 1)
            return original(store, state, kind, payload)
        try:
            with mock.patch.object(self.runner.StateStore, "write_snapshot", delayed), \
                    mock.patch.object(self.runner.time, "time", side_effect=lambda: real_time() + clock["offset"]):
                with self.assertRaises(ValueError):
                    self.controller.dispatch(intent, req, row["command"])
            events = self.store.load_events()
            self.assertEqual("case_manifest_bound", events[-1]["kind"])
            self.assertLess(events[-1]["payload"]["persisted_observed_clock"]["wall"], intent["command_deadline"])
            self.assertGreater(events[-1]["ts"], intent["command_deadline"])
            self.runner._validate_timed_case_events(events)
            # The binding keeps its timing proof; verify-case rejects it only
            # while the launch stays unreconciled (active launch present).
            binding = self.runner._sealed_case_binding(str(self.root), "W4-02/worker")
            self.assertIn("pending_hash", binding)
            self.runner._case_bound_candidates(str(self.root))
        finally:
            os.close(pipe)
            proc.wait(timeout=5)

    def test_expired_task_deadline_never_releases_real_child(self):
        self.state["tasks"]["W4-02"]["task_deadline"] = time.time() + 2
        self.state = self.store.write_snapshot(self.state)
        intent, _, proc, pipe = self._prepared()
        time.sleep(2.1)
        with self.assertRaises(SystemExit):
            self.controller.release_prepared(pipe, intent, self.argv, case_deadline=intent["command_deadline"])
        self.assertEqual(75, proc.wait(timeout=5))
        self.assertFalse(self.marker.exists())

    def test_deadline_expires_during_candidate_validation(self):
        deadline = time.time() + 2
        self.state["time_budgets"]["run"] = {"started_at": deadline - 604800, "deadline": deadline}
        self.state = self.store.write_snapshot(self.state)
        intent, _, proc, pipe = self._prepared()
        real_check = self.runner.candidate_errors
        def slow_candidate(candidate):
            time.sleep(2.1)
            return real_check(candidate)
        try:
            with mock.patch.object(self.runner, "candidate_errors", side_effect=slow_candidate):
                with self.assertRaises(SystemExit):
                    self.controller.release_prepared(pipe, intent, self.argv,
                                                     case_deadline=intent["command_deadline"])
            self.assertEqual(75, proc.wait(timeout=5))
            self.assertFalse(self.marker.exists())
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)

    def test_competing_process_cannot_acquire_and_replaced_path_fails_closed(self):
        intent = self.controller.prepare(self.state, "W4-02", self.argv)
        code = ("import sys;sys.path.insert(0,sys.argv[1]);"
                "import attachment_lifecycle_v5_launch_controller as c;"
                "c.LaunchController(sys.argv[2])")
        competing = subprocess.run([sys.executable, "-c", code, str(ROOT / "scripts/test"),
                                    str(self.root)], capture_output=True, text=True, timeout=5)
        self.assertNotEqual(0, competing.returncode)
        self.assertIn("runner lock unavailable", competing.stderr)
        # A separate process replaces the pathname while the old inode is locked.
        replace = "import os,sys;os.unlink(sys.argv[1]);open(sys.argv[1], 'w').close()"
        subprocess.run([sys.executable, "-c", replace, str(self.root / ".runner.lock")],
                       check=True, timeout=5)
        before = (self.root / "events.jsonl").read_bytes()
        with self.assertRaises(ValueError):
            self.controller.launch_prepared(self.store, self.state, intent, self.argv)
        with self.assertRaises(ValueError):
            self.controller.dispatch(intent, {}, "synthetic")
        self.assertEqual(before, (self.root / "events.jsonl").read_bytes())
        self.assertFalse(self.marker.exists())

    def test_closed_controller_and_external_lock_are_rejected(self):
        self.controller.close()
        with self.assertRaises(ValueError):
            self.controller.prepare(self.state, "W4-02", self.argv)
        self.assertFalse(hasattr(controller, "prepare"))
        self.assertFalse(hasattr(controller, "dispatch"))
        with self.assertRaises(TypeError):
            controller.LaunchController(str(self.root), self.runner.RunLock(str(self.root)))

    def test_closed_controller_cannot_be_revived_with_foreign_lock(self):
        # A conflict probe cannot identify its owner; close must be final.
        lock = self.controller._lock
        token = lock.token
        self.controller.close()
        ready = self.root / "foreign-lock-ready"
        code = ("import fcntl,os,sys,time;from pathlib import Path;"
                "f=os.open(sys.argv[1],os.O_RDWR);fcntl.flock(f,fcntl.LOCK_EX);"
                "Path(sys.argv[2]).write_text('ready');time.sleep(30)")
        foreign = subprocess.Popen([sys.executable, "-c", code, str(lock.path), str(ready)])
        try:
            deadline = time.monotonic() + 5
            while not ready.exists() and foreign.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(ready.exists())
            lock.fd = os.open(lock.path, os.O_RDWR)
            lock.token = token
            try:
                with self.assertRaises(ValueError):
                    self.controller.prepare(self.state, "W4-02", self.argv)
            finally:
                os.close(lock.fd)
                lock.fd = None
        finally:
            foreign.terminate()
            foreign.wait(timeout=5)

    def test_lock_alias_cannot_be_passed_as_payload_fd(self):
        alias = os.dup(self.controller._lock.fd)
        try:
            intent = self.controller.prepare(self.state, "W4-02", self.argv)
            with self.assertRaises(ValueError):
                self.controller.launch_prepared(self.store, self.state, intent, self.argv,
                                                pass_fds=(alias,))
            with self.assertRaises(ValueError):
                self.controller._launch_prepared(self.store, self.state, intent, self.argv,
                                                 pass_fds=(alias,))
            self.assertFalse(self.marker.exists())
        finally:
            os.close(alias)

    def test_controller_close_keeps_lock_until_stopped_shim_exits(self):
        _, _, proc, pipe = self._prepared()
        probe = os.open(self.controller._lock.path, os.O_RDWR)
        try:
            os.kill(proc.pid, signal.SIGSTOP)
            # Wait until the shim is actually stopped, not just signalled.
            deadline = time.monotonic() + 5
            while time.monotonic() < deadline:
                state = subprocess.check_output(['ps', '-o', 'state=', '-p', str(proc.pid)], text=True).strip()
                if state.startswith('T'):
                    break
                time.sleep(0.02)
            else:
                self.fail('shim did not stop')
            self.controller.close()
            pipe = None
            with self.assertRaises(BlockingIOError):
                fcntl.flock(probe, fcntl.LOCK_EX | fcntl.LOCK_NB)
            os.kill(proc.pid, signal.SIGCONT)
            self.assertEqual(75, proc.wait(timeout=5))
            fcntl.flock(probe, fcntl.LOCK_EX | fcntl.LOCK_NB)
            fcntl.flock(probe, fcntl.LOCK_UN)
        finally:
            if pipe is not None:
                os.close(pipe)
            if proc.poll() is None:
                os.kill(proc.pid, signal.SIGCONT)
                proc.terminate()
                proc.wait(timeout=5)
            os.close(probe)

    @unittest.expectedFailure
    def test_foreign_owner_is_not_proof_live_controller_holds_lock(self):
        # A live controller that lost flock must not authenticate another owner.
        import fcntl
        fcntl.flock(self.controller._lock.fd, fcntl.LOCK_UN)
        ready = self.root / "foreign-owner-ready"
        code = ("import fcntl,os,sys,time;from pathlib import Path;"
                "f=os.open(sys.argv[1],os.O_RDWR);fcntl.flock(f,fcntl.LOCK_EX);"
                "Path(sys.argv[2]).write_text('ready');time.sleep(30)")
        proc = subprocess.Popen([sys.executable, '-c', code, self.controller._lock.path, str(ready)])
        try:
            deadline = time.monotonic() + 5
            while not ready.exists() and proc.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(ready.exists())
            with self.assertRaises(ValueError):
                self.controller.prepare(self.state, 'W4-02', self.argv)
        finally:
            proc.terminate()
            proc.wait(timeout=5)

    def test_payload_does_not_inherit_shim_lock_fd(self):
        lock = os.fstat(self.controller._lock.fd)
        code = ("import fcntl,os,sys;from pathlib import Path;"
                "p=Path(sys.argv[1]);target=(int(sys.argv[2]),int(sys.argv[3]));"
                "fds=[];"
                "\nfor name in os.listdir('/dev/fd'):\n"
                " try:\n  s=os.fstat(int(name))\n"
                " except OSError:\n  continue\n"
                " if (s.st_dev,s.st_ino)==target:\n"
                "  fds.append(name);fcntl.flock(int(name),fcntl.LOCK_UN)\n"
                "f=os.open(sys.argv[4],os.O_RDWR)\n"
                "try:\n fcntl.flock(f,fcntl.LOCK_EX|fcntl.LOCK_NB);result='released'\n"
                "except BlockingIOError:\n result='held'\n"
                "os.close(f);p.write_text(result+':'+','.join(fds))")
        argv = [sys.executable, '-c', code, str(self.marker), str(lock.st_dev), str(lock.st_ino),
                str(self.controller._lock.path)]
        intent, _, proc, pipe = self._prepared(argv)
        try:
            self.controller.release_prepared(pipe, intent, argv, case_deadline=intent['command_deadline'])
            deadline = time.monotonic() + 5
            while not self.marker.exists() and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(self.marker.exists())
            self.assertEqual('held:', self.marker.read_text())
        finally:
            self.controller.close()
            proc.wait(timeout=5)

    def test_sigkill_controller_before_go_does_not_run_worker(self):
        self.controller.close()
        ready = self.root / "ready"
        code = ("import sys,time;from pathlib import Path;sys.path.insert(0,sys.argv[1]);"
                "import attachment_lifecycle_v5_launch_controller as c;"
                "r=c.runner;l=c.LaunchController(sys.argv[2]);"
                "s=r._require_state(sys.argv[2]);st=r.StateStore(sys.argv[2]);"
                "a=[sys.executable,'-c',\"from pathlib import Path;Path(%r).write_text('bad')\" % sys.argv[3]];"
                "i=l.prepare(s,'W4-02',a);s,p,w=l.launch_prepared(st,s,i,a);"
                "Path(sys.argv[4]).write_text(str(p.pid));time.sleep(60)")
        proc = subprocess.Popen([sys.executable, "-c", code, str(ROOT / "scripts/test"),
                                 str(self.root), str(self.marker), str(ready)],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.monotonic() + 10
            while not ready.exists() and proc.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(ready.exists(), proc.stderr.read() if proc.poll() is not None else "not ready")
            proc.kill()
            proc.wait(timeout=5)
            time.sleep(0.25)
            self.assertFalse(self.marker.exists())
            state = self.store.load_state()
            self.assertEqual("PREPARED", state["active_launch"]["status"])
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)
            proc.stdout.close()
            proc.stderr.close()

    def test_sigkill_controller_after_go_stops_delayed_child_action(self):
        self.controller.close()
        ready = self.root / "payload-ready"
        launched = self.root / "controller-launched"
        payload = ("import sys,time;from pathlib import Path;"
                   "Path(sys.argv[1]).write_text('ready');time.sleep(1.5);"
                   "Path(sys.argv[2]).write_text('unexpected')")
        code = ("import sys,time;from pathlib import Path;sys.path.insert(0,sys.argv[1]);"
                "import attachment_lifecycle_v5_launch_controller as c;"
                "r=c.runner;l=c.LaunchController(sys.argv[2]);"
                "s=r._require_state(sys.argv[2]);st=r.StateStore(sys.argv[2]);"
                "a=[sys.executable,'-c',sys.argv[3],sys.argv[4],sys.argv[5]];"
                "i=l.prepare(s,'W4-02',a);s,p,w=l.launch_prepared(st,s,i,a);"
                "l.release_prepared(w,i,a,case_deadline=i['command_deadline']);"
                "Path(sys.argv[6]).write_text(str(p.pid));time.sleep(60)")
        proc = subprocess.Popen([sys.executable, "-c", code, str(ROOT / "scripts/test"),
                                 str(self.root), payload, str(ready), str(self.marker), str(launched)],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.monotonic() + 10
            while (not ready.exists() or not launched.exists()) and proc.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(ready.exists() and launched.exists(),
                            proc.stderr.read() if proc.poll() is not None else "payload not ready")
            proc.kill()
            proc.wait(timeout=5)
            time.sleep(1.8)
            self.assertFalse(self.marker.exists())
            self.assertEqual("PREPARED", self.store.load_state()["active_launch"]["status"])
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)
            proc.stdout.close()
            proc.stderr.close()

    def test_exited_leader_does_not_abandon_same_group_descendant(self):
        self.controller.close()
        ready = self.root / "descendant-ready"
        launched = self.root / "controller-launched"
        payload = ("import os,sys,time;from pathlib import Path;"
                   "pid=os.fork();"
                   "(os._exit(0) if pid else None);"
                   "Path(sys.argv[1]).write_text('ready');time.sleep(2);"
                   "Path(sys.argv[2]).write_text('unexpected')")
        code = ("import sys,time;from pathlib import Path;sys.path.insert(0,sys.argv[1]);"
                "import attachment_lifecycle_v5_launch_controller as c;"
                "r=c.runner;l=c.LaunchController(sys.argv[2]);"
                "s=r._require_state(sys.argv[2]);st=r.StateStore(sys.argv[2]);"
                "a=[sys.executable,'-c',sys.argv[3],sys.argv[4],sys.argv[5]];"
                "i=l.prepare(s,'W4-02',a);s,p,w=l.launch_prepared(st,s,i,a);"
                "l.release_prepared(w,i,a,case_deadline=i['command_deadline']);"
                "Path(sys.argv[6]).write_text(str(p.pid));time.sleep(60)")
        proc = subprocess.Popen([sys.executable, "-c", code, str(ROOT / "scripts/test"),
                                 str(self.root), payload, str(ready), str(self.marker), str(launched)],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.monotonic() + 10
            while (not ready.exists() or not launched.exists()) and proc.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(ready.exists() and launched.exists())
            # Give the leader time to exit before killing the controller.
            time.sleep(0.2)
            proc.kill()
            proc.wait(timeout=5)
            time.sleep(2.1)
            self.assertFalse(self.marker.exists())
            self.assertEqual("PREPARED", self.store.load_state()["active_launch"]["status"])
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)
            proc.stdout.close()
            proc.stderr.close()


if __name__ == "__main__":
    unittest.main()
