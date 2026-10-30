#!/usr/bin/env python3
"""Controlled launch primitives for V5; production gate admission remains blocked.

The controller runs in the holder of RunLock. A child shim waits for one byte
before exec; the caller must durably record its identity before releasing it.
This module deliberately does not authorize PostgreSQL or assert that a
process group excludes foreign processes.
"""
import hashlib
import fcntl
import stat
import contextlib
import io
import json
import math
import time
import os
import socket
import select
import signal
import subprocess
import sys
import uuid
import types
import threading
import functools
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import attachment_lifecycle_v5_runner as runner  # noqa: E402


def digest(value):
    return hashlib.sha256(runner.canonical_json(value).encode()).hexdigest()


def process_start(pid):
    out = subprocess.check_output(["ps", "-o", "lstart=", "-p", str(pid)],
                                  stderr=subprocess.DEVNULL, text=True, timeout=2).strip()
    if not out:
        raise RuntimeError("process start unavailable")
    return out


def validate_command_deadline(intent, state, contract):
    task_id = intent.get("task_id")
    task = state.get("tasks", {}).get(task_id, {})
    case_id = intent.get("case_id")
    start, deadline = intent.get("command_started_at"), intent.get("command_deadline")
    maximum = contract.get("timeouts", {}).get("command_seconds", 1800)
    if (case_id not in (runner.task_case_ids(contract, task_id) or [])
            or type(maximum) not in (int, float) or not 0 < maximum <= 1800
            or type(start) not in (int, float) or type(deadline) not in (int, float)
            or not math.isfinite(start) or not math.isfinite(deadline)
            or start > time.time() or deadline <= start or deadline > start + maximum):
        raise ValueError("invalid persisted command deadline")
    bounds = [task.get("task_deadline")]
    budgets = state.get("time_budgets", {})
    if not isinstance(budgets.get("run"), dict):
        raise ValueError("run budget not initialized")
    bounds.append(budgets["run"].get("deadline"))
    wave = next((t.get("wave") for t in contract.get("tasks", []) if t.get("task_id") == task_id), None)
    wave_budget = budgets.get("waves", {}).get(wave)
    if not isinstance(wave_budget, dict):
        raise ValueError("wave budget not initialized")
    bounds.append(wave_budget.get("deadline"))
    if any(type(bound) not in (int, float) or not math.isfinite(bound) or deadline > bound for bound in bounds):
        raise ValueError("command exceeds persisted budget")


def _serialized(method):
    @functools.wraps(method)
    def call(self, *args, **kwargs):
        if os.getpid() != self._pid:
            raise ValueError("controller belongs to another process")
        with self._operation_lock:
            return method(self, *args, **kwargs)
    return call


class LaunchController:
    """Own the runner lock for this controller's lifetime, never accept a lock."""

    def __init__(self, run_dir):
        self.run_dir = os.path.realpath(run_dir)
        self._pid = os.getpid()
        self._closed = False
        self._operation_lock = threading.RLock()
        self._lifelines = {}
        self._lock = runner.RunLock(self.run_dir, wait_budget=0)
        if not self._lock.acquire_nb():
            raise RuntimeError("runner lock unavailable")
        self._identity = os.fstat(self._lock.fd)
        try:
            self._check()
        except BaseException:
            self.close()
            raise

    def _check(self):
        lock = self._lock
        try:
            if self._closed or os.getpid() != self._pid or lock.fd is None:
                raise ValueError("controller lock expired")
            held = os.fstat(lock.fd)
            named = os.stat(lock.path, follow_symlinks=False)
            expected = (self._identity.st_dev, self._identity.st_ino)
            if ((held.st_dev, held.st_ino) != expected
                    or (named.st_dev, named.st_ino) != expected
                    or held.st_nlink != 1 or not stat.S_ISREG(named.st_mode)
                    or held.st_uid != os.geteuid() or held.st_mode & 0o022
                    or os.pread(lock.fd, 128, 0) != (lock.token + "\n").encode()):
                raise ValueError("controller lock identity changed")
            # An independent open must fail to acquire the currently held flock.
            probe = os.open(lock.path, os.O_RDWR | os.O_NOFOLLOW | os.O_NONBLOCK)
            try:
                if (os.fstat(probe).st_dev, os.fstat(probe).st_ino) != expected:
                    raise ValueError("controller lock path changed")
                try:
                    fcntl.flock(probe, fcntl.LOCK_EX | fcntl.LOCK_NB)
                except BlockingIOError:
                    pass
                else:
                    fcntl.flock(probe, fcntl.LOCK_UN)
                    raise ValueError("controller lock no longer held")
            finally:
                os.close(probe)
        except OSError as exc:
            raise ValueError("controller lock cannot be verified") from exc

    @_serialized
    def close(self):
        with self._operation_lock:
            self._close()

    @_serialized
    def _close(self):
        if os.getpid() == self._pid:
            if self._closed:
                return
            self._closed = True
            for fd in tuple(self._lifelines):
                self._close_lifeline(fd)
            # Do not issue LOCK_UN: the watchdog inherits this open file
            # description and must retain the lock until its own exit.
            fd, self._lock.fd = self._lock.fd, None
            if fd is not None:
                os.close(fd)

    def _close_lifeline(self, fd):
        identity = self._lifelines.pop(fd, None)
        if identity is None:
            return
        try:
            current = os.fstat(fd)
            if (current.st_dev, current.st_ino) == identity:
                os.close(fd)
        except OSError:
            pass

    @_serialized
    def prepare(self, state, task_id, argv, *, case_id=None, command_seconds=None):
        with self._operation_lock:
            self._check()
            return self._prepare(state, task_id, argv, case_id=case_id, command_seconds=command_seconds)

    @_serialized
    def launch_prepared(self, store, state, intent, argv, *, pass_fds=()):
        with self._operation_lock:
            self._check()
            if os.path.realpath(store.run_dir) != self.run_dir:
                raise ValueError("controller run mismatch")
            return self._launch_prepared(store, state, intent, argv, pass_fds=pass_fds)

    @_serialized
    def release_prepared(self, pipe_fd, intent, argv, *, case_deadline):
        with self._operation_lock:
            return self._release_prepared(pipe_fd, intent, argv, case_deadline=case_deadline)

    @_serialized
    def _release_prepared(self, pipe_fd, intent, argv, *, case_deadline):
        """Revalidate immediately before GO; this is still not product admission."""
        released = False
        try:
            self._check()
            if pipe_fd not in self._lifelines:
                raise ValueError("unknown launch lifeline")
            pipe_stat = os.fstat(pipe_fd)
            if (pipe_stat.st_dev, pipe_stat.st_ino) != self._lifelines[pipe_fd]:
                raise ValueError("launch lifeline identity changed")
            state = runner._require_state(self.run_dir)
            contract = runner._load_bound_contract(state)
            task_id = intent["task_id"]
            runner._budget_check(state, contract, task_id)
            runner._task_attempt_check(state, contract, task_id)
            validate_command_deadline(intent, state, contract)
            active = state.get("active_launch")
            task = state.get("tasks", {}).get(task_id, {})
            if (not isinstance(active, dict) or active.get("status") != "PREPARED"
                    or any(active.get(k) != v for k, v in intent.items() if k != "status")
                    or task.get("state") not in ("RUNNING", "VERIFYING")
                    or task.get("attempt") != intent.get("attempt")
                    or intent.get("owner_token") != self._lock.token
                    or digest(argv) != intent.get("command_digest")
                    or digest(state.get("verification_candidate")) != intent.get("candidate_digest")
                    or runner.candidate_errors(state.get("verification_candidate"))
                    or type(case_deadline) not in (int, float)
                    or not math.isfinite(case_deadline)
                    or case_deadline != intent.get("command_deadline")
                    or active.get("command_deadline") != case_deadline
                    or case_deadline > task["task_deadline"]):
                raise ValueError("launch release binding or case deadline invalid")
            # Prior candidate and filesystem validation can take time. Re-read
            # admission/clock and budgets after those checks, immediately before GO.
            self._check()
            latest = runner._require_state(self.run_dir)
            latest_contract = runner._load_bound_contract(latest)
            runner._budget_check(latest, latest_contract, task_id)
            runner._task_attempt_check(latest, latest_contract, task_id)
            if (runner.canonical_json(latest) != runner.canonical_json(state)
                    or time.time() >= case_deadline):
                raise ValueError("launch state changed or case deadline expired")
            if os.write(pipe_fd, b"G") != 1:
                raise OSError("uncertain launch release")
            released = True
        finally:
            if not released:
                self._close_lifeline(pipe_fd)

    @_serialized
    def dispatch(self, intent, request, expected_command):
        with self._operation_lock:
            self._check()
            return self._dispatch(intent, request, expected_command)

    def serve_once(self, stream, intent, expected_command, *, timeout=10):
        return _serve_once(stream, self, intent, expected_command, timeout=timeout)

    @_serialized
    def _prepare(self, state, task_id, argv, *, case_id=None, command_seconds=None):
        """Construct launch intent only from already revalidated runner state."""
        self._check()
        lock = self._lock
        task = state.get("tasks", {}).get(task_id, {})
        candidate = state.get("verification_candidate")
        if (lock.fd is None or task.get("state") not in ("RUNNING", "VERIFYING")
                or type(task.get("attempt")) is not int or task["attempt"] < 1
                or not isinstance(candidate, dict) or state.get("active_launch") is not None
                or not isinstance(argv, list) or not argv
                or any(not isinstance(v, str) or not v for v in argv)):
            raise ValueError("launch binding invalid")
        contract = runner._load_bound_contract(state)
        cases = runner.task_case_ids(contract, task_id)
        if case_id is None and isinstance(cases, list) and len(cases) == 1:
            case_id = cases[0]
        if not isinstance(cases, list) or case_id not in cases:
            raise ValueError("launch requires a registered case")
        maximum = contract.get("timeouts", {}).get("command_seconds", 1800)
        seconds = maximum if command_seconds is None else command_seconds
        if (type(maximum) not in (int, float) or not 0 < maximum <= 1800
                or type(seconds) not in (int, float) or not 0 < seconds <= maximum):
            raise ValueError("invalid command timeout")
        start = time.time()
        deadlines = [start + seconds, task.get("task_deadline")]
        budgets = state.get("time_budgets", {})
        run_budget = budgets.get("run")
        if not isinstance(run_budget, dict):
            raise ValueError("run budget not initialized")
        deadlines.append(run_budget.get("deadline"))
        wave = next((t.get("wave") for t in contract.get("tasks", []) if t.get("task_id") == task_id), None)
        wave_budget = budgets.get("waves", {}).get(wave)
        if not isinstance(wave_budget, dict):
            raise ValueError("wave budget not initialized")
        deadlines.append(wave_budget.get("deadline"))
        if any(type(d) not in (int, float) or not math.isfinite(d) or d <= start for d in deadlines):
            raise ValueError("launch deadline missing or exhausted")
        return {"launch_id": str(uuid.uuid4()), "task_id": task_id,
                "attempt": task["attempt"], "candidate_digest": digest(candidate),
                "command_digest": digest(argv), "prior_revision": state["revision"],
                "owner_token": lock.token, "controller_boot": runner._boot_id(),
                "controller_pid": os.getpid(), "controller_start": process_start(os.getpid()),
                "case_id": case_id, "command_started_at": start, "command_deadline": min(deadlines),
                "status": "INTENT"}


    @_serialized
    def _launch_prepared(self, store, state, intent, argv, *, pass_fds=()):
        """Record INTENT, spawn a gated shim, record PREPARED, then return its pipe.

        The caller must keep the runner lock and check real resource identity.
        No code here grants authorization to run the product Gate.
        """
        self._check()
        lock = self._lock
        for fd in pass_fds:
            item = os.fstat(fd)
            if (item.st_dev, item.st_ino) == (self._identity.st_dev, self._identity.st_ino):
                raise ValueError("runner lock cannot be passed to payload")
        if state.get("active_launch") is not None:
            raise ValueError("runner lock or prior launch invalid")
        current = runner._require_state(store.run_dir)
        contract = runner._load_bound_contract(current)
        task_id = intent.get("task_id")
        runner._budget_check(current, contract, task_id)
        runner._task_attempt_check(current, contract, task_id)
        validate_command_deadline(intent, current, contract)
        start_clock = runner._clock_check(current.get("clock_anchor"))
        if (runner.canonical_json(current) != runner.canonical_json(state)
                or current.get("tasks", {}).get(task_id, {}).get("state") not in ("RUNNING", "VERIFYING")
                or current["tasks"][task_id].get("attempt") != intent.get("attempt")
                or runner.candidate_errors(current.get("verification_candidate"))):
            raise ValueError("launch state or candidate changed")
        if (intent.get("prior_revision") != state.get("revision")
                or intent.get("owner_token") != lock.token
                or intent.get("command_digest") != digest(argv)
                or intent.get("candidate_digest") != digest(state.get("verification_candidate"))
                or intent.get("status") != "INTENT"):
            raise ValueError("launch intent changed")
        # Candidate hashing and admission checks can consume the remaining
        # command or task budget. Revalidate the unchanged snapshot and all
        # durable limits before either the intent event or the shim exists.
        self._check()
        latest = runner._require_state(store.run_dir)
        latest_contract = runner._load_bound_contract(latest)
        if (runner.canonical_json(latest) != runner.canonical_json(current)
                or runner.candidate_errors(latest.get("verification_candidate"))):
            raise ValueError("launch state or candidate changed during validation")
        runner._require_admission(latest)
        end_clock = runner._clock_check(start_clock)
        runner._clock_transition(start_clock, end_clock)
        runner._budget_check(latest, latest_contract, task_id)
        runner._task_attempt_check(latest, latest_contract, task_id)
        validate_command_deadline(intent, latest, latest_contract)
        self._check()
        if time.time() >= intent["command_deadline"]:
            raise ValueError("command deadline expired before durable launch")
        updated = dict(latest, active_launch=intent)
        updated = store.write_snapshot(updated, "launch_intent", {"launch": intent})
        read_fd, write_fd = os.pipe()
        pipe_stat = os.fstat(write_fd)
        self._lifelines[write_fd] = (pipe_stat.st_dev, pipe_stat.st_ino)
        proc = None
        try:
            shim = str(Path(__file__).resolve())
            proc = subprocess.Popen([sys.executable, shim, "--worker", str(read_fd), str(lock.fd),
                                     str(intent["command_deadline"]), "--", *argv],
                                    start_new_session=True, pass_fds=(read_fd, lock.fd, *pass_fds))
            os.close(read_fd)
            read_fd = None
            identity = dict(intent, status="PREPARED", worker_pid=proc.pid,
                            worker_pgid=os.getpgid(proc.pid), worker_start=process_start(proc.pid))
            if identity["worker_pgid"] != proc.pid:
                raise RuntimeError("worker process group mismatch")
            self._check()
            updated = store.write_snapshot(dict(updated, active_launch=identity),
                                           "launch_prepared", {"launch": identity})
            return updated, proc, write_fd
        except BaseException:
            if read_fd is not None:
                os.close(read_fd)
            self._close_lifeline(write_fd)  # EOF prevents the shim from executing argv.
            if proc is not None:
                try:
                    proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    # Do not kill an unverified process identity.
                    pass
            raise


    @_serialized
    def _dispatch(self, intent, request, expected_command):
        """Execute a limited runner RPC in the process which holds the lock.

        An inherited socket alone is not a verified peer identity; callers must
        establish that separately before enabling product Gate execution.
        """
        self._check()
        run_dir = self.run_dir
        if not isinstance(request, dict):
            raise ValueError("invalid controller request")
        if set(request) != {"op", "task_id", "case_id", "attempt", "candidate_digest"}:
            raise ValueError("unexpected controller request fields")
        if (request["task_id"] != intent["task_id"] or request["attempt"] != intent["attempt"]
                or request["candidate_digest"] != intent["candidate_digest"]
                or request["op"] not in ("inspect-phase", "verify-case", "seal-case")):
            raise ValueError("controller request binding mismatch")
        state, contract = self._revalidate_dispatch(intent)
        if (request["case_id"] != intent.get("case_id")
                or request["case_id"] not in (runner.task_case_ids(contract, intent["task_id"]) or [])):
            raise ValueError("case outside task")
        args = types.SimpleNamespace(run_dir=run_dir, task_id=intent["task_id"], case_id=request["case_id"])
        function = {"inspect-phase": runner.cmd_inspect_phase,
                    "verify-case": runner.cmd_verify_case,
                    "seal-case": runner.cmd_seal_case}[request["op"]]
        if request["op"] == "seal-case":
            case_name = request["case_id"].split("/", 1)[1]
            args.case_dir = os.path.join(run_dir, "evidence", intent["task_id"], case_name,
                                         "attempt-%d" % intent["attempt"])
            args.receipts = os.path.join(args.case_dir, "measured-receipts.json")
            args.command = expected_command
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            try:
                self._check()
                if request["op"] == "seal-case":
                    code = function(args, held_lock=True, deadline_guard=lambda: self._revalidate_dispatch(intent),
                                    launch_binding=intent)
                else:
                    code = function(args, held_lock=True)
                self._revalidate_dispatch(intent)
            except SystemExit as exc:
                code = exc.code if isinstance(exc.code, int) else runner.EXIT_CONTRACT
        return {"exit_code": code, "stdout": out.getvalue(), "stderr": err.getvalue()}

    @_serialized
    def _revalidate_dispatch(self, intent):
        """Recheck durable authorization and budgets around controller RPCs."""
        self._check()
        state = runner._require_state(self.run_dir)
        contract = runner._load_bound_contract(state)
        start_clock = runner._clock_check(state.get("clock_anchor"))
        active = state.get("active_launch", {})
        if (not isinstance(active, dict) or active.get("launch_id") != intent["launch_id"]
                or active.get("status") != "PREPARED"
                or digest(state.get("verification_candidate")) != intent["candidate_digest"]
                or state.get("tasks", {}).get(intent["task_id"], {}).get("attempt") != intent["attempt"]
                or any(active.get(k) != v for k, v in intent.items() if k != "status")):
            raise ValueError("persisted launch binding changed")
        runner._budget_check(state, contract, intent["task_id"])
        runner._task_attempt_check(state, contract, intent["task_id"])
        validate_command_deadline(intent, state, contract)
        if runner.candidate_errors(state.get("verification_candidate")):
            raise ValueError("candidate drift during controller request")
        runner._require_admission(state)
        end_clock = runner._clock_check(start_clock)
        runner._clock_transition(start_clock, end_clock)
        runner._budget_check(state, contract, intent["task_id"])
        runner._task_attempt_check(state, contract, intent["task_id"])
        self._check()
        if time.time() >= intent["command_deadline"]:
            raise ValueError("command deadline expired during controller request")
        return state, contract


def _receive_line(stream, limit=16384):
    result = bytearray()
    while len(result) <= limit:
        part = stream.recv(min(4096, limit + 1 - len(result)))
        if not part:
            raise ValueError("controller channel closed")
        result.extend(part)
        if b"\n" in result:
            line, remainder = bytes(result).split(b"\n", 1)
            if remainder or len(line) > limit:
                raise ValueError("invalid controller frame")
            return line
    raise ValueError("controller frame too large")


def _serve_once(stream, controller, intent, expected_command, *, timeout=10):
    """Serve one framed request; possession of the socket is not identity proof."""
    stream.settimeout(timeout)
    try:
        request = json.loads(_receive_line(stream))
        response = controller.dispatch(intent, request, expected_command)
    except (OSError, ValueError, TypeError, KeyError):
        response = {"exit_code": runner.EXIT_CONTRACT, "stdout": "", "stderr": "controller request rejected"}
    stream.sendall(json.dumps(response, ensure_ascii=False).encode() + b"\n")
    return response["exit_code"]


def client_call(fd, request, *, timeout=10):
    """Send one bounded request over the inherited controller socket."""
    data = json.dumps(request, ensure_ascii=False).encode()
    if len(data) > 16384 or b"\n" in data:
        raise ValueError("controller request frame invalid")
    stream = socket.socket(fileno=os.dup(fd))
    try:
        stream.settimeout(timeout)
        stream.sendall(data + b"\n")
        response = json.loads(_receive_line(stream))
        if (not isinstance(response, dict) or type(response.get("exit_code")) is not int
                or not isinstance(response.get("stdout"), str)
                or not isinstance(response.get("stderr"), str)):
            raise ValueError("invalid controller response")
        return response
    finally:
        stream.close()


def worker(argv):
    if len(argv) < 5 or argv[3] != "--":
        return 2
    try:
        fd = int(argv[0])
        lock_fd = int(argv[1])
        # The shim retains the shared lock while it watches the controller,
        # but the external command must never inherit its lock description.
        os.set_inheritable(lock_fd, False)
        deadline = float(argv[2])
        if not math.isfinite(deadline) or deadline <= time.time():
            os.close(fd)
            return 75
        ready, _, _ = select.select([fd], [], [], max(0.0, deadline - time.time()))
        if not ready or time.time() >= deadline:
            os.close(fd)
            return 75
        first = os.read(fd, 1)
        if first != b"G":
            os.close(fd)
            return 75
        # The shim remains outside the payload's new session and watches the
        # controller's pipe. EOF after GO means the controller is gone.
        ready_r, ready_w = os.pipe()
        go_r, go_w = os.pipe()
        pid = os.fork()
        if pid == 0:
            try:
                os.close(fd)
                os.close(ready_r)
                os.close(go_w)
                os.setsid()
                os.write(ready_w, b"R")
                os.close(ready_w)
                if os.read(go_r, 1) != b"G":
                    os._exit(75)
                os.close(go_r)
                os.execvp(argv[4], argv[4:])
            except BaseException:
                os._exit(20)
        os.close(ready_w)
        os.close(go_r)
        try:
            ready, _, _ = select.select([fd, ready_r], [], [], 5)
            if (fd in ready or ready_r not in ready or os.read(ready_r, 1) != b"R"
                    or time.time() >= deadline):
                os.close(go_w)
                os.waitpid(pid, 0)
                return 75
            os.write(go_w, b"G")
            os.close(go_w)
            go_w = None
            # Keep the session leader unreaped until the controller disappears.
            # Reaping it early both loses our PID identity anchor and says
            # nothing about descendants still running in the process group.
            # Consequently completion is intentionally unknown, never PASS.
            monotonic_deadline = time.monotonic() + max(0.0, deadline - time.time())
            while True:
                ready, _, _ = select.select([fd], [], [], 0.05)
                if fd in ready or time.time() >= deadline or time.monotonic() >= monotonic_deadline:
                    # PID remains reserved by the unreaped child; its group was
                    # created by setsid before GO. Signal the entire group even
                    # if the leader has already exited.
                    for sig in (signal.SIGTERM, signal.SIGKILL):
                        try:
                            os.killpg(pid, sig)
                        except ProcessLookupError:
                            break
                        if sig == signal.SIGTERM:
                            time.sleep(0.2)
                    os.waitpid(pid, os.WNOHANG)
                    return 75
        finally:
            os.close(fd)
            os.close(ready_r)
            if go_w is not None:
                os.close(go_w)
    except (OSError, ValueError):
        return 20
    return 20


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--worker":
        sys.exit(worker(sys.argv[2:]))
    sys.exit(2)
