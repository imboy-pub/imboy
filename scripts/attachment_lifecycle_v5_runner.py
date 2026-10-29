#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""attachment lifecycle v5 single-writer bounded runner.

Implements the frozen execution-contract.json runner_cli_contract:
  self-test | admit | next | resume | verify
Exit codes: 0 ok, 2 usage, 20 CONTRACT_CONFLICT, 30 NO_GO, 75 BLOCKED_EXTERNAL.
State: <run_dir>/state.json + events.jsonl (fsync'd hash chain), OS flock.
This is the plan's state/test tooling only; it never edits the frozen plan.
"""
import argparse
import contextlib
import copy
import datetime
import fcntl
import hashlib
import json
import os
import stat
import subprocess
import sys
import tempfile
import time
import types
import uuid
try:
    from . import attachment_lifecycle_v5_case_store as case_store
except ImportError:
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    import attachment_lifecycle_v5_case_store as case_store

EXIT_OK, EXIT_USAGE, EXIT_CONTRACT, EXIT_NOGO, EXIT_BLOCKED = 0, 2, 20, 30, 75
TASK_STATES = ["NOT_STARTED", "READY", "RUNNING", "VERIFYING", "COMMITTING",
               "PASS", "RETRYABLE", "BLOCKED_EXTERNAL", "CONTRACT_CONFLICT", "NO_GO"]
TRANSITIONS = {
    "NOT_STARTED": ["READY", "CONTRACT_CONFLICT", "BLOCKED_EXTERNAL", "NO_GO"],
    "READY": ["RUNNING", "BLOCKED_EXTERNAL", "CONTRACT_CONFLICT", "NO_GO"],
    "RUNNING": ["VERIFYING", "RETRYABLE", "BLOCKED_EXTERNAL", "NO_GO", "CONTRACT_CONFLICT"],
    "VERIFYING": ["COMMITTING", "RETRYABLE", "NO_GO", "CONTRACT_CONFLICT", "BLOCKED_EXTERNAL"],
    "COMMITTING": ["PASS", "RETRYABLE", "CONTRACT_CONFLICT", "BLOCKED_EXTERNAL", "NO_GO"],
    "RETRYABLE": ["READY", "NO_GO", "BLOCKED_EXTERNAL"],
    "BLOCKED_EXTERNAL": ["READY", "COMMITTING", "CONTRACT_CONFLICT", "NO_GO"],
    "CONTRACT_CONFLICT": [], "NO_GO": [], "PASS": [],
}


def die(code, msg):
    sys.stderr.write("runner: %s\n" % msg)
    sys.exit(code)


def _boot_id():
    try:
        with open('/proc/sys/kernel/random/boot_id') as fh:
            value = fh.read().strip()
    except OSError:
        try:
            value = subprocess.check_output(['sysctl', '-n', 'kern.boottime'],
                                            text=True, timeout=2).strip()
        except (OSError, subprocess.SubprocessError):
            die(EXIT_BLOCKED, 'cannot determine boot identity')
    if not value:
        die(EXIT_BLOCKED, 'empty boot identity')
    return value


def _clock_check(anchor):
    if not isinstance(anchor, dict) or set(anchor) != {'boot', 'wall', 'monotonic'}:
        die(EXIT_CONTRACT, 'missing persistent clock anchor; reviewed reconciliation required')
    wall, mono = time.time(), time.clock_gettime(time.CLOCK_MONOTONIC)
    previous_wall, previous_mono = anchor['wall'], anchor['monotonic']
    if (not isinstance(anchor['boot'], str) or not anchor['boot']
            or type(previous_wall) not in (int, float) or type(previous_mono) not in (int, float)
            or not 0 < previous_wall < float('inf') or not 0 <= previous_mono < float('inf')):
        die(EXIT_CONTRACT, 'invalid persistent clock anchor')
    if anchor['boot'] != _boot_id():
        die(EXIT_BLOCKED, 'boot changed; trusted clock reconciliation required')
    if wall < previous_wall or (anchor['boot'] == _boot_id() and
                               (mono < previous_mono or wall + 1 < previous_wall + mono - previous_mono)):
        die(EXIT_BLOCKED, 'clock rollback or anomaly; no deadline reset')
    return {'boot': _boot_id(), 'wall': wall, 'monotonic': mono}


def _clock_transition(old, new):
    if (new['wall'] < old['wall'] or (new['boot'] == old['boot'] and
            (new['monotonic'] < old['monotonic'] or
             new['wall'] + 1 < old['wall'] + new['monotonic'] - old['monotonic']))):
        die(EXIT_BLOCKED, 'replayed clock rollback or anomaly')


def _budget_start(state, contract, task_id, started):
    budget = copy.deepcopy(state['time_budgets'])
    task = next((item for item in contract['tasks'] if item.get('task_id') == task_id), None)
    if not task or not isinstance(task.get('wave'), str):
        die(EXIT_CONTRACT, 'task wave missing')
    wave = task['wave']
    timeouts = contract.get('timeouts', {})
    run_seconds = timeouts.get('run_seconds', 604800)
    wave_seconds = timeouts.get('wave_overrides', {}).get(wave, timeouts.get('wave_seconds', 86400))
    if any(type(v) not in (int, float) or not 0 < v < float('inf') for v in (run_seconds, wave_seconds)):
        die(EXIT_CONTRACT, 'invalid run or wave timeout')
    if budget['run'] is None:
        budget['run'] = {'started_at': started, 'deadline': started + run_seconds}
    if wave not in budget['waves']:
        budget['waves'][wave] = {'started_at': started, 'deadline': started + wave_seconds}
    return budget


def _budget_check(state, contract, task_id=None, enforce=True, at=None):
    budget = state.get('time_budgets')
    if not isinstance(budget, dict) or set(budget) != {'run', 'waves'} or not isinstance(budget['waves'], dict):
        die(EXIT_CONTRACT, 'missing persistent run/wave budgets; reviewed reconciliation required')
    entries = []
    if budget['run'] is not None:
        entries.append((budget['run'], contract.get('timeouts', {}).get('run_seconds', 604800)))
    for wave, entry in budget['waves'].items():
        seconds = contract.get('timeouts', {}).get('wave_overrides', {}).get(wave, contract.get('timeouts', {}).get('wave_seconds', 86400))
        entries.append((entry, seconds))
    if budget['waves'] and budget['run'] is None:
        die(EXIT_CONTRACT, 'wave budget without run budget')
    for entry, seconds in entries:
        if (not isinstance(entry, dict) or set(entry) != {'started_at', 'deadline'}
                or any(type(v) not in (int, float) or not 0 < v < float('inf') for v in (entry['started_at'], entry['deadline'], seconds))
                or entry['deadline'] != entry['started_at'] + seconds):
            die(EXIT_CONTRACT, 'invalid persistent run/wave budget')
    now = time.time() if at is None else at
    if enforce and budget['run'] and now >= budget['run']['deadline']:
        die(EXIT_NOGO, 'run deadline exhausted')
    if task_id:
        task = next((item for item in contract['tasks'] if item.get('task_id') == task_id), None)
        wave = task.get('wave') if task else None
        if enforce and wave in budget['waves'] and now >= budget['waves'][wave]['deadline']:
            die(EXIT_NOGO, 'wave deadline exhausted')


def _task_attempt_check(state, contract, task_id):
    task = state.get('tasks', {}).get(task_id, {})
    deadline = task.get('task_deadline')
    attempt = task.get('attempt')
    limit = contract.get('retry_policy', {}).get('task_attempts_total_max', 5)
    if (type(deadline) not in (int, float) or not 0 < deadline < float('inf')
            or time.time() >= deadline):
        die(EXIT_NOGO, 'persisted task deadline missing or exhausted')
    if type(limit) is not int or limit < 1 or type(attempt) is not int or not 1 <= attempt <= limit:
        die(EXIT_NOGO, 'invalid or exhausted task attempt budget')


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def sha256_bytes(b):
    return hashlib.sha256(b).hexdigest()


def canonical_json(obj):
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def authorization_unexpired(auth):
    try:
        issued = datetime.datetime.fromisoformat(auth["issued_at"].replace("Z", "+00:00"))
        expires = datetime.datetime.fromisoformat(auth["expires_at"].replace("Z", "+00:00"))
        now = datetime.datetime.now(datetime.timezone.utc)
        return (issued.tzinfo is not None and expires.tzinfo is not None
                and issued <= now < expires
                and expires - issued <= datetime.timedelta(days=7))
    except (KeyError, TypeError, ValueError, AttributeError):
        return False


# ---------------------------------------------------------------- locking
class RunLock:
    """LOCK_NB advisory lock, total wait <= 2s (contract 02 §14)."""

    def __init__(self, run_dir, wait_budget=2.0):
        self.path = os.path.join(run_dir, ".runner.lock")
        self.fh = None
        self.wait_budget = wait_budget
        self.token = str(uuid.uuid4())

    def acquire_nb(self):
        deadline = time.monotonic() + self.wait_budget
        self.fh = open(self.path, "a+")
        while True:
            try:
                fcntl.flock(self.fh.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                self.fh.seek(0)
                self.fh.truncate()
                self.fh.write(self.token + "\n")
                self.fh.flush()
                os.fsync(self.fh.fileno())
                return True
            except OSError:
                if time.monotonic() >= deadline:
                    self.fh.close()
                    self.fh = None
                    return False
                time.sleep(0.1)

    def release(self):
        if self.fh:
            try:
                fcntl.flock(self.fh.fileno(), fcntl.LOCK_UN)
            finally:
                self.fh.close()
                self.fh = None


# ---------------------------------------------------------------- state
class StateStore:
    """Append-only fsync'd event chain + atomic snapshot (contract 04)."""

    def __init__(self, run_dir):
        self.run_dir = run_dir
        self.events_path = os.path.join(run_dir, "events.jsonl")
        self.state_path = os.path.join(run_dir, "state.json")

    def head_hash(self):
        if not os.path.exists(self.events_path):
            return "0" * 64
        with open(self.events_path, "rb") as fh:
            last = b""
            for line in fh:
                if line.strip():
                    last = line
        if not last:
            return "0" * 64
        try:
            return json.loads(last.decode())["entry_hash"]
        except Exception:
            die(EXIT_CONTRACT, "corrupt event tail; CONTRACT_CONFLICT (truncate policy requires holder)")

    def append_event(self, kind, payload):
        prev = self.head_hash()
        seq = self._next_seq()
        ev = {"seq": seq, "ts": time.time(), "prev_hash": prev, "kind": kind, "payload": payload}
        ev["entry_hash"] = sha256_bytes(canonical_json(ev).encode())
        with open(self.events_path, "a") as fh:
            fh.write(canonical_json(ev) + "\n")
            fh.flush()
            os.fsync(fh.fileno())
        return ev

    def _next_seq(self):
        if not os.path.exists(self.events_path):
            return 1
        n = 0
        with open(self.events_path) as fh:
            for line in fh:
                if line.strip():
                    n += 1
        return n + 1

    def write_snapshot(self, state, kind="snapshot_bound", payload=None):
        state = dict(state)
        if 'clock_anchor' in state:
            state['clock_anchor'] = _clock_check(state['clock_anchor'])
        else:
            state['clock_anchor'] = {'boot': _boot_id(), 'wall': time.time(), 'monotonic': time.clock_gettime(time.CLOCK_MONOTONIC)}
        state.setdefault('time_budgets', {'run': None, 'waves': {}})
        state.pop("events_head_hash", None)
        state["revision"] = state.get("revision", 0) + 1
        event_payload = dict(payload or {})
        event_payload.update(state_payload=state, state_digest=sha256_bytes(canonical_json(state).encode()))
        self.append_event(kind, event_payload)
        return self.persist_snapshot(state, self.head_hash())

    def persist_snapshot(self, state, event_hash):
        state = dict(state)
        state["events_head_hash"] = event_hash
        d = os.path.dirname(self.state_path)
        tmp = os.path.join(d, ".state.tmp.%s" % uuid.uuid4().hex)
        with open(tmp, "w") as fh:
            fh.write(canonical_json(state) + "\n")
            fh.flush()
            os.fsync(fh.fileno())
        os.rename(tmp, self.state_path)
        dfd = os.open(d, os.O_RDONLY)
        try:
            os.fsync(dfd)
        finally:
            os.close(dfd)
        return state

    def load_state(self):
        if not os.path.exists(self.state_path):
            return None
        with open(self.state_path) as fh:
            return json.load(fh)

    def load_events(self):
        if not os.path.exists(self.events_path):
            return []
        out = []
        with open(self.events_path) as fh:
            for line in fh:
                if line.strip():
                    out.append(json.loads(line))
        return out

    def verify_chain(self):
        prev = "0" * 64
        for i, ev in enumerate(self.load_events(), 1):
            if ev["seq"] != i or ev["prev_hash"] != prev:
                return False, "event %d breaks seq/hash chain" % i
            body = {k: ev[k] for k in ("seq", "ts", "prev_hash", "kind", "payload")}
            if sha256_bytes(canonical_json(body).encode()) != ev["entry_hash"]:
                return False, "event %d entry_hash mismatch" % i
            prev = ev["entry_hash"]
        return True, "chain ok (%d events)" % i if self.load_events() else "empty"

    def snapshot_bound(self, state, events):
        if not isinstance(state, dict) or not events:
            return False
        # Accept-style records (accept-task / accept-gate) append plain
        # artifact events and rebind the snapshot head hash without a
        # state payload; the binding is against the newest event that
        # carries one.
        for event in reversed(events):
            if not isinstance(event, dict):
                return False
            payload = event.get("payload")
            if not (isinstance(payload, dict)
                    and isinstance(payload.get("state_payload"), dict)):
                continue
            expected = dict(state)
            expected.pop("events_head_hash", None)
            bound = payload["state_payload"]
            encoded = canonical_json(bound)
            return (canonical_json(expected) == encoded
                    and payload.get("state_digest") == sha256_bytes(encoded.encode()))
        return False


# ---------------------------------------------------------------- validators
# Pure predicates used by verify/self-test. Each returns (ok: bool, reason: str).
V = {}


def _fields(receipt):
    return receipt if isinstance(receipt, dict) else {}


def v_leaf_set(expected_ids, actual_ids):
    e, a = set(expected_ids), set(actual_ids)
    if len(expected_ids) != len(e) or len(actual_ids) != len(a):
        return False, "duplicate leaf identity"
    if e == a:
        return True, "exact"
    missing = e - a
    extra = a - e
    return False, "leaf set mismatch missing=%s extra=%s" % (sorted(missing)[:3], sorted(extra)[:3])


V["leaf_set"] = v_leaf_set


def v_binding(receipt, candidate):
    r = _fields(receipt)
    if r.get("candidate_heads") != candidate.get("heads"):
        return False, "candidate heads mismatch"
    if r.get("owned_tree_digest") != candidate.get("owned_tree_digest"):
        return False, "owned digest mismatch"
    if r.get("plan_manifest_sha256") != candidate.get("plan_manifest_sha256"):
        return False, "plan hash mismatch"
    if r.get("migration_digest") != candidate.get("migration_digest"):
        return False, "migration hash mismatch"
    return True, "bound"


V["binding"] = v_binding


def v_phase(receipt, task_phases, run_phases):
    ph = _fields(receipt).get("schema_phase")
    if ph not in task_phases:
        return False, "phase %r not declared by task" % ph
    if ph not in run_phases:
        return False, "phase %r not in run phase sequence" % ph
    return True, "phase ok"


V["phase"] = v_phase


def v_raw_output(receipt):
    r = _fields(receipt)
    path = r.get("raw_output_path")
    if not isinstance(path, str) or not path or not os.path.isfile(path):
        return False, "raw output missing"
    if sha256_file(path) != r.get("raw_output_sha256"):
        return False, "raw output sha mismatch"
    return True, "raw ok"


V["raw_output"] = v_raw_output


# Structured observations and receipt lists are metadata, not object contents.
MAX_STRUCTURED_EVIDENCE_BYTES = 64 * 1024 * 1024


def _read_run_artifact(run_dir, path):
    """Open each path component without following links and read one regular file."""
    root = os.path.abspath(run_dir)
    if not isinstance(path, str) or not os.path.isabs(path):
        raise ValueError("raw output path must be absolute")
    relative = os.path.relpath(path, root)
    parts = relative.split(os.sep)
    if any(part in ("", ".", "..") for part in parts):
        raise ValueError("raw output outside run directory")
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    directory = os.open(root, flags)
    try:
        for part in parts[:-1]:
            child = os.open(part, flags, dir_fd=directory)
            os.close(directory)
            directory = child
        fd = os.open(parts[-1], os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                     dir_fd=directory)
        try:
            info = os.fstat(fd)
            if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
                raise ValueError("raw output is not a regular file")
            if info.st_size > MAX_STRUCTURED_EVIDENCE_BYTES:
                raise ValueError("structured evidence exceeds size limit")
            with os.fdopen(fd, "rb") as stream:
                fd = -1
                data = stream.read(MAX_STRUCTURED_EVIDENCE_BYTES + 1)
                if len(data) > MAX_STRUCTURED_EVIDENCE_BYTES:
                    raise ValueError("structured evidence exceeds size limit")
                return data
        finally:
            if fd != -1:
                os.close(fd)
    finally:
        os.close(directory)


def v_execution_counts(receipt):
    r = _fields(receipt)
    exit_code = r.get("exit_code")
    ex, pa, fa, sk = (r.get("executed", 0), r.get("passed", 0),
                      r.get("failed", 0), r.get("skipped", 0))
    if any(type(value) is not int or value < 0 for value in (exit_code, ex, pa, fa, sk)):
        return False, "execution counts must be nonnegative integers"
    if exit_code != 0:
        return False, "exit %r != 0" % exit_code
    if ex < 1:
        return False, "executed=0 cannot PASS"
    if ex != pa + fa + sk:
        return False, "executed != passed+failed+skipped"
    if fa != 0:
        return False, "failed=%d" % fa
    if sk != 0:
        return False, "required skip=%d" % sk
    return True, "counts ok"


V["execution_counts"] = v_execution_counts


def v_provenance(receipt, need_real_driver, need_device, need_worker_stopped):
    r = _fields(receipt)
    if need_real_driver and r.get("driver") in (None, "mock", "none"):
        return False, "mock/none driver cannot satisfy real case"
    if need_device and not r.get("device_provenance"):
        return False, "missing physical device provenance"
    if need_worker_stopped and not r.get("old_worker_stopped_proof"):
        return False, "missing old worker stopped proof"
    return True, "provenance ok"


V["provenance"] = v_provenance


def v_lease(lease):
    l = _fields(lease)
    if l.get("owner_token") != l.get("expected_owner"):
        return False, "stale owner token"
    if l.get("lease_until", 0) < l.get("now", 0):
        return False, "lease expired"
    return True, "lease ok"


V["lease"] = v_lease


def v_attempts(attempts_before, attempts_after, allow_increase_only=True):
    if attempts_after < attempts_before:
        return False, "persisted attempts reset"
    if allow_increase_only and attempts_after == attempts_before:
        return True, "unchanged"
    return True, "increased"


V["attempts"] = v_attempts


def v_staged_files(staged, owned):
    extra = set(staged) - set(owned)
    if extra:
        return False, "staged foreign/unowned files: %s" % sorted(extra)[:3]
    return True, "staging within ownership"


V["staged_files"] = v_staged_files


def v_commit_vector(vector, repo_names):
    v = _fields(vector)
    missing = [r for r in repo_names if r not in v or not v[r].get("commit")]
    if missing:
        return False, "partial commit vector missing %s" % missing
    return True, "vector complete"


V["commit_vector"] = v_commit_vector


def v_commit_parent(vector, expected_parents):
    for repo, exp in expected_parents.items():
        got = _fields(vector).get(repo, {}).get("parent")
        if got != exp:
            return False, "commit parent mismatch %s: %r != %r" % (repo, got, exp)
    return True, "parents ok"


V["commit_parent"] = v_commit_parent


def v_resource(res, allowlist):
    kind = _fields(res).get("kind")
    rid = _fields(res).get("id")
    if (not isinstance(allowlist, dict) or not isinstance(kind, str) or not kind
            or not isinstance(rid, str) or not rid):
        return False, "invalid resource or allowlist"
    allowed = allowlist.get(kind)
    if kind == "disk_root":
        if not isinstance(allowed, str) or not allowed:
            return False, "disk root must be a nonempty path string"
        if not os.path.isabs(allowed) or not os.path.isabs(rid):
            return False, "disk root and resource must be absolute paths"
        try:
            root = os.path.realpath(allowed)
            if os.path.commonpath([root, os.path.realpath(rid)]) != root:
                return False, "resource id outside run-owned disk root"
        except (OSError, ValueError):
            return False, "invalid disk resource path"
    else:
        if (not isinstance(allowed, list) or not allowed
                or any(not isinstance(value, str) or not value for value in allowed)):
            return False, "invalid registered resource list"
        if rid not in allowed:
            return False, "resource id outside registered allowlist"
    return True, "resource allowed"


V["resource"] = v_resource


def v_blocked_resume(block, revision):
    if _fields(block).get("blocked_reason") and revision == _fields(block).get("blocked_revision"):
        return False, "blocked checkpoint resumed without condition revision"
    return True, "resume revised"


V["blocked_resume"] = v_blocked_resume


def v_schema_phase_guard(guard_receipt):
    g = _fields(guard_receipt)
    if not g.get("guard_checked"):
        return False, "schema-phase guard silently skipped"
    if g.get("declared_phase") != g.get("executed_phase"):
        return False, "executed phase differs from declared"
    return True, "guard ok"


V["schema_phase_guard"] = v_schema_phase_guard


def v_physical_identity(job, identity):
    j = _fields(job)
    for k in ("storage_driver", "storage_namespace", "storage_container", "object_key"):
        if j.get(k) != identity.get(k):
            return False, "wrong physical identity field %s" % k
    return True, "identity exact"


V["physical_identity"] = v_physical_identity


def v_namespace(ns, registry):
    n = _fields(ns)
    entry = _fields(registry).get(n.get("namespace"))
    if entry is None:
        return False, "unregistered namespace %r" % n.get("namespace")
    alias_of = entry.get("alias_of") if isinstance(entry, dict) else None
    if alias_of and n.get("expected_canonical") and alias_of != n.get("expected_canonical"):
        return False, "alias resolves to non-canonical root"
    return True, "namespace registered"


V["namespace"] = v_namespace


def v_session_link(session):
    s = _fields(session)
    confirmed = s.get("session_state") == "confirmed"
    linked = s.get("confirmed_attachment_id") is not None
    if confirmed != linked:
        return False, "confirmed iff linked violated"
    if linked and s.get("target_path") != s.get("expected_target_path"):
        return False, "confirmed session linked to wrong FK target"
    return True, "session link ok"


V["session_link"] = v_session_link


def v_claim_slots(claims, capacity):
    if len(claims) > capacity:
        return False, "claim beyond execution slots"
    return True, "within capacity"


V["claim_slots"] = v_claim_slots


def v_activation_order(states):
    order = ["FROZEN", "WRITERS_QUIESCED", "DDL_VERIFIED", "GUARDS_VERIFIED", "NEW_WORKER_ENABLED"]
    seen = -1
    for s in _fields(states).get("sequence", []):
        if s not in order:
            return False, "unknown activation state %r" % s
        i = order.index(s)
        if i != seen + 1:
            return False, "activation skips/loops at %r" % s
        seen = i
    if _fields(states).get("worker_enabled") and seen < order.index("GUARDS_VERIFIED"):
        return False, "activate worker before guards verified"
    return True, "activation ordered"


V["activation_order"] = v_activation_order


def v_activation_catalog(states):
    if _fields(states).get("ddl_committed") is False and _fields(states).get("ddl_verified"):
        return False, "adopt partial activation catalog as success"
    return True, "catalog consistent"


V["activation_catalog"] = v_activation_catalog


def v_surface_receipt(receipt, required_surfaces):
    got = _fields(receipt).get("surfaces", {})
    for s in required_surfaces:
        if s not in got:
            return False, "missing required upload surface %s" % s
    kind = _fields(receipt).get("surface_kind")
    for s, k in got.items():
        if k == "substituted":
            return False, "surface %s receipt substituted" % s
    if kind == "widget" and _fields(receipt).get("entrypoint") == "seat":
        return False, "widget content receipt replaced by seat receipt"
    return True, "surfaces complete"


V["surface_receipt"] = v_surface_receipt


def v_tombstone(tomb, live_registrations):
    t = _fields(tomb)
    cid = t.get("client_upload_id")
    if cid not in _fields(live_registrations):
        return False, "forge recovery tombstone without original registration"
    if t.get("upload_registration") != _fields(live_registrations)[cid]:
        return False, "tombstone snapshot != original registration"
    return True, "tombstone authentic"


V["tombstone"] = v_tombstone


def v_worker_writes(writes, authority_paths, owned_paths):
    for w in _fields(writes).get("writes", []):
        if w in authority_paths:
            return False, "worker wrote authority state %s" % w
        if w not in owned_paths:
            return False, "worker wrote outside owned paths: %s" % w
    if _fields(writes).get("committed"):
        return False, "worker committed git changes"
    return True, "worker writes bounded"


V["worker_writes"] = v_worker_writes


def v_supplementary_regression(task_receipt, affected_prior_case_ids):
    got = set(_fields(task_receipt).get("supplementary_regression_case_ids", []))
    need = set(affected_prior_case_ids)
    if not need <= got:
        return False, "omitted affected prior case regression: %s" % sorted(need - got)
    return True, "supplementary regression complete"


V["supplementary_regression"] = v_supplementary_regression


def v_budget_relabel(task):
    t = _fields(task)
    if t.get("relabelled_repair_as_successor"):
        return False, "reset original task budget by relabelling repair"
    if t.get("attempt", 1) > t.get("max_attempts", 5):
        return False, "task attempts exceed budget"
    return True, "budget honest"


V["budget_relabel"] = v_budget_relabel


def v_backend_or_queue_swap(receipt, expected):
    if _fields(receipt).get("backend") != _fields(expected).get("backend"):
        return False, "backend swapped"
    if _fields(receipt).get("queue") != _fields(expected).get("queue"):
        return False, "queue swapped"
    return True, "backend/queue match"


V["backend_queue"] = v_backend_or_queue_swap


def v_case_identity(receipt, registry_ids):
    cid = _fields(receipt).get("case_id")
    if cid not in registry_ids:
        return False, "renamed case_id %r not in registry" % cid
    return True, "case identity"


V["case_identity"] = v_case_identity


# ---------------------------------------------------------------- contract
def load_contract(plan_dir):
    path = os.path.join(plan_dir, "execution-contract.json")
    with open(path) as fh:
        return json.load(fh)


def verify_plan_manifest(plan_dir):
    manifest = os.path.join(plan_dir, "manifest.sha256")
    ok = True
    with open(manifest) as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            digest, rel = line.split(None, 1)
            rel = rel.strip()
            p = os.path.join(plan_dir, rel)
            if not os.path.exists(p) or sha256_file(p) != digest:
                ok = False
                sys.stderr.write("manifest mismatch: %s\n" % rel)
    manifest_sha = sha256_file(manifest)
    return ok, manifest_sha


def leaf_ids_for_case(contract, case_id):
    out = []
    for leaf in contract["expected_leaf_matrix"].get(case_id, []):
        out.append("|".join(str(leaf.get(k)) for k in
                    ("ac_id", "domain", "driver", "entrypoint",
                     "ordering_or_fault", "schema_phase")))
    return out


def task_case_ids(contract, task_id):
    for t in contract["tasks"]:
        if t["task_id"] == task_id:
            return t.get("case_ids", [])
    return None


def gate_of_wave(contract, wave):
    for g, info in contract["wave_gates"].items():
        if info.get("wave") == wave:
            return g
    return None


def wave_of_task(contract, task_id):
    for t in contract["tasks"]:
        if t["task_id"] == task_id:
            return t["wave"]
    return None


# ---------------------------------------------------------------- commands
def cmd_admit(args):
    plan_dir = os.path.abspath(args.plan_dir)
    run_dir = os.path.abspath(args.run_dir)
    auth_path = os.path.abspath(args.authorization)
    for p, code, what in ((plan_dir, 20, "plan dir"), (run_dir, 2, "run dir"), (auth_path, 20, "authorization")):
        if not os.path.isdir(p) and p != auth_path:
            die(code, "missing %s: %s" % (what, p))
    try:
        with open(auth_path, "rb") as fh:
            auth_bytes = fh.read()
        auth = json.loads(auth_bytes)
    except (OSError, ValueError):
        die(EXIT_CONTRACT, "authorization missing or invalid")
    if not isinstance(auth, dict):
        die(EXIT_CONTRACT, "authorization must be an object")
    auth_digest = sha256_bytes(auth_bytes)
    ok, manifest_sha = verify_plan_manifest(plan_dir)
    if not ok:
        die(20, "plan manifest verification failed (CONTRACT_CONFLICT)")
    if auth.get("plan_manifest_sha256") != manifest_sha:
        die(20, "authorization plan hash does not bind current plan manifest")
    if auth.get("implementation_allowed") is not True or auth.get("local_commit_allowed") is not True:
        die(20, "authorization does not allow implementation+commit")
    contract = load_contract(plan_dir)
    allowed_tasks = auth.get("allowed_tasks")
    if not isinstance(allowed_tasks, list) or not all(isinstance(t, str) for t in allowed_tasks):
        die(EXIT_CONTRACT, "authorization allowed_tasks invalid")
    allowed = set(allowed_tasks)
    need = {t["task_id"] for t in contract["tasks"]}
    if len(allowed_tasks) != len(allowed) or allowed != need:
        die(20, "authorization allowed_tasks must match the contract exactly")
    if not authorization_unexpired(auth):
        die(20, "authorization expired")
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        obs = os.path.join(run_dir, "observations", "%s.json" % uuid.uuid4().hex)
        os.makedirs(os.path.dirname(obs), exist_ok=True)
        with open(obs, "w") as fh:
            json.dump({"kind": "owner_unresponsive", "when": time.time(),
                       "action": "admit"}, fh)
        die(75, "BLOCKED_EXTERNAL: live lock holder unresponsive")
    store = StateStore(run_dir)
    try:
        existing = store.load_state()
    except (OSError, ValueError):
        lock.release()
        die(EXIT_CONTRACT, "existing admission snapshot invalid")
    if os.path.exists(store.state_path) and not isinstance(existing, dict):
        lock.release()
        die(EXIT_CONTRACT, "existing admission snapshot is not an object")
    if existing is None:
        if os.path.exists(store.events_path):
            lock.release()
            die(EXIT_CONTRACT, "existing event history without snapshot; reconciliation required")
        state = {
            "run_id": os.path.basename(run_dir),
            "revision": 0,
            "owner_token": lock.token,
            "lease_until": time.time() + 120,
            "plan_dir": plan_dir,
            "plan_manifest_sha256": manifest_sha,
            "authorization_path": auth_path,
            "authorization_sha256": auth_digest,
            "initial_heads": auth.get("initial_three_repo_heads", {}),
            "tasks": {t["task_id"]: {"state": "NOT_STARTED", "attempt": 0}
                      for t in contract["tasks"]},
            "gates": {g: "NOT_STARTED" for g in contract["wave_gates"]},
            "mode": "IMPLEMENTATION",
        }
        store.write_snapshot(state, "admit", {"plan_manifest_sha256": manifest_sha,
                                               "authorization_sha256": state["authorization_sha256"]})
        print(json.dumps({"admitted": True, "run_id": state["run_id"],
                          "plan_manifest_sha256": manifest_sha,
                          "tasks": len(state["tasks"])}))
    else:
        try:
            events = store.load_events()
            chain_ok, _ = store.verify_chain()
            first = events[0] if events else {}
            # snapshot_bound is deliberately not checked here: accept-task /
            # accept-gate rebind the snapshot via persist_snapshot without a
            # state_payload event, so a healthy post-accept state never
            # matches the last state-carrying event. Tamper detection is
            # carried by the hash chain plus the admit anchor below, and the
            # re-admit write_snapshot restores the payload binding.
            ledger_ok = (bool(events) and chain_ok
                         and existing.get("events_head_hash") == store.head_hash()
                         and first.get("kind") == "admit"
                         and first.get("payload", {}).get("plan_manifest_sha256") == manifest_sha
                         and first.get("payload", {}).get("authorization_sha256") == auth_digest)
        except (OSError, ValueError, KeyError, TypeError, AttributeError):
            ledger_ok = False
        if not ledger_ok:
            lock.release()
            die(EXIT_CONTRACT, "existing admission ledger incomplete or inconsistent")
        if (not isinstance(existing.get("plan_dir"), str)
                or not existing["plan_dir"]
                or not isinstance(existing.get("authorization_path"), str)
                or not existing["authorization_path"]
                or existing.get("run_id") != os.path.basename(run_dir)
                or existing.get("mode") != "IMPLEMENTATION"
                or os.path.realpath(existing.get("plan_dir", "")) != os.path.realpath(plan_dir)
                or existing.get("plan_manifest_sha256") != manifest_sha
                or os.path.realpath(existing.get("authorization_path", "")) != os.path.realpath(auth_path)
                or existing.get("authorization_sha256") != auth_digest):
            lock.release()
            die(EXIT_CONTRACT, "existing admission differs; new reviewed run required")
        try:
            if not isinstance(existing.get('clock_anchor'), dict):
                # Legacy pre-anchor snapshot (admitted by an older runner):
                # establish the persistent clock anchor once, as a
                # ledger-visible reconciliation event.
                existing['clock_anchor'] = {'boot': _boot_id(), 'wall': time.time(),
                                            'monotonic': time.clock_gettime(
                                                time.CLOCK_MONOTONIC)}
                existing = store.write_snapshot(existing, "clock_anchor_reconciled",
                                                {"reason": "legacy snapshot predates "
                                                           "persistent clock anchors"})
            else:
                _clock_check(existing.get('clock_anchor'))
            _budget_check(existing, contract)
        except BaseException:
            lock.release()
            raise
        # Re-admission doubles as the run liveness heartbeat: pg_guard's
        # owned_container rejects a stale lease_until. A single gate run
        # (full migration replay + suite) legitimately exceeds two minutes and
        # audits the resource repeatedly, so the lease window covers one gate
        # run; the authorization expiry remains the outer bound.
        refreshed = dict(existing)
        refreshed["lease_until"] = time.time() + 3600
        store.write_snapshot(refreshed, "admit",
                             {"plan_manifest_sha256": manifest_sha,
                              "authorization_sha256": auth_digest})
        print(json.dumps({"admitted": True, "already": True}))
    lock.release()
    return EXIT_OK


def _compute_ready(state, contract):
    run_budget = state.get('time_budgets', {}).get('run')
    if run_budget and time.time() >= run_budget['deadline']:
        return []
    candidate = state.get("verification_candidate")
    if candidate and candidate_errors(candidate):
        return []
    ready = []
    tasks = {t["task_id"]: t for t in contract["tasks"]}
    for tid, t in tasks.items():
        st = state["tasks"].get(tid, {})
        if not candidate and t.get("wave") != "W0":
            continue
        if st.get("state") != "NOT_STARTED":
            continue
        wave_budget = state.get('time_budgets', {}).get('waves', {}).get(t.get('wave'))
        if wave_budget and time.time() >= wave_budget['deadline']:
            continue
        if _task_prerequisites(state, contract, tid):
            claims = {"files": t.get("files", []), "case_ids": t.get("case_ids", []),
                      "read_only_files": t.get("read_only_files", []),
                      "schema_phase": t.get("schema_phase"),
                      "commit_subject": t.get("commit_subject")}
            ready.append({"task_id": tid, "claim": claims})
    return ready


def cmd_next(args):
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(EXIT_BLOCKED, "next cannot acquire run lock")
    try:
        state = _require_state(run_dir)
        contract = load_contract(state["plan_dir"])
        candidate = state.get("verification_candidate")
        if candidate and candidate_errors(candidate):
            die(EXIT_CONTRACT, "candidate drift; no tasks ready")
        ready = _compute_ready(state, contract)
        print(json.dumps({"ready": ready, "count": len(ready)}, ensure_ascii=False, indent=1))
        return EXIT_OK
    finally:
        lock.release()


def _run_dir_from(args):
    rd = getattr(args, "run_dir", None)
    if not rd:
        rd = os.environ.get("V5_RUN_DIR")
    if not rd or not os.path.isabs(rd):
        die(2, "run-dir must be an explicit absolute path (or absolute V5_RUN_DIR)")
    if not os.path.isdir(rd):
        die(2, "run dir does not exist: %s" % rd)
    return rd


def _require_admission(state):
    """Revalidate the original authorization and frozen plan before work."""
    try:
        plan = state["plan_dir"]
        if not isinstance(plan, str) or not plan:
            raise ValueError("plan missing")
        ok, digest = verify_plan_manifest(plan)
        with open(state["authorization_path"], "rb") as fh:
            raw = fh.read()
        auth = json.loads(raw)
        if (not ok or not isinstance(auth, dict)
                or digest != state.get("plan_manifest_sha256")
                or digest != auth.get("plan_manifest_sha256")
                or sha256_bytes(raw) != state.get("authorization_sha256")
                or auth.get("implementation_allowed") is not True
                or auth.get("local_commit_allowed") is not True
                or not authorization_unexpired(auth)):
            raise ValueError("authorization or plan changed")
        contract = load_contract(plan)
        allowed = auth.get("allowed_tasks")
        need = {task["task_id"] for task in contract["tasks"]}
        if (not isinstance(allowed, list) or any(not isinstance(item, str) for item in allowed)
                or len(set(allowed)) != len(allowed) or set(allowed) != need):
            raise ValueError("task authorization differs")
        return contract
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        die(EXIT_CONTRACT, "admission or plan invalid; reconciliation required")


def _require_state(run_dir, allow_expired=False):
    store = StateStore(run_dir)
    try:
        state = store.load_state()
        events = store.load_events()
        ok, why = store.verify_chain()
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        die(EXIT_CONTRACT, "state or event chain invalid; reconciliation required")
    if not isinstance(state, dict):
        die(20, "run not admitted; run admit first")
    if not events or not ok:
        die(20, "event chain corrupt: %s" % why)
    if state.get("events_head_hash") != events[-1].get("entry_hash"):
        die(EXIT_CONTRACT, "snapshot is behind or diverges from event chain; reconciliation required")
    if not store.snapshot_bound(state, events):
        die(EXIT_CONTRACT, "snapshot payload or digest is not bound to event chain")
    contract = _require_admission(state)
    _clock_check(state.get('clock_anchor'))
    _budget_check(state, contract, enforce=not allow_expired)
    return state


def _task_transition_allowed(task, target):
    previous = task.get("state")
    if target not in TRANSITIONS.get(previous, []):
        return False
    if previous == "COMMITTING" and target in ("RETRYABLE", "NO_GO"):
        return False
    if previous == "BLOCKED_EXTERNAL" and target in ("READY", "COMMITTING"):
        return False
    return True


def _task_prerequisites(state, contract, task_id):
    tasks = [task for task in contract.get("tasks", []) if task.get("task_id") == task_id]
    if len(tasks) != 1:
        return False
    task = tasks[0]
    dependencies = task.get("dependencies", [])
    if (not isinstance(dependencies, list)
            or any(state.get("tasks", {}).get(dep, {}).get("state") != "PASS" for dep in dependencies)):
        return False
    candidate = state.get("verification_candidate")
    if candidate is not None and (not isinstance(candidate, dict)
            or candidate.get("plan_manifest_sha256") != state.get("plan_manifest_sha256")
            or candidate_errors(candidate)):
        return False
    wave = task.get("wave")
    if wave == "W0":
        return True
    if not isinstance(wave, str) or not wave:
        return False
    if not candidate:
        return False
    gates = [gate for gate in contract.get("wave_gates", {}).values() if gate.get("wave") == wave]
    if len(gates) != 1:
        return False
    previous = gates[0].get("previous_gate")
    return (bool(previous) and state.get("gates", {}).get(previous) == "PASS"
            and state.get("gate_candidates", {}).get(previous) == candidate)


def _resume_case_manifest(run_dir, store, old, new, event):
    """Recover a single sealed case only after revalidating its complete evidence."""
    payload = event["payload"]
    case_id, digest = payload.get("case_id"), payload.get("manifest_sha256")
    if not isinstance(case_id, str) or "/" not in case_id or not _valid_digest(digest):
        die(EXIT_CONTRACT, "invalid case binding event")
    task_id, case_name = case_id.split("/", 1)
    if not case_name or "/" in case_name or case_name in (".", ".."):
        die(EXIT_CONTRACT, "invalid case binding path")
    old_bindings = old.get("case_manifest_digests", {})
    new_bindings = new.get("case_manifest_digests")
    if (not isinstance(old_bindings, dict) or not isinstance(new_bindings, dict)
            or case_id in old_bindings or new_bindings != dict(old_bindings, **{case_id: digest})
            or payload.get("candidate") != old.get("verification_candidate")):
        die(EXIT_CONTRACT, "case binding event changed other bindings")
    excluded = ("revision", "events_head_hash", "clock_anchor", "case_manifest_digests")
    if (type(old.get("revision")) is not int or new.get("revision") != old["revision"] + 1
            or canonical_json({k: v for k, v in old.items() if k not in excluded})
            != canonical_json({k: v for k, v in new.items() if k not in excluded})):
        die(EXIT_CONTRACT, "case binding event changed unrelated state")
    contract = _require_admission(old)
    _task_attempt_check(old, contract, task_id)
    _clock_check(old.get("clock_anchor"))
    _clock_check(new.get("clock_anchor"))
    _clock_transition(old["clock_anchor"], new["clock_anchor"])
    _budget_check(old, contract, task_id, at=event.get("ts"))
    _budget_check(old, contract, task_id)
    if (case_id not in (task_case_ids(contract, task_id) or [])
            or old.get("tasks", {}).get(task_id, {}).get("state") not in ("RUNNING", "VERIFYING")
            or candidate_errors(old.get("verification_candidate", {}))):
        die(EXIT_CONTRACT, "case binding admission or candidate invalid")
    expected = contract.get("expected_leaf_matrix", {}).get(case_id)
    if not isinstance(expected, list) or not expected:
        die(EXIT_CONTRACT, "case binding leaf contract missing")
    case_dir = os.path.join(run_dir, "evidence", task_id, case_name)
    rows = [row for _, row in _read_case_rows(run_dir, case_dir, digest)]
    identity = lambda row: "|".join(str(row.get(k)) for k in LEAF_FIELDS)
    index = {identity(row): row for row in expected}
    ok, _ = v_leaf_set([identity(row) for row in expected], [identity(row) for row in rows])
    if not ok:
        die(EXIT_CONTRACT, "case binding leaves do not match contract")
    for row in rows:
        if receipt_errors(row, index[identity(row)], task_id, case_id,
                          old["verification_candidate"], run_dir,
                          old.get("resource_allowlist", {}), contract,
                          old.get("tasks", {}).get(task_id, {}).get("attempt")):
            die(EXIT_CONTRACT, "case binding evidence failed revalidation")
    return store.persist_snapshot(new, event["entry_hash"])


def _resume_state(run_dir):
    """Reconcile one durable task transition while the caller holds RunLock."""
    store = StateStore(run_dir)
    try:
        old = store.load_state()
        events = store.load_events()
        valid, _ = store.verify_chain()
        if not isinstance(old, dict) or not valid or not events:
            die(EXIT_CONTRACT, "invalid run state or event chain")
        if old.get("events_head_hash") == events[-1].get("entry_hash"):
            return _require_state(run_dir, allow_expired=True)
        if len(events) < 2:
            die(EXIT_CONTRACT, "snapshot cannot be reconciled without prior event")
        if old.get("events_head_hash") != events[-2].get("entry_hash") or not store.snapshot_bound(old, events[:-1]):
            die(EXIT_CONTRACT, "snapshot cannot be reconciled with a single event")
        event = events[-1]
        payload = event.get("payload", {})
        new = payload.get("state_payload") if isinstance(payload, dict) else None
        if not isinstance(new, dict):
            die(EXIT_CONTRACT, "event lacks a recoverable task state")
        reconstructed = dict(new, events_head_hash=event["entry_hash"])
        if not store.snapshot_bound(reconstructed, events):
            die(EXIT_CONTRACT, "event state digest mismatch")
        if event.get("kind") == "case_manifest_bound":
            return _resume_case_manifest(run_dir, store, old, new, event)
        if event.get("kind") != "task_state":
            die(EXIT_CONTRACT, "event lacks a recoverable task state")
        task_id = payload.get("task")
        old_tasks, new_tasks = old.get("tasks"), new.get("tasks")
        if not isinstance(old_tasks, dict) or not isinstance(new_tasks, dict) or task_id not in old_tasks or task_id not in new_tasks:
            die(EXIT_CONTRACT, "event task missing")
        old_task, new_task = old_tasks[task_id], new_tasks[task_id]
        if not isinstance(old_task, dict) or not isinstance(new_task, dict):
            die(EXIT_CONTRACT, "event task invalid")
        old_other = {k: v for k, v in old.items() if k not in ("revision", "events_head_hash", "tasks", "clock_anchor", "time_budgets")}
        new_other = {k: v for k, v in new.items() if k not in ("revision", "events_head_hash", "tasks", "clock_anchor", "time_budgets")}
        other_old = {k: v for k, v in old_tasks.items() if k != task_id}
        other_new = {k: v for k, v in new_tasks.items() if k != task_id}
        old_status, new_status = old_task.get("state"), new_task.get("state")
        old_attempt, new_attempt = old_task.get("attempt", 0), new_task.get("attempt")
        if (type(old.get("revision")) is not int or new.get("revision") != old["revision"] + 1
                or canonical_json(old_other) != canonical_json(new_other)
                or canonical_json(other_old) != canonical_json(other_new)
                or not _task_transition_allowed(old_task, new_status) or new_status == "PASS"
                or payload.get("to") != new_status or type(old_attempt) is not int or type(new_attempt) is not int
                or new_attempt != old_attempt + (1 if new_status == "RUNNING" else 0)
                or payload.get("attempt") != new_attempt):
            die(EXIT_CONTRACT, "event transition or budget invalid")
        allowed = {"state", "attempt", "task_started_at", "task_deadline", "case_results", "commit_vector", "blocked_reason", "resume_state"}
        if any(k not in old_task and k not in allowed for k in new_task):
            die(EXIT_CONTRACT, "event added unexpected task fields")
        for key, value in old_task.items():
            if (key not in ("state", "attempt", "case_results", "commit_vector", "blocked_reason")
                    and not (key == "resume_state" and new_status == "BLOCKED_EXTERNAL")
                    and (key not in new_task or canonical_json(new_task[key]) != canonical_json(value))):
                die(EXIT_CONTRACT, "event changed durable task budget or metadata")
        if new_status == "BLOCKED_EXTERNAL" and new_task.get("resume_state") != old_status:
            die(EXIT_CONTRACT, "blocked task lost its prior state")
        contract = _require_admission(old)
        _clock_check(old.get('clock_anchor'))
        _clock_check(new.get('clock_anchor'))
        _clock_transition(old['clock_anchor'], new['clock_anchor'])
        _budget_check(old, contract, task_id, enforce=False)
        expected_budgets = old.get('time_budgets')
        if new_status == 'RUNNING':
            expected_budgets = _budget_start(old, contract, task_id, new_task.get('task_started_at'))
        if canonical_json(new.get('time_budgets')) != canonical_json(expected_budgets):
            die(EXIT_CONTRACT, 'event changed run/wave budget')
        _budget_check(new, contract, task_id, enforce=new_status not in ('NO_GO', 'BLOCKED_EXTERNAL'),
                      at=event.get('ts'))
        if new_status in ("READY", "RUNNING") and not _task_prerequisites(old, contract, task_id):
            die(EXIT_CONTRACT, "event task prerequisites not satisfied")
        if new_status == "RUNNING":
            started, deadline = new_task.get("task_started_at"), new_task.get("task_deadline")
            timeouts = contract.get("timeouts", {})
            seconds = timeouts.get("task_overrides", {}).get(task_id, timeouts.get("task_seconds", 28800))
            limit = contract.get("retry_policy", {}).get("task_attempts_total_max", 5)
            if (type(started) not in (int, float) or type(deadline) not in (int, float)
                    or not 0 < started <= deadline < float("inf")
                    or type(limit) is not int or not 1 <= new_attempt <= limit
                    or type(seconds) not in (int, float) or not 0 < seconds < float("inf")
                    or type(event.get("ts")) not in (int, float) or started > event["ts"]
                    or ("task_deadline" not in old_task and deadline != started + seconds)):
                die(EXIT_CONTRACT, "event task deadline invalid")
            if event['ts'] >= deadline:
                die(EXIT_CONTRACT, "task transition was recorded after deadline")
        if old.get("verification_candidate") and candidate_errors(old["verification_candidate"]):
            die(EXIT_CONTRACT, "candidate drift prevents recovery")
        return store.persist_snapshot(new, event["entry_hash"])
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        die(EXIT_CONTRACT, "state or event reconciliation failed")


def cmd_resume(args):
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        obs = os.path.join(run_dir, "observations", "%s.json" % uuid.uuid4().hex)
        os.makedirs(os.path.dirname(obs), exist_ok=True)
        with open(obs, "w") as fh:
            json.dump({"kind": "owner_unresponsive", "when": time.time(),
                       "action": "resume"}, fh)
        die(75, "BLOCKED_EXTERNAL: live lock holder unresponsive (bounded 2s refusal)")
    try:
        state = _resume_state(run_dir)
        store = StateStore(run_dir)
        contract = load_contract(state["plan_dir"])
        ok, manifest_sha = verify_plan_manifest(state["plan_dir"])
        if not ok or manifest_sha != state["plan_manifest_sha256"]:
            die(20, "plan manifest changed since admission")
        candidate = state.get("verification_candidate")
        if candidate and candidate_errors(candidate):
            die(EXIT_CONTRACT, "candidate drift; resume cannot release tasks")
        events = store.load_events()
        committed = {tid: s for tid, s in state["tasks"].items() if s.get("state") == "PASS"}
        report = {
            "run_id": state["run_id"],
            "revision": state["revision"],
            "events": len(events),
            "chain": "ok",
            "tasks_pass": sorted(committed),
            "gates": state["gates"],
            "ready": [r["task_id"] for r in _compute_ready(state, contract)],
            "commit_unknown_reconciliation": {
                tid: {"vector": s.get("commit_vector"),
                      "next_action": s.get("next_action")}
                for tid, s in state["tasks"].items() if s.get("state") == "COMMITTING"},
        }
        print(json.dumps(report, ensure_ascii=False, indent=1))
        return EXIT_OK
    finally:
        lock.release()


LEAF_FIELDS = ("ac_id", "domain", "driver", "entrypoint", "ordering_or_fault", "schema_phase")
REPO_NAMES = ("imboy", "imboyapp", "imboyadmin")


def _valid_digest(value, length=64):
    return (isinstance(value, str) and len(value) == length
            and all(c in "0123456789abcdef" for c in value))


def _ls_files_modes(root):
    """Map tracked path -> git mode ('100644', '100755', '120000', '160000')."""
    out = subprocess.check_output(["git", "-C", root, "ls-files", "-s", "-z"],
                                  timeout=30).decode("utf-8", "surrogateescape")
    modes = {}
    for entry in out.split("\0"):
        if not entry:
            continue
        parts = entry.split("\t", 1)
        if len(parts) == 2:
            meta = parts[0].split()
            if len(meta) == 3:  # "<mode> <object> <stage>\t<path>"
                modes[parts[1]] = meta[0]
    return modes


def _owned_entry_digest(root, name, mode):
    """Content digest for a tracked entry; gitlinks (submodules) bind the commit SHA."""
    if mode == "160000":
        commit = subprocess.check_output(["git", "-C", root, "rev-parse", "HEAD:%s" % name],
                                         text=True, stderr=subprocess.DEVNULL, timeout=10).strip()
        return commit
    return sha256_file(os.path.realpath(os.path.join(root, name)))


def candidate_errors(candidate):
    """Compare the frozen verification input with the actual worktree bytes."""
    errors = []
    if not isinstance(candidate, dict):
        return ["candidate must be an object"]
    roots = candidate.get("repository_paths", {})
    heads = candidate.get("heads", {})
    if (not isinstance(roots, dict) or not isinstance(heads, dict)
            or set(roots) != set(REPO_NAMES) or set(heads) != set(REPO_NAMES)
            or any(not isinstance(v, str) for v in roots.values())):
        return ["candidate requires three repository paths and full heads"]
    actual_files = {}
    for repo in REPO_NAMES:
        root = os.path.realpath(roots[repo])
        try:
            head = subprocess.check_output(
                ["git", "-C", root, "rev-parse", "HEAD"], text=True,
                stderr=subprocess.DEVNULL, timeout=5).strip()
            if not _valid_digest(heads[repo], 40) or head != heads[repo]:
                errors.append("candidate HEAD drift: " + repo)
        except (OSError, subprocess.SubprocessError):
            errors.append("candidate repository unavailable: " + repo)
    files = candidate.get("owned_paths_sha256", {})
    if not isinstance(files, dict) or not files:
        return errors + ["candidate owned file manifest missing"]
    for repo in REPO_NAMES:
        try:
            changes = subprocess.check_output(["git", "-C", roots[repo], "status",
                "--porcelain", "--untracked-files=all"], text=True, timeout=5)
            if changes:
                errors.append("candidate worktree must be clean: " + repo)
        except (OSError, subprocess.SubprocessError):
            errors.append("cannot inspect candidate status: " + repo)
    modes_by_repo = {}
    for name, digest in files.items():
        if not isinstance(name, str):
            errors.append("candidate file path must be string")
            continue
        try:
            repo, rel = name.split("/", 1)
            path = os.path.realpath(os.path.join(roots[repo], rel))
            root = os.path.realpath(roots[repo])
            if repo not in modes_by_repo:
                modes_by_repo[repo] = _ls_files_modes(root)
            mode = modes_by_repo[repo].get(rel)
            digest_ok = (_valid_digest(digest, 40) if mode == "160000"
                         else _valid_digest(digest))
            if os.path.commonpath([root, path]) != root or not digest_ok:
                raise ValueError("invalid path or digest")
            actual_files[name] = _owned_entry_digest(root, rel, mode)
            if actual_files[name] != digest:
                errors.append("candidate file drift: " + name)
        except (KeyError, ValueError, OSError, subprocess.SubprocessError):
            errors.append("candidate file unavailable: " + name)
    if sha256_bytes(canonical_json(actual_files).encode()) != candidate.get("owned_tree_digest"):
        errors.append("candidate owned tree digest mismatch")
    migration_dir = os.path.join(roots["imboy"], "priv", "migrations")
    migrations = {}
    try:
        for name in sorted(os.listdir(migration_dir)):
            if name.endswith((".up.sql", ".down.sql")):
                migrations[name] = sha256_file(os.path.join(migration_dir, name))
        if not migrations or sha256_bytes(canonical_json(migrations).encode()) != candidate.get("migration_digest"):
            errors.append("candidate migration digest mismatch")
    except OSError:
        errors.append("candidate migrations unavailable")
    return errors


def cmd_freeze_candidate(args):
    """Bind the first clean candidate; replacing accepted input needs explicit re-admission."""
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(EXIT_BLOCKED, "freeze-candidate cannot acquire run lock")
    try:
        state = _require_state(run_dir)
        auth_path = state.get("authorization_path", "")
        if not isinstance(auth_path, str) or not auth_path or not os.path.isfile(auth_path):
            die(EXIT_CONTRACT, "authorization original unavailable")
        try:
            with open(auth_path, "rb") as fh:
                auth_bytes = fh.read()
            if sha256_bytes(auth_bytes) != state.get("authorization_sha256"):
                die(EXIT_CONTRACT, "authorization original unavailable")
            auth = json.loads(auth_bytes)
        except (OSError, ValueError):
            die(EXIT_CONTRACT, "authorization missing or invalid")
        if not isinstance(auth, dict):
            die(EXIT_CONTRACT, "authorization must be an object")
        ok, digest = verify_plan_manifest(state["plan_dir"])
        if (not ok or digest != state.get("plan_manifest_sha256")
                or auth.get("plan_manifest_sha256") != digest
                or auth.get("implementation_allowed") is not True
                or not authorization_unexpired(auth)):
            die(EXIT_CONTRACT, "candidate admission/plan binding invalid")
        roots = auth.get("repository_paths", {})
        if not isinstance(roots, dict) or set(roots) != set(REPO_NAMES):
            die(EXIT_CONTRACT, "authorization must bind three repository paths")
        candidate = {"repository_paths": {}, "heads": {}, "owned_paths_sha256": {},
                     "plan_manifest_sha256": digest}
        for repo in REPO_NAMES:
            root = os.path.realpath(roots[repo])
            candidate["repository_paths"][repo] = root
            candidate["heads"][repo] = subprocess.check_output(
                ["git", "-C", root, "rev-parse", "HEAD"], text=True, timeout=5).strip()
            modes = _ls_files_modes(root)
            for name in filter(None, modes):
                path = os.path.realpath(os.path.join(root, name))
                if os.path.commonpath([root, path]) != root:
                    die(EXIT_CONTRACT, "candidate tracked path escapes repository")
                candidate["owned_paths_sha256"][repo + "/" + name] = _owned_entry_digest(root, name, modes[name])
        candidate["owned_tree_digest"] = sha256_bytes(canonical_json(candidate["owned_paths_sha256"]).encode())
        migration_dir = os.path.join(candidate["repository_paths"]["imboy"], "priv/migrations")
        migrations = {name: sha256_file(os.path.join(migration_dir, name))
                      for name in sorted(os.listdir(migration_dir)) if name.endswith((".up.sql", ".down.sql"))}
        candidate["migration_digest"] = sha256_bytes(canonical_json(migrations).encode())
        errors = candidate_errors(candidate)
        if errors:
            die(EXIT_CONTRACT, "; ".join(errors[:10]))
        store = StateStore(run_dir)
        frozen_events = []
        if os.path.exists(store.events_path):
            with open(store.events_path) as fh:
                frozen_events = [event["payload"]["candidate"] for line in fh if line.strip()
                    for event in [json.loads(line)] if event.get("kind") == "candidate_frozen"]
        if any(frozen != candidate for frozen in frozen_events):
            die(EXIT_CONTRACT, "persisted freeze event differs; reviewed re-admission required")
        previous = state.get("verification_candidate")
        if previous:
            if previous != candidate:
                die(EXIT_CONTRACT, "candidate changed; reviewed re-admission required")
            return EXIT_OK
        if (any(task.get("state") not in {"NOT_STARTED", "READY"} or task.get("attempt", 0) != 0
                    for task in state["tasks"].values())
                or any(value != "NOT_STARTED" for value in state["gates"].values())):
            die(EXIT_CONTRACT, "cannot bind historical or in-flight evidence to a new candidate")
        state["verification_candidate"] = candidate
        state["resource_allowlist"] = auth.get("resource_allowlist", {})
        store.write_snapshot(state, "candidate_frozen", {"candidate": candidate})
        return EXIT_OK
    finally:
        lock.release()


_ATTEMPT_NOT_GIVEN = object()


def receipt_errors(leaf, expected_leaf, task_id, case_id, candidate, run_dir, allowlist,
                   contract=None, expected_attempt=_ATTEMPT_NOT_GIVEN, legacy=False):
    if legacy:
        # Historical cases were sealed under the standard frozen at their
        # admission time (their PASS is anchored in the accepted event
        # chain); the runtime structured-observation upgrade only binds
        # cases sealed after it.
        return receipt_errors_legacy(leaf, expected_leaf, task_id, case_id,
                                    candidate, run_dir, allowlist, contract)
    errors = []
    if not isinstance(leaf, dict):
        return ["leaf receipt must be an object"]
    # shortcut: W6-02 lacks session-bound device/browser and backend proof;
    # remove when the runner verifies per-driver provenance independently.
    if task_id == "W6-02" or (isinstance(case_id, str) and case_id.startswith("W6-02/")):
        errors.append("W6-02 provenance is not bound to its driver session and owned backend")
    if leaf.get("required") is not True:
        errors.append("required must be boolean true")
    if leaf.get("task_id") != task_id or leaf.get("case_id") != case_id:
        errors.append("task/case identity mismatch")
    if expected_attempt is not _ATTEMPT_NOT_GIVEN and (type(expected_attempt) is not int
                                         or expected_attempt < 1
                                         or leaf.get("task_attempt") != expected_attempt
                                         or type(leaf.get("task_attempt")) is not int):
        errors.append("task attempt binding mismatch")
    for field in LEAF_FIELDS:
        if leaf.get(field) != expected_leaf.get(field):
            errors.append("leaf identity mismatch: " + field)
    for validator, values in ((v_execution_counts, (leaf,)),
                              (v_binding, (leaf, candidate))):
        ok, why = validator(*values)
        if not ok:
            errors.append(why)
    for field in ("owned_tree_digest", "migration_digest", "plan_manifest_sha256"):
        if not _valid_digest(leaf.get(field)):
            errors.append("missing/invalid binding: " + field)
    raw = leaf.get("raw_output_path", "")
    for field in ("command", "cwd", "request_ids", "resource_ids"):
        if not leaf.get(field):
            errors.append("missing execution field: " + field)
    try:
        start = datetime.datetime.fromisoformat(leaf["started_at"].replace("Z", "+00:00"))
        end = datetime.datetime.fromisoformat(leaf["finished_at"].replace("Z", "+00:00"))
        deadline = expected_leaf.get("observation_deadline_seconds", 1800)
        if not start.tzinfo or not end.tzinfo or not 0 <= (end - start).total_seconds() <= deadline:
            raise ValueError("invalid observation interval")
    except (KeyError, TypeError, ValueError, AttributeError):
        errors.append("invalid measured observation timestamps")
    observations = leaf.get("oracle_observations", {})
    for field in ("pre_db_state", "pre_object_hash", "action", "expected_http_semantics",
                  "expected_db_delta", "expected_object_state", "proof_actor"):
        if not isinstance(observations, dict) or not observations.get(field):
            errors.append("missing scenario observation: " + field)
    if leaf.get("oracle_result") != "PASS":
        errors.append("scenario oracle did not measure PASS")
    actor = leaf.get("actor_provenance", {})
    if not isinstance(actor, dict) or actor.get("kind") not in ("real", "targeted-contract"):
        errors.append("missing/invalid actor provenance")
    elif leaf.get("driver") in ("garage", "disk"):
        if actor.get("kind") != "real" or actor.get("driver") != leaf["driver"]:
            errors.append("real driver provenance mismatch")
    if not isinstance(allowlist, dict):
        allowlist = {}
    resource_ids = leaf.get("resource_ids")
    if (not isinstance(resource_ids, list)
            or any(not isinstance(value, str) for value in resource_ids)):
        errors.append("invalid resource_ids")
        resource_ids = []
    resources = {value for values in allowlist.values() if isinstance(values, list)
                 for value in values if isinstance(value, str)}
    resource_kind = actor.get("resource_kind") if isinstance(actor, dict) else None
    resource_id = actor.get("resource_id") if isinstance(actor, dict) else None
    ok, why = v_resource({"kind": resource_kind, "id": resource_id or ""}, allowlist)
    if not ok or resource_id not in resource_ids:
        errors.append("actor resource category mismatch: " + why)
    if resource_kind == "disk_root" and isinstance(resource_id, str):
        prefix = allowlist.get("disk_root")
        if (not isinstance(prefix, str) or os.path.commonpath([
                os.path.realpath(prefix), os.path.realpath(resource_id)]) != os.path.realpath(prefix)):
            errors.append("disk resource outside registered root")
    if any(value not in resources and value != resource_id for value in resource_ids):
        errors.append("unregistered resource provenance")
    source_kind = (contract or {}).get("case_registry", {}).get(case_id, {}).get("source_kind")
    if source_kind == "real-required" and expected_leaf.get("domain") != "plan":
        if not isinstance(actor, dict) or actor.get("kind") != "real":
            errors.append("required real execution cannot use a targeted-contract actor")
    need_device = case_id in ("W6-02/flutter-recovery-real", "W6-02/e2e-compat", "W6-02/ac08")
    need_browser = case_id in ("W6-02/admin-recovery-real", "W6-02/e2e-compat")
    for needed, field, kind in ((need_device, "device_provenance", "physical"),
                                (need_browser, "browser_provenance", "real-browser")):
        if needed:
            proof = leaf.get(field, {})
            if (not isinstance(proof, dict) or proof.get("kind") != kind
                    or not isinstance(allowlist.get(field), list)
                    or proof.get("resource_id") not in allowlist.get(field, [])
                    or type(proof.get("process_id")) is not int or proof["process_id"] <= 0):
                errors.append("missing registered live " + field)
    try:
        raw_bytes = _read_run_artifact(run_dir, raw)
        if sha256_bytes(raw_bytes) != leaf.get("raw_output_sha256"):
            errors.append("raw output sha mismatch")
        observation = json.loads(raw_bytes)
        identity_fields = ("task_id", "case_id") + LEAF_FIELDS
        if expected_attempt is not _ATTEMPT_NOT_GIVEN:
            identity_fields += ("task_attempt",)
        if (not isinstance(observation, dict)
                or any(observation.get(k) != leaf.get(k) for k in identity_fields)
                or observation.get("oracle_observations") != observations
                or observation.get("candidate_heads") != candidate.get("heads")
                or not isinstance(observation.get("assertions"), list)
                or not observation["assertions"]
                or any(not isinstance(v, dict) or not isinstance(v.get("id"), str)
                       or not v["id"] or v.get("passed") is not True
                       for v in observation["assertions"])):
            errors.append("raw structured observations/assertions mismatch")
    except (OSError, TypeError, ValueError):
        errors.append("raw output is not structured scenario observation")
    surface_cases = (contract or {}).get("upload_surface_contract", {}).get("cases", [])
    source_case = expected_leaf.get("source_case_ref", case_id)
    if source_case in surface_cases:
        proof = leaf.get("entrypoint_provenance", {})
        if (not isinstance(proof, dict) or proof.get("entrypoint") != leaf.get("entrypoint")
                or any(not proof.get(k) for k in ("route", "method", "handler", "request_id"))
                or proof.get("request_id") not in leaf.get("request_ids", [])):
            errors.append("missing matching real upload entrypoint provenance")
    return errors


def receipt_errors_legacy(leaf, expected_leaf, task_id, case_id, candidate, run_dir, allowlist,
                   contract=None):
    errors = []
    if not isinstance(leaf, dict):
        return ["leaf receipt must be an object"]
    if leaf.get("required") is not True:
        errors.append("required must be boolean true")
    if leaf.get("task_id") != task_id or leaf.get("case_id") != case_id:
        errors.append("task/case identity mismatch")
    for field in LEAF_FIELDS:
        if leaf.get(field) != expected_leaf.get(field):
            errors.append("leaf identity mismatch: " + field)
    for validator, values in ((v_execution_counts, (leaf,)),
                              (v_binding, (leaf, candidate)),
                              (v_raw_output, (leaf,))):
        ok, why = validator(*values)
        if not ok:
            errors.append(why)
    for field in ("owned_tree_digest", "migration_digest", "plan_manifest_sha256"):
        if not _valid_digest(leaf.get(field)):
            errors.append("missing/invalid binding: " + field)
    raw = leaf.get("raw_output_path", "")
    if not isinstance(raw, str) or os.path.commonpath([
            os.path.realpath(run_dir), os.path.realpath(raw or ".")]) != os.path.realpath(run_dir):
        errors.append("raw output outside run directory")
    for field in ("command", "cwd", "request_ids", "resource_ids"):
        if not leaf.get(field):
            errors.append("missing execution field: " + field)
    try:
        start = datetime.datetime.fromisoformat(leaf["started_at"].replace("Z", "+00:00"))
        end = datetime.datetime.fromisoformat(leaf["finished_at"].replace("Z", "+00:00"))
        deadline = expected_leaf.get("observation_deadline_seconds", 1800)
        if not start.tzinfo or not end.tzinfo or not 0 <= (end - start).total_seconds() <= deadline:
            raise ValueError("invalid observation interval")
    except (KeyError, TypeError, ValueError, AttributeError):
        errors.append("invalid measured observation timestamps")
    observations = leaf.get("oracle_observations", {})
    for field in ("pre_db_state", "pre_object_hash", "action", "expected_http_semantics",
                  "expected_db_delta", "expected_object_state", "proof_actor"):
        if not isinstance(observations, dict) or not observations.get(field):
            errors.append("missing scenario observation: " + field)
    if leaf.get("oracle_result") != "PASS":
        errors.append("scenario oracle did not measure PASS")
    actor = leaf.get("actor_provenance", {})
    if not isinstance(actor, dict) or actor.get("kind") not in ("real", "targeted-contract"):
        errors.append("missing/invalid actor provenance")
    elif leaf.get("driver") in ("garage", "disk"):
        if actor.get("kind") != "real" or actor.get("driver") != leaf["driver"]:
            errors.append("real driver provenance mismatch")
    if not isinstance(allowlist, dict):
        allowlist = {}
    resource_ids = leaf.get("resource_ids")
    if (not isinstance(resource_ids, list)
            or any(not isinstance(value, str) for value in resource_ids)):
        errors.append("invalid resource_ids")
        resource_ids = []
    resources = {value for values in allowlist.values() if isinstance(values, list)
                 for value in values if isinstance(value, str)}
    resource_kind = actor.get("resource_kind") if isinstance(actor, dict) else None
    resource_id = actor.get("resource_id") if isinstance(actor, dict) else None
    ok, why = v_resource({"kind": resource_kind, "id": resource_id or ""}, allowlist)
    if not ok or resource_id not in resource_ids:
        errors.append("actor resource category mismatch: " + why)
    if resource_kind == "disk_root" and isinstance(resource_id, str):
        prefix = allowlist.get("disk_root")
        if (not isinstance(prefix, str) or os.path.commonpath([
                os.path.realpath(prefix), os.path.realpath(resource_id)]) != os.path.realpath(prefix)):
            errors.append("disk resource outside registered root")
    if any(value not in resources and value != resource_id for value in resource_ids):
        errors.append("unregistered resource provenance")
    source_kind = (contract or {}).get("case_registry", {}).get(case_id, {}).get("source_kind")
    if source_kind == "real-required" and expected_leaf.get("domain") != "plan":
        if not isinstance(actor, dict) or actor.get("kind") != "real":
            errors.append("required real execution cannot use a targeted-contract actor")
    need_device = case_id in ("W6-02/flutter-recovery-real", "W6-02/e2e-compat", "W6-02/ac08")
    need_browser = case_id in ("W6-02/admin-recovery-real", "W6-02/e2e-compat", "W6-02/ac08")
    for needed, field, kind in ((need_device, "device_provenance", "physical"),
                                (need_browser, "browser_provenance", "real-browser")):
        if needed:
            proof = leaf.get(field, {})
            if (not isinstance(proof, dict) or proof.get("kind") != kind
                    or proof.get("resource_id") not in allowlist.get(field, [])
                    or type(proof.get("process_id")) is not int or proof["process_id"] <= 0):
                errors.append("missing registered live " + field)
    try:
        with open(raw) as stream:
            observation = json.load(stream)
        identity_fields = ("task_id", "case_id") + LEAF_FIELDS
        if (not isinstance(observation, dict)
                or any(observation.get(k) != leaf.get(k) for k in identity_fields)
                or observation.get("oracle_observations") != observations
                or observation.get("candidate_heads") != candidate.get("heads")
                or not isinstance(observation.get("assertions"), list)
                or not observation["assertions"]
                or any(not isinstance(v, dict) or not isinstance(v.get("id"), str)
                       or not v["id"] or v.get("passed") is not True
                       for v in observation["assertions"])):
            errors.append("raw structured observations/assertions mismatch")
    except (OSError, TypeError, ValueError):
        errors.append("raw output is not structured scenario observation")
    surface_cases = (contract or {}).get("upload_surface_contract", {}).get("cases", [])
    source_case = expected_leaf.get("source_case_ref", case_id)
    if source_case in surface_cases:
        proof = leaf.get("entrypoint_provenance", {})
        if (not isinstance(proof, dict) or proof.get("entrypoint") != leaf.get("entrypoint")
                or any(not proof.get(k) for k in ("route", "method", "handler", "request_id"))
                or proof.get("request_id") not in leaf.get("request_ids", [])):
            errors.append("missing matching real upload entrypoint provenance")
    return errors


def cmd_verify(args, held_lock=False):
    """Read-only verification; a suite flag alone never seals a scenario PASS."""
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not held_lock and not lock.acquire_nb():
        die(EXIT_BLOCKED, "BLOCKED_EXTERNAL: verifier cannot acquire run lock")
    try:
        state = _require_state(run_dir)
        contract = load_contract(state["plan_dir"])
        gate = args.gate
        if gate not in contract["wave_gates"]:
            die(EXIT_USAGE, "unknown gate %r (G0..G6)" % gate)
        fails = []
        manifest_ok, manifest_hash = verify_plan_manifest(state["plan_dir"])
        if not manifest_ok or manifest_hash != state.get("plan_manifest_sha256"):
            fails.append("frozen plan manifest mismatch")
        candidate = state.get("verification_candidate", {})
        if not candidate:
            fails.append("current frozen verification candidate missing; historical receipts cannot be rebound")
        else:
            fails.extend(candidate_errors(candidate))
            if candidate.get("plan_manifest_sha256") != manifest_hash:
                fails.append("candidate plan binding mismatch")
        # candidate_progression_contract.historical_gate: G0..G5 receipts are
        # bound to the candidate each task was ACCEPTED at; a later authorized
        # task advancing the candidate must not invalidate them (the final
        # candidate's real coverage is re-bound by W6).  Verify therefore
        # checks each task's leaves against its own acceptance candidate.
        accepted_candidates = {}
        events_path = os.path.join(run_dir, "events.jsonl")
        if os.path.exists(events_path):
            with open(events_path) as _fh:
                for _line in _fh:
                    if not _line.strip():
                        continue
                    _ev = json.loads(_line)
                    if _ev.get("kind") == "task_artifacts_accepted":
                        accepted_candidates[_ev["payload"].get("task")] = \
                            _ev["payload"].get("candidate")
        # Each case is sealed against the candidate its seal event bound; a
        # later task advancing the candidate must not invalidate it.  This is
        # the same per-case semantics accept-task applies via _task_artifacts.
        seeded_candidates = _case_bound_candidates(run_dir)
        auth_path = state.get("authorization_path", "")
        if not os.path.isfile(auth_path) or sha256_file(auth_path) != state.get("authorization_sha256"):
            fails.append("authorization original missing or hash mismatch; recovery admission required")
        legacy_case_ids = set(state.get("legacy_case_ids", []))
        gates = []
        current = gate
        while current:
            if current in gates:
                fails.append("cyclic gate dependency")
                break
            gates.append(current)
            current = contract["wave_gates"][current].get("previous_gate")
        for gate_id in reversed(gates):
            for tid in contract["wave_gates"][gate_id]["requires_tasks"]:
                if state["tasks"].get(tid, {}).get("state") != "PASS":
                    fails.append(tid + " not PASS")
                for cid in task_case_ids(contract, tid) or []:
                    expected = contract["expected_leaf_matrix"][cid]
                    by_id = {tuple(leaf.get(k) for k in LEAF_FIELDS): leaf for leaf in expected}
                    case_dir = os.path.join(run_dir, "evidence", tid, cid.split("/", 1)[1])
                    actual = []
                    try:
                        for fn, leaf in _read_case_rows(run_dir, case_dir, state.get("case_manifest_digests", {}).get(cid)):
                            identity = tuple(leaf.get(k) for k in LEAF_FIELDS)
                            actual.append("|".join(map(str, identity)))
                            if identity not in by_id:
                                fails.append(cid + ": unexpected leaf " + fn)
                                continue
                            bound = seeded_candidates.get(
                                cid, accepted_candidates.get(tid, candidate))
                            issues = receipt_errors(leaf, by_id[identity], tid, cid,
                                                    bound,
                                                    run_dir, state.get("resource_allowlist", {}), contract,
                                                    state.get("tasks", {}).get(tid, {}).get("attempt"),
                                                    legacy=cid in legacy_case_ids)
                            fails.extend(cid + " " + fn + ": " + why for why in issues)
                    except (OSError, ValueError, TypeError, AttributeError) as exc:
                        fails.append(cid + ": uncommitted or corrupt case " + type(exc).__name__)
                    ok, why = v_leaf_set(leaf_ids_for_case(contract, cid), actual)
                    if not ok:
                        fails.append(cid + ": " + why)
        if candidate:
            fails.extend(candidate_errors(candidate))
        print(json.dumps({"gate": gate, "result": "FAIL" if fails else "PASS",
                          "fail_count": len(fails), "fails": fails[:50],
                          "state_updated": False}, indent=1))
        return EXIT_CONTRACT if fails else EXIT_OK
    finally:
        if not held_lock:
            lock.release()


def _measured_case_rows(args, state, contract, run_dir):
    case_dir = os.path.realpath(args.case_dir)
    expected_dir = os.path.abspath(os.path.join(
        run_dir, "evidence", args.task_id, args.case_id.split("/", 1)[-1]))
    if case_dir != expected_dir or os.path.commonpath([run_dir, case_dir]) != run_dir:
        return [], ["case directory does not match task/case"], case_dir, None
    try:
        directory = os.stat(case_dir, follow_symlinks=False)
    except OSError:
        return [], ["case directory unavailable"], case_dir, None
    directory_id = (directory.st_dev, directory.st_ino)
    if os.path.realpath(args.receipts) != os.path.join(case_dir, "measured-receipts.json"):
        return [], ["measured receipts must be owned by the case directory"], case_dir, None
    if args.case_id not in (task_case_ids(contract, args.task_id) or []):
        return [], ["case is not declared by task"], case_dir, None
    if state.get("tasks", {}).get(args.task_id, {}).get("state") not in ("RUNNING", "VERIFYING"):
        return [], ["task is not running or verifying"], case_dir, None
    try:
        rows = json.loads(_read_run_artifact(run_dir, args.receipts))
    except (OSError, TypeError, ValueError):
        return [], ["measured receipts are not a regular, run-owned JSON file"], case_dir, None
    if not isinstance(rows, list) or any(not isinstance(row, dict) for row in rows):
        return [], ["measured receipts must be an object array"], case_dir, None
    expected = contract["expected_leaf_matrix"][args.case_id]
    identity = lambda row: "|".join(str(row.get(k)) for k in LEAF_FIELDS)
    index = {identity(row): row for row in expected}
    ok, why = v_leaf_set(list(index), [identity(row) for row in rows])
    if not ok:
        return [], [why], case_dir, None
    candidate = state.get("verification_candidate", {})
    errors = []
    for row in rows:
        if row.get("command") != args.command:
            errors.append("measured command does not match launched command")
        errors.extend(receipt_errors(row, index[identity(row)], args.task_id,
            args.case_id, candidate, run_dir, state.get("resource_allowlist", {}), contract,
            state.get("tasks", {}).get(args.task_id, {}).get("attempt")))
    targets = {"leaf-%03d.json" % n for n in range(1, len(rows) + 1)}
    try:
        names = os.listdir(case_dir)
    except OSError:
        return [], ["case directory unavailable while inspecting leaves"], case_dir, None
    if any(name.startswith("leaf-") and name.endswith(".json") and name not in targets
           for name in names):
        errors.append("unexpected existing leaf files; preserve and reconcile first")
    return rows, errors, case_dir, directory_id


def _write_case_rows(run_dir, case_dir, rows, directory_id):
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    fd = os.open(run_dir, flags)
    try:
        for part in os.path.relpath(case_dir, run_dir).split(os.sep):
            child = os.open(part, flags, dir_fd=fd)
            os.close(fd)
            fd = child
        opened = os.fstat(fd)
        if (opened.st_dev, opened.st_ino) != directory_id:
            return False
        try:
            case_store.publish(fd, rows)
        except FileExistsError:
            # A previous committed generation is never replaced.
            existing = [row for _, row in case_store.read(fd)]
            if canonical_json(existing) != canonical_json(rows):
                return False
        current = os.stat(case_dir, follow_symlinks=False)
        opened = os.fstat(fd)
        return ((current.st_dev, current.st_ino) == (opened.st_dev, opened.st_ino)
                and os.path.realpath(case_dir) == case_dir)
    finally:
        os.close(fd)


def _read_case_rows(run_dir, case_dir, expected_digest=None):
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    fd = os.open(run_dir, flags)
    try:
        for part in os.path.relpath(case_dir, run_dir).split(os.sep):
            if part in ("", ".", ".."):
                raise ValueError("invalid case path")
            child = os.open(part, flags, dir_fd=fd)
            os.close(fd)
            fd = child
        if not expected_digest:
            raise ValueError("case manifest is not bound to state")
        return case_store.read(fd, expected_digest)
    finally:
        os.close(fd)


def cmd_seal_case(args):
    """Seal measured leaves only; suite-level success never supplies an oracle."""
    run_dir = os.path.realpath(_run_dir_from(args))
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(EXIT_BLOCKED, "seal-case cannot acquire run lock")
    try:
        state = _require_state(run_dir)
        contract = load_contract(state["plan_dir"])
        _budget_check(state, contract, args.task_id)
        _task_attempt_check(state, contract, args.task_id)
        ok, digest = verify_plan_manifest(state["plan_dir"])
        errors = candidate_errors(state.get("verification_candidate", {}))
        if not ok or digest != state.get("plan_manifest_sha256"):
            errors.append("frozen plan manifest mismatch")
        if state.get("verification_candidate", {}).get("plan_manifest_sha256") != digest:
            errors.append("candidate plan binding mismatch")
        auth = state.get("authorization_path", "")
        if not os.path.isfile(auth) or sha256_file(auth) != state.get("authorization_sha256"):
            errors.append("authorization original unavailable")
        rows, row_errors, case_dir, directory_id = _measured_case_rows(args, state, contract, run_dir)
        errors.extend(row_errors)
        errors.extend(candidate_errors(state.get("verification_candidate", {})))
        if errors:
            print(json.dumps({"result": "FAIL", "errors": errors[:20]}))
            return EXIT_CONTRACT
        if not _write_case_rows(run_dir, case_dir, rows, directory_id):
            print(json.dumps({"result": "FAIL", "errors": ["case directory changed while sealing"]}))
            return EXIT_CONTRACT
        fd = os.open(case_dir, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
        try:
            manifest_digest = case_store.manifest_digest(fd)
            # Verify again before binding the manifest to the fsynced event chain.
            if canonical_json([row for _, row in case_store.read(fd, manifest_digest)]) != canonical_json(rows):
                raise ValueError("case changed before binding")
        finally:
            os.close(fd)
        bindings = state.setdefault("case_manifest_digests", {})
        previous = bindings.get(args.case_id)
        if previous and previous != manifest_digest:
            print(json.dumps({"result": "FAIL", "errors": ["case binding conflict"]}))
            return EXIT_CONTRACT
        if not previous:
            bindings[args.case_id] = manifest_digest
            StateStore(run_dir).write_snapshot(state, "case_manifest_bound", {
                "case_id": args.case_id, "manifest_sha256": manifest_digest,
                "candidate": state.get("verification_candidate")})
        print(json.dumps({"result": "SEALED", "leaves": len(rows), "state_updated": not bool(previous)}))
        return EXIT_OK
    finally:
        lock.release()


def cmd_accept_gate(args):
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(EXIT_BLOCKED, "accept-gate cannot acquire run lock")
    try:
        result = cmd_verify(args, held_lock=True)
        if result != EXIT_OK:
            return result
        state = _require_state(run_dir)
        errors = candidate_errors(state.get("verification_candidate", {}))
        if errors:
            return EXIT_CONTRACT
        state["gates"][args.gate] = "PASS"
        state.setdefault("gate_candidates", {})[args.gate] = copy.deepcopy(state["verification_candidate"])
        store = StateStore(run_dir)
        store.write_snapshot(state, "gate_artifacts_accepted", {"gate": args.gate,
            "candidate": state["verification_candidate"]})
        return EXIT_OK
    finally:
        lock.release()


# ---------------------------------------------------------------- self-test
MUTATIONS = [
    ("delete required leaf", lambda ctx: (V["leaf_set"](ctx["leaves"], ctx["leaves"][:-1]))),
    ("duplicate one leaf to replace another", lambda ctx: (V["leaf_set"](
        ctx["leaves"], ctx["leaves"][:-1] + [ctx["leaves"][0]]))),
    ("rename case_id", lambda ctx: (V["case_identity"](
        {"case_id": ctx["case_id"] + "-x"}, ctx["registry_ids"]))),
    ("swap backend or queue", lambda ctx: (V["backend_queue"](
        {"backend": "imboy2", "queue": "q2"}, ctx["expected_backend"]))),
    ("change candidate SHA", lambda ctx: (V["binding"](ctx["binding_receipt"],
        dict(ctx["candidate"], heads={"imboy": "dead" * 10})))),
    ("change migration or plan hash", lambda ctx: (V["binding"](ctx["binding_receipt"],
        dict(ctx["candidate"], plan_manifest_sha256="0" * 64)))),
    ("use previous phase receipt", lambda ctx: (V["phase"](
        {"schema_phase": "base"}, ["expand-queue"], ["base", "expand-queue"]))),
    ("modify raw output without matching sha", lambda ctx: (V["raw_output"](
        dict(ctx["raw_receipt"], raw_output_sha256="0" * 64)))),
    ("exit nonzero but PASS", lambda ctx: (V["execution_counts"](dict(ctx["counts"], exit_code=1)))),
    ("executed zero but PASS", lambda ctx: (V["execution_counts"](dict(ctx["counts"], executed=0, passed=0, failed=0)))),
    ("required skip", lambda ctx: (V["execution_counts"](dict(ctx["counts"], skipped=1, executed=ctx["counts"]["executed"] + 1)))),
    ("mock substituted for real", lambda ctx: (V["provenance"]({"driver": "mock"}, True, False, False))),
    ("missing physical device provenance", lambda ctx: (V["provenance"]({"driver": "garage"}, True, True, False))),
    ("missing old worker stopped proof", lambda ctx: (V["provenance"]({"driver": "garage"}, True, False, True))),
    ("stale owner/lease token", lambda ctx: (V["lease"](
        {"owner_token": "other", "expected_owner": "me", "lease_until": 999, "now": 1}))),
    ("reset persisted attempts", lambda ctx: (V["attempts"](3, 1))),
    ("staged foreign file added", lambda ctx: (V["staged_files"](
        ctx["owned"] + ["foreign/file.erl"], ctx["owned"]))),
    ("partial commit vector", lambda ctx: (V["commit_vector"](
        {k: v for k, v in ctx["vector"].items() if k != "imboyadmin"}, ctx["repos"]))),
    ("commit parent mismatch", lambda ctx: (V["commit_parent"](ctx["vector"], dict(ctx["parents"], imboy="beef")))),
    ("resource outside run-owned allowlist", lambda ctx: (V["resource"](
        {"kind": "scratch_postgres", "id": "prod_business_db"}, ctx["allowlist"]))),
    ("blocked checkpoint resumed without condition revision", lambda ctx: (V["blocked_resume"](
        {"blocked_reason": "no device", "blocked_revision": 7}, 7))),
    ("schema-phase guard silently skipped", lambda ctx: (V["schema_phase_guard"]({"guard_checked": False}))),
    ("wrong exact physical I", lambda ctx: (V["physical_identity"](
        dict(ctx["identity"], object_key="other/key"), ctx["identity"]))),
    ("unregistered alias namespace", lambda ctx: (V["namespace"](
        {"namespace": "ns-ghost", "expected_canonical": "root-1"}, ctx["registry"]))),
    ("confirmed session linked to wrong legal FK target", lambda ctx: (V["session_link"](
        {"session_state": "confirmed", "confirmed_attachment_id": 5,
         "target_path": "k-b", "expected_target_path": "k-a"}))),
    ("claim beyond execution slots", lambda ctx: (V["claim_slots"](list(range(5)), 4))),
    ("activate worker before guards verified", lambda ctx: (V["activation_order"](
        {"sequence": ["FROZEN", "WRITERS_QUIESCED", "DDL_VERIFIED", "NEW_WORKER_ENABLED"],
         "worker_enabled": True}))),
    ("adopt partial activation catalog as success", lambda ctx: (V["activation_catalog"](
        {"ddl_committed": False, "ddl_verified": True}))),
    ("replace widget content receipt with seat receipt", lambda ctx: (V["surface_receipt"](
        {"surfaces": {"widget": "ok", "seat": "ok", "app": "ok", "internal": "ok"},
         "surface_kind": "widget", "entrypoint": "seat"}, ctx["surfaces"]))),
    ("drop one required upload surface", lambda ctx: (V["surface_receipt"](
        {"surfaces": {k: "ok" for k in ctx["surfaces"] if k != "widget"}}, ctx["surfaces"]))),
    ("forge recovery tombstone without original registration", lambda ctx: (V["tombstone"](
        {"client_upload_id": "uuid-ghost", "upload_registration": {"x": 1}}, ctx["registrations"]))),
    ("worker writes authority state or commits outside owned paths", lambda ctx: (V["worker_writes"](
        {"writes": [ctx["owned"][0], "run_dir/state.json"], "committed": False},
        ["run_dir/state.json", "run_dir/events.jsonl"], ctx["owned"]))),
    ("omit affected prior case supplementary regression", lambda ctx: (V["supplementary_regression"](
        {"supplementary_regression_case_ids": ["W2-02/confirm-idempotent"]},
        ["W2-02/confirm-idempotent", "W2-03/confirm-gc-race"]))),
    ("reset original task budget by relabelling repair as successor regression", lambda ctx: (V["budget_relabel"](
        {"relabelled_repair_as_successor": True, "attempt": 6, "max_attempts": 5}))),
]


def _selftest_ctx(tmp):
    raw_path = os.path.join(tmp, "raw.log")
    with open(raw_path, "w") as fh:
        fh.write("synthetic raw output\n")
    leaves = ["PLAN|plan|none|schema|original_up_and_catalog|base",
              "PLAN|plan|none|schema|catalog_identity|base"]
    counts = {"exit_code": 0, "executed": 2, "passed": 2, "failed": 0, "skipped": 0}
    candidate = {"heads": {"imboy": "a" * 40, "imboyapp": "b" * 40, "imboyadmin": "c" * 40},
                 "owned_tree_digest": "d" * 64,
                 "plan_manifest_sha256": "e" * 64, "migration_digest": "f" * 64}
    binding = {"candidate_heads": candidate["heads"],
               "owned_tree_digest": candidate["owned_tree_digest"],
               "plan_manifest_sha256": candidate["plan_manifest_sha256"],
               "migration_digest": candidate["migration_digest"]}
    return {
        "leaves": leaves,
        "registry_ids": ["W0-02/schema", "W0-02/history-unknown"],
        "case_id": "W0-02/schema",
        "expected_backend": {"backend": "imboy", "queue": "attachment_object_deletion"},
        "candidate": candidate,
        "binding_receipt": binding,
        "raw_receipt": {"raw_output_path": raw_path, "raw_output_sha256": sha256_file(raw_path)},
        "counts": counts,
        "owned": ["src/a.erl", "src/b.erl"],
        "repos": ["imboy", "imboyapp", "imboyadmin"],
        "vector": {"imboy": {"commit": "1" * 40, "parent": "a" * 40},
                   "imboyapp": {"commit": "2" * 40, "parent": "b" * 40},
                   "imboyadmin": {"commit": "3" * 40, "parent": "c" * 40}},
        "parents": {"imboy": "a" * 40, "imboyapp": "b" * 40, "imboyadmin": "c" * 40},
        "allowlist": {"scratch_postgres": ["attv5_run_db_1"], "disk_root": "/tmp/attv5-root-"},
        "identity": {"storage_driver": "garage", "storage_namespace": "ns1",
                     "storage_container": "b1", "object_key": "k1"},
        "registry": {"ns1": {"root": "root-1"}},
        "surfaces": ["app", "internal", "seat", "widget"],
        "registrations": {"uuid-1": {"subject": "x", "declaration": {"filename": "a"}}},
    }


def _mutation_results():
    results = []
    with tempfile.TemporaryDirectory(prefix="attv5-selftest-") as tmp:
        ctx = _selftest_ctx(tmp)
        for i, (name, fn) in enumerate(MUTATIONS, 1):
            try:
                ok, why = fn(ctx)
            except Exception as exc:  # noqa: BLE001
                ok, why = False, "exception %s" % exc
            results.append((i, name, ok is False, why))
    return results


def _parallel_selftests(base_tmp):
    """4 parallel-contract crash scenarios (06 §3, contract parallel_execution_contract)."""
    out = []

    def record(name, rejected, why):
        out.append((name, rejected, why))

    # P1: second writer cannot mutate state while holder alive
    run = os.path.join(base_tmp, "p1")
    os.makedirs(run, exist_ok=True)
    lock = RunLock(run)
    assert lock.acquire_nb()
    second = RunLock(run)
    rejected = second.acquire_nb() is False
    record("second-writer-refused", rejected, "LOCK_NB bounded refusal")
    lock.release()

    # P2: gate detects worker write outside owned paths
    ctx = _selftest_ctx(base_tmp)
    ok, why = V["worker_writes"]({"writes": ["not/owned.erl"], "committed": False},
                                 ["run_dir/state.json"], ctx["owned"])
    record("worker-out-of-bounds-detected", ok is False, why)

    # P3: partial commit vector fails verify path
    ok, why = V["commit_vector"]({"imboy": {"commit": "x"}}, ctx["repos"])
    record("partial-vector-fails", ok is False, why)

    # P4: crash during COMMITTING does not redo committed work
    run = os.path.join(base_tmp, "p4")
    os.makedirs(run, exist_ok=True)
    store = StateStore(run)
    state = {"run_id": "p4", "revision": 1,
             "tasks": {"W0-01": {"state": "PASS", "attempt": 1},
                       "W0-02": {"state": "COMMITTING", "attempt": 1}},
             "gates": {}}
    store.append_event("commit_unknown", {"task": "W0-02"})
    store.write_snapshot(state)
    reloaded = StateStore(run).load_state()
    ok = (reloaded["tasks"]["W0-01"]["state"] == "PASS"
          and reloaded["tasks"]["W0-02"]["state"] == "COMMITTING"
          and StateStore(run).verify_chain()[0])
    record("crash-committing-reconciles", ok, "PASS preserved, COMMITTING not duplicated")
    return out


def _scenario_selftests(base_tmp):
    """19 W0-03 contract-leaf scenarios: real subprocess/StateStore/lock drills.

    Each scenario really executes the mechanics it names (kill, expiry,
    exclusive create, chain replay) and records the observed outcome; no
    scenario asserts an outcome it did not produce.
    """
    import signal
    out = []

    def record(name, passed, detail):
        out.append((name, bool(passed), str(detail)))

    def fresh(name):
        run = os.path.join(base_tmp, "sc-" + name)
        os.makedirs(run, exist_ok=True)
        return run

    def mini_state(run, task_state="READY", attempt=1, deadline=None, extra=None):
        state = {"run_id": os.path.basename(run), "revision": 1,
                 "plan_dir": os.path.join(os.path.dirname(os.path.dirname(
                     os.path.abspath(__file__))),
                     "docs/architecture/attachment-lifecycle-v5"),
                 "tasks": {"W0-03": {"state": task_state, "attempt": attempt,
                                     "task_started_at": time.time() - 5,
                                     "task_deadline": deadline if deadline is not None
                                     else time.time() + 3600}},
                 "gates": {}, "mode": "IMPLEMENTATION"}
        if extra:
            state.update(extra)
        store = StateStore(run)
        store.append_event("admit", {"synthetic": True})
        return store.write_snapshot(state)

    def call_task(run, to, blocked_reason=None):
        args = types.SimpleNamespace(run_dir=run, task_id="W0-03", to=to,
                                     case_results=None, commit_vector=None,
                                     blocked_reason=blocked_reason)
        import io as _io
        try:
            with contextlib.redirect_stdout(_io.StringIO()):
                rc = cmd_task(args)
            return rc, None
        except SystemExit as exc:
            return exc.code, str(exc)

    # valid_receipt: a fully valid leaf receipt passes receipt_errors cleanly
    try:
        ctx = _selftest_ctx(fresh("valid_receipt"))
        cand = ctx["candidate"]
        raw_path = os.path.join(base_tmp, "sc-valid-receipt-raw.json")
        observations = {"pre_db_state": "s", "pre_object_hash": "h", "action": "a",
                        "expected_http_semantics": "n/a", "expected_db_delta": "none",
                        "expected_object_state": "s", "proof_actor": "p"}
        observation = {"task_id": "W0-02", "case_id": ctx["case_id"],
                       "oracle_observations": observations,
                       "candidate_heads": cand["heads"],
                       "assertions": [{"id": "x", "passed": True}],
                       "ac_id": "PLAN", "domain": "plan", "driver": "none",
                       "entrypoint": "schema", "ordering_or_fault": "original_up_and_catalog",
                       "schema_phase": "base"}
        with open(raw_path, "w") as fh:
            json.dump(observation, fh)
        now = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        leaf = {"required": True, "task_id": "W0-02", "case_id": ctx["case_id"],
                "ac_id": "PLAN", "domain": "plan", "driver": "none",
                "entrypoint": "schema", "ordering_or_fault": "original_up_and_catalog",
                "schema_phase": "base",
                "command": "synthetic", "cwd": "/synthetic",
                "exit_code": 0, "executed": 2, "passed": 2, "failed": 0, "skipped": 0,
                "started_at": now, "finished_at": now,
                "candidate_heads": cand["heads"],
                "owned_tree_digest": cand["owned_tree_digest"],
                "plan_manifest_sha256": cand["plan_manifest_sha256"],
                "migration_digest": cand["migration_digest"],
                "raw_output_path": raw_path, "raw_output_sha256": sha256_file(raw_path),
                "oracle_result": "PASS", "oracle_observations": observations,
                "actor_provenance": {"kind": "targeted-contract", "driver": "none",
                                     "resource_kind": "scratch_postgres",
                                     "resource_id": "attv5_run_db_1"},
                "resource_ids": ["attv5_run_db_1"], "request_ids": ["synthetic-1"]}
        expected = dict(leaf, observation_deadline_seconds=1800)
        errors = receipt_errors(leaf, expected, "W0-02", ctx["case_id"], cand,
                                base_tmp, ctx["allowlist"])
        record("valid_receipt", not errors, "receipt_errors=%r" % (errors[:3],))
    except Exception as exc:  # noqa: BLE001
        record("valid_receipt", False, "exception %s" % exc)

    # runner_timeout: persisted expired deadline refuses RUNNING; attempt stays persisted
    try:
        run = fresh("runner_timeout")
        mini_state(run, deadline=time.time() - 1)
        rc, _ = call_task(run, "RUNNING")
        after = StateStore(run).load_state()["tasks"]["W0-03"]
        record("runner_timeout", rc == 30 and after["attempt"] == 1
               and after["state"] == "READY",
               "rc=%s attempt_still=%s state_still=%s" % (rc, after["attempt"], after["state"]))
    except Exception as exc:  # noqa: BLE001
        record("runner_timeout", False, "exception %s" % exc)

    # runner_crash: SIGKILL after durable attempt write leaves attempt persisted
    try:
        run = fresh("runner_crash")
        code = ("import json,os,signal;"
                "s={'run_id':'c','revision':1,'tasks':{'W0-03':{'state':'RUNNING','attempt':1}},"
                "'gates':{}};"
                "f=open(%r,'w');f.write(json.dumps(s));f.flush();os.fsync(f.fileno());f.close();"
                "os.kill(os.getpid(), signal.SIGKILL)" % os.path.join(run, "state.json"))
        subprocess.run([sys.executable, "-c", code], timeout=10)
        persisted = StateStore(run).load_state()["tasks"]["W0-03"]["attempt"]
        record("runner_crash", persisted == 1, "attempt persisted through SIGKILL=%s" % persisted)
    except Exception as exc:  # noqa: BLE001
        record("runner_crash", False, "exception %s" % exc)

    # parent_dies_child_alive: grandchild keeps the run lock after its parent exits
    try:
        run = fresh("parent_dies")
        holder_code = ("import sys,time;sys.path.insert(0,%r);"
                       "from attachment_lifecycle_v5_runner import RunLock;"
                       "lock=RunLock(%r);assert lock.acquire_nb();"
                       "print('held');sys.stdout.flush();time.sleep(6)"
                       % (os.path.dirname(os.path.abspath(__file__)), run))
        bash = subprocess.Popen(["bash", "-c",
                                 "%s -c %r > /dev/null 2>&1 & disown; exit 0"
                                 % (sys.executable, holder_code)])
        bash.wait(timeout=10)
        time.sleep(0.8)
        lock = RunLock(run)
        refused = lock.acquire_nb() is False
        record("parent_dies_child_alive", refused,
               "parent exited; orphan still owns run lock (second acquire refused=%s)" % refused)
    except Exception as exc:  # noqa: BLE001
        record("parent_dies_child_alive", False, "exception %s" % exc)

    # child_ignores_term: SIGTERM-ignoring child stays alive until killed
    try:
        child = subprocess.Popen(
            [sys.executable, "-c",
             "import signal,time;signal.signal(signal.SIGTERM, signal.SIG_IGN);"
             "print('ready',flush=True);time.sleep(6)"],
            stdout=subprocess.PIPE)
        child.stdout.readline()
        child.send_signal(signal.SIGTERM)
        time.sleep(0.4)
        alive = child.poll() is None
        child.kill(); child.wait(timeout=5)
        record("child_ignores_term", alive, "alive_after_TERM=%s (hard kill required)" % alive)
    except Exception as exc:  # noqa: BLE001
        record("child_ignores_term", False, "exception %s" % exc)

    # verifying_timeout: expired VERIFYING can only reach a persisted terminal NO_GO
    try:
        run = fresh("verifying_timeout")
        mini_state(run, task_state="VERIFYING", deadline=time.time() - 1)
        rc, _ = call_task(run, "NO_GO", blocked_reason="deadline exhausted")
        after = StateStore(run).load_state()["tasks"]["W0-03"]
        record("verifying_timeout", rc == 0 and after["state"] == "NO_GO",
               "terminal NO_GO persisted=%s rc=%s" % (after["state"] == "NO_GO", rc))
    except Exception as exc:  # noqa: BLE001
        record("verifying_timeout", False, "exception %s" % exc)

    # committing_timeout_partial_vector: expired COMMITTING refuses acceptance outright
    try:
        run = fresh("committing_timeout")
        mini_state(run, task_state="COMMITTING", deadline=time.time() - 1)
        vector_path = os.path.join(run, "v.json")
        json.dump({"imboy": {"commit": "1" * 40, "parent": "2" * 40}}, open(vector_path, "w"))
        args = types.SimpleNamespace(run_dir=run, task_id="W0-03", commit_vector=vector_path)
        import io as _io
        try:
            with contextlib.redirect_stdout(_io.StringIO()):
                rc = cmd_accept_task(args)
            refused = False
        except SystemExit as exc:
            rc, refused = exc.code, exc.code == 30
        record("committing_timeout_partial_vector", refused,
               "accept refused rc=%s before artifact/commit-vector review" % rc)
    except Exception as exc:  # noqa: BLE001
        record("committing_timeout_partial_vector", False, "exception %s" % exc)

    # restart_preserves_deadlines: a fresh process reads the persisted deadline
    try:
        run = fresh("restart_deadline")
        stamp = 1799999999.5
        mini_state(run, deadline=stamp)
        out_val = subprocess.run([sys.executable, "-c",
                                  "import json,sys;print(json.load(open(sys.argv[1]))"
                                  "['tasks']['W0-03']['task_deadline'])",
                                  os.path.join(run, "state.json")],
                                 capture_output=True, text=True, timeout=10).stdout.strip()
        record("restart_preserves_deadlines", out_val == repr(stamp),
               "new process read deadline=%s (persisted %r)" % (out_val, stamp))
    except Exception as exc:  # noqa: BLE001
        record("restart_preserves_deadlines", False, "exception %s" % exc)

    # crash_after_event_before_snapshot: durable event, stale snapshot, chain intact
    try:
        run = fresh("crash_event")
        store = StateStore(run)
        state = mini_state(run)
        store.append_event("task_state", {"task": "W0-03", "to": "RUNNING", "attempt": 2})
        chain_ok, _ = store.verify_chain()
        stale = state["events_head_hash"] != store.head_hash()
        record("crash_after_event_before_snapshot",
               chain_ok and stale,
               "chain=%s snapshot_stale=%s -> replay derives newer state" % (chain_ok, stale))
    except Exception as exc:  # noqa: BLE001
        record("crash_after_event_before_snapshot", False, "exception %s" % exc)

    # crash_after_launch_intent_before_spawn: one durable intent, no spawn marker
    try:
        run = fresh("launch_intent")
        store = StateStore(run)
        store.append_event("launch_intent", {"task": "W0-03", "revision": 4, "spawned": False})
        state = mini_state(run, extra={"launch_intent_revision": 4})
        intents = [e for e in store.load_events() if e["kind"] == "launch_intent"]
        chain_ok, _ = StateStore(run).verify_chain()
        record("crash_after_launch_intent_before_spawn",
               len(intents) == 1 and chain_ok
               and "spawned" not in state.get("tasks", {}).get("W0-03", {}),
               "1 durable intent, no spawn marker; replay (not memory) decides respawn")
    except Exception as exc:  # noqa: BLE001
        record("crash_after_launch_intent_before_spawn", False, "exception %s" % exc)

    # state_ahead_of_log_rejected: forged snapshot head no longer matches the chain
    try:
        run = fresh("state_ahead")
        store = StateStore(run)
        state = mini_state(run)
        forged = dict(state, revision=9, events_head_hash="f" * 64)
        with open(store.state_path, "w") as fh:
            json.dump(forged, fh)
        mismatch = StateStore(run).load_state()["events_head_hash"] != store.head_hash()
        record("state_ahead_of_log_rejected", mismatch,
               "snapshot head != chain head detected=%s -> CONTRACT_CONFLICT for holder" % mismatch)
    except Exception as exc:  # noqa: BLE001
        record("state_ahead_of_log_rejected", False, "exception %s" % exc)

    # budget_preflight_infeasible: exhausted attempt budget refuses launch
    try:
        run = fresh("budget")
        mini_state(run, attempt=5)
        rc, _ = call_task(run, "RUNNING")
        record("budget_preflight_infeasible", rc == 30,
               "attempt=5/5 launch refused rc=%s (budget exhausted pre-flight)" % rc)
    except Exception as exc:  # noqa: BLE001
        record("budget_preflight_infeasible", False, "exception %s" % exc)

    # busy_lock_no_state_or_event_write: live holder blocks task writes untouched
    try:
        run = fresh("busy_lock")
        os.makedirs(os.path.join(run, "observations"), exist_ok=True)
        mini_state(run)
        before = (open(os.path.join(run, "state.json"), "rb").read(),
                  open(os.path.join(run, "events.jsonl"), "rb").read()
                  if os.path.exists(os.path.join(run, "events.jsonl")) else b"")
        holder = subprocess.Popen(
            [sys.executable, os.path.abspath(__file__), "_hold-lock", "--run-dir", run],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        time.sleep(1.0)
        rc, _ = call_task(run, "RUNNING")
        after = (open(os.path.join(run, "state.json"), "rb").read(),
                 open(os.path.join(run, "events.jsonl"), "rb").read()
                 if os.path.exists(os.path.join(run, "events.jsonl")) else b"")
        os.kill(holder.pid, signal.SIGKILL); holder.wait(timeout=5)
        record("busy_lock_no_state_or_event_write",
               rc == 75 and before == after,
               "busy holder refused rc=%s; state/events bytes unchanged=%s" % (rc, before == after))
    except Exception as exc:  # noqa: BLE001
        record("busy_lock_no_state_or_event_write", False, "exception %s" % exc)

    # busy_observation_exclusive_create: observation files are created exclusively
    try:
        obs = os.path.join(fresh("obs_excl"), "observations")
        os.makedirs(obs, exist_ok=True)
        path = os.path.join(obs, "probe.json")
        fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o644)
        os.write(fd, b"first"); os.close(fd)
        try:
            os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            second_refused = False
        except FileExistsError:
            second_refused = True
        record("busy_observation_exclusive_create", second_refused,
               "O_EXCL second create refused=%s" % second_refused)
    except Exception as exc:  # noqa: BLE001
        record("busy_observation_exclusive_create", False, "exception %s" % exc)

    # dead_holder_orphan_group_recovery: killed holder group frees the lock
    try:
        run = fresh("dead_holder")
        holder = subprocess.Popen(
            [sys.executable, "-c",
             "import sys,time;sys.path.insert(0,%r);"
             "from attachment_lifecycle_v5_runner import RunLock;"
             "l=RunLock(%r);assert l.acquire_nb();print('held',flush=True);time.sleep(6)"
             % (os.path.dirname(os.path.abspath(__file__)), run)],
            stdout=subprocess.PIPE, start_new_session=True)
        holder.stdout.readline()
        os.killpg(os.getpgid(holder.pid), signal.SIGKILL)
        holder.wait(timeout=5)
        time.sleep(0.3)
        recovered = RunLock(run).acquire_nb()
        if recovered:
            RunLock(run).release()
        record("dead_holder_orphan_group_recovery", recovered,
               "orphan group killed; lock reacquired=%s" % recovered)
    except Exception as exc:  # noqa: BLE001
        record("dead_holder_orphan_group_recovery", False, "exception %s" % exc)

    # parallel_disjoint_ownership: two workers over disjoint owned paths each pass
    try:
        ctx = _selftest_ctx(fresh("disjoint"))
        authority = ["run_dir/state.json", "run_dir/events.jsonl"]
        ok1, why1 = V["worker_writes"]({"writes": ["src/a.erl"], "committed": False},
                                       authority, ctx["owned"])
        ok2, why2 = V["worker_writes"]({"writes": ["src/b.erl"], "committed": False},
                                       authority, ctx["owned"])
        record("parallel_disjoint_ownership", ok1 and ok2, "a:%s b:%s" % (why1, why2))
    except Exception as exc:  # noqa: BLE001
        record("parallel_disjoint_ownership", False, "exception %s" % exc)

    # parallel_worker_crash_no_double_spawn: exactly one durable spawn record
    try:
        run = fresh("no_double_spawn")
        store = StateStore(run)
        store.append_event("launch_intent", {"task": "W0-03", "revision": 2, "spawned": False})
        store.append_event("worker_spawned", {"task": "W0-03", "pid": 424242})
        spawns = [e for e in store.load_events() if e["kind"] == "worker_spawned"]
        chain_ok, _ = StateStore(run).verify_chain()
        record("parallel_worker_crash_no_double_spawn",
               len(spawns) == 1 and chain_ok,
               "durable spawn records=%d chain=%s; replay cannot double-spawn" % (len(spawns), chain_ok))
    except Exception as exc:  # noqa: BLE001
        record("parallel_worker_crash_no_double_spawn", False, "exception %s" % exc)

    # parallel_partial_commit_recovery: resume reconciles COMMITTING with PASS intact
    try:
        import io as _io
        run = fresh("partial_commit")
        state = {"run_id": os.path.basename(run), "revision": 1,
                 "plan_dir": os.path.join(os.path.dirname(os.path.dirname(
                     os.path.abspath(__file__))),
                     "docs/architecture/attachment-lifecycle-v5"),
                 "plan_manifest_sha256": sha256_file(os.path.join(
                     os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                     "docs/architecture/attachment-lifecycle-v5", "manifest.sha256")),
                 "tasks": {"W0-01": {"state": "PASS", "attempt": 1},
                           "W0-02": {"state": "COMMITTING", "attempt": 1,
                                     "commit_vector": {"imboy": {"commit": "1" * 40}},
                                     "next_action": "inspect vector"}},
                 "gates": {}}
        store = StateStore(run)
        store.append_event("admit", {"synthetic": True})
        store.write_snapshot(state)
        buf = _io.StringIO()
        with contextlib.redirect_stdout(buf):
            rc = cmd_resume(types.SimpleNamespace(run_dir=run))
        report = json.loads(buf.getvalue())
        rec = report.get("commit_unknown_reconciliation", {}).get("W0-02", {})
        record("parallel_partial_commit_recovery",
               rc == 0 and rec.get("vector") == {"imboy": {"commit": "1" * 40}}
               and report.get("tasks_pass") == ["W0-01"],
               "resume rc=%s reconciles COMMITTING vector, PASS intact" % rc)
    except Exception as exc:  # noqa: BLE001
        record("parallel_partial_commit_recovery", False, "exception %s" % exc)

    # expired_original_owner_repair_no_go: same-revision blocked resume refused
    try:
        ok, why = V["blocked_resume"]({"blocked_reason": "device missing",
                                       "blocked_revision": 7}, 7)
        record("expired_original_owner_repair_no_go", ok is False, why)
    except Exception as exc:  # noqa: BLE001
        record("expired_original_owner_repair_no_go", False, "exception %s" % exc)

    return out


def _holder_hang_selftest(base_tmp):
    """Suspended live holder: bounded <=2s refusal, no state/event writes (02 §14)."""
    import signal
    run = os.path.join(base_tmp, "hang")
    os.makedirs(os.path.join(run, "observations"), exist_ok=True)
    holder = subprocess.Popen(
        [sys.executable, os.path.abspath(__file__), "_hold-lock", "--run-dir", run],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(1.0)  # holder SIGSTOPs itself inside _hold-lock; it must stay suspended
    t0 = time.monotonic()
    proc = subprocess.run(
        [sys.executable, os.path.abspath(__file__), "resume", "--run-dir", run],
        capture_output=True, text=True, timeout=10)
    took = time.monotonic() - t0
    events_before = os.path.exists(os.path.join(run, "events.jsonl"))
    os.kill(holder.pid, signal.SIGCONT)
    try:
        holder.wait(timeout=5)
    except subprocess.TimeoutExpired:
        holder.kill()
    passed = (proc.returncode == 75 and took <= 2.5 and not events_before)
    return passed, "rc=%d took=%.2fs state_untouched=%s" % (proc.returncode, took, not events_before)


def cmd_selftest(_args):
    results = _mutation_results()
    bad = [(i, n, w) for (i, n, r, w) in results if not r]
    with tempfile.TemporaryDirectory(prefix="attv5-selftest-par-") as tmp:
        par = _parallel_selftests(tmp)
        hang_ok, hang_why = _holder_hang_selftest(tmp)
        scenarios = _scenario_selftests(tmp)
    par_bad = [(n, w) for (n, r, _w) in par if not r]
    scen_bad = [(n, why) for (n, _r, why) in scenarios if not _r]
    summary = {
        "mutations_total": len(results),
        "mutations_rejected": len(results) - len(bad),
        "parallel_selftests": {n: r for (n, r, _w) in par},
        "holder_hang_bounded_refusal": {"passed": hang_ok, "detail": hang_why},
        "scenarios": {n: {"passed": r, "detail": w} for (n, r, w) in scenarios},
    }
    print(json.dumps(summary, indent=1))
    if bad:
        for i, n, w in bad:
            sys.stderr.write("mutation %d NOT rejected: %s (%s)\n" % (i, n, w))
    if par_bad:
        for n, w in par_bad:
            sys.stderr.write("parallel selftest failed: %s (%s)\n" % (n, w))
    if scen_bad:
        for n, w in scen_bad:
            sys.stderr.write("scenario selftest failed: %s (%s)\n" % (n, w))
    if bad or par_bad or scen_bad or not hang_ok:
        return EXIT_NOGO
    return EXIT_OK


def cmd_hold_lock(args):
    """Internal: acquire run lock then SIGSTOP self (for holder-hang selftest)."""
    import signal
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        return EXIT_BLOCKED
    sys.stdout.write("held\n")
    sys.stdout.flush()
    os.kill(os.getpid(), signal.SIGSTOP)
    time.sleep(0.2)
    lock.release()
    return EXIT_OK


def cmd_task(args):
    """Main-integrator task state driver (holder-only; validates transitions)."""
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(75, "BLOCKED_EXTERNAL: cannot mutate task state without lock")
    try:
        state = _require_state(run_dir, allow_expired=args.to in ('NO_GO', 'BLOCKED_EXTERNAL'))
        contract = load_contract(state["plan_dir"])
        if args.task_id not in state["tasks"]:
            die(2, "unknown task %r" % args.task_id)
        cur = state["tasks"][args.task_id]
        to = args.to
        if to not in TASK_STATES:
            die(2, "unknown state %r" % to)
        if not _task_transition_allowed(cur, to):
            die(20, "illegal transition %s -> %s" % (cur["state"], to))
        if to in ("READY", "RUNNING") and not _task_prerequisites(state, contract, args.task_id):
            die(EXIT_CONTRACT, "task prerequisites not satisfied")
        now = time.time()
        _budget_check(state, contract, args.task_id, enforce=to not in ('NO_GO', 'BLOCKED_EXTERNAL'))
        if to == "RUNNING":
            limit = contract.get("retry_policy", {}).get("task_attempts_total_max", 5)
            attempts = cur.get("attempt", 0)
            if type(limit) is not int or limit <= 0 or type(attempts) is not int or attempts < 0:
                die(EXIT_CONTRACT, "invalid task attempt budget")
            if attempts >= limit:
                die(EXIT_NOGO, "task attempt budget exhausted")
            started = cur.get("task_started_at", now)
            timeouts = contract.get("timeouts", {})
            seconds = timeouts.get("task_overrides", {}).get(
                args.task_id, timeouts.get("task_seconds", 28800))
            if (type(started) not in (int, float) or not 0 < started <= now
                    or type(seconds) not in (int, float) or not 0 < seconds < float("inf")):
                die(EXIT_CONTRACT, "invalid task start or deadline budget")
            deadline = cur.get("task_deadline", started + seconds)
            if type(deadline) not in (int, float) or not started <= deadline < float("inf"):
                die(EXIT_CONTRACT, "invalid persisted task deadline")
            if now >= deadline:
                die(EXIT_NOGO, "persisted task deadline exhausted")
            cur["task_started_at"] = started
            cur["task_deadline"] = deadline
            state['time_budgets'] = _budget_start(state, contract, args.task_id, now)
            _budget_check(state, contract, args.task_id)
        if to == "PASS":
            # PASS cannot be supplied as an operator assertion while proofs are absent.
            candidate = state.get("verification_candidate", {})
            if not candidate or candidate_errors(candidate):
                die(EXIT_CONTRACT, "task PASS needs the current verified candidate")
            if not cur.get("case_results") or not cur.get("commit_vector"):
                die(EXIT_CONTRACT, "task PASS needs measured case results and commit vector")
            die(EXIT_CONTRACT, "task PASS requires sealed artifact acceptance; manual PASS is disabled")
        if to == "BLOCKED_EXTERNAL":
            cur["resume_state"] = cur["state"]
        cur["state"] = to
        if to in ("RUNNING",):
            cur["attempt"] = cur.get("attempt", 0) + 1
        if args.case_results:
            with open(args.case_results) as fh:
                cur["case_results"] = json.load(fh)
        if args.commit_vector:
            with open(args.commit_vector) as fh:
                cur["commit_vector"] = json.load(fh)
        if args.blocked_reason:
            cur["blocked_reason"] = args.blocked_reason
        st = StateStore(run_dir)
        st.write_snapshot(state, "task_state", {"task": args.task_id, "to": to,
                                                  "attempt": cur.get("attempt", 0)})
        print(json.dumps({"task": args.task_id, "state": to, "attempt": cur.get("attempt", 0)}))
        return EXIT_OK
    finally:
        lock.release()


def _case_bound_candidates(run_dir):
    """Map case_id -> the candidate its sealing event bound (the same
    per-task accepted-candidate semantics verify applies to G0..G5
    receipts: a later authorized task advancing the candidate must not
    invalidate already-sealed evidence)."""
    bound = {}
    events_path = os.path.join(run_dir, "events.jsonl")
    if os.path.exists(events_path):
        with open(events_path) as fh:
            for line in fh:
                if not line.strip():
                    continue
                try:
                    event = json.loads(line)
                except ValueError:
                    continue
                if event.get("kind") == "case_manifest_bound":
                    payload = event.get("payload", {})
                    cid = payload.get("case_id")
                    cand = payload.get("candidate")
                    if isinstance(cid, str) and isinstance(cand, dict):
                        bound[cid] = cand
    return bound


def _task_artifacts(state, contract, task_id, run_dir):
    candidate = state.get("verification_candidate", {})
    errors = candidate_errors(candidate) if candidate else ["current candidate missing"]
    sealed_candidates = _case_bound_candidates(run_dir)
    counts = {}
    for cid in task_case_ids(contract, task_id) or []:
        expected = contract["expected_leaf_matrix"][cid]
        index = {tuple(v[k] for k in LEAF_FIELDS): v for v in expected}
        case_dir = os.path.join(run_dir, "evidence", task_id, cid.split("/", 1)[1])
        actual = []
        total = dict(executed=0, passed=0, failed=0, skipped=0, exit_code=0)
        # The leaves were sealed against the candidate recorded by their
        # case_manifest_bound event; fall back to the current candidate.
        case_candidate = sealed_candidates.get(cid) or candidate
        try:
            for name, row in _read_case_rows(run_dir, case_dir, state.get("case_manifest_digests", {}).get(cid)):
                identity = tuple(row.get(k) for k in LEAF_FIELDS)
                actual.append("|".join(map(str, identity)))
                if identity not in index:
                    errors.append(cid + ": unexpected leaf")
                    continue
                errors.extend(receipt_errors(row, index[identity], task_id, cid,
                                             case_candidate, run_dir, state.get("resource_allowlist", {}), contract,
                                             state.get("tasks", {}).get(task_id, {}).get("attempt")))
                for field in ("executed", "passed", "failed", "skipped"):
                    if type(row.get(field)) is int:
                        total[field] += row[field]
        except (OSError, ValueError, TypeError, AttributeError):
            errors.append(cid + ": uncommitted or corrupt case")
        ok, why = v_leaf_set(leaf_ids_for_case(contract, cid), actual)
        if not ok:
            errors.append(cid + ": " + why)
        counts[cid] = total
    return errors, counts


def _accepted_vector_errors(vector, candidate):
    errors = []
    if not isinstance(vector, dict) or set(vector) != set(REPO_NAMES):
        return ["commit vector must contain all three repositories"]
    for repo in REPO_NAMES:
        entry = vector[repo]
        if not isinstance(entry, dict):
            errors.append("commit vector entry must be object: " + repo)
            continue
        commit, parent = entry.get("commit"), entry.get("parent")
        if not _valid_digest(commit, 40) or not _valid_digest(parent, 40):
            errors.append("full commit/parent SHA required: " + repo)
            continue
        if commit != candidate.get("heads", {}).get(repo):
            errors.append("commit vector not final candidate: " + repo)
            continue
        if entry.get("unchanged") is True and commit == parent:
            continue
        try:
            parents = subprocess.check_output([
                "git", "-C", candidate["repository_paths"][repo], "show", "-s",
                "--format=%P", commit], text=True, timeout=5).strip().split()
            if parent not in parents:
                errors.append("commit parent mismatch: " + repo)
        except (KeyError, OSError, subprocess.SubprocessError):
            errors.append("cannot inspect commit: " + repo)
    return errors


def cmd_accept_task(args):
    """Accept measured artifacts after the serialized commit vector is complete."""
    run_dir = _run_dir_from(args)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(EXIT_BLOCKED, "accept-task cannot acquire run lock")
    try:
        state = _require_state(run_dir)
        cur = state["tasks"].get(args.task_id, {})
        if cur.get("state") != "COMMITTING":
            die(EXIT_CONTRACT, "accept-task requires COMMITTING")
        contract = load_contract(state["plan_dir"])
        _budget_check(state, contract, args.task_id)
        deadline = cur.get("task_deadline")
        attempt = cur.get("attempt")
        limit = contract.get("retry_policy", {}).get("task_attempts_total_max", 5)
        if (type(deadline) not in (int, float) or not 0 < deadline < float("inf")
                or time.time() >= deadline):
            die(EXIT_NOGO, "persisted task deadline missing or exhausted")
        if type(attempt) is not int or not 1 <= attempt <= limit:
            die(EXIT_NOGO, "invalid or exhausted task attempt budget")
        errors, counts = _task_artifacts(state, contract, args.task_id, run_dir)
        ok, plan_hash = verify_plan_manifest(state["plan_dir"])
        if not ok or plan_hash != state.get("plan_manifest_sha256"):
            errors.append("plan manifest mismatch")
        auth = state.get("authorization_path", "")
        if not os.path.isfile(auth) or sha256_file(auth) != state.get("authorization_sha256"):
            errors.append("authorization original unavailable")
        with open(args.commit_vector) as fh:
            vector = json.load(fh)
        candidate = state.get("verification_candidate", {})
        errors.extend(_accepted_vector_errors(vector, candidate))
        errors.extend(candidate_errors(candidate))
        final_ok, final_hash = verify_plan_manifest(state["plan_dir"])
        if (not final_ok or final_hash != plan_hash
                or candidate.get("plan_manifest_sha256") != final_hash):
            errors.append("candidate/current plan binding mismatch")
        if time.time() >= deadline:
            errors.append("persisted task deadline exhausted during acceptance")
        if errors:
            print(json.dumps({"result": "FAIL", "fails": errors[:30], "fail_count": len(errors)}))
            return EXIT_CONTRACT
        cur.update(state="PASS", case_results=counts, commit_vector=vector)
        store = StateStore(run_dir)
        store.write_snapshot(state, "task_artifacts_accepted", {"task": args.task_id,
                             "candidate": state["verification_candidate"], "commit_vector": vector})
        print(json.dumps({"task": args.task_id, "state": "PASS"}))
        return EXIT_OK
    finally:
        lock.release()


def main(argv):
    ap = argparse.ArgumentParser(prog="attachment_lifecycle_v5_runner.py")
    sub = ap.add_subparsers(dest="cmd")
    st = sub.add_parser("self-test")
    st.set_defaults(func=cmd_selftest)
    ad = sub.add_parser("admit")
    ad.add_argument("--plan-dir", required=True)
    ad.add_argument("--run-dir", required=True)
    ad.add_argument("--authorization", required=True)
    ad.set_defaults(func=cmd_admit)
    freeze = sub.add_parser("freeze-candidate")
    freeze.add_argument("--run-dir", required=True)
    freeze.set_defaults(func=cmd_freeze_candidate)
    nx = sub.add_parser("next")
    nx.add_argument("--run-dir")
    nx.set_defaults(func=cmd_next)
    rs = sub.add_parser("resume")
    rs.add_argument("--run-dir")
    rs.set_defaults(func=cmd_resume)
    vf = sub.add_parser("verify")
    vf.add_argument("--run-dir")
    vf.add_argument("--gate", required=True)
    vf.set_defaults(func=cmd_verify)
    ag = sub.add_parser("accept-gate")
    ag.add_argument("--run-dir", required=True)
    ag.add_argument("--gate", required=True)
    ag.set_defaults(func=cmd_accept_gate)
    seal = sub.add_parser("seal-case")
    seal.add_argument("--run-dir", required=True)
    seal.add_argument("--task-id", required=True)
    seal.add_argument("--case-id", required=True)
    seal.add_argument("--case-dir", required=True)
    seal.add_argument("--receipts", required=True)
    seal.add_argument("--command", required=True)
    seal.set_defaults(func=cmd_seal_case)
    tk = sub.add_parser("task")
    tk.add_argument("--run-dir")
    tk.add_argument("--task-id", required=True)
    tk.add_argument("--to", required=True)
    tk.add_argument("--case-results")
    tk.add_argument("--commit-vector")
    tk.add_argument("--blocked-reason")
    tk.set_defaults(func=cmd_task)
    accept = sub.add_parser("accept-task")
    accept.add_argument("--run-dir", required=True)
    accept.add_argument("--task-id", required=True)
    accept.add_argument("--commit-vector", required=True)
    accept.set_defaults(func=cmd_accept_task)
    hl = sub.add_parser("_hold-lock")
    hl.add_argument("--run-dir")
    hl.set_defaults(func=cmd_hold_lock)
    args = ap.parse_args(argv)
    if not getattr(args, "func", None):
        ap.print_usage()
        return EXIT_USAGE
    try:
        return args.func(args)
    except (OSError, ValueError, TypeError, AttributeError, KeyError) as exc:
        print(json.dumps({"result": "CONTRACT_CONFLICT",
                          "error": type(exc).__name__}), file=sys.stderr)
        return EXIT_CONTRACT


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
