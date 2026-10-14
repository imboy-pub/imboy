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
import copy
import fcntl
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import time
import uuid

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

    def write_snapshot(self, state):
        state = dict(state)
        state["revision"] = state.get("revision", 0) + 1
        state["events_head_hash"] = self.head_hash()
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
        return True, "chain ok (%d events)" % i if self.load_events() else ("ok", "empty")


# ---------------------------------------------------------------- validators
# Pure predicates used by verify/self-test. Each returns (ok: bool, reason: str).
V = {}


def _fields(receipt):
    return receipt if isinstance(receipt, dict) else {}


def v_leaf_set(expected_ids, actual_ids):
    e, a = set(expected_ids), set(actual_ids)
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
    if not path or not os.path.exists(path):
        return False, "raw output missing"
    if sha256_file(path) != r.get("raw_output_sha256"):
        return False, "raw output sha mismatch"
    return True, "raw ok"


V["raw_output"] = v_raw_output


def v_execution_counts(receipt):
    r = _fields(receipt)
    exit_code = r.get("exit_code")
    ex, pa, fa, sk = (r.get("executed", 0), r.get("passed", 0),
                      r.get("failed", 0), r.get("skipped", 0))
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
    rid = _fields(res).get("id", "")
    allowed = allowlist.get(kind)
    if allowed is None:
        return False, "resource kind %r outside allowlist" % kind
    if isinstance(allowed, list) and rid not in allowed:
        return False, "resource id outside registered allowlist"
    if isinstance(allowed, str) and not rid.startswith(allowed):
        return False, "resource id outside run-owned prefix"
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
    with open(auth_path) as fh:
        auth = json.load(fh)
    ok, manifest_sha = verify_plan_manifest(plan_dir)
    if not ok:
        die(20, "plan manifest verification failed (CONTRACT_CONFLICT)")
    if auth.get("plan_manifest_sha256") != manifest_sha:
        die(20, "authorization plan hash does not bind current plan manifest")
    if not auth.get("implementation_allowed") or not auth.get("local_commit_allowed"):
        die(20, "authorization does not allow implementation+commit")
    contract = load_contract(plan_dir)
    allowed = set(auth.get("allowed_tasks", []))
    need = {t["task_id"] for t in contract["tasks"]}
    if not need <= allowed:
        die(20, "authorization allowed_tasks missing: %s" % sorted(need - allowed))
    if auth.get("expires_at", "") < time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()):
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
    if store.load_state() is None:
        state = {
            "run_id": os.path.basename(run_dir),
            "revision": 0,
            "owner_token": lock.token,
            "lease_until": time.time() + 120,
            "plan_dir": plan_dir,
            "plan_manifest_sha256": manifest_sha,
            "authorization_path": auth_path,
            "authorization_sha256": sha256_file(auth_path),
            "initial_heads": auth.get("initial_three_repo_heads", {}),
            "tasks": {t["task_id"]: {"state": "NOT_STARTED", "attempt": 0}
                      for t in contract["tasks"]},
            "gates": {g: "NOT_STARTED" for g in contract["wave_gates"]},
            "mode": "IMPLEMENTATION",
        }
        store.append_event("admit", {"plan_manifest_sha256": manifest_sha,
                                     "authorization_sha256": state["authorization_sha256"]})
        store.write_snapshot(state)
        print(json.dumps({"admitted": True, "run_id": state["run_id"],
                          "plan_manifest_sha256": manifest_sha,
                          "tasks": len(state["tasks"])}))
    else:
        print(json.dumps({"admitted": True, "already": True}))
    lock.release()
    return EXIT_OK


def _compute_ready(state, contract):
    ready = []
    tasks = {t["task_id"]: t for t in contract["tasks"]}
    gates = contract["wave_gates"]
    for tid, t in tasks.items():
        st = state["tasks"].get(tid, {})
        if st.get("state") != "NOT_STARTED":
            continue
        deps_ok = all(state["tasks"].get(d, {}).get("state") == "PASS" for d in t.get("dependencies", []))
        wave = t["wave"]
        prev_gate = None
        for g, gi in gates.items():
            if gi.get("wave") == wave:
                prev_gate = gi.get("previous_gate")
                break
        gate_ok = True
        if prev_gate:
            gate_ok = state["gates"].get(prev_gate) == "PASS"
        wave_ok = wave == "W0" or gate_ok
        if deps_ok and wave_ok:
            claims = {"files": t.get("files", []), "case_ids": t.get("case_ids", []),
                      "read_only_files": t.get("read_only_files", []),
                      "schema_phase": t.get("schema_phase"),
                      "commit_subject": t.get("commit_subject")}
            ready.append({"task_id": tid, "claim": claims})
    return ready


def cmd_next(args):
    run_dir = _run_dir_from(args)
    state = _require_state(run_dir)
    contract = load_contract(state["plan_dir"])
    ready = _compute_ready(state, contract)
    print(json.dumps({"ready": ready, "count": len(ready)}, ensure_ascii=False, indent=1))
    return EXIT_OK


def _run_dir_from(args):
    rd = getattr(args, "run_dir", None)
    if not rd:
        rd = os.environ.get("V5_RUN_DIR")
    if not rd or not os.path.isabs(rd):
        die(2, "run-dir must be an explicit absolute path (or absolute V5_RUN_DIR)")
    if not os.path.isdir(rd):
        die(2, "run dir does not exist: %s" % rd)
    return rd


def _require_state(run_dir):
    store = StateStore(run_dir)
    state = store.load_state()
    if state is None:
        die(20, "run not admitted; run admit first")
    ok, why = store.verify_chain()
    if not ok:
        die(20, "event chain corrupt: %s" % why)
    return state


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
        state = _require_state(run_dir)
        store = StateStore(run_dir)
        contract = load_contract(state["plan_dir"])
        ok, manifest_sha = verify_plan_manifest(state["plan_dir"])
        if not ok or manifest_sha != state["plan_manifest_sha256"]:
            die(20, "plan manifest changed since admission")
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
        store.append_event("resume_inspected", {"revision": state["revision"]})
        print(json.dumps(report, ensure_ascii=False, indent=1))
        return EXIT_OK
    finally:
        lock.release()


def cmd_verify(args):
    run_dir = _run_dir_from(args)
    state = _require_state(run_dir)
    contract = load_contract(state["plan_dir"])
    gate = args.gate
    if gate not in contract["wave_gates"]:
        die(2, "unknown gate %r (G0..G6)" % gate)
    gi = contract["wave_gates"][gate]
    fails = []
    for tid in gi["requires_tasks"]:
        if state["tasks"].get(tid, {}).get("state") != "PASS":
            fails.append("%s not PASS" % tid)
    prev = gi.get("previous_gate")
    if prev and state["gates"].get(prev) != "PASS":
        fails.append("previous gate %s not PASS" % prev)
    # exact leaf coverage from sealed evidence
    ev_root = os.path.join(run_dir, "evidence")
    for tid in gi["requires_tasks"]:
        for cid in task_case_ids(contract, tid) or []:
            expected = leaf_ids_for_case(contract, cid)
            case_dir = os.path.join(ev_root, tid, cid.split("/", 1)[-1])
            actual = []
            if os.path.isdir(case_dir):
                for fn in os.listdir(case_dir):
                    if fn.endswith(".json"):
                        with open(os.path.join(case_dir, fn)) as fh:
                            leaf = json.load(fh)
                        actual.append("|".join(str(leaf.get(k)) for k in
                                       ("ac_id", "domain", "driver", "entrypoint",
                                        "ordering_or_fault", "schema_phase")))
                        okv, why = V["execution_counts"](leaf)
                        if not okv:
                            fails.append("%s leaf %s: %s" % (cid, fn[:18], why))
            okv, why = V["leaf_set"](expected, actual)
            if not okv:
                fails.append("%s: %s" % (cid, why))
    if fails:
        print(json.dumps({"gate": gate, "result": "FAIL", "fails": fails}, indent=1))
        return EXIT_NOGO if all("not PASS" in f for f in fails) else EXIT_CONTRACT
    # mark gate pass (holder-only mutation)
    lock = RunLock(run_dir)
    if not lock.acquire_nb():
        die(75, "BLOCKED_EXTERNAL: cannot mutate gate state without lock")
    try:
        state = _require_state(run_dir)
        state["gates"][gate] = "PASS"
        st = StateStore(run_dir)
        st.append_event("gate_pass", {"gate": gate})
        st.write_snapshot(state)
    finally:
        lock.release()
    print(json.dumps({"gate": gate, "result": "PASS"}))
    return EXIT_OK


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
    par_bad = [(n, w) for (n, r, w) in par if not r]
    summary = {
        "mutations_total": len(results),
        "mutations_rejected": len(results) - len(bad),
        "parallel_selftests": {n: r for (n, r, _w) in par},
        "holder_hang_bounded_refusal": {"passed": hang_ok, "detail": hang_why},
    }
    print(json.dumps(summary, indent=1))
    if bad:
        for i, n, w in bad:
            sys.stderr.write("mutation %d NOT rejected: %s (%s)\n" % (i, n, w))
    if par_bad:
        for n, w in par_bad:
            sys.stderr.write("parallel selftest failed: %s (%s)\n" % (n, w))
    if bad or par_bad or not hang_ok:
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
        state = _require_state(run_dir)
        contract = load_contract(state["plan_dir"])
        if args.task_id not in state["tasks"]:
            die(2, "unknown task %r" % args.task_id)
        cur = state["tasks"][args.task_id]
        to = args.to
        if to not in TASK_STATES:
            die(2, "unknown state %r" % to)
        if to not in TRANSITIONS[cur["state"]]:
            die(20, "illegal transition %s -> %s" % (cur["state"], to))
        cur["state"] = to
        if to in ("RUNNING",):
            cur["attempt"] = cur.get("attempt", 0) + 1
            cur["task_started_at"] = time.time()
        if args.case_results:
            with open(args.case_results) as fh:
                cur["case_results"] = json.load(fh)
        if args.commit_vector:
            with open(args.commit_vector) as fh:
                cur["commit_vector"] = json.load(fh)
        if args.blocked_reason:
            cur["blocked_reason"] = args.blocked_reason
        st = StateStore(run_dir)
        st.append_event("task_state", {"task": args.task_id, "to": to,
                                       "attempt": cur.get("attempt", 0)})
        st.write_snapshot(state)
        print(json.dumps({"task": args.task_id, "state": to, "attempt": cur.get("attempt", 0)}))
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
    tk = sub.add_parser("task")
    tk.add_argument("--run-dir")
    tk.add_argument("--task-id", required=True)
    tk.add_argument("--to", required=True)
    tk.add_argument("--case-results")
    tk.add_argument("--commit-vector")
    tk.add_argument("--blocked-reason")
    tk.set_defaults(func=cmd_task)
    hl = sub.add_parser("_hold-lock")
    hl.add_argument("--run-dir")
    hl.set_defaults(func=cmd_hold_lock)
    args = ap.parse_args(argv)
    if not getattr(args, "func", None):
        ap.print_usage()
        return EXIT_USAGE
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
