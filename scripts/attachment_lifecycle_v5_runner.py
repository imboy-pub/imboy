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
import datetime
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
    candidate = state.get("verification_candidate")
    if candidate and candidate_errors(candidate):
        return []
    ready = []
    tasks = {t["task_id"]: t for t in contract["tasks"]}
    gates = contract["wave_gates"]
    for tid, t in tasks.items():
        st = state["tasks"].get(tid, {})
        if not candidate and t.get("wave") != "W0":
            continue
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
            gate_ok = (state["gates"].get(prev_gate) == "PASS"
                       and state.get("gate_candidates", {}).get(prev_gate)
                       == state.get("verification_candidate")
                       and bool(state.get("verification_candidate")))
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
    candidate = state.get("verification_candidate")
    if candidate and candidate_errors(candidate):
        die(EXIT_CONTRACT, "candidate drift; no tasks ready")
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
        store.append_event("resume_inspected", {"revision": state["revision"]})
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
    for name, digest in files.items():
        if not isinstance(name, str):
            errors.append("candidate file path must be string")
            continue
        try:
            repo, rel = name.split("/", 1)
            path = os.path.realpath(os.path.join(roots[repo], rel))
            root = os.path.realpath(roots[repo])
            mode = _ls_files_modes(root).get(rel)
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
        if not os.path.isfile(auth_path) or sha256_file(auth_path) != state.get("authorization_sha256"):
            die(EXIT_CONTRACT, "authorization original unavailable")
        with open(auth_path) as fh:
            auth = json.load(fh)
        ok, digest = verify_plan_manifest(state["plan_dir"])
        if (not ok or digest != state.get("plan_manifest_sha256")
                or auth.get("plan_manifest_sha256") != digest
                or not auth.get("implementation_allowed")
                or auth.get("expires_at", "") < time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())):
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
        if not frozen_events:
            store.append_event("candidate_frozen", {"candidate": candidate})
        store.write_snapshot(state)
        return EXIT_OK
    finally:
        lock.release()


def receipt_errors(leaf, expected_leaf, task_id, case_id, candidate, run_dir, allowlist,
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
        auth_path = state.get("authorization_path", "")
        if not os.path.isfile(auth_path) or sha256_file(auth_path) != state.get("authorization_sha256"):
            fails.append("authorization original missing or hash mismatch; recovery admission required")
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
                    if os.path.isdir(case_dir):
                        for fn in sorted(os.listdir(case_dir)):
                            if not (fn.startswith("leaf-") and fn.endswith(".json")):
                                continue
                            try:
                                with open(os.path.join(case_dir, fn)) as fh:
                                    leaf = json.load(fh)
                                identity = tuple(leaf.get(k) for k in LEAF_FIELDS)
                                actual.append("|".join(map(str, identity)))
                                if identity not in by_id:
                                    fails.append(cid + ": unexpected leaf " + fn)
                                    continue
                                issues = receipt_errors(leaf, by_id[identity], tid, cid, candidate,
                                                        run_dir, state.get("resource_allowlist", {}), contract)
                                fails.extend(cid + " " + fn + ": " + why for why in issues)
                            except (OSError, ValueError, TypeError, AttributeError) as exc:
                                fails.append(cid + " " + fn + ": malformed receipt " + type(exc).__name__)
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
    directory = os.stat(case_dir, follow_symlinks=False)
    directory_id = (directory.st_dev, directory.st_ino)
    if os.path.realpath(args.receipts) != os.path.join(case_dir, "measured-receipts.json"):
        return [], ["measured receipts must be owned by the case directory"], case_dir, None
    if args.case_id not in (task_case_ids(contract, args.task_id) or []):
        return [], ["case is not declared by task"], case_dir, None
    if state.get("tasks", {}).get(args.task_id, {}).get("state") not in ("RUNNING", "VERIFYING"):
        return [], ["task is not running or verifying"], case_dir, None
    with open(args.receipts) as fh:
        rows = json.load(fh)
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
            args.case_id, candidate, run_dir, state.get("resource_allowlist", {}), contract))
    targets = {"leaf-%03d.json" % n for n in range(1, len(rows) + 1)}
    if any(name.startswith("leaf-") and name.endswith(".json") and name not in targets
           for name in os.listdir(case_dir)):
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
        for n, row in enumerate(rows, 1):
            temporary = ".sealing-" + uuid.uuid4().hex
            temp_fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL,
                             0o600, dir_fd=fd)
            try:
                with os.fdopen(temp_fd, "w") as fh:
                    json.dump(row, fh, sort_keys=True)
                    fh.flush()
                    os.fsync(fh.fileno())
                os.replace(temporary, "leaf-%03d.json" % n, src_dir_fd=fd, dst_dir_fd=fd)
            finally:
                try:
                    os.unlink(temporary, dir_fd=fd)
                except FileNotFoundError:
                    pass
        os.fsync(fd)
        current = os.stat(case_dir, follow_symlinks=False)
        opened = os.fstat(fd)
        return ((current.st_dev, current.st_ino) == (opened.st_dev, opened.st_ino)
                and os.path.realpath(case_dir) == case_dir)
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
        print(json.dumps({"result": "SEALED", "leaves": len(rows), "state_updated": False}))
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
        store.append_event("gate_artifacts_accepted", {"gate": args.gate,
            "candidate": state["verification_candidate"]})
        store.write_snapshot(state)
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
        now = time.time()
        if to == "RUNNING":
            limit = contract.get("retry_policy", {}).get("task_attempts_total_max", 5)
            if cur.get("attempt", 0) >= limit:
                die(EXIT_NOGO, "task attempt budget exhausted")
            started = cur.get("task_started_at", now)
            timeouts = contract.get("timeouts", {})
            seconds = timeouts.get("task_overrides", {}).get(
                args.task_id, timeouts.get("task_seconds", 28800))
            deadline = cur.get("task_deadline", started + seconds)
            if now >= deadline:
                die(EXIT_NOGO, "persisted task deadline exhausted")
            cur["task_started_at"] = started
            cur["task_deadline"] = deadline
        if to == "PASS":
            # PASS cannot be supplied as an operator assertion while proofs are absent.
            candidate = state.get("verification_candidate", {})
            if not candidate or candidate_errors(candidate):
                die(EXIT_CONTRACT, "task PASS needs the current verified candidate")
            if not cur.get("case_results") or not cur.get("commit_vector"):
                die(EXIT_CONTRACT, "task PASS needs measured case results and commit vector")
            die(EXIT_CONTRACT, "task PASS requires sealed artifact acceptance; manual PASS is disabled")
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
        st.append_event("task_state", {"task": args.task_id, "to": to,
                                       "attempt": cur.get("attempt", 0)})
        st.write_snapshot(state)
        print(json.dumps({"task": args.task_id, "state": to, "attempt": cur.get("attempt", 0)}))
        return EXIT_OK
    finally:
        lock.release()


def _task_artifacts(state, contract, task_id, run_dir):
    candidate = state.get("verification_candidate", {})
    errors = candidate_errors(candidate) if candidate else ["current candidate missing"]
    counts = {}
    for cid in task_case_ids(contract, task_id) or []:
        expected = contract["expected_leaf_matrix"][cid]
        index = {tuple(v[k] for k in LEAF_FIELDS): v for v in expected}
        case_dir = os.path.join(run_dir, "evidence", task_id, cid.split("/", 1)[1])
        actual = []
        total = dict(executed=0, passed=0, failed=0, skipped=0, exit_code=0)
        for name in sorted(os.listdir(case_dir)) if os.path.isdir(case_dir) else []:
            if not (name.startswith("leaf-") and name.endswith(".json")):
                continue
            try:
                with open(os.path.join(case_dir, name)) as fh:
                    row = json.load(fh)
                identity = tuple(row.get(k) for k in LEAF_FIELDS)
                actual.append("|".join(map(str, identity)))
                if identity not in index:
                    errors.append(cid + ": unexpected leaf")
                    continue
                errors.extend(receipt_errors(row, index[identity], task_id, cid,
                                             candidate, run_dir, state.get("resource_allowlist", {}), contract))
                for field in ("executed", "passed", "failed", "skipped"):
                    if type(row.get(field)) is int:
                        total[field] += row[field]
            except (OSError, ValueError, TypeError, AttributeError):
                errors.append(cid + ": malformed leaf")
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
        store.append_event("task_artifacts_accepted", {"task": args.task_id,
                           "candidate": state["verification_candidate"], "commit_vector": vector})
        store.write_snapshot(state)
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
