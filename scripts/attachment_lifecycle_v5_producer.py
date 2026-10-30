#!/usr/bin/env python3
"""Measured evidence producer for attachment lifecycle v5 closeout.

Converts real case-execution observations into per-leaf measured receipts
(<case_dir>/measured-receipts.json) that the runner's strict seal-case
verifier accepts. The producer never fabricates measurements: every leaf
row must arrive from a case adapter with real observation values, real
timestamps and real execution counts; anything missing is a hard error.

Inputs per leaf (adapter-supplied, JSON via --observations):
  leaf identity fields (must match the frozen expected_leaf_matrix),
  started_at/finished_at (ISO-8601 Z, inside observation window),
  execution counts (exit_code 0, executed>=1, failed=0, skipped=0),
  oracle_observations (7 required non-empty fields),
  assertions (non-empty list, every item passed=true),
  actor {kind, driver, resource_kind, resource_id},
  resource_ids (non-empty, includes actor resource_id),
  request_ids (non-empty), assertions list.

The producer binds each leaf to the frozen verification candidate from
state.json (candidate_heads / owned_tree_digest / plan_manifest_sha256 /
migration_digest), writes one structured raw JSON per leaf under the case
directory and emits the receipts array. Validation mirrors the runner's
receipt_errors so failures surface here with clearer diagnostics.
"""
import argparse
import ctypes
import errno
import datetime
import hashlib
import json
import os
import sys
import uuid

import attachment_lifecycle_v5_runner as runner

LEAF_FIELDS = ("ac_id", "domain", "driver", "entrypoint",
               "ordering_or_fault", "schema_phase")
ORACLE_FIELDS = ("pre_db_state", "pre_object_hash", "action",
                 "expected_http_semantics", "expected_db_delta",
                 "expected_object_state", "proof_actor")


def _die(code, msg):
    sys.stderr.write("producer error: %s\n" % msg)
    sys.exit(code)


def _open_case_dir(run_dir, task_id, case_id, case_dir, attempt):
    # New evidence is scoped to the persisted task attempt. Historical flat
    # cases are read only by the runner; they are never producer destinations.
    parts = case_id.split("/")
    if len(parts) != 2 or parts[0] != task_id or any(
            not p or p in (".", "..") for p in parts):
        raise ValueError("invalid case identity")
    if type(attempt) is not int or attempt < 1:
        raise ValueError("invalid task attempt")
    components = ("evidence", *parts, "attempt-%d" % attempt)
    expected = os.path.abspath(os.path.join(run_dir, *components))
    if case_dir != expected:
        raise ValueError("case directory does not match case identity")
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    fd = os.open(os.path.abspath(run_dir), flags)
    try:
        for part in components:
            child = os.open(part, flags, dir_fd=fd)
            os.close(fd)
            fd = child
        return fd
    except BaseException:
        os.close(fd)
        raise


def _write_exclusive(directory_fd, name, data):
    fd = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                 0o600, dir_fd=directory_fd)
    with os.fdopen(fd, "wb") as fh:
        fh.write(data)
        fh.flush()
        os.fsync(fh.fileno())


def _publish_noreplace(source_fd, target_fd):
    """Atomically rename without replacement; fail closed on unsupported OS/filesystem."""
    libc = ctypes.CDLL(None, use_errno=True)
    name = b"measured-receipts.json"
    if sys.platform == "darwin":
        function = getattr(libc, "renameatx_np", None)
        flag = 0x4  # RENAME_EXCL
    elif sys.platform.startswith("linux"):
        function = getattr(libc, "renameat2", None)
        flag = 1  # RENAME_NOREPLACE
    else:
        function = None
    if function is None:
        raise OSError(errno.ENOTSUP, "atomic no-replace rename unavailable")
    function.argtypes = (ctypes.c_int, ctypes.c_char_p, ctypes.c_int,
                         ctypes.c_char_p, ctypes.c_uint)
    function.restype = ctypes.c_int
    if function(source_fd, name, target_fd, name, flag) != 0:
        code = ctypes.get_errno()
        raise OSError(code, os.strerror(code))


def _same_published(run_dir, path, fresh):
    """Reconcile only byte-verified, semantically identical current-attempt output."""
    try:
        previous = json.loads(runner._read_run_artifact(run_dir, path))
        if not isinstance(previous, list) or len(previous) != len(fresh):
            return False
        for old, new in zip(previous, fresh):
            if not isinstance(old, dict):
                return False
            old_path = old.get("raw_output_path")
            old_fields = {k: v for k, v in old.items() if k != "raw_output_path"}
            new_fields = {k: v for k, v in new.items() if k != "raw_output_path"}
            if old_fields != new_fields:
                return False
            raw = runner._read_run_artifact(run_dir, old_path)
            if hashlib.sha256(raw).hexdigest() != old.get("raw_output_sha256"):
                return False
        return True
    except (OSError, ValueError, TypeError):
        return False


def _parse_ts(value):
    try:
        return datetime.datetime.fromisoformat(value.replace("Z", "+00:00"))
    except (AttributeError, ValueError):
        return None


def main():
    ap = argparse.ArgumentParser(prog="attachment_lifecycle_v5_producer.py")
    sub = ap.add_subparsers(dest="cmd")
    pr = sub.add_parser("produce")
    pr.add_argument("--run-dir", required=True)
    pr.add_argument("--task-id", required=True)
    pr.add_argument("--case-id", required=True)
    pr.add_argument("--case-dir", required=True)
    pr.add_argument("--command", required=True)
    pr.add_argument("--cwd", required=True)
    pr.add_argument("--observations", required=True,
                    help="adapter JSON with per-leaf real observations")
    args = ap.parse_args()
    if args.cmd != "produce":
        ap.print_usage()
        sys.exit(2)

    case_dir = os.path.abspath(args.case_dir)
    with open(os.path.join(args.run_dir, "state.json")) as fh:
        state = json.load(fh)
    candidate = state.get("verification_candidate") or {}
    if not candidate.get("heads"):
        _die(20, "verification candidate not frozen; run freeze-candidate first")
    with open(os.path.join(state["plan_dir"], "execution-contract.json")) as fh:
        contract = json.load(fh)
    expected_leaves = contract["expected_leaf_matrix"].get(args.case_id)
    if not expected_leaves:
        _die(2, "case %s not in expected_leaf_matrix" % args.case_id)

    task = state.get("tasks", {}).get(args.task_id, {})
    attempt = task.get("attempt")
    if task.get("state") not in ("RUNNING", "VERIFYING") or type(attempt) is not int or attempt < 1:
        _die(20, "task is not in a valid active attempt")
    expected_observations = os.path.join(case_dir, "observations.json")
    if os.path.abspath(args.observations) != expected_observations:
        _die(20, "observations must belong to the case directory")
    try:
        obs_doc = json.loads(runner._read_run_artifact(args.run_dir, expected_observations))
    except (OSError, ValueError, TypeError) as exc:
        _die(20, "unsafe observations: %s" % exc)

    rows = obs_doc.get("leaves") if isinstance(obs_doc, dict) else obs_doc
    if not isinstance(rows, list) or not rows:
        _die(2, "observations must be a non-empty leaves array")

    identity = lambda leaf: "|".join(str(leaf.get(k)) for k in LEAF_FIELDS)
    expected_index = {identity(leaf): leaf for leaf in expected_leaves}
    actual_ids = [identity(row.get("leaf", row)) for row in rows]
    if sorted(actual_ids) != sorted(expected_index):
        _die(20, "leaf identity set mismatch: expected %d leaves, got %r"
             % (len(expected_index), actual_ids))

    allowlist = state.get("resource_allowlist", {})
    resources_flat = {v for vs in allowlist.values()
                      if isinstance(vs, list) for v in vs}
    # A failed attempt is left isolated for diagnosis; it is never published.
    try:
        case_fd = _open_case_dir(args.run_dir, args.task_id, args.case_id, case_dir, attempt)
        attempt_name = "producer-attempt-" + uuid.uuid4().hex
        os.mkdir(attempt_name, 0o700, dir_fd=case_fd)
        attempt_fd = os.open(attempt_name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                             dir_fd=case_fd)
    except (OSError, ValueError) as exc:
        _die(20, "cannot create private attempt: %s" % exc)
    receipts = []
    for n, row in enumerate(rows, 1):
        leaf = row.get("leaf", row)
        exp = expected_index[identity(leaf)]
        errors = []
        counts = (row.get("exit_code"), row.get("executed"), row.get("passed"),
                  row.get("failed"), row.get("skipped"))
        if any(type(v) is not int or v < 0 for v in counts):
            errors.append("execution counts must be nonnegative integers")
        elif row["exit_code"] != 0 or row["failed"] != 0 or row["skipped"] != 0:
            errors.append("execution must be exit 0 / failed 0 / skipped 0")
        elif row["executed"] < 1 or row["executed"] != row["passed"] + row["failed"] + row["skipped"]:
            errors.append("executed != passed+failed+skipped (>=1)")
        observations = row.get("oracle_observations")
        if not isinstance(observations, dict):
            errors.append("oracle_observations must be an object")
        else:
            missing = [f for f in ORACLE_FIELDS if not observations.get(f)]
            if missing:
                errors.append("oracle_observations missing: %s" % missing)
        actor = row.get("actor") or {}
        resource_id = actor.get("resource_id", "")
        allowed = allowlist.get(actor.get("resource_kind"))
        if allowed is None or (isinstance(allowed, list) and resource_id not in allowed) \
                or (isinstance(allowed, str) and not resource_id.startswith(allowed)):
            errors.append("actor resource %r outside allowlist kind %r"
                          % (resource_id, actor.get("resource_kind")))
        if actor.get("kind") not in ("real", "targeted-contract"):
            errors.append("actor kind must be real|targeted-contract")
        if actor.get("kind") == "real" and leaf.get("driver") in ("garage", "disk") \
                and actor.get("driver") != leaf.get("driver"):
            errors.append("real driver provenance mismatch")
        res_ids = row.get("resource_ids") or []
        if not res_ids:
            errors.append("resource_ids empty")
        if any(v not in resources_flat and v != resource_id for v in res_ids):
            errors.append("resource_ids contain unregistered values")
        if not row.get("request_ids"):
            errors.append("request_ids empty")
        start = _parse_ts(row.get("started_at", ""))
        end = _parse_ts(row.get("finished_at", ""))
        deadline = exp.get("observation_deadline_seconds", 1800)
        if not start or not end or not 0 <= (end - start).total_seconds() <= deadline:
            errors.append("invalid observation window")
        assertions = row.get("assertions")
        if not isinstance(assertions, list) or not assertions or any(
                not isinstance(a, dict) or a.get("passed") is not True for a in assertions):
            errors.append("assertions must be non-empty and all passed")
        if errors:
            _die(20, "leaf %d (%s): %s" % (n, identity(leaf), "; ".join(errors)))

        raw_name = "raw-%03d.json" % n
        raw_path = os.path.join(case_dir, attempt_name, raw_name)
        raw_payload = {
            "task_id": args.task_id, "case_id": args.case_id, "task_attempt": attempt,
            "oracle_observations": observations,
            "candidate_heads": candidate["heads"],
            "assertions": assertions,
        }
        for k in LEAF_FIELDS:
            raw_payload[k] = leaf.get(k)
        raw_bytes = json.dumps(raw_payload, ensure_ascii=False, indent=1).encode("utf-8")
        _write_exclusive(attempt_fd, raw_name, raw_bytes)
        receipt = {
            "required": True, "task_id": args.task_id, "case_id": args.case_id,
            "task_attempt": attempt, "command": args.command, "cwd": args.cwd,
            "exit_code": row["exit_code"],
            "executed": row["executed"], "passed": row["passed"],
            "failed": row["failed"], "skipped": row["skipped"],
            "started_at": row["started_at"], "finished_at": row["finished_at"],
            "candidate_heads": candidate["heads"],
            "owned_tree_digest": candidate["owned_tree_digest"],
            "plan_manifest_sha256": candidate["plan_manifest_sha256"],
            "migration_digest": candidate["migration_digest"],
            "raw_output_path": raw_path, "raw_output_sha256": hashlib.sha256(raw_bytes).hexdigest(),
            "oracle_result": "PASS", "oracle_observations": observations,
            "actor_provenance": actor, "resource_ids": res_ids,
            "request_ids": row["request_ids"],
        }
        for k in LEAF_FIELDS:
            receipt[k] = leaf.get(k)
        # Provenance the runner verifies verbatim must survive into the receipt:
        # surface entrypoints, physical device and real-browser process identity.
        for proof in ("entrypoint_provenance", "device_provenance", "browser_provenance"):
            if row.get(proof):
                receipt[proof] = row[proof]
        receipts.append(receipt)

    out = os.path.join(case_dir, "measured-receipts.json")
    receipt_bytes = json.dumps(receipts, ensure_ascii=False, indent=1).encode("utf-8")
    try:
        _write_exclusive(attempt_fd, "measured-receipts.json", receipt_bytes)
        os.fsync(attempt_fd)
        try:
            _publish_noreplace(attempt_fd, case_fd)
        except OSError as exc:
            if exc.errno != errno.EEXIST or not _same_published(args.run_dir, out, receipts):
                raise
            # Only remove this invocation's private, known files after reconciliation.
            info = os.stat(attempt_name, dir_fd=case_fd, follow_symlinks=False)
            opened = os.fstat(attempt_fd)
            if (info.st_dev, info.st_ino) != (opened.st_dev, opened.st_ino):
                raise OSError(errno.ESTALE, "private attempt directory changed")
            for name in ["measured-receipts.json"] + ["raw-%03d.json" % n for n in range(1, len(receipts) + 1)]:
                os.unlink(name, dir_fd=attempt_fd)
            os.rmdir(attempt_name, dir_fd=case_fd)
        os.fsync(case_fd)
        os.fsync(attempt_fd)
    except OSError as exc:
        _die(20, "cannot publish or reconcile receipts; attempt preserved: %s" % exc)
    finally:
        os.close(attempt_fd)
        os.close(case_fd)
    print(json.dumps({"produced": len(receipts), "output": out}))


if __name__ == "__main__":
    main()
