#!/usr/bin/env python3
"""W1-01/contract adapter: structural contract oracle from real artifacts.

The gate's W1-01 branch runs a Python structural check over
docs/architecture/attachment-lifecycle-v5/evidence/w1-contract-output.json and
ADR-0008, writing one line into CASE_DIR/raw.log.  This adapter re-parses that
line, re-derives every structural value from the same evidence file and the
ADR text, and refuses to emit observations if the run and the current
repository disagree.  The case is static: gate.run_case does not provision a
scratch PostgreSQL database for W1-01, so the actor resource is the admitted
repository, not a fabricated scratch database.
"""
import datetime
import json
import os
import sys

from attachment_lifecycle_v5_adapter_w1_support import (
    Gap, fail_gaps, iso, load_context, read_json, sha256_file, utc_now)

CASE_ID = "W1-01/contract"
ADR_REL = "docs/adr/0008-pluggable-attachment-storage.md"
EVIDENCE_REL = ("docs/architecture/attachment-lifecycle-v5/evidence/"
                "w1-contract-output.json")


def _truncate(value, limit=200):
    text = value if isinstance(value, str) else json.dumps(value, ensure_ascii=False)
    return text if len(text) <= limit else text[:limit] + "..."


def main():
    started = utc_now() - datetime.timedelta(seconds=1)
    run_dir = os.path.abspath(sys.argv[1])
    case_dir = os.path.abspath(sys.argv[2])
    repo_root = os.path.abspath(sys.argv[3])
    gaps = []
    try:
        _state, contract, _candidate, allowlist = load_context(run_dir, repo_root)
        expected = contract["expected_leaf_matrix"].get(CASE_ID)
        if not isinstance(expected, list) or len(expected) != 1:
            raise Gap("expected_leaf_matrix[%s] must contain exactly one leaf"
                      % CASE_ID)
        leaf = expected[0]
        allowed_repositories = allowlist.get("repository")
        if (not isinstance(allowed_repositories, list)
                or repo_root not in allowed_repositories):
            raise Gap("actor resource %s is not in "
                      "state.resource_allowlist.repository" % repo_root)
        raw_path = os.path.join(case_dir, "raw.log")
        if not os.path.isfile(raw_path):
            raise Gap("raw.log missing: %s" % raw_path)
        with open(raw_path, encoding="utf-8", errors="replace") as stream:
            raw = stream.read()
        if not raw.strip():
            raise Gap("raw.log is empty")
        evidence_path = os.path.join(repo_root, EVIDENCE_REL)
        evidence = read_json(evidence_path, "w1-contract-output.json")
        if not isinstance(evidence, dict):
            raise Gap("w1-contract-output.json is not an object")
        adr_path = os.path.join(repo_root, ADR_REL)
        try:
            with open(adr_path, encoding="utf-8", errors="replace") as stream:
                adr = stream.read()
        except OSError as exc:
            raise Gap("ADR-0008 unreadable: %s" % exc)
    except Gap as exc:
        fail_gaps(CASE_ID, [str(exc)])

    raw_lines = [line.strip() for line in raw.splitlines() if line.strip()]
    raw_hits = [line for line in raw_lines
                if line.startswith("contract-check errors:")]
    subjects = evidence.get("five_subjects")
    subjects = subjects if isinstance(subjects, dict) else {}
    acceptance = evidence.get("acceptance_contracts")
    acceptance = acceptance if isinstance(acceptance, list) else []
    acceptance_lengths = [len(item)
                          if isinstance(item, (str, list, dict, tuple)) else -1
                          for item in acceptance]
    entrypoints = evidence.get("delete_entrypoints_mapped")
    entrypoints = entrypoints if isinstance(entrypoints, list) else []
    lock_order = evidence.get("lock_order")
    lock_order = lock_order if isinstance(lock_order, list) else []
    storage_frozen = evidence.get("storage_interface_frozen")
    callbacks = (storage_frozen.get("callbacks")
                 if isinstance(storage_frozen, dict) else None)
    callbacks = callbacks if isinstance(callbacks, list) else []
    key_lifetime = evidence.get("key_lifetime")
    key_lifetime = key_lifetime if isinstance(key_lifetime, str) else ""
    hold_linearization = evidence.get("hold_linearization")
    hold_linearization = hold_linearization if isinstance(hold_linearization, str) else ""
    adr_sentence = "存量附件不做按 scope 的全量 garage 回填"

    checks = [
        ("raw_contract_check_no_errors",
         len(raw_hits) == 1 and raw_hits[0] == "contract-check errors: none",
         "raw.log contract-check lines=%r" % (raw_hits,)),
        ("evidence_task_id_w1_01",
         evidence.get("task_id") == "W1-01",
         "w1-contract-output.json task_id=%r" % (evidence.get("task_id"),)),
        ("five_subjects_eq_5",
         len(subjects) == 5,
         "five_subjects=%d keys=%s" % (len(subjects), list(subjects))),
        ("acceptance_contracts_eq_8_min_items",
         len(acceptance) == 8 and all(length >= 10
                                      for length in acceptance_lengths),
         "acceptance_contracts=%d item_lengths=%s"
         % (len(acceptance), acceptance_lengths)),
        ("delete_entrypoints_mapped_eq_12",
         len(entrypoints) == 12,
         "delete_entrypoints_mapped=%d head=%s"
         % (len(entrypoints), entrypoints[:4])),
        ("todo_placeholders_eq_0",
         evidence.get("todo_placeholders") == 0,
         "todo_placeholders=%r" % (evidence.get("todo_placeholders"),)),
        ("key_lifetime_never_reassigned",
         "never reassigned" in key_lifetime,
         "key_lifetime=%r" % _truncate(key_lifetime)),
        ("hold_linearization_gc_must_reject",
         "GC must reject" in hold_linearization,
         "hold_linearization=%r" % _truncate(hold_linearization)),
        ("lock_order_at_least_5",
         len(lock_order) >= 5,
         "lock_order=%d first=%r" % (len(lock_order),
                                     _truncate(lock_order[0] if lock_order else ""))),
        ("storage_callbacks_eq_6",
         len(callbacks) == 6,
         "storage_interface_frozen.callbacks=%d values=%s"
         % (len(callbacks), callbacks)),
        ("adr0008_v5_alignment",
         adr_sentence in adr,
         "ADR-0008 contains %r" % adr_sentence),
    ]
    for check_id, passed, detail in checks:
        if not passed:
            gaps.append("%s: %s" % (check_id, detail))
    if gaps:
        fail_gaps(CASE_ID, gaps)

    assertions = [{"id": check_id, "passed": True, "detail": detail}
                  for check_id, _passed, detail in checks]
    evidence_digest = sha256_file(evidence_path)
    finished = utc_now()
    if not 0 <= (finished - started).total_seconds() <= 1800:
        fail_gaps(CASE_ID, ["observation window exceeds 1800s"])
    observation = {
        "leaf": leaf,
        "started_at": iso(started),
        "finished_at": iso(finished),
        "exit_code": 0,
        "executed": len(assertions),
        "passed": len(assertions),
        "failed": 0,
        "skipped": 0,
        "oracle_observations": {
            "pre_db_state": ("not exercised: W1-01/contract is static; "
                             "gate.run_case W1-01 branch does not call ensure_pg"),
            "pre_object_hash": "sha256(w1-contract-output.json)=%s" % evidence_digest,
            "action": ("python structural oracle in "
                       "attachment_lifecycle_v5_gate.sh W1-01 branch + adapter "
                       "re-derivation from w1-contract-output.json and ADR-0008"),
            "expected_http_semantics": ("not exercised: no HTTP surface in "
                                        "W1-01/contract"),
            "expected_db_delta": "none: no database access in this case",
            "expected_object_state": ("five_subjects=%d acceptance_contracts=%d "
                                      "delete_entrypoints_mapped=%d "
                                      "todo_placeholders=0 lock_order=%d "
                                      "storage_callbacks=%d"
                                      % (len(subjects), len(acceptance),
                                         len(entrypoints), len(lock_order),
                                         len(callbacks))),
            "proof_actor": "targeted-contract:w1-contract-output.json",
        },
        "assertions": assertions,
        "actor": {"kind": "targeted-contract", "driver": leaf.get("driver"),
                  "resource_kind": "repository", "resource_id": repo_root},
        "resource_ids": [repo_root],
        "request_ids": ["w1-01-contract@%s" % evidence_digest[:16]],
    }
    out = os.path.join(case_dir, "observations.json")
    with open(out, "w", encoding="utf-8") as stream:
        json.dump({"leaves": [observation]}, stream, ensure_ascii=False, indent=1)
    print(json.dumps({"case_id": CASE_ID, "leaves": 1, "output": out}))


if __name__ == "__main__":
    main()
