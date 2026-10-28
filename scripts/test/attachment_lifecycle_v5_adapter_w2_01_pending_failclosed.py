#!/usr/bin/env python3
"""W2-01/pending-failclosed adapter.

Measures the six fail-closed leaves from the real w201 probe sections:
ordinary/internal registration errors plus the seat/widget EB registration
path.  Every leaf cites the exact probe line that produced its pass; a
missing line, a non-green probe, a failed section summary or an unreplayed
migration chain is a hard exit 30.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-01/pending-failclosed"
NAME = "w2_01_pending_failclosed"
SECTIONS = ("ordinary", "internal", "eb", "widget")


def _plan(leaf):
    """Return [(section, probe_entrypoint, [probe names], note)]."""
    domain = leaf["domain"]
    scenario = leaf["ordering_or_fault"]
    if domain == "ordinary":
        names = {
            "insert_error": ["failclosed_insert_error"],
            "insert_exception": ["failclosed_snapshot_conflict"],
            "commit_unknown": ["commit_unknown_reconcile_same_id"],
        }[scenario]
        return [("ordinary", "ordinary", names,
                 "ordinary attach_pending registration chain")]
    return {
        "insert_error": [
            ("internal", "internal", ["failclosed_bad_subject"],
             "application subject rejected"),
            ("eb", "enterprise_seat", ["eb_register_requires_row"],
             "seat registration without pending row rejected"),
            ("widget", "cs_widget", ["w_register_requires_row"],
             "visitor registration without pending row rejected"),
        ],
        "insert_exception": [
            ("internal", "internal", ["failclosed_snapshot_conflict"],
             "duplicate request id with different snapshot raises"),
            ("eb", "enterprise_seat", ["eb_identity_reassignment"],
             "seat identity reassignment raises"),
            ("widget", "cs_widget", ["w_identity_reassignment"],
             "visitor identity reassignment raises"),
        ],
        "commit_unknown": [
            ("eb", "enterprise_seat", ["eb_commit_unknown_replay"],
             "seat replay returns the registered session"),
            ("widget", "cs_widget", ["w_commit_unknown_replay"],
             "visitor replay returns the registered session"),
            ("internal", "internal", ["internal_idempotency"],
             "application registration keeps one durable row"),
        ],
    }[scenario]


def _oracle(leaf, db, migrations, section_notes, probe_names):
    scenario = leaf["ordering_or_fault"]

    if scenario == "insert_error":
        delta = ("no attach_pending/enterprise_asset row is created; the "
                 "registration call rejects the input before a capability exists")
        http = "fail-closed: no signed URL or capability is issued"
    elif scenario == "insert_exception":
        delta = ("a second registration with the same request id and a "
                 "different snapshot raises; the first durable row is unchanged")
        http = "fail-closed: exception response, no new capability"
    else:
        delta = ("same client_upload_id/request id reconciles to the same "
                 "durable session state; no second location or key is created")
        http = "recovery returns the durable session state, never a fresh signed URL for an ambiguous commit"
    return {
        "pre_db_state": "fresh %s with %s replayed up migrations (%s)"
                        % (db, migrations, section_notes),
        "pre_object_hash": "n/a (driver=none: registration leaf, no object bytes)",
        "action": "psql restricted-registration probes: %s"
                  % ", ".join(probe_names),
        "expected_http_semantics": http,
        "expected_db_delta": delta,
        "expected_object_state": "no storage object or provider PUT (pending row only)",
        "proof_actor": "psql imboy_user on %s; creator 101 and application "
                       "77001 + seat/visitor fixtures" % db,
    }



def build():
    run_dir, case_dir, _root = C.argv_paths()
    started = C.now_utc()
    ctx = C.load_context(run_dir)
    leaves = C.case_leaves(ctx["contract"], CASE_ID)
    slot = C.resolve_slot(run_dir)
    db = C.db_resource(ctx["allowlist"], slot, "w201")
    migrations = C.w201_migration_count(case_dir)
    gaps = C.Gaps()
    sections = {name: C.try_section(case_dir, name, gaps) for name in SECTIONS}
    section_assertions = []
    for name in SECTIONS:
        data = sections.get(name)
        if data is not None:
            section_assertions.append(C.section_assertion(name, data[1], data[2]))
    rows = []
    for leaf in leaves:
        plan = _plan(leaf)
        assertions = []
        request_ids = []
        executed = 0
        notes = []
        probe_names = []
        for section, probe_ep, names, note in plan:
            accepted = C.require_probe(gaps, sections.get(section), section,
                                       probe_ep, leaf["ordering_or_fault"],
                                       names, "%s/%s" % (leaf["domain"], leaf["ordering_or_fault"]))
            if len(accepted) != len(names):
                continue
            assertions.extend(accepted)
            executed += len(accepted)
            request_ids.extend("w201:%s:%s" % (section, name) for name in names)
            probe_names.extend(names)
            notes.append(note)
        if executed == 0:
            continue
        assertions.extend(section_assertions)
        assertions.append({
            "id": "migration_chain_replayed",
            "passed": True,
            "detail": "raw.log: w201 replayed %d up files; no SECTION_FAIL"
                      % migrations,
        })
        actor = {"kind": "real", "driver": "none",
                 "resource_kind": "scratch_postgres", "resource_id": db}
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": executed, "passed": executed,
             "failed": 0, "skipped": 0},
            _oracle(leaf, db, migrations, "; ".join(notes), probe_names),
            assertions, actor, [db], request_ids))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
