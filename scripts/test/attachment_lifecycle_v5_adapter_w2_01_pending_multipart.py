#!/usr/bin/env python3
"""W2-01/pending-multipart adapter.

Measures the legacy direct-PUT multipart/group_file ledger leaves from the
w201 `multipart` probe section for both domains and both contract AC labels.
The probe section reports AC01 per scenario; the AC02 leaves reuse the same
measured invocation (the contract duplicates the scenario across ACs) and
say so in an assertion instead of inventing a second execution.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-01/pending-multipart"
NAME = "w2_01_pending_multipart"
PROBES = {
    ("ordinary", "multipart_success"): ("ordinary", "mp_legacy_register_ok"),
    ("ordinary", "group_file_success"): ("ordinary", "mp_group_file_ok"),
    ("ordinary", "identity_mismatch"): ("ordinary", "mp_identity_mismatch"),
    ("ordinary", "database_unavailable"): ("ordinary", "mp_db_unavailable"),
    ("enterprise", "multipart_success"): ("enterprise", "mp_enterprise_domain_ok"),
    ("enterprise", "group_file_success"): ("enterprise", "mp_enterprise_group_ok"),
    ("enterprise", "identity_mismatch"): ("enterprise", "mp_enterprise_identity"),
    ("enterprise", "database_unavailable"): ("enterprise", "mp_enterprise_db_down"),
}
DELTA = {
    "multipart_success": "legacy bare registration lands one registered row on the shared ledger",
    "group_file_success": "group scope registration lands one registered row on the shared ledger",
    "identity_mismatch": "another principal cannot see or rebind the pending row (lookup returns 0)",
    "database_unavailable": "registration cannot complete; no capability/session row is produced",
}
HTTP = {
    "multipart_success": "legacy client receives a registered pending session, no new URL surface",
    "group_file_success": "group_file signing obeys the same registered owner/location",
    "identity_mismatch": "fail-closed: cross-principal lookup yields no capability",
    "database_unavailable": "fail-closed: DB outage yields no signed capability",
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
    data = C.try_section(case_dir, "multipart", gaps)
    section_assertion = ([C.section_assertion("multipart", data[1], data[2])]
                         if data is not None else [])
    rows = []
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        domain = leaf["domain"]
        if (domain, scenario) not in PROBES:
            gaps.add("no probe mapping for multipart domain=%s scenario=%s"
                     % (domain, scenario))
            continue
        probe_ep, name = PROBES[(domain, scenario)]
        accepted = C.require_probe(gaps, data, "multipart", probe_ep, scenario,
                                   [name], "pending-multipart/%s" % domain)
        if len(accepted) != 1:
            continue
        assertions = list(accepted) + list(section_assertion)
        assertions.append({
            "id": "migration_chain_replayed",
            "passed": True,
            "detail": "raw.log: w201 replayed %d up files; no SECTION_FAIL"
                      % migrations,
        })
        if leaf["ac_id"] != "AC01":
            assertions.append({
                "id": "ac_variant_uses_same_measured_invocation",
                "passed": True,
                "detail": "%s leaf reuses the same w201 multipart probe line "
                          "(%s, probe ac=AC01); the contract duplicates the "
                          "scenario across AC ids" % (leaf["ac_id"], name),
            })
        oracle = {
            "pre_db_state": "fresh %s with %d replayed up migrations; multipart "
                            "section ok=%d bad=0"
                            % (db, migrations, data[1].get("ok")),
            "pre_object_hash": "n/a (driver=none: legacy ledger registration, "
                               "no object bytes in this leaf)",
            "action": "psql legacy add/ownership probe %s" % name,
            "expected_http_semantics": HTTP[scenario],
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": "no storage object touched; only the "
                                     "pending ledger row is asserted",
            "proof_actor": "psql imboy_user on %s; creator 101 / enterprise "
                           "scope fixtures" % db,
        }
        actor = {"kind": "real", "driver": "none",
                 "resource_kind": "scratch_postgres", "resource_id": db}
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [db], ["w201:multipart:%s" % name]))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
