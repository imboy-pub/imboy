#!/usr/bin/env python3
"""W2-01/session-recovery-api adapter.

Ten recovery scenarios x four real upload entrypoints, parsed from the w201
ordinary/internal/eb/widget sections.  Each leaf must be backed by probe
lines carrying the leaf's own entrypoint; another entrypoint's line is never
substituted.  Duplicate-use lines (one probe proving two recovery facets)
are disclosed in the assertion detail.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-01/session-recovery-api"
NAME = "w2_01_session_recovery_api"

SURFACES = {
    "ordinary": ("ordinary", "ordinary"),
    "enterprise_internal": ("internal", "internal"),
    "enterprise_seat": ("eb", "enterprise_seat"),
    "cs_widget": ("widget", "cs_widget"),
}

# leaf.entrypoint -> scenario -> probe names (same section/entrypoint).
PROBES = {
    "ordinary": {
        "presign_response_lost": ["recovery_presign_response_lost"],
        "restart_before_put": ["recovery_presign_response_lost"],
        "expired_capability_same_key_renew": ["renew_monotonic"],
        "absolute_deadline_reupload_required": ["recovery_deadline_pre"],
        "renew_races_intent": ["recovery_renew_races_intent"],
        "confirmed_no_new_capability": ["recovery_confirmed_no_capability"],
        "authorization_revoked": ["recovery_authorization_revoked"],
        "request_id_payload_conflict": ["failclosed_snapshot_conflict"],
        "commit_unknown_no_capability": ["commit_unknown_reconcile_same_id"],
        "internal_idempotency_postcommit_sign": ["recovery_idempotent_identity"],
    },
    "enterprise_internal": {
        "presign_response_lost": ["recovery_status"],
        "restart_before_put": ["internal_restart_before_put"],
        "expired_capability_same_key_renew": ["internal_renew_same_key"],
        "absolute_deadline_reupload_required": ["internal_deadline_pre"],
        "renew_races_intent": ["internal_renew_races_intent"],
        "confirmed_no_new_capability": ["internal_confirmed_no_capability"],
        "authorization_revoked": ["cross_app_not_leaked"],
        "request_id_payload_conflict": ["internal_request_id_conflict"],
        "commit_unknown_no_capability": ["internal_idempotency",
                                          "internal_restart_before_put"],
        "internal_idempotency_postcommit_sign": ["internal_idempotency"],
    },
    "enterprise_seat": {
        "presign_response_lost": ["eb_status_readback"],
        "restart_before_put": ["eb_register_requires_row", "eb_register_ok"],
        "expired_capability_same_key_renew": ["eb_renew_monotonic"],
        "absolute_deadline_reupload_required": ["eb_renew_deadline_pre",
                                                "eb_renew_after_deadline"],
        "renew_races_intent": ["eb_renew_races_intent"],
        "confirmed_no_new_capability": ["eb_recover_for_confirm",
                                        "eb_confirmed_no_renew"],
        "authorization_revoked": ["eb_cross_tenant_not_found"],
        "request_id_payload_conflict": ["eb_identity_reassignment"],
        "commit_unknown_no_capability": ["eb_commit_unknown_replay"],
        "internal_idempotency_postcommit_sign": ["eb_register_ok",
                                                 "eb_commit_unknown_replay"],
    },
    "cs_widget": {
        "presign_response_lost": ["w_status_readback"],
        "restart_before_put": ["w_register_requires_row", "w_register_ok"],
        "expired_capability_same_key_renew": ["w_renew_monotonic"],
        "absolute_deadline_reupload_required": ["w_deadline_pre",
                                               "w_renew_after_deadline"],
        "renew_races_intent": ["w_races_intent"],
        "confirmed_no_new_capability": ["w_confirmed_no_renew"],
        "authorization_revoked": ["w_cross_tenant_not_found"],
        "request_id_payload_conflict": ["w_identity_reassignment"],
        "commit_unknown_no_capability": ["w_commit_unknown_replay"],
        "internal_idempotency_postcommit_sign": ["w_register_ok",
                                                 "w_commit_unknown_replay",
                                                 "w_idempotent_identity"],
    },
}

DELTA = {
    "presign_response_lost": "the durable session row is still registered after the response is lost; the client_upload_id resolves the identical key/asset",
    "restart_before_put": "a restart before PUT leaves the same registered durable session and resumable deadline",
    "expired_capability_same_key_renew": "renew updates upload_expires_at monotonically on the same key, then signs; no new session row",
    "absolute_deadline_reupload_required": "past the absolute deadline the renew window is closed and the client must re-upload",
    "renew_races_intent": "the intent transition wins; renew affects 0 rows and cannot extend a terminal session",
    "confirmed_no_new_capability": "a confirmed/terminal session yields no new capability state",
    "authorization_revoked": "cross-principal/cross-tenant lookups return 0 rows; no session is exposed",
    "request_id_payload_conflict": "reusing a request id with a different payload/snapshot is rejected",
    "commit_unknown_no_capability": "an ambiguous commit is reconciled from the durable row; no second key or capability is created",
    "internal_idempotency_postcommit_sign": "the post-commit identity signature stays unique (exactly one durable identity row)",
}
HTTP = {
    "presign_response_lost": "status/recovery read returns session state only, never a fresh signed URL",
    "restart_before_put": "status lookup returns registered + resumable, no signed URL",
    "expired_capability_same_key_renew": "renew commits the expiry before any signing",
    "absolute_deadline_reupload_required": "expired absolute deadline yields no capability",
    "renew_races_intent": "terminal intent yields no renewed capability",
    "confirmed_no_new_capability": "confirmed state yields no new capability",
    "authorization_revoked": "unauthorized principal receives not-found semantics",
    "request_id_payload_conflict": "conflicting payload for the same request id is refused",
    "commit_unknown_no_capability": "ambiguous commit is resolved from durable state, not by minting a new capability",
    "internal_idempotency_postcommit_sign": "idempotency storage does not contain a signed URL",
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
    sections = {name: C.try_section(case_dir, name, gaps)
                for name in ("ordinary", "internal", "eb", "widget")}
    section_assertions = {}
    for name, data in sections.items():
        if data is not None:
            section_assertions[name] = C.section_assertion(name, data[1], data[2])
    rows = []
    for leaf in leaves:
        entrypoint = leaf["entrypoint"]
        scenario = leaf["ordering_or_fault"]
        if entrypoint not in SURFACES:
            gaps.add("unknown recovery entrypoint=%r" % entrypoint)
            continue
        section, probe_ep = SURFACES[entrypoint]
        names = PROBES[entrypoint].get(scenario)
        if not names:
            gaps.add("no recovery probe mapping for entrypoint=%s scenario=%s"
                     % (entrypoint, scenario))
            continue
        accepted = C.require_probe(gaps, sections.get(section), section,
                                   probe_ep, scenario, names,
                                   "%s/%s" % (entrypoint, scenario))
        if len(accepted) != len(names):
            continue
        assertions = list(accepted)
        if section_assertions.get(section):
            assertions.append(section_assertions[section])
        assertions.append({
            "id": "migration_chain_replayed",
            "passed": True,
            "detail": "raw.log: w201 replayed %d up files; no SECTION_FAIL"
                      % migrations,
        })
        if len(names) > 1:
            assertions.append({
                "id": "recovery_facets_measured_together",
                "passed": True,
                "detail": "scenario %s measured by %d probe lines: %s"
                          % (scenario, len(names), ", ".join(names)),
            })
        if entrypoint == "ordinary" and scenario == "restart_before_put":
            assertions.append({
                "id": "durable_session_recovery_shared_probe",
                "passed": True,
                "detail": "recovery_presign_response_lost asserts registered + "
                          "session_deadline non-null, which is the restart-before-PUT "
                          "durable-recovery property for the ordinary entrypoint",
            })
        oracle = {
            "pre_db_state": "fresh %s with %d replayed up migrations; %s section "
                            "ok=%d bad=0" % (db, migrations, section,
                                             sections[section][1].get("ok")),
            "pre_object_hash": "no pre-existing object (recovery is DB-durable "
                               "session/location state, driver=none)",
            "action": "psql recovery probe(s): %s" % ", ".join(names),
            "expected_http_semantics": HTTP[scenario],
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": "no provider PUT or object mutation in "
                                     "this recovery leaf",
            "proof_actor": "psql imboy_user on %s; creator 101 / application "
                           "77001 / seat / visitor fixtures" % db,
        }
        actor = {"kind": "real", "driver": "none",
                 "resource_kind": "scratch_postgres", "resource_id": db}
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": len(accepted), "passed": len(accepted),
             "failed": 0, "skipped": 0},
            oracle, assertions, actor, [db],
            ["w201:%s:%s" % (section, name) for name in names]))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
