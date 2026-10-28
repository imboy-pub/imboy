#!/usr/bin/env python3
"""W6-02/admin-recovery-real measured adapter.

Binds all 20 widget/seat recovery scenarios, expanded over the two real storage
drivers, to the real-browser Playwright run captured by the gate (one JSON
report per driver) and to the live Chromium provenance recorded during that
run.  Missing spec/report/browser/provenance or an absent test is a hard gap.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402
import attachment_lifecycle_v5_adapter_w6_02_support as S2  # noqa: E402

CASE_ID = "W6-02/admin-recovery-real"
NAME = "w6_02_admin_recovery_real"
SCENARIOS = (
    "widget_timeout", "seat_timeout", "refresh", "process_restart",
    "widget_presign_response_lost", "widget_restart_before_put",
    "widget_expired_capability_resume", "widget_account_switch_late_callback",
    "widget_workspace_switch_late_callback", "widget_authorization_revoked",
    "widget_durable_checkpoint_failure", "seat_presign_response_lost",
    "seat_restart_before_put", "seat_expired_capability_resume",
    "seat_account_switch_late_callback", "seat_workspace_switch_late_callback",
    "seat_authorization_revoked", "seat_durable_checkpoint_failure",
    "widget_reselected_different_bytes_rejected",
    "seat_reselected_different_bytes_rejected",
)


def build():
    run_dir, case_dir, root = S6.C.argv_paths()
    ctx = S6.context(run_dir, CASE_ID, root)
    leaves = S6.leaves(ctx, CASE_ID)
    preflight = S2.require_preflight(case_dir, CASE_ID, False, True)
    gaps = S2.spec_gaps(preflight, [("admin_playwright",
                                      "Admin Playwright E2E spec")])
    gaps.extend(S2.provenance_pipeline_gaps(root, CASE_ID, False, True))
    S2.require_all(CASE_ID, gaps, len(leaves))
    S2.require_spec(case_dir, CASE_ID, preflight, "admin_playwright",
                    "Admin Playwright E2E spec")
    _device, browser = S2.read_provenance(case_dir, ctx, CASE_ID, False, True)
    try:
        bucket = S6.C.garage_bucket(run_dir, ctx["allowlist"])
    except S6.C.AdapterGap as exc:
        S2.require_all(CASE_ID, [str(exc)], len(leaves))
    disk_root = S6.W4.resolve_disk_root(ctx, CASE_ID)

    suites = {}
    for driver in ("garage", "disk"):
        suites[("playwright", driver)] = S2.playwright_report(
            case_dir, "admin-playwright-%s.json" % driver, CASE_ID,
            "W6-02 admin Playwright (%s)" % driver)
    mapping = {scenario: ("playwright", None) for scenario in SCENARIOS}

    def actor_lookup(leaf):
        return S2.actor_for(leaf["driver"], bucket, disk_root, CASE_ID)

    def oracle_for(leaf, test, suite, _device, browser, resource):
        backend = (preflight.get("backend") or {})
        return {
            "pre_db_state": "real backend %r (health probe captured in "
                            "w602-preflight.json) with driver=%s; real browser "
                            "%s pid=%d"
                            % (backend.get("base_url"), leaf["driver"],
                               browser["resource_id"], browser["process_id"]),
            "pre_object_hash": "the E2E spec controls its synthetic bytes and "
                               "asserts the asset identity; report %s"
                               % os.path.basename(suite["path"]),
            "action": "%s: test %r in the real Chromium run"
                      % (os.path.basename(suite["path"]), test["name"]),
            "expected_http_semantics": "the scenario's real backend calls "
                                       "succeeded as asserted by the browser "
                                       "spec (no route.fulfill substitution)",
            "expected_db_delta": "the spec asserts the durable task/asset/"
                                 "message identity and no cross-account journal "
                                 "write",
            "expected_object_state": "driver=%s real object state asserted by "
                                     "the browser spec" % leaf["driver"],
            "proof_actor": "real browser %s pid=%d; report sha256=%s; real "
                           "driver resource %s"
                           % (browser["resource_id"], browser["process_id"],
                              suite["sha256"], resource),
        }

    rows = S2.rows_for(ctx, CASE_ID, leaves, case_dir, suites, mapping, None,
                       browser, actor_lookup, oracle_for, "playwright")
    return case_dir, rows


if __name__ == "__main__":
    S6.run_adapter(NAME, build)
