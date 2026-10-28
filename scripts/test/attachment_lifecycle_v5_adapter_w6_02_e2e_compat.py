#!/usr/bin/env python3
"""W6-02/e2e-compat measured adapter.

Covers physical-device and real-browser compatibility for old requests, historic
fixtures, multi-business references and network/process recovery, expanded over
the two real drivers.  A leaf is only measured when a real captured suite (the
physical-device Flutter integration run or the real-browser Playwright run)
contains a test carrying that scenario token, and the corresponding live
provenance record exists.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402
import attachment_lifecycle_v5_adapter_w6_02_support as S2  # noqa: E402

CASE_ID = "W6-02/e2e-compat"
NAME = "w6_02_e2e_compat"

MAPPING = {
    "physical_device": ("flutter", None),
    "real_browser": ("playwright", None),
    "old_request": ("any", None),
    "historic_fixture": ("any", None),
    "multi_business_reference": ("any", None),
    "network_recovery": ("any", None),
    "process_kill_recovery": ("any", None),
}


def build():
    run_dir, case_dir, root = S6.C.argv_paths()
    ctx = S6.context(run_dir, CASE_ID, root)
    leaves = S6.leaves(ctx, CASE_ID)
    preflight = S2.require_preflight(case_dir, CASE_ID, True, True)
    gaps = S2.spec_gaps(preflight, [
        ("flutter_integration", "Flutter integration suite"),
        ("admin_playwright", "Admin Playwright E2E spec"),
    ])
    gaps.extend(S2.provenance_pipeline_gaps(root, CASE_ID, True, True))
    S2.require_all(CASE_ID, gaps, len(leaves))
    S2.require_spec(case_dir, CASE_ID, preflight, "flutter_integration",
                    "Flutter integration suite")
    S2.require_spec(case_dir, CASE_ID, preflight, "admin_playwright",
                    "Admin Playwright E2E spec")
    device, browser = S2.read_provenance(case_dir, ctx, CASE_ID, True, True)
    try:
        bucket = S6.C.garage_bucket(run_dir, ctx["allowlist"])
    except S6.C.AdapterGap as exc:
        S2.require_all(CASE_ID, [str(exc)], len(leaves))
    disk_root = S6.W4.resolve_disk_root(ctx, CASE_ID)

    suites = {}
    for driver in ("garage", "disk"):
        suites[("flutter", driver)] = S2.flutter_suite(
            case_dir, "flutter-integration-%s.jsonl" % driver, CASE_ID,
            "W6-02 flutter integration (%s)" % driver)
        suites[("playwright", driver)] = S2.playwright_report(
            case_dir, "admin-playwright-%s.json" % driver, CASE_ID,
            "W6-02 admin Playwright (%s)" % driver)

    def actor_lookup(leaf):
        return S2.actor_for(leaf["driver"], bucket, disk_root, CASE_ID)

    def oracle_for(leaf, test, suite, device, browser, resource):
        backend = (preflight.get("backend") or {})
        return {
            "pre_db_state": "real backend %r (health probe captured in "
                            "w602-preflight.json); driver=%s; device=%s pid=%d; "
                            "browser=%s pid=%d"
                            % (backend.get("base_url"), leaf["driver"],
                               device["resource_id"], device["process_id"],
                               browser["resource_id"], browser["process_id"]),
            "pre_object_hash": "the captured suite controls old-request/"
                               "historian fixtures and asserts their byte "
                               "identity; artifact %s"
                               % os.path.basename(suite["path"]),
            "action": "%s: test %r"
                      % (os.path.basename(suite["path"]), test["name"]),
            "expected_http_semantics": "the scenario's real backend/storage "
                                       "calls succeeded without route.fulfill "
                                       "substitution of the core chain",
            "expected_db_delta": "the test asserts the historic/multi-business "
                                 "reference stays readable and no duplicate "
                                 "message is published",
            "expected_object_state": "driver=%s object state asserted by the "
                                     "captured suite" % leaf["driver"],
            "proof_actor": "physical device %s pid=%d; real browser %s pid=%d; "
                           "suite sha256=%s; driver resource %s"
                           % (device["resource_id"], device["process_id"],
                              browser["resource_id"], browser["process_id"],
                              suite["sha256"], resource),
        }

    rows = S2.rows_for(ctx, CASE_ID, leaves, case_dir, suites, MAPPING, device,
                       browser, actor_lookup, oracle_for, "e2e")
    return case_dir, rows


if __name__ == "__main__":
    S6.run_adapter(NAME, build)
