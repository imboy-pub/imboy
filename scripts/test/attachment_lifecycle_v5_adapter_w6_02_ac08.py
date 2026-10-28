#!/usr/bin/env python3
"""W6-02/ac08 measured adapter.

AC08 compatibility: legacy ACL/cipher bytes, retention of unknown historical
objects, and physical-device restart, expanded over garage and disk.  Leaves
bind to the real physical-device integration run captured by the gate plus the
live device provenance.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402
import attachment_lifecycle_v5_adapter_w6_02_support as S2  # noqa: E402

CASE_ID = "W6-02/ac08"
NAME = "w6_02_ac08"
SCENARIOS = ("legacy_acl_cipher_bytes", "unknown_history_retained",
             "physical_device_restart")
MAPPING = {scenario: ("flutter", None) for scenario in SCENARIOS}


def build():
    run_dir, case_dir, root = S6.C.argv_paths()
    ctx = S6.context(run_dir, CASE_ID, root)
    leaves = S6.leaves(ctx, CASE_ID)
    preflight = S2.require_preflight(case_dir, CASE_ID, True, False)
    gaps = S2.spec_gaps(preflight, [("flutter_integration",
                                      "Flutter integration suite")])
    gaps.extend(S2.provenance_pipeline_gaps(root, CASE_ID, True, False))
    S2.require_all(CASE_ID, gaps, len(leaves))
    S2.require_spec(case_dir, CASE_ID, preflight, "flutter_integration",
                    "Flutter integration suite")
    device, _browser = S2.read_provenance(case_dir, ctx, CASE_ID, True, False)
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

    def actor_lookup(leaf):
        return S2.actor_for(leaf["driver"], bucket, disk_root, CASE_ID)

    def oracle_for(leaf, test, suite, device, _browser, resource):
        backend = (preflight.get("backend") or {})
        return {
            "pre_db_state": "real backend %r (health probe captured in "
                            "w602-preflight.json); driver=%s; device=%s pid=%d"
                            % (backend.get("base_url"), leaf["driver"],
                               device["resource_id"], device["process_id"]),
            "pre_object_hash": "the integration suite seeds historic synthetic "
                               "fixtures and asserts the read-ACL/cipher byte "
                               "hash; artifact %s"
                               % os.path.basename(suite["path"]),
            "action": "%s: test %r"
                      % (os.path.basename(suite["path"]), test["name"]),
            "expected_http_semantics": "old-client/historic reads keep their "
                                       "authorized ACL and payload semantics",
            "expected_db_delta": "unknown historical objects stay retained; no "
                                 "resurrection or destructive cleanup",
            "expected_object_state": "driver=%s historic object bytes/hash "
                                     "preserved" % leaf["driver"],
            "proof_actor": "physical device %s pid=%d; suite sha256=%s; driver "
                           "resource %s"
                           % (device["resource_id"], device["process_id"],
                              suite["sha256"], resource),
        }

    rows = S2.rows_for(ctx, CASE_ID, leaves, case_dir, suites, MAPPING, device,
                       None, actor_lookup, oracle_for, "ac08")
    return case_dir, rows


if __name__ == "__main__":
    S6.run_adapter(NAME, build)
