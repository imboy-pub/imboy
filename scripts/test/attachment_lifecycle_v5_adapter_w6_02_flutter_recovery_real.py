#!/usr/bin/env python3
"""W6-02/flutter-recovery-real measured adapter.

Binds all 14 source recovery scenarios, expanded over the two real storage
drivers, to the physical-device Flutter integration run captured by the gate
(one JSONL per driver) and to the live device provenance recorded during that
run.  Missing artifact/device/provenance or an absent test is a hard gap.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402
import attachment_lifecycle_v5_adapter_w6_02_support as S2  # noqa: E402

CASE_ID = "W6-02/flutter-recovery-real"
NAME = "w6_02_flutter_recovery_real"
SCENARIOS = (
    "kill_after_registered", "kill_after_put", "kill_after_confirm",
    "unknown_message_commit", "user_switch", "presign_response_lost",
    "restart_before_put", "expired_capability_resume",
    "account_switch_late_callback", "workspace_switch_late_callback",
    "authorization_revoked", "durable_checkpoint_failure",
    "same_key_same_ciphertext_after_restart",
    "lost_cipher_material_requires_new_task",
)


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
    mapping = {scenario: ("flutter", None) for scenario in SCENARIOS}

    def actor_lookup(leaf):
        return S2.actor_for(leaf["driver"], bucket, disk_root, CASE_ID)

    def oracle_for(leaf, test, suite, device, _browser, resource):
        backend = (preflight.get("backend") or {})
        return {
            "pre_db_state": "real backend %r (health probe captured in "
                            "w602-preflight.json) with driver=%s; integration "
                            "test %r ran on the physical device"
                            % (backend.get("base_url"), leaf["driver"],
                               test["name"]),
            "pre_object_hash": "the integration suite controls its synthetic "
                               "bytes and asserts the ciphertext/hash; the "
                               "captured run is %s"
                               % os.path.basename(suite["path"]),
            "action": "%s: test %r on device %s (pid=%d)"
                      % (os.path.basename(suite["path"]), test["name"],
                         device["resource_id"], device["process_id"]),
            "expected_http_semantics": "the real backend/storage calls of the "
                                       "scenario returned success as asserted by "
                                       "the integration test",
            "expected_db_delta": "the integration test asserts the scenario's "
                                 "stable key/asset/message identity; no "
                                 "cross-account journal write",
            "expected_object_state": "driver=%s real object state asserted by "
                                     "the integration test" % leaf["driver"],
            "proof_actor": "physical device %s pid=%d; suite sha256=%s; real "
                           "driver resource %s"
                           % (device["resource_id"], device["process_id"],
                              suite["sha256"], resource),
        }

    rows = S2.rows_for(ctx, CASE_ID, leaves, case_dir, suites, mapping, device,
                       None, actor_lookup, oracle_for, "flutter-integration")
    return case_dir, rows


if __name__ == "__main__":
    S6.run_adapter(NAME, build)
