#!/usr/bin/env python3
"""W1-02/intent-authority measured adapter (guards-contract, two domains)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/intent-authority"
PHASE = "guards-contract"

ORDINARY_ADEQUATE = {
    "queue_insert_ready": [("queue_insert_ready", 0)],
    "queue_insert_leased": [("queue_insert_leased", 0)],
    "queue_insert_retry_wait": [("queue_insert_retry_wait", 0)],
    "queue_insert_done": [("queue_insert_done", 0)],
    "activate_legacy_without_proof": [("activate_legacy_without_proof", 0)],
    # The guards block runs immutable_probes before intent_authority_probes;
    # the intent-authority occurrence is the second forge_gc_verified row.
    "forge_gc_verified": [("forge_gc_verified", 1)],
    "direct_metadata_intent": [("direct_metadata_intent", 0)],
    "runtime_set_role_owner": [("runtime_set_role_owner", 0)],
    "worker_arbitrary_enqueue": [("worker_arbitrary_enqueue", 0)],
    "authorized_intent_atomic": [("authorized_intent_atomic", 0)],
    "forge_gc_verified_on_insert": [("forge_gc_verified_on_insert", 0)],
    "direct_session_intent": [("direct_session_intent", 0)],
}

ORDINARY_GAPS = {
    "intent_with_live_reference": (
        [("intent_with_live_reference", 0)],
        "probe first runs SET ROLE v5_app_runtime; UPDATE attachment SET "
        "gc_verified=true ...; ON_ERROR_STOP aborts on that statement, so "
        "fn_v5_intent_ordinary_delete is never reached and live-reference "
        "rejection is not exercised"),
    "intent_under_hold": (
        [("intent_under_hold", 0)],
        "probe calls fn_v5_intent_ordinary_delete('v5p/none','probe') on a "
        "nonexistent key; no held object fixture is created, so the hold path "
        "is not exercised"),
    "intent_future_retention": (
        [("intent_future_retention", 0)],
        "the only probe with this name updates enterprise_asset 910000101 and "
        "calls fn_v5_intent_eb_delete; no ordinary-domain retention probe "
        "exists"),
    "intent_missing_audit": (
        [("intent_missing_audit", 0)],
        "probe is a read-only count of attachment_lifecycle_v5_audit rows for "
        "v5p/seed/1; no intent-without-audit is attempted, so rejection is not "
        "measured"),
    "intent_missing_metadata_transition": (
        [("intent_missing_metadata_transition", 0)],
        "probe is an invariant count of queue rows lacking a metadata "
        "transition; no missing-transition intent is attempted, so rejection "
        "is not measured"),
}

ENTERPRISE_ADEQUATE = {
    "intent_future_retention": [("intent_future_retention", 0)],
    "runtime_set_role_owner": [("runtime_set_role_owner", 0)],
}

ENTERPRISE_ORDINARY_ONLY = (
    "the probe with this ordering targets attachment_object_deletion / "
    "attach_pending / attachment (ordinary domain); no "
    "enterprise_asset_delete_queue or fn_v5_intent_eb_delete measurement "
    "exists for this ordering"
)

ENTERPRISE_GAPS = {}
for _ordering in ORDINARY_ADEQUATE:
    if _ordering in ENTERPRISE_ADEQUATE:
        continue
    if _ordering in ORDINARY_GAPS:
        _probes, _reason = ORDINARY_GAPS[_ordering]
        ENTERPRISE_GAPS[_ordering] = (_probes, _reason)
    else:
        ENTERPRISE_GAPS[_ordering] = (
            [ORDINARY_ADEQUATE[_ordering][0]], ENTERPRISE_ORDINARY_ONLY)
for _ordering, (_probes, _reason) in ORDINARY_GAPS.items():
    if _ordering not in ENTERPRISE_GAPS and _ordering not in ENTERPRISE_ADEQUATE:
        ENTERPRISE_GAPS[_ordering] = (_probes, _reason)

MAPPING = {}
for _ordering, _probes in ORDINARY_ADEQUATE.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in ORDINARY_GAPS.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}
for _ordering, _probes in ENTERPRISE_ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in ENTERPRISE_GAPS.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
