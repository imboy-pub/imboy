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
    # real live reference: group_file 8001 binds attachment 900000330 with a
    # full location; fn_v5_intent_ordinary_delete is invoked and refused.
    "intent_with_live_reference": [("intent_with_live_reference", 0)],
    # real EAR hold fixture on the attachment; the intent call is refused.
    "intent_under_hold": [("intent_under_hold", 0)],
    # real future-retention EAR row (no hold, not yet purged); refused.
    "intent_future_retention": [("intent_future_retention", 0)],
    # injected audit-write failure during the three-write intent; rejection
    # measured and full rollback asserted (no intent row, no queue job, no
    # audit, lifecycle stays live).
    "intent_missing_audit": [("intent_missing_audit", 0),
                             ("intent_missing_audit_rollback", 0)],
    # injected metadata-transition failure after eligibility; rejection
    # measured and full rollback asserted.
    "intent_missing_metadata_transition":
        [("intent_missing_metadata_transition", 0),
         ("intent_missing_metadata_transition_rollback", 0)],
}

ENTERPRISE_ADEQUATE = {
    # worker/app denied any direct or function-driven enqueue on the
    # enterprise queue; the v5_fn_owner escalation check is domain-neutral.
    "runtime_set_role_owner": [("runtime_set_role_owner", 0)],
    "worker_arbitrary_enqueue": [("worker_arbitrary_enqueue_eb", 0)],
    "queue_insert_ready": [("eb_queue_insert_ready", 0)],
    "queue_insert_leased": [("eb_queue_insert_leased", 0)],
    "queue_insert_retry_wait": [("eb_queue_insert_retry_wait", 0)],
    "queue_insert_done": [("eb_queue_insert_done", 0)],
    "activate_legacy_without_proof": [("eb_activate_legacy_without_proof", 0)],
    "forge_gc_verified": [("eb_forge_gc_verified", 0)],
    # forged non-live lifecycle_state on INSERT refused by the
    # 00000231 INSERT-branch guard ("v5 eb asset must start live").
    "forge_gc_verified_on_insert": [("eb_forge_lifecycle_on_insert", 0)],
    "direct_metadata_intent": [("eb_direct_metadata_intent", 0)],
    "direct_session_intent": [("eb_direct_session_intent", 0)],
    # real authorized eligible intent (per-run EA_AUTH asset with a complete
    # storage identity): fn_v5_intent_eb_delete commits atomically — ready
    # job, delete_intent lifecycle, one enterprise audit row.
    "authorized_intent_atomic": [("eb_authorized_intent_atomic", 0)],
    # real live-reference enterprise asset (blocked queue row seeded first);
    # second intent refused with "job already exists".
    "intent_with_live_reference": [("eb_intent_with_live_reference", 0)],
    # real workspace hold (dedicated workspace 97000102, mandatory id column)
    # blocks the intent for the asset under it.
    "intent_under_hold": [("eb_intent_under_hold", 0)],
    # real retain_until in the future; intent refused.
    "intent_future_retention": [("eb_future_retention", 0)],
    # injected audit-write failure on the enterprise intent; rejection and
    # full rollback asserted.
    "intent_missing_audit": [("eb_intent_missing_audit", 0),
                             ("eb_intent_missing_audit_rollback", 0)],
    # injected metadata-transition failure on the enterprise intent;
    # rejection and full rollback asserted.
    "intent_missing_metadata_transition":
        [("eb_intent_missing_metadata_transition", 0),
         ("eb_intent_missing_metadata_transition_rollback", 0)],
}

MAPPING = {}
for _ordering, _probes in ORDINARY_ADEQUATE.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes}
for _ordering, _probes in ENTERPRISE_ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
