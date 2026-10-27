#!/usr/bin/env python3
"""W1-02/audit-actor-anonymization measured adapter (guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/audit-actor-anonymization"
PHASE = "guards-contract"

ADEQUATE = {
    "direct_actor_substitution_denied": [("direct_actor_substitution_denied", 0)],
    "direct_actor_null_with_live_user_denied":
        [("direct_actor_null_with_live_user_denied", 0)],
    # real lawful anonymization: DELETE FROM "user" (99000101) committed with
    # a dedicated asset referencing it; FK ON DELETE SET NULL clears
    # uploaded_by_user_id while object_key/status/lifecycle/location stay
    # unchanged, the FK is retained (confdeltype=n) and no queue job/GC
    # follows.
    "fk_user_delete_sets_audit_null": [("fk_user_delete_sets_audit_null", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
