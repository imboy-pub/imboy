#!/usr/bin/env python3
"""W1-02/audit-actor-anonymization measured adapter (guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/audit-actor-anonymization"
PHASE = "guards-contract"

ADEQUATE = {
    "direct_actor_substitution_denied": [("direct_actor_substitution_denied", 0)],
    "direct_actor_null_with_live_user_denied":
        [("direct_actor_null_with_live_user_denied", 0)],
}

GAPS = {
    "fk_user_delete_sets_audit_null": (
        [("fk_user_delete_sets_audit_null", 0)],
        "probe executes UPDATE enterprise_asset SET uploaded_by_user_id=NULL "
        "WHERE id=910000101 and expects failure; no user DELETE is executed, "
        "so the allowed ON DELETE SET NULL path named by the leaf is not "
        "measured (only direct NULL denial)"),
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in GAPS.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
