#!/usr/bin/env python3
"""W1-02/session-link-integrity measured adapter (guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/session-link-integrity"
PHASE = "guards-contract"

ADEQUATE = {
    "confirmed_null": [("confirmed_null", 0)],
    "registered_link_nonnull": [("registered_link_nonnull", 0)],
    "legal_fk_wrong_key": [("legal_fk_wrong_key", 0)],
    "legal_fk_wrong_owner": [("legal_fk_wrong_owner", 0)],
    "legal_fk_wrong_scope": [("legal_fk_wrong_scope", 0)],
    "confirmed_rebind": [("confirmed_rebind", 0)],
    "confirmed_to_registered": [("confirmed_to_registered", 0)],
    "lawful_repeat": [("lawful_repeat", 0)],
    # real fn_v5_confirm_pending call against a session/asset pair whose
    # immutable locator digests differ (LOCA vs LOCB); refused with the
    # frozen-location mismatch error.
    "legal_fk_wrong_location": [("legal_fk_wrong_location", 0)],
    # real confirm-then-forgery sequence: cipher replaced and cleared, both
    # refused with 23510, fixed declaration re-verified with the confirmed
    # link intact.
    "legal_fk_wrong_cipher": [("legal_fk_wrong_cipher", 0)],
    # real GC intent (fn_v5_intent_ordinary_delete after capability expiry,
    # migration_probes occurrence 0) committing delete_intent + ready job
    # while the confirmed session keeps its link (confirmed:<id>).
    "confirmed_gc_preserves_session_link": [("confirmed_gc_preserves_session_link", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
