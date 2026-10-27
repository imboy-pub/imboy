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
}

GAPS = {
    "legal_fk_wrong_location": (
        [("legal_fk_wrong_location", 0)],
        "probe SQL references OLD outside a trigger ( ... AND OLD IS NULL), so "
        "the expected failure is a parser error; no fn_v5_confirm_pending call "
        "with a wrong-location target is executed"),
    "legal_fk_wrong_cipher": (
        [("legal_fk_wrong_cipher", 0)],
        "probe executes UPDATE attachment SET cipher='abc' WHERE id=900000201; "
        "it never calls fn_v5_confirm_pending with a cipher-mismatched "
        "attachment, so confirm-time cipher equivalence is not measured"),
    "confirmed_gc_preserves_session_link": (
        [("confirmed_gc_preserves_session_link", 0)],
        "probe is a read-only count of confirmed sessions without a link; no "
        "GC is executed, so preservation of the confirmed link across GC is "
        "not measured"),
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in GAPS.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
