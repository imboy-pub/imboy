#!/usr/bin/env python3
"""W1-02/registration-integrity measured adapter (guards-contract, two domains)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/registration-integrity"
PHASE = "guards-contract"

ORDINARY_ADEQUATE = {
    "original_mime_conflict_after_restart":
        [("original_mime_conflict_after_restart", 0)],
    "original_filename_conflict_after_restart":
        [("original_filename_conflict_after_restart", 0)],
    "snapshot_direct_dml_rejected": [("snapshot_direct_dml_rejected", 0)],
    "legacy_null_not_fabricated": [("legacy_null_not_fabricated", 0)],
    "pg_canonical_json_digest_vector": [("pg_canonical_json_digest_vector", 0)],
}

ORDINARY_GAPS = {
    "audit_null_does_not_grant_resume": (
        [("audit_null_does_not_grant_resume", 0)],
        "probe executes a direct NULL update on enterprise_asset and expects "
        "failure; no resume request after a legitimate anonymization is "
        "attempted, so 'does not grant resume' is not measured"),
}

ENTERPRISE_ADEQUATE = {
    # jsonb canonicalisation is database-level and domain-independent.
    "pg_canonical_json_digest_vector": [("pg_canonical_json_digest_vector", 0)],
}

ENTERPRISE_GAPS = {
    "original_mime_conflict_after_restart": (
        [("original_mime_conflict_after_restart", 0)],
        "probe calls fn_v5_register_pending on ordinary attach_pending "
        "(v5p/reg1); no fn_v5_register_eb / enterprise_asset_delete_queue "
        "replay is executed"),
    "original_filename_conflict_after_restart": (
        [("original_filename_conflict_after_restart", 0)],
        "probe calls fn_v5_register_pending on ordinary attach_pending "
        "(v5p/reg1); no fn_v5_register_eb / enterprise_asset_delete_queue "
        "replay is executed"),
    "widget_other_visitor_session_denied": (
        [("widget_other_visitor_session_denied", 0)],
        "probe calls fn_v5_register_eb for asset_id=910000999, but the probe "
        "seed only creates 910000101; failure may be a missing asset rather "
        "than visitor-session denial"),
    "seat_contact_subject_kind_conflict": (
        [("seat_contact_subject_kind_conflict", 0)],
        "probe calls fn_v5_register_eb for asset_id=910000102, but the probe "
        "seed only creates 910000101; failure may be a missing asset rather "
        "than a seat subject-kind conflict"),
    "snapshot_direct_dml_rejected": (
        [("snapshot_direct_dml_rejected", 0)],
        "probe updates attach_pending.upload_registration (ordinary); no "
        "enterprise_asset_delete_queue.upload_registration DML is attempted"),
    "audit_null_does_not_grant_resume": (
        [("audit_null_does_not_grant_resume", 0)],
        "probe updates enterprise_asset.uploaded_by_user_id directly and "
        "expects failure; no enterprise resume is attempted after an "
        "anonymization"),
    "legacy_null_not_fabricated": (
        [("legacy_null_not_fabricated", 0)],
        "probe updates attach_pending.client_upload_id (ordinary legacy row); "
        "no enterprise queue legacy-NULL backfill attempt is made"),
}

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
