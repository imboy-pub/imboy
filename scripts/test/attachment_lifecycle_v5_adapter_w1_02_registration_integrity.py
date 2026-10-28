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
    # real resume attempt after lawful anonymization: a session registered by
    # user 99000103 whose parent user row is deleted still refuses a changed
    # resume (the snapshot is authoritative), via fn_v5_register_pending.
    "audit_null_does_not_grant_resume": [("audit_null_does_not_grant_resume", 0)],
}

ENTERPRISE_ADEQUATE = {
    # jsonb canonicalisation is database-level and domain-independent.
    "pg_canonical_json_digest_vector": [("pg_canonical_json_digest_vector", 0)],
    # real fn_v5_register_eb replays against per-run EA_* assets (created with
    # empty registration columns, then registered once by the restricted
    # function): changed mime / changed filename / other visitor session are
    # each refused on replay.
    "original_mime_conflict_after_restart":
        [("eb_original_mime_conflict_after_restart", 0)],
    "original_filename_conflict_after_restart":
        [("eb_original_filename_conflict_after_restart", 0)],
    "widget_other_visitor_session_denied":
        [("eb_widget_other_visitor_session_denied", 0)],
    # declared subject kind "contact" conflicts with the function's subject
    # kind argument ("seat") on a pre-created fresh asset; refused at
    # registration time.
    "seat_contact_subject_kind_conflict":
        [("eb_seat_contact_subject_kind_conflict", 0)],
    # v5_app_runtime denied DML on enterprise_asset registration columns
    # (column grants revoked; only fn_v5_register_eb fills them).
    "snapshot_direct_dml_rejected": [("eb_snapshot_direct_dml_rejected", 0)],
    # real enterprise resume after anonymization: user 99000102 deleted, the
    # changed-declaration replay of the registered EA_SUB4 session refused.
    "audit_null_does_not_grant_resume":
        [("eb_audit_null_does_not_grant_resume", 0)],
    # direct client_upload_id fabrication on a legacy (never-registered)
    # enterprise asset denied for v5_app_runtime; no identity is fabricated.
    "legacy_null_not_fabricated": [("eb_legacy_null_not_fabricated", 0)],
}

MAPPING = {}
for _ordering, _probes in ORDINARY_ADEQUATE.items():
    MAPPING[("ordinary", _ordering, PHASE)] = {"probes": _probes}
for _ordering, _probes in ENTERPRISE_ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
