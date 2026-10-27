#!/usr/bin/env python3
"""W1-02/recovery-tombstone-integrity measured adapter (guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/recovery-tombstone-integrity"
PHASE = "guards-contract"

ADEQUATE = {
    "copy_uuid_snapshot_before_metadata_delete":
        [("copy_uuid_snapshot_before_metadata_delete", 0)],
    "missing_snapshot_rollback": [("missing_snapshot_rollback", 0)],
    "cross_table_duplicate_uuid_rejected":
        [("cross_table_duplicate_uuid_rejected", 0)],
    "direct_dml_resurrection_denied": [("direct_dml_resurrection_denied", 0)],
    "tombstone_update_delete_denied": [("tombstone_update_delete_denied", 0)],
    "legacy_null_preserved": [("legacy_null_preserved", 0)],
}

GAPS = {
    "down_with_recovery_tombstone_refused": (
        [("down_with_recovery_tombstone_refused", 0)],
        "probe is a read-only CASE over enterprise_asset_delete_queue "
        "client_upload_id; no down migration or down guard is executed, so "
        "the refusal is a local predicate, not a measured down result"),
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in GAPS.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
