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
    # real 00000179.down.sql replay on the controlled rollback-cycle database
    # (after fn_v5_copy_eb_recovery_tombstone copied the live UUID+snapshot
    # into the queue) refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX before any
    # DDL, tombstone row unchanged.
    "down_with_recovery_tombstone_refused":
        [("down_with_recovery_tombstone_refused", 0),
         ("down_with_recovery_tombstone_block_attributable", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("enterprise", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
