#!/usr/bin/env python3
"""W1-02/migration measured adapter (expand-queue + guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/migration"
PHASES = ("expand-queue", "guards-contract")

# Probes that execute the leaf's defining action against the scratch database.
ADEQUATE = {
    "base_read_compat": [("base_read_compat", 0), ("base_read_compat2", 0)],
    "pk_fk_attempts_preserved": [("pk_fk_attempts_preserved", 0),
                                  ("pk_fk_attempts_preserved2", 0)],
    "ddl_rollback": [("ddl_rollback", 0), ("ddl_rollback2", 0)],
    "partial_location_null_rejected": [("partial_location_null_rejected", 0)],
    "recovery_id_unique_owner_conflict": [("recovery_id_unique_owner_conflict", 0)],
    "renew_expiry_monotonic_and_deadline_fixed":
        [("renew_expiry_monotonic_and_deadline_fixed", 0)],
    "function_digest_same_across_phases": [("function_digest_same_across_phases", 0)],
    # real legacy NULL session: fn_v5_pin_pending_content invoked on it and
    # refused with 22023 frozen-location-unavailable; identity columns and
    # session_state verified byte-identical afterwards.
    "old_null_session_protected": [("old_null_session_protected", 0)],
    # real 1,000,000-row synthetic load, concurrent writer + ROW EXCLUSIVE
    # locker, ALTER TABLE timing asserted against lock_timeout=5s with bounded
    # cancel, column absence, row-count integrity and writer-gap bound.
    "large_table_write_block_bound": [("large_table_write_block_bound", 0)],
    # real keyset<=500 batches over a committed cursor table, injected
    # interrupt rolled back (promoted/cursor stable), resume on the same
    # evidence version, identity/lifecycle checksum unchanged.
    "backfill_interrupt_resume": [("backfill_interrupt_resume", 0)],
    # real pre-V5 writer path (metadata + legacy completion row, no V5
    # identity); fn_v5_intent_ordinary_delete attempted and refused with the
    # proven reference-zero oracle; no queue job, no audit, blocked with
    # legacy_location_unverified.
    "rolling_old_writer_no_gc": [("rolling_old_writer_no_gc", 0)],
    # 15 M2 protocol functions actually invoked under their granted roles
    # (app_runtime/worker/proof) plus a definer/ACL inventory check.
    "expand_required_functions_callable": [("expand_required_functions_callable", 0)],
    # complete fn_v5_* signature/owner/definition inventory compared between
    # the expand and guards phase databases.
    "guards_do_not_install_missing_functions":
        [("guards_do_not_install_missing_functions", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    for _phase in PHASES:
        MAPPING[("plan", _ordering, _phase)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=True)
