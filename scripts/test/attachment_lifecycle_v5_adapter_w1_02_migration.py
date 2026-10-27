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
}

# Probes whose SQL does not exercise the named property.  They are named here
# so the gap report can confirm the harness row exists and state why it is not
# a measurement; the adapter refuses to convert these rows into PASS receipts.
GAPS = {
    "old_null_session_protected": (
        [("old_null_session_protected", 0)],
        "probe SQL returns 'ok' from both CASE branches and only reads existing "
        "attach_pending rows; no backfill/UPDATE is attempted against legacy "
        "NULL client_upload_id, so protection is not measured"),
    "large_table_write_block_bound": (
        [("large_table_write_block_bound", 0)],
        "probe SQL is the constant SELECT 5 (expected 5); no 1,000,000-row load, "
        "lock acquisition, write-block timing, cancel or rollback is executed"),
    "backfill_interrupt_resume": (
        [("backfill_interrupt_resume", 0)],
        "probe is a DO block that UPDATEs and then explicitly RAISE EXCEPTION; "
        "the expected failure is guaranteed by the RAISE, with no batch/cursor, "
        "interrupt, resume or post-state rollback assertion"),
    "rolling_old_writer_no_gc": (
        [("rolling_old_writer_no_gc", 0)],
        "probe counts rows with last_error_code='legacy_location_unverified' in "
        "ready/leased state; no old writer is executed and no GC authorization "
        "attempt is made"),
    "expand_required_functions_callable": (
        [("expand_required_functions_callable", 0)],
        "probe counts 15 pg_proc rows owned by v5_fn_owner; no function is "
        "invoked and no EXECUTE/ACL callability is checked"),
    "guards_do_not_install_missing_functions": (
        [("guards_do_not_install_missing_functions", 0)],
        "probe counts 9 non-internal trg_v5_* triggers in the guards DB; it "
        "does not check whether M3 installed or omitted any function"),
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    for _phase in PHASES:
        MAPPING[("plan", _ordering, _phase)] = {"probes": _probes}
for _ordering, (_probes, _reason) in GAPS.items():
    for _phase in PHASES:
        MAPPING[("plan", _ordering, _phase)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=True)
