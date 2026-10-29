#!/usr/bin/env python3
"""W4-04 activation adapter support: emit 36 driver=none leaf rows.

Every leaf is sealed from its own suite observation (case/entrypoint/scenario/
domain + measured.driver=none), the parsed named eunit unit, the clean suite
summary, the run-owned scratch pool and the suite source emit check.  W4-04
leaves adjudicate database queue/catalog protocols, not object bytes, so the
proof actor is the real scratch PostgreSQL pool.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w4_03_support as S  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as W4  # noqa: E402

MODULE = "attachment_lifecycle_v5_w404_activation_tests"
MODULES = [MODULE]
SRC_REL = "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl"
LOG_NAMES = ("eunit-w404.log",)
MARKER = "W404_DIRECT_OK"

TEXT = {
    "startup_ready": (
        "no HTTP surface: the startup reconciler reaps the expired lease and "
        "returns the job to retry_wait without touching object bytes"),
    "periodic_retry": (
        "no HTTP surface: the periodic tick claims the due retry and resolves "
        "it to done"),
    "restart_expired_lease": (
        "no HTTP surface: after restart the expired lease is reaped and the "
        "job is claimed again with an incremented attempt counter"),
    "nonowner_discrepancy_only": (
        "no HTTP surface: a non-owner reconciliation reads counts and reports "
        "the discrepancy without mutating rows or deleting bytes"),
    "task_attempt5_restart": (
        "no HTTP surface: attempts survive a restart unchanged (never reset)"),
    "claim_attempt8_restart": (
        "no HTTP surface: a job at the attempt ceiling is never claimed a "
        "ninth time"),
    "blocked_no_poll": (
        "no HTTP surface: blocked terminal jobs are skipped while eligible "
        "jobs continue to be consumed"),
    "db_outage_bounded_wakeup": (
        "no HTTP surface: a lost database connection fails the batch at once "
        "(bounded wakeup, no tight retry loop)"),
    "all_prerequisites_present": (
        "no HTTP surface: the activation pre-check reports all four "
        "prerequisites and refuses to re-activate an already guarded schema"),
    "producer_missing": (
        "no HTTP surface: a missing intent producer function is reported in "
        "missing_functions and activation is refused"),
    "old_worker_running": (
        "no HTTP surface: a live legacy gc worker is reported and blocks "
        "activation"),
    "guard_missing": (
        "no HTTP surface: a disabled guard trigger flips "
        "triggers_already_attached to false so activation is allowed"),
    "supported_schema_writer": (
        "no HTTP surface: an adapted writer (register pending / enterprise "
        "intent) succeeds under the installed guards"),
    "old_writer_incomplete_identity": (
        "no HTTP surface: a legacy writer that omits the storage identity is "
        "rejected by the identity check and no row is created"),
    "ddl_committed_checkpoint_lost": (
        "no HTTP surface: replaying committed DDL after a lost checkpoint "
        "raises duplicate_object and the catalog stays single-attached"),
    "proven_rollback": (
        "no HTTP surface: the M3 down migration is refused with "
        "V5_DOWN_BLOCKED_USE_FORWARD_FIX and the catalog is unchanged"),
    "partial_catalog_conflict": (
        "no HTTP surface: a conflicting duplicate DDL fails without leaving a "
        "half-attached catalog"),
    "guard_failure_worker_off": (
        "no HTTP surface: the delete worker and reaper are disabled by "
        "default, so a failed guard never has a live writer consuming jobs"),
}


def oracle(leaf, obs, ctx, pool, bucket, disk_root, endpoint):
    """Measured oracle row for one W4-04 driver=none leaf."""
    measured = obs.get("measured") or {}
    scenario = leaf["ordering_or_fault"].rsplit("/", 1)[-1]
    case = leaf["_case"]
    detail = S.measured_detail(obs)

    if case.startswith("W4-04/reconcile"):
        db_delta = (
            "the queue row transitions exactly as recorded in measured "
            "(reaped=%s, state_after=%s, attempts=%s)"
            % (measured.get("reaped", "n/a"), measured.get("state_after", "n/a"),
               measured.get("attempts", measured.get("attempts_after", "n/a"))))
    elif case.startswith("W4-04/no-infinite-loop"):
        db_delta = (
            "attempt counters and terminal states match measured "
            "(attempts_before=%s, attempts_after=%s, blocked_state=%s)"
            % (measured.get("attempts_before", "n/a"),
               measured.get("attempts_after", measured.get("attempts", "n/a")),
               measured.get("blocked_state", "n/a")))
    elif case.startswith("W4-04/guards-activation"):
        db_delta = (
            "no data rows are written; the pre-check report is measured "
            "(missing_functions=%s, triggers_already_attached=%s, "
            "old_workers_running=%s)"
            % (measured.get("missing_functions", "n/a"),
               measured.get("triggers_already_attached", "n/a"),
               measured.get("old_workers_running", "n/a")))
    elif case.startswith("W4-04/all-writer-compat"):
        db_delta = (
            "the adapted writer writes (state=%s, rows=%s) or the legacy "
            "writer is rejected with %s and creates no row"
            % (measured.get("state", "n/a"), measured.get("rows", "1"),
               measured.get("rejected", "n/a")))
    else:
        db_delta = (
            "catalog rows are unchanged (triggers_before=%s, "
            "triggers_after=%s); the down path is refused with %s"
            % (measured.get("triggers_before", "n/a"),
               measured.get("triggers_after", "n/a"),
               measured.get("rollback_error", measured.get("replay_error", "n/a"))))

    return {
        "pre_db_state": (
            "scratch pool %s (run-owned marker database) holds the seeded "
            "queue/asset rows and the installed M3 guard catalog for this leaf"
            % pool),
        "pre_object_hash": (
            "n/a: this leaf is driver=none; it adjudicates database queue and "
            "catalog state, so no object bytes are read or claimed. The "
            "attachment_storage registry only supplies a canonical locator for "
            "the seeded rows"),
        "action": (
            "direct erl eunit unit %r -> real claim/ack/reaper functions, "
            "attachment_guards_activation_check:check/1, or catalog DDL probes "
            "on %s; measured=%s"
            % (S.description(leaf["_case"], leaf), pool, detail)),
        "expected_http_semantics": TEXT[scenario],
        "expected_db_delta": db_delta,
        "expected_object_state": (
            "no object bytes are created or deleted by this leaf; object "
            "deletion semantics are covered by the W4-01..W4-03 driver cases"),
        "proof_actor": (
            "real scratch PostgreSQL pool %s; the suite appends the observation "
            "from the same eunit VM after the asserts (emitted_at_ms=%s) and "
            "the adapter matches case/entrypoint/scenario/domain uniquely"
            % (pool, obs.get("emitted_at_ms"))),
    }


def run(case_id):
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])

    def oracle_fn(leaf, obs, ctx, pool, bucket, disk_root, endpoint):
        row = oracle(leaf, obs, ctx, pool, bucket, disk_root, endpoint)
        return row, [
            W4.assertion(
                "driver_none_actor",
                "driver=none leaf adjudicates database state on the run-owned "
                "scratch pool"),
            W4.assertion("measured_state", S.measured_detail(obs)),
        ]

    S.bind(
        case_id,
        run_dir=run_dir,
        case_dir=case_dir,
        root=root,
        log_names=LOG_NAMES,
        marker=MARKER,
        modules=MODULES,
        module=MODULE,
        oracle_fn=oracle_fn,
        source_rel=SRC_REL,
    )
