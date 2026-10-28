#!/usr/bin/env python3
"""W4-04 leaf models (activation/reconcile aggregate suite)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from attachment_lifecycle_v5_adapter_w4_models_common import (  # noqa: E402
    S,
    _oracle,
)

def model_reconcile(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl")
    line = S.line_of(text, "w404_reconcile(Conn, Storage) ->")
    unit = f.test("attachment_lifecycle_v5_w404_activation_tests", func="run")
    note = {
        "startup_ready": "first block claims, expires the lease, runs attachment_delete_worker:reconcile/1 and reads retry_wait",
        "periodic_retry": "run_batch/2 consumes the due retry to done",
        "restart_expired_lease": "second key repeats claim/lease-expiry/reconcile; no process restart occurs",
        "nonowner_discrepancy_only": "the final attachment_delete_worker:reconcile() call has no assertion on discrepancy counts",
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append(
                "domain=enterprise: tenant rows 9101/9102 are seeded but queue "
                "jobs are unscoped; no enterprise reconcile fixture")
        return [S.per_leaf_gap(unit, "w404_reconcile/2"),
                "%s is asserted inside w404_reconcile/2 (%s:%d) within the "
                "single activation run/0; no per-scenario status"
                % (leaf["ordering_or_fault"],
                   "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
                   line)] + extra + [_oracle()]

    return gaps, ["reconcile has one aggregate unit for four orderings"]


def model_no_infinite_loop(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl")
    line = S.line_of(text, "w404_no_infinite_loop(Conn, Storage) ->")
    unit = f.test("attachment_lifecycle_v5_w404_activation_tests", func="run")
    note = {
        "task_attempt5_restart": "seeds attempts=5 retry_wait and reads it back; no restart occurs",
        "claim_attempt8_restart": "seeds attempts=8 blocked and counts the row; no claim attempt or rejection is recorded",
        "blocked_no_poll": "run_batch/2 consumes a normal job and reads blocked unchanged; partial but aggregate",
        "db_outage_bounded_wakeup": "simulates an outage as throw({abort_tx,simulated_outage}) and asserts {error,_}; no wakeup deadline or tick timing is measured",
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise queue fixture")
        return [S.per_leaf_gap(unit, "w404_no_infinite_loop/2"),
                "%s is asserted inside w404_no_infinite_loop/2 (%s:%d) within "
                "the single activation run/0"
                % (leaf["ordering_or_fault"],
                   "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
                   line)] + extra + [_oracle()]

    return gaps, ["no-infinite-loop has one aggregate unit"]


def model_guards_activation(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl")
    line = S.line_of(text, "w404_guards_activation(Conn, Storage) ->")
    producer_line = S.line_of(text, "check(#{conn => undefined})")
    has_old_worker = "old_worker" in text
    unit = f.test("attachment_lifecycle_v5_w404_activation_tests", func="run")
    note = {
        "all_prerequisites_present": "check/1 returns a map with size>=4; no per-prerequisite status",
        "producer_missing": "check(#{conn=>undefined}) accepts both {ok,_} and {error,_} (%s:%d), so nothing is measured" % ("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl", producer_line),
        "old_worker_running": "no old-worker fixture exists (old_worker token present=%s)" % has_old_worker,
        "guard_missing": "no missing-guard probe exists; the only missing-function path is the nonexistent probe name under conn=>undefined",
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise activation fixture")
        return [S.per_leaf_gap(unit, "w404_guards_activation/2"),
                "guard activation is asserted inside w404_guards_activation/2 "
                "(%s:%d) within the single activation run/0"
                % ("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
                   line)] + extra + [_oracle()]

    return gaps, ["guards-activation has one aggregate unit; producer_missing "
                  "is a no-op oracle"]


def model_all_writer_compat(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl")
    line = S.line_of(text, "w404_all_writer_compat(Conn) ->")
    unit = f.test("attachment_lifecycle_v5_w404_activation_tests", func="run")
    note = {
        "supported_schema_writer": "fn_v5_register_pending succeeds inside the aggregate unit; no per-writer status",
        "old_writer_incomplete_identity": "a direct INSERT is rejected 23514 inside the same aggregate unit; no separate writer unit",
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: only the ordinary fn_v5_register_pending writer is exercised")
        return [S.per_leaf_gap(unit, "w404_all_writer_compat/1"),
                "%s is asserted inside w404_all_writer_compat/1 (%s:%d) within "
                "the single activation run/0"
                % (leaf["ordering_or_fault"],
                   "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
                   line)] + extra + [_oracle()]

    return gaps, ["two writers share one aggregate unit"]


def model_activation_commit_unknown(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w404_activation_tests.erl")
    line = S.line_of(text, "w404_activation_commit_unknown(Conn) ->")
    guard_off_line = S.line_of(text, "guard_failure_worker_off")
    unit = f.test("attachment_lifecycle_v5_w404_activation_tests", func="run")
    note = {
        "ddl_committed_checkpoint_lost": "asserts pg_trigger count=1 after the migration; no checkpoint-loss simulation or replay",
        "proven_rollback": "only asserts pg_proc contains fn_v5_intent_eb_delete; no down/up replay",
        "partial_catalog_conflict": "a duplicate CREATE TRIGGER errors inside the aggregate unit; no migration-runner rollback is observed",
        "guard_failure_worker_off": "is a literal ok after the guard_failure_worker_off comment (source %s:%d); nothing is measured" % (
            "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
            guard_off_line),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise activation fixture")
        return [S.per_leaf_gap(unit, "w404_activation_commit_unknown/1"),
                "%s is asserted inside w404_activation_commit_unknown/1 "
                "(%s:%d) within the single activation run/0"
                % (leaf["ordering_or_fault"],
                   "test/repo/attachment_lifecycle_v5_w404_activation_tests.erl",
                   line)] + extra + [_oracle()]

    return gaps, ["activation-commit-unknown has one aggregate unit"]

W404_MODELS = {
    "W4-04/reconcile": model_reconcile,
    "W4-04/no-infinite-loop": model_no_infinite_loop,
    "W4-04/guards-activation": model_guards_activation,
    "W4-04/all-writer-compat": model_all_writer_compat,
    "W4-04/activation-commit-unknown": model_activation_commit_unknown,
}
