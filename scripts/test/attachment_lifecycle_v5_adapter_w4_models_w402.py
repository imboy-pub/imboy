#!/usr/bin/env python3
"""W4-02 leaf models (worker protocol, recovery and queue adoption)."""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from attachment_lifecycle_v5_adapter_w4_models_common import (  # noqa: E402
    PURGED_SCENARIOS,
    S,
    _oracle,
)

def _w402_common(f, leaf, test, extra, aggregate_note):
    module = test["module"] if test else "attachment_lifecycle_v5_w402"
    missing = [S.per_leaf_gap(test, "%s aggregate run/0" % module),
               "%s; eunit reports one unit for the whole suite" % aggregate_note]
    missing.extend(extra)
    if leaf["driver"] == "garage":
        missing.append(
            "driver=garage: the W4-02 suite exercises only DB CAS functions "
            "(attachment_gc_repo / eb_pg_asset_delete_queue); it never calls "
            "attachment_storage_garage/elib_oss and no W202_GARAGE_* value is "
            "read, so no real garage object receipt exists")
    missing.append(_oracle())
    return missing


def model_worker_crash(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        if scenario == "kill_after_claim_before_delete":
            extra = [
                "w402_worker_crash_no_loss/1 simulates a post-claim crash by "
                "UPDATE lease_until and a later reaper reclaim; no process is "
                "killed and no per-fault status is reported"]
        elif scenario == "kill_before_claim_commit":
            extra = [
                "no kill-before-claim fixture: w402_worker_crash_no_loss/1 "
                "always completes v5_claim before touching the lease"]
        else:
            extra = [
                "no delete step or completion-crash fixture exists in "
                "attachment_lifecycle_v5_w402_worker_tests"]
        if leaf["domain"] == "enterprise":
            extra.append(
                "domain=enterprise: crash rows are unscoped "
                "attachment_object_deletion rows; no enterprise queue fixture")
        return _w402_common(f, leaf, test, extra,
                            "worker-crash is asserted inside w402_worker_crash_no_loss/1")

    return gaps, ["suite implements one ordinary crash ordering only"]


def model_worker_lease(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "concurrent_two_claims": (
            "w402_claim_capacity_and_liveness/1 claims three distinct ready "
            "jobs sequentially and then expects empty; no two-worker "
            "concurrent-claim race"),
        "stale_token_completion": (
            "w402_worker_db_fault/1 calls fn_v5_ack_ordinary with a fresh token "
            "and asserts stale_token; no stale-token completion fixture"),
        "expired_delete_same_tombstone": (
            "w402_worker_crash_no_loss/1 reaps an expired lease and re-claims "
            "the same row; there is no delete/tombstone assertion"),
        "fixed_lease_no_renewal": (
            "no lease-renewal or expiry-duration measurement exists"),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append(
                "domain=enterprise: no enterprise queue/lease fixture")
        return _w402_common(f, leaf, test, extra,
                            "worker lease semantics live in the aggregate run/0")

    return gaps, ["worker lease ordering leaves have no dedicated unit"]


def model_worker_db_fault(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "db_lost_before_claim_verified": (
            "w402_worker_db_fault/1 only checks stale-token and missing-ack "
            "outcomes; it injects no database outage/lost connection around "
            "claim verification"),
        "db_lost_after_delete": (
            "the suite has no delete step, so a DB loss after delete is not "
            "executed"),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append(
                "domain=enterprise: no enterprise queue fixture")
        return _w402_common(f, leaf, test, extra,
                            "worker-db-fault is asserted inside w402_worker_db_fault/1")

    return gaps, ["no database-fault injection exists in the W4-02 suite"]


def model_queue_adoption(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "enterprise_pending_adoption": (
            "w402_queue_adoption/1 inserts an enterprise_asset deleted+pending "
            "row and calls fn_v5_intent_eb_delete; this is asserted only inside "
            "the aggregate worker run/0"),
        "enterprise_intent_replay": (
            "no replay fixture exists (fn_v5_intent_eb_delete is called once)"),
        "ordinary_pk_preserved": (
            "the adoption fixture does not assert that the ordinary queue "
            "primary key/identity is preserved"),
        "cross_queue_identity_conflict": (
            "the conflict lives in w402_cross_queue_identity/1, not in the "
            "queue-adoption block, and has no per-scenario unit"),
    }

    def gaps(leaf):
        return _w402_common(f, leaf, test, [note[leaf["ordering_or_fault"]]],
                            "queue-adoption is asserted inside w402_queue_adoption/1")

    return gaps, ["enterprise tenant 9101/9102 is seeded; no entrypoint HTTP call"]


def model_last_attempt_crash(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "kill_attempt8_then_reap": (
            "w402_last_attempt_crash/1 seeds attempts=7, claims to 8 and "
            "acks false to blocked; it never kills a worker or runs the reaper"),
        "attempt9_claim_rejected": (
            "after blocked the suite asserts v5_claim returns empty; it never "
            "creates an attempt-9 claim or records its rejection"),
        "restart_preserves_attempts": (
            "no process restart occurs; the row is inspected in the same "
            "connection"),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise queue fixture")
        return _w402_common(f, leaf, test, extra,
                            "last-attempt is asserted inside w402_last_attempt_crash/1")

    return gaps, ["attempt-8 kill/reap is simulated, not executed"]


def model_old_attempts_preserved(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "legacy_nonzero_attempts": (
            "w402_old_attempts_preserved/1 seeds attempts=5, claims to 6 and "
            "acks false to retry_wait; partial but aggregate"),
        "legacy_attempts_above8": "no fixture seeds attempts greater than 8",
        "legacy_done": "no legacy done-row fixture",
        "nonzero_proof_exception_rejected": (
            "no non-zero proof exception fixture"),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise queue fixture")
        return _w402_common(f, leaf, test, extra,
                            "old-attempts is asserted inside w402_old_attempts_preserved/1")

    return gaps, ["only legacy attempts=5->6 is exercised"]


def model_cross_queue_identity(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "simultaneous_domain_producers": (
            "w402_cross_queue_identity/1 inserts an enterprise queue row first "
            "and then calls the ordinary intent on one connection; there is no "
            "simultaneous producer/concurrency barrier"),
        "unknown_shared_history": (
            "no unknown/shared-history fixture exists; only a known "
            "(ns,container,key) duplicate is covered"),
    }

    def gaps(leaf):
        return _w402_common(f, leaf, test,
                            [note[leaf["ordering_or_fault"]]],
                            "cross-queue identity is asserted inside "
                            "w402_cross_queue_identity/1")

    return gaps, ["cross_domain is represented by one sequential DB unit"]


def model_ordinary_queue_recovery(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "pending_recovery": (
            "w402_ordinary_queue_recovery/1 covers retry_wait->claim->ack(false)"
            "->retry_wait->claim->ack(true)->done; partial but aggregate"),
        "orphan_recovery": "no orphan recovery fixture exists",
        "confirmed_fk_preserved": (
            "no confirmed foreign-key/reference preservation assertion exists"),
        "done_path_reuse_rejected": (
            "the done row is read once; no second claim/reuse attempt asserts "
            "rejection"),
    }

    def gaps(leaf):
        return _w402_common(f, leaf, test,
                            [note[leaf["ordering_or_fault"]]],
                            "ordinary recovery is asserted inside "
                            "w402_ordinary_queue_recovery/1")

    return gaps, ["recovery is DB CAS only; no real garage drain"]


def model_claim_capacity_and_liveness(f):
    test = f.test("attachment_lifecycle_v5_w402_worker_tests", func="run")
    note = {
        "claim_capacity": (
            "w402_claim_capacity_and_liveness/1 claims three sequential ready "
            "jobs and then empty; partial but aggregate"),
        "independent_queue_quota": (
            "no independent ordinary/enterprise queue quota fixture"),
        "finite_backlog_deadline": (
            "no backlog deadline/timing measurement is recorded"),
        "final_lease_reaper_deadline": (
            "no lease-reaper deadline/timing measurement is recorded"),
    }

    def gaps(leaf):
        extra = [note[leaf["ordering_or_fault"]]]
        if leaf["domain"] == "enterprise":
            extra.append("domain=enterprise: no enterprise queue fixture")
        return _w402_common(f, leaf, test, extra,
                            "claim capacity is asserted inside "
                            "w402_claim_capacity_and_liveness/1")

    return gaps, [
        "per-unit durations in eunit-w402.log are test wall times, not lease "
        "deadlines; no timing oracle is emitted",
    ]


def model_purged_session_recovery(f):
    test = f.test("attachment_lifecycle_v5_w402_purged_recovery_tests", func="run")
    src_rel = "test/repo/attachment_lifecycle_v5_w402_purged_recovery_tests.erl"
    _path, text = f.src(src_rel)

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        missing = [
            S.per_leaf_gap(test, "purged recovery aggregate run/0"),
            "entrypoint=%s: the suite calls fn_v5_* directly; no seat/widget "
            "HTTP entrypoint request or response is recorded"
            % leaf["entrypoint"],
            "driver=garage: tombstone copying is a JSONB queue write; no real "
            "garage object operation or namespace stat exists",
        ]
        mapped = PURGED_SCENARIOS.get(scenario)
        if mapped:
            fn, what = mapped
            line = S.line_of(text, fn + "(Conn) ->") or 0
            missing.append(
                "%r is only partially represented by %s/1 (%s:%d): %s; eunit "
                "reports the surrounding aggregate run/0, not this scenario"
                % (scenario, fn, src_rel, line, what))
        else:
            missing.append(
                "no fixture or assertion for scenario %r in %s (%d aggregate "
                "functions exist: %s)"
                % (scenario, src_rel, len(PURGED_SCENARIOS),
                   ", ".join(sorted({v[0] for v in PURGED_SCENARIOS.values()}))))
        missing.append(_oracle())
        return missing

    return gaps, [
        "contract: 10 scenarios x 2 entrypoints = 20 units; suite has 4 "
        "aggregate functions and no entrypoint dimension",
    ]

W402_MODELS = {
    "W4-02/worker-crash": model_worker_crash,
    "W4-02/worker-lease": model_worker_lease,
    "W4-02/worker-db-fault": model_worker_db_fault,
    "W4-02/queue-adoption": model_queue_adoption,
    "W4-02/last-attempt-crash": model_last_attempt_crash,
    "W4-02/old-attempts-preserved": model_old_attempts_preserved,
    "W4-02/cross-queue-identity": model_cross_queue_identity,
    "W4-02/ordinary-queue-recovery": model_ordinary_queue_recovery,
    "W4-02/claim-capacity-and-liveness": model_claim_capacity_and_liveness,
    "W4-02/purged-session-recovery": model_purged_session_recovery,
}
