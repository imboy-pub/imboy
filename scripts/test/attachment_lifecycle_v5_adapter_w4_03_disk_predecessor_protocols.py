#!/usr/bin/env python3
"""W4-03/disk-predecessor-protocols adapter: emit 66 disk-driver leaf rows.

Each leaf is sealed from its own suite observation (case/entrypoint/scenario/
domain + measured.driver), the parsed named eunit unit, the clean three-suite
summary, the run-owned disk root and the suite source emit check.  The 66
leaves replay the W4-01 intent-rollback and W4-02 worker/queue protocols on the
real disk driver; the adapter never relabels an aggregate unit as a leaf.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w4_03_support as S  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as W4  # noqa: E402

CASE = "W4-03/disk-predecessor-protocols"
LOG_NAMES = ("eunit-w403.log",)
MARKER = "W403_DIRECT_OK"
MODULES = [
    "attachment_lifecycle_v5_w403_storage_tests",
    "attachment_lifecycle_v5_w403_driver_matrix_tests",
    "attachment_lifecycle_v5_w403_predecessor_tests",
]
MODULE = "attachment_lifecycle_v5_w403_predecessor_tests"
SRC_REL = "test/lib/attachment_lifecycle_v5_w403_predecessor_tests.erl"

INTENT_TEXT = (
    "no HTTP surface: the delete intent is refused inside the entrypoint's "
    "transaction; the entrypoint returns an error shape and never reports "
    "success, and no storage delete call is reached"
)

HTTP_TEXT = {
    "W4-01/intent-rollback/intent_insert_error": INTENT_TEXT,
    "W4-01/intent-rollback/audit_insert_error": INTENT_TEXT,
    "W4-01/intent-rollback/sql_rollback": INTENT_TEXT,
    "W4-01/intent-rollback/commit_response_lost": INTENT_TEXT,
    "W4-02/worker-crash/kill_before_claim_commit": (
        "no HTTP surface: a worker killed before its claim commits leaves the "
        "queue row ready with zero attempts and the object bytes untouched"
    ),
    "W4-02/worker-crash/kill_after_claim_before_delete": (
        "no HTTP surface: a worker killed after claim but before delete is "
        "recovered by the lease reaper and re-claimed on the same exact "
        "location; the object bytes are still present"
    ),
    "W4-02/worker-crash/kill_after_delete_before_completion": (
        "no HTTP surface: a worker killed after the physical delete but before "
        "completion is re-claimed after reaping and resolves the job to done "
        "without resurrecting the object"
    ),
    "W4-02/worker-db-fault/db_lost_before_claim_verified": (
        "no HTTP surface: a stale token and a missing job both update zero "
        "rows and the worker does not crash; the object bytes are untouched"
    ),
    "W4-02/worker-db-fault/db_lost_after_delete": (
        "no HTTP surface: the completion receipt is lost after the delete; "
        "reaping and re-claiming resolve the same exact location to done"
    ),
    "W4-02/queue-adoption/enterprise_pending_adoption": (
        "no HTTP surface: an enterprise asset already flagged for deletion is "
        "adopted into exactly one queue row by the intent"
    ),
    "W4-02/queue-adoption/enterprise_intent_replay": (
        "no HTTP surface: replaying the enterprise delete intent is refused "
        "(23510, tombstone) and the queue keeps exactly one row"
    ),
    "W4-02/queue-adoption/ordinary_pk_preserved": (
        "no HTTP surface: the ordinary queue row keeps its path primary key "
        "and state; the enterprise path never rewrites it"
    ),
    "W4-02/queue-adoption/cross_queue_identity_conflict": (
        "no HTTP surface: a legacy blocked row without verified location is "
        "never claimed by the worker"
    ),
    "W4-02/ordinary-queue-recovery/pending_recovery": (
        "no HTTP surface: an expired pending session is swept into the "
        "existing ordinary queue (exactly one row)"
    ),
    "W4-02/ordinary-queue-recovery/orphan_recovery": (
        "no HTTP surface: an orphan attachment with verified location is "
        "queued through the existing ordinary queue"
    ),
    "W4-02/ordinary-queue-recovery/confirmed_fk_preserved": (
        "no HTTP surface: a confirmed group binding is refused (23510) and "
        "the FK and object bytes stay intact"
    ),
    "W4-02/ordinary-queue-recovery/done_path_reuse_rejected": (
        "no HTTP surface: after done the path is a tombstone and a new intent "
        "is rejected; the object is not resurrected"
    ),
}


def predecessor_oracle(leaf, obs, ctx, pool, bucket, disk_root, endpoint):
    """Measured oracle row for one disk-predecessor leaf."""
    measured = obs.get("measured") or {}
    scenario = leaf["ordering_or_fault"]
    key = measured.get("object_key", "?")
    digest = measured.get("probe_sha256") or "n/a"
    resource = "real disk driver rooted at %s (V5_DISK_ROOT, allowlisted)" % disk_root
    target = "disk object key %s in namespace %s (container %s)" % (
        key, measured.get("namespace", "?"), measured.get("container", "?"))

    if scenario.startswith("W4-01/intent-rollback/"):
        db_delta = (
            "no queue row is created for the probe key (queue_rows=%s): the "
            "failing delete intent never reaches the queue; the only DB writes "
            "are the injected repo-layer failure shapes recorded in measured"
            % measured.get("queue_rows", "0"))
        object_state = (
            "the probe bytes under %s are unchanged (sha256 %s) and the disk "
            "root file listing is identical before and after the entrypoint "
            "call; zero storage deletes are issued" % (target, digest))
    elif measured.get("bytes_gone") is True:
        db_delta = (
            "the queue row reaches job_state=done for the same path "
            "(attempts recorded in measured: %s)"
            % measured.get("resolved_attempts", "n/a"))
        object_state = (
            "the object at %s is physically gone after the worker delete "
            "(file read returns enoent); no resurrected or partial object "
            "remains" % target)
    elif scenario.startswith("W4-02/queue-adoption/"):
        db_delta = (
            "the enterprise/ordinary queue rows keep the exact identity "
            "recorded in measured (queue_rows=%s, untouched=%s)"
            % (measured.get("queue_rows", measured.get("queue_rows_after", "?")),
               measured.get("untouched", "n/a")))
        object_state = (
            "the object at %s is preserved untouched (sha256 %s); the leaf "
            "adjudicates queue identity, not object bytes" % (target, digest))
    elif scenario.startswith("W4-02/ordinary-queue-recovery/"):
        db_delta = (
            "the existing ordinary queue is used (queue_rows=%s); a confirmed "
            "binding or done tombstone is refused with 23510 as recorded"
            % measured.get("queue_rows", measured.get("queue_rows_after", "?")))
        object_state = (
            "the object at %s is preserved (sha256 %s) for recovery leaves; "
            "done-tombstone leaves assert the bytes are gone" % (target, digest))
    else:
        db_delta = (
            "the queue row state machine matches measured (attempts=%s, "
            "recovered=%s)"
            % (measured.get("attempts_after", measured.get("resolved_attempts", "n/a")),
               measured.get("recovered_after_reap",
                            measured.get("stale_ack_zero_rows", "n/a"))))
        object_state = (
            "the object at %s is preserved with sha256 %s; the crash/fault "
            "window never deletes bytes it has not verified" % (target, digest))

    oracle = {
        "pre_db_state": (
            "scratch pool %s (run-owned marker DB) holds the seeded queue/"
            "attachment/enterprise rows for this leaf; the intent-rollback "
            "leaves seed their preconditions through the source oracle's "
            "repo-layer expectations, the W4-02 leaves seed real queue rows "
            "with storage_driver=disk and the same locator digest" % pool),
        "pre_object_hash": (
            "sha256=%s of the probe bytes written to %s through the real disk "
            "driver before the entrypoint/worker call" % (digest, target)),
        "action": (
            "direct erl eunit unit %r -> %s; measured=%s"
            % (S.description(leaf["_case"], leaf), resource, S.measured_detail(obs))),
        "expected_http_semantics": HTTP_TEXT[scenario],
        "expected_db_delta": db_delta,
        "expected_object_state": object_state,
        "proof_actor": (
            "%s; the suite appends the observation from the same eunit VM "
            "after the asserts (emitted_at_ms=%s) and the adapter matches "
            "case/entrypoint/scenario/domain/driver uniquely"
            % (resource, obs.get("emitted_at_ms"))),
    }
    assertions = [
        W4.assertion("driver_provenance", resource),
        W4.assertion("measured_object_state", S.measured_detail(obs)),
    ]
    return oracle, assertions


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    S.bind(
        CASE,
        run_dir=run_dir,
        case_dir=case_dir,
        root=root,
        log_names=LOG_NAMES,
        marker=MARKER,
        modules=MODULES,
        module=MODULE,
        oracle_fn=predecessor_oracle,
        source_rel=SRC_REL,
    )


if __name__ == "__main__":
    main()
