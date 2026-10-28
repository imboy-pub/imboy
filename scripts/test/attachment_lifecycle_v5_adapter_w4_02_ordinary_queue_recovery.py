#!/usr/bin/env python3
"""W4-02/ordinary-queue-recovery adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), the parsed eunit run line +
suite summary, the suite source emit check and the real scratch pool target.
ordinary-queue-recovery is the ordinary-domain single-face case (4 contract leaves, domain=ordinary).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-02/ordinary-queue-recovery"
LOG_NAMES = ("eunit-w402.log",)
MARKER = "W402_DIRECT_OK"
MODULE = 'attachment_lifecycle_v5_w402_worker_tests'
SRC_REL = 'test/repo/attachment_lifecycle_v5_w402_worker_tests.erl'
SCENARIO_TEST = 'w402_emit'

ORACLE = {'pending_recovery': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); a registered attach_pending session (capability already expired)', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> pending_recovery cell: fn_v5_intent_pending_delete(mode=sweep) adopts the existing queue: one ready attachment_object_deletion row for the pending key; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'exactly 1 queue row created from the pending session (existing_queue_used=true, queue_rows=1)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'orphan_recovery': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one gc_verified attachment row without any binding (orphan)', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> orphan_recovery cell: fn_v5_intent_ordinary_delete enqueues the orphan into the existing ordinary queue; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'exactly 1 queue row created for the orphan path (existing_queue_used=true, queue_rows=1)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'confirmed_fk_preserved': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one confirmed attachment row bound to group_file (group_file_id FK set, scope=group)', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> confirmed_fk_preserved cell: fn_v5_intent_ordinary_delete on the bound row is rejected 23510 and the metadata FK binding survives the recovery window; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'intent rejected 23510 (intent_rejected); the attachment->group_file binding stays intact (fk_intact=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'done_path_reuse_rejected': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one row consumed to the done tombstone', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> done_path_reuse_rejected cell: a new delete intent on the done path is refused by the tombstone and the job row keeps its terminal identity; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'reuse intent refused (reuse_rejected=true); done row stays done', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)
    entries = support.read_observations(case_dir, CASE)
    pool = support.pool_resource(ctx, CASE)[0]
    test = support.named_test_in_module(parsed, "run", MODULE, CASE)
    rows = []
    for leaf in leaves:
        scenario = ORACLE.get(leaf["ordering_or_fault"])
        if scenario is None:
            support.die(CASE, "no evidence definition for leaf scenario %r"
                        % leaf["ordering_or_fault"])
        obs = support.matching_observation(entries, leaf, CASE)
        oracle = {key: value.replace("attv5_<slot>_w201", pool)
                  for key, value in scenario.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test=SCENARIO_TEST,
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
