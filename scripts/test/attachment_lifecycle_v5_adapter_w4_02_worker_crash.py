#!/usr/bin/env python3
"""W4-02/worker-crash adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), the parsed eunit run line +
suite summary, the suite source emit check and the real scratch pool target.
source_kind is real-required: leaf_observation_row binds actor kind=real on the app pool the suite actually ran on.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-02/worker-crash"
LOG_NAMES = ("eunit-w402.log",)
MARKER = "W402_DIRECT_OK"
MODULE = 'attachment_lifecycle_v5_w402_worker_tests'
SRC_REL = 'test/repo/attachment_lifecycle_v5_w402_worker_tests.erl'
SCENARIO_TEST = 'w402_emit'

ORACLE = {'kill_before_claim_commit': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one ready row; the claim transaction is killed before commit', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> kill_before_claim_commit cell: the simulated pre-commit kill leaves the row untouched (ready, attempts 0) and zero delete calls happen; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'row state stays ready with attempts=0 (zero_delete_calls=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'kill_after_claim_before_delete': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one claimed row whose worker dies before deleting (no ack)', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> kill_after_claim_before_delete cell: expired lease -> reaper returns the job to retry_wait with attempts incremented and a later claim re-runs it; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'attempts 1 -> 2 after re-claim (attempts_after=2); the job is not lost (recovered_after_reap=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'kill_after_delete_before_completion': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one row whose delete already ran but whose completion ack was lost', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> kill_after_delete_before_completion cell: lease expiry then re-claim and ack(true) resolves the job to done; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'job reaches the done terminal state (resolved_to_done=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}}


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
