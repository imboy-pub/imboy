#!/usr/bin/env python3
"""W4-02/last-attempt-crash adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), the parsed eunit run line +
suite summary, the suite source emit check and the real scratch pool target.
source_kind is real-required: leaf_observation_row binds actor kind=real on the app pool the suite actually ran on.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-02/last-attempt-crash"
LOG_NAMES = ("eunit-w402.log",)
MARKER = "W402_DIRECT_OK"
MODULE = 'attachment_lifecycle_v5_w402_worker_tests'
SRC_REL = 'test/repo/attachment_lifecycle_v5_w402_worker_tests.erl'
SCENARIO_TEST = 'w402_emit'

ORACLE = {'attempt9_claim_rejected': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one row seeded at attempts=7', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> attempt9_claim_rejected cell: claim lifts attempts 7->8, ack(false) blocks the row as max_attempts and the ninth claim returns empty; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'blocked + last_error_code=max_attempts after ack(false); ninth claim empty (blocked_after_ack_false=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'kill_attempt8_then_reap': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one attempts=8 leased row whose worker died', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> kill_attempt8_then_reap cell: the reaper moves the expired attempts-8 lease straight to blocked, preserving attempts; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'leased -> blocked with attempts=8 kept (reaped_to_blocked=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'restart_preserves_attempts': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one legacy row at attempts=5 driven through the reclaim chain', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> restart_preserves_attempts cell: restart/reclaim cycles advance attempts monotonically 5->6->7 without any reset; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'attempts_monotonic=[5,6,7]; no attempts reset across the restart chain', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}}


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
