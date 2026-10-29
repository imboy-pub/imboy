#!/usr/bin/env python3
"""W4-02/old-attempts-preserved adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), the parsed eunit run line +
suite summary, the suite source emit check and the real scratch pool target.
source_kind is real-required: leaf_observation_row binds actor kind=real on the app pool the suite actually ran on.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-02/old-attempts-preserved"
LOG_NAMES = ("eunit-w402.log",)
MARKER = "W402_DIRECT_OK"
MODULE = 'attachment_lifecycle_v5_w402_worker_tests'
SRC_REL = 'test/repo/attachment_lifecycle_v5_w402_worker_tests.erl'
SCENARIO_TEST = 'w402_emit'

ORACLE = {'legacy_nonzero_attempts': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one legacy adoption row seeded at attempts=5', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> legacy_nonzero_attempts cell: claim advances attempts 5->6, ack(false) lands retry_wait with attempts=6 and last_error_code=delete_failed (history not reset); emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'attempts_after=6 with retry_wait/delete_failed; legacy attempt history preserved (not_reset=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'legacy_done': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one done terminal row at attempts=5', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> legacy_done cell: the done row keeps its final state and attempts history and is never claimed again; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'row stays done with attempts=5 (stays_done=true); claim skips terminal rows', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'legacy_attempts_above8': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one legacy row seeded at attempts=9 ready', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> legacy_attempts_above8 cell: the claim gate (attempts - attempt_base < 8) refuses to adopt the row, claim returns empty and the row keeps attempts=9/ready unchanged; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'claim empty; row unchanged at attempts=9/ready (attempts_above8_preserved=9, unchanged=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'nonzero_proof_exception_rejected': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one legacy blocked row at attempts=3 (legacy_location_unverified)', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> nonzero_proof_exception_rejected cell: the zero-attempt proof exception is not applicable to a nonzero blocked row; claim does not pick it and the row stays blocked/3; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'row keeps blocked with attempts=3 (blocked_attempts_unchanged=3, claimed=false)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}}


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
