#!/usr/bin/env python3
"""W4-02/queue-adoption adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), the parsed eunit run line +
suite summary, the suite source emit check and the real scratch pool target.
queue-adoption is the enterprise-face single-domain case (4 contract leaves, domain=enterprise).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-02/queue-adoption"
LOG_NAMES = ("eunit-w402.log",)
MARKER = "W402_DIRECT_OK"
MODULE = 'attachment_lifecycle_v5_w402_worker_tests'
SRC_REL = 'test/repo/attachment_lifecycle_v5_w402_worker_tests.erl'
SCENARIO_TEST = 'w402_emit'

ORACLE = {'cross_queue_identity_conflict': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one blocked legacy_location_unverified adoption row plus ready rows', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> cross_queue_identity_conflict cell: the blocked legacy row is not claimable (adoption gate) while completed ready work flows; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'blocked legacy row untouched by claims (blocked_legacy_not_claimed=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'enterprise_pending_adoption': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one enterprise_asset deleted row with pending_object_delete=true', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> enterprise_pending_adoption cell: fn_v5_intent_eb_delete CASes the legacy pending flag into one enterprise queue job; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'exactly 1 enterprise_asset_delete_queue row in (blocked,ready) for the asset (queue_rows=1)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'enterprise_intent_replay': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one enterprise_asset whose first intent already flipped it to delete_intent', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> enterprise_intent_replay cell: the replay intent on the non-live asset is refused 23510 and the queue keeps exactly one row; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'replay rejected 23510 (replay_rejected); queue row count stays 1 (idempotent_replay=true)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}, 'ordinary_pk_preserved': {'pre_db_state': 'attv5_<slot>_w201 marker DB (full 227-file migration replay); one ordinary queue row whose path PK would collide with an enterprise intent on the same key', 'pre_object_hash': 'n/a (worker-protocol leaf; the cell adjudicates queue/state transitions, not object bytes)', 'action': 'direct erl eunit -> ordinary_pk_preserved cell: the ordinary path primary key is not rewritten by the enterprise intent; the ordinary row keeps its identity and state; emitted after asserts', 'expected_http_semantics': 'no HTTP surface; the queue state machine owns the outcome', 'expected_db_delta': 'ordinary row untouched with its path PK intact (untouched=true, state=ready)', 'expected_object_state': 'garage probe identity unchanged; the protocol cell adjudicates queue/state transitions, not object bytes', 'proof_actor': 'epgsql marker-DB connections on attv5_<slot>_w201 marker DB (full 227-file migration replay) fixtures; production fn_v5_* worker-protocol entrypoints'}}


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
