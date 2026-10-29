#!/usr/bin/env python3
"""W4-01/confirm-gc-race adapter: emits measured leaves for sealing.

The w203 battery inside the W4-01 gate run emits per-ordering measured
values (ordinary leg + an enterprise event that discloses the shared
restricted fn guard).  Each frozen leaf binds to its observation stream
event, the parsed w203_run eunit line, the suite summary, the suite
source emit check and the real scratch pool target.  source_kind is
real-required: actor kind=real on the pool the suite ran on.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-01/confirm-gc-race"
LOG_NAMES = ("eunit-w401.log", "eunit-w203.log", "raw.log")
MARKER = "W401_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

ORACLE = {
    "confirm_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                        "V5_INTTEST_W203_* (%s); registered+confirmed asset "
                        "with an open capability window; a real second "
                        "connection holds the attachment row lock while the "
                        "confirm replays" % "attv5_<slot>_pool",
        "pre_object_hash": "n/a (confirm/intent commit-order protocol leaf; "
                           "the barrier adjudicates row state, not object "
                           "bytes)",
        "action": "direct erl eunit -> w203 battery ordering: confirm commits "
                  "first behind a real FOR UPDATE row lock barrier, then the "
                  "intent entrypoint runs and the confirm replay is retried "
                  "within bounds",
        "expected_http_semantics": "no HTTP surface; restricted "
                                   "fn_v5_confirm_pending / "
                                   "fn_v5_intent_ordinary_delete protocol",
        "expected_db_delta": "confirm commits first: intent rejected (23510 "
                             "family), no attachment_object_deletion row is "
                             "created, the confirmed lifecycle_state "
                             "survives, and the bounded retry returns the "
                             "original attachment id",
        "expected_object_state": "confirmed bytes remain; no GC deletion task "
                                 "owns the object",
        "proof_actor": "epgsql marker-DB connections (main + real second "
                       "connection holding the row lock) on "
                       "attv5_<slot>_pool fixtures",
    },
    "intent_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "asset whose capability window has expired; the "
                        "delete intent commits first",
        "pre_object_hash": "n/a (confirm/intent commit-order protocol leaf)",
        "action": "direct erl eunit -> w203 battery ordering: intent commits "
                  "first, then the confirm protocol replays against the "
                  "delete_intent row",
        "expected_http_semantics": "no HTTP surface; confirm replay must fail "
                                   "with 23510 once the intent has committed",
        "expected_db_delta": "the confirm replay fails 23510, "
                             "lifecycle_state stays delete_intent, and no "
                             "metadata/reference is recreated",
        "expected_object_state": "the intent owns the object for bounded GC "
                                 "deletion; confirm cannot recreate metadata",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production fn_v5_intent_ordinary_delete and "
                       "fn_v5_confirm_pending",
    },
}

SCENARIO_FN = {
    ("confirm_commits_first", "ordinary"): "w203_confirm_first",
    ("confirm_commits_first", "enterprise"): "w203_confirm_first",
    ("intent_commits_first", "ordinary"): "w203_intent_first",
    ("intent_commits_first", "enterprise"): "w203_intent_first",
}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)
    entries = support.read_observations(case_dir, CASE)
    rows = []
    for leaf in leaves:
        scenario = ORACLE.get(leaf["ordering_or_fault"])
        if scenario is None:
            support.die(CASE, "no evidence definition for leaf scenario %r"
                        % leaf["ordering_or_fault"])
        test = support.named_test(parsed, "w203_run", CASE)
        if test["module"] != MODULE:
            support.die(CASE, "w203 battery ran in %r, expected %r"
                        % (test["module"], MODULE))
        scenario_fn = SCENARIO_FN.get(
            (leaf["ordering_or_fault"], leaf["domain"]))
        if scenario_fn is None:
            support.die(CASE, "no suite function mapping for leaf %r/%r"
                        % (leaf["ordering_or_fault"], leaf["domain"]))
        obs = support.matching_observation(entries, leaf, CASE)
        oracle = dict(scenario)
        pool = support.pool_resource(ctx, CASE)[0]
        oracle = {key: value.replace("attv5_<slot>_pool", pool)
                  for key, value in oracle.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test=scenario_fn,
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
