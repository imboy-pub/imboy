#!/usr/bin/env python3
"""W4-01/reference-gc-race adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts), parsed eunit module line +
suite summary, suite source emit check, real scratch pool target.
source_kind is real-required: leaf_observation_row binds actor kind=real
on the app pool the suite actually ran on.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-01/reference-gc-race"
LOG_NAMES = ("eunit-w401.log", "raw.log")
MARKER = "W401_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

ORACLE = {
    "bind_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                        "V5_INTTEST_W301_*; confirmed unbound group asset "
                        "(ordinary) / enterprise_asset (enterprise); binding "
                        "transaction commits first",
        "pre_object_hash": "n/a (reference/GC protocol leaf; GC eligibility "
                           "adjudicates row state, not object bytes)",
        "action": "direct erl eunit -> w301 battery scenario: binding commits, "
                  "then fn_v5_intent_ordinary_delete / fn_v5_intent_eb_delete "
                  "runs against the bound asset; measured values emitted to "
                  "the observation stream after the scenario asserts",
        "expected_http_semantics": "no HTTP surface; intent must be rejected "
                                   "with the 23510 tombstone family after the "
                                   "reference commits",
        "expected_db_delta": "intent rejected 23510; lifecycle_state stays "
                             "live; no attachment_object_deletion row; the "
                             "committed binding survives",
        "expected_object_state": "referenced bytes retained; no GC deletion "
                                 "task owns the object",
        "proof_actor": "epgsql marker-DB connections (main + second "
                       "connection) on attv5_<slot>_pool fixtures; production "
                       "repo entrypoints exercised",
    },
    "intent_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; unbound "
                        "confirmed asset; delete intent commits first "
                        "(tombstone active)",
        "pre_object_hash": "n/a (reference/GC protocol leaf)",
        "action": "direct erl eunit -> w301 battery scenario: intent commits "
                  "first, then direct DML insert and the production binding "
                  "entrypoint run against the tombstoned asset",
        "expected_http_semantics": "no HTTP surface; bind must be rejected "
                                   "with the same refusal shape as absent "
                                   "assets",
        "expected_db_delta": "bind transaction changes zero business rows "
                             "(23510 / tombstone); no metadata or reference "
                             "is recreated",
        "expected_object_state": "the intent owns the object for bounded GC "
                                 "deletion; bind cannot resurrect metadata",
        "proof_actor": "epgsql marker-DB connections on attv5_<slot>_pool "
                       "fixtures; production fn_v5_intent_* entrypoints",
    },
    "direct_dml_bind_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; unbound "
                        "confirmed asset; the constrained direct DML binding "
                        "(UPDATE enterprise_asset SET message_id / group_file "
                        "insert family) commits first",
        "pre_object_hash": "n/a (reference/GC protocol leaf)",
        "action": "direct erl eunit -> w301 battery scenario: direct DML "
                  "binding commits, then the intent entrypoint runs",
        "expected_http_semantics": "no HTTP surface; the lifecycle guard "
                                   "(guards-contract trigger family) rejects "
                                   "the intent with 23510",
        "expected_db_delta": "intent rejected 23510; the directly-bound "
                             "reference survives; no deletion queue row",
        "expected_object_state": "referenced bytes retained",
        "proof_actor": "epgsql marker-DB connections on attv5_<slot>_pool "
                       "fixtures; guards-contract phase exercised through the "
                       "schema triggers (no runtime role bypass)",
    },
    "direct_dml_intent_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; unbound "
                        "confirmed asset; intent commits first (delete_intent "
                        "lifecycle active)",
        "pre_object_hash": "n/a (reference/GC protocol leaf)",
        "action": "direct erl eunit -> w301 battery scenario: intent commits, "
                  "then the constrained direct DML binding runs",
        "expected_http_semantics": "no HTTP surface; the guard refuses new "
                                   "references on delete_intent rows",
        "expected_db_delta": "direct DML binding changes zero business rows "
                             "(0 updated / 23510 refusal); lifecycle stays "
                             "delete_intent",
        "expected_object_state": "the intent owns the object for bounded GC "
                                 "deletion",
        "proof_actor": "epgsql marker-DB connections on attv5_<slot>_pool "
                       "fixtures; guards-contract phase exercised through the "
                       "schema triggers (no runtime role bypass)",
    },
}


SCENARIO_FN = {
    ("bind_commits_first", "ordinary"): "w301_reference_gc_race",
    ("intent_commits_first", "ordinary"): "w301_reference_gc_race",
    ("direct_dml_bind_commits_first", "ordinary"): "w301_reference_gc_race",
    ("direct_dml_intent_commits_first", "ordinary"): "w301_reference_gc_race",
    ("bind_commits_first", "enterprise"): "w301_enterprise_direct_race",
    ("intent_commits_first", "enterprise"): "w301_enterprise_direct_race",
    ("direct_dml_bind_commits_first", "enterprise"):
        "w301_direct_enterprise_message_after_intent",
    ("direct_dml_intent_commits_first", "enterprise"):
        "w301_direct_enterprise_message_after_intent",
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
        test = support.named_test(parsed, "w301_run", CASE)
        if test["module"] != MODULE:
            support.die(CASE, "w301 battery ran in %r, expected %r"
                        % (test["module"], MODULE))
        scenario_fn = SCENARIO_FN.get(
            (leaf["ordering_or_fault"], leaf["domain"]))
        if scenario_fn is None:
            support.die(CASE, "no suite function mapping for leaf %r/%r"
                        % (leaf["ordering_or_fault"], leaf["domain"]))
        obs = support.matching_observation(entries, leaf, CASE)
        oracle = dict(scenario)
        pool = support.pool_resource(ctx, CASE)[0]
        if leaf["domain"] == "enterprise":
            oracle["pre_db_state"] += "; enterprise domain row " \
                                      "(enterprise_asset / scope_ref frozen " \
                                      "at registration)"
        oracle = {key: value.replace("attv5_<slot>_pool", pool)
                  for key, value in oracle.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test=scenario_fn,
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
