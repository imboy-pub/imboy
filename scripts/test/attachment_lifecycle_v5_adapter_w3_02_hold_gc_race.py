#!/usr/bin/env python3
"""W3-02/hold-gc-race adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each ordering's asserts, ordinary and enterprise
orderings independently), parsed eunit run line + suite summary, suite
source emit check, real scratch pool target.  source_kind is
real-required: leaf_observation_row binds actor kind=real.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-02/hold-gc-race"
LOG_NAMES = ("eunit-w302.log", "raw.log")
MARKER = "W302_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_hold_protocol_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_hold_protocol_tests.erl"

ORACLE = {
    "hold_commits_first": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                        "V5_INTTEST_W302_* (%s); confirmed unbound "
                        "plaintext-eligible attachment (ordinary: group "
                        "scope; enterprise: enterprise scope with EAR row "
                        "installed through the production governance_tx "
                        "hold entrypoint)" % "attv5_<slot>_pool",
        "pre_object_hash": "n/a (hold-race protocol leaf; the race "
                           "adjudicates row eligibility, not object bytes)",
        "action": "direct erl eunit -> hold-gc-race ordering: hold commits "
                  "first, then fn_v5_intent_ordinary_delete runs; measured "
                  "values emitted to the observation stream after the "
                  "ordering asserts",
        "expected_http_semantics": "no HTTP surface; the intent must be "
                                   "rejected with 23510 while the hold is in "
                                   "place (EAR held / WAR workspace hold)",
        "expected_db_delta": "intent rejected 23510; the attachment row "
                             "survives; no deletion queue row",
        "expected_object_state": "held bytes retained; no GC deletion task "
                                 "owns the object",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production intent entrypoint plus the "
                       "production governance_tx hold (enterprise leg)",
    },
    "intent_before_asset_hold": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "plaintext-eligible attachment whose delete intent "
                        "already committed (lifecycle_state=delete_intent)",
        "pre_object_hash": "n/a (hold-race protocol leaf)",
        "action": "direct erl eunit -> hold-gc-race ordering: intent commits "
                  "first, then the per-asset hold operation runs (enterprise: "
                  "production governance_tx hold; ordinary: production repo "
                  "hold on the EAR row)",
        "expected_http_semantics": "no HTTP surface; governance must not "
                                   "ratify an object already in the deletion "
                                   "flow — enterprise returns the explicit "
                                   "already_deleting refusal, the ordinary "
                                   "protocol face keeps the object excluded "
                                   "from hold adoption",
        "expected_db_delta": "lifecycle stays delete_intent; no resurrection "
                             "of metadata or references",
        "expected_object_state": "the intent owns the object for bounded GC "
                                 "deletion",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production governance_tx / intent "
                       "entrypoints",
    },
    "intent_before_retention_extension": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "plaintext-eligible attachment whose delete intent "
                        "already committed",
        "pre_object_hash": "n/a (hold-race protocol leaf)",
        "action": "direct erl eunit -> hold-gc-race ordering: intent commits "
                  "first, then the retention extension runs (enterprise: "
                  "production governance_tx set_retention; ordinary: "
                  "production repo extend on the EAR row)",
        "expected_http_semantics": "no HTTP surface; retention extension must "
                                   "not ratify an object already in the "
                                   "deletion flow — enterprise returns "
                                   "already_deleting, the ordinary protocol "
                                   "face keeps the lifecycle at "
                                   "delete_intent",
        "expected_db_delta": "lifecycle stays delete_intent; the extension "
                             "never revives the object",
        "expected_object_state": "the intent owns the object for bounded GC "
                                 "deletion",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production governance_tx / repo "
                       "entrypoints",
    },
    "workspace_hold_exclusion_receipt": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "plaintext-eligible attachment with a workspace-level "
                        "hold row in place (ordinary WAR; enterprise EAR via "
                        "production governance_tx)",
        "pre_object_hash": "n/a (hold-race protocol leaf)",
        "action": "direct erl eunit -> hold-gc-race ordering: the workspace "
                  "hold receipt explicitly excludes the object — intent "
                  "rejected with 23510 while held (and, on the enterprise "
                  "leg, still excluded by the retention window after "
                  "release_hold)",
        "expected_http_semantics": "no HTTP surface; the hold/retention "
                                   "receipt excludes the deleted object from "
                                   "adoption explicitly and persistently",
        "expected_db_delta": "intent rejected 23510; no deletion queue row; "
                             "the attachment row survives",
        "expected_object_state": "held bytes retained; the exclusion is "
                                 "explicit and persistent",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production intent + governance_tx "
                       "entrypoints",
    },
}

SCENARIO_FN = {
    ("hold_commits_first", "ordinary"): "w302_hold_gc_race",
    ("intent_before_asset_hold", "ordinary"): "w302_hold_gc_race",
    ("intent_before_retention_extension", "ordinary"): "w302_hold_gc_race",
    ("workspace_hold_exclusion_receipt", "ordinary"): "w302_hold_gc_race",
    ("hold_commits_first", "enterprise"): "w302_hold_gc_race",
    ("intent_before_asset_hold", "enterprise"): "w302_hold_gc_race",
    ("intent_before_retention_extension", "enterprise"): "w302_hold_gc_race",
    ("workspace_hold_exclusion_receipt", "enterprise"): "w302_hold_gc_race",
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
        test = support.named_test(parsed, "run", CASE)
        if test["module"] != MODULE:
            support.die(CASE, "hold suite ran in %r, expected %r"
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
