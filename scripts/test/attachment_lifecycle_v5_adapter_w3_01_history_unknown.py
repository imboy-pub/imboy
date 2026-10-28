#!/usr/bin/env python3
"""W3-01/history-unknown adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts, ordinary and enterprise
legs independently), parsed eunit module line + suite summary, suite
source emit check, real scratch pool target.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-01/history-unknown"
LOG_NAMES = ("eunit-w203.log", "eunit-w301.log", "raw.log")
MARKER = "W301_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

ORACLE = {
    "unknown_location": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                        "V5_INTTEST_W301_*; confirmed attachment whose "
                        "storage locator tuple is NULL (ordinary group row / "
                        "enterprise scope='enterprise' row) — the history "
                        "cannot be located",
        "pre_object_hash": "original file_hash256 recorded before the probe "
                           "(history-unknown keeps the hash unchanged)",
        "action": "direct erl eunit -> w301 battery scenario: gc_verified "
                  "refresh probe then the intent entrypoint; measured values "
                  "emitted to the observation stream after the scenario "
                  "asserts",
        "expected_http_semantics": "no HTTP surface; an asset whose location "
                                   "is unknown is never eligible for GC "
                                   "deletion",
        "expected_db_delta": "gc_verified stays false; intent rejected 23510; "
                             "zero attachment_object_deletion rows",
        "expected_object_state": "bytes untouched; original hash unchanged",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production refresh/intent entrypoints",
    },
    "opaque_reference": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "attachment with cipher='AES-256-GCM' (ordinary group "
                        "row / enterprise scope row) — the object mapping is "
                        "unresolvable",
        "pre_object_hash": "n/a (hash unchanged by protocol; recorded in the "
                           "observation stream where asserted)",
        "action": "direct erl eunit -> w301 battery scenario: gc_verified "
                  "refresh probe on the opaque (ciphered) reference",
        "expected_http_semantics": "no HTTP surface; opaque references are "
                                   "never GC-eligible",
        "expected_db_delta": "gc_verified stays false; zero "
                             "attachment_object_deletion rows",
        "expected_object_state": "ciphered bytes untouched",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production refresh entrypoint",
    },
    "ambiguous_reference": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; confirmed "
                        "attachment whose file_hash256 matches an active "
                        "user_collect row (ordinary group row / enterprise "
                        "scope row)",
        "pre_object_hash": "n/a (hash association is the ambiguity under "
                           "test)",
        "action": "direct erl eunit -> w301 battery scenario: gc_verified "
                  "refresh probe with the active collection association",
        "expected_http_semantics": "no HTTP surface; ambiguous references are "
                                   "never GC-eligible",
        "expected_db_delta": "gc_verified stays false; zero deletion queue "
                             "rows",
        "expected_object_state": "bytes untouched",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production refresh entrypoint",
    },
}

SCENARIO_FN = {
    ("unknown_location", "ordinary"): "w301_history_unknown",
    ("opaque_reference", "ordinary"): "w301_history_unknown",
    ("ambiguous_reference", "ordinary"): "w301_history_unknown",
    ("unknown_location", "enterprise"): "w301_history_unknown_enterprise",
    ("opaque_reference", "enterprise"): "w301_history_unknown_enterprise",
    ("ambiguous_reference", "enterprise"): "w301_history_unknown_enterprise",
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
        oracle = {key: value.replace("attv5_<slot>_pool", pool)
                  for key, value in oracle.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test=scenario_fn,
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
