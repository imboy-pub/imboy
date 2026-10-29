#!/usr/bin/env python3
"""W3-01/shared-owner adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each scenario's asserts, ordinary and enterprise
legs independently), parsed eunit module line + suite summary, suite
source emit check, real scratch pool target.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-01/shared-owner"
LOG_NAMES = ("eunit-w203.log", "eunit-w301.log", "raw.log")
MARKER = "W301_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

ORACLE = {
    "owner_logout": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                        "V5_INTTEST_W301_*; active shared reference "
                        "(ordinary: confirmed group-bound / collected asset; "
                        "enterprise: scope='enterprise' reference row)",
        "pre_object_hash": "n/a (reference-protection protocol leaf; the "
                           "owner-deletion SQL adjudicates row references)",
        "action": "direct erl eunit -> w301 battery scenario: run the real "
                  "account-deletion attachment cleanup SQL against the "
                  "shared reference; measured values emitted to the "
                  "observation stream after the scenario asserts",
        "expected_http_semantics": "no HTTP surface; owner logout cannot "
                                   "delete an active collection/forward/"
                                   "group/enterprise reference",
        "expected_db_delta": "owner-deletion DELETE removes zero rows for the "
                             "shared reference; the attachment row survives",
        "expected_object_state": "referenced bytes retained; no deletion task "
                                 "created",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production deletion SQL shape",
    },
    "owner_account_delete": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; shared "
                        "reference additionally protected by an active "
                        "user_collect hash association (ordinary) or the "
                        "enterprise scope exclusion (enterprise)",
        "pre_object_hash": "n/a (reference-protection protocol leaf)",
        "action": "direct erl eunit -> w301 battery scenario: account-deletion "
                  "cleanup SQL against the protected reference",
        "expected_http_semantics": "no HTTP surface; account deletion cannot "
                                   "delete actively collected/shared "
                                   "references",
        "expected_db_delta": "owner-deletion DELETE removes zero rows; the "
                             "attachment row survives",
        "expected_object_state": "referenced bytes retained; no deletion task "
                                 "created",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production deletion SQL shape",
    },
    "original_post_remove": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; (ordinary) "
                        "personal plaintext asset with committed delete_intent "
                        "per the M3 guard; (enterprise) enterprise_asset whose "
                        "eb intent committed delete_intent while the "
                        "scope='enterprise' attachment row remains",
        "pre_object_hash": "n/a (reference-protection protocol leaf)",
        "action": "direct erl eunit -> w301 battery scenario: reference "
                  "release path (intent commits first) then the cleanup SQL",
        "expected_http_semantics": "no HTTP surface; post removal releases "
                                   "the reference through the intent protocol, "
                                   "not through owner cleanup",
        "expected_db_delta": "ordinary: exactly 1 row deleted after the "
                             "committed intent; enterprise: the "
                             "scope='enterprise' row is excluded from owner "
                             "cleanup (0 deleted) and the eb delete_intent "
                             "survives",
        "expected_object_state": "ordinary: object becomes GC-eligible through "
                                 "the intent; enterprise: reference release "
                                 "stays on the enterprise lifecycle",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; production deletion SQL shape",
    },
    "unrelated_reader_denied": {
        "pre_db_state": "isolated scratch PostgreSQL marker DB; group/org "
                        "membership tables queried for an unrelated user id",
        "pre_object_hash": "n/a (authorization leaf)",
        "action": "direct erl eunit -> w301 battery scenario: membership "
                  "EXISTS probe for the unrelated reader",
        "expected_http_semantics": "unrelated reader gets no access and no "
                                   "deletion job details",
        "expected_db_delta": "none (read-only authorization probe)",
        "expected_object_state": "no object state change",
        "proof_actor": "epgsql marker-DB connection on attv5_<slot>_pool "
                       "fixtures; membership probe only",
    },
}

SCENARIO_FN = {
    ("owner_logout", "ordinary"): "w301_shared_owner",
    ("owner_account_delete", "ordinary"): "w301_shared_owner",
    ("original_post_remove", "ordinary"): "w301_shared_owner",
    ("unrelated_reader_denied", "ordinary"): "w301_shared_owner",
    ("owner_logout", "enterprise"): "w301_shared_owner_enterprise",
    ("owner_account_delete", "enterprise"): "w301_shared_owner_enterprise",
    ("original_post_remove", "enterprise"): "w301_shared_owner_enterprise",
    ("unrelated_reader_denied", "enterprise"): "w301_shared_owner_enterprise",
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
