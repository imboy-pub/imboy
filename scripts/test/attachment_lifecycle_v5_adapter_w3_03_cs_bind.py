#!/usr/bin/env python3
"""W3-03/cs-bind adapter: emits measured leaves for sealing.

Evidence chain per leaf: suite observation stream event (real measured
values emitted after each leg's asserts), parsed eunit named-test line,
suite summary, suite source emit check, real scratch pool target.  The
wrong_org / wrong_workspace leaves come from one eunit test but are proven
independently: the test emits one observation per leg between its own
asserts, so each leaf has its own measured values.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-03/cs-bind"
LOG_NAMES = ("eunit-w303.log", "raw.log")
MARKER = "W303_DIRECT_OK"
MODULE = "eb_message_assets_projection_tests"
SRC_REL = ("test/features/enterprise_business/application/message/"
           "eb_message_assets_projection_tests.erl")

SCENARIOS = {
    "same_message_replay": {
        "test": "post_replay_echoes_same_assets",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool "
                            "attv5_<slot>_pool; eb_pg_test_fixture random-TSID "
                            "tenant isolated per scenario; asset seeded "
                            "unbound (message_id NULL)",
            "pre_object_hash": "n/a (message/asset projection leaf; no object "
                               "bytes stored — frozen whitelist projection "
                               "{id,mime,size_bytes,file_name,status})",
            "action": "direct erl eunit:test([eb_message_assets_projection_tests],"
                      "[verbose]) -> post_replay_echoes_same_assets: "
                      "eb_message_app:append_message twice with the same "
                      "client_msg_id (sender_type=contact), then per-scenario "
                      "measured values emitted to the observation stream",
            "expected_http_semantics": "no HTTP surface; cs message projection "
                                       "frozen five-key assets whitelist; "
                                       "replay returns the same message_id "
                                       "and identical assets",
            "expected_db_delta": "first append binds the asset (version+1, "
                                 "message_id set); replay appends zero rows — "
                                 "same message echoed, unbound stays 0",
            "expected_object_state": "no object state change; projection "
                                     "assets identical between first and "
                                     "replay",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; contact sender",
        },
    },
    "wrong_org": {
        "test": "cross_org_and_workspace_bind_rejected",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool; two "
                            "random-TSID tenants: the caller's tenant and "
                            "another organization owning the foreign unbound "
                            "asset",
            "pre_object_hash": "n/a (bind-eligibility protocol leaf; no "
                               "object bytes stored)",
            "action": "direct erl eunit -> cross_org_and_workspace_bind_rejected "
                      "(org leg): append_with_assets referencing another "
                      "organization's asset id, measured values emitted from "
                      "inside the test after the org-leg asserts",
            "expected_http_semantics": "no HTTP surface; cross-organization "
                                       "reference rejected with the same "
                                       "not_found error shape as absent assets",
            "expected_db_delta": "zero side effects: no message row for the "
                                 "caller, foreign asset stays unbound with "
                                 "unchanged version",
            "expected_object_state": "no object state change; foreign asset "
                                     "never gains a reference",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; contact sender",
        },
    },
    "wrong_workspace": {
        "test": "cross_org_and_workspace_bind_rejected",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool; caller "
                            "tenant plus a workspace belonging to another "
                            "organization holding the foreign unbound asset",
            "pre_object_hash": "n/a (bind-eligibility protocol leaf; no "
                               "object bytes stored)",
            "action": "direct erl eunit -> cross_org_and_workspace_bind_rejected "
                      "(workspace leg): eb_message_app:append_message with a "
                      "workspace from another organization, measured values "
                      "emitted from inside the test after the ws-leg asserts",
            "expected_http_semantics": "no HTTP surface; tenant ownership gate "
                                       "rejects before any database write",
            "expected_db_delta": "zero side effects: no message row in the "
                                 "foreign workspace, foreign asset stays "
                                 "unbound with unchanged version",
            "expected_object_state": "no object state change; foreign asset "
                                     "never gains a reference",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; contact sender",
        },
    },
    "wrong_owner": {
        "test": "cs_wrong_owner_bind_rejected",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool; caller "
                            "tenant with an unbound asset uploaded by another "
                            "user (peer)",
            "pre_object_hash": "n/a (bind-eligibility protocol leaf; no "
                               "object bytes stored)",
            "action": "direct erl eunit -> cs_wrong_owner_bind_rejected: "
                      "append_with_assets referencing a peer-uploaded asset, "
                      "measured values emitted from inside the test",
            "expected_http_semantics": "no HTTP surface; cs member message "
                                       "may only bind assets the sender "
                                       "uploaded; rejected with the same "
                                       "not_found error shape as absent assets",
            "expected_db_delta": "zero side effects: no message row, asset "
                                 "stays unbound with unchanged version",
            "expected_object_state": "no object state change; peer-owned "
                                     "asset never gains a reference",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; contact sender",
        },
    },
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
        scenario = SCENARIOS.get(leaf["ordering_or_fault"])
        if scenario is None:
            support.die(CASE, "no evidence definition for leaf scenario %r"
                        % leaf["ordering_or_fault"])
        test = support.named_test(parsed, scenario["test"], CASE)
        if test["module"] != MODULE:
            support.die(CASE, "named test ran in %r, expected %r" % (test["module"], MODULE))
        obs = support.matching_observation(entries, leaf, CASE)
        oracle = dict(scenario["oracle"])
        pool = support.pool_resource(ctx, CASE)[0]
        oracle = {key: value.replace("attv5_<slot>_pool", pool)
                  for key, value in oracle.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test=scenario["test"],
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
