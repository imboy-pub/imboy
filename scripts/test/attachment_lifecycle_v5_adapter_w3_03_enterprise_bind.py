#!/usr/bin/env python3
"""W3-03/enterprise-bind adapter: emits measured leaves for sealing.

Evidence chain per leaf (no fabrication at any step):
  1. CASE_DIR/observations.jsonl — the suite emitted this scenario's real
     measured values (versions/counts/equality booleans) from inside the
     test function, after its own asserts passed;
  2. CASE_DIR/eunit-w303.log — parsed named eunit test line + suite summary;
  3. the suite source must contain the scenario's emit call;
  4. the real scratch pool target (container/host/port) from the gate env.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-03/enterprise-bind"
LOG_NAMES = ("eunit-w303.log", "raw.log")
MARKER = "W303_DIRECT_OK"
MODULE = "eb_message_assets_projection_tests"
SRC_REL = ("test/features/enterprise_business/application/message/"
           "eb_message_assets_projection_tests.erl")

SCENARIOS = {
    "same_message_replay": {
        "test": "enterprise_replay_echoes_same_assets",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool "
                            "attv5_<slot>_pool; eb_pg_test_fixture random-TSID "
                            "tenant isolated per scenario; asset seeded "
                            "unbound (message_id NULL)",
            "pre_object_hash": "n/a (message/asset projection leaf; no object "
                               "bytes stored — frozen whitelist projection "
                               "{id,mime,size_bytes,file_name,status})",
            "action": "direct erl eunit:test([eb_message_assets_projection_tests],"
                      "[verbose]) -> enterprise_replay_echoes_same_assets: "
                      "eb_message_app:append_message twice with the same "
                      "client_msg_id (sender_type=business_identity), then "
                      "per-scenario measured values emitted to the "
                      "observation stream",
            "expected_http_semantics": "no HTTP surface; enterprise message "
                                       "projection frozen five-key assets "
                                       "whitelist; replay returns the same "
                                       "message_id and identical assets",
            "expected_db_delta": "first append binds the asset (version+1, "
                                 "message_id set); replay appends zero rows — "
                                 "same message echoed, unbound stays 0",
            "expected_object_state": "no object state change; projection "
                                     "assets identical between first and "
                                     "replay",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; business_identity sender "
                           "with actor_user_id",
        },
    },
    "one_invalid_asset_all_rollback": {
        "test": "one_invalid_asset_all_rollback",
        "oracle": {
            "pre_db_state": "isolated scratch PostgreSQL app pool; one live "
                            "unbound asset and one tombstoned asset "
                            "(lifecycle_state='delete_intent') in the same "
                            "random-TSID tenant",
            "pre_object_hash": "n/a (bind-eligibility protocol leaf; no "
                               "object bytes stored)",
            "action": "direct erl eunit -> one_invalid_asset_all_rollback: "
                      "append_enterprise_with_assets with [valid, tombstone], "
                      "then tombstone-solo and valid-solo probes",
            "expected_http_semantics": "no HTTP surface; delete-intent asset "
                                       "refuses new references with the same "
                                       "not_found error shape as absent assets",
            "expected_db_delta": "canonical single transaction rolls back "
                                 "wholly: no message row, both assets stay "
                                 "unbound (unbound_count=1 each), versions "
                                 "unchanged; valid-only append then binds",
            "expected_object_state": "no object state change; tombstone asset "
                                     "never gains a message reference",
            "proof_actor": "epgsql app-pool connection on attv5_<slot>_pool "
                           "via eb_pg_test_fixture; business_identity sender",
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
