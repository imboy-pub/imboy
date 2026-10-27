#!/usr/bin/env python3
"""W3-03/enterprise-bind measured leaves.

The executed case output is the real-PG eunit run of
eb_message_assets_projection_tests (gate w303_case, eunit-w303.log).  Each
contract leaf is bound to the one named eunit test that exercises it; the
suite summary, per-test timing, source assertions and log digest are all
parsed from that output.  No suite flag is reused as a leaf measurement.
"""
import json
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
        "test": "post_replay_echoes_same_assets",
        "joint": False,
        "source_tokens": [
            "client_msg_id => <<\"csbe01-replay-1\">>",
            "maps:get(replayed, Replay)",
            "maps:get(assets, maps:get(message, First))",
        ],
        "oracle": {
            "pre_db_state": "enterprise_asset U1 active+unbound in the owned org/workspace/conversation fixture; no enterprise_message row for client_msg_id csbe01-replay-1",
            "pre_object_hash": "n/a: eb_message_assets_projection_tests seeds database rows only; this case has no storage object bytes",
            "action": "eb_message_app:append_message/2 called twice with identical client_msg_id, sender_type=contact, asset_ids=[U1] and key_ref",
            "expected_http_semantics": "in-process API result: first append {ok, replayed=false}; replay {ok, replayed=true} (this suite has no HTTP endpoint)",
            "expected_db_delta": "first append creates one enterprise_message and one asset binding; replay adds zero messages and zero new bindings",
            "expected_object_state": "replay assets list equals first assets list (same id/mime/size/file_name/status); one reference only",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; named eunit test post_replay_echoes_same_assets",
        },
    },
    "one_invalid_asset_all_rollback": {
        "test": "one_invalid_asset_all_rollback",
        "joint": False,
        "source_tokens": [
            "lifecycle_state='delete_intent'",
            "csbe03-rollback-1",
            "unbound_count(Org, Ws, U1)",
            "unbound_count(Org, Ws, U2)",
        ],
        "oracle": {
            "pre_db_state": "U1 active+unbound; U2 lifecycle_state=delete_intent (tombstone) in the same owned org/workspace/conversation",
            "pre_object_hash": "n/a: database row fixture only; this case has no storage object bytes",
            "action": "append_with_assets([U1,U2]) -> append_with_assets([U2]) -> append_with_assets([U1])",
            "expected_http_semantics": "in-process API: {error,_} for [U1,U2] and for [U2]; {ok,_} only for [U1] (no HTTP endpoint in this suite)",
            "expected_db_delta": "zero enterprise_message row for csbe03-rollback-1; U1 and U2 both remain unbound after the failed transaction; rollback-3 binds only U1",
            "expected_object_state": "no partial binding survives; the tombstoned asset never gains a message_id",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; named eunit test one_invalid_asset_all_rollback",
        },
    },
}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    resource_id, container_id = support.pool_resource(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)
    started, finished = support.measurement_window(evidence, parsed, CASE)
    probes = support.probe_entries(case_dir)

    rows = []
    for leaf in leaves:
        scenario = SCENARIOS.get(leaf["ordering_or_fault"])
        if scenario is None:
            support.die(CASE, "no evidence definition for leaf scenario %r"
                        % leaf["ordering_or_fault"])
        test = support.named_test(parsed, scenario["test"], CASE)
        if test["module"] != MODULE:
            support.die(CASE, "named test ran in %r, expected %r"
                        % (test["module"], MODULE))
        block = support.erl_function_block(source, scenario["test"], CASE, source_path)
        missing = [token for token in scenario["source_tokens"] if token not in block["text"]]
        if missing:
            support.die(CASE, "suite source %s lacks scenario assertions %r"
                        % (scenario["test"], missing))
        joint_note = ("; joint runtime unit: one named eunit test covers both "
                      "wrong_org and wrong_workspace and reports one status"
                      if scenario["joint"] else "")
        assertions = [
            support.assertion(
                "contract_leaf_identity",
                json.dumps({field: leaf[field] for field in support.LEAF_FIELDS},
                           sort_keys=True)),
            support.assertion(
                "eunit_named_test_%s_ok" % scenario["test"],
                "%s line %d: %s; parsed duration=%ss%s"
                % (evidence["name"], test["line"], test["raw"],
                   test["duration"], joint_note)),
            support.assertion(
                "eunit_suite_summary_%d_passed" % parsed["summary_count"],
                "parsed summary=%r; parsed tests=%d; failure_markers=%d; marker=%s"
                % (parsed["summary_raw"], len(parsed["tests"]),
                   len(parsed["failures"]), MARKER)),
            support.assertion(
                "scenario_source_assertions_present",
                "%s:%d-%d contains %s"
                % (SRC_REL, block["start"], block["end"], scenario["source_tokens"])),
            support.assertion(
                "evidence_log_sha256_recomputed",
                "%s sha256=%s size=%d mtime_window=%s..%s"
                % (evidence["name"], evidence["sha256"], evidence["size"],
                   started, finished)),
        ]
        probe = support.matching_probe(probes, leaf)
        if probe:
            assertions.append(support.assertion(
                "probe_%s_pass" % str(probe.get("name", "probe")),
                "probes.jsonl entry=%s" % json.dumps(probe, sort_keys=True)))
        rows.append(support.build_observation(
            leaf=leaf,
            assertions=assertions,
            oracle=scenario["oracle"],
            actor_provenance=support.actor(resource_id),
            resource_ids=[resource_id] + ([container_id] if container_id else []),
            request_ids=[support.request_id(CASE, evidence, leaf)],
            started_at=started,
            finished_at=finished,
        ))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
