#!/usr/bin/env python3
"""W3-03/cs-bind measured leaves.

Each leaf is bound to the named real-PG eunit test that exercises it in
eb_message_assets_projection_tests (gate w303_case, eunit-w303.log).
wrong_org and wrong_workspace are asserted inside one joint named test;
the adapter emits two separate leaves and discloses that runtime
granularity instead of pretending they were reported independently.
"""
import json
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
        "joint": False,
        "source_tokens": [
            "client_msg_id => <<\"csbe01-replay-1\">>",
            "sender_type => contact",
            "maps:get(replayed, Replay)",
            "maps:get(assets, maps:get(message, First))",
        ],
        "oracle": {
            "pre_db_state": "enterprise_asset U1 active+unbound in the owned org/workspace/conversation fixture; no enterprise_message row for client_msg_id csbe01-replay-1",
            "pre_object_hash": "n/a: eb_message_assets_projection_tests seeds database rows only; this case has no storage object bytes",
            "action": "eb_message_app:append_message/2 called twice with identical client_msg_id, sender_type=contact, contact_id and asset_ids=[U1]",
            "expected_http_semantics": "in-process canonical append: first {ok, replayed=false}; retry {ok, replayed=true} (no HTTP endpoint in this suite)",
            "expected_db_delta": "executed-once canonical transaction creates one message and one reference; retry creates zero extra messages and zero extra asset references",
            "expected_object_state": "retry echoes the same asset ids and frozen projection values; no second message/reference",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; named eunit test post_replay_echoes_same_assets",
        },
    },
    "wrong_org": {
        "test": "cross_org_and_workspace_bind_rejected",
        "joint": True,
        "source_tokens": [
            "OtherOrg",
            "Foreign1",
            "csbe03-xorg-1",
            "unbound_count(OtherOrg, OtherWs, Foreign1)",
        ],
        "oracle": {
            "pre_db_state": "Foreign1 enterprise_asset active+unbound in OtherOrg/OtherWs, outside the caller org; caller org conversation fixture exists",
            "pre_object_hash": "n/a: database row fixture only; this case has no storage object bytes",
            "action": "append_with_assets(caller Scope, [Foreign1])",
            "expected_http_semantics": "{error,_} with the not_found-shaped refusal; no asset storage details returned",
            "expected_db_delta": "zero enterprise_message rows; Foreign1 unbound_count(OtherOrg,OtherWs)=1; no binding",
            "expected_object_state": "foreign asset untouched (no message_id); zero side effects",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; joint named eunit test cross_org_and_workspace_bind_rejected (wrong_org leg)",
        },
    },
    "wrong_workspace": {
        "test": "cross_org_and_workspace_bind_rejected",
        "joint": True,
        "source_tokens": [
            "workspace_id => OtherWs",
            "csbe03-xws-1",
            "message_count(Org, OtherWs",
        ],
        "oracle": {
            "pre_db_state": "workspace OtherWs belongs to a different org; caller org/conversation fixture and Foreign1 asset exist",
            "pre_object_hash": "n/a: database row fixture only; this case has no storage object bytes",
            "action": "eb_message_app:append_message(Org, #{workspace_id => OtherWs, conversation_id => own conv, client_msg_id => csbe03-xws-1, asset_ids => [Foreign1]})",
            "expected_http_semantics": "{error,_} from the tenant gate before touching the database",
            "expected_db_delta": "message_count(Org, OtherWs, csbe03-xws-1)=0; no message, no binding",
            "expected_object_state": "no asset mutation and no storage detail leak",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; joint named eunit test cross_org_and_workspace_bind_rejected (wrong_workspace leg)",
        },
    },
    "wrong_owner": {
        "test": "wrong_owner_bind_rejected",
        "joint": False,
        "source_tokens": [
            "uploader => Peer",
            "csbe03-owner-1",
            "unbound_count(Org, maps:get(workspace_id, Scope), U1)",
        ],
        "oracle": {
            "pre_db_state": "U1 enterprise_asset active+unbound uploaded by Peer (not the acting user); business_identity sender fixture",
            "pre_object_hash": "n/a: database row fixture only; this case has no storage object bytes",
            "action": "eb_message_app:append_message(Org, #{sender_type => business_identity, actor_user_id => Actor, asset_ids => [U1]})",
            "expected_http_semantics": "{error,_}; refusal shape is indistinguishable from not_found and exposes no storage details",
            "expected_db_delta": "no enterprise_message row; unbound_count(Org,Ws,U1)=1 (no binding)",
            "expected_object_state": "asset untouched; no message_id",
            "proof_actor": "targeted-contract: real scratch PostgreSQL via eb_pg_test_fixture; named eunit test wrong_owner_bind_rejected",
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
