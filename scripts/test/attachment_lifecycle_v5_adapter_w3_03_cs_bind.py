#!/usr/bin/env python3
"""W3-03 evidence assessment; no leaf is sealed without runtime observations."""
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
    },
    "wrong_workspace": {
        "test": "cross_org_and_workspace_bind_rejected",
        "joint": True,
        "source_tokens": [
            "workspace_id => OtherWs",
            "csbe03-xws-1",
            "message_count(Org, OtherWs",
        ],
    },
    "wrong_owner": {
        "test": "cs_wrong_owner_bind_rejected",
        "joint": False,
        "source_tokens": [
            "uploader => Peer",
            "csbe03-contact-owner-1",
            "append_with_assets",
            "unbound_count(Org, Ws, AssetId)",
        ],
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
    gaps = []
    for leaf in leaves:
        scenario = SCENARIOS.get(leaf["ordering_or_fault"])
        if scenario is None:
            support.die(CASE, "no evidence definition for leaf scenario %r"
                        % leaf["ordering_or_fault"])
        test = support.named_test(parsed, scenario["test"], CASE)
        if test["module"] != MODULE:
            support.die(CASE, "named test ran in %r, expected %r" % (test["module"], MODULE))
        block = support.erl_function_block(source, scenario["test"], CASE, source_path)
        missing_tokens = [token for token in scenario["source_tokens"]
                          if token not in block["text"]]
        if missing_tokens:
            support.die(CASE, "suite source %s lacks scenario assertions %r"
                        % (scenario["test"], missing_tokens))
        missing = [
            support.oracle_missing_fields(),
            "scenario-level raw observations and actual assertion values are absent",
            "runtime database/actor provenance and schema-phase evidence are absent",
            "candidate-bound start/finish evidence is absent; log mtime is not a receipt",
        ]
        if scenario["joint"]:
            missing.append("joint EUnit status cannot prove each leg independently")
        gaps.append({"leaf": leaf, "missing": missing})
    support.report_gaps(CASE, evidence, parsed, gaps,
                        notes=["named EUnit status and source tokens are diagnostic only"])


if __name__ == "__main__":
    main()
