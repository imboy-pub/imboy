#!/usr/bin/env python3
"""W4-01/delete-entrypoints adapter: emits measured leaves for sealing.

Each of the 12 future delete entrypoints emits one physical-delete
callgraph observation: its production modules are scanned for direct
storage-delete calls (elib_oss:delete_object / ObjectStore:delete) and
none may exist outside the single disclosed eb_asset_store drain-worker
exemption.  The adapter binds each frozen leaf to its observation stream
event, the parsed w401 battery line, the suite summary, the suite source
emit check and the real scratch pool target.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-01/delete-entrypoints"
LOG_NAMES = ("eunit-w401.log", "raw.log")
MARKER = "W401_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_w401_entrypoints_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_w401_entrypoints_tests.erl"

ORACLE = {
    "pre_db_state": "repository working tree of the frozen candidate "
                    "(no live database needed for the static gate; the "
                    "scan runs inside the W4-01 scratch-PG suite process)",
    "pre_object_hash": "n/a (static callgraph leaf; the gate scans source "
                       "text, not object bytes)",
    "action": "direct erl eunit -> w401 static callgraph: per-entrypoint "
              "module scan for direct storage-delete calls; measured "
              "values emitted to the observation stream after the scan",
    "expected_http_semantics": "no HTTP surface; the physical delete "
                               "callgraph must stay exclusive to the "
                               "driver/worker side",
    "expected_db_delta": "none (static scan)",
    "expected_object_state": "no object state change; entrypoints only "
                             "emit intents/jobs, never storage deletes",
    "proof_actor": "file scan of the frozen candidate sources executed by "
                   "the W4-01 suite process on attv5_<slot>_pool fixtures",
}

MODULES_BY_EP = {
    "reject-size": ["attach_logic"],
    "reject-mime": ["attach_logic"],
    "reject-teaching": ["attach_logic"],
    "teaching-unbound": ["attach_logic"],
    "orphan-cleanup": ["attach_cleanup_logic", "attachment_ds"],
    "pending-cleanup": ["attach_cleanup_logic", "attachment_ds"],
    "account-deletion": ["user_deletion_logic", "user_deletion_executor"],
    "app-confirm-reject": ["enterprise_asset_logic"],
    "app-retention-purge": ["enterprise_asset_retention_logic"],
    "eb-private-delete": ["eb_asset_app", "eb_asset_store"],
    "eb-pending-cleanup": ["eb_asset_app", "eb_asset_store"],
    "eb-purge-drain": ["eb_pg_purge"],
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
        mods = MODULES_BY_EP.get(leaf["entrypoint"])
        if mods is None:
            support.die(CASE, "no module mapping for leaf entrypoint %r"
                        % leaf["entrypoint"])
        test = support.named_test_in_module(parsed, "run", MODULE, CASE)
        obs = support.matching_observation(entries, leaf, CASE)
        measured_mods = obs.get("measured", {}).get("modules_scanned", [])
        if sorted(measured_mods) != sorted(mods):
            support.die(CASE, "observation modules %r != source mapping %r"
                        % (measured_mods, mods))
        pool = support.pool_resource(ctx, CASE)[0]
        oracle = {key: value.replace("attv5_<slot>_pool", pool)
                  for key, value in ORACLE.items()}
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test="w401_static_callgraph",
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
