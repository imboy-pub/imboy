#!/usr/bin/env python3
"""W4-01/intent-rollback adapter: emits measured leaves for sealing.

Every (entrypoint, fault) cell of the 12x4 failure-injection matrix emits
its own observation (storage_delete_calls=0, unknown_success_reported=
false).  The adapter binds each frozen leaf to its cell event, the parsed
suite line, the suite summary, the suite source emit check and the real
scratch pool target.  Oracle: when the intent insert, the audit write or
the commit response fails, or the transaction rolls back, the entrypoint
must never touch storage bytes and must never report success.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-01/intent-rollback"
LOG_NAMES = ("eunit-w401.log", "raw.log")
MARKER = "W401_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_w401_rollback_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_w401_rollback_tests.erl"

ENTRYPOINT_ORACLE = {
    "pending-cleanup": "attachment_ds:pending_cleanup sweep; the injected "
                       "intent failure surfaces as cleaned=0 errors>=1",
    "reject-size": "pending-domain reject-size entrypoint over the shared "
                   "pending cleanup injection face",
    "reject-mime": "pending-domain reject-mime entrypoint over the shared "
                   "pending cleanup injection face",
    "reject-teaching": "pending-domain reject-teaching entrypoint over the "
                       "shared pending cleanup injection face",
    "teaching-unbound": "pending-domain teaching-unbound entrypoint over the "
                        "shared pending cleanup injection face",
    "orphan-cleanup": "attachment_ds:orphan_cleanup over the orphan intent "
                      "injection point",
    "account-deletion": "user_deletion_executor collect+intent production "
                        "shape; the failure aborts the whole tx",
    "eb-pending-cleanup": "eb_asset_app:cleanup_pending claim failure "
                          "returns as skipped cleanup_failed",
    "app-retention-purge": "enterprise_asset_retention_logic:governance_tx "
                           "purge; the intent failure returns "
                           "internal_error purge_intent_failed",
    "app-confirm-reject": "enterprise confirm-reject path "
                          "(attach_pending_repo:intent_pending_delete_tx "
                          "mode=reject); the failure propagates as error",
    "eb-private-delete": "eb_asset_store:delete_private; the fn intent "
                         "failure propagates as error",
    "eb-purge-drain": "eb_pg_purge:purge_batch drain; the drain failure "
                      "propagates as error",
}

FAULTS = ("rollback", "intent_insert_error", "audit_insert_error",
          "commit_response_lost")

DOMAINS = {"eb-private-delete": "enterprise",
           "eb-pending-cleanup": "enterprise",
           "eb-purge-drain": "enterprise"}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)
    entries = support.read_observations(case_dir, CASE)
    pool = support.pool_resource(ctx, CASE)[0]
    rows = []
    for leaf in leaves:
        fault = leaf["ordering_or_fault"]
        ep_oracle = ENTRYPOINT_ORACLE.get(leaf["entrypoint"])
        if ep_oracle is None or fault not in FAULTS:
            support.die(CASE, "no evidence definition for leaf %r/%r"
                        % (leaf["entrypoint"], fault))
        if leaf["domain"] != DOMAINS.get(leaf["entrypoint"], "ordinary"):
            support.die(CASE, "domain mismatch for leaf %r" % leaf["entrypoint"])
        test = support.named_test_in_module(
            parsed, "pending_cleanup_rollback", MODULE, CASE)
        obs = support.matching_observation(entries, leaf, CASE)
        measured = obs.get("measured", {})
        if measured.get("storage_delete_calls") != 0                 or measured.get("unknown_success_reported") is not False:
            support.die(CASE, "cell %r/%r did not measure zero storage "
                              "deletes and no success report"
                        % (leaf["entrypoint"], fault))
        oracle = {
            "pre_db_state": "meck-instrumented unit context over the "
                            "production entrypoint modules (no live rows); "
                            "pool %s backs the suite process" % pool,
            "pre_object_hash": "n/a (failure-injection leaf; the cell "
                               "adjudicates call behaviour, not object "
                               "bytes)",
            "action": "direct erl eunit -> intent-rollback cell: %s with "
                      "the %s fault injected at the intent call; measured "
                      "values emitted to the observation stream after the "
                      "asserts" % (ep_oracle, fault),
            "expected_http_semantics": "no HTTP surface; the entrypoint must "
                                       "not report success under the "
                                       "injected failure",
            "expected_db_delta": "no committed intent under the injected "
                                 "failure; tombstone/state machine owns the "
                                 "retry",
            "expected_object_state": "storage bytes untouched (zero "
                                     "elib_oss:delete_object calls)",
            "proof_actor": "meck-instrumented production modules executed by "
                           "the W4-01 suite process on %s" % pool,
        }
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test="rollback_emit",
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
