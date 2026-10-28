#!/usr/bin/env python3
"""W4-01/hold-entrypoints adapter: emits measured leaves for sealing.

Every (entrypoint, governance fixture) cell of the 12x6 production
entrypoint matrix emits its own observation (fixture, run base, explicit
delete elimination).  The adapter binds each frozen leaf to its cell
event, the parsed suite line, the suite summary, the suite source emit
check and the real scratch pool target.  Oracle: each of the 12 real
production delete entrypoints under each of the four hold scopes and
future/permanent retention produces no ready intent and zero deletes;
unresolvable scope is explicitly protected (pending sessions stay
registered/expired), never N/A.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W4-01/hold-entrypoints"
LOG_NAMES = ("eunit-w401.log", "raw.log")
MARKER = "W401_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_w401_hold_entrypoints_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_w401_hold_entrypoints_tests.erl"

ENTRYPOINT_ORACLE = {
    "orphan-cleanup": {
        "entry": "production attachment_ds:orphan_cleanup (real sweep over "
                 "orphan-predicate metadata assets)",
        "protection": "the installed governance fixture keeps the asset "
                      "outside the sweep result set",
    },
    "account-deletion": {
        "entry": "production user_deletion_executor:collect_attachment_keys_tx "
                 "+ attachment_repo:intent_ordinary_delete per key",
        "protection": "the eligibility face (governance rows + scope "
                      "protection) rejects the intent with intent_conflict; "
                      "the row stays as the isolation record",
    },
    "app-confirm-reject": {
        "entry": "production enterprise_asset_logic confirm-reject path "
                 "(reject_object -> attach_pending_repo:"
                 "intent_pending_delete_tx with mode reject)",
        "protection": "the metadata row plus the governance fixture refuse "
                      "the reject intent (metadata domain requires the "
                      "metadata intent; the governance hold makes the "
                      "metadata intent 23510)",
    },
    "app-retention-purge": {
        "entry": "production enterprise_asset_retention_logic:governance_tx "
                 "with op=purge",
        "protection": "the DB purge guard refuses while the EAR hold/retention "
                      "window is active",
    },
    "eb-private-delete": {
        "entry": "production eb_asset_store:delete_private",
        "protection": "the enterprise workspace hold (enterprise_retention_"
                      "hold row) yields intent_conflict before any queue row",
    },
    "eb-pending-cleanup": {
        "entry": "production eb_asset_app:cleanup_pending",
        "protection": "the app-level hold gate returns the asset as skipped "
                      "with reason active_hold; zero deletes",
    },
    "reject-size": {
        "entry": "pending-domain reject-size entrypoint over the production "
                 "pending cleanup face (attachment_ds:pending_cleanup)",
        "protection": "the pending session stays explicitly isolated in "
                      "registered/expired state — never N/A",
    },
    "reject-mime": {
        "entry": "pending-domain reject-mime entrypoint over the production "
                 "pending cleanup face",
        "protection": "the pending session stays explicitly isolated in "
                      "registered/expired state — never N/A",
    },
    "reject-teaching": {
        "entry": "pending-domain reject-teaching entrypoint over the "
                 "production pending cleanup face",
        "protection": "the pending session stays explicitly isolated in "
                      "registered/expired state — never N/A",
    },
    "teaching-unbound": {
        "entry": "pending-domain teaching-unbound entrypoint over the "
                 "production pending cleanup face",
        "protection": "the pending session stays explicitly isolated in "
                      "registered/expired state — never N/A",
    },
    "pending-cleanup": {
        "entry": "production attachment_ds:pending_cleanup over registered-"
                 "expired pending sessions",
        "protection": "sessions transition only within the pending domain; "
                      "zero metadata/object deletes",
    },
    "eb-purge-drain": {
        "entry": "production eb_pg_purge drain over the delete queue",
        "protection": "the held scenario assets produce no queue row of any "
                      "state, so the drain has nothing to reclaim",
    },
}

GOVERNANCE_FIXTURES = (
    "hold_workspace", "hold_conversation", "hold_message", "hold_attachment",
    "future_retention", "permanent_workspace_retention",
)


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
        ep = ENTRYPOINT_ORACLE.get(leaf["entrypoint"])
        if ep is None or leaf["ordering_or_fault"] not in GOVERNANCE_FIXTURES:
            support.die(CASE, "no evidence definition for leaf %r/%r"
                        % (leaf["entrypoint"], leaf["ordering_or_fault"]))
        test = support.named_test_in_module(parsed, "run", MODULE, CASE)
        obs = support.matching_observation(entries, leaf, CASE)
        oracle = {
            "pre_db_state": "isolated scratch PostgreSQL app pool %s; "
                            "metadata asset (group scope) + enterprise "
                            "scope_ref-frozen asset + pending session seeded "
                            "per fixture; the %s governance fixture installed "
                            "on the attachment rows" % (pool, leaf["ordering_or_fault"]),
            "pre_object_hash": "n/a (hold-eligibility leaf; the cell "
                               "adjudicates row eligibility, not object "
                               "bytes)",
            "action": "direct erl eunit -> W4-01 hold-entrypoints cell: %s "
                      "run under the %s governance fixture; %s; measured "
                      "values emitted to the observation stream after the "
                      "cell asserts" % (ep["entry"], leaf["ordering_or_fault"],
                                        ep["protection"]),
            "expected_http_semantics": "no HTTP surface; no ready intent and "
                                       "zero deletes under the hold",
            "expected_db_delta": "asset rows survive, no ready "
                                 "attachment_object_deletion / "
                                 "enterprise_asset_delete_queue rows",
            "expected_object_state": "held bytes retained; the governance "
                                     "fixture owns the protection",
            "proof_actor": "production entrypoints invoked on %s fixtures "
                           "through the app pool connection" % pool,
        }
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test="w401h_emit",
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
