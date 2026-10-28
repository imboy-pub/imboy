#!/usr/bin/env python3
"""W3-02/hold-all-entrypoints adapter: emits measured leaves for sealing.

Every (entrypoint, governance fixture) cell of the 12x6 protocol matrix
emits its own observation stream event with the real measured values
(asset path, attachment id, scope, governance fixture, refresh result and
the 23510 eligibility rejection).  The adapter binds each frozen leaf to
its cell event, the parsed eunit run line, the suite summary, the suite
source emit check and the real scratch pool target.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-02/hold-all-entrypoints"
LOG_NAMES = ("eunit-w302.log", "raw.log")
MARKER = "W302_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_hold_protocol_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_hold_protocol_tests.erl"

ENTRYPOINT_ORACLE = {
    "reject-size": "future delete entrypoint reject-size (fixture mapping; "
                   "production caller routing is W4-01 scope)",
    "reject-mime": "future delete entrypoint reject-mime (fixture mapping; "
                   "production caller routing is W4-01 scope)",
    "reject-teaching": "future delete entrypoint reject-teaching (fixture "
                       "mapping; production caller routing is W4-01 scope)",
    "orphan-cleanup": "future delete entrypoint orphan-cleanup (fixture "
                      "mapping; production caller routing is W4-01 scope)",
    "pending-cleanup": "future delete entrypoint pending-cleanup (fixture "
                       "mapping; production caller routing is W4-01 scope)",
    "account-deletion": "future delete entrypoint account-deletion (fixture "
                        "mapping; production caller routing is W4-01 scope)",
    "teaching-unbound": "future delete entrypoint teaching-unbound (fixture "
                        "mapping; production caller routing is W4-01 scope)",
    "app-confirm-reject": "future delete entrypoint app-confirm-reject "
                          "(fixture mapping; production caller routing is "
                          "W4-01 scope)",
    "app-retention-purge": "future delete entrypoint app-retention-purge "
                           "(fixture mapping; production caller routing is "
                           "W4-01 scope)",
    "eb-private-delete": "future delete entrypoint eb-private-delete "
                         "(enterprise domain; fixture mapping; production "
                         "caller routing is W4-01 scope)",
    "eb-pending-cleanup": "future delete entrypoint eb-pending-cleanup "
                          "(enterprise domain, ciphered history form; "
                          "fixture mapping)",
    "eb-purge-drain": "future delete entrypoint eb-purge-drain (enterprise "
                      "domain; fixture mapping)",
}

GOVERNANCE_ORACLE = {
    "hold_workspace": {
        "governance": "workspace attachment retention row (WAR, workspace "
                      "scope) installed on the attachment",
        "effect": "the uniform eligibility protocol (fn_v5_intent_ordinary_"
                  "delete) rejects the intent with 23510 while the workspace "
                  "hold is in place",
    },
    "hold_conversation": {
        "governance": "workspace attachment retention row (WAR, channel "
                      "scope) installed on the attachment",
        "effect": "uniform eligibility protocol rejects the intent with "
                  "23510 under the conversation-scoped hold",
    },
    "hold_message": {
        "governance": "enterprise attachment retention row (EAR, hold_state="
                      "'held') installed on the attachment",
        "effect": "uniform eligibility protocol rejects the intent with "
                  "23510 under the per-asset legal hold",
    },
    "hold_attachment": {
        "governance": "enterprise attachment retention row (EAR, hold_state="
                      "'held') installed on the attachment",
        "effect": "uniform eligibility protocol rejects the intent with "
                  "23510 under the per-asset legal hold",
    },
    "future_retention": {
        "governance": "enterprise attachment retention row (EAR, hold_state="
                      "'none', retention_until ~50 years) installed on the "
                      "attachment",
        "effect": "uniform eligibility protocol rejects the intent with "
                  "23510 inside the future retention window",
    },
    "permanent_workspace_retention": {
        "governance": "workspace attachment retention row (WAR, permanent "
                      "scope_ref) installed on the attachment",
        "effect": "uniform eligibility protocol rejects the intent with "
                  "23510 under the permanent workspace retention",
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
        entry_oracle = ENTRYPOINT_ORACLE.get(leaf["entrypoint"])
        gov = GOVERNANCE_ORACLE.get(leaf["ordering_or_fault"])
        if entry_oracle is None or gov is None:
            support.die(CASE, "no evidence definition for leaf %r/%r"
                        % (leaf["entrypoint"], leaf["ordering_or_fault"]))
        test = support.named_test(parsed, "run", CASE)
        if test["module"] != MODULE:
            support.die(CASE, "hold suite ran in %r, expected %r"
                        % (test["module"], MODULE))
        obs = support.matching_observation(entries, leaf, CASE)
        pool = support.pool_resource(ctx, CASE)[0]
        oracle = {
            "pre_db_state": "isolated scratch PostgreSQL marker DB from "
                            "V5_INTTEST_W302_* (%s); confirmed unbound "
                            "plaintext-eligible attachment seeded via the "
                            "production pending protocol"
                            % pool,
            "pre_object_hash": "n/a (eligibility/hold protocol leaf; the "
                               "matrix adjudicates row eligibility, not "
                               "object bytes)",
            "action": "direct erl eunit -> hold protocol matrix cell: "
                      "refresh_gc_verified_tx proves plaintext eligibility, "
                      "then %s; %s; measured values emitted to the "
                      "observation stream after the cell asserts"
                      % (gov["governance"], entry_oracle),
            "expected_http_semantics": "no HTTP surface (protocol-only case); "
                                       "uniform eligibility intent must be "
                                       "rejected with 23510; " + gov["effect"],
            "expected_db_delta": "no attachment_object_deletion row; the "
                                 "governance row stays in place and the "
                                 "attachment lifecycle stays live",
            "expected_object_state": "no object state change; bytes remain "
                                     "referenced",
            "proof_actor": "epgsql marker-DB connection on %s fixtures; "
                           "production fn_v5_intent_ordinary_delete and "
                           "attachment_repo:refresh_gc_verified_tx" % pool,
        }
        rows.append(support.leaf_observation_row(
            leaf, obs=obs, test=test, evidence=evidence, parsed=parsed,
            ctx=ctx, oracle=oracle, scenario_test="w302_entrypoint_matrix",
            source_path=source_path, source=source, case_id=CASE))
    support.write_observations(case_dir, rows, leaves, CASE)


if __name__ == "__main__":
    main()
