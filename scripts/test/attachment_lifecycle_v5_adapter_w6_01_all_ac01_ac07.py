#!/usr/bin/env python3
"""W6-01/all-ac01-ac07 PLAN union verifier.

This is the only PLAN-domain leaf of the union case.  It does not run a
scenario; it verifies, from the frozen contract and the real sealed W6-01
receipts, that:

  * the W6-01 matrix AC set is exactly AC01..AC07 (no PLAN/AC08),
  * every contract source case/scenario pair declared by final_ac_oracle_refs
    is represented in the W6-01 expansion (and nothing else is),
  * the W6-01/real-pg-garage-disk case directory contains exactly one sealed
    measured receipt per frozen leaf, each with a measured PASS and a real
    actor for every non-plan leaf.

Missing sealed receipts are a hard evidence gap: a structural plan check never
stands in for the runtime AC01..AC07 receipts.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S  # noqa: E402

CASE_ID = "W6-01/all-ac01-ac07"
NAME = "w6_01_all_ac01_ac07"
SOURCE_CASE = "W6-01/real-pg-garage-disk"
ACS = ("AC01", "AC02", "AC03", "AC04", "AC05", "AC06", "AC07")


def _pairs(leaves):
    """(source case, source scenario) for source leaves and W6 expansions."""
    pairs = set()
    for leaf in leaves:
        scenario = leaf.get("source_scenario") or leaf.get("ordering_or_fault")
        pairs.add((leaf.get("source_case_ref"), scenario))
    return pairs


def build():
    run_dir, case_dir, root = S.C.argv_paths()
    ctx = S.context(run_dir, CASE_ID, root)
    leaves = S.leaves(ctx, CASE_ID)
    if len(leaves) != 1:
        S.die(CASE_ID, "PLAN union case must have exactly one leaf")
    leaf = leaves[0]
    contract = ctx["contract"]
    matrix = contract["expected_leaf_matrix"]
    refs = contract["final_ac_oracle_refs"]
    real = matrix.get(SOURCE_CASE) or []
    if not real:
        S.die(CASE_ID, "%s missing from expected_leaf_matrix" % SOURCE_CASE)
    repo = S.worktree(ctx, "imboy", CASE_ID)

    aces = {item["ac_id"] for item in real}
    expected_acs = set(ACS)
    source_pairs = set()
    for ac in ACS:
        for case in refs[ac]:
            for source_leaf in matrix[case]:
                source_pairs.add((case, source_leaf["ordering_or_fault"]))
    real_pairs = _pairs(real)
    identities = [S.identity(item) for item in real]

    gaps = []
    if aces != expected_acs:
        gaps.append("W6-01 matrix AC set %r != %r"
                    % (sorted(aces), sorted(expected_acs)))
    if len(set(identities)) != len(identities):
        gaps.append("W6-01 matrix contains duplicate leaf identities")
    missing_pairs = source_pairs - real_pairs
    extra_pairs = real_pairs - source_pairs
    if missing_pairs:
        gaps.append("%d declared AC01..AC07 source pairs missing from the W6-01 "
                    "expansion (e.g. %r)"
                    % (len(missing_pairs), sorted(missing_pairs)[:3]))
    if extra_pairs:
        gaps.append("%d W6-01 source pairs are not declared by "
                    "final_ac_oracle_refs (e.g. %r)"
                    % (len(extra_pairs), sorted(extra_pairs)[:3]))

    sealed_dir = os.path.join(run_dir, "evidence", "W6-01",
                              SOURCE_CASE.split("/", 1)[1])
    sealed = {}
    if os.path.isdir(sealed_dir):
        for name in sorted(os.listdir(sealed_dir)):
            if not (name.startswith("leaf-") and name.endswith(".json")):
                continue
            try:
                with open(os.path.join(sealed_dir, name), "r",
                          encoding="utf-8") as handle:
                    sealed[name] = json.load(handle)
            except (OSError, ValueError) as exc:
                gaps.append("unreadable sealed receipt %s (%s)" % (name, exc))
    if not sealed:
        gaps.append("no sealed measured receipts under %s; the runtime "
                    "AC01..AC07 union is unmeasured" % sealed_dir)
    else:
        by_id = {S.identity(row): row for row in sealed.values()}
        missing = [item for item in identities if item not in by_id]
        extra = [key for key in by_id if key not in set(identities)]
        if missing or extra:
            gaps.append("sealed leaf set differs: %d missing, %d unexpected"
                        % (len(missing), len(extra)))
        bad = []
        for name, row in sealed.items():
            if row.get("oracle_result") != "PASS":
                bad.append("%s oracle_result=%r"
                           % (name, row.get("oracle_result")))
                continue
            if (row.get("exit_code") != 0 or row.get("failed") != 0
                    or row.get("skipped") != 0 or row.get("executed", 0) < 1):
                bad.append("%s execution counts invalid" % name)
            actor = row.get("actor_provenance") or {}
            if not actor.get("resource_id") or not row.get("request_ids"):
                bad.append("%s actor/request ids missing" % name)
            if row.get("domain") != "plan" and actor.get("kind") != "real":
                bad.append("%s non-plan leaf has non-real actor" % name)
        if bad:
            gaps.append("%d sealed receipts are not measured PASS (e.g. %r)"
                        % (len(bad), bad[:3]))

    if gaps:
        S.report_gaps(CASE_ID, 0, {
            "W6-01/all-ac01-ac07": (1, gaps),
        })

    count = len(real)
    started, finished = S.step_window(
        S.step_record(case_dir, "ac-union", CASE_ID), CASE_ID)
    assertions = [
        S.assertion(
            "ac_set_exact",
            "%d W6-01 leaves cover AC01..AC07 exactly (counts: %s)"
            % (count, ", ".join("%s=%d" % (ac, sum(
                1 for item in real if item["ac_id"] == ac)) for ac in ACS))),
        S.assertion(
            "source_pair_union_exact",
            "%d declared source (case, scenario) pairs are all represented; no "
            "undeclared pair appears" % len(source_pairs)),
        S.assertion(
            "sealed_receipt_union_exact",
            "%s holds %d sealed receipts matching the %d frozen identities, all "
            "measured PASS with real actors"
            % (sealed_dir, len(sealed), count)),
        S.assertion(
            "no_duplicate_or_substitution",
            "identities are unique and every sealed receipt oracle_result=PASS "
            "with exit_code=0, failed=0, skipped=0, executed>=1"),
    ]
    oracle = {
        "pre_db_state": "frozen contract matrix W6-01/real-pg-garage-disk "
                        "(%d leaves) plus the real sealed receipts in %s"
                        % (count, sealed_dir),
        "pre_object_hash": "n/a: PLAN union verifier over receipt identity and "
                           "measured-PASS fields, no object bytes",
        "action": "compare final_ac_oracle_refs AC01..AC07 source pairs with "
                  "the W6-01 expansion and the sealed receipt identity set",
        "expected_http_semantics": "n/a: no HTTP call in the PLAN verifier",
        "expected_db_delta": "n/a: no database mutation in the PLAN verifier",
        "expected_object_state": "n/a: no storage object is read or written",
        "proof_actor": "repository %s; sealed receipt directory %s"
                       % (repo, sealed_dir),
    }
    rows = [S.C.build_row(
        leaf, started, finished,
        {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
         "skipped": 0},
        oracle, assertions, S.repo_actor(repo), [repo],
        ["ledger:%s:%d-leaves" % (SOURCE_CASE, count)])]
    S.C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    S.run_adapter(NAME, build)
