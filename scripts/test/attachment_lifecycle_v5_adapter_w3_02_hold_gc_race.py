#!/usr/bin/env python3
"""W3-02/hold-gc-race leaf assessment.

The hold protocol suite runs w302_hold_gc_race/1 inside one aggregate
eunit test named run (eunit-w302.log).  It covers hold-first rejection and
the intent-then-governance already_deleting path, but it has no
retention-extension or workspace-hold-exclusion receipt, and each ordering
is seeded for only one domain (hold-first group, intent-first enterprise).
The eight frozen leaves therefore lack scenario-level receipts; the adapter
exits 30 naming each missing field.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-02/hold-gc-race"
LOG_NAMES = ("eunit-w302.log", "raw.log")
MARKER = "W302_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_hold_protocol_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_hold_protocol_tests.erl"

TOKENS = {
    "hold_commits_first": "v5e2e/w302/race/hold-first",
    "intent_before_asset_hold": "v5e2e/w302/race/intent-first",
    "intent_before_retention_extension": None,
    "workspace_hold_exclusion_receipt": None,
}
# Domain actually seeded by w302_hold_gc_race/1 for each implemented ordering.
SEEDED_DOMAIN = {
    "hold_commits_first": "ordinary",
    "intent_before_asset_hold": "enterprise",
}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)

    aggregate = support.named_test(parsed, "run", CASE)
    race = support.erl_function_block(source, "w302_hold_gc_race", CASE, source_path)
    runner = support.erl_function_block(source, "run", CASE, source_path)
    if "w302_hold_gc_race(" not in runner["text"]:
        support.die(CASE, "run/0 does not invoke w302_hold_gc_race/1")
    for scenario, token in TOKENS.items():
        if token is not None and token not in race["text"]:
            support.die(CASE, "%s fixture token %r missing from w302_hold_gc_race/1"
                        % (scenario, token))
    if "already_deleting" not in race["text"]:
        support.die(CASE, "w302_hold_gc_race/1 has no already_deleting assertion")

    gaps = []
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        missing = []
        token = TOKENS.get(scenario)
        if token is None:
            missing.append("scenario fixture absent from suite source for %r" % scenario)
        else:
            missing.append(
                "per-leaf runtime receipt: parsed eunit test %r is the only "
                "runtime unit (%.3fs); %r is asserted inside w302_hold_gc_race/1 "
                "with no per-ordering status line"
                % (aggregate["name"], aggregate["duration"] or 0.0, scenario))
            seeded = SEEDED_DOMAIN.get(scenario)
            if seeded and leaf["domain"] != seeded:
                missing.append(
                    "domain=%s: suite seeds %r for %s only (scope fixture "
                    "selects the other domain)"
                    % (leaf["domain"], seeded, scenario))
        missing.append(support.oracle_missing_fields(
            "pre_db_state", "pre_object_hash", "action",
            "expected_db_delta", "expected_object_state"))
        gaps.append({"leaf": leaf, "missing": missing})

    notes = [
        "contract orderings: 4 x 2 domains = %d leaves" % len(leaves),
        "suite implements 2 orderings (hold_commits_first scope=group, "
        "intent_before_asset_hold scope=enterprise) in one aggregate test",
        "no fixture for intent_before_retention_extension or "
        "workspace_hold_exclusion_receipt in %s" % SRC_REL,
    ]
    support.report_gaps(CASE, evidence, parsed, gaps, notes=notes)


if __name__ == "__main__":
    main()
