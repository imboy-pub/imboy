#!/usr/bin/env python3
"""W3-02/hold-all-entrypoints leaf assessment.

The gate executes attachment_lifecycle_v5_hold_protocol_tests once and
writes eunit-w302.log.  The suite enumerates its fixture matrix in source
(entrypoints/0 x install_governance/3) but emits one aggregate eunit test
named run; there is no per-entrypoint/per-scope status line.  Worse, the
executed matrix has 10 entrypoints while the frozen contract requires 12
(reject-teaching and teaching-unbound are absent).  The adapter therefore
cannot produce 72 measured leaves from this output: it parses the real
matrix and exits 30 naming every leaf and missing field.
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
RETENTION_VARIANTS = ("future_retention", "permanent_workspace_retention")


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)

    aggregate = support.named_test(parsed, "run", CASE)
    entrypoints = support.erl_entrypoints(source, CASE)
    fixtures = support.erl_governance_fixtures(source, CASE)
    expected_entrypoints = sorted({leaf["entrypoint"] for leaf in leaves})
    expected_fixtures = sorted({leaf["ordering_or_fault"] for leaf in leaves})
    missing_entrypoints = [ep for ep in expected_entrypoints if ep not in entrypoints]
    missing_fixtures = [fx for fx in expected_fixtures if fx not in fixtures]

    matrix_block = support.erl_function_block(
        source, "w302_entrypoint_matrix", CASE, source_path)
    runner = support.erl_function_block(source, "run", CASE, source_path)
    if "w302_entrypoint_matrix(" not in runner["text"]:
        support.die(CASE, "run/0 does not invoke w302_entrypoint_matrix/5")
    assert_line = None
    for offset, line in enumerate(matrix_block["text"].splitlines()):
        if '23510' in line:
            assert_line = matrix_block["start"] + offset
            break
    if assert_line is None:
        support.die(CASE, "w302_entrypoint_matrix/5 has no 23510 eligibility assertion")

    gaps = []
    for leaf in leaves:
        missing = []
        if leaf["entrypoint"] not in entrypoints:
            missing.append(
                "entrypoint %r absent from the executed fixture matrix "
                "(source lists %d entrypoints: %s; contract requires %d: %s)"
                % (leaf["entrypoint"], len(entrypoints), ", ".join(entrypoints),
                   len(expected_entrypoints), ", ".join(expected_entrypoints)))
        else:
            missing.append(
                "per-leaf runtime receipt: parsed eunit test %r is the only "
                "runtime unit (%.3fs); no per-entrypoint/per-scope status line"
                % (aggregate["name"], aggregate["duration"] or 0.0))
        if leaf["ordering_or_fault"] not in fixtures:
            missing.append("governance fixture %r absent from suite clauses"
                           % leaf["ordering_or_fault"])
        elif leaf["ordering_or_fault"] in RETENTION_VARIANTS:
            missing.append(
                "retention variant %r installed inside the aggregate matrix "
                "loop; no separate receipt" % leaf["ordering_or_fault"])
        missing.append(support.oracle_missing_fields(
            "pre_db_state", "pre_object_hash", "action",
            "expected_db_delta", "expected_object_state"))
        gaps.append({"leaf": leaf, "missing": missing})

    notes = [
        "contract leaf combinations: %d (%d entrypoints x %d governance fixtures)"
        % (len(leaves), len(expected_entrypoints), len(expected_fixtures)),
        "executed fixture combinations: %d (%d entrypoints x %d fixtures)"
        % (len(entrypoints) * len(fixtures), len(entrypoints), len(fixtures)),
        "missing entrypoints: %s" % (", ".join(missing_entrypoints) or "none"),
        "missing fixtures: %s" % (", ".join(missing_fixtures) or "none"),
        "eligibility assertion ?assertMatch(23510) is at %s:%d but only inside "
        "w302_entrypoint_matrix/5; eunit reports nothing per combination"
        % (SRC_REL, assert_line),
    ]
    support.report_gaps(CASE, evidence, parsed, gaps, notes=notes)


if __name__ == "__main__":
    main()
