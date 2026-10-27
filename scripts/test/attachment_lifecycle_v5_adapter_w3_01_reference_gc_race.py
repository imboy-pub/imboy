#!/usr/bin/env python3
"""W3-01/reference-gc-race leaf assessment.

The gate executes attachment_lifecycle_v5_pg_tests once and writes
eunit-w203.log.  This adapter parses that real output and checks whether
every frozen leaf has its own scenario-level runtime receipt.  The current
suite reports three aggregate tests (run / w203_run / w301_run); it neither
names a test per ordering nor seeds an enterprise fixture, and the
guards-contract phase is not applied.  Those leaves cannot be measured
from this output, so the adapter exits 30 and names every missing field
instead of sealing a suite flag as a leaf PASS.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-01/reference-gc-race"
LOG_NAMES = ("eunit-w203.log", "eunit-w301.log", "raw.log")
MARKER = "W301_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

TOKENS = {
    "bind_commits_first": "v5e2e/w301/race/bind-first",
    "intent_commits_first": "v5e2e/w301/race/intent-first",
    "direct_dml_bind_commits_first": "v5e2e/w301/race/dml-bind-first",
    "direct_dml_intent_commits_first": "v5e2e/w301/race/dml-intent-first",
}
FUNCTIONS = {scenario: "w301_reference_gc_race/1" for scenario in TOKENS}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)

    race = support.erl_function_block(source, "w301_reference_gc_race", CASE, source_path)
    runner = support.erl_function_block(source, "w301_run", CASE, source_path)
    if "w301_reference_gc_race(" not in runner["text"]:
        support.die(CASE, "w301_run/0 does not invoke w301_reference_gc_race/1")
    missing_tokens = [token for token in TOKENS.values() if token not in race["text"]]
    if missing_tokens:
        support.die(CASE, "suite function w301_reference_gc_race/1 lacks fixtures %r"
                    % missing_tokens)

    gaps = support.w301_gaps(leaves, parsed, source, evidence["text"], TOKENS, FUNCTIONS)
    support.report_gaps(CASE, evidence, parsed, gaps)


if __name__ == "__main__":
    main()
