#!/usr/bin/env python3
"""W3-01/history-unknown leaf assessment.

The PG suite seeds all three ordinary history forms (NULL locator, cipher,
collection hash ambiguity) inside one aggregate test and never seeds an
enterprise fixture.  eunit-w203.log therefore carries no scenario-level
status line, so the six frozen leaves cannot be measured from this output.
The adapter exits 30 and names the exact unmeasured oracle fields per leaf.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-01/history-unknown"
LOG_NAMES = ("eunit-w203.log", "eunit-w301.log", "raw.log")
MARKER = "W301_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

TOKENS = {
    "unknown_location": "v5e2e/w301/hist/unknown-location",
    "opaque_reference": "v5e2e/w301/hist/cipher",
    "ambiguous_reference": "v5e2e/w301/hist/collect",
}
FUNCTIONS = {scenario: "w301_history_unknown/1" for scenario in TOKENS}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)

    history = support.erl_function_block(source, "w301_history_unknown", CASE, source_path)
    runner = support.erl_function_block(source, "w301_run", CASE, source_path)
    if "w301_history_unknown(" not in runner["text"]:
        support.die(CASE, "w301_run/0 does not invoke w301_history_unknown/1")
    missing_tokens = [token for token in TOKENS.values() if token not in history["text"]]
    if missing_tokens:
        support.die(CASE, "w301_history_unknown/1 lacks fixtures %r" % missing_tokens)

    gaps = support.w301_gaps(leaves, parsed, source, evidence["text"], TOKENS, FUNCTIONS)
    support.report_gaps(CASE, evidence, parsed, gaps)


if __name__ == "__main__":
    main()
