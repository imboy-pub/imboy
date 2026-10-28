#!/usr/bin/env python3
"""W3-01/shared-owner leaf assessment.

Same real-PG suite and log as reference-gc-race (eunit-w203.log).  The
suite exercises account-deletion sweep protection and unrelated-reader
denial, but it has no owner_logout or original_post_remove fixture, no
enterprise domain fixture, and reports only aggregate test status.  Leaves
without a scenario-level receipt are reported field-by-field and the
adapter exits 30.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as support

CASE = "W3-01/shared-owner"
LOG_NAMES = ("eunit-w203.log", "eunit-w301.log", "raw.log")
MARKER = "W301_DIRECT_OK"
MODULE = "attachment_lifecycle_v5_pg_tests"
SRC_REL = "test/repo/attachment_lifecycle_v5_pg_tests.erl"

TOKENS = {
    "owner_logout": None,
    "owner_account_delete": "w301 account deletion",
    "original_post_remove": None,
    "unrelated_reader_denied": "user_id=999999",
}
FUNCTIONS = {
    "owner_logout": None,
    "owner_account_delete": "w301_shared_owner/1",
    "original_post_remove": None,
    "unrelated_reader_denied": "w301_shared_owner/1",
}


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    ctx = support.context(run_dir, CASE)
    leaves = support.leaves(ctx, CASE)
    evidence = support.read_evidence(case_dir, LOG_NAMES, CASE)
    parsed = support.parse_eunit(evidence["text"], CASE)
    support.require_clean_suite(parsed, MARKER, CASE, MODULE)
    source_path, source = support.erl_source(root, SRC_REL, CASE)

    shared = support.erl_function_block(source, "w301_shared_owner", CASE, source_path)
    runner = support.erl_function_block(source, "w301_run", CASE, source_path)
    if "w301_shared_owner(" not in runner["text"]:
        support.die(CASE, "w301_run/0 does not invoke w301_shared_owner/1")
    for scenario, token in TOKENS.items():
        if token is not None and token not in shared["text"]:
            support.die(CASE, "w301_shared_owner/1 lacks fixture token %r for %s"
                        % (token, scenario))

    gaps = support.w301_gaps(leaves, parsed, source, evidence["text"], TOKENS, FUNCTIONS)
    support.report_gaps(CASE, evidence, parsed, gaps)


if __name__ == "__main__":
    main()
