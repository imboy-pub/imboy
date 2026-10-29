#!/usr/bin/env python3
"""W5-02/admin-governance measured adapter.

The gate executor runs the contract's eight Admin test files with Bun and a
JUnit reporter in the run-owned imboyadmin worktree.  This adapter binds the
governance leaves to the real `StorageOverviewPage.test.tsx` testcases that
assert logical vs physical lifecycle display and unknown-state preservation.

All three leaves bind to executed `StorageOverviewPage.test.tsx` testcases:
the physical-state matrix for logical/physical display, and a dedicated
authorized_pagination_entityid testcase asserting that pagination params
reach GET /storage/index and that a big EntityId id round-trips as a string
through render → dialog → POST body (with a numeric-precision sentinel).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w5_support as S  # noqa: E402

CASE_ID = "W5-02/admin-governance"
NAME = "w5_02_admin_governance"

PAGE = "src/pages/storage/StorageOverviewPage.tsx"
PAGE_TEST = "src/pages/storage/StorageOverviewPage.test.tsx"

PHYSICAL_STATES = ["ready", "retry_wait", "blocked", "done", "future_state"]

MAPPING = {
    "logical_physical_states": {
        "tests": [(PAGE_TEST,
                   "shows logical and physical %s independently" % state)
                  for state in PHYSICAL_STATES],
        "action": "render every logical=delete_intent / physical=<state> row and "
                  "assert the row shows both dimensions independently",
        "db": "no DB mutation: the UI projects the mocked list payload",
        "object": "no storage mutation; only the governance label is rendered",
    },
    "authorized_pagination_entityid": {
        "tests": [(PAGE_TEST,
                   "authorized_pagination_entityid: pagination params reach "
                   "GET /storage/index and big EntityId id round-trips as "
                   "string")],
        "action": "drive the real page: initial page=1/size=10 request "
                  "params, next-page page=2, page-size 50 with page reset to "
                  "1, and the EntityId '8100000000000000001' round-tripping "
                  "from the list render through the disable dialog into the "
                  "POST /storage/disable body as a string",
        "db": "no DB mutation: the governance page projects the mocked list "
              "payload and the disable action targets the stub client",
        "object": "no storage mutation; only governance actions are exercised",
    },
    "unknown_state_preserved": {
        "tests": [(PAGE_TEST,
                   "shows logical and physical future_state independently")],
        "action": "render physical_state=future_state and assert the UI shows "
                  "'物理状态未知' while hiding the destructive force action",
        "db": "no DB mutation: the UI projects the mocked list payload",
        "object": "no storage mutation; unknown state stays unknown",
    },
}


def build():
    run_dir, case_dir, root = S.C.argv_paths()
    ctx = S.context(run_dir, CASE_ID, root)
    leaves = S.leaves(ctx, CASE_ID)
    admin = S.worktree(ctx, "imboyadmin", CASE_ID)

    page_test_path = os.path.join(admin, PAGE_TEST)
    page_path = os.path.join(admin, PAGE)
    header_line, _ = S.source_line(
        page_test_path, "it.each(['ready', 'retry_wait', 'blocked', 'done', "
                        "'future_state'])", CASE_ID,
        "StorageOverviewPage physical-state matrix")
    label_line, _ = S.source_line(
        page_test_path, "future_state: '物理状态未知'", CASE_ID,
        "unknown-state label")
    force_line, _ = S.source_line(
        page_test_path, "not.toContain('一键物理删除')", CASE_ID,
        "force-delete absence assertion")
    page_params_line, _ = S.source_line(
        page_path, "page: params.page", CASE_ID, "page pagination params")
    page_size_line, _ = S.source_line(
        page_path, "size: params.pageSize", CASE_ID, "pageSize params")

    junit = S.junit_suite(case_dir, "bun-junit.xml", CASE_ID, "W5-02 bun suite")
    bun_rc, bun_line = S.command_exit(case_dir, "CMD[bun]", CASE_ID)
    if bun_rc != 0:
        S.die(CASE_ID, "executor recorded bun exit=%d" % bun_rc)

    actor = S.repo_actor(admin)
    rows = []
    gaps = S.C.Gaps()
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        entry = MAPPING.get(scenario)
        if entry is None:
            gaps.add("%s: no scenario mapping in the W5-02 governance adapter"
                     % scenario)
            continue
        if entry.get("gap"):
            gaps.add("%s: %s" % (scenario, entry["gap"]))
            continue
        evidence = []
        missing = []
        for file_fragment, name in entry["tests"]:
            test = S.find_junit_test(junit, file_fragment, name)
            if test is None:
                missing.append("%r not in captured JUnit report" % name)
                continue
            evidence.append(test)
        if missing:
            gaps.add("%s: %s" % (scenario, "; ".join(missing)))
            continue
        started, finished = S.window(junit["path"], junit["duration_s"])
        assertions = [
            S.assertion(
                "junit_testcases_passed",
                "%s: %s"
                % (os.path.basename(junit["path"]),
                   "; ".join("%r line=%s time=%ss"
                             % (test["name"], test["line"], test["time"])
                             for test in evidence))),
            S.assertion(
                "bun_suite_green",
                "%s parsed %d testcases, no failure/error/skipped, duration=%.3fs"
                % (os.path.basename(junit["path"]), len(junit["tests"]),
                   junit["duration_s"])),
            S.assertion(
                "executor_exit_zero", "raw.log: %r" % bun_line),
            S.assertion(
                "physical_state_matrix_source",
                "%s:%d enumerates ready/retry_wait/blocked/done/future_state"
                % (os.path.basename(page_test_path), header_line)),
        ]
        if scenario == "unknown_state_preserved":
            assertions.extend([
                S.assertion(
                    "unknown_label_asserted",
                    "%s:%d maps future_state to '物理状态未知'"
                    % (os.path.basename(page_test_path), label_line)),
                S.assertion(
                    "destructive_action_absent",
                    "%s:%d asserts the rendered text does not contain "
                    "'一键物理删除'" % (os.path.basename(page_test_path),
                                      force_line)),
            ])
        oracle = {
            "pre_db_state": "UI unit fixture: %s:%d seeds a row with status=-1, "
                            "lifecycle_state='delete_intent' and "
                            "physical_state=<case>; no real database is used"
                            % (os.path.basename(page_test_path), header_line),
            "pre_object_hash": "n/a: the governance page is a read-only "
                               "projection; no storage object is read or hashed",
            "action": "%s: %s (%s:%s)"
                      % (entry["action"],
                         "; ".join(test["name"] for test in evidence),
                         os.path.basename(page_test_path),
                         evidence[0]["line"]),
            "expected_http_semantics": "GET /storage/stats and /storage/index "
                                       "are served by the test's mutableClient "
                                       "stub (unit layer, not a real backend)",
            "expected_db_delta": entry["db"],
            "expected_object_state": entry["object"],
            "proof_actor": "repository %s; bun JUnit sha256=%s; executor line "
                           "%r; page params cited at %s:%d/%d"
                           % (admin, junit["sha256"], bun_line,
                              os.path.basename(page_path), page_params_line,
                              page_size_line),
        }
        rows.append(S.C.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": len(evidence),
             "passed": len(evidence), "failed": 0, "skipped": 0},
            oracle, assertions, actor, [admin],
            ["bun-junit:%s#%s" % (PAGE_TEST, test["name"])
             for test in evidence]))
    gaps.check()
    S.C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    S.run_adapter(NAME, build)
