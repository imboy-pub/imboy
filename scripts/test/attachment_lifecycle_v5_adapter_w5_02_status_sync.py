#!/usr/bin/env python3
"""W5-02/status-sync measured adapter.

Binds the widget/seat status-sync leaves to the real Bun JUnit testcases from
the gate's Admin battery.  The W5-02 files are unit / mocked-transport / JSDOM
suites; their own describe blocks state they are not browser/API/SQL
acceptance.  This adapter therefore only claims a leaf when an executed test
asserts exactly that scenario's recovery/identity behaviour, and reports the
remaining scenarios as named evidence gaps (no suite flag is used as a leaf).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w5_support as S  # noqa: E402

CASE_ID = "W5-02/status-sync"
NAME = "w5_02_status_sync"

RECOVERY = "src/modules/customer_service/seat/workbench/seatSend.attachmentRecovery.test.tsx"
REVOCATION = "src/modules/customer_service/seat/workbench/seatSend.revocation.test.tsx"
WORKBENCH_API = "src/modules/customer_service/seat/workbench/workbenchApi.test.ts"
WIDGET_API = "src/widget/customer_service/app/widgetApi.test.ts"
UPLOADER = "src/widget/customer_service/app/uploader.test.ts"

WIDGET_GAP = (
    "the W5-02 widget suites are unit/mock tests: %s exists for status/renew "
    "identity and %s exists for presign 422/PUT failure+retry, but neither "
    "asserts this scenario's timeout/restart/journal behaviour"
    % (WIDGET_API, UPLOADER))

MAPPING = {
    "refresh": {
        "tests": [(RECOVERY,
                   "refresh enumerates confirmed seat task and blocks "
                   "re-publish when message result is unknown")],
        "http": "refresh re-queries the authoritative message result and does "
                "not re-publish while the result is unknown",
        "db": "the durable confirmed task is enumerated; no duplicate business "
              "publish is written",
        "object": "the confirmed asset is reused, not re-uploaded",
    },
    "process_restart": {
        "tests": [
            (RECOVERY,
             "confirmed before dispatch survives restart and first-sends the "
             "original identity once"),
            (RECOVERY,
             "seat lost send response persists intent across restart and only "
             "queries original task"),
        ],
        "http": "after restart the persisted intent queries only the original "
                "message identity; no second publish is issued",
        "db": "the durable intent/checkpoint survives the restart with the same "
              "identities",
        "object": "the originally confirmed asset is reused",
    },
    "seat_authorization_revoked": {
        "tests": [
            (REVOCATION,
             "403确定拒绝保稿且不清登录；send/retry和编辑均不能直接重放"),
            (REVOCATION, "UNKNOWN+403允许只查询结果，不显示直接重发"),
        ],
        "http": "403 is treated as a definite rejection; only result queries "
                "remain allowed and no POST replay is issued",
        "db": "the draft/journal entry is retained according to the 403 branch; "
              "no successful publish is recorded",
        "object": "no asset write after revocation",
    },
    "seat_durable_checkpoint_failure": {
        "tests": [(RECOVERY,
                   "seat dispatch checkpoint denial stops before business send")],
        "http": "when the durable dispatch checkpoint is denied, no business "
                "send request is issued",
        "db": "the denied checkpoint prevents the business-send transition",
        "object": "no object upload is attempted after the denial",
    },
    "seat_reselected_different_bytes_rejected": {
        "tests": [(RECOVERY,
                   "reselected different bytes cannot replace a registered "
                   "recovery task")],
        "http": "a re-selected file with different bytes cannot replace the "
                "registered task; no new upload capability is used",
        "db": "the registered recovery task keeps its original content identity",
        "object": "the originally registered bytes remain the only object",
    },
}

GAPS = {
    "widget_timeout": "no executed widget test induces a timeout; the widget "
                      "suites resolve stubs immediately and only the seat "
                      "recovery suite has response-loss cases",
    "seat_timeout": "no executed seat test induces an HTTP timeout; %s covers "
                    "explicit 4xx rejections and unknown message results but "
                    "not a socket/query timeout" % RECOVERY,
    "widget_presign_response_lost": "no executed widget test drops the presign "
                                    "response; %s covers presign 422/PUT "
                                    "failure+retry, not response loss" % UPLOADER,
    "widget_restart_before_put": "no executed widget test restarts after "
                                 "presign and before PUT; the widget uploader "
                                 "suite is single-process",
    "widget_expired_capability_resume": "no executed widget test expires a "
                                        "capability and resumes; %s only "
                                        "asserts status/renew retain the "
                                        "installation and UUID" % WIDGET_API,
    "widget_account_switch_late_callback": "no executed widget test switches "
                                           "accounts and delivers a late "
                                           "callback; the widget subject is an "
                                           "installation/session, and no test "
                                           "asserts journal isolation",
    "widget_workspace_switch_late_callback": "no executed widget test switches "
                                             "workspace and delivers a late "
                                             "callback; the seat revocation "
                                             "suite covers scope cancellation, "
                                             "not widget journal writes",
    "widget_authorization_revoked": "no executed widget test revokes the "
                                    "authorization/capability mid-upload; the "
                                    "only revocation tests are seat-side (%s)"
                                    % REVOCATION,
    "widget_durable_checkpoint_failure": "no executed widget test fails a "
                                         "durable checkpoint; only the seat "
                                         "suite asserts checkpoint denial (%s)"
                                         % RECOVERY,
    "seat_presign_response_lost": "no executed seat test drops a presign "
                                  "response; %s covers confirm/message-result "
                                  "loss only" % RECOVERY,
    "seat_restart_before_put": "no executed seat test restarts between presign "
                               "and PUT; %s restarts around confirmed "
                               "message dispatch" % RECOVERY,
    "seat_expired_capability_resume": "no executed seat test expires a presign "
                                      "capability and resumes; %s only asserts "
                                      "the authenticated presign route and "
                                      "stable UUID" % WORKBENCH_API,
    "seat_account_switch_late_callback": "no executed seat test switches "
                                         "account and delivers a late "
                                         "callback; %s covers re-auth/SSE "
                                         "revocation isolation, not journal "
                                         "account isolation" % REVOCATION,
    "seat_workspace_switch_late_callback": "no executed seat test asserts that "
                                           "a late callback for a switched "
                                           "workspace is refused; %s covers "
                                           "scope cancellation and draft "
                                           "cleanup" % REVOCATION,
    "widget_reselected_different_bytes_rejected": "no executed widget test "
                                                  "re-selects different bytes; "
                                                  "only the seat variant is "
                                                  "implemented (%s)" % RECOVERY,
}


def build():
    run_dir, case_dir, root = S.C.argv_paths()
    ctx = S.context(run_dir, CASE_ID, root)
    leaves = S.leaves(ctx, CASE_ID)
    admin = S.worktree(ctx, "imboyadmin", CASE_ID)

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
            gaps.add("%s: %s" % (scenario, GAPS.get(
                scenario,
                "no executed unit asserts this scenario and no gap reason is "
                "declared")))
            continue
        evidence = []
        missing = []
        for file_fragment, name in entry["tests"]:
            test = S.find_junit_test(junit, file_fragment, name)
            if test is None:
                missing.append("%s#%r not in the captured JUnit report"
                               % (file_fragment, name))
                continue
            evidence.append((file_fragment, test))
        if missing:
            gaps.add("%s: %s" % (scenario, "; ".join(missing)))
            continue
        started, finished = S.window(junit["path"], junit["duration_s"])
        assertions = [
            S.assertion(
                "junit_testcases_passed",
                "%s: %s"
                % (os.path.basename(junit["path"]),
                   "; ".join("%s#%r line=%s time=%ss"
                             % (fragment, test["name"], test["line"],
                                test["time"])
                             for fragment, test in evidence))),
            S.assertion(
                "bun_suite_green",
                "%s parsed %d testcases, no failure/error/skipped, duration=%.3fs"
                % (os.path.basename(junit["path"]), len(junit["tests"]),
                   junit["duration_s"])),
            S.assertion("executor_exit_zero", "raw.log: %r" % bun_line),
            S.assertion(
                "mock_transport_disclaimer_present",
                "suite describe blocks state the evidence is unit/mock "
                "transport only; the leaf is scored as a targeted-contract "
                "unit measurement, not browser/API/SQL acceptance"),
        ]
        oracle = {
            "pre_db_state": "JSDOM/unit harness in %s; no real database or "
                            "backend is contacted (the suites use mocked "
                            "transport)" % entry["tests"][0][0],
            "pre_object_hash": "n/a: the suite never reads or hashes a real "
                               "storage object",
            "action": "; ".join(test["name"] for _fragment, test in evidence),
            "expected_http_semantics": entry["http"],
            "expected_db_delta": entry["db"],
            "expected_object_state": entry["object"],
            "proof_actor": "repository %s; bun JUnit sha256=%s; executor line "
                           "%r; source files %s"
                           % (admin, junit["sha256"], bun_line,
                              ", ".join(sorted({fragment for fragment, _t in
                                                evidence}))),
        }
        rows.append(S.C.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": len(evidence),
             "passed": len(evidence), "failed": 0, "skipped": 0},
            oracle, assertions, actor, [admin],
            ["bun-junit:%s#%s" % (fragment, test["name"])
             for fragment, test in evidence]))
    gaps.check()
    S.C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    S.run_adapter(NAME, build)
