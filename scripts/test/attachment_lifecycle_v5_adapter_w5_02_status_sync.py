#!/usr/bin/env python3
"""W5-02/status-sync measured adapter.

Binds the widget/seat status-sync leaves to the real Bun JUnit testcases from
the gate's Admin battery.  The W5-02 files are unit / mocked-transport / JSDOM
suites; their own describe blocks state they are not browser/API/SQL
acceptance.  This adapter therefore only claims a leaf when an executed test
asserts exactly that scenario's recovery/identity behaviour; every one of the
20 leaves binds to executed testcases (no suite flag is used as a leaf).
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
    "widget_timeout": {
        "tests": [(UPLOADER,
                   "widget_timeout: hung presign times out and stops without "
                   "retry storm")],
        "http": "a hung presign is aborted by the transport timeout; exactly "
                "one presign request is issued and the pipeline stops without "
                "any PUT or confirm retry storm",
        "db": "the durable journal records the final failed state; no "
              "duplicate upload intent is written",
        "object": "no object PUT is issued after the timeout",
    },
    "widget_presign_response_lost": {
        "tests": [(UPLOADER,
                   "widget_presign_response_lost: persisted intent resumes "
                   "with status/renew only, never re-presigns")],
        "http": "after the presign response is lost, resume issues only "
                "status then renew (never a re-presign) reusing the persisted "
                "client_upload_id",
        "db": "the journal keeps the phase='intent' row with the original "
              "UUID and is cleared after successful linking",
        "object": "the object identity (assetId) is preserved; no second "
                  "registration is created",
    },
    "widget_restart_before_put": {
        "tests": [(UPLOADER,
                   "widget_restart_before_put: page killed mid-PUT resumes "
                   "the persisted capability without re-presign")],
        "http": "restart resumes the persisted capability with status+renew "
                "only; the PUT goes to the renewed URL and confirm keeps the "
                "original asset_id",
        "db": "the journal's phase='registered' checkpoint with the persisted "
              "assetId survives the restart",
        "object": "the originally registered object identity is reused; no "
                  "re-presign is issued",
    },
    "widget_expired_capability_resume": {
        "tests": [(UPLOADER,
                   "widget_expired_capability_resume: expired PUT capability "
                   "renews with fresh URL, same object identity")],
        "http": "the expired capability is renewed to a fresh URL; PUT only "
                "hits the new URL and confirm/message carry the original "
                "assetId",
        "db": "the journal retains the original identities across the renewal",
        "object": "same object identity: new signed URL, unchanged asset",
    },
    "widget_account_switch_late_callback": {
        "tests": [(UPLOADER,
                   "widget_account_switch_late_callback: late PUT callback "
                   "after account switch is refused by journal principal "
                   "isolation")],
        "http": "the late PUT callback after a principal switch is refused; "
                "the new principal registers with a fresh client_upload_id",
        "db": "journal principal isolation: the old principal's journal stays "
              "at 'registered' and the new principal's scope has no residue "
              "of the old upload",
        "object": "the old principal's draft is retained; no object write "
                  "advances under the old principal",
    },
    "widget_workspace_switch_late_callback": {
        "tests": [(UPLOADER,
                   "widget_workspace_switch_late_callback: late PUT callback "
                   "after workspace switch is refused by journal scope "
                   "isolation")],
        "http": "the late PUT callback after a workspace (installation) "
                "switch is refused; the new workspace registers with a fresh "
                "client_upload_id and installation_id",
        "db": "journal scope isolation: the old installation's journal stays "
              "at 'registered'; the two scopes' journals are invisible to "
              "each other",
        "object": "no object write advances in either scope from the late "
                  "callback",
    },
    "widget_authorization_revoked": {
        "tests": [(UPLOADER,
                   "widget_authorization_revoked: 403 keeps the draft and "
                   "refuses upload replay")],
        "http": "confirm 403 triggers credential-refused handling; replay "
                "issues only a single status query (403) reusing the original "
                "client_upload_id — no register/renew/PUT/confirm",
        "db": "the uploaded-phase draft is retained and no success is recorded",
        "object": "no object write after revocation; the draft bytes are not "
                  "re-uploaded",
    },
    "widget_durable_checkpoint_failure": {
        "tests": [(UPLOADER,
                   "widget_durable_checkpoint_failure: checkpoint write "
                   "failure stops before PUT and keeps the intent")],
        "http": "a durable checkpoint write failure stops the pipeline before "
                "any PUT; only the single register call reaches the network",
        "db": "the journal keeps the phase='intent' first-write record and "
              "no registered checkpoint is written",
        "object": "no object upload is attempted after the checkpoint failure",
    },
    "widget_reselected_different_bytes_rejected": {
        "tests": [(UPLOADER,
                   "widget_reselected_different_bytes_rejected: different "
                   "bytes cannot replace the registered task")],
        "http": "re-selecting different bytes issues zero network calls (no "
                "new upload capability and no query of the original task)",
        "db": "the journal row keeps its original objectHash/sizeBytes/mime/"
              "clientUploadId unchanged",
        "object": "the originally registered bytes remain the only registered "
                  "content",
    },
    "seat_timeout": {
        "tests": [(WORKBENCH_API,
                   "seat_timeout: hung presign/result-query transports "
                   "classify as network and abort with a single request, no "
                   "retry storm")],
        "http": "a hung presign or result-query transport rejects as "
                "kind='network' with exactly one request; the client performs "
                "no internal retry",
        "db": "no journal mutation: the failure happens before any checkpoint "
              "write is issued",
        "object": "no object PUT or confirm is issued after the timeout",
    },
    "seat_presign_response_lost": {
        "tests": [(RECOVERY,
                   "seat_presign_response_lost: lost presign response "
                   "persists intent and resume only queries the original task "
                   "without re-presign")],
        "http": "resume after a lost presign response only queries the "
                "original task (status, renew); register is issued exactly "
                "once at the start and never re-presigned",
        "db": "the journal persists the intent checkpoint (UUID / seat-web-* "
              "message identity) across the restart and is cleared after "
              "completion",
        "object": "the same upload UUID and message identity are reused; no "
                  "duplicate object registration",
    },
    "seat_restart_before_put": {
        "tests": [(RECOVERY,
                   "seat_restart_before_put: restart between presign and PUT "
                   "reuses the persisted registration without a second "
                   "presign")],
        "http": "restart between presign and PUT reuses the persisted "
                "registration (status+renew only, no second presign); PUT "
                "succeeds exactly once after the restart",
        "db": "the journal stays at phase='registered' with a non-empty "
              "assetId across the restart; confirm/send use the original "
              "message identity",
        "object": "the registered object is uploaded once and confirmed with "
                  "the original asset",
    },
    "seat_expired_capability_resume": {
        "tests": [(RECOVERY,
                   "seat_expired_capability_resume: expired capability "
                   "resumes with a fresh presign keeping the same upload UUID "
                   "and content hash")],
        "http": "with the server session gone (status 404+intent), resume "
                "re-registers with the SAME client_upload_id and identical "
                "object_hash/size/mime, then confirms and sends with the "
                "original clientMsgId",
        "db": "the intent checkpoint survives and the re-registration reuses "
              "the original content identity",
        "object": "same content identity: fresh presign, unchanged upload "
                  "UUID and content hash",
    },
    "seat_account_switch_late_callback": {
        "tests": [(REVOCATION,
                   "seat_account_switch_late_callback: late send callback "
                   "after account switch is refused by journal account "
                   "isolation")],
        "http": "the late send success callback after an account switch is "
                "refused and no second send is issued",
        "db": "journal account isolation ([seat, userId, ...] scope): the old "
              "account keeps dispatch_intent and the new account scope has "
              "zero rows and zero writes",
        "object": "no object action advances under the old account",
    },
    "seat_workspace_switch_late_callback": {
        "tests": [(REVOCATION,
                   "seat_workspace_switch_late_callback: late send callback "
                   "after workspace switch is refused by journal scope "
                   "isolation")],
        "http": "the late callback after a workspace switch is refused by "
                "scope isolation; no second send and no error pollution in "
                "the new scope",
        "db": "the old workspace's dispatch_intent entry is preserved and the "
              "new workspace scope stays empty",
        "object": "no object action is issued from either scope after the "
                  "switch",
    },
}

# Every status-sync scenario binds to executed bun testcases above; GAPS is
# kept as an empty fallback so an unmatched future scenario still fails
# closed with a named reason instead of being silently dropped.
GAPS = {}


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
