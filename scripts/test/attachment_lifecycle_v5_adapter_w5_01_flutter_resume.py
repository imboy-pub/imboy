#!/usr/bin/env python3
"""W5-01/flutter-resume measured adapter.

Source of measurement: the real `flutter test --reporter json` runs captured by
the W5-01 gate executor for
  test/unit_test/store/attachment_upload_presign_test.dart
  test/unit_test/store/attachment_upload_task_recovery_test.dart
in the run-owned imboyapp worktree.  Each contract scenario is bound to the
exact same-named test (the suite prefixes every test name with its scenario
id); a missing/failed/skipped test or a failed suite is an evidence gap.

The suite header itself states that these unit tests do not replace a real
server/device acceptance run.  The adapter records that limitation verbatim in
every oracle instead of presenting the unit row as a physical-device result;
W6-02 owns the real-device rerun.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w5_support as S  # noqa: E402

CASE_ID = "W5-01/flutter-resume"
NAME = "w5_01_flutter_resume"

META = {
    "kill_after_registered": {
        "action": "seed a pending task row, rebuild the uploader on the same DB "
                  "(process restart) and resume",
        "http": "no provider call before presign; resume re-runs "
                "presign -> PUT -> confirm exactly once",
        "db": "the same task row advances pending->confirmed; no duplicate row",
        "object": "no object exists before PUT; the resumed PUT uses the same "
                  "object key",
        "pre_object": "no pre-existing provider object: nothing was sealed or "
                      "uploaded before the simulated kill",
    },
    "kill_after_put": {
        "action": "kill at the putDone checkpoint after a successful PUT and "
                  "resume with a new uploader",
        "http": "resume performs no second PUT and confirms the same object key "
                "once (fake confirm count stays 1)",
        "db": "put_done task row advances to confirmed without a second task row",
        "object": "the single already-uploaded object key is confirmed, not "
                  "re-uploaded",
        "pre_object": "the first PUT already stored the deterministic ciphertext "
                      "for the object key",
    },
    "kill_after_confirm": {
        "action": "replay the same task id after confirm and resume",
        "http": "the confirmed task is terminal: resume performs zero network "
                "calls and returns the original object key",
        "db": "row remains confirmed; no stage regression and no new row",
        "object": "the original object key is returned unchanged",
        "pre_object": "the confirmed object from the first run is untouched",
    },
    "unknown_message_commit": {
        "action": "attempt a late/unknown callback CAS that moves a confirmed "
                  "row back to put_done",
        "http": "no additional business send is triggered by the refused CAS",
        "db": "the CAS is refused; the terminal confirmed row is unchanged",
        "object": "object state is untouched by the refused callback",
        "pre_object": "the confirmed object from the completed upload",
    },
    "user_switch": {
        "action": "resume under a different uid while another user's task row "
                  "exists",
        "http": "no upload/confirm call is made for the other account's row",
        "db": "only the current account's rows are selected; the other user's "
              "row is neither resumed nor mutated",
        "object": "no object operation for the other account's task",
        "pre_object": "no object for the other account is touched",
    },
    "presign_response_lost": {
        "action": "make presign raise while the task is pending, then resume "
                  "with presign restored",
        "http": "the failed presign leaves the task pending with no PUT; the "
                "resumed run performs presign -> PUT -> confirm once",
        "db": "row stays pending after the loss and advances after resume",
        "object": "no object is created by the failed presign; the resumed PUT "
                  "creates it once",
        "pre_object": "no provider object exists for the pending task",
    },
    "restart_before_put": {
        "action": "kill at the sealed checkpoint after key material is persisted "
                  "and resume before PUT",
        "http": "the resumed run PUTs the already sealed ciphertext without "
                "re-sealing",
        "db": "the same row keeps the same upload identity through sealed/put",
        "object": "the PUT body is the same ciphertext sealed before the kill",
        "pre_object": "the sealed ciphertext exists locally but no object has "
                      "been uploaded yet",
    },
    "expired_capability_resume": {
        "action": "fail confirm as an expired capability while the checkpoint is "
                  "retained, then resume",
        "http": "confirm failure keeps the upload identity and records the error; "
                "resume re-queries/renews instead of inventing success",
        "db": "the task row retains the checkpoint and the recorded failure",
        "object": "the uploaded object is not re-created or overwritten",
        "pre_object": "the ciphertext object exists from the put_done stage",
    },
    "account_switch_late_callback": {
        "action": "switch account and let the old task's callback arrive late",
        "http": "no network write is performed for the old account after switch",
        "db": "the late callback CAS is refused; the old row is unchanged",
        "object": "no object write for the old account",
        "pre_object": "the object from the pre-switch account remains untouched",
    },
    "workspace_switch_late_callback": {
        "action": "reuse the same idempotency key with a changed scopeRef after "
                  "the confirmed upload",
        "http": "the confirmed pipeline returns the original result; no second "
                "publish for the other workspace",
        "db": "no second task row is created for the changed workspace scope",
        "object": "the single confirmed object is reused, not re-uploaded",
        "pre_object": "the workspace A object exists and stays unchanged",
    },
    "authorization_revoked": {
        "action": "make confirm return 403 (authorization revoked) and observe "
                  "the task outcome",
        "http": "the 403 is terminal for the attempt; the task is abandoned with "
                "the reason and no success is fabricated",
        "db": "row records the abandon reason; it is not advanced to confirmed",
        "object": "no confirmed object is claimed for the revoked authorization",
        "pre_object": "the PUT object exists but stays unconfirmed",
    },
    "durable_checkpoint_failure": {
        "action": "fail the key-material write at the sealed checkpoint",
        "http": "the upload aborts before any PUT; no confirm is attempted",
        "db": "the row does not advance past presigned when the checkpoint write "
              "fails",
        "object": "no object write occurs",
        "pre_object": "no provider object for the aborted task",
    },
    "same_key_same_ciphertext_after_restart": {
        "action": "kill at sealed, restart, and resume the same task",
        "http": "resume PUTs the persisted ciphertext without a new random seal",
        "db": "the same key/cipher descriptor is reused for the same task",
        "object": "ciphertext bytes after restart equal the pre-kill bytes",
        "pre_object": "the locally sealed ciphertext file persisted by the "
                      "implementation",
    },
    "lost_cipher_material_requires_new_task": {
        "action": "remove the cipher material for a presigned task and resume",
        "http": "no re-seal under the old key and no PUT; the task is explicitly "
                "abandoned",
        "db": "the old row is abandoned (new task/new key required) instead of "
              "being silently resumed",
        "object": "no object is written under the old key",
        "pre_object": "the expected ciphertext file is absent",
    },
}

HEADER_NEEDLE = "W5-01 客户端恢复单元测试（不代替真实服务、设备或消息发送验收）"


def build():
    run_dir, case_dir, root = S.C.argv_paths()
    ctx = S.context(run_dir, CASE_ID, root)
    leaves = S.leaves(ctx, CASE_ID)
    app = S.worktree(ctx, "imboyapp", CASE_ID)
    test_file = os.path.join(
        app, "test", "unit_test", "store",
        "attachment_upload_task_recovery_test.dart")
    header_line, header_text = S.source_line(
        test_file, HEADER_NEEDLE, CASE_ID, "W5-01 recovery suite header")

    presign = S.flutter_suite(case_dir, "flutter-presign.jsonl", CASE_ID,
                              "W5-01 presign contract suite")
    recovery = S.flutter_suite(case_dir, "flutter-recovery.jsonl", CASE_ID,
                               "W5-01 recovery suite")
    if not any("attachment_upload_presign_test.dart" in (path or "")
               for path in presign["suite_paths"]):
        S.die(CASE_ID, "presign artifact does not name the contract suite: %r"
              % presign["suite_paths"])
    if not any("attachment_upload_task_recovery_test.dart" in (path or "")
               for path in recovery["suite_paths"]):
        S.die(CASE_ID, "recovery artifact does not name the recovery suite: %r"
              % recovery["suite_paths"])
    presign_rc, presign_line = S.command_exit(case_dir, "CMD[presign]", CASE_ID)
    recovery_rc, recovery_line = S.command_exit(
        case_dir, "CMD[recovery]", CASE_ID)
    if presign_rc != 0 or recovery_rc != 0:
        S.die(CASE_ID, "executor recorded a non-zero test exit: presign=%d "
              "recovery=%d" % (presign_rc, recovery_rc))

    actor = S.repo_actor(app)
    rows = []
    gaps = S.C.Gaps()
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        meta = META.get(scenario)
        if meta is None:
            gaps.add("%s: no scenario metadata in the W5-01 adapter" % scenario)
            continue
        hits = S.find_flutter_test(recovery, scenario)
        if len(hits) != 1:
            gaps.add("%s: expected exactly one same-named recovery test, got %r"
                     % (scenario, [hit["name"] for hit in hits]))
            continue
        test = hits[0]
        line, _text = S.source_line(
            test_file, "test('%s:" % scenario, CASE_ID,
            "W5-01 scenario %s" % scenario)
        started, finished = S.window(recovery["path"], recovery["duration_s"])
        assertions = [
            S.assertion(
                "flutter_recovery_test_passed",
                "%s: test %r result=%s skipped=%s testDone_ms=%s "
                "(suite sha256=%s)"
                % (os.path.basename(recovery["path"]), test["name"],
                   test["result"], test["skipped"], test["time_ms"],
                   recovery["sha256"])),
            S.assertion(
                "recovery_suite_green",
                "%s parsed %d visible tests, done.success=true, duration=%.3fs"
                % (os.path.basename(recovery["path"]),
                   len(recovery["tests"]), recovery["duration_s"])),
            S.assertion(
                "presign_contract_suite_green",
                "%s parsed %d visible tests, done.success=true; executor line %r"
                % (os.path.basename(presign["path"]), len(presign["tests"]),
                   presign_line)),
            S.assertion(
                "executor_exit_zero",
                "raw.log: %r; %r" % (presign_line, recovery_line)),
            S.assertion(
                "scenario_source_line",
                "%s:%d declares test('%s:...)"
                % (os.path.basename(test_file), line, scenario)),
        ]
        oracle = {
            "pre_db_state": "sqflite_common_ffi in-memory DB with the v33 "
                            "attachment_upload_task DDL installed by the suite "
                            "harness (%s:%d); rows are the real rows this test "
                            "inserted"
                            % (os.path.basename(test_file), header_line),
            "pre_object_hash": meta["pre_object"],
            "action": "%s (%s:%d)"
                      % (test["name"], os.path.basename(test_file), line),
            "expected_http_semantics": meta["http"],
            "expected_db_delta": meta["db"],
            "expected_object_state": meta["object"],
            "proof_actor": "repository %s; flutter test pid=%d; recovery suite "
                           "sha256=%s; presign suite sha256=%s; client-unit "
                           "harness (header %s:%d states it does not replace "
                           "real server/device acceptance; W6-02 owns that run)"
                           % (app, recovery["pid"], recovery["sha256"],
                              presign["sha256"], os.path.basename(test_file),
                              header_line),
        }
        rows.append(S.C.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [app],
            ["flutter:%s#%s" % (os.path.basename(test_file), test["name"])]))
    gaps.check()
    S.C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    S.run_adapter(NAME, build)
