#!/usr/bin/env python3
"""W5-01/flutter-old-client measured adapter.

Legacy-compatibility scenarios: old payload/download/Range behaviour, E2EE byte
preservation, re-seal under a new key and unknown-state safety.  Each contract
leaf binds to the exact same-named recovery-suite test captured by the W5-01
gate executor; source lines and suite digests are cited from the real run.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w5_support as S  # noqa: E402

CASE_ID = "W5-01/flutter-old-client"
NAME = "w5_01_flutter_old_client"

META = {
    "legacy_download_range": {
        "action": "seed a legacy presigned row without key_ref material and run "
                  "the recovery path",
        "http": "no new capability or upload call is invented for the legacy "
                "row; the legacy download/Range contract is left intact",
        "db": "the legacy row is tolerated without an integrity failure and "
              "without being rewritten",
        "object": "no object is created or overwritten for the legacy row",
        "pre_object": "legacy object referenced by the row; the test does not "
                      "mutate it",
    },
    "e2ee_byte_preservation": {
        "action": "seal the same deterministic plaintext twice with the same "
                  "key and nonce",
        "http": "no network call: the measurement is the local E2EE sealing "
                "operation",
        "db": "no DB mutation; the assertion is over the two sealed byte "
              "results",
        "object": "both ciphertext buffers are byte-identical (same sha256), so "
                  "an existing object stays readable with the same key",
        "pre_object": "no pre-existing provider object; ciphertext is computed "
                      "inside the test over deterministic bytes",
    },
    "new_key_reseals": {
        "action": "abandon the old material-bearing task and start a new task",
        "http": "the new task performs its own seal/upload; the old task is not "
                "replayed",
        "db": "a new task row with new key material is created; the abandoned "
              "row is not revived",
        "object": "new ciphertext differs from the old ciphertext because the "
                  "key changed",
        "pre_object": "old ciphertext remains bound to the abandoned task; it is "
                      "not re-keyed in place",
    },
    "unknown_state_no_delete": {
        "action": "insert a task row with an unknown stage value and run "
                  "resumeAll",
        "http": "no network call is made for the unknown-state row",
        "db": "the unknown-stage row is neither resumed nor deleted",
        "object": "no object operation for the unknown-state row",
        "pre_object": "no provider object is associated with the unknown row",
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
    header_line, _text = S.source_line(
        test_file, HEADER_NEEDLE, CASE_ID, "W5-01 recovery suite header")

    presign = S.flutter_suite(case_dir, "flutter-presign.jsonl", CASE_ID,
                              "W5-01 presign contract suite")
    recovery = S.flutter_suite(case_dir, "flutter-recovery.jsonl", CASE_ID,
                               "W5-01 recovery suite")
    if not any("attachment_upload_presign_test.dart" in (path or "")
               for path in presign["suite_paths"]):
        S.die(CASE_ID, "presign artifact does not name the contract suite: %r"
              % presign["suite_paths"])
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
            gaps.add("%s: no scenario metadata in the W5-01 old-client adapter"
                     % scenario)
            continue
        hits = S.find_flutter_test(recovery, scenario)
        if len(hits) != 1:
            gaps.add("%s: expected exactly one same-named recovery test, got %r"
                     % (scenario, [hit["name"] for hit in hits]))
            continue
        test = hits[0]
        line, _ = S.source_line(test_file, "test('%s:" % scenario, CASE_ID,
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
                            "harness (%s:%d)"
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
