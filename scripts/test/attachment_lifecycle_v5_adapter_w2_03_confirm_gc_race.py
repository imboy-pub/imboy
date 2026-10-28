#!/usr/bin/env python3
"""W2-03/confirm-gc-race adapter.

Measures both commit orders from the real two-connection w203 barrier:
`w203_run` must appear as ok in eunit-w203.log, the module's own pass count
must be non-zero and failure-free, and the source must still define the
intent-first / confirm-first barrier with a real FOR UPDATE row lock.
The enterprise leaves share the same restricted fn_v5_confirm_pending
lifecycle guard; the disclosure assertion states that the suite has no
separate enterprise fixture instead of claiming one.
"""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-03/confirm-gc-race"
NAME = "w2_03_confirm_gc_race"
SOURCE = os.path.join("test", "repo", "attachment_lifecycle_v5_pg_tests.erl")
DELTA = {
    "confirm_commits_first": "confirm commits first: intent is rejected (23510 family), no attachment_object_deletion row is created, the confirmed lifecycle_state survives, and the bounded retry returns the original attachment id",
    "intent_commits_first": "intent commits first: the confirm replay fails 23510, lifecycle_state stays delete_intent, and no metadata/reference is recreated",
}
OBJECT = {
    "confirm_commits_first": "confirmed bytes remain; no GC deletion task owns the object",
    "intent_commits_first": "the intent owns the object for bounded GC deletion; confirm cannot recreate metadata",
}


def build():
    run_dir, case_dir, root = C.argv_paths()
    started = C.now_utc()
    ctx = C.load_context(run_dir)
    leaves = C.case_leaves(ctx["contract"], CASE_ID)
    slot = C.resolve_slot(run_dir)
    pool = C.db_resource(ctx["allowlist"], slot, "pool")
    gaps = C.Gaps()

    log_path = os.path.join(case_dir, "eunit-w203.log")
    text = C.ANSI_RE.sub("", C.read_text(log_path, "w203 eunit log"))
    result = C.eunit_result(log_path, "w203 barrier suite")
    if result["total"] < 1:
        gaps.add("%s reports zero passed tests; the w203 barrier did not run"
                 % log_path)
    run_line = re.search(
        r"attachment_lifecycle_v5_pg_tests:\s*w203_run\.\.\.[^\n]*?\bok\b",
        text)
    if not run_line:
        gaps.add("%s has no 'attachment_lifecycle_v5_pg_tests: w203_run... "
                 "ok' line; the two-order barrier result is not measured"
                 % log_path)
    total_line = re.search(r"(?:All\s+)?(\d+)\s+tests?\s+passed\b", text)
    if not total_line:
        gaps.add("%s has no 'N tests passed' summary" % log_path)

    src_path = os.path.join(root, SOURCE)
    src = C.read_text(src_path, "w203 suite source")
    intent_line = C.source_line(src, "w203_intent_first(")
    confirm_line = C.source_line(src, "w203_confirm_first(")
    lock_line = C.source_line(src, "FOR UPDATE")
    if intent_line is None or confirm_line is None:
        gaps.add("%s lacks w203_intent_first/w203_confirm_first barrier "
                 "functions" % src_path)
    if lock_line is None:
        gaps.add("%s lacks the FOR UPDATE row-lock barrier" % src_path)

    env_host = os.environ.get("V5_INTTEST_W203_PG_HOST", "")
    env_port = os.environ.get("V5_INTTEST_W203_PG_PORT", "")
    if not env_host or not env_port:
        gaps.add("V5_INTTEST_W203_PG_HOST/PORT are absent; the real marker DB "
                 "target cannot be proven for this receipt")

    rows = []
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        if scenario == "confirm_commits_first":
            fn = "w203_confirm_first"
            fn_line = confirm_line
        elif scenario == "intent_commits_first":
            fn = "w203_intent_first"
            fn_line = intent_line
        else:
            gaps.add("no w203 mapping for scenario=%s" % scenario)
            continue
        assertions = [
            {
                "id": "w203_run_ok",
                "passed": True,
                "detail": "%s: %r" % (os.path.basename(log_path),
                                      run_line.group(0).strip())
                if run_line else "",
            },
            {
                "id": "w203_suite_total_passed",
                "passed": True,
                "detail": "%s: %r (module contains r20/w203/w301 generators; "
                          "the w203_run line is required separately)"
                          % (os.path.basename(log_path),
                             total_line.group(0).strip() if total_line else ""),
            },
            {
                "id": "scenario_function_present",
                "passed": True,
                "detail": "%s:%d defines %s() with the two-connection barrier; "
                          "FOR UPDATE row lock at line %d"
                          % (SOURCE, fn_line, fn, lock_line),
            },
            {
                "id": "real_marker_db_target",
                "passed": True,
                "detail": "V5_INTTEST_W203_PG_HOST=%s V5_INTTEST_W203_PG_PORT=%s"
                          % (env_host, env_port),
            },
        ]
        if leaf["domain"] == "enterprise":
            assertions.append({
                "id": "enterprise_shared_fn_path_disclosed",
                "passed": True,
                "detail": "no separate enterprise fixture runs in w203; both "
                          "domains share the restricted fn_v5_confirm_pending "
                          "lifecycle guard (migration 181) measured by w203_run",
            })
        oracle = {
            "pre_db_state": "isolated PostgreSQL; w203 marker DB provisioned "
                            "from V5_INTTEST_W203_* on app pool attv5_%s_pool"
                            % slot,
            "pre_object_hash": "n/a (state/queue protocol leaf; w203 adjudicates "
                               "row state, not object bytes)",
            "action": "direct erl eunit:test([attachment_lifecycle_v5_pg_tests],"
                      " [verbose]) -> w203_run (%s)" % fn,
            "expected_http_semantics": "no HTTP surface; restricted "
                                       "fn_v5_confirm_pending / "
                                       "fn_v5_intent_ordinary_delete protocol",
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": OBJECT[scenario],
            "proof_actor": "epgsql imboy_user on the transient w203 marker DB; "
                           "real second connection holds the attachment row lock",
        }
        actor = {"kind": "real", "driver": "none",
                 "resource_kind": "scratch_postgres", "resource_id": pool}
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [pool],
            ["eunit:attachment_lifecycle_v5_pg_tests:w203_run:%s" % fn]))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
