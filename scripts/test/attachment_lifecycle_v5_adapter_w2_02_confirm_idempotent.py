#!/usr/bin/env python3
"""W2-02/confirm-idempotent adapter.

Measures the sequential-100-replay and two-synchronized-confirm leaves from
the real-PG/real-Garage `attachment_lifecycle_v5_confirm_pg_tests` battery.
The suite log is non-verbose, so the adapter requires the exact number of
tests defined by the two generators to have passed (a skipped/empty battery
cannot masquerade as green) and cross-checks the named scenario test in the
suite source.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-02/confirm-idempotent"
NAME = "w2_02_confirm_idempotent"
SCENARIOS = {
    "sequential_100_replays": "sequential_100_replays",
    "concurrent_two_confirms": "concurrent_two_confirms",
}
DELTA = {
    "sequential_100_replays": "100 replays return the same attachment id; attachment row count stays 1, referer_time stays 1 after the first confirm, the session stays confirmed and exactly one pending row remains",
    "concurrent_two_confirms": "two synchronized confirms return the same attachment id with attachment row count 1 and referer_time 1",
}
OBJECT = {
    "sequential_100_replays": "confirmed bytes stay identical across all 100 replays (suite verifies the real Garage object)",
    "concurrent_two_confirms": "the single confirmed object is unchanged by the concurrent second confirm",
}


def build():
    run_dir, case_dir, root = C.argv_paths()
    started = C.now_utc()
    ctx = C.load_context(run_dir)
    leaves = C.case_leaves(ctx["contract"], CASE_ID)
    slot = C.resolve_slot(run_dir)
    pool = C.db_resource(ctx["allowlist"], slot, "pool")
    bucket = C.garage_bucket(run_dir, ctx["allowlist"])
    gaps = C.Gaps()
    suite = C.confirm_suite_names(root)
    result = C.assert_eunit_total(
        os.path.join(case_dir, "eunit-confirm.log"),
        "W2-02 confirm suite", suite["total"], gaps)
    log_path = os.path.join(case_dir, "eunit-confirm.log")
    text = C.read_text(log_path, "W2-02 confirm suite log")
    hash_checks = C.ANSI_RE.sub("", text).count("content_hash_mismatch")
    rows = []
    for leaf in leaves:
        domain = leaf["domain"]
        scenario = leaf["ordering_or_fault"]
        base = SCENARIOS.get(scenario)
        if not base:
            gaps.add("no confirm-idempotent mapping for scenario=%s" % scenario)
            continue
        test = ("t_" if domain == "ordinary" else "ent_") + base
        allowed = suite["ordinary"] if domain == "ordinary" else suite["enterprise"]
        if test not in allowed:
            gaps.add("suite source %s does not define %s for domain=%s"
                     % (suite["path"], test, domain))
            continue
        line = C.source_line(suite["text"], test + "()")
        if line is None:
            gaps.add("cannot locate %s in %s" % (test, suite["path"]))
            continue
        if result is None:
            continue
        assertions = [
            {
                "id": "confirm_suite_exact_total_passed",
                "passed": True,
                "detail": "%s == %d tests defined by the ordinary(%d) + "
                          "enterprise(%d) generators"
                          % (result["detail"], suite["total"],
                             len(suite["ordinary"]),
                             len(suite["enterprise"])),
            },
            {
                "id": "named_scenario_test_present",
                "passed": True,
                "detail": "%s:%d defines %s() (non-verbose suite log has no "
                          "per-test names; exact-total pass is the execution "
                          "evidence)" % (os.path.basename(suite["path"]), line,
                                         test),
            },
            {
                "id": "real_garage_suite_not_skipped",
                "passed": True,
                "detail": "garage bucket %s allowlisted; a missing W202 garage "
                          "env would yield 0 suite tests and fail the exact "
                          "total check" % bucket,
            },
        ]
        oracle = {
            "pre_db_state": "attv5_%s_pool on the isolated PostgreSQL; each "
                            "test creates fresh fixtures via the real fn_v5_* "
                            "path" % slot,
            "pre_object_hash": "suite-controlled real Garage bytes in bucket %s; "
                               "object hash verified by elib_oss:object_sha256 "
                               "(content_hash_mismatch evidence lines in log: %d)"
                               % (bucket, hash_checks),
            "action": "make eunit-local t=attachment_lifecycle_v5_confirm_pg_tests"
                      " -> %s" % test,
            "expected_http_semantics": "logic-layer entry attach_logic:confirm/5; "
                                       "the eunit battery has no HTTP surface",
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": OBJECT[scenario],
            "proof_actor": "eunit worker u9 / enterprise fixture tenant; real "
                           "PG attv5_%s_pool + real Garage bucket %s"
                           % (slot, bucket),
        }
        actor = {"kind": "real", "driver": "none",
                 "resource_kind": "scratch_postgres", "resource_id": pool}
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [pool],
            ["eunit:attachment_lifecycle_v5_confirm_pg_tests:%s" % test]))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
