#!/usr/bin/env python3
"""W6-03/final-regression measured adapter.

Seven PLAN leaves, one per real command the gate executes on the frozen
candidate: backend full EUnit, every backend suite excluded from the full VM
(run individually), Flutter analyze/test, Admin test/build/lint.

Each leaf binds to a real step window/exit code recorded in CASE_DIR/raw.log and
to the command's own log.  A missing log, non-zero exit, failed/skipped test or
absent build artifact is an evidence gap; a suite-level flag never becomes a
leaf PASS on its own.
"""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402
import attachment_lifecycle_v5_adapter_w5_support as S5  # noqa: E402

CASE_ID = "W6-03/final-regression"
NAME = "w6_03_final_regression"

SUMMARY_RE = re.compile(r"^\s*(?:All (\d+) tests passed\.|(\d+) tests passed\.|"
                        r"Test passed\.)\s*$", re.M)
FAIL_RE = re.compile(r"(?:\bFailed:\s*[1-9]\d*|\*\*?failed\*\*?|"
                     r"\b\d+ tests? failed|\bCancelled\b)", re.I)
BUN_PASS_RE = re.compile(r"^\s*(\d+)\s+pass\b", re.M)
BUN_FAIL_RE = re.compile(r"^\s*(\d+)\s+fail\b", re.M)
EXCL_RE = re.compile(r"Excl = \[(.*?)\]", re.S)
STEP_LABELS = {
    "backend_full_eunit": "eunit-full",
    "flutter_analyze": "flutter-analyze",
    "flutter_test": "flutter-test",
    "admin_test": "admin-test",
    "admin_build": "admin-build",
    "admin_lint": "admin-lint",
}


def excluded_modules(root, case_id):
    path = os.path.join(root, "Makefile")
    text = S6.read_text(path, case_id, "backend Makefile")
    match = EXCL_RE.search(text)
    if not match:
        S6.die(case_id, "no Excl = [...] list found in %s" % path)
    modules = [item.strip() for item in match.group(1).split(",")
               if item.strip()]
    if not modules:
        S6.die(case_id, "Excl list in %s is empty" % path)
    return modules, path


def eunit_summary(path, case_id, role):
    text = S6.read_text(path, case_id, role)
    if FAIL_RE.search(text):
        S6.die(case_id, "%s reports failures/cancellations: %s"
              % (role, path))
    matches = SUMMARY_RE.findall(text)
    if not matches:
        S6.die(case_id, "%s has no 'N tests passed' summary: %s"
              % (role, path))
    counts = [int(item[0] or item[1]) if (item[0] or item[1]) else 1
              for item in matches]
    total = max(counts)
    if total < 1:
        S6.die(case_id, "%s summary reports zero tests: %s" % (role, path))
    return total, S6.sha256_file(path)


def build():
    run_dir, case_dir, root = S6.C.argv_paths()
    ctx = S6.context(run_dir, CASE_ID, root)
    leaves = S6.leaves(ctx, CASE_ID)
    expected = {leaf["ordering_or_fault"]: leaf for leaf in leaves}
    app = S6.worktree(ctx, "imboyapp", CASE_ID)
    admin = S6.worktree(ctx, "imboyadmin", CASE_ID)
    try:
        slot = S6.C.resolve_slot(run_dir)
        pool_db = S6.C.db_resource(ctx["allowlist"], slot, "pool")
    except S6.C.AdapterGap as exc:
        S6.die(CASE_ID, str(exc))
    modules, makefile = excluded_modules(root, CASE_ID)

    gaps = []
    rows = {}

    def step(label):
        try:
            return S6.step_record(case_dir, label, CASE_ID)
        except SystemExit:
            raise
        except Exception as exc:  # noqa: BLE001
            gaps.append("%s: %s" % (label, exc))
            return None

    def add(scenario, repo, oracle_action, log_path, extra_assertions,
            counts=1, step_label=None):
        record = step(step_label or scenario)
        if record is None:
            return
        if record["exit"] != 0:
            gaps.append("%s: executor recorded %r"
                        % (scenario, record["line"]))
            return
        try:
            started, finished = S6.step_window(record, CASE_ID)
        except SystemExit as exc:
            gaps.append("%s: %s" % (scenario, exc))
            return
        if not os.path.isfile(log_path):
            gaps.append("%s: missing command log %s" % (scenario, log_path))
            return
        assertions = list(extra_assertions)
        assertions.append(S6.assertion(
            "executor_step_recorded", record["line"]))
        assertions.append(S6.assertion(
            "command_log_present",
            "%s sha256=%s" % (os.path.basename(log_path),
                              S6.sha256_file(log_path))))
        rows[scenario] = (repo, started, finished, counts, assertions,
                          oracle_action, log_path)

    # 1. backend full eunit
    full_log = os.path.join(case_dir, "eunit-full.log")
    if not os.path.isfile(full_log):
        gaps.append("backend_full_eunit: missing %s" % full_log)
    else:
        try:
            total, digest = eunit_summary(full_log, CASE_ID,
                                          "backend full eunit")
            add("backend_full_eunit", root,
                "make eunit-local (full candidate suite, isolated config)",
                full_log,
                [S6.assertion(
                    "full_eunit_summary",
                    "All %d tests passed (log sha256=%s); isolated DB %s"
                    % (total, digest, pool_db))],
                counts=total,
                step_label=STEP_LABELS["backend_full_eunit"])
        except SystemExit:
            raise

    # 2. excluded suites, one real single-module run each
    failing = []
    missing = []
    for module in modules:
        log = os.path.join(case_dir, "excl-%s.log" % module)
        record = step("excl-%s" % module)
        if record is None or record["exit"] != 0:
            failing.append("%s exit=%r"
                           % (module, record["exit"] if record else "missing"))
            continue
        if not os.path.isfile(log):
            missing.append("%s missing log" % module)
            continue
        try:
            eunit_summary(log, CASE_ID, "excluded suite %s" % module)
        except SystemExit:
            raise
    if failing or missing:
        gaps.append("backend_required_excluded_suites: %d/%d modules "
                    "failing/missing (%s%s)"
                    % (len(failing) + len(missing), len(modules),
                       ", ".join(failing[:5]),
                       "; " + ", ".join(missing[:3]) if missing else ""))
    else:
        add("backend_required_excluded_suites", root,
            "make eunit-local t=<module> for every Excl module in %s"
            % os.path.basename(makefile),
            os.path.join(case_dir, "excl-%s.log" % modules[0]),
            [S6.assertion(
                "all_excluded_suites_green",
                "%d/%d excluded modules executed individually with a passing "
                "summary: %s" % (len(modules), len(modules),
                                 ", ".join(modules))),
             ], counts=len(modules), step_label="excl-%s" % modules[0])

    # 3. flutter analyze
    analyze_log = os.path.join(case_dir, "flutter-analyze.log")
    if os.path.isfile(analyze_log):
        text = S6.read_text(analyze_log, CASE_ID, "flutter analyze")
        if "No issues found!" not in text:
            gaps.append("flutter_analyze: log does not say 'No issues found!'")
        else:
            add("flutter_analyze", app,
                "flutter analyze (imboyapp worktree)", analyze_log,
                [S6.assertion("flutter_analyze_clean",
                              "'No issues found!' present")],
                step_label=STEP_LABELS["flutter_analyze"])

    # 4. flutter test (JSON reporter)
    flutter_json = os.path.join(case_dir, "flutter-test.jsonl")
    if os.path.isfile(flutter_json):
        try:
            suite = S5.flutter_suite(case_dir, "flutter-test.jsonl", CASE_ID,
                                     "flutter full test")
            add("flutter_test", app, "flutter test (full imboyapp suite)",
                flutter_json,
                [S6.assertion(
                    "flutter_test_suite_green",
                    "%d visible tests succeeded, done.success=true, "
                    "duration=%.3fs" % (len(suite["tests"]),
                                       suite["duration_s"]))],
                counts=len(suite["tests"]),
                step_label=STEP_LABELS["flutter_test"])
        except SystemExit:
            raise

    # 5. admin test
    admin_test_log = os.path.join(case_dir, "admin-test.log")
    if os.path.isfile(admin_test_log):
        text = S6.read_text(admin_test_log, CASE_ID, "admin test")
        pass_match = BUN_PASS_RE.search(text)
        fail_match = BUN_FAIL_RE.search(text)
        passed = int(pass_match.group(1)) if pass_match else 0
        failed = int(fail_match.group(1)) if fail_match else -1
        if failed != 0 or passed < 1:
            gaps.append("admin_test: parsed pass=%d fail=%d from %s"
                        % (passed, failed, admin_test_log))
        else:
            add("admin_test", admin,
                "bun run test (full Admin suite)", admin_test_log,
                [S6.assertion("admin_test_summary",
                              "%d pass / 0 fail" % passed)],
                counts=passed, step_label=STEP_LABELS["admin_test"])

    # 6. admin build
    build_log = os.path.join(case_dir, "admin-build.log")
    dist_index = os.path.join(admin, "dist", "index.html")
    if os.path.isfile(build_log):
        if not os.path.isfile(dist_index):
            gaps.append("admin_build: %s missing after build" % dist_index)
        else:
            add("admin_build", admin, "bun run build (Admin)",
                build_log,
                [S6.assertion(
                    "admin_build_artifact",
                    "dist/index.html exists sha256=%s"
                    % S6.sha256_file(dist_index))],
                step_label=STEP_LABELS["admin_build"])

    # 7. admin lint
    lint_log = os.path.join(case_dir, "admin-lint.log")
    if os.path.isfile(lint_log):
        add("admin_lint", admin, "bun run lint (Admin)", lint_log,
            [S6.assertion("admin_lint_exit_zero",
                          "eslint completed with exit=0")],
            step_label=STEP_LABELS["admin_lint"])

    missing_scenarios = [scenario for scenario in expected
                         if scenario not in rows]
    for scenario in missing_scenarios:
        if not any(gap.startswith(scenario) for gap in gaps):
            gaps.append("%s: no executed step/log for this leaf" % scenario)
    if gaps:
        S6.report_gaps(CASE_ID, len(rows),
                       {CASE_ID: (len(gaps), gaps)})

    out = []
    for scenario in expected:
        repo, started, finished, counts, assertions, action, log_path = \
            rows[scenario]
        log_text = S6.read_text(log_path, CASE_ID, "command log")
        out.append(S6.C.build_row(
            expected[scenario], started, finished,
            {"exit_code": 0, "executed": counts, "passed": counts,
             "failed": 0, "skipped": 0},
            {
                "pre_db_state": ("isolated EUNIT_CONFIG scratch DB %s"
                                 % pool_db if repo == root else
                                 "n/a: client-side command in %s" % repo),
                "pre_object_hash": "n/a: this command does not touch a storage "
                                   "object",
                "action": action,
                "expected_http_semantics": "n/a: build/test command, no "
                                           "backend HTTP scenario",
                "expected_db_delta": "the executed suite's own assertions; the "
                                     "command exited 0 with no failed/skipped "
                                     "test",
                "expected_object_state": "n/a: no object mutation in this "
                                         "regression command",
                "proof_actor": "repository %s; log %s sha256=%s; %s"
                               % (repo, os.path.basename(log_path),
                                  S6.sha256_file(log_path),
                                  row_step_line(
                                      case_dir,
                                      STEP_LABELS.get(
                                          scenario,
                                          "excl-%s" % modules[0]))),
            },
            assertions, S6.repo_actor(repo), [repo],
            ["w6-03:%s" % scenario]))
    S6.C.assert_leaf_set(leaves, out)
    return case_dir, out


def row_step_line(case_dir, label):
    try:
        return S6.step_record(case_dir, label, CASE_ID)["line"]
    except SystemExit:
        return "step missing"

if __name__ == "__main__":
    S6.run_adapter(NAME, build)
