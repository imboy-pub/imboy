#!/usr/bin/env python3
"""Shared measured-evidence helpers for the W6-02 device/browser adapters.

The W6-02 gate executor writes, per case:
  w602-preflight.json         real device/browser discovery + spec presence
  device-provenance.json      live physical-device PID captured during the run
  browser-provenance.json     live Chromium PID captured during the run
  flutter-integration-<driver>.jsonl
  admin-playwright-<driver>.json

Nothing below invents a result: every row binds a contract scenario to a parsed
test with status success and to a live provenance record whose PID is an
integer > 0 and whose resource id is in the admission allowlist.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w5_support as S5  # noqa: E402
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402

C = S6.C


def require_preflight(case_dir, case_id, need_device, need_browser):
    doc = S6.preflight(case_dir, case_id)
    problems = []
    device = doc.get("device") or {}
    browser = doc.get("browser") or {}
    if need_device and device.get("available") is not True:
        problems.append("physical device not available: %r"
                        % device.get("detail"))
    if need_browser and browser.get("available") is not True:
        problems.append("real browser not available: %r"
                        % browser.get("detail"))
    if problems:
        S6.report_gaps(case_id, 0, {
            case_id: (1, problems + [
                "device/browser provisioning must come from the W0-verified "
                "allowlist (XWE6R19916004085, 00008140-000E30561E32801C, "
                "playwright-chromium)"]),
        })
    return doc


def require_spec(case_dir, case_id, preflight, key, label):
    spec = (preflight.get("specs") or {}).get(key) or {}
    if spec.get("exists") is not True:
        S6.report_gaps(case_id, 0, {
            case_id: (1, [
                "required %s is missing: %s (W6-02 task deliverable; the "
                "gate cannot synthesize this suite)" % (label, spec.get("path")),
            ]),
        })
    return spec["path"]


def flutter_suite(case_dir, filename, case_id, role):
    return S5.flutter_suite(case_dir, filename, case_id, role)


def find_flutter_test(suite, scenario):
    hits = [test for test in suite["tests"]
            if test["name"] == scenario
            or test["name"].startswith(scenario + ":")
            or scenario in test["name"]]
    return hits


def playwright_report(case_dir, filename, case_id, role):
    path = os.path.join(case_dir, filename)
    if not os.path.isfile(path) or os.path.getsize(path) == 0:
        err = os.path.join(case_dir,
                           filename.rsplit(".", 1)[0] + ".err")
        tail = ""
        if os.path.isfile(err):
            with open(err, "r", encoding="utf-8", errors="replace") as handle:
                tail = " stderr=" + " ".join(handle.read().splitlines()[-3:])
        S6.die(case_id, "missing %s artifact %s%s" % (role, path, tail))
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            doc = json.load(handle)
    except ValueError as exc:
        S6.die(case_id, "unparsable %s JSON %s: %s" % (role, path, exc))
    tests = []

    def walk(suite, prefix):
        title = (suite.get("title") or "").strip()
        chain = (prefix + " > " + title).strip(" >") if title else prefix
        for spec in suite.get("specs") or []:
            names = [spec.get("title") or ""]
            for test in spec.get("tests") or []:
                results = test.get("results") or []
                tests.append({
                    "name": " > ".join([item for item in (chain, names[0])
                                        if item]),
                    "status": test.get("status"),
                    "results": [result.get("status") for result in results],
                    "duration_ms": sum(result.get("duration") or 0
                                       for result in results),
                })
        for child in suite.get("suites") or []:
            walk(child, chain)

    for suite in doc.get("suites") or []:
        walk(suite, "")
    if not tests:
        S6.die(case_id, "%s produced no Playwright testcase: %s"
               % (role, path))
    bad = [test for test in tests
           if test["status"] != "expected"
           or any(status != "passed" for status in test["results"])]
    if bad:
        S6.die(case_id, "%s reports non-passing tests: %r"
               % (role, bad[:5]))
    stats = doc.get("stats") or {}
    if stats.get("unexpected") or stats.get("skipped") or stats.get("flaky"):
        S6.die(case_id, "%s stats report unexpected/skipped/flaky: %r"
               % (role, stats))
    duration = sum(test["duration_ms"] for test in tests) / 1000.0
    if duration <= 0:
        duration = float(stats.get("duration") or 0) / 1000.0
    if duration <= 0:
        S6.die(case_id, "%s reports no measured duration" % role)
    return {"path": path, "tests": tests, "duration_s": duration,
            "sha256": S6.sha256_file(path)}


def find_playwright_test(suite, scenario):
    return [test for test in suite["tests"] if scenario in test["name"]]


def actor_for(driver, bucket, disk_root, case_id):
    if driver == "garage":
        return S6.garage_actor(bucket), bucket
    if driver == "disk":
        return S6.disk_actor(disk_root), disk_root
    S6.die(case_id, "W6-02 leaf has unsupported driver %r" % driver)
    return None, None


def spec_gaps(preflight, specs_needed):
    gaps = []
    for key, label in specs_needed:
        spec = (preflight.get("specs") or {}).get(key) or {}
        if spec.get("exists") is not True:
            gaps.append("required %s is missing: %s (W6-02 task deliverable; "
                        "the gate cannot synthesize this suite)"
                        % (label, spec.get("path")))
    return gaps


def provenance_pipeline_gaps(root, case_id, need_device, need_browser):
    """Name fields the frozen producer drops before the runner can verify them."""
    fields = []
    if need_device:
        fields.append("device_provenance")
    if need_browser:
        fields.append("browser_provenance")
    path = os.path.join(root, "scripts", PRODUCER)
    dropped = producer_drops(path, fields, case_id)
    if not dropped:
        return []
    return ["frozen producer never copies %s from a row into the receipt "
            "(producer.py receipt assembly); the runner's receipt_errors then "
            "rejects every W6-02 receipt with 'missing registered live %s' even "
            "when the adapter measured a live provenance record"
            % (", ".join(dropped), "/".join(dropped))]


def require_all(case_id, gaps, total_leaves):
    if gaps:
        S6.report_gaps(case_id, 0, {case_id: (total_leaves, gaps)})


def read_provenance(case_dir, ctx, case_id, need_device, need_browser):
    device = S6.device_provenance(case_dir, ctx["allowlist"], case_id) \
        if need_device else None
    browser = S6.browser_provenance(case_dir, ctx["allowlist"], case_id) \
        if need_browser else None
    return device, browser


PRODUCER = "attachment_lifecycle_v5_producer.py"


def producer_drops(producer_path, fields, case_id):
    """Return the fields that the frozen producer never copies to a receipt."""
    text = S6.read_text(producer_path, case_id, "frozen producer source")
    dropped = []
    for field in fields:
        if ('receipt["%s"]' % field) not in text \
                and ("receipt['%s']" % field) not in text:
            dropped.append(field)
    return dropped


def rows_for(ctx, case_id, leaves, case_dir, suites, mapping, device, browser,
             actor_lookup, oracle_for, request_kind):
    """Assemble W6-02 rows with device/browser provenance and fail-closed gaps.

    `mapping` maps scenario -> ("flutter"|"playwright"|"any", gap_reason|None).
    `suites` maps ("flutter"|"playwright", driver) -> parsed suite.
    """
    gaps = []
    rows = []
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"].split("/")[-1]
        kind, reason = mapping.get(scenario, ("any", None))
        if reason:
            gaps.append("%s: %s" % (scenario, reason))
            continue
        candidates = []
        if kind in ("flutter", "any"):
            suite = suites.get(("flutter", leaf["driver"]))
            if suite is not None:
                candidates.extend((("flutter", suite, test)
                                   for test in find_flutter_test(suite, scenario)))
        if kind in ("playwright", "any"):
            suite = suites.get(("playwright", leaf["driver"]))
            if suite is not None:
                candidates.extend((("playwright", suite, test)
                                   for test in find_playwright_test(suite,
                                                                    scenario)))
        if not candidates:
            gaps.append("%s (driver=%s): no executed %s test carries this "
                        "scenario token" % (scenario, leaf["driver"], kind))
            continue
        suite_kind, suite, test = candidates[0]
        started, finished = S5.window(suite["path"], suite["duration_s"])
        actor, resource = actor_lookup(leaf)
        assertions = [
            S6.assertion(
                "%s_test_passed" % suite_kind,
                "%s: %r status=%r duration=%ss"
                % (os.path.basename(suite["path"]), test["name"],
                   test.get("status", test.get("result")),
                   test.get("duration_ms", test.get("time_ms")))),
            S6.assertion(
                "suite_green",
                "%s parsed %d tests, all passing, duration=%.3fs"
                % (os.path.basename(suite["path"]), len(suite["tests"]),
                   suite["duration_s"])),
        ]
        if device:
            assertions.append(S6.assertion(
                "live_device_provenance",
                "physical device %r pid=%r captured by the executor"
                % (device.get("resource_id"), device.get("process_id"))))
        if browser:
            assertions.append(S6.assertion(
                "live_browser_provenance",
                "real browser %r pid=%r captured by the executor"
                % (browser.get("resource_id"), browser.get("process_id"))))
        oracle = oracle_for(leaf, test, suite, device, browser, resource)
        row = S6.C.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [resource],
            ["%s:%s#%s" % (request_kind, os.path.basename(suite["path"]),
                           test["name"])])
        if device:
            row["device_provenance"] = device
        if browser:
            row["browser_provenance"] = browser
        rows.append(row)
    if gaps:
        S6.report_gaps(case_id, len(rows), {case_id: (len(gaps), gaps)})
    C.assert_leaf_set(leaves, rows)
    return rows
