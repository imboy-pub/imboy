#!/usr/bin/env python3
"""Shared measured-evidence helpers for the attachment-lifecycle V5 W5 adapters.

The W5 gate executors capture real client test runs:
  * Flutter unit suites  -> `flutter test --reporter json` JSONL
  * Admin (Bun) suites   -> `bun test --reporter=junit` JUnit XML + console log

Everything below parses those real artifacts plus the frozen contract/state.
A missing artifact, a failed or skipped test, a missing summary line, an
unresolvable worktree or an out-of-allowlist repository raises AdapterGap,
which the calling adapter turns into sys.exit(30).  No helper invents a test
result, a duration, a request id or an oracle value.
"""
import datetime
import hashlib
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

EXIT_EVIDENCE = 30
LEAF_FIELDS = C.LEAF_FIELDS
ORACLE_FIELDS = C.ORACLE_FIELDS
ANSI_RE = C.ANSI_RE

# A second of slack when reconstructing a run window from the artifact mtime
# and the reporter's own elapsed time; the reporter time is the measurement.
_MIN_WINDOW = 0.001


def die(case_id, message):
    sys.stderr.write("adapter %s: %s\n" % (case_id, message))
    sys.exit(EXIT_EVIDENCE)


def context(run_dir, case_id, repo_root=None):
    try:
        ctx = C.load_context(run_dir)
    except C.AdapterGap as exc:
        die(case_id, str(exc))
    if repo_root:
        ctx["repo_root"] = os.path.abspath(repo_root)
    return ctx


def leaves(ctx, case_id):
    try:
        return C.case_leaves(ctx["contract"], case_id)
    except C.AdapterGap as exc:
        die(case_id, str(exc))


def identity(leaf):
    return "|".join(str(leaf.get(field)) for field in LEAF_FIELDS)


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 16), b""):
            digest.update(chunk)
    return digest.hexdigest()


def worktree(ctx, name, case_id):
    """Resolve the sibling client worktree and bind it to the allowlist."""
    root = ctx.get("repo_root")
    if not root:
        die(case_id, "adapter was not given REPO_ROOT")
    path = os.path.realpath(os.path.join(os.path.dirname(root), name))
    if not os.path.isdir(path):
        die(case_id, "BLOCKED_EXTERNAL: worktree %s missing at %s"
            % (name, path))
    allowed = ctx["allowlist"].get("repository")
    entries = [os.path.realpath(item) for item in allowed] \
        if isinstance(allowed, list) else []
    if path not in entries:
        die(case_id, "worktree %s is outside resource_allowlist.repository %r"
            % (path, allowed))
    return path


def repo_actor(worktree_path):
    return {"kind": "targeted-contract", "driver": "none",
            "resource_kind": "repository", "resource_id": worktree_path}


def _read_jsonl(path, what, case_id):
    events = []
    bad = []
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for number, raw in enumerate(handle, 1):
            line = ANSI_RE.sub("", raw).strip()
            if not line:
                continue
            if not line.startswith("{"):
                continue
            try:
                events.append(json.loads(line))
            except ValueError as exc:
                bad.append("%s:%d %s" % (os.path.basename(path), number, exc))
    if bad:
        die(case_id, "%s contains unparsable JSONL lines: %s"
            % (what, "; ".join(bad[:3])))
    if not events:
        die(case_id, "%s produced no flutter JSON events: %s" % (what, path))
    return events


def flutter_suite(case_dir, filename, case_id, role):
    """Parse one `flutter test --reporter json` artifact, fail-closed."""
    path = os.path.join(case_dir, filename)
    if not os.path.isfile(path) or os.path.getsize(path) == 0:
        err = os.path.join(case_dir, filename.rsplit(".", 1)[0] + ".err")
        tail = ""
        if os.path.isfile(err):
            with open(err, "r", encoding="utf-8", errors="replace") as handle:
                tail = " stderr=" + " ".join(handle.read().splitlines()[-3:])
        die(case_id, "missing %s artifact %s%s" % (role, path, tail))
    events = _read_jsonl(path, role, case_id)
    starts = {}
    for event in events:
        if event.get("type") == "testStart":
            test = event.get("test") or {}
            starts[test.get("id")] = test
    results = {}
    for event in events:
        if event.get("type") == "testDone":
            results[event.get("testID")] = event
    done = [event for event in events if event.get("type") == "done"]
    if not done or done[-1].get("success") is not True:
        die(case_id, "%s did not finish successfully (done=%r)"
            % (role, done[-1] if done else None))
    tests = []
    for test_id, start in starts.items():
        result = results.get(test_id)
        if result is None:
            die(case_id, "%s test %r has no testDone event"
                % (role, start.get("name")))
        if result.get("hidden"):
            continue
        tests.append({
            "name": start.get("name") or "",
            "suite_id": start.get("suiteID"),
            "result": result.get("result"),
            "skipped": result.get("skipped"),
            "time_ms": result.get("time"),
        })
    if not tests:
        die(case_id, "%s produced no visible test result" % role)
    failures = [test for test in tests
                if test["result"] != "success" or test["skipped"]]
    if failures:
        die(case_id, "%s reports failing/skipped tests: %s"
            % (role, json.dumps(failures[:5], ensure_ascii=False)))
    pid = None
    for event in events:
        if event.get("type") == "start" and isinstance(event.get("pid"), int):
            pid = event["pid"]
            break
    if not pid or pid <= 0:
        die(case_id, "%s JSON start event has no positive pid" % role)
    duration_ms = done[-1].get("time")
    if not isinstance(duration_ms, (int, float)) or duration_ms <= 0:
        duration_ms = sum(test["time_ms"] or 0 for test in tests)
    if duration_ms <= 0:
        die(case_id, "%s reports no measured duration" % role)
    return {
        "path": path,
        "name": filename,
        "sha256": sha256_file(path),
        "pid": pid,
        "duration_s": float(duration_ms) / 1000.0,
        "tests": tests,
        "suite_paths": [event["suite"].get("path") for event in events
                        if event.get("type") == "suite"],
    }


def find_flutter_test(suite, prefix):
    """Match a scenario by the exact test-name prefix used by the suite."""
    hits = []
    marker = prefix.rstrip(":") + ":"
    for test in suite["tests"]:
        name = test["name"]
        if name == prefix or name.startswith(marker):
            hits.append(test)
    return hits


def junit_suite(case_dir, filename, case_id, role):
    """Parse a Bun JUnit XML report, fail-closed."""
    path = os.path.join(case_dir, filename)
    if not os.path.isfile(path) or os.path.getsize(path) == 0:
        die(case_id, "missing %s artifact %s" % (role, path))
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError as exc:
        die(case_id, "unparsable %s XML %s: %s" % (role, path, exc))
    cases = list(root.iter("testcase"))
    if not cases:
        die(case_id, "%s JUnit report has no testcase: %s" % (role, path))
    failures = []
    for case in cases:
        for child in case:
            tag = child.tag.rsplit("}", 1)[-1]
            if tag in ("failure", "error", "skipped"):
                failures.append({"name": case.get("name"), "kind": tag,
                                 "message": (child.get("message") or "")[:200]})
    if failures:
        die(case_id, "%s reports failing/skipped tests: %s"
            % (role, json.dumps(failures[:5], ensure_ascii=False)))
    total = root.get("tests")
    if total is not None and int(total) != len(cases):
        die(case_id, "%s JUnit tests attr %r != %d testcases"
            % (role, total, len(cases)))
    duration = 0.0
    if root.tag.rsplit("}", 1)[-1] == "testsuites":
        try:
            duration = float(root.get("time") or 0)
        except ValueError:
            duration = 0.0
    if duration <= 0:
        duration = sum(float(case.get("time") or 0) for case in cases)
    if duration <= 0:
        die(case_id, "%s reports no measured duration" % role)
    return {
        "path": path,
        "name": filename,
        "sha256": sha256_file(path),
        "duration_s": duration,
        "tests": [{
            "name": case.get("name") or "",
            "classname": case.get("classname") or "",
            "file": case.get("file") or "",
            "line": case.get("line"),
            "time": float(case.get("time") or 0),
        } for case in cases],
    }


def find_junit_test(suite, file_fragment, name):
    for test in suite["tests"]:
        if file_fragment not in (test["file"] or ""):
            continue
        if test["name"] == name:
            return test
    return None


def window(path, duration_s):
    """Reconstruct the observation window from artifact mtime + duration."""
    mtime = os.path.getmtime(path)
    finished = datetime.datetime.fromtimestamp(mtime, datetime.timezone.utc)
    started = finished - datetime.timedelta(
        seconds=max(float(duration_s), _MIN_WINDOW))
    return started, finished


def source_line(path, needle, case_id, what):
    if not os.path.isfile(path):
        die(case_id, "source artifact missing for %s: %s" % (what, path))
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        text = handle.read()
    line = C.source_line(text, needle)
    if line is None:
        die(case_id, "%s marker %r not found in %s" % (what, needle, path))
    return line, text


def command_exit(case_dir, marker, case_id):
    """Read the real executor exit code recorded in CASE_DIR/raw.log."""
    path = os.path.join(case_dir, "raw.log")
    if not os.path.isfile(path):
        die(case_id, "missing executor log %s" % path)
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if marker in line:
                match = re.search(r"exit=(\d+)", line)
                if match:
                    return int(match.group(1)), line.strip()
    die(case_id, "raw.log has no %r line with exit=<code>" % marker)
    return None, ""


def assertion(identifier, detail):
    return {"id": identifier, "passed": True, "detail": detail}


def emit(case_dir, rows, adapter_name):
    C.emit(case_dir, rows, adapter_name)


def run_adapter(adapter_name, builder):
    C.run_adapter(adapter_name, builder)
