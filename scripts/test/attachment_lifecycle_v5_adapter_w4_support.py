#!/usr/bin/env python3
"""Shared evidence helpers for the attachment-lifecycle V5 W4 adapters.

The W4 executors run Erlang eunit suites directly (erl -eval eunit:test/2) and
write one verbose log per case.  This module parses those real logs, resolves
the run-owned resources from state.json/authorization.json, and formats
fail-closed evidence gaps.  Nothing here synthesizes a measurement: every
value cited by an adapter comes from a parsed log line, a suite source line at
the candidate head, the frozen contract, or the admission allowlist.
"""
import datetime
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as base  # noqa: E402

EXIT_EVIDENCE = 30
LEAF_FIELDS = base.LEAF_FIELDS
ORACLE_FIELDS = base.ORACLE_FIELDS
ANSI_RE = base.ANSI_RE

# eunit_tty verbose shape:
#   attachment_lifecycle_v5_w403_driver_matrix_tests:
#     -w403_driver_matrix_test_/0-fun-7- (ordinary/disk/foo)...[0.1 s] ok
HEADER_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*):\s*(?:(\d+):\s*)?(.*?)\.\.\.(.*)$")
DESC_RE = re.compile(r"\((.*)\)\s*$")
OK_RE = re.compile(r"^(?:\[([0-9.]+) s\]\s*)?ok\s*$")
FAIL_RE = re.compile(r"\*(failed|timed out|skipped|did not run|not executed)\*")
SUMMARY_RE = re.compile(
    r"^\s*(?:All (\d+) tests passed\.|(\d+) tests passed\.|Test passed\.)\s*$")
DONE_RE = re.compile(r"\[done in ([0-9.]+) s\]")
MARKER_RE = re.compile(r"\bW40[1-4]_DIRECT_OK\b")
W403_EVIDENCE_RE = re.compile(
    r"\bW403_(HTTP_EVIDENCE|HTTP_OVERLAP|PROVIDER_BYPASS|GATEWAY_BUDGET|"
    r"GARAGE_EXPIRY)\s+(\{.*\})\s*$")
DRIVER_LEAF_RE = re.compile(r"^([a-z_]+)/(disk|garage)/([a-z0-9_]+)$")


def die(case_id, message):
    sys.stderr.write("adapter %s: %s\n" % (case_id, message))
    sys.exit(EXIT_EVIDENCE)


def context(run_dir, case_id):
    try:
        return base.load_context(run_dir)
    except base.AdapterGap as exc:
        die(case_id, str(exc))


def leaves(ctx, case_id):
    try:
        return base.case_leaves(ctx["contract"], case_id)
    except base.AdapterGap as exc:
        die(case_id, str(exc))


def identity(leaf):
    return "|".join(str(leaf.get(field)) for field in LEAF_FIELDS)


def source(root, relpath, case_id):
    path = os.path.join(root, relpath)
    if not os.path.isfile(path):
        die(case_id, "suite source missing: %s" % path)
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        return path, handle.read()


def line_of(text, needle):
    value = base.source_line(text, needle)
    # Keep formatting code fail-safe; 0 in a gap detail means the marker was
    # absent from the candidate source and must be reviewed.
    return value if value is not None else 0


def block(text, start_needle, case_id, what):
    """Return the source text from start_needle to the next top-level fun."""
    lines = text.splitlines()
    start = None
    for index, line in enumerate(lines):
        if line.startswith(start_needle):
            start = index
            break
    if start is None:
        die(case_id, "%s not found (%r)" % (what, start_needle))
    end = len(lines)
    for index in range(start + 1, len(lines)):
        if re.match(r"^[a-z][a-z0-9_]*\(", lines[index]):
            end = index
            break
    return {
        "start": start + 1,
        "end": end,
        "text": "\n".join(lines[start:end]),
    }


def read_log(case_dir, names, case_id):
    """Return the first non-empty case log with digest/mtime, else None."""
    for name in names:
        path = os.path.join(case_dir, name)
        if os.path.isfile(path) and os.path.getsize(path) > 0:
            with open(path, "rb") as handle:
                raw = handle.read()
            return {
                "name": name,
                "path": path,
                "text": ANSI_RE.sub("", raw.decode("utf-8", "replace")),
                "sha256": hashlib.sha256(raw).hexdigest(),
                "size": len(raw),
                "mtime": os.path.getmtime(path),
            }
    return None


def parse_verbose(text):
    """Parse eunit [verbose] output into real units and failure markers.

    eunit_tty prints \"<module>: [line:] <fun-or-name> (<desc>)...[t s] ok\".
    Application start reports or error_logger output can interrupt the line;
    the timing/terminator then arrives on a later line, which the pending
    state handles.  No unit is invented from a summary line.
    """
    tests = []
    failures = []
    pending = None
    for lineno, line in enumerate(text.splitlines(), 1):
        header = HEADER_RE.match(line)
        if header:
            if pending is not None:
                failures.append({
                    "test": pending["func"], "line": pending["line"],
                    "detail": "test line never reached an ok/failure terminator",
                })
                pending = None
            module, _line_no, head, rest = header.groups()
            desc_match = DESC_RE.search(head)
            desc = desc_match.group(1) if desc_match else ""
            func = head[:desc_match.start()].strip() if desc_match else head.strip()
            ok_match = OK_RE.match(rest.strip())
            if ok_match:
                tests.append({
                    "module": module, "func": func, "desc": desc,
                    "label": desc or func, "ok": True,
                    "duration": float(ok_match.group(1))
                    if ok_match.group(1) else None,
                    "line": lineno, "raw": line.strip(),
                })
            elif FAIL_RE.search(rest):
                failures.append({"test": func, "line": lineno,
                                 "detail": line.strip()})
            else:
                pending = {"module": module, "func": func, "desc": desc,
                           "line": lineno, "raw": line.strip()}
            continue
        if pending is not None:
            stripped = line.strip()
            ok_match = OK_RE.match(stripped)
            if ok_match:
                tests.append({
                    "module": pending["module"], "func": pending["func"],
                    "desc": pending["desc"], "label": pending["desc"]
                    or pending["func"], "ok": True,
                    "duration": float(ok_match.group(1))
                    if ok_match.group(1) else None,
                    "line": pending["line"], "raw": pending["raw"],
                })
                pending = None
                continue
            if FAIL_RE.search(line):
                failures.append({"test": pending["func"],
                                 "line": pending["line"],
                                 "detail": stripped})
                pending = None
                continue
    if pending is not None:
        failures.append({
            "test": pending["func"], "line": pending["line"],
            "detail": "test line never reached an ok/failure terminator",
        })
    summary_count = 0
    summary_raw = ""
    for line in text.splitlines():
        match = SUMMARY_RE.match(line)
        if match:
            count = match.group(1) or match.group(2)
            summary_count = int(count) if count else 1
            summary_raw = line.strip()
    done = None
    for match in DONE_RE.finditer(text):
        done = float(match.group(1))
    return {
        "tests": tests,
        "failures": failures,
        "summary_count": summary_count,
        "summary_raw": summary_raw,
        "done_in": done,
        "markers": sorted(set(MARKER_RE.findall(text))),
    }


def suite_health(evidence, parsed, marker, modules, log_names):
    """Return every reason the parsed suite run is not a clean, measured run."""
    problems = []
    if evidence is None:
        problems.append("missing CASE_DIR log (%s)" % ", ".join(log_names))
    if not parsed["tests"]:
        problems.append("no eunit test line parsed from the case log")
    if parsed["failures"]:
        problems.append("eunit failure markers: %r"
                        % parsed["failures"][:3])
    if parsed["summary_count"] < 1:
        problems.append("no passing eunit summary line")
    elif parsed["summary_count"] != len(parsed["tests"]):
        problems.append("summary count %d != parsed test lines %d"
                        % (parsed["summary_count"], len(parsed["tests"])))
    if marker not in parsed["markers"]:
        problems.append("direct marker %s missing (parsed %r)"
                        % (marker, parsed["markers"]))
    seen = {test["module"] for test in parsed["tests"]}
    for module in modules:
        if module not in seen:
            problems.append("module %s produced no parsed test line" % module)
    return problems


def find_test(parsed, module, func=None, desc=None):
    for test in parsed["tests"]:
        if test["module"] != module:
            continue
        if func is not None and test["func"] != func:
            continue
        if desc is not None and test["desc"] != desc:
            continue
        return test
    return None


def describe_test(test):
    suffix = " (%s)" % test["desc"] if test["desc"] else ""
    duration = ("%.3fs" % test["duration"]
                if test["duration"] is not None else "duration unparsed")
    return "%s:%s%s [%s] ok" % (test["module"], test["func"], suffix, duration)


def first_unit(parsed, modules):
    for test in parsed["tests"]:
        if test["module"] in modules and test["ok"]:
            return test
    return None


def parse_w403_evidence(text):
    entries = []
    for lineno, line in enumerate(text.splitlines(), 1):
        match = W403_EVIDENCE_RE.search(line)
        if not match:
            continue
        try:
            payload = json.loads(match.group(2))
        except ValueError as exc:
            entries.append({"kind": match.group(1), "line": lineno,
                            "error": "unparsable JSON: %s" % exc})
            continue
        entries.append({"kind": match.group(1), "line": lineno,
                        "payload": payload})
    return entries


def measurement_window(evidence, parsed, case_id):
    duration = parsed["done_in"]
    if duration is None:
        duration = sum(test["duration"] or 0.0 for test in parsed["tests"])
    if not duration or duration <= 0:
        die(case_id, "no measured eunit duration ([done in] or per-test timing)")
    finished = datetime.datetime.fromtimestamp(evidence["mtime"],
                                               datetime.timezone.utc)
    started = finished - datetime.timedelta(seconds=duration)
    return started, finished


def resolve_pool(ctx, case_id):
    try:
        slot = base.resolve_slot(ctx["run_dir"])
        return base.db_resource(ctx["allowlist"], slot, "pool")
    except base.AdapterGap as exc:
        die(case_id, str(exc))


def resolve_disk_root(ctx, case_id):
    allowed = ctx["allowlist"].get("disk_root")
    if not isinstance(allowed, str) or not allowed:
        die(case_id, "resource_allowlist.disk_root is missing")
    env = os.environ.get("V5_DISK_ROOT", "").strip()
    if env:
        candidate = os.path.abspath(env)
        allowed_clean = allowed.rstrip("/")
        if candidate != allowed_clean and not candidate.startswith(
                allowed_clean + "/"):
            die(case_id, "V5_DISK_ROOT=%s is outside allowlisted disk_root %s"
                % (candidate, allowed))
        if not os.path.isdir(candidate):
            die(case_id, "V5_DISK_ROOT=%s is not a directory" % candidate)
        return candidate
    if os.path.isdir(allowed):
        return os.path.abspath(allowed)
    die(case_id, "V5_DISK_ROOT unset and allowlisted disk_root %s does not "
        "exist" % allowed)


def resolve_garage(ctx, case_id, require_env=True):
    try:
        bucket = base.garage_bucket(ctx["run_dir"], ctx["allowlist"])
    except base.AdapterGap as exc:
        die(case_id, str(exc))
    env_bucket = os.environ.get("W202_GARAGE_BUCKET", "").strip()
    endpoint = os.environ.get("W202_GARAGE_ENDPOINT", "").strip()
    if env_bucket and env_bucket != bucket:
        die(case_id, "W202_GARAGE_BUCKET=%r differs from allowlisted bucket %r"
            % (env_bucket, bucket))
    if require_env:
        if not env_bucket:
            die(case_id, "W202_GARAGE_BUCKET is not exported to the adapter; "
                "the driver-matrix fixture cannot be proven to use the "
                "allowlisted garage bucket")
        if not endpoint:
            die(case_id, "W202_GARAGE_ENDPOINT is not exported to the adapter; "
                "real garage provenance cannot be proven")
    return bucket, endpoint


def db_actor(pool):
    return {"kind": "real", "driver": "none",
            "resource_kind": "scratch_postgres", "resource_id": pool}


def garage_actor(bucket):
    # state.resource_allowlist names this key "garage_bucket"; the frozen
    # producer/runner resolve actor.resource_kind by exact allowlist key, so
    # "garage" (the driver name) would be rejected before the receipt is sealed.
    return {"kind": "real", "driver": "garage",
            "resource_kind": "garage_bucket", "resource_id": bucket}


def disk_actor(root):
    return {"kind": "real", "driver": "disk",
            "resource_kind": "disk_root", "resource_id": root}


def assertion(identifier, detail):
    return {"id": identifier, "passed": True, "detail": detail}


def oracle_gap(fields=None):
    fields = fields or ORACLE_FIELDS
    return ("oracle_observations.%s unmeasured at leaf granularity"
            % "/".join(fields))


def per_leaf_gap(test, what):
    if test is not None:
        return ("per-leaf runtime receipt: only %s exists; there is no own "
                "status line for %s" % (describe_test(test), what))
    return ("per-leaf runtime receipt: no eunit unit exists for %s in the "
            "case log" % what)


def gap(leaf, missing):
    return {"leaf": leaf, "missing": [item for item in missing if item]}


def report_gaps(case_id, evidence, parsed, gaps, notes=None):
    lines = [
        "EVIDENCE_GAP %s: %d contract leaf(s); the full measured leaf set "
        "cannot be completed" % (case_id, len(gaps)),
    ]
    if evidence is not None:
        lines.append("  evidence: %s sha256=%s size=%d"
                     % (evidence["name"], evidence["sha256"],
                        evidence["size"]))
    else:
        lines.append("  evidence: none")
    lines.append("  parsed eunit units: %s" % json.dumps(
        [{"module": test["module"], "func": test["func"],
          "desc": test["desc"], "duration": test["duration"],
          "line": test["line"]} for test in parsed["tests"]],
        sort_keys=True))
    lines.append("  summary=%r done_in=%s markers=%r failures=%r"
                 % (parsed["summary_raw"], parsed["done_in"],
                    parsed["markers"], parsed["failures"][:5]))
    for note in notes or []:
        lines.append("  note: %s" % note)
    for item in gaps:
        lines.append("  leaf %s" % identity(item["leaf"]))
        for missing in item["missing"]:
            lines.append("    missing: %s" % missing)
    sys.stderr.write("\n".join(lines) + "\n")
    sys.exit(EXIT_EVIDENCE)


def model_gaps(leaves_list, model):
    gaps = []
    for leaf in leaves_list:
        missing = model(leaf) or []
        if not missing:
            missing = ["adapter model produced no measured value for this leaf"]
        gaps.append(gap(leaf, missing))
    return gaps
