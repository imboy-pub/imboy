#!/usr/bin/env python3
"""W3-specific evidence helpers on top of the shared adapter common module.

The shared helpers (run context, leaf matrix, slot/allowlist resolution,
atomic emit) live in attachment_lifecycle_v5_adapter_w2_support.  This module
adds only what the W3 eunit-driven cases need: strict eunit line parsing,
Erlang fixture/source inspection, evidence-time measurement windows and
per-leaf gap reporting.  Every helper is fail-closed with exit 30.
"""
import datetime
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

EXIT_EVIDENCE = 30
LEAF_FIELDS = C.LEAF_FIELDS
ORACLE_FIELDS = C.ORACLE_FIELDS

ANSI = re.compile(r"\x1b\[[0-9;]*m")
TEST_BEGIN = re.compile(
    r"^\s*([a-z][a-z0-9_]*):\s*(?:(\d+):\s*)?([a-z][a-z0-9_]*)(?:\s+\([^)]*\))?\.\.\.(.*)$")
TEST_END = re.compile(r"^(?:\[([0-9.]+) s\] )?ok\s*$")
TIMING = re.compile(r"^\[([0-9.]+) s\] ok")
SUMMARY = re.compile(
    r"^\s*(?:All (\d+) tests passed\.|(\d+) tests passed\.|Test passed\.)\s*$")
DONE_IN = re.compile(r"\[done in ([0-9.]+) s\]")
MARKER_RE = re.compile(r"\bW3[0-9][0-9]_DIRECT_OK\b")
FAILURE = re.compile(r"\*failed\*|\*did not run\*|Failed:\s*\d+|Internal error:")
ENTERPRISE_SCOPE = '<<"enterprise">>'


def die(case_id, message):
    sys.stderr.write("adapter %s: %s\n" % (case_id, message))
    sys.exit(EXIT_EVIDENCE)


def context(run_dir, case_id):
    try:
        return C.load_context(run_dir)
    except C.AdapterGap as exc:
        die(case_id, str(exc))


def leaves(ctx, case_id):
    try:
        return C.case_leaves(ctx["contract"], case_id)
    except C.AdapterGap as exc:
        die(case_id, str(exc))


def _slot_from_config(path):
    if not os.path.isfile(path):
        return None
    with open(path, "r", encoding="utf-8", errors="replace") as fh:
        text = fh.read()
    for match in re.finditer(r'database\s*=>\s*"attv5_(slot[0-9]+)_[a-z0-9_]+"', text):
        return match.group(1)
    return None


def pool_resource(ctx, case_id):
    """The isolated pool database that backs this gate invocation's eunit config."""
    slot = os.environ.get("V5_SLOT", "").strip()
    if not slot:
        config = os.environ.get("EUNIT_CONFIG", "").strip()
        if config:
            slot = _slot_from_config(
                config if config.endswith(".config") else config + ".config") or ""
    try:
        if not slot:
            slot = C.resolve_slot(ctx["run_dir"])
        pool = C.db_resource(ctx["allowlist"], slot, "pool")
    except C.AdapterGap as exc:
        die(case_id, str(exc))
    containers = ctx["allowlist"].get("postgres_container") or []
    return pool, (containers[0] if containers else None)


def read_evidence(case_dir, names, case_id):
    for name in names:
        path = os.path.join(case_dir, name)
        if os.path.isfile(path) and os.path.getsize(path) > 0:
            with open(path, "rb") as fh:
                raw = fh.read()
            return {
                "name": name,
                "path": path,
                "text": ANSI.sub("", raw.decode("utf-8", "replace")),
                "sha256": hashlib.sha256(raw).hexdigest(),
                "size": len(raw),
                "mtime": os.path.getmtime(path),
            }
    die(case_id, "no case evidence log in %s (looked for %s)"
        % (case_dir, ", ".join(names)))


def parse_eunit(text, case_id):
    """Parse eunit [verbose] output into real test lines and suite summary.

    eunit_tty prints "<module>: [line:] <name> (<desc>)...[<t> s] ok"; an
    application start report can interrupt that line, leaving the timing and
    "ok" on a later line (the W3-01/W3-02 logs do exactly that).
    """
    tests = []
    failures = []
    pending = None
    for lineno, line in enumerate(text.splitlines(), 1):
        begin = TEST_BEGIN.match(line)
        if begin:
            module, _line_no, name, rest = begin.groups()
            timing = TIMING.match(rest)
            if timing:
                tests.append({"module": module, "name": name, "ok": True,
                              "duration": float(timing.group(1)),
                              "line": lineno, "raw": line.strip()})
                pending = None
            elif rest.strip() == "ok":
                tests.append({"module": module, "name": name, "ok": True,
                              "duration": None, "line": lineno, "raw": line.strip()})
                pending = None
            else:
                pending = {"module": module, "name": name,
                           "line": lineno, "begin": line.strip()}
            continue
        if pending:
            end = TEST_END.match(line.strip())
            if end:
                tests.append({
                    "module": pending["module"], "name": pending["name"], "ok": True,
                    "duration": float(end.group(1)) if end.group(1) else None,
                    "line": pending["line"], "raw": pending["begin"],
                })
                pending = None
                continue
            if "*failed*" in line or "*did not run*" in line:
                failures.append({"test": pending["name"], "line": lineno,
                                 "detail": line.strip()})
                pending = None
                continue
        if FAILURE.search(line):
            failures.append({"test": pending["name"] if pending else None,
                             "line": lineno, "detail": line.strip()})
    if pending is not None:
        failures.append({"test": pending["name"], "line": pending["line"],
                         "detail": "test line never reached an ok/failed terminator"})
    summary_count = 0
    summary_raw = ""
    for line in text.splitlines():
        match = SUMMARY.match(line)
        if match:
            count = match.group(1) or match.group(2)
            summary_count = int(count) if count else 1
            summary_raw = line.strip()
    done = DONE_IN.search(text)
    return {
        "tests": tests,
        "failures": failures,
        "summary_count": summary_count,
        "summary_raw": summary_raw,
        "done_in": float(done.group(1)) if done else None,
        "markers": sorted(set(MARKER_RE.findall(text))),
    }


def require_clean_suite(parsed, expected_marker, case_id, expected_module=None):
    problems = []
    if not parsed["tests"]:
        problems.append("no eunit test line parsed")
    elif any(not test["ok"] for test in parsed["tests"]):
        problems.append("non-ok eunit test line")
    if parsed["summary_count"] < 1:
        problems.append("no passing eunit summary line parsed")
    elif parsed["summary_count"] != len(parsed["tests"]):
        problems.append("summary count %d != parsed test lines %d"
                        % (parsed["summary_count"], len(parsed["tests"])))
    if parsed["failures"]:
        problems.append("eunit failure markers: %r" % parsed["failures"][:3])
    if expected_marker not in parsed["markers"]:
        problems.append("direct marker %s missing (parsed %r)"
                        % (expected_marker, parsed["markers"]))
    if expected_module and any(test["module"] != expected_module
                               for test in parsed["tests"]):
        problems.append("test module mismatch: expected %s, parsed %r"
                        % (expected_module,
                           sorted({test["module"] for test in parsed["tests"]})))
    if problems:
        die(case_id, "case output is not a clean suite run: " + "; ".join(problems))


def measurement_window(evidence, parsed, case_id):
    """Real UTC window: log mtime ends the measured eunit interval."""
    duration = parsed["done_in"]
    if duration is None:
        duration = sum(test["duration"] or 0.0 for test in parsed["tests"])
    if not duration or duration <= 0:
        die(case_id, "no measured eunit duration ([done in] or per-test timing)")
    finished = datetime.datetime.fromtimestamp(evidence["mtime"], datetime.timezone.utc)
    started = finished - datetime.timedelta(seconds=duration)
    return iso_z(started), iso_z(finished)


def iso_z(moment):
    return moment.astimezone(datetime.timezone.utc).strftime(
        "%Y-%m-%dT%H:%M:%S.%f")[:-3] + "Z"


def named_test(parsed, name, case_id):
    for test in parsed["tests"]:
        if test["name"] == name:
            if not test["ok"]:
                die(case_id, "eunit test %s reported failure" % name)
            return test
    die(case_id, "eunit test %s absent from parsed log" % name)


def dedicated_test(parsed, scenario):
    """Only a test named after the scenario counts as a per-leaf runtime unit."""
    names = {scenario, scenario.replace("-", "_")}
    for test in parsed["tests"]:
        if test["name"] in names:
            return test
    return None


def erl_source(root, relpath, case_id):
    path = os.path.join(root, relpath)
    if not os.path.isfile(path):
        die(case_id, "suite source missing: %s" % path)
    with open(path) as fh:
        return path, fh.read()


def erl_function_block(text, name, case_id, source_path):
    lines = text.splitlines()
    head = re.compile(r"^%s\(" % re.escape(name))
    start = None
    for idx, line in enumerate(lines):
        if head.match(line):
            start = idx
            break
    if start is None:
        die(case_id, "function %s(...) not found in %s" % (name, source_path))
    end = len(lines)
    for idx in range(start + 1, len(lines)):
        if re.match(r"^[a-z][a-z0-9_]*\(", lines[idx]):
            end = idx
            break
    return {"start": start + 1, "end": end,
            "text": "\n".join(lines[start:end])}


def erl_entrypoints(text, case_id):
    block = erl_function_block(text, "entrypoints", case_id, "entrypoints/0")
    entries = re.findall(r"\{\s*([a-z][a-z0-9_]*)\s*,", block["text"])
    if not entries:
        die(case_id, "entrypoints/0 fixture matrix is empty")
    return [entry.replace("_", "-") for entry in entries]


def erl_governance_fixtures(text, case_id):
    fixtures = re.findall(
        r"^install_governance\(Conn, AttId, ([a-z][a-z0-9_]*)\)\s*->", text, re.M)
    if not fixtures:
        die(case_id, "install_governance/3 fixture clauses not found")
    return fixtures


def probe_entries(case_dir):
    entries = []
    path = os.path.join(case_dir, "probes.jsonl")
    if not os.path.isfile(path):
        return entries
    with open(path) as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except ValueError:
                continue
            if isinstance(entry, dict):
                entries.append(entry)
    return entries


def matching_probe(entries, leaf):
    for entry in entries:
        if entry.get("pass") is not True:
            continue
        entrypoint = entry.get("entrypoint")
        scenario = entry.get("scenario") or entry.get("ordering_or_fault")
        ac_id = entry.get("ac") or entry.get("ac_id")
        if entrypoint and entrypoint != leaf.get("entrypoint"):
            continue
        if scenario and scenario != leaf.get("ordering_or_fault"):
            continue
        if ac_id and ac_id != leaf.get("ac_id"):
            continue
        if not (entrypoint or scenario or ac_id):
            continue
        return entry
    return None


def assertion(identifier, detail):
    return {"id": identifier, "passed": True, "detail": detail}


def actor(resource_id, kind="targeted-contract"):
    return {"kind": kind, "driver": "none",
            "resource_kind": "scratch_postgres", "resource_id": resource_id}


def request_id(case_id, evidence, leaf):
    return "%s:%s:%s@%s:%s" % (
        case_id, evidence["sha256"][:12], leaf["entrypoint"],
        leaf["domain"], leaf["ordering_or_fault"])


def build_observation(leaf, *, assertions, oracle, actor_provenance,
                      resource_ids, request_ids, started_at, finished_at):
    missing = [field for field in ORACLE_FIELDS if not oracle.get(field)]
    if missing:
        die(leaf.get("entrypoint"), "oracle_observations missing: %r" % missing)
    if not assertions or any(item.get("passed") is not True for item in assertions):
        die(leaf.get("entrypoint"), "assertions must be non-empty and all passed")
    if not resource_ids or not request_ids:
        die(leaf.get("entrypoint"), "resource_ids/request_ids must be non-empty")
    return {
        "leaf": leaf,
        "started_at": started_at,
        "finished_at": finished_at,
        "exit_code": 0,
        "executed": 1,
        "passed": 1,
        "failed": 0,
        "skipped": 0,
        "oracle_observations": oracle,
        "assertions": assertions,
        "actor": actor_provenance,
        "resource_ids": resource_ids,
        "request_ids": request_ids,
    }


def leaf_identity(leaf):
    return "|".join(str(leaf.get(field)) for field in LEAF_FIELDS)


def write_observations(case_dir, rows, expected_leaves, case_id):
    expected = {leaf_identity(leaf): leaf for leaf in expected_leaves}
    actual = [leaf_identity(row["leaf"]) for row in rows]
    if len(actual) != len(expected) or sorted(actual) != sorted(expected):
        die(case_id, "leaf set mismatch: expected %d, got %d"
            % (len(expected), len(actual)))
    if len(set(actual)) != len(actual):
        die(case_id, "duplicate leaves emitted")
    if any(row["leaf"] not in expected_leaves for row in rows):
        die(case_id, "leaf dict is not the frozen contract leaf")
    for row in rows:
        try:
            started = datetime.datetime.fromisoformat(
                row["started_at"].replace("Z", "+00:00"))
            finished = datetime.datetime.fromisoformat(
                row["finished_at"].replace("Z", "+00:00"))
            duration = (finished - started).total_seconds()
            deadline = row["leaf"].get("observation_deadline_seconds", 0)
        except (KeyError, TypeError, ValueError):
            die(case_id, "invalid observation timestamps")
        if not started.tzinfo or not finished.tzinfo or duration < 0 \
                or duration > deadline:
            die(case_id, "observation window %ss outside deadline %ss"
                % (duration, deadline))
        counts = (row["executed"], row["passed"], row["failed"], row["skipped"])
        if row["exit_code"] != 0 or row["failed"] or row["skipped"] \
                or counts[0] < 1 or counts[0] != counts[1] + counts[2] + counts[3]:
            die(case_id, "invalid execution counts on a sealed row")
    try:
        C.emit(case_dir, rows, case_id)
    except (OSError, C.AdapterGap) as exc:
        die(case_id, "cannot emit observations: %s" % exc)


def report_gaps(case_id, evidence, parsed, gaps, notes=None):
    lines = [
        "EVIDENCE_GAP %s: %d contract leaves, none can be emitted from this output"
        % (case_id, len(gaps)),
        "  evidence: %s sha256=%s size=%d" % (
            evidence["name"], evidence["sha256"], evidence["size"]),
        "  parsed eunit tests: %s" % json.dumps(
            [{"module": test["module"], "name": test["name"],
              "duration": test["duration"], "line": test["line"]}
             for test in parsed["tests"]], sort_keys=True),
        "  summary=%r done_in=%s" % (parsed["summary_raw"], parsed["done_in"]),
    ]
    for note in notes or []:
        lines.append("  note: %s" % note)
    for gap in gaps:
        lines.append("  leaf %s" % leaf_identity(gap["leaf"]))
        lines.append("    missing: %s" % "; ".join(gap["missing"]))
    sys.stderr.write("\n".join(lines) + "\n")
    sys.exit(EXIT_EVIDENCE)


def oracle_missing_fields(*fields):
    fields = fields or ORACLE_FIELDS
    return "%s unmeasured at leaf granularity" % ", ".join(
        "oracle_observations." + field for field in fields)


def w301_gaps(leaves_list, parsed, source_text, evidence_text, tokens, functions):
    test_names = [test["name"] for test in parsed["tests"]]
    gaps = []
    for leaf in leaves_list:
        scenario = leaf["ordering_or_fault"]
        missing = []
        if dedicated_test(parsed, scenario) is None:
            function = functions.get(scenario)
            if function and function.split("/")[0] in source_text:
                where = "asserted inside %s" % function
            else:
                where = "scenario is not implemented in suite source"
            missing.append(
                "per-leaf runtime receipt: parsed eunit tests %r; %s; no "
                "per-scenario status line" % (test_names, where))
        token = tokens.get(scenario)
        if token is None:
            missing.append("scenario fixture absent from suite source for %r"
                           % scenario)
        elif token not in source_text:
            missing.append("scenario token %r absent from suite source" % token)
        if leaf["domain"] == "enterprise" and ENTERPRISE_SCOPE not in source_text:
            missing.append(
                "domain=enterprise: suite seeds no enterprise scope (token %r absent)"
                % ENTERPRISE_SCOPE)
        if leaf["schema_phase"] == "guards-contract" and \
                "guards-contract" not in evidence_text.lower():
            missing.append(
                "schema_phase=guards-contract: no guards-contract application in "
                "case output (w301_case replays migrations only)")
        missing.append(oracle_missing_fields(
            "pre_db_state", "pre_object_hash", "action",
            "expected_db_delta", "expected_object_state"))
        gaps.append({"leaf": leaf, "missing": missing})
    return gaps
