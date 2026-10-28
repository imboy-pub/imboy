#!/usr/bin/env python3
"""Shared measured-evidence helpers for the attachment-lifecycle V5 W6 adapters.

W6 cases bind frozen contract leaves to artifacts produced by real executions:

  * W6-01:  w102 SQL probes (two real schema phases) and the real-PG/real-Garage
            /real-Disk W4-03 driver-matrix battery replayed by the gate.
  * W6-02:  physical-device Flutter integration runs and real-browser Playwright
            runs, plus live device/browser provenance captured by the executor.
  * W6-03:  full backend EUnit, excluded-suite reruns, Flutter analyze/test and
            Admin test/build/lint, plus the read-only ledger and runner
            self-test (mutation battery).

Every helper is fail-closed: a missing/empty/failed artifact, an unregistered
device/browser or an out-of-allowlist repository raises AdapterGap (exit 30).
No helper invents a test result, an oracle value, a request id or a PID.
"""
import datetime
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as W4  # noqa: E402

EXIT_EVIDENCE = 30
LEAF_FIELDS = C.LEAF_FIELDS
ORACLE_FIELDS = C.ORACLE_FIELDS
ANSI_RE = C.ANSI_RE
STEP_RE = re.compile(
    r"\bSTEP (\S+) exit=(\d+)(?: started=(\d+) finished=(\d+))?")
DRIVER_LEAF_RE = W4.DRIVER_LEAF_RE
PHASE_MAP = {"expand-queue": "expand", "guards-contract": "guards",
             "integrated": "integrated"}


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


def read_json(path, case_id, what):
    if not os.path.isfile(path) or os.path.getsize(path) == 0:
        die(case_id, "missing %s artifact %s" % (what, path))
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return json.load(handle)
    except ValueError as exc:
        die(case_id, "unparsable %s artifact %s: %s" % (what, path, exc))


def read_text(path, case_id, what):
    if not os.path.isfile(path):
        die(case_id, "missing %s artifact %s" % (what, path))
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        return handle.read()


def source_line(path, needle, case_id, what):
    text = read_text(path, case_id, what)
    line = C.source_line(text, needle)
    if line is None:
        die(case_id, "%s marker %r missing from %s" % (what, needle, path))
    return line, text


def worktree(ctx, name, case_id):
    root = ctx.get("repo_root")
    if not root:
        die(case_id, "adapter was not given REPO_ROOT")
    path = os.path.realpath(os.path.join(os.path.dirname(root), name))
    if not os.path.isdir(path):
        die(case_id, "worktree %s missing at %s" % (name, path))
    allowed = ctx["allowlist"].get("repository")
    entries = [os.path.realpath(item) for item in allowed] \
        if isinstance(allowed, list) else []
    if path not in entries:
        die(case_id, "worktree %s is outside resource_allowlist.repository"
            % path)
    return path


def repo_actor(path):
    return {"kind": "real", "driver": "none",
            "resource_kind": "repository", "resource_id": path}


def db_actor(resource_id):
    return {"kind": "real", "driver": "none",
            "resource_kind": "scratch_postgres", "resource_id": resource_id}


def garage_actor(resource_id):
    return {"kind": "real", "driver": "garage",
            "resource_kind": "garage_bucket", "resource_id": resource_id}


def disk_actor(resource_id):
    return {"kind": "real", "driver": "disk",
            "resource_kind": "disk_root", "resource_id": resource_id}


def assertion(identifier, detail):
    return {"id": identifier, "passed": True, "detail": detail}


def step_record(case_dir, label, case_id):
    """Read one real battery exit code/timestamps recorded by the executor."""
    path = os.path.join(case_dir, "raw.log")
    if not os.path.isfile(path):
        die(case_id, "missing executor log %s" % path)
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            match = STEP_RE.search(line)
            if match and match.group(1) == label:
                start = int(match.group(3)) if match.group(3) else None
                finish = int(match.group(4)) if match.group(4) else None
                return {"exit": int(match.group(2)), "line": line.strip(),
                        "started": start, "finished": finish}
    die(case_id, "raw.log has no W6-01 STEP %s exit=<code> line" % label)
    return None


def step_window(record, case_id, deadline=1800):
    start, finish = record.get("started"), record.get("finished")
    if (type(start) is not int or type(finish) is not int or finish < start
            or finish - start > deadline):
        die(case_id, "W6-01 STEP has no usable measured window "
            "(started=%r finished=%r deadline=%r)"
            % (start, finish, deadline))
    return (datetime.datetime.fromtimestamp(start, datetime.timezone.utc),
            datetime.datetime.fromtimestamp(finish, datetime.timezone.utc))


def eunit_log(path, case_id, role):
    text = read_text(path, case_id, role)
    parsed = W4.parse_verbose(ANSI_RE.sub("", text))
    if not parsed["tests"]:
        die(case_id, "%s parsed no eunit test line: %s" % (role, path))
    if parsed["failures"]:
        die(case_id, "%s reports failures: %r"
            % (role, parsed["failures"][:3]))
    if parsed["summary_count"] < 1:
        die(case_id, "%s has no passing summary line: %s" % (role, path))
    if parsed["summary_count"] != len(parsed["tests"]):
        die(case_id, "%s summary count %d != parsed tests %d"
            % (role, parsed["summary_count"], len(parsed["tests"])))
    return {
        "path": path,
        "sha256": sha256_file(path),
        "text": text,
        "parsed": parsed,
        "evidence": W4.parse_w403_evidence(ANSI_RE.sub("", text)),
    }


def measurement_window(log, case_id):
    parsed = log["parsed"]
    duration = parsed["done_in"]
    if duration is None:
        duration = sum(test["duration"] or 0.0 for test in parsed["tests"])
    if not duration or duration <= 0:
        die(case_id, "%s reports no measured duration" % log["path"])
    finished = datetime.datetime.fromtimestamp(
        os.path.getmtime(log["path"]), datetime.timezone.utc)
    started = finished - datetime.timedelta(seconds=duration)
    return started, finished, duration


def find_eunit_unit(log, module, desc):
    for test in log["parsed"]["tests"]:
        if test["module"] == module and test["desc"] == desc:
            return test
    return None


def http_evidence(log, leaf_desc):
    for entry in log["evidence"]:
        if entry.get("kind") != "HTTP_EVIDENCE":
            continue
        payload = entry.get("payload")
        if isinstance(payload, dict) and payload.get("leaf") == leaf_desc:
            return entry
    return None


def validate_requests(entry, case_id, desc):
    if entry is None:
        die(case_id, "no W403_HTTP_EVIDENCE record for leaf %r" % desc)
    payload = entry.get("payload")
    requests = payload.get("requests") if isinstance(payload, dict) else None
    if not isinstance(requests, list) or not requests:
        die(case_id, "W403_HTTP_EVIDENCE for %r has no requests array" % desc)
    ids = []
    statuses = []
    for index, request in enumerate(requests, 1):
        if not isinstance(request, dict):
            die(case_id, "%r request %d is not an object" % (desc, index))
        request_id = request.get("request_id")
        if not (isinstance(request_id, str) and request_id.startswith("w403-")):
            die(case_id, "%r request %d has no w403- request_id (%r)"
                % (desc, index, request_id))
        status = request.get("status")
        if type(status) is not int:
            die(case_id, "%r request %d has no integer status (%r)"
                % (desc, index, status))
        for field in ("route", "method", "handler"):
            if not isinstance(request.get(field), str) or not request[field]:
                die(case_id, "%r request %d has no %s"
                    % (desc, index, field))
        ids.append(request_id)
        statuses.append(status)
    if not any(200 <= status < 300 for status in statuses):
        die(case_id, "%r has no 2xx request in its evidence" % desc)
    return ids, statuses, requests


def probe_file(path, phase, case_id):
    """Parse one w102 probe jsonl (per-name pass flags + phase summary)."""
    text = read_text(path, case_id, "w102 %s probe" % phase)
    by_name = {}
    summary = None
    for number, raw in enumerate(text.splitlines(), 1):
        if not raw.strip():
            continue
        try:
            obj = json.loads(raw)
        except ValueError as exc:
            die(case_id, "unparsable probe line %s:%d (%s)"
                % (path, number, exc))
        if obj.get("phase") == phase:
            summary = obj
            continue
        by_name.setdefault(obj.get("name"), []).append((number, obj))
    if summary is None:
        die(case_id, "missing %s probe summary line in %s" % (phase, path))
    if summary.get("bad") != 0:
        die(case_id, "%s probe summary reports bad=%r"
            % (phase, summary.get("bad")))
    if type(summary.get("ok")) is not int or summary["ok"] < 1:
        die(case_id, "%s probe summary reports ok=%r"
            % (phase, summary.get("ok")))
    return {"path": path, "sha256": sha256_file(path), "summary": summary,
            "by_name": by_name}


def require_probe(probes, name, case_id):
    hits = probes["by_name"].get(name)
    if not hits:
        die(case_id, "probe %r missing from %s" % (name, probes["path"]))
    for number, obj in hits:
        if obj.get("pass") is not True:
            die(case_id, "probe %r line %d reports pass=%r detail=%r"
                % (name, number, obj.get("pass"), obj.get("detail", "")))
    return hits[0][0]


def device_provenance(case_dir, allowlist, case_id):
    path = os.path.join(case_dir, "device-provenance.json")
    doc = read_json(path, case_id, "physical device provenance")
    allowed = allowlist.get("device_provenance")
    if (not isinstance(doc, dict) or doc.get("kind") != "physical"
            or not isinstance(allowed, list)
            or doc.get("resource_id") not in allowed
            or type(doc.get("process_id")) is not int
            or doc["process_id"] <= 0):
        die(case_id, "device-provenance.json is not a registered live physical "
            "device (kind/resource_id/process_id): %r" % (doc,))
    return {"kind": "physical", "resource_id": doc["resource_id"],
            "process_id": doc["process_id"]}


def browser_provenance(case_dir, allowlist, case_id):
    path = os.path.join(case_dir, "browser-provenance.json")
    doc = read_json(path, case_id, "real browser provenance")
    allowed = allowlist.get("browser_provenance")
    if (not isinstance(doc, dict) or doc.get("kind") != "real-browser"
            or not isinstance(allowed, list)
            or doc.get("resource_id") not in allowed
            or type(doc.get("process_id")) is not int
            or doc["process_id"] <= 0):
        die(case_id, "browser-provenance.json is not a registered live real "
            "browser (kind/resource_id/process_id): %r" % (doc,))
    return {"kind": "real-browser", "resource_id": doc["resource_id"],
            "process_id": doc["process_id"]}


def preflight(case_dir, case_id):
    return read_json(os.path.join(case_dir, "w602-preflight.json"), case_id,
                     "W6-02 preflight")


def write_summary(path, payload, case_id):
    """Write a task-output summary only when every guard already passed."""
    temporary = path + ".tmp"
    with open(temporary, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, ensure_ascii=False, indent=1)
        handle.write("\n")
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, path)


def report_gaps(case_id, measured, gap_groups):
    """Print bounded, grouped gaps and exit 30 (no observations emitted)."""
    total = sum(count for count, _reason in gap_groups.values())
    lines = [
        "EVIDENCE_GAP %s: %d contract leaves; %d measured, %d unmeasured"
        % (case_id, measured + total, measured, total),
    ]
    for source in sorted(gap_groups):
        count, reasons = gap_groups[source]
        lines.append("  %s: %d leaf(s)" % (source, count))
        for reason in reasons[:4]:
            lines.append("    missing: %s" % reason)
    sys.stderr.write("\n".join(lines) + "\n")
    sys.exit(EXIT_EVIDENCE)


def group_gap(gap_groups, source, count, reason):
    if source in gap_groups:
        existing = gap_groups[source]
        gap_groups[source] = (existing[0] + count,
                              existing[1] if reason in existing[1]
                              else existing[1] + [reason])
    else:
        gap_groups[source] = (count, [reason])


def emit(case_dir, rows, adapter_name):
    C.emit(case_dir, rows, adapter_name)


def run_adapter(adapter_name, builder):
    C.run_adapter(adapter_name, builder)
