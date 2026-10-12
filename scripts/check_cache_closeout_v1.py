#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""C14 independent mechanical re-verifier for CACHE_V221_BACKEND_CLOSEOUT_V1.

Re-computes closeout evidence from real files on disk. Anti-fake-PASS doctrine:

  * Verdicts are derived ONLY from bytes of the files given via --required-ids /
    --ledger / --closeout-plan / --original-plan / --baseline / --evidence(-dir).
  * The two frozen plan SHA-256 references are compiled into this script and are
    never accepted from CLI arguments or environment variables.
  * No environment variable can flip any verdict; there is no --assume-pass.
  * Evidence JSON must carry a non-empty scenarios[] where every scenario has
    name/status/detail and status == "pass"; empty/hand-drawn "pass" objects
    are rejected.

Checks (each prints one [PASS]/[FAIL] line; exit code 0 iff all pass):
  --ids        every required-ids.json id/alias appears in ledger exactly once
  --sha256     both plan files hash to the frozen references (baseline cross-check)
  --candidate  no candidate mixing inside one evidence run series; every
               evidence file and the ledger must carry a candidate binding
  --exits      every evidence scenario status == "pass" with no missing fields
  --utc        all evidence/ledger timestamps are tz-aware ISO8601, sane range
               (post-1970, not far future)

--self-test builds fixtures under /tmp/check_cache_closeout_fixtures/ (never in
the repo) and asserts 6 negative fixtures fail + 1 positive fixture passes.
Writes its log to /tmp/check_cache_closeout_fixtures/selftest.log.

stdlib only; Python 3.9 compatible.
"""

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from datetime import datetime, timedelta, timezone

TASK_ID = "CACHE_V221_BACKEND_CLOSEOUT_V1"

# Frozen references (given by task owner; never overridable via CLI/env).
EXPECTED_CLOSEOUT_PLAN_SHA256 = (
    "806114da93a904108dfa9a2747d1ee59d4efcb1a2562a92ae7ea6ae905882f0f")
EXPECTED_ORIGINAL_PLAN_SHA256 = (
    "a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713")

SCRIPT_PATH = os.path.abspath(__file__)
REPO_ROOT = os.path.dirname(os.path.dirname(SCRIPT_PATH))
_CO = "docs/architecture/cache-v2.2.1"
DEFAULT_REQUIRED_IDS = os.path.join(REPO_ROOT, _CO, "closeout-v1/required-ids.json")
DEFAULT_LEDGER = os.path.join(REPO_ROOT, _CO, "closeout-v1/ledger-v1.json")
DEFAULT_CLOSEOUT_PLAN = os.path.join(REPO_ROOT, _CO, "backend-closeout-plan-v1.md")
DEFAULT_ORIGINAL_PLAN = os.path.join(REPO_ROOT, _CO, "w0/plan-v2.2.1.md")
DEFAULT_BASELINE = os.path.join(REPO_ROOT, _CO, "closeout-v1/baseline.json")
DEFAULT_EVIDENCE_DIR = os.path.expanduser(
    "~/.codex/cache-closeout-runs/zcode-20261011T130725Z/raw")

CANDIDATE_KEYS = ("candidate_sha256", "candidate", "candidate_digest",
                  "working_tree_digest")
CHECK_ORDER = ("ids", "sha256", "candidate", "exits", "utc")

HEX64_RE = re.compile(r"^[0-9a-f]{64}$")
_TS_RE = re.compile(
    r"^(\d{4})-(\d{2})-(\d{2})[Tt ](\d{2}):(\d{2}):(\d{2})"
    r"(?:\.(\d{1,9}))?(Z|z|[+-]\d{2}:?\d{2})$")
MIN_UTC = datetime(1971, 1, 1, tzinfo=timezone.utc)


class CheckResult(object):
    def __init__(self, name):
        self.name = name
        self.ok = False
        self.summary = ""
        self.details = []

    def fail(self, summary, details):
        self.ok, self.summary, self.details = False, summary, list(details)
        return self

    def passed(self, summary):
        self.ok, self.summary, self.details = True, summary, []
        return self


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(131072), b""):
            h.update(chunk)
    return h.hexdigest()


def load_json(path):
    """Return (obj, None) or (None, error-message)."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f), None
    except OSError as exc:
        return None, "%s: cannot read (%s)" % (path, exc)
    except ValueError as exc:
        return None, "%s: invalid JSON (%s)" % (path, exc)


def parse_utc(text):
    """Parse ISO8601 with explicit offset; return (aware-datetime-UTC, None)
    or (None, reason). Naive timestamps (no offset) are rejected."""
    if not isinstance(text, str):
        return None, "timestamp is not a string: %r" % (text,)
    m = _TS_RE.match(text.strip())
    if not m:
        return None, "not tz-aware ISO8601 (need Z or +hh:mm offset): %r" % text
    y, mo, d, h, mi, s = (int(m.group(i)) for i in range(1, 7))
    frac = m.group(7) or ""
    micro = int((frac + "000000")[:6]) if frac else 0
    tz = m.group(8)
    if tz in ("Z", "z"):
        tzi = timezone.utc
    else:
        compact = tz.replace(":", "")
        sign = 1 if tz[0] == "+" else -1
        tzi = timezone(sign * timedelta(hours=int(compact[1:3]),
                                        minutes=int(compact[3:5])))
    try:
        dt = datetime(y, mo, d, h, mi, s, micro, tzinfo=tzi)
    except ValueError as exc:
        return None, "invalid date/time value (%s): %r" % (exc, text)
    return dt.astimezone(timezone.utc), None


def utc_in_window(dt):
    """Return None if dt is in the sane window, else a reason string."""
    if dt < MIN_UTC:
        return "timestamp at/before 1970 epoch sentinel: %s" % dt.isoformat()
    if dt > datetime.now(timezone.utc) + timedelta(hours=24):
        return "timestamp more than 24h in the future: %s" % dt.isoformat()
    return None


def iter_utc_fields(obj, path=""):
    """Yield (path, value) for every string field named utc/*_utc/*_at_utc."""
    if isinstance(obj, dict):
        for k, v in obj.items():
            p = "%s.%s" % (path, k) if path else k
            if isinstance(v, str) and (k == "utc" or k.endswith("_utc")):
                yield p, v
            else:
                for item in iter_utc_fields(v, p):
                    yield item
    elif isinstance(obj, list):
        for i, v in enumerate(obj):
            for item in iter_utc_fields(v, "%s[%d]" % (path, i)):
                yield item


def series_of(path):
    """three_node_c09_r1.json -> three_node_c09 (evidence run series)."""
    stem = os.path.splitext(os.path.basename(path))[0]
    return re.sub(r"_r\d+$", "", stem) or stem


def gather_evidence(args):
    """Return sorted list of existing evidence files (dir glob + explicit)."""
    files = set()
    if args.evidence_dir and os.path.isdir(args.evidence_dir):
        for name in sorted(os.listdir(args.evidence_dir)):
            if name.endswith(".json"):
                files.add(os.path.join(args.evidence_dir, name))
    for extra in args.evidence or []:
        files.add(os.path.abspath(extra))
    return sorted(files)


# ---------------------------------------------------------------------------
# check: --ids
# ---------------------------------------------------------------------------

def _parse_required_ids(doc):
    """Return (specs, errors): specs = [(id, alias_or_None)]."""
    errors = []
    if not isinstance(doc, dict):
        return [], ["required-ids.json: top level is not an object"]
    if doc.get("task_id") != TASK_ID:
        errors.append("required-ids.json: task_id %r != %r"
                      % (doc.get("task_id"), TASK_ID))
    raw = doc.get("ids")
    if not isinstance(raw, list) or not raw:
        return [], errors + ["required-ids.json: 'ids' missing or empty"]
    total = doc.get("total")
    if total is not None and total != len(raw):
        errors.append("required-ids.json: total=%r but len(ids)=%d"
                      % (total, len(raw)))
    specs, seen = [], {}
    for i, e in enumerate(raw):
        if not isinstance(e, dict) or not isinstance(e.get("id"), str) \
                or not e["id"].strip():
            errors.append("required-ids.json: ids[%d] lacks a valid 'id'" % i)
            continue
        rid = e["id"]
        alias = e.get("alias_hist_ledger")
        if alias is not None and (not isinstance(alias, str) or not alias.strip()):
            errors.append("required-ids.json: ids[%d] %s has invalid alias %r"
                          % (i, rid, alias))
            alias = None
        if rid in seen:
            errors.append("required-ids.json: duplicate id %s" % rid)
        seen[rid] = True
        specs.append((rid, alias))
    return specs, errors


def _parse_ledger_ids(doc):
    """Return (ids, errors)."""
    errors = []
    if not isinstance(doc, dict):
        return [], ["ledger: top level is not an object"]
    if doc.get("task_id") != TASK_ID:
        errors.append("ledger: task_id %r != %r" % (doc.get("task_id"), TASK_ID))
    entries = doc.get("entries")
    if not isinstance(entries, list) or not entries:
        return [], errors + ["ledger: 'entries' missing or empty"]
    ids = []
    for i, e in enumerate(entries):
        if not isinstance(e, dict) or not isinstance(e.get("id"), str):
            errors.append("ledger: entries[%d] lacks string 'id'" % i)
            continue
        ids.append(e["id"])
    return ids, errors


def check_ids(args):
    res = CheckResult("ids")
    req_doc, err = load_json(args.required_ids)
    if err:
        return res.fail("cannot load required-ids.json", [err])
    led_doc, err = load_json(args.ledger)
    if err:
        return res.fail("cannot load ledger", [err])

    specs, errors = _parse_required_ids(req_doc)
    ledger_ids, led_errors = _parse_ledger_ids(led_doc)
    errors.extend(led_errors)
    if errors:
        return res.fail("input structure invalid", errors)

    ledger_counts = {}
    for lid in ledger_ids:
        ledger_counts[lid] = ledger_counts.get(lid, 0) + 1
    duplicates = sorted(k for k, v in ledger_counts.items() if v > 1)
    direct = set(ledger_counts)

    missing, both_forms = [], []
    for rid, alias in specs:
        hit_direct, hit_alias = rid in direct, bool(alias) and alias in direct
        if hit_direct and hit_alias:
            both_forms.append("%s (+alias %s)" % (rid, alias))
        elif not (hit_direct or hit_alias):
            missing.append(rid)
    known = {rid for rid, _ in specs} | {a for _, a in specs if a}
    extras = sorted(set(ledger_ids) - known)

    if duplicates or missing or extras or both_forms:
        details = []
        for rid in missing:
            details.append("required id %s not found in ledger (id or alias)" % rid)
        for rid in duplicates:
            details.append("ledger has duplicate entries for %s" % rid)
        for rid in both_forms:
            details.append("ledger contains both forms of %s" % rid)
        for lid in extras:
            details.append("ledger id %s is not in required-ids.json" % lid)
        return res.fail("%d missing / %d duplicate / %d extra / %d both-forms"
                        % (len(missing), len(duplicates), len(extras),
                           len(both_forms)), details)
    return res.passed("%d/%d required ids found in ledger; duplicates=0 "
                      "extras=0 both-forms=0" % (len(specs), len(specs)))


# ---------------------------------------------------------------------------
# check: --sha256
# ---------------------------------------------------------------------------

def check_sha256(args):
    res = CheckResult("sha256")
    errors = []
    digests = {}
    for label, path, expected in (
            ("closeout-plan", args.closeout_plan, EXPECTED_CLOSEOUT_PLAN_SHA256),
            ("original-plan", args.original_plan, EXPECTED_ORIGINAL_PLAN_SHA256)):
        if not os.path.isfile(path):
            errors.append("%s: file not found (%s)" % (label, path))
            continue
        actual = sha256_file(path)
        digests[label] = actual
        if actual != expected:
            errors.append("%s: sha256 %s != frozen reference %s"
                          % (label, actual, expected))

    base_doc, err = load_json(args.baseline)
    if err:
        errors.append(err)
    else:
        pb = base_doc.get("plan_binding") if isinstance(base_doc, dict) else None
        if not isinstance(pb, dict):
            errors.append("baseline.json: missing plan_binding object")
        else:
            if base_doc.get("task_id") != TASK_ID:
                errors.append("baseline.json: task_id %r != %r"
                              % (base_doc.get("task_id"), TASK_ID))
            for key, expected in (
                    ("closeout_plan_sha256", EXPECTED_CLOSEOUT_PLAN_SHA256),
                    ("original_plan_sha256", EXPECTED_ORIGINAL_PLAN_SHA256)):
                if pb.get(key) != expected:
                    errors.append("baseline.json plan_binding.%s %r != reference"
                                  % (key, pb.get(key)))
            if pb.get("hash_match") is not True:
                errors.append("baseline.json plan_binding.hash_match is not true")
            for key in ("expected_closeout_sha256", "expected_original_sha256"):
                if key in pb and pb[key] not in (EXPECTED_CLOSEOUT_PLAN_SHA256,
                                                 EXPECTED_ORIGINAL_PLAN_SHA256):
                    errors.append("baseline.json plan_binding.%s %r does not "
                                  "match any frozen reference" % (key, pb[key]))
    if errors:
        return res.fail("plan digest / baseline cross-check failed", errors)
    short = {k: v[:12] + ".." + v[-6:] for k, v in digests.items()}
    return res.passed("both plans match frozen references "
                      "(closeout=%s original=%s, baseline cross-check ok)"
                      % (short.get("closeout-plan", "?"),
                         short.get("original-plan", "?")))


# ---------------------------------------------------------------------------
# check: --candidate
# ---------------------------------------------------------------------------

def _collect_candidate_values(obj, where, out, errors):
    """Pull top-level candidate fields from an evidence/ledger object."""
    if not isinstance(obj, dict):
        errors.append("%s: top level is not an object" % where)
        return
    for key in CANDIDATE_KEYS:
        if key in obj and obj[key] is not None:
            val = obj[key]
            if not isinstance(val, str) or \
                    not HEX64_RE.match(val.strip().lower()):
                errors.append("%s: %s=%r is not a 64-hex sha256"
                              % (where, key, val))
            else:
                out.append((where, val.strip().lower()))


def check_candidate(args):
    res = CheckResult("candidate")
    files = gather_evidence(args)
    if not files:
        return res.fail("no evidence files found",
                        ["evidence dir %r contains no *.json" % args.evidence_dir])
    led_doc, err = load_json(args.ledger)
    if err:
        return res.fail("cannot load ledger", [err])

    errors, ledger_pairs = [], []
    _collect_candidate_values(led_doc, "ledger(top)", ledger_pairs, errors)
    entries = led_doc.get("entries") if isinstance(led_doc, dict) else None
    if isinstance(entries, list):
        for i, e in enumerate(entries):
            if isinstance(e, dict):
                _collect_candidate_values(e, "ledger entry[%d]" % i,
                                          ledger_pairs, errors)
    ledger_vals = {v for _, v in ledger_pairs}
    if len(ledger_vals) > 1:
        errors.append("ledger itself mixes candidates: %s"
                      % ", ".join(sorted(ledger_vals)))

    series_map, missing_binding = {}, []
    for path in files:
        doc, lerr = load_json(path)
        if lerr:
            errors.append(lerr)
            continue
        pairs = []
        _collect_candidate_values(doc, os.path.basename(path), pairs, errors)
        if not pairs:
            missing_binding.append(os.path.basename(path))
            continue
        series_map.setdefault(series_of(path), set()).update(v for _, v in pairs)

    for series, vals in sorted(series_map.items()):
        if len(vals) > 1:
            errors.append("series %r mixes candidates: %s"
                          % (series, ", ".join(sorted(vals))))
    if missing_binding:
        errors.append("%d evidence file(s) carry no candidate binding: %s"
                      % (len(missing_binding), ", ".join(missing_binding)))
    if len(ledger_vals) == 1:
        final = next(iter(ledger_vals))
        for path in files:
            doc, _ = load_json(path)
            pairs = []
            _collect_candidate_values(doc, os.path.basename(path), pairs, [])
            for _, v in pairs:
                if v != final:
                    errors.append("%s: candidate %s != ledger final candidate %s"
                                  % (os.path.basename(path), v, final))
    if errors:
        return res.fail("candidate binding inconsistent", errors)
    fingerprint = next(iter(ledger_vals)) if len(ledger_vals) == 1 else \
        next(iter(next(iter(series_map.values()))))
    return res.passed("%d evidence file(s) in %d series, single candidate %s.. "
                      "consistent with ledger" % (len(files), len(series_map),
                                                  fingerprint[:12]))


# ---------------------------------------------------------------------------
# check: --exits
# ---------------------------------------------------------------------------

def _audit_evidence_file(path):
    """Return list of structural/exit problems for one evidence file."""
    problems = []
    doc, err = load_json(path)
    if err:
        return [err]
    name = os.path.basename(path)
    if not isinstance(doc, dict):
        return ["%s: top level is not an object" % name]
    scenarios = doc.get("scenarios")
    if not isinstance(scenarios, list):
        return ["%s: 'scenarios' missing or not a list" % name]
    if not scenarios:
        return ["%s: 'scenarios' is empty (empty evidence rejected)" % name]
    elapsed = doc.get("elapsed_ms")
    if isinstance(elapsed, bool) or not isinstance(elapsed, (int, float)) \
            or elapsed < 0:
        problems.append("%s: elapsed_ms missing/not a non-negative number (%r)"
                        % (name, elapsed))
    for i, sc in enumerate(scenarios):
        if not isinstance(sc, dict):
            problems.append("%s: scenarios[%d] is not an object" % (name, i))
            continue
        missing = [k for k in ("name", "status", "detail") if k not in sc]
        if missing:
            problems.append("%s: scenarios[%d] missing field(s): %s"
                            % (name, i, ",".join(missing)))
            continue
        if not isinstance(sc["name"], str) or not sc["name"].strip():
            problems.append("%s: scenarios[%d] has empty name" % (name, i))
        if not isinstance(sc["detail"], str):
            problems.append("%s: scenarios[%d] detail is not a string"
                            % (name, i))
        if sc["status"] != "pass":
            problems.append("%s: scenarios[%d] %r status=%r (non-zero exit)"
                            % (name, i, sc["name"][:60], sc["status"]))
    return problems


def check_exits(args):
    res = CheckResult("exits")
    files = gather_evidence(args)
    if not files:
        return res.fail("no evidence files found",
                        ["evidence dir %r contains no *.json" % args.evidence_dir])
    problems, total_scenarios = [], 0
    for path in files:
        problems.extend(_audit_evidence_file(path))
        doc, _ = load_json(path)
        if isinstance(doc, dict) and isinstance(doc.get("scenarios"), list):
            total_scenarios += len(doc["scenarios"])
    if problems:
        return res.fail("%d problem(s) across %d evidence file(s)"
                        % (len(problems), len(files)), problems)
    return res.passed("all %d scenario(s) in %d evidence file(s) status=pass, "
                      "no missing fields" % (total_scenarios, len(files)))


# ---------------------------------------------------------------------------
# check: --utc
# ---------------------------------------------------------------------------

def check_utc(args):
    res = CheckResult("utc")
    files = gather_evidence(args)
    if not files:
        return res.fail("no evidence files found",
                        ["evidence dir %r contains no *.json" % args.evidence_dir])
    led_doc, err = load_json(args.ledger)
    if err:
        return res.fail("cannot load ledger", [err])

    problems, checked = [], 0

    def _validate(where, value):
        nonlocal checked
        dt, perr = parse_utc(value)
        if perr:
            problems.append("%s: %s" % (where, perr))
            return
        werr = utc_in_window(dt)
        if werr:
            problems.append("%s: %s" % (where, werr))
            return
        checked += 1

    for path in files:
        doc, lerr = load_json(path)
        if lerr:
            problems.append(lerr)
            continue
        name = os.path.basename(path)
        if not isinstance(doc, dict):
            problems.append("%s: top level is not an object" % name)
            continue
        if "utc" not in doc:
            problems.append("%s: top-level 'utc' missing" % name)
        else:
            _validate("%s(utc)" % name, doc["utc"])
    for fpath, value in iter_utc_fields(led_doc, "ledger"):
        _validate("%s: %s" % ("ledger", fpath), value)

    if problems:
        return res.fail("%d invalid timestamp(s)" % len(problems), problems)
    return res.passed("%d timestamp(s) validated (evidence + ledger), all "
                      "tz-aware ISO8601 in sane window" % checked)


CHECKS = {"ids": check_ids, "sha256": check_sha256, "candidate": check_candidate,
          "exits": check_exits, "utc": check_utc}


def run_checks(names, args):
    results = [CHECKS[name](args) for name in names]
    for r in results:
        print("[%s] %s: %s" % ("PASS" if r.ok else "FAIL", r.name, r.summary))
        for line in r.details:
            print("    - %s" % line)
    passed = sum(1 for r in results if r.ok)
    print("SUMMARY: %d/%d checks passed%s"
          % (passed, len(results),
             "" if passed == len(results)
             else " (failed: %s)" % ",".join(r.name for r in results if not r.ok)))
    return passed == len(results)


# ---------------------------------------------------------------------------
# --self-test
# ---------------------------------------------------------------------------

SELFTEST_ROOT = "/tmp/check_cache_closeout_fixtures"
SELFTEST_MARKER = ".check_cache_closeout_fixtures_root"
CAND_A = "ab" * 32
CAND_B = "cd" * 32


def _now_z():
    now = datetime.now(timezone.utc)
    return now.strftime("%Y-%m-%dT%H:%M:%S.") + "%03dZ" % (now.microsecond // 1000)


def _now_offset():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S+00:00")


def _base_ids_doc():
    return {"task_id": TASK_ID, "total": 4,
            "alias_rule": "two-digit -> three-digit when number >= 10",
            "ids": [
                {"id": "W0-01", "kind": "task", "alias_hist_ledger": None},
                {"id": "W0-02", "kind": "task", "alias_hist_ledger": None},
                {"id": "W4-10", "kind": "task", "alias_hist_ledger": "W4-010"},
                {"id": "AC-10", "kind": "ac", "alias_hist_ledger": "AC-010"}]}


def _base_ledger():
    return {"task_id": TASK_ID, "generated_at_utc": _now_offset(),
            "candidate_sha256": CAND_A,
            "entries": [
                {"id": "W0-01", "kind": "task", "new_class": "REVERIFY"},
                {"id": "W0-02", "kind": "task", "new_class": "REVERIFY"},
                {"id": "W4-010", "kind": "task", "new_class": "REVERIFY"},
                {"id": "AC-10", "kind": "ac", "new_class": "REVERIFY"}]}


def _base_baseline():
    return {"task_id": TASK_ID, "generated_at_utc": _now_offset(),
            "plan_binding": {
                "closeout_plan_sha256": EXPECTED_CLOSEOUT_PLAN_SHA256,
                "expected_closeout_sha256": EXPECTED_CLOSEOUT_PLAN_SHA256,
                "hash_match": True,
                "original_plan_sha256": EXPECTED_ORIGINAL_PLAN_SHA256,
                "expected_original_sha256": EXPECTED_ORIGINAL_PLAN_SHA256}}


def _evidence_doc(candidate, statuses):
    scenarios = []
    for i, st in enumerate(statuses, 1):
        scenarios.append({"name": "scenario-%02d" % i, "status": st,
                          "detail": "" if st == "pass" else "induced failure"})
    return {"driver_node": "selftest@nohost", "utc": _now_z(),
            "elapsed_ms": 42100, "candidate_sha256": candidate,
            "scenarios": scenarios}


def _write_json(path, obj):
    with open(path, "w", encoding="utf-8") as f:
        json.dump(obj, f, indent=1, ensure_ascii=False)


def _write_case(dest, closeout_bytes, original_bytes, evidences,
                ids_doc=None, ledger=None):
    raw = os.path.join(dest, "raw")
    os.makedirs(raw, exist_ok=True)
    _write_json(os.path.join(dest, "required-ids.json"),
                ids_doc if ids_doc is not None else _base_ids_doc())
    _write_json(os.path.join(dest, "ledger-v1.json"),
                ledger if ledger is not None else _base_ledger())
    _write_json(os.path.join(dest, "baseline.json"), _base_baseline())
    with open(os.path.join(dest, "closeout-plan.md"), "wb") as f:
        f.write(closeout_bytes)
    with open(os.path.join(dest, "plan-v2.2.1.md"), "wb") as f:
        f.write(original_bytes)
    for fname, obj in evidences.items():
        _write_json(os.path.join(raw, fname), obj)


def _read_file(path):
    with open(path, "rb") as f:
        return f.read()


def _cli_paths(case_dir):
    return ["--required-ids", os.path.join(case_dir, "required-ids.json"),
            "--ledger", os.path.join(case_dir, "ledger-v1.json"),
            "--closeout-plan", os.path.join(case_dir, "closeout-plan.md"),
            "--original-plan", os.path.join(case_dir, "plan-v2.2.1.md"),
            "--baseline", os.path.join(case_dir, "baseline.json"),
            "--evidence-dir", os.path.join(case_dir, "raw")]


def _run_case(cmd):
    return subprocess.run(cmd, capture_output=True, text=True)


def self_test():
    lines = []

    def emit(text):
        print(text)
        lines.append(text)

    for plan in (DEFAULT_CLOSEOUT_PLAN, DEFAULT_ORIGINAL_PLAN):
        if not os.path.isfile(plan):
            print("[FAIL] self-test prerequisite missing: %s" % plan)
            return 1
    closeout_bytes = _read_file(DEFAULT_CLOSEOUT_PLAN)
    original_bytes = _read_file(DEFAULT_ORIGINAL_PLAN)

    if os.path.isdir(SELFTEST_ROOT):
        if not os.path.isfile(os.path.join(SELFTEST_ROOT, SELFTEST_MARKER)):
            print("[FAIL] refusing to rm unknown dir %s" % SELFTEST_ROOT)
            return 1
        shutil.rmtree(SELFTEST_ROOT)
    os.makedirs(SELFTEST_ROOT)
    with open(os.path.join(SELFTEST_ROOT, SELFTEST_MARKER), "w") as f:
        f.write("managed by check_cache_closeout_v1.py --self-test\n")

    ok_evidence = {"verify_r1.json": _evidence_doc(CAND_A, ("pass",) * 3),
                   "verify_r2.json": _evidence_doc(CAND_A, ("pass",) * 3)}
    cases = {}  # label -> (case_dir, flags, expect_fail_check or None)

    def case(label, dirname, flags, expect_fail, **kw):
        dest = os.path.join(SELFTEST_ROOT, dirname)
        _write_case(dest, kw.get("closeout_bytes", closeout_bytes),
                    original_bytes, kw["evidences"],
                    ids_doc=kw.get("ids_doc"), ledger=kw.get("ledger"))
        cases[label] = (dest, flags, expect_fail)

    case("positive: valid fixture passes all five checks", "positive",
         ["--all"], None, evidences=ok_evidence)

    ledger_missing = _base_ledger()
    ledger_missing["entries"] = [e for e in ledger_missing["entries"]
                                 if e["id"] != "W0-02"]
    case("negative-1 missing ID in ledger", "neg_missing_id",
         ["--ids"], "ids", evidences=ok_evidence, ledger=ledger_missing)

    ledger_dup = _base_ledger()
    ledger_dup["entries"].append(dict(ledger_dup["entries"][0]))
    case("negative-2 duplicate ID in ledger", "neg_dup_id",
         ["--ids"], "ids", evidences=ok_evidence, ledger=ledger_dup)

    case("negative-3 tampered plan sha256", "neg_tampered_sha",
         ["--sha256"], "sha256",
         closeout_bytes=closeout_bytes + b"\n# tampered for self-test\n",
         evidences=ok_evidence)

    fail_evidence = {"exitfail_r1.json": _evidence_doc(
        CAND_A, ("pass", "fail", "pass"))}
    case("negative-4 scenario status=fail (non-zero exit)", "neg_exit_fail",
         ["--exits"], "exits", evidences=fail_evidence)

    mixed = {"mixed_r1.json": _evidence_doc(CAND_A, ("pass",) * 3),
             "mixed_r2.json": _evidence_doc(CAND_B, ("pass",) * 3)}
    case("negative-5 mixed candidates in one series", "neg_mixed_candidate",
         ["--candidate"], "candidate", evidences=mixed)

    fake = {"fake_minimal.json": {"status": "pass", "scenarios": []},
            "fake_no_detail.json": {"utc": _now_z(), "elapsed_ms": 5,
                                    "candidate_sha256": CAND_A,
                                    "scenarios": [{"name": "hand-written",
                                                   "status": "pass"}]}}
    case("negative-6 forged PASS structure", "neg_fake_pass",
         ["--exits"], "exits", evidences=fake)

    results = []
    for label, (dest, flags, expect_fail) in cases.items():
        cmd = [sys.executable, SCRIPT_PATH] + flags + _cli_paths(dest)
        proc = _run_case(cmd)
        if expect_fail is None:
            good = proc.returncode == 0 and "[FAIL]" not in proc.stdout \
                and proc.stdout.count("[PASS]") == len(CHECK_ORDER)
            results.append((label, good, proc.returncode))
        else:
            good = proc.returncode != 0 and \
                ("[FAIL] %s" % expect_fail) in proc.stdout
            results.append((label, good, proc.returncode))
        status = "PASS" if good else "FAIL"
        emit("[%s] self-test %s (subprocess exit=%d)" % (status, label,
                                                         proc.returncode))
        if not good:
            for outline in proc.stdout.rstrip().splitlines():
                emit("    | %s" % outline)
            if proc.stderr.strip():
                emit("    ! stderr: %s" % proc.stderr.strip()[:400])

    passed = sum(1 for _, good, _ in results if good)
    emit("SELF-TEST SUMMARY: %d/%d fixture expectations met "
         "(1 positive + 6 negative)" % (passed, len(results)))
    log_path = os.path.join(SELFTEST_ROOT, "selftest.log")
    os.makedirs(SELFTEST_ROOT, exist_ok=True)
    with open(log_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")
    print("self-test log written to %s" % log_path)
    return 0 if passed == len(results) else 1


# ---------------------------------------------------------------------------

def main(argv):
    parser = argparse.ArgumentParser(
        description="C14 mechanical re-verifier for "
                    "CACHE_V221_BACKEND_CLOSEOUT_V1 closeout evidence.")
    parser.add_argument("--ids", action="store_true",
                        help="verify required ids/aliases vs ledger, no dups")
    parser.add_argument("--sha256", action="store_true",
                        help="verify both plan files against frozen sha256")
    parser.add_argument("--candidate", action="store_true",
                        help="verify candidate sha consistency per run series")
    parser.add_argument("--exits", action="store_true",
                        help="verify all evidence scenarios status=pass, complete")
    parser.add_argument("--utc", action="store_true",
                        help="verify evidence/ledger timestamps are valid UTC")
    parser.add_argument("--all", action="store_true", help="run every check")
    parser.add_argument("--self-test", action="store_true",
                        help="build /tmp fixtures and verify 6 negatives fail")
    parser.add_argument("--required-ids", default=DEFAULT_REQUIRED_IDS)
    parser.add_argument("--ledger", default=DEFAULT_LEDGER)
    parser.add_argument("--closeout-plan", default=DEFAULT_CLOSEOUT_PLAN)
    parser.add_argument("--original-plan", default=DEFAULT_ORIGINAL_PLAN)
    parser.add_argument("--baseline", default=DEFAULT_BASELINE)
    parser.add_argument("--evidence-dir", default=DEFAULT_EVIDENCE_DIR)
    parser.add_argument("--evidence", action="append", default=[],
                        help="explicit evidence json path (repeatable)")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()
    names = [n for n in CHECK_ORDER if getattr(args, n) or args.all]
    if not names:
        parser.error("select at least one check (--ids/--sha256/--candidate/"
                     "--exits/--utc/--all) or --self-test")
        return 2
    return 0 if run_checks(names, args) else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
