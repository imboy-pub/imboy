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

--self-test builds unique temporary fixtures (never in the repo) and asserts negative fixtures fail and a positive fixture passes.
Writes selftest.log inside that temporary fixture directory.

stdlib only; Python 3.9 compatible.
"""

import argparse
import hashlib
import json
import math
import os
import re
import subprocess
import sys
from datetime import datetime, timedelta, timezone

TASK_ID = "CACHE_V221_BACKEND_CLOSEOUT_V1"
EXPECTED_IDS_SHA256 = "3fcafa2215ba26b6363141c1138807baa8b53a955067f3e3b9dc560890c972b0"

# Frozen references (given by task owner; never overridable via CLI/env).
EXPECTED_CLOSEOUT_PLAN_SHA256 = (
    "806114da93a904108dfa9a2747d1ee59d4efcb1a2562a92ae7ea6ae905882f0f")
EXPECTED_ORIGINAL_PLAN_SHA256 = (
    "a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713")

SCRIPT_PATH = os.path.abspath(__file__)
REPO_ROOT = os.path.dirname(os.path.dirname(SCRIPT_PATH))
_CO = "docs/architecture/cache-v2.2.1"
DEFAULT_REQUIRED_IDS = os.path.join(REPO_ROOT, _CO, "closeout-v1/required-ids.json")
DEFAULT_LEDGER = os.path.join(REPO_ROOT, _CO, "closeout-v1/ledger-v2.json")
DEFAULT_CLOSEOUT_PLAN = os.path.join(REPO_ROOT, _CO, "backend-closeout-plan-v1.md")
DEFAULT_ORIGINAL_PLAN = os.path.join(REPO_ROOT, _CO, "w0/plan-v2.2.1.md")
DEFAULT_BASELINE = os.path.join(REPO_ROOT, _CO, "closeout-v1/baseline.json")
DEFAULT_EVIDENCE_DIR = None
DEFAULT_MANIFEST = os.path.join(REPO_ROOT, _CO, "closeout-v1/candidate-manifest.json")

CANDIDATE_KEYS = ("candidate_sha256", "candidate", "candidate_digest",
                  "working_tree_digest")
CHECK_ORDER = ("ids", "sha256", "candidate", "exits", "utc", "receipts")

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
        hours, minutes = int(compact[1:3]), int(compact[3:5])
        if hours > 23 or minutes > 59:
            return None, "invalid timezone offset: %r" % tz
        try:
            tzi = timezone(sign * timedelta(hours=hours, minutes=minutes))
        except ValueError as exc:
            return None, "invalid timezone (%s)" % exc
    try:
        dt = datetime(y, mo, d, h, mi, s, micro, tzinfo=tzi)
    except ValueError as exc:
        return None, "invalid date/time value (%s): %r" % (exc, text)
    try:
        return dt.astimezone(timezone.utc), None
    except OverflowError as exc:
        return None, "UTC conversion out of range (%s): %r" % (exc, text)


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


# check: --ids

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
    digest = hashlib.sha256(json.dumps(specs, separators=(",", ":")).encode()).hexdigest()
    if digest != EXPECTED_IDS_SHA256:
        errors.append("required ID/alias contract differs from frozen 96-item set")
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


# check: --sha256

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


# check: --candidate

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
    if not ledger_vals:
        errors.append("ledger has no candidate binding")
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


# check: --exits

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


# check: --utc

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



# Strict closeout receipts are independent of diagnostic driver JSON. A driver
# summary alone cannot prove the shell exit or the complete raw log.
INPUT_PREFIXES = (
    "src/", "include/", "config/", "scripts/", "test/", "priv/migrations/",
    "asn1/", "mibs/", "lib/",
)
INPUT_FILES = {"Makefile", "erlang.mk", "VERSION", "rebar.lock"}
RUNTIME_INPUTS = {
    "config/sys.closeout-local.config",
    "deploy/docker-compose.prod.yml",
    "deploy/prometheus/prometheus.yml",
    "deploy/prometheus/rules/imboy-alerts.yml",
    "deploy/prometheus/rules/imboy-alerts-test.yml",
    "deploy/grafana/dashboards/imboy-cache-sync.json",
    "deploy/grafana/provisioning/dashboards/default.yml",
    "deploy/grafana/provisioning/datasources/prometheus.yml",
}
EXTERNAL_IDS = {"W0-09", "AC-26", "AC-27"}


def candidate_inputs(root):
    proc = subprocess.run(["git", "-C", root, "ls-files", "-z", "--cached",
                           "--others", "--exclude-standard"],
                          capture_output=True, check=True)
    paths = {p for p in proc.stdout.decode().split("\0") if p and
             (p.startswith(INPUT_PREFIXES) or p in INPUT_FILES)}
    ignored = subprocess.run(["git", "-C", root, "ls-files", "-z", "--others",
                              "--ignored", "--exclude-standard", "--",
                              "src", "include", "scripts", "test", "priv/migrations",
                              "asn1", "mibs", "lib"],
                             capture_output=True, check=True)
    source_suffixes = (
        ".erl", ".hrl", ".app.src", ".core", ".xrl", ".yrl", ".asn1",
        ".mib", ".ex", ".py", ".sh", ".sql",
    )
    ignored_sources = sorted(
        p for p in ignored.stdout.decode().split("\0")
        if p.endswith(source_suffixes)
    )
    if ignored_sources:
        raise ValueError("ignored build-consumable sources: %s"
                         % ", ".join(ignored_sources[:20]))
    paths.update(p for p in RUNTIME_INPUTS if os.path.isfile(os.path.join(root, p)))
    # Dependencies are ignored by Git; bind all files that may affect their
    # build or runtime, including extensionless tools and platform build inputs.
    deps_root = os.path.join(root, "deps")
    if os.path.islink(deps_root):
        raise ValueError("dependency root symlink is unsupported: deps")
    root_real = os.path.realpath(root)
    for base, dirs, names in os.walk(deps_root):
        dirs[:] = [d for d in dirs if d not in (".git", "_build")]
        linked_dirs = [d for d in dirs if os.path.islink(os.path.join(base, d))]
        if linked_dirs:
            raise ValueError("dependency directory symlinks are unsupported: %s"
                             % os.path.relpath(os.path.join(base, linked_dirs[0]), root))
        for name in names:
            path = os.path.join(base, name)
            if os.path.islink(path):
                try:
                    inside_root = os.path.commonpath((root_real, os.path.realpath(path))) == root_real
                except ValueError:
                    inside_root = False
                if not inside_root:
                    raise ValueError("dependency file symlink escapes source root: %s"
                                     % os.path.relpath(path, root))
            paths.add(os.path.relpath(path, root))
    for directory in ("src", "test", "priv", "scripts", "include", "config", "deploy", "deps"):
        directory_path = os.path.join(root, directory)
        if os.path.islink(directory_path):
            raise ValueError("candidate directory symlink is unsupported: %s" % directory)
        for base, dirs, names in os.walk(directory_path):
            for name in dirs:
                path = os.path.join(base, name)
                if os.path.islink(path):
                    raise ValueError("candidate directory symlink is unsupported: %s"
                                     % os.path.relpath(path, root))
            for name in names:
                if name.endswith(".dump"):
                    continue
                path = os.path.join(base, name)
                if os.path.commonpath((root_real, os.path.realpath(path))) != root_real:
                    raise ValueError("candidate input escapes source root: %s"
                                     % os.path.relpath(path, root))
                if not os.path.isfile(path):
                    raise ValueError("candidate input is not a regular file: %s"
                                     % os.path.relpath(path, root))
                paths.add(os.path.relpath(path, root))
    paths.update(p for p in INPUT_FILES if os.path.isfile(os.path.join(root, p)))
    for path in paths:
        if os.path.commonpath((root_real, os.path.realpath(os.path.join(root, path)))) != root_real:
            raise ValueError("candidate input escapes source root: %s" % path)
    return sorted(path for path in paths if not path.endswith(".dump"))


def _bound_file(ref, base, errors):
    if not isinstance(ref, dict) or not isinstance(ref.get("path"), str) or not \
            HEX64_RE.fullmatch(str(ref.get("sha256", ""))):
        errors.append("invalid evidence reference: %r" % ref)
        return None
    path = os.path.abspath(os.path.join(base, ref["path"]))
    if not os.path.isfile(path):
        errors.append("missing evidence: %s" % path)
        return None
    if sha256_file(path) != ref["sha256"]:
        errors.append("evidence digest mismatch: %s" % path)
        return None
    return path


def _performance_run_errors(doc, candidate, where, evidence_root):
    """Validate one C12 result in the documented per-arm/per-round format."""
    errors = []
    if not isinstance(doc, dict):
        return ["invalid performance result: %s" % where]
    arm, round_no = doc.get("arm"), doc.get("round")
    if arm not in ("env", "base", "cand") or type(round_no) is not int or round_no < 1:
        return ["performance result lacks arm/round: %s" % where]
    if doc.get("candidate_sha256") != candidate:
        errors.append("performance result candidate mismatch: %s" % where)
    elapsed = doc.get("elapsed_ms")
    if not isinstance(elapsed, (int, float)) or isinstance(elapsed, bool) or not math.isfinite(elapsed) or elapsed <= 0:
        errors.append("performance result elapsed_ms must be positive: %s" % where)
    profile = {key: doc.get(key) for key in
               ("operations", "concurrency", "keyspace", "value_bytes", "ttl_seconds")}
    profile_sha = hashlib.sha256(json.dumps(profile, sort_keys=True,
        separators=(",", ":")).encode()).hexdigest()
    if doc.get("profile_sha256") != profile_sha:
        errors.append("performance result profile digest invalid: %s" % where)
    budget = "budget_v1: Nmax=50000 Nmax_bytes=16MiB Tmax=20000 Tmax_bytes=8MiB Bmax=2048 Bmax_bytes=32MiB Pmax=256 Pmax_bytes=4MiB Rmax=2 Rmax_rounds=3 Dmax=300s MaxValue=32768B MaxTTL=600s HighWater=10000 W=720s"
    if doc.get("budget_digest") != budget:
        errors.append("performance result differs from frozen C01 budget: %s" % where)
    if doc.get("operations") != 10000 or doc.get("operations_per_worker") != 10000 or doc.get("concurrency") != [1, 8, 32] or doc.get("keyspace") != 1024 or doc.get("value_bytes") != 256 or doc.get("ttl_seconds") != (3600 if arm == "env" else 600):
        errors.append("performance profile differs from frozen C12 profile: %s" % where)
    load = doc.get("load")
    load_keys = ("before_1m", "after_1m", "before_15m", "after_15m")
    if not isinstance(load, dict) or any(not isinstance(load.get(k), (int, float)) or isinstance(load.get(k), bool) or not math.isfinite(load[k]) or load[k] < 0 for k in load_keys) or max(load.get("before_1m", 99), load.get("after_1m", 99)) > 6 or max(load.get("before_15m", 99), load.get("after_15m", 99)) > 8 or doc.get("parallel_full_load") is not False:
        errors.append("performance result violates quiet-window limits: %s" % where)
    if type(doc.get("compile_exit")) is not int or doc["compile_exit"] != 0 or type(doc.get("run_exit")) is not int or doc["run_exit"] != 0:
        errors.append("performance result compile/run exit is not zero: %s" % where)
    scenarios = doc.get("scenarios")
    names = [s.get("name") for s in scenarios if isinstance(s, dict)] if isinstance(scenarios, list) else []
    required = {"environment_anchor"} if arm == "env" else {"profile_self_check", "throughput", "p99", "convergence", "resource_envelope"}
    if any(not isinstance(name, str) for name in names) or set(names) != required or len(names) != len(required):
        errors.append("performance result scenario names differ from plan: %s" % where)
    by_name = {s["name"]: s for s in scenarios if isinstance(s, dict) and isinstance(s.get("name"), str)} if isinstance(scenarios, list) else {}
    for name in required:
        scenario = by_name.get(name, {})
        if scenario.get("status") != "pass" or not isinstance(scenario.get("detail"), str) or not scenario["detail"].strip():
            errors.append("performance scenario is not a detailed PASS (%s): %s" % (name, where))
    def number(value):
        return isinstance(value, (int, float)) and not isinstance(value, bool) and math.isfinite(value) and value >= 0
    if arm == "env":
        anchor = by_name.get("environment_anchor", {})
        artifacts = anchor.get("artifacts")
        if not isinstance(artifacts, dict) or set(artifacts) != {"env", "w0_r7", "w0_r8"}:
            return errors + ["W0 anchor must reference env/r7/r8 runner artifacts: %s" % where]
        runs = {}
        component_hashes = []
        for role, files in artifacts.items():
            if not isinstance(files, dict) or set(files) != {"baseline", "performance", "sha256"}:
                errors.append("W0 %s references must include baseline/performance/sha256.json" % role)
                continue
            docs = {}
            paths = {}
            for kind, ref in files.items():
                path = _bound_file(ref, os.path.dirname(where), errors)
                if path is None:
                    continue
                try:
                    in_root = os.path.commonpath((os.path.realpath(evidence_root), os.path.realpath(path))) == os.path.realpath(evidence_root)
                except ValueError:
                    in_root = False
                if not in_root:
                    errors.append("W0 raw artifact escapes evidence root: %s" % path)
                    continue
                docs[kind], load_error = load_json(path)
                paths[kind] = path
                if load_error or not isinstance(docs[kind], dict):
                    errors.append("invalid W0 %s artifact: %s" % (role, load_error or path))
            if len(docs) != 3:
                continue
            manifest = docs["sha256"]
            for name, kind in (("baseline.json", "baseline"), ("performance.json", "performance")):
                if manifest.get(name) != sha256_file(paths[kind]):
                    errors.append("W0 sha256.json does not bind %s: %s" % (name, role))
            baseline = docs["baseline"]
            if type(baseline.get("compile_exit")) is not int or baseline["compile_exit"] != 0 or type(baseline.get("run_exit")) is not int or baseline["run_exit"] != 0:
                errors.append("W0 %s compile/run did not pass: %s" % (role, paths["baseline"]))
            if baseline.get("operations") != 10000:
                errors.append("W0 %s baseline operations differ from frozen profile" % role)
            deps_hashes = baseline.get("dependency_hashes")
            scripts_hashes = baseline.get("script_hashes")
            if not isinstance(deps_hashes, dict) or not isinstance(scripts_hashes, dict):
                errors.append("W0 %s lacks dependency/script hash maps" % role)
                continue
            hashes = dict(deps_hashes)
            hashes.update(scripts_hashes)
            if len(hashes) != 30 or any(not isinstance(value, str) or not HEX64_RE.fullmatch(value) for value in hashes.values()):
                errors.append("W0 %s must bind all 30 dependency/script components: %s" % (role, paths["baseline"]))
            component_hashes.append(hashes)
            runs[role] = docs["performance"]
        if len(component_hashes) == 3 and any(item != component_hashes[0] for item in component_hashes[1:]):
            errors.append("W0 env/r7/r8 dependency and runner component hashes differ")
        def rows_for(performance):
            if not isinstance(performance, dict) or performance.get("operations_per_worker") != 10000 or performance.get("keyspace") != 1024 or performance.get("value_bytes") != 256 or performance.get("ttl_seconds") != 3600 or not isinstance(performance.get("single_node"), list):
                return {}
            rows = {}
            expected_single = {(kind, workers) for kind in
                               ("get", "get_wait", "set", "mixed", "memo", "syn")
                               for workers in (1, 8, 32)}
            for row in performance["single_node"]:
                if not isinstance(row, dict) or not isinstance(row.get("kind"), str) or type(row.get("workers")) is not int:
                    return {}
                key = (row["kind"], row["workers"])
                if key not in expected_single or key in rows or row.get("operations") != row["workers"] * 10000:
                    return {}
                rows[key] = row
            three = performance.get("three_node")
            if (set(rows) != expected_single or not isinstance(three, dict) or
                    three.get("status") != "measured" or three.get("nodes") != 3 or
                    not isinstance(three.get("results"), list)):
                return {}
            for row in three["results"]:
                if not isinstance(row, dict) or type(row.get("workers")) is not int:
                    return {}
                key = ("three_node_syn", row["workers"])
                if row["workers"] not in (1, 8, 32) or key in rows or row.get("operations") != row["workers"] * 10000:
                    return {}
                rows[key] = row
            return rows
        measured, r7, r8 = (rows_for(runs.get(role)) for role in ("env", "w0_r7", "w0_r8"))
        required_rows = {(kind, workers) for kind in ("get", "get_wait", "set", "mixed", "memo", "syn")
                         for workers in (1, 8, 32)} | {("three_node_syn", workers) for workers in (1, 8, 32)}
        if any(set(rows) != required_rows for rows in (measured, r7, r8)):
            errors.append("W0 env/r7/r8 performance rows incomplete")
        else:
            for key in required_rows:
                for metric in ("throughput_ops_sec", "p50_us", "p95_us", "p99_us"):
                    actual = measured[key].get(metric)
                    if any(not number(actual) or not number(reference[key].get(metric)) or reference[key][metric] <= 0 or not reference[key][metric] * 0.9 <= actual <= reference[key][metric] * 1.1 for reference in (r7, r8)):
                        errors.append("W0 environment metric outside r7/r8 noise band: %s/%s/%s" % (where, key, metric))
                if any(type(rows[key].get("errors")) is not int or rows[key]["errors"] != 0 or not isinstance(rows[key].get("error_rate"), (int, float)) or isinstance(rows[key].get("error_rate"), bool) or not math.isfinite(rows[key]["error_rate"]) or rows[key]["error_rate"] != 0 for rows in (measured, r7, r8)):
                    errors.append("W0 environment or reference run has errors: %s/%s" % (where, key))
        return errors
    check = by_name.get("profile_self_check", {}).get("checks", {})
    if not isinstance(check, dict) or set(check) != {"memo_single_flight", "invalidation_race", "loader_exception", "writer_crash", "get_wait_handoff"} or any(value is not True for value in check.values()):
        errors.append("performance self-check values incomplete/false: %s" % where)
    throughput, p99, convergence, resource = (by_name.get(name, {}) for name in ("throughput", "p99", "convergence", "resource_envelope"))
    if not number(throughput.get("ops_per_sec")) or throughput.get("ops_per_sec", 0) <= 0 or type(throughput.get("errors")) is not int or throughput["errors"] != 0 or isinstance(throughput.get("error_rate"), bool) or throughput.get("error_rate") != 0:
        errors.append("throughput missing or has errors: %s" % where)
    if any(not number(p99.get(key)) or p99.get(key, 0) <= 0 for key in ("get_us", "set_us")) or any(not number(p99.get(key)) for key in ("get_p50_us", "get_p95_us", "set_p50_us", "set_p95_us")) or type(p99.get("errors")) is not int or p99["errors"] != 0 or isinstance(p99.get("error_rate"), bool) or p99.get("error_rate") != 0:
        errors.append("P99 missing or has errors: %s" % where)
    if not number(convergence.get("set_ms")) or convergence.get("set_ms", 0) <= 0 or type(convergence.get("errors")) is not int or convergence["errors"] != 0 or isinstance(convergence.get("error_rate"), bool) or convergence.get("error_rate") != 0:
        errors.append("convergence missing or has errors: %s" % where)
    limits = {"n_count": 50000, "n_bytes": 16 * 1024 * 1024, "t_count": 20000,
              "t_bytes": 8 * 1024 * 1024, "journal_count": 2048,
              "journal_bytes": 32 * 1024 * 1024, "buffer_count": 2048,
              "buffer_bytes": 32 * 1024 * 1024, "page_count": 256,
              "page_bytes": 4 * 1024 * 1024, "generation_count": 50000,
              "generation_bytes": 6400000, "grace_count": 256, "peer_count": 64}
    observed = resource.get("budget_usage", {})
    if not isinstance(observed, dict):
        observed = {}
    if any(not number(resource.get(k)) for k in ("mailbox_peak", "heap_peak_bytes", "metadata_peak_bytes")) or any(type(observed.get(k)) is not int or observed[k] < 0 or observed[k] > cap for k, cap in limits.items()):
        errors.append("resource usage missing or exceeds frozen C01 limits: %s" % where)
    if all(number(p99.get(key)) for key in ("get_us", "set_us")) and number(convergence.get("set_ms")) and (p99["get_us"] > 2000 or p99["set_us"] > 5000 or convergence["set_ms"] > 1000):
        errors.append("performance absolute threshold exceeded: %s" % where)
    return errors


def _manifest_binding(args, errors):
    doc, err = load_json(args.candidate_manifest)
    if err:
        errors.append(err)
        return None, None
    if not isinstance(doc, dict) or not isinstance(doc.get("files"), list) or not doc["files"]:
        errors.append("candidate manifest lacks nonempty files")
        return None, None
    rows = doc["files"]
    paths = [r.get("path") for r in rows if isinstance(r, dict)]
    try:
        expected = candidate_inputs(args.source_root)
    except (OSError, ValueError, subprocess.CalledProcessError) as exc:
        errors.append("cannot inventory current candidate: %s" % exc)
        return None, None
    if paths != expected:
        errors.append("candidate manifest input set/order != current source inventory")
    for row in rows:
        path = row.get("path") if isinstance(row, dict) else None
        if not isinstance(path, str) or path not in expected:
            errors.append("invalid candidate input: %r" % row)
            continue
        _bound_file(row, args.source_root, errors)
    digest = hashlib.sha256(json.dumps(rows, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    if doc.get("candidate_sha256") != digest:
        errors.append("candidate manifest digest mismatch")
    frozen, err = parse_utc(doc.get("frozen_at_utc"))
    if err or utc_in_window(frozen):
        errors.append("invalid candidate freeze timestamp")
        frozen = None
    return digest, frozen


def check_receipts(args):
    res = CheckResult("receipts")
    errors = []
    candidate, frozen = _manifest_binding(args, errors)
    ledger, err = load_json(args.ledger)
    req, reqerr = load_json(args.required_ids)
    if err or reqerr or not isinstance(ledger, dict):
        return res.fail("receipt inputs invalid", errors + [err or reqerr or "invalid ledger"])
    if ledger.get("candidate_sha256") != candidate:
        errors.append("ledger candidate is absent or differs from current source")
    specs, spec_errors = _parse_required_ids(req)
    errors.extend(spec_errors)
    entries = {e.get("id"): e for e in ledger.get("entries", []) if isinstance(e, dict)}
    tasks = ledger.get("closeout_tasks")
    if not isinstance(tasks, list):
        errors.append("missing C00-C14 closeout_tasks")
        tasks = []
    task_ids = [e.get("id") for e in tasks if isinstance(e, dict)]
    if sorted(task_ids) != ["C%02d" % i for i in range(15)]:
        errors.append("closeout_tasks must contain C00-C14 exactly once")
    required = list(tasks)
    for rid, alias in specs:
        e = entries.get(rid, entries.get(alias))
        if not isinstance(e, dict):
            errors.append("missing required entry %s" % rid)
            continue
        if args.scope == "backend" and rid in EXTERNAL_IDS and e.get("status") == "BLOCKED_EXTERNAL":
            if not all(e.get(k) for k in ("owner", "missing_inputs", "resume_condition")):
                errors.append("external blocker %s lacks owner/inputs/resume" % rid)
            continue
        required.append(e)
    checked = set()
    final_gates = set()
    performance_runs, performance_paths = [], set()
    legal_ids = set(entries) | set(task_ids)
    for entry in required:
        if not isinstance(entry, dict) or entry.get("status") != "PASS":
            errors.append("required item is not PASS: %r" % entry.get("id") if isinstance(entry, dict) else "invalid required item")
            continue
        refs = entry.get("receipts")
        if not isinstance(refs, list) or not refs:
            errors.append("PASS %s lacks receipts" % entry.get("id"))
            continue
        for ref in refs:
            path = _bound_file(ref, os.path.dirname(args.ledger), errors)
            if path is None:
                continue
            receipt, err = load_json(path)
            if err or not isinstance(receipt, dict):
                errors.append(err or "invalid receipt %s" % path)
                continue
            covers = receipt.get("covers")
            if not isinstance(covers, list) or not covers or any(not isinstance(v, str) or v not in legal_ids for v in covers) or len(set(covers)) != len(covers):
                errors.append("covers must be a unique nonempty legal ID list: %s" % path)
                covers = []
            if entry.get("id") not in covers:
                errors.append("receipt does not cover required item %s: %s" % (entry.get("id"), path))
            if entry.get("id") == "C12":
                gate = receipt.get("gate")
                if isinstance(gate, str):
                    final_gates.add(gate)
                else:
                    errors.append("invalid gate: %s" % path)
            if path in checked:
                continue
            checked.add(path)
            if receipt.get("task_id") != TASK_ID or receipt.get("candidate_sha256") != candidate:
                errors.append("receipt task/candidate mismatch: %s" % path)
            if type(receipt.get("exit_code")) is not int or receipt["exit_code"] != 0:
                errors.append("real command exit is not zero: %s" % path)
            for key in ("command", "oracle", "input_sha256"):
                if not isinstance(receipt.get(key), str) or not receipt[key].strip():
                    errors.append("receipt missing %s: %s" % (key, path))
            if receipt.get("input_sha256") != candidate:
                errors.append("input digest does not bind frozen candidate: %s" % path)
            times = []
            for key in ("started_at_utc", "finished_at_utc"):
                dt, err = parse_utc(receipt.get(key))
                if err or utc_in_window(dt):
                    errors.append("invalid receipt time %s: %s" % (key, path))
                else:
                    times.append(dt)
            if len(times) == 2 and (times[0] > times[1] or (frozen and times[0] < frozen)):
                errors.append("receipt predates frozen candidate or time reversed: %s" % path)
            if receipt.get("gate") == "performance" and len(times) == 2 and times[0] >= times[1]:
                errors.append("performance receipt interval must have positive duration: %s" % path)
            counts = receipt.get("counts")
            if not isinstance(counts, dict) or any(type(counts.get(k)) is not int or counts[k] < 0 for k in ("passed", "failed", "cancelled", "skipped")):
                errors.append("receipt counts missing/invalid: %s" % path)
            elif counts["passed"] == 0 or any(counts[k] != 0 for k in ("failed", "cancelled", "skipped")):
                errors.append("receipt has failures/cancellations/skips or zero passes: %s" % path)
            log = _bound_file(receipt.get("log"), os.path.dirname(path), errors)
            result = _bound_file(receipt.get("result"), os.path.dirname(path), errors)
            count_kind = receipt.get("count_kind")
            if count_kind not in ("scenarios", "tests", "checks"):
                errors.append("missing/invalid count_kind: %s" % path)
            if receipt.get("gate") == "full_eunit" and count_kind != "tests":
                errors.append("full_eunit requires native EUnit test counts: %s" % path)
            if log:
                body = _read_file(log).decode("utf-8", errors="replace")
                if re.search(r"(?im)(\[FAIL\]|\*failed\*|\*timed out\*|crash dump|unexpected termination|tests? (?:were )?cancelled|Failed:\s*[1-9]|Skipped:\s*[1-9]|\*\*\s+Generic server\b[^\n]*\bterminating\b|\*\*\s+Reason for termination\b|COMPONENT_LIVENESS\s*(?:[:=]\s*)?false\b)", body):
                    errors.append("failure/cancellation marker in complete log: %s" % log)
                if count_kind == "tests" and isinstance(counts, dict):
                    matches = re.findall(r"(?m)(?:All (\d+) tests passed|Passed:\s*(\d+))", body)
                    if not matches or int(next(x for x in matches[-1] if x)) != counts.get("passed"):
                        errors.append("EUnit log count does not match receipt: %s" % log)
                marker = "CLOSEOUT_COUNTS " + json.dumps(counts, sort_keys=True, separators=(",", ":"))
                if body.count(marker) != 1:
                    errors.append("complete log lacks unique matching count footer: %s" % log)
            if result:
                rd, err = load_json(result)
                result_dt = None
                if err or not isinstance(rd, dict) or rd.get("candidate_sha256") != candidate or rd.get("counts") != counts:
                    errors.append("result candidate/count mismatch: %s" % result)
                if isinstance(rd, dict):
                    result_utc, utc_err = parse_utc(rd.get("utc"))
                    if utc_err or utc_in_window(result_utc):
                        errors.append("result has missing/invalid UTC: %s" % result)
                    else:
                        result_dt = result_utc
                    for key in ("exit", "exit_code"):
                        if key in rd and (type(rd[key]) is not int or rd[key] != 0):
                            errors.append("result reports non-zero %s: %s" % (key, result))
                errors.extend(_audit_evidence_file(result))
                if receipt.get("gate") == "performance" and isinstance(rd, dict):
                    evidence_root = args.evidence_dir or os.path.dirname(args.ledger)
                    errors.extend(_performance_run_errors(rd, candidate, result, evidence_root))
                    if len(times) == 2 and result_dt is not None and not times[0] <= result_dt <= times[1]:
                        errors.append("performance result UTC is outside command receipt interval: %s" % result)
                    if result not in performance_paths:
                        performance_paths.add(result)
                        performance_runs.append((rd, times[0], times[1]) if len(times) == 2 else (rd, None, None))
                if count_kind == "scenarios" and isinstance(rd, dict) and isinstance(rd.get("scenarios"), list) and isinstance(counts, dict):
                    passed = sum(isinstance(sc, dict) and sc.get("status") == "pass" for sc in rd["scenarios"])
                    if counts.get("passed") != passed or len(rd["scenarios"]) != passed:
                        errors.append("scenario/count mismatch: %s" % result)
    expected_gates = {"compile", "targeted", "three_node", "mixed", "rollback", "qr", "full_eunit", "performance", "independent_review"}
    if not expected_gates.issubset(final_gates):
        errors.append("C12 lacks final-candidate gates: %s" % ",".join(sorted(expected_gates - final_gates)))
    expected_perf = [("env", 1)] + [(arm, round_no) for round_no in range(1, 4)
                                    for arm in ("base", "cand")]
    actual_perf = [(run.get("arm"), run.get("round")) for run, _, _ in performance_runs]
    if actual_perf != expected_perf:
        errors.append("C12 performance receipts must be ordered env_r1 then three base/candidate pairs")
    for previous, current in zip(performance_runs, performance_runs[1:]):
        if previous[2] is None or current[1] is None or previous[2] >= current[1]:
            errors.append("C12 performance receipt intervals are not strictly chronological")
            break
    perf_by_arm_round = {(run.get("arm"), run.get("round")): run for run, _, _ in performance_runs
                         if run.get("arm") in ("env", "base", "cand") and type(run.get("round")) is int}
    v2_profiles = {run.get("profile_sha256") for run, _, _ in performance_runs
                   if run.get("arm") in ("base", "cand") and
                   isinstance(run.get("profile_sha256"), str)}
    if len(v2_profiles) != 1:
        errors.append("C12 base/candidate performance receipts use mixed profiles")
    for round_no in range(1, 4):
        base = perf_by_arm_round.get(("base", round_no), {})
        cand = perf_by_arm_round.get(("cand", round_no), {})
        def scenario_metric(run, name, key):
            scenarios = run.get("scenarios") if isinstance(run.get("scenarios"), list) else []
            return next((s.get(key) for s in scenarios
                         if isinstance(s, dict) and s.get("name") == name), None)
        base_get = scenario_metric(base, "p99", "get_us")
        cand_get = scenario_metric(cand, "p99", "get_us")
        base_throughput = scenario_metric(base, "throughput", "ops_per_sec")
        cand_throughput = scenario_metric(cand, "throughput", "ops_per_sec")
        if all(isinstance(v, (int, float)) and not isinstance(v, bool) and math.isfinite(v) and v > 0
               for v in (base_get, cand_get, base_throughput, cand_throughput)) and (
                cand_get > base_get * 1.10 or cand_throughput < base_throughput * 0.95):
            errors.append("C12 performance relative threshold exceeded in pair %d" % round_no)
    if errors:
        return res.fail("closeout receipts/current source binding rejected", errors)
    return res.passed("%d complete command receipts bound to current candidate %s" % (len(checked), candidate))


CHECKS = {"ids": check_ids, "sha256": check_sha256, "candidate": check_candidate,
          "exits": check_exits, "utc": check_utc, "receipts": check_receipts}


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


# --self-test

def _read_file(path):
    with open(path, "rb") as f:
        return f.read()


def self_test():
    import runpy
    import tempfile
    from types import SimpleNamespace

    with tempfile.TemporaryDirectory(prefix="cache-input-selftest-") as temporary:
        root = os.path.join(temporary, "source")
        os.mkdir(root)
        subprocess.run(["git", "init", "-q", root], check=True)
        with open(os.path.join(root, ".gitignore"), "w") as output:
            output.write("/deploy/plain.txt\n/priv/plain.txt\n/config/sys.runtime.config\n")
        inputs = ("Makefile", "deploy/plain.txt", "priv/plain.txt",
                  "config/sys.runtime.config", "test/.hidden/generated.beam",
                  "deps/example/_build/generated.beam", "deps/example/.git/input")
        for relative in inputs + ("priv/crash.dump", ".root-input"):
            path = os.path.join(root, relative)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as output:
                output.write("fixture\n")
        assert candidate_inputs(root) == sorted(inputs)
        rows = [{"path": path, "sha256": sha256_file(os.path.join(root, path))}
                for path in candidate_inputs(root)]
        manifest = os.path.join(temporary, "manifest.json")
        args = SimpleNamespace(candidate_manifest=manifest, source_root=root)

        def binding_errors(entries):
            digest = hashlib.sha256(json.dumps(entries, sort_keys=True,
                                               separators=(",", ":")).encode()).hexdigest()
            with open(manifest, "w") as output:
                json.dump({"files": entries, "candidate_sha256": digest,
                           "frozen_at_utc": (datetime.now(timezone.utc) -
                                             timedelta(seconds=1)).isoformat()}, output)
            errors = []
            _manifest_binding(args, errors)
            return errors

        assert not binding_errors(rows)
        for missing in ("deploy/plain.txt", "priv/plain.txt"):
            assert any("input set/order" in error for error in
                       binding_errors([row for row in rows if row["path"] != missing]))
        assert any("input set/order" in error for error in binding_errors(rows + [rows[0]]))
        assert any("input set/order" in error for error in binding_errors(list(reversed(rows))))
        for escaped in ("../outside", os.path.join(temporary, "outside")):
            assert any("invalid candidate input" in error for error in
                       binding_errors(rows + [{"path": escaped, "sha256": "0" * 64}]))
        with open(os.path.join(root, "deploy/plain.txt"), "w") as output:
            output.write("changed\n")
        assert any("digest mismatch" in error for error in binding_errors(rows))
        outside = os.path.join(temporary, "outside")
        with open(outside, "w") as output:
            output.write("outside\n")
        os.symlink(outside, os.path.join(root, "priv", "escape"))
        try:
            candidate_inputs(root)
        except ValueError as error:
            assert "escapes source root" in str(error)
        else:
            raise AssertionError("escaped physical input accepted")
    print("[PASS] physical input coverage, manifest omissions/tampering/order/escape")
    path = os.path.join(REPO_ROOT, "test/scripts/cache_closeout_verifier_selftest.py")
    return runpy.run_path(path)["self_test"]()



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
    parser.add_argument("--receipts", action="store_true", help="strict statuses, command receipts and current source binding")
    parser.add_argument("--candidate-manifest", default=DEFAULT_MANIFEST)
    parser.add_argument("--source-root", default=REPO_ROOT)
    parser.add_argument("--scope", choices=("backend", "original"), default="backend")
    parser.add_argument("--all", action="store_true", help="run every check")
    parser.add_argument("--self-test", action="store_true",
                        help="build unique temporary fixtures and verify rejection oracles")
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
