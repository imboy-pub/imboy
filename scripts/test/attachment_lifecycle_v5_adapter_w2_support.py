#!/usr/bin/env python3
"""Shared strict parsing helpers for attachment-lifecycle V5 case adapters.

Every helper is fail-closed: a missing artifact, an unparsable line, a
non-green suite result or an unresolvable run-owned resource raises
AdapterGap, which the calling adapter turns into sys.exit(30).  No helper
invents a measurement value, a route, a handler or a request id; values are
always parsed from the case directory or from the frozen run state.
"""
import datetime
import json
import os
import re
import sys

LEAF_FIELDS = (
    "ac_id",
    "domain",
    "driver",
    "entrypoint",
    "ordering_or_fault",
    "schema_phase",
)
ORACLE_FIELDS = (
    "pre_db_state",
    "pre_object_hash",
    "action",
    "expected_http_semantics",
    "expected_db_delta",
    "expected_object_state",
    "proof_actor",
)
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
SLOT_DB_RE = re.compile(r"^attv5_(slot[0-9]+)_[a-z0-9_]+$")
DB_ASSIGN_RE = re.compile(r'database\s*=>\s*"([^"]+)"')
PROVENANCE_MARK_RE = re.compile(r"ENTRYPOINT[_-]PROVENANCE\s+(\{.*\})\s*$")
ENTRYPOINT_ALIASES = {"internal": "enterprise_internal"}


def _entrypoint_matches(value, entrypoint):
    return value == entrypoint or ENTRYPOINT_ALIASES.get(value) == entrypoint


class AdapterGap(Exception):
    """A required, non-synthesizable input is missing or malformed."""


def gap(message):
    raise AdapterGap(message)


class Gaps(object):
    """Collect independent gaps so one run reports every missing field."""

    def __init__(self):
        self.items = []

    def add(self, message):
        self.items.append(str(message))

    def check(self):
        if self.items:
            head = self.items[:30]
            more = "" if len(self.items) <= 30 else (
                "\n  - ... %d more gap(s)" % (len(self.items) - 30))
            raise AdapterGap("%d evidence gap(s):\n  - %s%s"
                             % (len(self.items), "\n  - ".join(head), more))


def now_utc():
    return datetime.datetime.now(datetime.timezone.utc)


def iso(value):
    return value.strftime("%Y-%m-%dT%H:%M:%SZ")


def read_text(path, what):
    if not os.path.isfile(path):
        gap("missing %s: %s" % (what, path))
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return handle.read()
    except OSError as exc:
        gap("cannot read %s (%s): %s" % (what, path, exc))


def read_json(path, what):
    text = read_text(path, what)
    try:
        return json.loads(text)
    except ValueError as exc:
        gap("invalid JSON in %s (%s): %s" % (what, path, exc))


def load_context(run_dir):
    """Read frozen run state, contract and the authorization allowlist."""
    state = read_json(os.path.join(run_dir, "state.json"), "run state")
    if not isinstance(state, dict):
        gap("run state is not an object")
    plan_dir = state.get("plan_dir")
    if not isinstance(plan_dir, str) or not plan_dir:
        gap("run state has no plan_dir; cannot load the frozen contract")
    contract = read_json(os.path.join(plan_dir, "execution-contract.json"),
                         "frozen execution contract")
    allowlist = state.get("resource_allowlist")
    if not isinstance(allowlist, dict):
        auth_path = state.get("authorization_path")
        if not isinstance(auth_path, str) or not auth_path:
            gap("run state has no resource_allowlist and no authorization_path")
        auth = read_json(auth_path, "authorization receipt")
        allowlist = auth.get("resource_allowlist") \
            if isinstance(auth, dict) else None
    if not isinstance(allowlist, dict) or not allowlist:
        gap("resource allowlist is missing from run state/authorization")
    return {
        "state": state,
        "contract": contract,
        "allowlist": allowlist,
        "plan_dir": plan_dir,
        "run_dir": run_dir,
    }


def case_leaves(contract, case_id):
    matrix = contract.get("expected_leaf_matrix") if isinstance(contract, dict) else None
    leaves = (matrix or {}).get(case_id)
    if not isinstance(leaves, list) or not leaves:
        gap("case %s not present in expected_leaf_matrix" % case_id)
    return leaves


def _db_from_config(path):
    text = read_text(path, "isolated eunit config")
    for match in DB_ASSIGN_RE.finditer(text):
        value = match.group(1)
        if value.startswith("attv5_"):
            return value
    return None


def resolve_slot(run_dir):
    """Resolve the single fixture slot from the gate-generated configs.

    The gate exports EUNIT_CONFIG for the current case, so it wins when it
    names an allowlisted attv5 database; otherwise the run-owned
    resources/*-config*/isolated*.config copies are scanned.
    """
    candidates = []
    env_conf = os.environ.get("EUNIT_CONFIG")
    if env_conf:
        path = env_conf if env_conf.endswith(".config") else env_conf + ".config"
        if os.path.isfile(path):
            db = _db_from_config(path)
            if db:
                candidates.append((db, path))
    if len(candidates) == 1:
        match = SLOT_DB_RE.match(candidates[0][0])
        if match:
            return _checked_slot(run_dir, match.group(1))
    if candidates:
        candidates = []
    resources = os.path.join(run_dir, "resources")
    if os.path.isdir(resources):
        for name in sorted(os.listdir(resources)):
            directory = os.path.join(resources, name)
            if "-config" not in name or not os.path.isdir(directory):
                continue
            for filename in sorted(os.listdir(directory)):
                if not (filename.startswith("isolated") and filename.endswith(".config")):
                    continue
                path = os.path.join(directory, filename)
                db = _db_from_config(path)
                if db:
                    candidates.append((db, path))
    slots = set()
    for db, _path in candidates:
        match = SLOT_DB_RE.match(db)
        if match:
            slots.add(match.group(1))
    if len(slots) != 1:
        gap("cannot resolve a single fixture slot from isolated configs "
            "(candidates=%r)" % sorted(candidates))
    return _checked_slot(run_dir, slots.pop())


def _checked_slot(run_dir, slot):
    resources = os.path.join(run_dir, "resources")
    receipt_path = os.path.join(resources, "postgres.json")
    if os.path.isfile(receipt_path):
        receipt = read_json(receipt_path, "postgres resource receipt")
        allowed_slots = receipt.get("allowed_slots") \
            if isinstance(receipt, dict) else None
        if isinstance(allowed_slots, list) and slot not in allowed_slots:
            gap("slot %r is not in the run-owned postgres receipt" % slot)
    return slot


def db_resource(allowlist, slot, suffix):
    rid = "attv5_%s_%s" % (slot, suffix)
    allowed = allowlist.get("scratch_postgres")
    if not isinstance(allowed, list) or rid not in allowed:
        gap("scratch_postgres %r is not in the authorization allowlist" % rid)
    return rid


def garage_bucket(run_dir, allowlist):
    allowed = allowlist.get("garage_bucket")
    if not isinstance(allowed, list) or not allowed:
        gap("garage_bucket allowlist entry is missing")
    env_path = os.path.join(run_dir, "resources", "garage-w202.env")
    value = ""
    if os.path.isfile(env_path):
        for line in read_text(env_path, "W202 garage env").splitlines():
            if line.startswith("W202_GARAGE_BUCKET="):
                value = line.split("=", 1)[1].strip().strip('"').strip("'")
                break
    if value:
        if value not in allowed:
            gap("W202_GARAGE_BUCKET=%r is outside the authorization allowlist"
                % value)
        return value
    if len(allowed) == 1:
        return allowed[0]
    gap("cannot resolve the real garage bucket (garage-w202.env missing and "
        "allowlist has %d candidates)" % len(allowed))


def w201_migration_count(case_dir):
    """Read the latest w201 replay count and reject a failed latest section.

    raw.log is append-only across gate retries, so only the last
    "w201 replayed N up files" segment describes the current attempt.
    """
    raw = read_text(os.path.join(case_dir, "raw.log"), "w201 raw log")
    marker = "w201 replayed "
    if marker not in raw:
        gap("CASE_DIR/raw.log has no 'w201 replayed N up files' line; "
            "migration replay cannot be evidenced")
    latest = raw.rsplit(marker, 1)[1]
    match = re.match(r"(\d+) up files", latest)
    if not match or int(match.group(1)) < 1:
        gap("CASE_DIR/raw.log latest replay has no positive up-file count")
    if "SECTION_FAIL" in latest:
        gap("CASE_DIR/raw.log latest w201 attempt contains SECTION_FAIL; "
            "a probe section did not complete")
    return int(match.group(1))


def load_probe_section(case_dir, section):
    """Reject SQL-only W2-01 output as HTTP acceptance evidence."""
    # shortcut: SQL probe logs cannot prove HTTP calls; replace when real
    # request and response observations are bound to the candidate.
    gap("W2-01 %s SQL probe artifacts in %s cannot establish real HTTP entrypoint evidence"
        % (section, case_dir))


def try_section(case_dir, section, gaps):
    try:
        return load_probe_section(case_dir, section)
    except AdapterGap as exc:
        gaps.add(str(exc))
        return None


def find_probes(lines, entrypoint=None, scenario=None, name=None):
    hits = []
    for line in lines:
        if entrypoint is not None and line.get("entrypoint") != entrypoint:
            continue
        if scenario is not None and line.get("scenario") != scenario:
            continue
        if name is not None and line.get("name") != name:
            continue
        hits.append(line)
    return hits


def require_probe(gaps, section_data, section, entrypoint, scenario,
                  names, role):
    """Require every named probe line to exist and report pass=true.

    A line is matched by (entrypoint, name) when the probe author grouped it
    under a differently named scenario but the name is unique inside that
    entrypoint; the assertion detail then records both the request scenario
    and the probe's own scenario label.
    """
    if section_data is None:
        gaps.add("%s: section %s unavailable for %s"
                 % (role, section, scenario))
        return []
    lines, _summary, path = section_data
    accepted = []
    for name in names:
        hits = find_probes(lines, entrypoint=entrypoint, scenario=scenario,
                           name=name)
        if not hits:
            alternative = find_probes(lines, entrypoint=entrypoint, name=name)
            if len(alternative) == 1:
                hits = alternative
        if not hits:
            gaps.add("%s: missing probe name=%s entrypoint=%s scenario=%s in %s"
                     % (role, name, entrypoint, scenario, path))
            continue
        line = hits[0]
        if line.get("pass") is not True:
            gaps.add("%s: probe name=%s line %s:%d pass=%r detail=%r"
                     % (role, name, path, line["_line"], line.get("pass"),
                        line.get("detail", "")))
            continue
        probe_scenario = line.get("scenario")
        accepted.append({
            "id": "probe_%s_passed" % name,
            "passed": True,
            "detail": "%s:%d name=%s entrypoint=%s probe_scenario=%s "
                      "leaf_scenario=%s ac=%s pass=true"
                      % (os.path.basename(path), line["_line"], name,
                         line.get("entrypoint"), probe_scenario, scenario,
                         line.get("ac")),
        })
    return accepted


def require_probe_any(gaps, section_data, section, entrypoint, scenario, role):
    """Require at least one passing probe line for entrypoint+scenario.

    Used where the probe names are not contract-visible; the line is still
    matched strictly on the entrypoint and scenario fields.  Returns
    (accepted_assertions, probe_names).
    """
    if section_data is None:
        gaps.add("%s: section %s unavailable for %s"
                 % (role, section, scenario))
        return [], []
    lines, _summary, path = section_data
    hits = find_probes(lines, entrypoint=entrypoint, scenario=scenario)
    if not hits:
        gaps.add("%s: no probe line for entrypoint=%s scenario=%s in %s"
                 % (role, entrypoint, scenario, path))
        return [], []
    accepted = []
    names = []
    for line in hits:
        if line.get("pass") is not True:
            gaps.add("%s: probe name=%s line %s:%d pass=%r detail=%r"
                     % (role, line.get("name"), path, line["_line"],
                        line.get("pass"), line.get("detail", "")))
            continue
        accepted.append({
            "id": "probe_%s_passed" % line.get("name"),
            "passed": True,
            "detail": "%s:%d name=%s entrypoint=%s scenario=%s pass=true"
                      % (os.path.basename(path), line["_line"],
                         line.get("name"), entrypoint, scenario),
        })
        names.append(line.get("name"))
    return accepted, names


def section_assertion(section, summary, path):
    return {
        "id": "probe_section_%s_complete" % section,
        "passed": True,
        "detail": "%s phase=%s ok=%s bad=%s"
                  % (os.path.basename(path), section, summary.get("ok"),
                     summary.get("bad")),
    }


def eunit_result(path, label):
    """Parse an eunit log; return total passed count and assertion detail."""
    text = ANSI_RE.sub("", read_text(path, label))
    counts = [int(value) for value in
              re.findall(r"(?:All\s+)?(\d+)\s+tests?\s+passed\b", text)]
    if not counts:
        gap("%s has no 'N tests passed' summary line: %s" % (label, path))
    total = max(counts)
    failures = re.search(r"Failed:\s*[1-9]\d*", text)
    if failures or re.search(r"\*\*?failed\*\*?", text, re.I):
        gap("%s reports failing tests: %s" % (label, path))
    marker = re.search(r"(?:All\s+)?(\d+)\s+tests?\s+passed\b", text)
    return {
        "total": total,
        "detail": "%s: %r" % (os.path.basename(path),
                              marker.group(0).strip() if marker else total),
    }


def assert_eunit_total(path, label, expected, gaps):
    try:
        result = eunit_result(path, label)
    except AdapterGap as exc:
        gaps.add(str(exc))
        return None
    if result["total"] != expected:
        gaps.add("%s reports %d passed tests but the exercised suite defines %d"
                 % (path, result["total"], expected))
        return None
    return result


def source_line(text, needle):
    index = text.find(needle)
    if index < 0:
        return None
    return text.count("\n", 0, index) + 1


def confirm_suite_names(repo_root):
    """Extract the per-domain confirm test funs from the suite source."""
    path = os.path.join(repo_root, "test", "logic",
                        "attachment_lifecycle_v5_confirm_pg_tests.erl")
    text = read_text(path, "W2-02 confirm suite source")
    parts = text.split("enterprise_test_()")
    if len(parts) != 2:
        gap("cannot split ordinary/enterprise generators in %s" % path)
    ordinary = re.findall(r"\?_test\((t_[a-z0-9_]+)\(\)\)", parts[0])
    enterprise = re.findall(r"\?_test\((ent_[a-z0-9_]+)\(\)\)", parts[1])
    if not ordinary or not enterprise:
        gap("no named confirm tests parsed from %s (ordinary=%d enterprise=%d)"
            % (path, len(ordinary), len(enterprise)))
    return {
        "path": path,
        "text": text,
        "ordinary": ordinary,
        "enterprise": enterprise,
        "total": len(ordinary) + len(enterprise),
    }


def leaf_identity(leaf):
    return tuple(str(leaf.get(field)) for field in LEAF_FIELDS)


def assert_leaf_set(expected_leaves, rows):
    """Emit exactly the contract leaf identity set, no duplicates or extras."""
    expected = [leaf_identity(leaf) for leaf in expected_leaves]
    actual = [leaf_identity(row.get("leaf", {})) for row in rows]
    if len(set(expected)) != len(expected):
        gap("contract leaf matrix contains duplicate identities")
    if len(set(actual)) != len(actual):
        gap("adapter emitted duplicate leaf identities")
    if set(expected) != set(actual):
        missing = sorted(set(expected) - set(actual))
        extra = sorted(set(actual) - set(expected))
        gap("emitted leaf set differs from the contract: missing=%r extra=%r"
            % (missing[:3], extra[:3]))


def eb_asset_evidence_facts(repo_root):
    """Parse the EB asset suite + its fixture to state what it really covers."""
    suite = os.path.join(repo_root, "test", "features", "enterprise_business",
                         "application", "asset", "eb_asset_tests.erl")
    text = read_text(suite, "EB asset suite source")
    start = text.find("cases({ok, _Conn}) ->")
    if start < 0:
        gap("no cases({ok, _Conn}) block in %s" % suite)
    end = text.find("];", start)
    block = text[start:end if end > start else len(text)]
    names = re.findall(r"fun ([a-z0-9_]+)/0", block)
    if not names:
        gap("no EB asset test funs parsed from %s" % suite)
    fixture = os.path.join(repo_root, "test", "features",
                           "enterprise_business", "infrastructure",
                           "eb_pg_test_fixture.erl")
    fixture_text = read_text(fixture, "EB asset fixture source")
    fixture_line = source_line(fixture_text, "install_asset_registry() ->")
    if fixture_line is None or "file:make_dir(Root)" not in fixture_text:
        gap("%s no longer installs a disk test root; EB storage claims must be "
            "re-inspected" % fixture)
    return {
        "suite": suite,
        "funs": names,
        "fixture": fixture,
        "fixture_line": fixture_line,
    }


def build_row(leaf, started, finished, counts, oracle, assertions, actor,
              resource_ids, request_ids, provenance=None):
    """Assemble one adapter row and validate the producer's hard fields."""
    ident = "|".join(str(leaf.get(field)) for field in LEAF_FIELDS)
    if any(not isinstance(value, int) or value < 0
           for value in counts.values()):
        gap("non-integer execution counts for leaf %s: %r" % (ident, counts))
    if counts["exit_code"] != 0 or counts["failed"] or counts["skipped"]:
        gap("non-zero failure/skip counts for leaf %s: %r" % (ident, counts))
    if counts["executed"] < 1 or counts["executed"] != (
            counts["passed"] + counts["failed"] + counts["skipped"]):
        gap("executed != passed+failed+skipped for leaf %s: %r"
            % (ident, counts))
    for field in ORACLE_FIELDS:
        if not oracle.get(field):
            gap("oracle_observations.%s is empty for leaf %s"
                % (field, ident))
    if not assertions or any(item.get("passed") is not True for item in assertions):
        gap("all assertions must be measured passes for leaf %s" % ident)
    if not isinstance(actor, dict) or not actor.get("resource_id"):
        gap("actor provenance incomplete for leaf %s" % ident)
    if not resource_ids or actor.get("resource_id") not in resource_ids:
        gap("resource_ids must include the actor resource for leaf %s" % ident)
    if not request_ids:
        gap("request_ids empty for leaf %s" % ident)
    row = {
        "leaf": leaf,
        "started_at": iso(started),
        "finished_at": iso(finished),
        "exit_code": counts["exit_code"],
        "executed": counts["executed"],
        "passed": counts["passed"],
        "failed": counts["failed"],
        "skipped": counts["skipped"],
        "oracle_observations": oracle,
        "assertions": assertions,
        "actor": actor,
        "resource_ids": resource_ids,
        "request_ids": request_ids,
    }
    if provenance is not None:
        row["entrypoint_provenance"] = provenance
    return row


def _provenance_from_object(obj, entrypoint, scenario):
    if not isinstance(obj, dict):
        return None
    candidates = [obj]
    for key in ("entrypoint_provenance", "provenance"):
        nested = obj.get(key)
        if isinstance(nested, dict):
            candidates.insert(0, nested)
    for candidate in candidates:
        route = candidate.get("route")
        method = candidate.get("method")
        handler = candidate.get("handler")
        request_id = candidate.get("request_id")
        proof_entry = candidate.get("entrypoint")
        proof_scenario = candidate.get("scenario")
        if proof_entry is not None and not _entrypoint_matches(proof_entry,
                                                              entrypoint):
            continue
        if proof_scenario is not None and proof_scenario != scenario:
            continue
        if not (isinstance(route, str) and route.strip()
                and isinstance(method, str) and method.strip()
                and isinstance(handler, str) and handler.strip()
                and isinstance(request_id, str) and request_id.strip()):
            continue
        return {
            "entrypoint": entrypoint,
            "route": route.strip(),
            "method": method.strip(),
            "handler": handler.strip(),
            "request_id": request_id.strip(),
        }
    return None


def find_entrypoint_provenance(case_dir, entrypoint, scenario, gaps, role):
    """Locate route/method/handler/request_id in real case artifacts.

    Sources, in order: any CASE_DIR *.jsonl line carrying the four fields
    (top-level or nested), a CASE_DIR/entrypoint_provenance.jsonl sidecar,
    and an explicit ENTRYPOINT_PROVENANCE <json> marker in CASE_DIR logs.
    Nothing is synthesized when all sources are absent.
    """
    checked = []
    if os.path.isdir(case_dir):
        for name in sorted(os.listdir(case_dir)):
            if not name.endswith(".jsonl"):
                continue
            path = os.path.join(case_dir, name)
            checked.append(name)
            text = read_text(path, "case jsonl artifact")
            for number, raw in enumerate(text.splitlines(), 1):
                if not raw.strip():
                    continue
                try:
                    obj = json.loads(raw)
                except ValueError:
                    continue
                proof = _provenance_from_object(obj, entrypoint, scenario)
                if proof:
                    proof["artifact"] = "%s:%d" % (name, number)
                    return proof
        sidecar = os.path.join(case_dir, "entrypoint_provenance.jsonl")
        if os.path.isfile(sidecar):
            checked.append("entrypoint_provenance.jsonl")
            for number, raw in enumerate(
                    read_text(sidecar, "entrypoint provenance sidecar").splitlines(), 1):
                if not raw.strip():
                    continue
                try:
                    obj = json.loads(raw)
                except ValueError:
                    gap("unparsable entrypoint provenance line %s:%d"
                        % (sidecar, number))
                proof = _provenance_from_object(obj, entrypoint, scenario)
                if proof:
                    proof["artifact"] = "entrypoint_provenance.jsonl:%d" % number
                    return proof
        for name in sorted(os.listdir(case_dir)):
            if not (name.endswith(".log") or name == "raw.log"):
                continue
            path = os.path.join(case_dir, name)
            checked.append(name)
            for number, raw in enumerate(
                    read_text(path, "case log artifact").splitlines(), 1):
                match = PROVENANCE_MARK_RE.search(ANSI_RE.sub("", raw))
                if not match:
                    continue
                try:
                    obj = json.loads(match.group(1))
                except ValueError:
                    continue
                proof = _provenance_from_object(obj, entrypoint, scenario)
                if proof:
                    proof["artifact"] = "%s:%d" % (name, number)
                    return proof
    gaps.add("%s: entrypoint_provenance route/method/handler/request_id not "
             "present for entrypoint=%s scenario=%s (checked %s)"
             % (role, entrypoint, scenario,
                ", ".join(checked) if checked else "no CASE_DIR artifacts"))
    return None


def emit(case_dir, rows, adapter_name):
    out = os.path.join(case_dir, "observations.json")
    temporary = out + ".tmp"
    with open(temporary, "w", encoding="utf-8") as handle:
        json.dump({"leaves": rows}, handle, ensure_ascii=False, indent=1)
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, out)
    print(json.dumps({"adapter": adapter_name, "leaves": len(rows),
                      "output": out}))


def run_adapter(adapter_name, builder):
    """Execute builder(); convert AdapterGap into exit code 30."""
    try:
        case_dir, rows = builder()
        emit(case_dir, rows, adapter_name)
    except AdapterGap as exc:
        sys.stderr.write("%s adapter: %s\n" % (adapter_name, exc))
        raise SystemExit(30)


def argv_paths():
    if len(sys.argv) != 4:
        sys.stderr.write("usage: %s <RUN_DIR> <CASE_DIR> <REPO_ROOT>\n"
                         % os.path.basename(sys.argv[0]))
        raise SystemExit(2)
    return (os.path.abspath(sys.argv[1]),
            os.path.abspath(sys.argv[2]),
            os.path.abspath(sys.argv[3]))
