#!/usr/bin/env python3
"""Strict measured-observation helpers for the W1 attachment lifecycle adapters.

Every W1-02 leaf is bound to probe rows actually written by
attachment_lifecycle_v5_w102_probe.sh.  Rows are parsed, never synthesised:
missing rows, failing rows, phase-block mismatches, unknown probe names,
missing run resources or a probe that does not exercise the leaf's defining
action all abort with exit 30 and name the exact gap.  No adapter emits a
PASS receipt for evidence the run did not produce.
"""
import datetime
import hashlib
import json
import os
import re
import sys

EXIT_GAP = 30
PROBE_SCRIPT_REL = "scripts/test/attachment_lifecycle_v5_w102_probe.sh"
PHASE_FOR_SCHEMA = {"expand-queue": "expand", "guards-contract": "guards"}


class Gap(Exception):
    """A real evidence gap; adapters must exit 30 instead of emitting PASS."""


def utc_now():
    return datetime.datetime.now(datetime.timezone.utc)


def iso(ts):
    return ts.strftime("%Y-%m-%dT%H:%M:%SZ")


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 16), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path, label):
    try:
        with open(path, encoding="utf-8") as stream:
            return json.load(stream)
    except OSError as exc:
        raise Gap("%s unreadable: %s" % (label, exc))
    except ValueError as exc:
        raise Gap("%s is not valid JSON: %s" % (label, exc))


def _valid_digest(value):
    return (isinstance(value, str) and len(value) == 64
            and all(char in "0123456789abcdef" for char in value))


def require_digest(value, label):
    if not _valid_digest(value):
        raise Gap("%s missing/invalid (want 64 lowercase hex)" % label)
    return value


def fail_gaps(case_id, gaps):
    sys.stderr.write("%s: %d evidence gap(s); refusing to emit observations\n"
                     % (case_id, len(gaps)))
    for item in gaps:
        sys.stderr.write("  GAP %s\n" % item)
    sys.exit(EXIT_GAP)


def load_context(run_dir, repo_root):
    """Load state/contract/candidate/allowlist; any absence is a gap."""
    del repo_root  # kept for a uniform adapter signature
    state = read_json(os.path.join(run_dir, "state.json"), "state.json")
    plan_dir = state.get("plan_dir")
    if not isinstance(plan_dir, str) or not os.path.isabs(plan_dir):
        raise Gap("state.json plan_dir missing or not absolute")
    contract = read_json(os.path.join(plan_dir, "execution-contract.json"),
                         "execution-contract.json")
    candidate = state.get("verification_candidate") or {}
    heads = candidate.get("heads")
    if not isinstance(heads, dict) or not heads:
        raise Gap("state.json verification_candidate.heads missing "
                  "(freeze-candidate not completed)")
    for field in ("owned_tree_digest", "plan_manifest_sha256", "migration_digest"):
        require_digest(candidate.get(field), "verification_candidate." + field)
    allowlist = state.get("resource_allowlist")
    if not isinstance(allowlist, dict) or not allowlist:
        raise Gap("state.json resource_allowlist missing "
                  "(admission/freeze-candidate not completed)")
    return state, contract, candidate, allowlist


def parse_probe_blocks(path):
    """Split probes.jsonl on its phase-summary lines and validate the totals."""
    if not os.path.isfile(path):
        raise Gap("probes.jsonl missing: %s" % path)
    with open(path, encoding="utf-8", errors="replace") as stream:
        text = stream.read()
    if not text.strip():
        raise Gap("probes.jsonl is empty")
    blocks, current = [], []
    for lineno, line in enumerate(text.splitlines(), 1):
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
        except ValueError:
            raise Gap("probes.jsonl line %d is not JSON: %r" % (lineno, line[:120]))
        if not isinstance(obj, dict):
            raise Gap("probes.jsonl line %d is not an object" % lineno)
        if "phase" in obj:
            if "name" in obj or obj["phase"] not in ("expand", "guards"):
                raise Gap("probes.jsonl line %d has an invalid phase summary: %r"
                          % (lineno, obj))
            blocks.append({"phase": obj["phase"], "summary": obj, "rows": current})
            current = []
            continue
        if not isinstance(obj.get("name"), str) or type(obj.get("pass")) is not bool:
            raise Gap("probes.jsonl line %d lacks string name / boolean pass: %r"
                      % (lineno, obj))
        current.append({"name": obj["name"], "pass": obj["pass"],
                        "detail": obj.get("detail"), "line": lineno,
                        "seq": len(current) + 1})
    if current:
        raise Gap("probes.jsonl has %d row(s) after the last phase summary"
                  % len(current))
    if not blocks:
        raise Gap("probes.jsonl has no phase summary line")
    phases = [block["phase"] for block in blocks]
    if len(set(phases)) != len(phases):
        raise Gap("probes.jsonl repeats a phase block: %r" % phases)
    for block in blocks:
        summary, rows = block["summary"], block["rows"]
        ok, bad = summary.get("ok"), summary.get("bad")
        if type(ok) is not int or type(bad) is not int:
            raise Gap("probes.jsonl %s summary lacks integer ok/bad: %r"
                      % (block["phase"], summary))
        if ok + bad != len(rows):
            raise Gap("probes.jsonl %s summary ok+bad=%d but %d rows exist"
                      % (block["phase"], ok + bad, len(rows)))
        observed_bad = sum(1 for row in rows if not row["pass"])
        if bad != observed_bad:
            raise Gap("probes.jsonl %s summary bad=%d but %d rows failed"
                      % (block["phase"], bad, observed_bad))
        if bad != 0:
            failed = [row["name"] for row in rows if not row["pass"]]
            raise Gap("probes.jsonl %s block has failing probes: %r"
                      % (block["phase"], failed))
        if ok < 1:
            raise Gap("probes.jsonl %s block has no passing probe" % block["phase"])
    return blocks


def require_phase_order(blocks, two_phase):
    phases = [block["phase"] for block in blocks]
    wanted = ["expand", "guards"] if two_phase else ["guards"]
    if phases != wanted:
        raise Gap("probes.jsonl phase blocks=%r but this case requires %r"
                  % (phases, wanted))
    return {block["phase"]: block for block in blocks}


def raw_log_facts(case_dir, blocks):
    """Validate raw.log replay markers and absence of harness failure markers.

    raw.log is append-only across case retries (w102_phase_run uses >>), so the
    newest attempt is the tail after the last successful up-chain replay
    marker.  Stale failure markers before that tail belong to superseded
    attempts and are not evidence about this observation.
    """
    path = os.path.join(case_dir, "raw.log")
    if not os.path.isfile(path):
        raise Gap("raw.log missing: %s" % path)
    with open(path, encoding="utf-8", errors="replace") as stream:
        raw = stream.read()
    if not raw.strip():
        raise Gap("raw.log is empty")
    matches = list(re.finditer(r"replayed (\d+) up files", raw))
    if not matches:
        raise Gap("raw.log has no 'replayed N up files' marker for %d phase "
                  "block(s) %r" % (len(blocks),
                                   [block["phase"] for block in blocks]))
    tail = raw[matches[-1].start():]
    for marker in ("REPLAY_FAIL", "GUARDS_REPLAY_FAIL", "GUARDS_FAIL"):
        if marker in tail:
            raise Gap("raw.log contains harness failure marker %s after the "
                      "last successful replay" % marker)
    return {
        "path": path,
        "replay_last": int(matches[-1].group(1)),
        "replay_markers": len(matches),
        "bytes": len(raw),
    }


def load_probe_definitions(repo_root):
    """Parse frozen probe names/expected tokens; dynamic rows are recognised."""
    path = os.path.join(repo_root, PROBE_SCRIPT_REL)
    if not os.path.isfile(path):
        raise Gap("frozen probe script missing: %s" % path)
    with open(path, encoding="utf-8", errors="replace") as stream:
        text = stream.read()
    defs = {}
    pattern = re.compile(r'(?<![\w])probe\s+([A-Za-z0-9_]+)\s+'
                         r'("[^"]*"|\'[^\']*\'|\S+)\s')
    for match in pattern.finditer(text):
        defs.setdefault(match.group(1), set()).add(match.group(2).strip("\"'"))
    # emit()-based leaves have no static expected token: the probe computes
    # pass/fail itself and emits the row (probe_expected returns None for
    # them, like literal echo rows).
    emit_names = frozenset(re.findall(r'(?<![\w])emit\s+([A-Za-z0-9_]+)\s', text))
    return text, defs, emit_names


def probe_expected(probe_text, defs, name, emit_names=frozenset()):
    """Return the frozen expected token, or None for a dynamic echo probe."""
    if name in defs:
        values = sorted(defs[name])
        if len(values) != 1:
            raise Gap("probe %s has conflicting expected tokens %r" % (name, values))
        return values[0]
    if ('"name":"%s"' % name) in probe_text or name in emit_names:
        return None
    raise Gap("probe %s is not defined in %s" % (name, PROBE_SCRIPT_REL))


def select_row(block, name, occurrence):
    rows = [row for row in block["rows"] if row["name"] == name]
    if not rows:
        raise Gap("phase %s block has no probe %s" % (block["phase"], name))
    if occurrence >= len(rows):
        raise Gap("probe %s occurrence %d requested but only %d row(s) exist"
                  % (name, occurrence, len(rows)))
    row = rows[occurrence]
    if not row["pass"]:
        raise Gap("probe %s occurrence %d (seq %d) failed: %s"
                  % (name, occurrence, row["seq"], row["detail"] or "no detail"))
    return row


def resolve_db(slot, phase, allowlist):
    if not isinstance(slot, str) or not re.fullmatch(r"[a-z][a-z0-9_]{0,31}", slot):
        raise Gap("V5_SLOT not exported by gate ensure_pg (got %r)" % (slot,))
    db = "attv5_%s_w102_%s" % (slot, phase)
    allowed = allowlist.get("scratch_postgres")
    if not isinstance(allowed, list) or db not in allowed:
        raise Gap("scratch database %s is not in "
                  "state.resource_allowlist.scratch_postgres" % db)
    return db


def actor_kind(contract, case_id, leaf):
    registry = (contract.get("case_registry") or {}).get(case_id) or {}
    if registry.get("source_kind") == "real-required" and leaf.get("domain") != "plan":
        return "real"
    return "targeted-contract"


def _assertion_rows(block, spec, probe_text, defs, emit_names=frozenset()):
    rows = []
    for name, occurrence in spec["probes"]:
        row = select_row(block, name, occurrence)
        rows.append((name, occurrence, row,
                     probe_expected(probe_text, defs, name, emit_names)))
    if not rows:
        raise Gap("leaf mapping contains no probe row")
    return rows


def _build_leaf(case_id, leaf, spec, blocks, raw_facts,
                probe_text, defs, emit_names, slot, allowlist, candidate, contract,
                started, finished):
    phase = PHASE_FOR_SCHEMA[leaf["schema_phase"]]
    block = blocks[phase]
    db = resolve_db(slot, phase, allowlist)
    rows = _assertion_rows(block, spec, probe_text, defs, emit_names)

    assertions, deltas = [], []
    for name, occurrence, row, expected in rows:
        detail = ("probes.jsonl %s block seq=%d name=%s occurrence=%d pass=true"
                  % (phase, row["seq"], name, occurrence))
        if expected is None:
            detail += "; dynamic harness probe (no static expected token)"
        else:
            detail += "; frozen probe expected=%s" % expected
        assertions.append({"id": "probe_%s_occ%d_seq%d"
                                 % (name, occurrence, row["seq"]),
                           "passed": True, "detail": detail})
        deltas.append("%s expected=%s observed=pass"
                      % (name, expected if expected is not None else "dynamic"))

    summary_json = json.dumps(block["summary"], sort_keys=True,
                              separators=(",", ":"))
    kind = actor_kind(contract, case_id, leaf)
    probes_digest = sha256_file(os.path.join(
        os.path.dirname(raw_facts["path"]), "probes.jsonl"))
    observation = {
        "leaf": leaf,
        "started_at": iso(started),
        "finished_at": iso(finished),
        "exit_code": 0,
        "executed": len(assertions),
        "passed": len(assertions),
        "failed": 0,
        "skipped": 0,
        "oracle_observations": {
            "pre_db_state": ("db=%s phase=%s; raw.log last up-chain replay "
                             "(%d up files, %d marker(s) total); probe block "
                             "summary=%s"
                             % (db, phase, raw_facts["replay_last"],
                                raw_facts["replay_markers"], summary_json)),
            "pre_object_hash": ("probes.jsonl sha256=%s; candidate "
                                "migration_digest=%s"
                                % (probes_digest, candidate["migration_digest"])),
            "action": ("w102_phase_run %s + attachment_lifecycle_v5_w102_probe.sh; "
                       "probes: %s"
                       % (phase, ", ".join("%s@%d" % (name, row["seq"])
                                           for name, _o, row, _e in rows))),
            "expected_http_semantics": ("not exercised: SQL-only leaf; "
                                        "w102_phase_run issues no HTTP request"),
            "expected_db_delta": "; ".join(deltas),
            "expected_object_state": ("not exercised: no storage object; "
                                      "scratch PostgreSQL %s only" % db),
            "proof_actor": "%s:%s:%s" % (kind, slot, db),
        },
        "assertions": assertions,
        "actor": {"kind": kind, "driver": leaf.get("driver"),
                  "resource_kind": "scratch_postgres", "resource_id": db},
        "resource_ids": [db],
        "request_ids": ["%s:%s:%s@%d" % (case_id, db, name, row["seq"])
                        for name, _occurrence, row, _expected in rows],
    }
    return observation


def _describe_gap_spec(block, spec, probe_text, defs, emit_names=frozenset()):
    """Confirm a gap leaf's named probes exist, then report their insufficiency."""
    notes = []
    for name, occurrence in spec["probes"]:
        try:
            probe_expected(probe_text, defs, name, emit_names)
        except Gap as exc:
            notes.append(str(exc))
            continue
        rows = [row for row in block["rows"] if row["name"] == name]
        if len(rows) <= occurrence:
            notes.append("probe %s occurrence %d missing from %s block"
                         % (name, occurrence, block["phase"]))
        else:
            notes.append("probe %s occurrence %d row present/pass=%s but "
                         "insufficient" % (name, occurrence,
                                           rows[occurrence]["pass"]))
    return "%s [rows: %s]" % (spec["gap"], "; ".join(notes))


def run_w102_case(case_id, mapping, two_phase):
    started = utc_now() - datetime.timedelta(seconds=1)
    run_dir = os.path.abspath(sys.argv[1])
    case_dir = os.path.abspath(sys.argv[2])
    repo_root = os.path.abspath(sys.argv[3])
    gaps, observations = [], []
    try:
        _state, contract, candidate, allowlist = load_context(run_dir, repo_root)
        expected = contract["expected_leaf_matrix"].get(case_id)
        if not expected:
            raise Gap("case %s not present in expected_leaf_matrix" % case_id)
        expected_by_key = {}
        for leaf in expected:
            key = (leaf.get("domain"), leaf.get("ordering_or_fault"),
                   leaf.get("schema_phase"))
            if key in expected_by_key:
                raise Gap("contract repeats leaf identity %r" % (key,))
            expected_by_key[key] = leaf
        missing = sorted(set(expected_by_key) - set(mapping))
        extra = sorted(set(mapping) - set(expected_by_key))
        if missing or extra:
            raise Gap("adapter mapping does not match contract; missing=%r extra=%r"
                      % (missing, extra))
        block_list = parse_probe_blocks(os.path.join(case_dir, "probes.jsonl"))
        blocks = require_phase_order(block_list, two_phase)
        raw_facts = raw_log_facts(case_dir, block_list)
        probe_text, defs, emit_names = load_probe_definitions(repo_root)
        slot = os.environ.get("V5_SLOT")
        for leaf in expected:
            resolve_db(slot, PHASE_FOR_SCHEMA[leaf["schema_phase"]], allowlist)
        for leaf in expected:
            key = (leaf.get("domain"), leaf.get("ordering_or_fault"),
                   leaf.get("schema_phase"))
            spec = mapping[key]
            phase = PHASE_FOR_SCHEMA[leaf["schema_phase"]]
            if spec.get("gap"):
                gaps.append("%s/%s/%s [%s]: %s"
                            % (leaf["domain"], leaf["entrypoint"],
                               leaf["ordering_or_fault"], leaf["schema_phase"],
                               _describe_gap_spec(blocks[phase], spec,
                                                  probe_text, defs, emit_names)))
                continue
            observations.append(_build_leaf(
                case_id, leaf, spec, blocks, raw_facts,
                probe_text, defs, emit_names, slot, allowlist, candidate, contract,
                started, started))
    except Gap as exc:
        gaps.append(str(exc))
    if gaps:
        fail_gaps(case_id, gaps)

    finished = utc_now()
    for observation in observations:
        observation["finished_at"] = iso(finished)
        start = datetime.datetime.fromisoformat(
            observation["started_at"].replace("Z", "+00:00"))
        end = datetime.datetime.fromisoformat(
            observation["finished_at"].replace("Z", "+00:00"))
        if not 0 <= (end - start).total_seconds() <= 1800:
            fail_gaps(case_id, ["observation window exceeds 1800s"])
    out = os.path.join(case_dir, "observations.json")
    with open(out, "w", encoding="utf-8") as stream:
        json.dump({"leaves": observations}, stream, ensure_ascii=False, indent=1)
    print(json.dumps({"case_id": case_id, "leaves": len(observations),
                      "output": out}))
