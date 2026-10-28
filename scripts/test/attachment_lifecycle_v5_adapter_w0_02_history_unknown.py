#!/usr/bin/env python3
"""W0-02/history-unknown adapter: measured baseline probes + static inventory.

The contract (/bootstrap.w0_02_schema) defines this case as a "static
inventory/protection decision" for three history classes:
unknown_location, opaque_reference, ambiguous_reference. The gate executor
(bootstrap_w002_history) produces only raw.log from evidence/baseline-probes.sql,
so this adapter demands BOTH:
  1. the measured probe set + rollback counter in <case>/raw.log, and
  2. a per-class inventory/protection classification inside CASE_DIR that maps
     each class to its enforcement (W3-01/history-unknown and W6).
If either half is absent the adapter exits 30 with the exact gap instead of
emitting a plausible-looking PASS.

Read-only live checks against the run-owned database use the same frozen probe
premises (legacy attach_pending.bucket default, attachment path uniqueness,
absence of gc_verified in the base phase).
"""
import datetime
import hashlib
import json
import os
import re
import subprocess
import sys

CASE_ID = "W0-02/history-unknown"
RUN_DIR = os.path.abspath(sys.argv[1])
CASE_DIR = os.path.abspath(sys.argv[2])
ROOT = os.path.abspath(sys.argv[3])

STARTED = datetime.datetime.now(datetime.timezone.utc)

PROBE_SET = (
    "OBSERVED_CONFIRM_STYLE_UPSERT_COUNT_2",
    "OBSERVED_PENDING_IDENTITY_MUTABLE",
    "OBSERVED_ATTACHMENT_IDENTITY_NO_DB_GUARD",
)
# Documented probe-to-class mapping from evidence/w0-02-input-summary.json
# (F04 confirm-style upsert counter = ambiguous reference accounting,
#  F02 pending identity mutable = unknown location row, F03 attachment identity
#  without DB guard = opaque reference). The probe set itself is shared by the
# case; class-specific proof additionally needs the inventory entry below.
CLASS_PROBES = {
    "unknown_location": "OBSERVED_PENDING_IDENTITY_MUTABLE",
    "opaque_reference": "OBSERVED_ATTACHMENT_IDENTITY_NO_DB_GUARD",
    "ambiguous_reference": "OBSERVED_CONFIRM_STYLE_UPSERT_COUNT_2",
}


def fail(gaps):
    if isinstance(gaps, str):
        gaps = [gaps]
    for gap in gaps:
        sys.stderr.write("w0_02_history_unknown adapter: %s\n" % gap.rstrip())
    sys.exit(30)


def load_json(path, what):
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError) as exc:
        fail("cannot read %s at %s: %s" % (what, path, exc))


def preflight():
    state = load_json(os.path.join(RUN_DIR, "state.json"), "run state")
    candidate = state.get("verification_candidate")
    if not isinstance(candidate, dict) or not candidate.get("heads"):
        fail("run is not frozen: state.json has no verification_candidate/heads")
    allow = state.get("resource_allowlist")
    if not isinstance(allow, dict) or not allow:
        fail("state.json has no resource_allowlist (admission snapshot incomplete)")
    return state, candidate, allow


def owned_container(allow):
    pg = load_json(os.path.join(RUN_DIR, "resources/postgres.json"),
                   "run-owned PostgreSQL receipt")
    container = pg.get("container")
    run_name = os.path.basename(RUN_DIR.rstrip("/"))
    if not isinstance(allow.get("postgres_container"), list) \
            or container not in allow["postgres_container"]:
        fail("owned container %r is not registered in resource_allowlist" % (container,))
    if pg.get("owner_run_id") != run_name:
        fail("postgres receipt owner_run_id %r != run %r"
             % (pg.get("owner_run_id"), run_name))
    fmt = ('{"Id":{{json .Id}},"Running":{{json .State.Running}},'
           '"Labels":{{json .Config.Labels}}}')
    try:
        out = subprocess.check_output(
            ["docker", "inspect", "--format", fmt, container],
            text=True, stderr=subprocess.PIPE, timeout=15)
    except (OSError, subprocess.SubprocessError) as exc:
        fail("cannot inspect owned container %r: %s" % (container, exc))
    info = json.loads(out)
    labels = info.get("Labels") or {}
    if info.get("Id") != pg.get("container_id") or info.get("Running") is not True:
        fail("live container identity/running state differs from receipt")
    if labels.get("codex.run") != run_name \
            or labels.get("codex.role") != "attachment-lifecycle-pg":
        fail("live container labels do not match the run-owned PostgreSQL role")
    return container


def pg_password():
    path = os.path.join(RUN_DIR, "resources/pg-attv5.password")
    try:
        with open(path, encoding="utf-8") as fh:
            password = fh.read().strip()
    except OSError as exc:
        fail("cannot read isolated PG password: %s" % exc)
    if not password:
        fail("isolated PG password file is empty")
    return password


def psql(container, database, password, sql):
    cmd = ["docker", "exec", "-i", "-e", "PGPASSWORD=" + password, container,
           "psql", "-X", "-U", "imboy_user", "-d", database, "-At",
           "-F", "\x1f", "-v", "ON_ERROR_STOP=1", "-c", sql]
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    except (OSError, subprocess.SubprocessError) as exc:
        fail("read-only psql invocation failed (db=%r): %s" % (database, exc))
    if proc.returncode != 0:
        fail("read-only psql failed (db=%r rc=%d): %s"
             % (database, proc.returncode, proc.stderr.strip()[:600]))
    return [line for line in proc.stdout.splitlines() if line != ""]


def resolve_scratch_db(container, password, allow):
    rows = psql(container, "postgres", password,
                "SELECT datname FROM pg_database "
                "WHERE datname LIKE 'attv5\\_%\\_raw2' ORDER BY oid")
    registered = allow.get("scratch_postgres") or []
    candidates = [db for db in rows if db in registered]
    if not candidates:
        fail("no run-owned database matching attv5_<slot>_raw2 is registered in "
             "resource_allowlist.scratch_postgres (live=%r)" % (rows,))
    if len(candidates) > 1:
        fail("ambiguous scratch database for W0-02/history-unknown: %r; run_case "
             "does not record the slot/DB identity in CASE_DIR" % (candidates,))
    return candidates[0]


def _walk(node):
    if isinstance(node, dict):
        yield node
        for value in node.values():
            for item in _walk(value):
                yield item
    elif isinstance(node, list):
        for value in node:
            for item in _walk(value):
                yield item


def _structured_entries(document):
    """Per-class JSON records: one class name value + protection + W3-01/W6."""
    found = {}
    for obj in _walk(document):
        values = [v for v in obj.values() if isinstance(v, str)]
        classes = [cls for cls in CLASS_PROBES if cls in values]
        if len(classes) != 1:
            continue
        joined = " ".join(values)
        if not re.search(r"W3-01|W6", joined):
            continue
        if not any(v.strip() for v in values if v not in CLASS_PROBES):
            continue
        found.setdefault(classes[0], obj)
    return found


def find_inventory():
    """Locate a per-class inventory/protection classification inside CASE_DIR.

    Only structured JSON records are accepted (a text mention is not a
    classification, and the adapter's own logs must never satisfy the check).
    """
    scanned = []
    for name in sorted(os.listdir(CASE_DIR)):
        if name in ("observations.json", "adapter.log") or not name.endswith(".json"):
            continue
        path = os.path.join(CASE_DIR, name)
        if not os.path.isfile(path) or os.path.getsize(path) > 8 * 1024 * 1024:
            continue
        scanned.append(name)
        try:
            with open(path, encoding="utf-8") as fh:
                document = json.load(fh)
        except (OSError, ValueError):
            continue
        entries = _structured_entries(document)
        if set(entries) == set(CLASS_PROBES):
            return path, entries, scanned
    return None, {}, scanned


def parse_probe_value(lines, marker):
    for idx, line in enumerate(lines):
        if marker not in line:
            continue
        for follow in lines[idx + 1:]:
            stripped = follow.strip()
            if not stripped or re.match(r"^[-+]+$", stripped):
                continue
            if stripped.startswith("("):
                break
            if re.match(r"^-?\d+$", stripped):
                return int(stripped)
    return None


def main():
    state, candidate, allow = preflight()
    container = owned_container(allow)
    password = pg_password()
    database = resolve_scratch_db(container, password, allow)

    contract = load_json(os.path.join(state["plan_dir"], "execution-contract.json"),
                         "frozen execution contract")
    leaves = contract.get("expected_leaf_matrix", {}).get(CASE_ID)
    if not leaves or len(leaves) != 3:
        fail("contract expected_leaf_matrix[%r] is not exactly 3 leaves"
             % CASE_ID)

    raw_path = os.path.join(CASE_DIR, "raw.log")
    if not os.path.isfile(raw_path):
        fail("CASE_DIR/raw.log is missing; bootstrap_w002_history did not produce "
             "the frozen baseline probe transcript")
    with open(raw_path, encoding="utf-8", errors="replace") as fh:
        raw = fh.read()
    lines = raw.splitlines()

    observed = sorted(set(re.findall(r"OBSERVED_[A-Z0-9_]+", raw)))
    remaining = parse_probe_value(lines, "remaining_synthetic_rows")
    rollback_seen = any(line.strip() == "ROLLBACK" for line in lines)
    error_tokens = re.findall(r"(?m)^(?:ERROR|FATAL|PANIC):|\bPROBES_MISSING\b|"
                              r"\bROLLBACK_PROBE_MISSING\b", raw)

    bucket_default = psql(
        container, database, password,
        "SELECT count(*) FROM information_schema.columns WHERE table_schema='public' "
        "AND table_name='attach_pending' AND column_name='bucket' "
        "AND column_default = '''''::text'")
    bucket_default_count = int(bucket_default[0]) if bucket_default else 0

    path_unique_rows = psql(
        container, database, password,
        "SELECT count(*) FROM pg_indexes WHERE schemaname='public' "
        "AND tablename='attachment' AND indexname='uk_attachment_path' "
        "AND indexdef ILIKE '%UNIQUE%' AND indexdef ILIKE '%(path)%'")
    path_unique_count = int(path_unique_rows[0]) if path_unique_rows else 0

    gc_columns = psql(
        container, database, password,
        "SELECT table_name || '.' || column_name FROM information_schema.columns "
        "WHERE table_schema='public' AND column_name='gc_verified' ORDER BY 1")

    attachment_guards = psql(
        container, database, password,
        "SELECT tgname FROM pg_trigger WHERE NOT tgisinternal "
        "AND tgrelid IN ('attachment'::regclass, 'attach_pending'::regclass) "
        "AND tgname IN ('trg_v5_attachment_identity_immutable', "
        "'trg_v5_pending_session_guard') ORDER BY 1")

    inventory_path, inventory_entries, scanned = find_inventory()

    gaps = []
    if set(observed) != set(PROBE_SET):
        gaps.append(
            "measured probe set mismatch: raw.log observed=%r expected=%r "
            "(error tokens=%r)"
            % (observed, sorted(PROBE_SET), error_tokens or "none"))
        if attachment_guards:
            gaps.append(
                "diagnostic: the pinned baseline-probes.sql is rejected by the "
                "current schema guards %r on %r before this adapter can run"
                % (attachment_guards, database))
    if remaining != 0:
        gaps.append("rollback counter remaining_synthetic_rows=%r (expected 0); "
                    "synthetic rows survived the probe transaction" % (remaining,))
    if not rollback_seen:
        gaps.append("raw.log has no ROLLBACK command tag; probe transaction "
                    "atomicity is not evidenced")
    if inventory_path is None:
        gaps.append(
            "per-leaf inventory/protection classification is absent from CASE_DIR "
            "(scanned json=%r); contract /bootstrap.w0_02_schema requires a static "
            "inventory that classifies unknown_location / opaque_reference / "
            "ambiguous_reference as protected and maps enforcement to "
            "W3-01/history-unknown and W6. raw.log holds only the 3 baseline "
            "probes + rollback counter, so per-leaf oracle evidence is missing. "
            "Accepted shape: a CASE_DIR *.json document with one record per class "
            "(the class name as a string value, a non-empty protection field, and "
            "a W3-01/W6 enforcement reference)"
            % (scanned,))
    if gc_columns:
        gaps.append(
            "oracle premise 'base schema has no gc_verified' does not hold for the "
            "measured database %r: gc_verified columns=%r (the replay includes "
            "00000178+ attachment_lifecycle_v5_expand, i.e. head, not base)"
            % (database, gc_columns))
    if gaps:
        fail(gaps)

    assertions_by_class = {
        "unknown_location": [
            {"id": "baseline_probe_pending_identity_mutable",
             "passed": "OBSERVED_PENDING_IDENTITY_MUTABLE" in observed,
             "detail": "raw.log OBSERVED set=%r" % (observed,)},
            {"id": "legacy_attach_pending_bucket_default_empty",
             "passed": bucket_default_count == 1,
             "detail": "attach_pending.bucket default '' rows=%d in db %r"
                       % (bucket_default_count, database)},
        ],
        "opaque_reference": [
            {"id": "baseline_probe_attachment_identity_no_db_guard",
             "passed": "OBSERVED_ATTACHMENT_IDENTITY_NO_DB_GUARD" in observed,
             "detail": "raw.log OBSERVED set=%r" % (observed,)},
        ],
        "ambiguous_reference": [
            {"id": "baseline_probe_confirm_style_upsert_counter",
             "passed": "OBSERVED_CONFIRM_STYLE_UPSERT_COUNT_2" in observed,
             "detail": "raw.log OBSERVED set=%r" % (observed,)},
            {"id": "attachment_path_unique_index_present",
             "passed": path_unique_count == 1,
             "detail": "uk_attachment_path UNIQUE(path) rows=%d in db %r"
                       % (path_unique_count, database)},
        ],
    }

    output_leaves = []
    for leaf in leaves:
        ordering = leaf.get("ordering_or_fault")
        assertions = list(assertions_by_class.get(ordering, []))
        assertions.append({
            "id": "probe_transaction_rolled_back_clean",
            "passed": remaining == 0 and rollback_seen and not error_tokens,
            "detail": "remaining_synthetic_rows=%d ROLLBACK=%s errors=%r"
                      % (remaining, rollback_seen, error_tokens or "none"),
        })
        assertions.append({
            "id": "inventory_classifies_%s_as_protected" % ordering,
            "passed": inventory_path is not None and ordering in inventory_entries,
            "detail": "inventory artifact=%r entry=%r"
                      % (inventory_path, inventory_entries.get(ordering)),
        })
        failed = [a for a in assertions if not a["passed"]]
        if failed:
            fail("leaf %r assertions failed: %r" % (ordering, failed))
        output_leaves.append({
            "leaf": leaf,
            "started_at": STARTED.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "finished_at": datetime.datetime.now(
                datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "exit_code": 0,
            "executed": len(assertions),
            "passed": len(assertions),
            "failed": 0,
            "skipped": 0,
            "oracle_observations": {
                "pre_db_state": "run-owned base database %r from frozen Up replay" % database,
                "pre_object_hash": "case raw.log sha256=%s"
                                   % hashlib.sha256(raw.encode()).hexdigest(),
                "action": "psql -v ON_ERROR_STOP=1 baseline-probes.sql "
                          "(BEGIN .. probes .. ROLLBACK) + static inventory read",
                "expected_http_semantics": "n/a (plan-domain leaf; no HTTP surface)",
                "expected_db_delta": "none: probe transaction rolled back, "
                                     "remaining_synthetic_rows=0",
                "expected_object_state": "history class %r classified protected; "
                                         "inventory=%r" % (ordering, inventory_path),
                "proof_actor": "main-integrator (run-owned scratch PostgreSQL)",
            },
            "assertions": assertions,
            "actor": {"kind": "real", "driver": leaf.get("driver"),
                      "resource_kind": "scratch_postgres",
                      "resource_id": database},
            "resource_ids": [database],
            "request_ids": ["w0-02-history-%s-%s"
                            % (ordering, hashlib.sha256(raw.encode()).hexdigest()[:12])],
        })

    output = os.path.join(CASE_DIR, "observations.json")
    with open(output, "w", encoding="utf-8") as fh:
        json.dump({"leaves": output_leaves}, fh, ensure_ascii=False, indent=1)
    print(json.dumps({"adapter": "w0_02_history_unknown",
                      "leaves": len(output_leaves), "output": output}))


if __name__ == "__main__":
    main()
