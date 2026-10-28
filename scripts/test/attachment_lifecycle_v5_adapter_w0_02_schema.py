#!/usr/bin/env python3
"""W0-02/schema adapter: measured replay + catalog leaf (never a fabricated PASS).

Evidence parsed from case artifacts / run-owned resources only:
  <case>/raw.log                 psql single-transaction Up replay transcript,
                                 then docs/.../evidence/catalog-query.sql output
  <run>/baseline-current.json    admission-time migration sha256 map (frozen Up set)
  <repo>/priv/migrations         live Up/Down file set
  <repo>/docs/.../evidence/catalog-query.sql  exact frozen catalog query text
  run-owned PostgreSQL container read-only catalog cross-check + resource identity

Failure policy: any missing artifact, structural mismatch or false assertion
exits 30 with the exact gap on stderr. No value is hardcoded.
"""
import datetime
import hashlib
import json
import os
import re
import subprocess
import sys

CASE_ID = "W0-02/schema"
RUN_DIR = os.path.abspath(sys.argv[1])
CASE_DIR = os.path.abspath(sys.argv[2])
ROOT = os.path.abspath(sys.argv[3])

STARTED = datetime.datetime.now(datetime.timezone.utc)


def fail(msg):
    sys.stderr.write("w0_02_schema adapter: %s\n" % msg.rstrip())
    sys.exit(30)


def load_json(path, what):
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError) as exc:
        fail("cannot read %s at %s: %s" % (what, path, exc))


def read_text(path, what):
    try:
        with open(path, encoding="utf-8", errors="replace") as fh:
            return fh.read()
    except OSError as exc:
        fail("cannot read %s at %s: %s" % (what, path, exc))


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def canonical(obj):
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def preflight():
    state = load_json(os.path.join(RUN_DIR, "state.json"), "run state")
    candidate = state.get("verification_candidate")
    if not isinstance(candidate, dict) or not candidate.get("heads"):
        fail("run is not frozen: state.json has no verification_candidate/heads "
             "(run freeze-candidate first); measured leaves cannot bind to a candidate")
    allow = state.get("resource_allowlist")
    if not isinstance(allow, dict) or not allow:
        fail("state.json has no resource_allowlist (admission snapshot incomplete); "
             "the producer would reject every actor provenance")
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
        fail("live container identity/running state differs from receipt for %r"
             % (container,))
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
        fail("cannot read isolated PG password %s: %s" % (path, exc))
    if not password:
        fail("isolated PG password file %s is empty" % path)
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
                "WHERE datname LIKE 'attv5\\_%\\_raw' ORDER BY oid")
    registered = allow.get("scratch_postgres") or []
    candidates = [db for db in rows if db in registered]
    if not candidates:
        fail("no run-owned database matching attv5_<slot>_raw is registered in "
             "resource_allowlist.scratch_postgres (live=%r)" % (rows,))
    if len(candidates) > 1:
        fail("ambiguous scratch database for W0-02/schema: %r; run_case does not "
             "record the slot/DB identity in CASE_DIR, refusing to guess"
             % (candidates,))
    return candidates[0]


def parse_catalog_blocks(catalog_lines):
    blocks, current = [], None
    for line in catalog_lines:
        footer = re.match(r"^\((\d+) rows?\)$", line.strip())
        if footer:
            if current is None:
                fail("catalog output has a result footer with no block header")
            blocks.append((int(footer.group(1)), current))
            current = None
        elif current is None:
            current = [line]
        else:
            current.append(line)
    if current is not None and any(line.strip() for line in current):
        fail("catalog output is truncated: %d trailing lines after the last "
             "result footer" % len(current))
    return blocks


def like_to_regex(pattern):
    out = "^"
    for ch in pattern:
        out += ".*" if ch == "%" else "." if ch == "_" else re.escape(ch)
    return re.compile(out + "$")


def block_data(lines):
    """Return data rows of one psql aligned block (skip blanks, header, rule)."""
    idx = 0
    while idx < len(lines) and not lines[idx].strip():
        idx += 1
    if idx + 1 >= len(lines):
        fail("catalog result block is missing its header/rule lines")
    if not re.match(r"^[-+_ ]+$", lines[idx + 1].strip() or "-"):
        fail("catalog result block has no aligned separator row: %r"
             % (lines[idx + 1][:120],))
    return lines[idx + 2:]


def main():
    state, candidate, allow = preflight()
    container = owned_container(allow)
    password = pg_password()
    database = resolve_scratch_db(container, password, allow)

    contract = load_json(os.path.join(state["plan_dir"], "execution-contract.json"),
                         "frozen execution contract")
    leaves = contract.get("expected_leaf_matrix", {}).get(CASE_ID)
    if not leaves or len(leaves) != 1:
        fail("contract expected_leaf_matrix[%r] is not exactly 1 leaf" % CASE_ID)

    raw_log = os.path.join(CASE_DIR, "raw.log")
    raw = read_text(raw_log, "case raw.log")
    lines = raw.splitlines()

    replayed_at, replayed_count = None, None
    for idx, line in enumerate(lines):
        match = re.match(r"^replayed=(\d+)$", line.strip())
        if match:
            replayed_at, replayed_count = idx, int(match.group(1))
    if replayed_at is None:
        fail("raw.log has no 'replayed=<N>' replay marker (gate bootstrap_w002_schema "
             "did not complete); artifact cannot show the frozen Up set replay")
    replay_text = "\n".join(lines[:replayed_at])
    catalog_text = "\n".join(lines[replayed_at + 1:])

    migration_dir = os.path.join(ROOT, "priv/migrations")
    live_ups = sorted(f for f in os.listdir(migration_dir) if f.endswith(".up.sql"))
    baseline_path = os.path.join(RUN_DIR, "baseline-current.json")
    baseline = load_json(baseline_path, "admission baseline snapshot")
    baseline_migrations = baseline.get("migration_files_sha256")
    if not isinstance(baseline_migrations, dict) or not baseline_migrations:
        fail("baseline-current.json has no migration_files_sha256 map; the frozen "
             "Up set cannot be reconstructed from artifacts")
    base_ups = sorted(f for f in baseline_migrations if f.endswith(".up.sql"))
    base_downs = sorted(f for f in baseline_migrations if f.endswith(".down.sql"))
    live_downs = sorted(f for f in os.listdir(migration_dir) if f.endswith(".down.sql"))

    hash_drift = []
    for name, digest in sorted(baseline_migrations.items()):
        path = os.path.join(migration_dir, name)
        if not os.path.exists(path) or sha256_file(path) != digest:
            hash_drift.append(name)
    set_drift = sorted(set(base_ups + base_downs)
                       ^ set(live_ups + live_downs))
    live_migration_map = {
        name: sha256_file(os.path.join(migration_dir, name))
        for name in sorted(live_ups + live_downs)}
    live_migration_digest = hashlib.sha256(
        canonical(live_migration_map).encode()).hexdigest()
    migration_digest = hashlib.sha256(
        canonical(baseline_migrations).encode()).hexdigest()

    catalog_sql = read_text(
        os.path.join(ROOT,
                     "docs/architecture/attachment-lifecycle-v5/evidence/catalog-query.sql"),
        "frozen catalog-query.sql")
    list_match = re.search(r"table_name\s+IN\s*\((.*?)\)", catalog_sql, re.S)
    if not list_match:
        fail("cannot extract the table_name IN list from the frozen catalog query")
    listed_tables = re.findall(r"'([^']+)'", list_match.group(1))
    like_match = re.search(r"table_name\s+LIKE\s+'([^']+)'", catalog_sql)
    like_rx = like_to_regex(like_match.group(1)) if like_match else None

    live_tables = set(psql(
        container, database, password,
        "SELECT table_name FROM information_schema.tables "
        "WHERE table_schema='public'"))
    expected_tables = {t for t in listed_tables if t in live_tables}
    if like_rx is not None:
        expected_tables |= {t for t in live_tables if like_rx.match(t)}

    blocks = parse_catalog_blocks(catalog_text.splitlines())
    artifacts_ok = len(blocks) == 7
    if not artifacts_ok:
        fail("catalog section has %d result blocks, expected 7 (version/extension/"
             "columns/constraints/indexes/triggers/functions); raw.log may be "
             "truncated" % len(blocks))

    def block_text(index):
        return "\n".join(blocks[index][1])

    version_rows = block_data(blocks[0][1])
    version_text = "\n".join(version_rows)
    version_match = re.search(r"PostgreSQL\s+(\d+)\.(\d+)", version_text)
    version_major = int(version_match.group(1)) if version_match else None
    version_minor = int(version_match.group(2)) if version_match else None

    artifact_tables = set()
    for line in block_data(blocks[2][1]):
        if "|" not in line:
            continue
        name = line.split("|", 1)[0].strip()
        if name and " " not in name:
            artifact_tables.add(name)

    constraint_data = block_data(blocks[3][1])
    constraint_rows = blocks[3][0]
    attachment_fk = sum(1 for line in constraint_data
                        if "REFERENCES attachment" in line)

    path_unique = [line for line in block_data(blocks[4][1])
                   if "uk_attachment_path" in line and "UNIQUE" in line
                   and "(path)" in line]

    attachment_triggers = [line for line in block_data(blocks[5][1])
                           if line.split("|", 1)[0].strip() == "attachment"]
    deleted_path_guard = any("trg_attachment_deleted_path_guard" in line
                             for line in attachment_triggers)

    function_rows = blocks[6][0]
    function_text = block_text(6)
    attach_functions = "CREATE OR REPLACE FUNCTION public.fn_attachment" in function_text

    error_tokens = re.findall(
        r"(?m)^(?:ERROR|FATAL|PANIC):|\bREPLAY_FAIL\b|\bCATALOG_EMPTY\b",
        replay_text + "\n" + catalog_text)

    assertions = [
        {"id": "replay_transcript_has_no_errors", "passed": not error_tokens,
         "detail": "error tokens in raw.log=%r" % (error_tokens or "none")},
        {"id": "replay_count_matches_frozen_up_set",
         "passed": replayed_count == len(base_ups) == len(live_ups),
         "detail": "replayed=%d admission_up_files=%d live_up_files=%d"
                   % (replayed_count, len(base_ups), len(live_ups))},
        {"id": "frozen_up_set_matches_admission_baseline",
         "passed": not hash_drift and not set_drift,
         "detail": "sha256 drift=%d set drift=%d over %d files; set digest=%s"
                   % (len(hash_drift), len(set_drift), len(baseline_migrations),
                      migration_digest)},
        {"id": "migration_set_matches_frozen_candidate_digest",
         "passed": candidate.get("migration_digest") == live_migration_digest,
         "detail": "candidate migration_digest=%r live digest=%r over %d files"
                   % (candidate.get("migration_digest"), live_migration_digest,
                      len(live_migration_map))},
        {"id": "catalog_query_produced_seven_result_blocks", "passed": artifacts_ok,
         "detail": "result blocks=%d row counts=%r"
                   % (len(blocks), [count for count, _ in blocks])},
        {"id": "postgres_server_major_at_least_18",
         "passed": version_major is not None and version_major >= 18,
         "detail": "version=%r" % (version_text.strip() or "missing")},
        {"id": "lifecycle_columns_match_expected_tables",
         "passed": artifact_tables == expected_tables and len(expected_tables) > 0,
         "detail": "artifact=%r expected=%r"
                   % (sorted(artifact_tables), sorted(expected_tables))},
        {"id": "attachment_referencing_constraints_present",
         "passed": constraint_rows > 0 and attachment_fk > 0,
         "detail": "constraint rows=%d REFERENCES attachment rows=%d"
                   % (constraint_rows, attachment_fk)},
        {"id": "attachment_path_unique_index_present", "passed": bool(path_unique),
         "detail": "unique path index rows=%d" % len(path_unique)},
        {"id": "attachment_triggers_present",
         "passed": len(attachment_triggers) >= 3 and deleted_path_guard,
         "detail": "attachment trigger rows=%d deleted_path_guard=%s"
                   % (len(attachment_triggers), deleted_path_guard)},
        {"id": "attachment_functions_present",
         "passed": function_rows > 0 and attach_functions,
         "detail": "function rows=%d fn_attachment_defs=%s"
                   % (function_rows, attach_functions)},
        {"id": "artifact_tables_exist_in_replayed_db",
         "passed": bool(artifact_tables) and artifact_tables <= live_tables,
         "detail": "artifact lifecycle tables=%d, missing from db %r=%r"
                   % (len(artifact_tables), database,
                      sorted(artifact_tables - live_tables))},
    ]
    failed = [a for a in assertions if not a["passed"]]
    if failed:
        for item in failed:
            sys.stderr.write("w0_02_schema assertion failed: %s (%s)\n"
                             % (item["id"], item["detail"]))
        fail("measured replay/catalog observations do not satisfy the W0-02/schema "
             "oracle; %d/%d assertions failed (see above)"
             % (len(failed), len(assertions)))

    leaf = leaves[0]
    finished = datetime.datetime.now(datetime.timezone.utc)
    observation = {
        "leaf": leaf,
        "started_at": STARTED.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "finished_at": finished.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "exit_code": 0,
        "executed": len(assertions),
        "passed": sum(1 for a in assertions if a["passed"]),
        "failed": 0,
        "skipped": 0,
        "oracle_observations": {
            "pre_db_state": (
                "run-owned fresh database %r (attv5_<slot>_raw, dropped/recreated by "
                "gate run_case); pre-replay emptiness is not recorded in CASE_DIR "
                "artifacts" % database),
            "pre_object_hash": (
                "admission baseline migration set sha256=%s over %d files "
                "(up=%d down=%d); live tree matches admission snapshot"
                % (migration_digest, len(baseline_migrations),
                   len(base_ups), len(base_downs))),
            "action": (
                "docker exec psql --single-transaction -f - for each of %d frozen "
                "Up files, then frozen evidence/catalog-query.sql read-only"
                % replayed_count),
            "expected_http_semantics": "n/a (plan-domain leaf; no HTTP surface)",
            "expected_db_delta": (
                "schema objects created by %d Up migrations; catalog query returns "
                "%d result blocks with row counts %r"
                % (replayed_count, len(blocks), [c for c, _ in blocks])),
            "expected_object_state": (
                "PostgreSQL %s.%s; lifecycle tables=%d %r; constraints=%d; "
                "uk_attachment_path UNIQUE(path)=%s; attachment trigger rows=%d; "
                "attach/retention function rows=%d"
                % (version_major, version_minor, len(artifact_tables),
                   sorted(artifact_tables), constraint_rows, bool(path_unique),
                   len(attachment_triggers), function_rows)),
            "proof_actor": "main-integrator (run-owned scratch PostgreSQL)",
        },
        "assertions": assertions,
        "actor": {"kind": "real", "driver": leaf.get("driver"),
                  "resource_kind": "scratch_postgres", "resource_id": database},
        "resource_ids": [database],
        "request_ids": [
            "w0-02-schema-raw-%s" % sha256_file(raw_log)[:16],
            "w0-02-schema-db-%s" % database,
        ],
    }
    if (finished - STARTED).total_seconds() > leaf.get("observation_deadline_seconds", 1800):
        fail("observation window exceeded the leaf deadline")

    output = os.path.join(CASE_DIR, "observations.json")
    with open(output, "w", encoding="utf-8") as fh:
        json.dump({"leaves": [observation]}, fh, ensure_ascii=False, indent=1)
    print(json.dumps({"adapter": "w0_02_schema", "leaves": 1, "output": output}))


if __name__ == "__main__":
    main()
