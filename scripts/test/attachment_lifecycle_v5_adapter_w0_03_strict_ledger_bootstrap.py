#!/usr/bin/env python3
"""W0-03/strict-ledger-bootstrap adapter: measured strict-ledger leaves.

Artifacts parsed:
  <case>/raw.log    output of scripts/test/attachment_lifecycle_v5_strict_ledger.escript
                    (real erlang_migrate:up/1 with strict=true, strict_bootstrap=fail)
                    plus the retroverify_w001_w002 RETRO_OK line
  <run>/baseline-current.json  admission-time migration sha256 map
  run-owned PostgreSQL container  independent read-only ledger inspection of the
                    database named by STRICT_LEDGER_OK
  <repo>/docs/.../evidence/w0-0{1,2}-input-summary.json  bootstrap receipts

Leaf 1 asserts the ledger facts (dirty=false, exact version set, head=max,
inputs unchanged since admission). Leaf 2 revalidates both bootstrap receipts
(existence, JSON, commit at candidate HEAD, reported exit codes). Any missing
artifact or false assertion exits 30 with the exact gap.
"""
import datetime
import hashlib
import json
import os
import re
import subprocess
import sys

CASE_ID = "W0-03/strict-ledger-bootstrap"
RUN_DIR = os.path.abspath(sys.argv[1])
CASE_DIR = os.path.abspath(sys.argv[2])
ROOT = os.path.abspath(sys.argv[3])

STARTED = datetime.datetime.now(datetime.timezone.utc)


def fail(gaps):
    if isinstance(gaps, str):
        gaps = [gaps]
    for gap in gaps:
        sys.stderr.write("w0_03_strict_ledger_bootstrap adapter: %s\n" % gap.rstrip())
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
    container_id = info.get("Id")
    if (not isinstance(container_id, str) or not re.fullmatch(r"[0-9a-f]{64}", container_id)
            or container_id != pg.get("container_id") or info.get("Running") is not True):
        fail("live container identity/running state differs from receipt")
    if labels.get("codex.run") != run_name \
            or labels.get("codex.role") != "attachment-lifecycle-pg":
        fail("live container labels do not match the run-owned PostgreSQL role")
    return container_id


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


def git(root, args):
    try:
        proc = subprocess.run(["git", "-C", root] + args, capture_output=True,
                              text=True, timeout=30)
    except (OSError, subprocess.SubprocessError) as exc:
        fail("git %r failed in %s: %s" % (args, root, exc))
    return proc


def version_of(filename):
    match = re.match(r"^(\d+)_", filename)
    if not match:
        fail("migration filename %r has no numeric version prefix" % filename)
    return int(match.group(1))


def main():
    state, candidate, allow = preflight()
    container = owned_container(allow)
    password = pg_password()

    contract = load_json(os.path.join(state["plan_dir"], "execution-contract.json"),
                         "frozen execution contract")
    leaves = contract.get("expected_leaf_matrix", {}).get(CASE_ID)
    if not leaves or len(leaves) != 2:
        fail("contract expected_leaf_matrix[%r] is not exactly 2 leaves" % CASE_ID)
    by_ordering = {leaf.get("ordering_or_fault"): leaf for leaf in leaves}

    raw_path = os.path.join(CASE_DIR, "raw.log")
    raw = read_text(raw_path, "case raw.log")

    cur_match = re.search(
        r"(?m)^current_version=(\S+) dirty=(\S+) history_rows=(\d+)$", raw)
    ok_match = re.search(r"STRICT_LEDGER_OK db=(\S+) versions=\[(.*?)\]", raw, re.S)
    retro_match = re.search(
        r"(?m)^RETRO_OK w0-01 w0-02 summaries committed and parseable in "
        r"candidate (.+)$", raw)
    strict_errors = re.findall(
        r"STRICT_UP_ERROR|STRICT_LEDGER_VERSION_MISMATCH|STRICT_LEDGER_DIRTY|"
        r"STRICT_LEDGER_EMPTY", raw)
    if not cur_match:
        fail("raw.log has no 'current_version=.. dirty=.. history_rows=..' line "
             "(strict escript did not verify the ledger); errors=%r"
             % (strict_errors or "none"))
    if not ok_match:
        fail("raw.log has no STRICT_LEDGER_OK db/versions line; the strict "
             "erlang_migrate:up run did not succeed")

    head_version = int(cur_match.group(1))
    dirty_raw = cur_match.group(2).strip().lower()
    history_rows = int(cur_match.group(3))
    ledger_db = ok_match.group(1)
    versions_raw = ok_match.group(2)
    reported_versions = sorted(int(v) for v in
                               re.split(r"\s*,\s*", versions_raw.strip())
                               if v.strip())
    dirty_false = dirty_raw in ("f", "false", "0", "no")

    registered_dbs = allow.get("scratch_postgres") or []
    if ledger_db not in registered_dbs:
        fail("STRICT_LEDGER_OK database %r is not registered in "
             "resource_allowlist.scratch_postgres" % (ledger_db,))

    baseline = load_json(os.path.join(RUN_DIR, "baseline-current.json"),
                         "admission baseline snapshot")
    migrations = baseline.get("migration_files_sha256")
    if not isinstance(migrations, dict) or not migrations:
        fail("baseline-current.json has no migration_files_sha256 map")
    base_files = sorted(migrations)
    expected_versions = sorted(version_of(name) for name in base_files
                               if name.endswith(".up.sql"))
    migration_dir = os.path.join(ROOT, "priv/migrations")
    live_files = sorted(name for name in os.listdir(migration_dir)
                        if name.endswith((".up.sql", ".down.sql")))
    hash_drift = [name for name in base_files
                  if not os.path.exists(os.path.join(migration_dir, name))
                  or sha256_file(os.path.join(migration_dir, name))
                  != migrations[name]]
    set_drift = sorted(set(base_files) ^ set(live_files))
    live_migration_map = {
        name: sha256_file(os.path.join(migration_dir, name))
        for name in live_files}
    live_migration_digest = hashlib.sha256(
        json.dumps(live_migration_map, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False).encode()).hexdigest()

    live_head_rows = psql(
        container, ledger_db, password,
        "SELECT version::text, dirty::text FROM schema_migrations "
        "ORDER BY version DESC LIMIT 1")
    if len(live_head_rows) != 1 or "\x1f" not in live_head_rows[0]:
        fail("live database %r has no single schema_migrations head row (rows=%r)"
             % (ledger_db, live_head_rows))
    live_head, live_dirty = live_head_rows[0].split("\x1f", 1)
    live_history = psql(
        container, ledger_db, password,
        "SELECT version::text FROM schema_migrations_history ORDER BY version")
    live_versions = sorted(int(v) for v in live_history)

    leaf1_assertions = [
        {"id": "strict_up_reported_no_error_tokens", "passed": not strict_errors,
         "detail": "strict error tokens=%r" % (strict_errors or "none")},
        {"id": "ledger_head_not_dirty", "passed": dirty_false,
         "detail": "current_version=%d dirty=%r history_rows=%d"
                   % (head_version, cur_match.group(2), history_rows)},
        {"id": "history_rows_match_frozen_up_set",
         "passed": history_rows == len(expected_versions),
         "detail": "history_rows=%d frozen up versions=%d (files=%d)"
                   % (history_rows, len(expected_versions), len(base_files))},
        {"id": "version_set_equals_frozen_up_set",
         "passed": reported_versions == expected_versions,
         "detail": "reported=%d versions, frozen=%d versions, equal=%s"
                   % (len(reported_versions), len(expected_versions),
                      reported_versions == expected_versions)},
        {"id": "head_version_is_frozen_max",
         "passed": head_version == (max(expected_versions)
                                    if expected_versions else -1),
         "detail": "head=%d frozen max=%r"
                   % (head_version, max(expected_versions) if expected_versions else None)},
        {"id": "live_ledger_matches_emitted_measurement",
         "passed": live_head == cur_match.group(1)
                   and live_dirty.lower() in ("f", "false", "0", "no")
                   and live_versions == expected_versions,
         "detail": "live head=%r dirty=%r history=%d in db %r"
                   % (live_head, live_dirty, len(live_versions), ledger_db)},
        {"id": "migration_inputs_match_admission_baseline",
         "passed": not hash_drift and not set_drift,
         "detail": "sha256 drift=%d set drift=%d over %d admission files "
                   "(no executor-emitted pre/post snapshot exists in CASE_DIR; "
                   "compared against the admission snapshot captured before "
                   "execution)"
                   % (len(hash_drift), len(set_drift), len(base_files))},
        {"id": "migration_set_matches_frozen_candidate_digest",
         "passed": candidate.get("migration_digest") == live_migration_digest,
         "detail": "candidate migration_digest=%r live digest=%r over %d files"
                   % (candidate.get("migration_digest"), live_migration_digest,
                      len(live_migration_map))},
    ]
    failed = [a for a in leaf1_assertions if not a["passed"]]
    if failed:
        for item in failed:
            sys.stderr.write("w0_03_strict_ledger assertion failed: %s (%s)\n"
                             % (item["id"], item["detail"]))
        fail("strict-ledger replay leaf does not satisfy its oracle")

    summaries = {}
    for name in ("w0-01-input-summary.json", "w0-02-input-summary.json"):
        path = os.path.join(ROOT, "docs/architecture/attachment-lifecycle-v5/evidence", name)
        if not os.path.isfile(path):
            fail("bootstrap receipt missing: %s" % path)
        summaries[name] = (path, load_json(path, "bootstrap receipt"))

    w01_path, w01 = summaries["w0-01-input-summary.json"]
    w02_path, w02 = summaries["w0-02-input-summary.json"]
    w01_task = w01.get("task_id")
    w02_task = w02.get("task_id")
    summary_ok = (w01_task == "W0-01" and w02_task == "W0-02"
                  and w01.get("exit_code") == 0
                  and isinstance(w02.get("case_results"), dict)
                  and w02["case_results"].get("W0-02/schema", {}).get("exit_code") == 0
                  and w02["case_results"].get("W0-02/history-unknown", {})
                  .get("exit_code") == 0)

    head = git(ROOT, ["rev-parse", "HEAD"]).stdout.strip()
    candidate_head = candidate["heads"].get("imboy")
    committed = {}
    for name, (path, _doc) in sorted(summaries.items()):
        rel = os.path.relpath(path, ROOT)
        log = git(ROOT, ["log", "--format=%H", "-1", "--", rel])
        cat = git(ROOT, ["cat-file", "-e", "HEAD:" + rel])
        committed[name] = (rel, log.stdout.strip(), cat.returncode == 0)

    retro_ok = (retro_match is not None
                and os.path.realpath(retro_match.group(1).strip()) == os.path.realpath(ROOT))
    retro_failures = re.findall(
        r"RETRO_MISSING|RETRO_INVALID_JSON|RETRO_NOT_COMMITTED", raw)
    commit_ok = (head == candidate_head and summary_ok
                 and len(committed) == 2
                 and all(sha and exists for _rel, sha, exists in committed.values()))

    leaf2_assertions = [
        {"id": "retro_ok_line_names_both_receipts_in_candidate",
         "passed": retro_ok,
         "detail": "RETRO_OK=%r candidate=%r" % (bool(retro_match), ROOT)},
        {"id": "bootstrap_receipts_present_and_report_exit_zero",
         "passed": summary_ok,
         "detail": "w0-01 task=%r exit=%r; w0-02 task=%r schema_exit=%r "
                   "history_exit=%r"
                   % (w01_task, w01.get("exit_code"), w02_task,
                      w02.get("case_results", {}).get("W0-02/schema", {}).get("exit_code"),
                      w02.get("case_results", {}).get("W0-02/history-unknown", {})
                      .get("exit_code"))},
        {"id": "bootstrap_receipts_committed_at_candidate_head",
         "passed": commit_ok,
         "detail": "HEAD=%r candidate_head=%r commits=%r"
                   % (head, candidate_head,
                      {n: {"commit": c[1][:12], "in_head": c[2]}
                       for n, c in committed.items()})},
        {"id": "no_retro_verify_failure_tokens", "passed": not retro_failures,
         "detail": "retro failure tokens=%r" % (retro_failures or "none")},
        {"id": "bootstrap_receipts_carry_raw_output_hashes",
         "passed": all(isinstance(doc.get("raw_output_sha256"), str)
                       and len(doc["raw_output_sha256"]) == 64
                       for _p, doc in summaries.values()),
         "detail": "%r" % ({n: doc.get("raw_output_sha256")
                            for n, (_p, doc) in summaries.items()},)},
    ]
    failed2 = [a for a in leaf2_assertions if not a["passed"]]
    if failed2:
        for item in failed2:
            sys.stderr.write("w0_03_bootstrap_revalidation assertion failed: %s (%s)\n"
                             % (item["id"], item["detail"]))
        fail("bootstrap receipt revalidation leaf does not satisfy its oracle")

    migration_digest = hashlib.sha256(
        json.dumps(migrations, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False).encode()).hexdigest()
    finished = datetime.datetime.now(datetime.timezone.utc)
    stamp_start = STARTED.strftime("%Y-%m-%dT%H:%M:%SZ")
    stamp_end = finished.strftime("%Y-%m-%dT%H:%M:%SZ")

    leaf1 = by_ordering["strict_ledger_replay"]
    leaf2 = by_ordering["bootstrap_receipt_revalidation"]
    observations = [
        {
            "leaf": leaf1,
            "started_at": stamp_start, "finished_at": stamp_end,
            "exit_code": 0,
            "executed": len(leaf1_assertions), "passed": len(leaf1_assertions),
            "failed": 0, "skipped": 0,
            "oracle_observations": {
                "pre_db_state": "fresh run-owned separate database %r created by "
                                "strict_ledger_case (never the W0-02 raw DB)"
                                % (ledger_db,),
                "pre_object_hash": "admission migration set sha256=%s over %d files"
                                   % (migration_digest, len(base_files)),
                "action": "escript attachment_lifecycle_v5_strict_ledger.escript -> "
                          "erlang_migrate:up(#{strict=>true, strict_bootstrap=>fail}) "
                          "over %d frozen Up files" % len(expected_versions),
                "expected_http_semantics": "n/a (plan-domain leaf; no HTTP surface)",
                "expected_db_delta": "schema_migrations head=%d dirty=false; "
                                     "schema_migrations_history rows=%d"
                                     % (head_version, history_rows),
                "expected_object_state": "ledger version set equals the frozen Up "
                                         "set (%d versions, max=%d); live re-read of "
                                         "%r matches"
                                         % (len(expected_versions),
                                            max(expected_versions) if expected_versions else -1,
                                            ledger_db),
                "proof_actor": "main-integrator (run-owned scratch PostgreSQL)",
            },
            "assertions": leaf1_assertions,
            "actor": {"kind": "real", "driver": leaf1.get("driver"),
                      "resource_kind": "scratch_postgres",
                      "resource_id": ledger_db},
            "resource_ids": [ledger_db],
            "request_ids": ["w0-03-strict-%s-head-%d" % (ledger_db, head_version)],
        },
        {
            "leaf": leaf2,
            "started_at": stamp_start, "finished_at": stamp_end,
            "exit_code": 0,
            "executed": len(leaf2_assertions), "passed": len(leaf2_assertions),
            "failed": 0, "skipped": 0,
            "oracle_observations": {
                "pre_db_state": "no database mutation; read-only receipt revalidation "
                                "in candidate %r" % ROOT,
                "pre_object_hash": "w0-01 sha256=%s w0-02 sha256=%s"
                                   % (sha256_file(w01_path), sha256_file(w02_path)),
                "action": "retroverify_w001_w002 output parsed + independent "
                          "existence/JSON/git-commit recheck",
                "expected_http_semantics": "n/a (no HTTP surface)",
                "expected_db_delta": "none (read-only revalidation)",
                "expected_object_state": "both bootstrap receipts present, parseable, "
                                         "committed at candidate HEAD %r; recorded "
                                         "run_ids w0-01=%r w0-02=%r"
                                         % (candidate_head, w01.get("run_id"),
                                            w02.get("run_id")),
                "proof_actor": "main-integrator",
            },
            "assertions": leaf2_assertions,
            "actor": {"kind": "targeted-contract", "driver": leaf2.get("driver"),
                      "resource_kind": "repository",
                      "resource_id": candidate["repository_paths"]["imboy"]},
            "resource_ids": [candidate["repository_paths"]["imboy"]],
            "request_ids": [
                "w0-03-retro-w0-01-%s" % sha256_file(w01_path)[:12],
                "w0-03-retro-w0-02-%s" % sha256_file(w02_path)[:12],
            ],
        },
    ]

    for observation in observations:
        deadline = observation["leaf"].get("observation_deadline_seconds", 1800)
        if (finished - STARTED).total_seconds() > deadline:
            fail("observation window exceeded leaf deadline %r" % deadline)

    output = os.path.join(CASE_DIR, "observations.json")
    with open(output, "w", encoding="utf-8") as fh:
        json.dump({"leaves": observations}, fh, ensure_ascii=False, indent=1)
    print(json.dumps({"adapter": "w0_03_strict_ledger_bootstrap",
                      "leaves": len(observations), "output": output}))


if __name__ == "__main__":
    main()
