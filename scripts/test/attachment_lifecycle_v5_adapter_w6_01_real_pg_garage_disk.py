#!/usr/bin/env python3
"""W6-01/real-pg-garage-disk measured adapter.

Measured leaves come only from artifacts the W6-01 gate executor produced by
real execution:

  * W1-02 plan leaves: the two real schema-phase SQL probe runs
    (w102-expand / w102-guards), using exactly the probe adequacy rules that the
    frozen W1-02 adapters apply (placeholder probes stay gaps).
  * W4-03/driver-content-immutable leaves: one real eunit unit plus one
    W403_HTTP_EVIDENCE record per (surface, driver, scenario) from the
    real-PG + real-Garage + real-Disk driver matrix.
  * the per-leaf-observing battery groups (OBS_STEP_FOR): the W4-03 six
    (w403 step), the W2 upload/confirm eight (w2upload step) and the
    W4 queue/worker/activation twenty-one (w4queue step) — each battery
    suite emits one observation line per executed leaf
    (attachment_lifecycle_v5_obs:emit, env-gated JSONL) keyed by
    (case_id, entrypoint, scenario, domain, measured.driver); the adapter
    binds each contract leaf to its unique matching observation inside
    the executing step's window (multi-line keys disambiguate via
    measured.ac_id).  The W2-01/pending-multipart enterprise leaves are
    measured on the enterprise_seat real chain (seat proxy PUT protocol
    per upload_surface_contract, same registered-ledger oracle) by the
    same w2upload battery and bind through the identical observation
    path — no scenario is blocked or substituted.

All other W6-01 leaves require a per-(domain, driver) rerun that the contract
itself sizes at ~39.8h of leaf time; the source batteries executed by the gate
emit per-entrypoint/aggregate units, not per-domain/per-driver receipts.  They
are reported as named evidence gaps (grouped by source case) and the adapter
exits 30 instead of relabelling an aggregate unit as 1271 leaf PASSes.
"""
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as W4  # noqa: E402

CASE_ID = "W6-01/real-pg-garage-disk"
NAME = "w6_01_real_pg_garage_disk"

DRIVER_MATRIX = "attachment_lifecycle_v5_w403_driver_matrix_tests"
BODY_RE = re.compile(r'-define\(BODY,\s*<<"(.*?)">>\)\.', re.S)

# Source groups whose battery suites emit one observation line per executed
# leaf; each group binds to the gate step that executed it (the step's
# subdirectory holds observations.jsonl and its window bounds the receipts).
OBS_STEP_FOR = {}
for _ref in (
    "W4-03/storage-switch",
    "W4-03/storage-disk",
    "W4-03/storage-garage",
    "W4-03/late-put",
    "W4-03/namespace-alias-integrity",
    "W4-03/disk-predecessor-protocols",
):
    OBS_STEP_FOR[_ref] = "w403"
for _ref in (
    "W2-01/pending-failclosed",
    "W2-01/pending-multipart",
    "W2-01/session-recovery-api",
    "W2-01/upload-content-pin",
    "W2-02/confirm-idempotent",
    "W2-02/confirm-old-client",
    "W2-02/confirmed-content-immutable",
    "W2-03/confirm-gc-race",
):
    OBS_STEP_FOR[_ref] = "w2upload"
for _ref in (
    "W4-01/confirm-gc-race",
    "W4-01/delete-entrypoints",
    "W4-01/hold-entrypoints",
    "W4-01/hold-gc-race",
    "W4-01/intent-rollback",
    "W4-01/reference-gc-race",
    "W4-02/claim-capacity-and-liveness",
    "W4-02/cross-queue-identity",
    "W4-02/last-attempt-crash",
    "W4-02/old-attempts-preserved",
    "W4-02/ordinary-queue-recovery",
    "W4-02/purged-session-recovery",
    "W4-02/queue-adoption",
    "W4-02/worker-crash",
    "W4-02/worker-db-fault",
    "W4-02/worker-lease",
    "W4-04/activation-commit-unknown",
    "W4-04/all-writer-compat",
    "W4-04/guards-activation",
    "W4-04/no-infinite-loop",
    "W4-04/reconcile",
):
    OBS_STEP_FOR[_ref] = "w4queue"
for _ref in (
    "W3-02/hold-all-entrypoints",
    "W3-02/hold-gc-race",
):
    OBS_STEP_FOR[_ref] = "w3hold"
for _ref in (
    "W3-01/reference-gc-race",
    "W3-01/shared-owner",
):
    OBS_STEP_FOR[_ref] = "w3race"
for _ref in (
    "W3-03/cs-bind",
    "W3-03/enterprise-bind",
):
    OBS_STEP_FOR[_ref] = "w3bind"
OBS_GROUPS = set(OBS_STEP_FOR)

LEGACY_WITHOUT_AC = frozenset({
    "W4-03/storage-switch", "W4-03/storage-disk", "W4-03/storage-garage",
    "W4-03/late-put", "W4-03/namespace-alias-integrity",
    "W4-03/disk-predecessor-protocols",
    "W4-01/confirm-gc-race", "W4-01/delete-entrypoints",
    "W4-01/hold-entrypoints", "W4-01/hold-gc-race",
    "W4-01/intent-rollback", "W4-01/reference-gc-race",
    "W4-02/claim-capacity-and-liveness", "W4-02/cross-queue-identity",
    "W4-02/last-attempt-crash", "W4-02/old-attempts-preserved",
    "W4-02/ordinary-queue-recovery", "W4-02/purged-session-recovery",
    "W4-02/queue-adoption", "W4-02/worker-crash",
    "W4-02/worker-db-fault", "W4-02/worker-lease",
    "W4-04/activation-commit-unknown", "W4-04/all-writer-compat",
    "W4-04/guards-activation", "W4-04/no-infinite-loop", "W4-04/reconcile",
})

# Same adequacy judgments as the frozen W1-02 adapters: only probes that
# execute the leaf's defining action become a measured plan leaf.
ADEQUATE = {
    ("W1-02/migration", "expand-queue"): {
        "base_read_compat": ["base_read_compat", "base_read_compat2"],
        "pk_fk_attempts_preserved": ["pk_fk_attempts_preserved",
                                     "pk_fk_attempts_preserved2"],
        "ddl_rollback": ["ddl_rollback", "ddl_rollback2"],
        "partial_location_null_rejected": ["partial_location_null_rejected"],
        "recovery_id_unique_owner_conflict": [
            "recovery_id_unique_owner_conflict"],
        "renew_expiry_monotonic_and_deadline_fixed": [
            "renew_expiry_monotonic_and_deadline_fixed"],
        "function_digest_same_across_phases": [
            "function_digest_same_across_phases"],
    },
    ("W1-02/migration", "guards-contract"): {
        "base_read_compat": ["base_read_compat", "base_read_compat2"],
        "pk_fk_attempts_preserved": ["pk_fk_attempts_preserved",
                                     "pk_fk_attempts_preserved2"],
        "ddl_rollback": ["ddl_rollback", "ddl_rollback2"],
        "partial_location_null_rejected": ["partial_location_null_rejected"],
        "recovery_id_unique_owner_conflict": [
            "recovery_id_unique_owner_conflict"],
        "renew_expiry_monotonic_and_deadline_fixed": [
            "renew_expiry_monotonic_and_deadline_fixed"],
        "function_digest_same_across_phases": [
            "function_digest_same_across_phases"],
    },
    ("W1-02/immutable", "guards-contract"): {
        name: [name] for name in (
            "mutate_driver", "mutate_namespace", "mutate_container",
            "mutate_key", "mutate_owner", "mutate_scope", "mutate_cipher",
            "mutate_locator_digest", "tombstone_recreation")
    },
}

INADEQUATE = {
    "old_null_session_protected":
        "probe returns 'ok' from both CASE branches and only reads existing "
        "rows; no backfill/UPDATE against legacy NULL client_upload_id",
    "large_table_write_block_bound":
        "probe SQL is the constant SELECT 5; no large load/lock/write-block "
        "timing is executed",
    "backfill_interrupt_resume":
        "probe UPDATEs then explicitly RAISE EXCEPTION; no batch/cursor/"
        "resume/post-state assertion",
    "rolling_old_writer_no_gc":
        "probe counts legacy_error rows; no old writer or GC authorization "
        "attempt is executed",
    "expand_required_functions_callable":
        "probe counts pg_proc rows; no function is invoked and no ACL "
        "callability is checked",
    "guards_do_not_install_missing_functions":
        "probe counts triggers; it does not check whether M3 installed or "
        "omitted a function",
    "first_bind_once":
        "probe runs UPDATE group_file_id=NULL then SELECT 'ok'; no first bind "
        "and no second-bind attempt",
    "same_value_replay":
        "the only same_value_replay row measures upload registration replay, "
        "not attachment-binding replay",
}

GROUP_REASONS = {
    "W1-02/migration":
        "this plan leaf needs the W1-02 probe that the frozen W1-02 adapter "
        "already declares inadequate for measurement",
    "W1-02/immutable":
        "this plan leaf needs the W1-02 probe that the frozen W1-02 adapter "
        "already declares inadequate for measurement",
}

DEFAULT_REASON = (
    "the source battery executed for W6-01 emits per-entrypoint/aggregate "
    "units; no per-(domain,driver) unit, HTTP request ids or object-byte hash "
    "exists for this source scenario")


def _body_sha256(repo_root, case_id):
    path = os.path.join(repo_root, "test", "lib",
                        "attachment_lifecycle_v5_w403_driver_matrix_tests.erl")
    text = S.read_text(path, case_id, "driver-matrix suite source")
    match = BODY_RE.search(text)
    if not match:
        S.die(case_id, "?BODY define missing from %s" % path)
    body = match.group(1).encode("utf-8")
    line = text.count("\n", 0, match.start()) + 1
    return hashlib.sha256(body).hexdigest(), line, path


def _scenario_line(repo_root, scenario, driver, case_id):
    path = os.path.join(repo_root, "test", "lib",
                        "attachment_lifecycle_v5_w403_driver_matrix_tests.erl")
    text = S.read_text(path, case_id, "driver-matrix suite source")
    for needle in ("scenario(S, Surface, %s, %s) ->" % (driver, scenario),
                   "scenario(S, Surface, Driver, %s) ->" % scenario):
        line = S.C.source_line(text, needle)
        if line:
            return line, path
    S.die(case_id, "scenario %r/%r has no source line in %s"
          % (scenario, driver, path))
    return None, path


def _replay_count(path, case_id):
    text = S.read_text(path, case_id, "w102 phase replay log")
    matches = re.findall(r"replayed (\d+) up files", text)
    if not matches:
        S.die(case_id, "%s has no 'replayed N up files' line" % path)
    count = int(matches[-1])
    if count < 1:
        S.die(case_id, "%s reports replay of 0 up files" % path)
    return count


def _obs_scenario(leaf):
    # ordering_or_fault = "<source_case_ref>/<schema_phase>/<scenario...>";
    # the suites emit the scenario literal without the leading two segments
    # (predecessor leaves keep their full "W4-01/<group>/<fault>" literal).
    rest = leaf["ordering_or_fault"][len(leaf["source_case_ref"]) + 1:]
    return rest.split("/", 1)[1]


def _obs_index(path, case_id):
    """Map the 5-tuple binding key to every observation line carrying it.

    A 5-tuple may legitimately appear on multiple lines (e.g. the
    pending-multipart group emits one line per ac_id for the same
    scenario); ambiguity is resolved at match time via measured.ac_id,
    so duplicates are collected instead of rejected here.
    """
    if not os.path.isfile(path) or os.path.getsize(path) == 0:
        S.die(case_id, "w403 battery produced no per-leaf observation "
                       "stream at %s" % path)
    index = {}
    with open(path, "r", encoding="utf-8") as handle:
        for line_no, raw in enumerate(handle, 1):
            raw = raw.strip()
            if not raw:
                continue
            obs = json.loads(raw)
            key = (obs.get("case_id"), obs.get("entrypoint"),
                   obs.get("scenario"), obs.get("domain"),
                   (obs.get("measured") or {}).get("driver"))
            index.setdefault(key, []).append((line_no, obs))
    return index, path


def _match_observation(index, key, ac_id):
    matches = []
    for line_no, obs in index.get(key, []):
        measured = obs.get("measured") or {}
        if ("ac_id" in measured and measured["ac_id"] == ac_id
                or "ac_id" not in measured and key[0] in LEGACY_WITHOUT_AC):
            matches.append((line_no, obs))
    if len(matches) > 1:
        S.die(CASE_ID, "observation %r is ambiguous for ac_id=%r at lines %r"
              % (key, ac_id, [line for line, _ in matches]))
    return matches[0][1] if matches else None


def _step_binding(case_dir, subdir, cache):
    """Load one battery step's record, window and observation stream."""
    record = S.step_record(case_dir, subdir, CASE_ID)
    if record["exit"] != 0:
        S.die(CASE_ID, "%s step recorded %r" % (subdir, record["line"]))
    log = S.eunit_log(os.path.join(case_dir, subdir, "eunit-%s.log" % subdir),
                      CASE_ID, "W6-01 %s battery" % subdir)
    started, finished = S.step_window(record, CASE_ID)
    index, path = _obs_index(
        os.path.join(case_dir, subdir, "observations.jsonl"), CASE_ID)
    cache[subdir] = {
        "record": record, "log": log, "markers": set(log["parsed"]["markers"]),
        "started": started, "finished": finished,
        "index": index, "path": path,
    }
    return cache[subdir]


def build():
    run_dir, case_dir, root = S.C.argv_paths()
    ctx = S.context(run_dir, CASE_ID, root)
    leaves = S.leaves(ctx, CASE_ID)
    allowlist = ctx["allowlist"]
    try:
        slot = S.C.resolve_slot(run_dir)
        expand_db = S.C.db_resource(allowlist, slot, "w102_expand")
        guards_db = S.C.db_resource(allowlist, slot, "w102_guards")
        bucket = S.C.garage_bucket(run_dir, allowlist)
    except S.C.AdapterGap as exc:
        S.die(CASE_ID, str(exc))
    disk_root = W4.resolve_disk_root(ctx, CASE_ID)
    body_sha, body_line, body_path = _body_sha256(root, CASE_ID)

    w403_step = S.step_record(case_dir, "w403", CASE_ID)
    w403 = S.eunit_log(os.path.join(case_dir, "w403", "eunit-w403.log"),
                       CASE_ID, "W6-01 w403 real-driver battery")
    w403_markers = set(w403["parsed"]["markers"])
    if w403_step["exit"] != 0 or "W403_DIRECT_OK" not in w403_markers:
        S.die(CASE_ID, "w403 battery did not complete: %r markers=%r"
              % (w403_step["line"], sorted(w403_markers)))
    started403, finished403 = S.step_window(w403_step, CASE_ID)
    duration403 = (finished403 - started403).total_seconds()
    if duration403 > leaves[0].get("observation_deadline_seconds", 1800):
        S.die(CASE_ID, "w403 battery measured %.1fs exceeds the leaf deadline"
              % duration403)
    idx403, obs_path403 = _obs_index(
        os.path.join(case_dir, "w403", "observations.jsonl"), CASE_ID)
    obs_cache = {"w403": {
        "record": w403_step, "log": w403, "markers": w403_markers,
        "started": started403, "finished": finished403,
        "index": idx403, "path": obs_path403}}

    phases = {}
    for phase, subdir, db in (("expand", "w102-expand", expand_db),
                              ("guards", "w102-guards", guards_db)):
        record = S.step_record(case_dir, "w102-%s" % phase, CASE_ID)
        if record["exit"] != 0:
            S.die(CASE_ID, "w102-%s step recorded %r" % (phase, record["line"]))
        sub = os.path.join(case_dir, subdir)
        phases[phase] = {
            "probes": S.probe_file(os.path.join(sub, "probes.jsonl"), phase,
                                   CASE_ID),
            "db": db,
            "replay": _replay_count(os.path.join(sub, "raw.log"), CASE_ID),
            "line": record["line"],
            "window": S.step_window(record, CASE_ID),
        }

    rows = []
    gap_groups = {}
    for leaf in leaves:
        source = leaf.get("source_case_ref") or leaf["entrypoint"]
        if (source == "W4-03/driver-content-immutable"
                and leaf["driver"] in ("garage", "disk")):
            desc = "%s/%s/%s" % (leaf["entrypoint"], leaf["driver"],
                                 leaf["ordering_or_fault"].split("/")[-1])
            unit = S.find_eunit_unit(w403, DRIVER_MATRIX, desc)
            if unit is None:
                S.group_gap(gap_groups, source, 1,
                            "driver-matrix unit %r parsed missing" % desc)
                continue
            entry = S.http_evidence(w403, desc)
            ids, statuses, requests = S.validate_requests(entry, CASE_ID, desc)
            scenario = leaf["ordering_or_fault"].split("/")[-1]
            scenario_line, scenario_path = _scenario_line(
                root, scenario, leaf["driver"], CASE_ID)
            resource = bucket if leaf["driver"] == "garage" else disk_root
            actor = (S.garage_actor(bucket) if leaf["driver"] == "garage"
                     else S.disk_actor(disk_root))
            chain = "; ".join("%s %s -> %s" % (
                request.get("method"), request.get("route"),
                request.get("status")) for request in requests)
            assertions = [
                S.assertion(
                    "driver_matrix_unit_passed",
                    "%s:%s desc=%r [%.3fs] ok"
                    % (DRIVER_MATRIX, unit["func"], desc,
                       unit["duration"] or 0.0)),
                S.assertion(
                    "http_evidence_requests",
                    "%s: %d request(s) request_ids=%r statuses=%r"
                    % (os.path.basename(w403["path"]), len(requests), ids,
                       statuses)),
                S.assertion(
                    "scenario_source_line",
                    "%s:%d declares scenario %r for driver=%s"
                    % (os.path.basename(scenario_path), scenario_line,
                       scenario, leaf["driver"])),
                S.assertion(
                    "entrypoint_provenance_bound",
                    "entrypoint=%s request_id=%s route=%s method=%s handler=%s"
                    % (leaf["entrypoint"], requests[0].get("request_id"),
                       requests[0].get("route"), requests[0].get("method"),
                       requests[0].get("handler"))),
                S.assertion(
                    "w403_battery_green",
                    "%r; W403_DIRECT_OK; %d parsed units, summary=%r"
                    % (w403_step["line"], len(w403["parsed"]["tests"]),
                       w403["parsed"]["summary_raw"])),
            ]
            oracle = {
                "pre_db_state": "real PG isolated config DB with the full "
                                "migration chain; driver-matrix unit %r ran "
                                "through the real HTTP fixture (%s)"
                                % (desc, w403["path"]),
                "pre_object_hash": "fixture body sha256=%s from ?BODY at %s:%d"
                                   % (body_sha, os.path.basename(body_path),
                                      body_line),
                "action": "%s (driver-matrix %s:%s); HTTP sequence: %s"
                          % (desc, DRIVER_MATRIX, unit["func"], chain),
                "expected_http_semantics": "the leaf's real HTTP requests "
                                           "returned %r (at least one 2xx)"
                                           % statuses,
                "expected_db_delta": "the unit asserts the scenario's DB/"
                                     "visibility invariant before returning ok",
                "expected_object_state": "the unit asserts the %s driver's "
                                         "object state for this scenario"
                                         % leaf["driver"],
                "proof_actor": "real driver %s resource %s; W403 log sha256=%s;"
                               " request ids %r"
                               % (leaf["driver"], resource, w403["sha256"], ids),
            }
            row = S.C.build_row(
                leaf, started403, finished403,
                {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
                 "skipped": 0},
                oracle, assertions, actor, [resource],
                ids or ["w403:%s" % desc])
            # driver-content-immutable is a frozen upload-surface case: the
            # receipt must carry the real route/method/handler/request_id of
            # the leaf's own entrypoint (never another surface's receipt).
            first = requests[0]
            row["entrypoint_provenance"] = {
                "entrypoint": leaf["entrypoint"],
                "route": first.get("route"),
                "method": first.get("method"),
                "handler": first.get("handler"),
                "request_id": first.get("request_id"),
            }
            rows.append(row)
            continue
        if leaf["domain"] == "plan" and source in (
                "W1-02/migration", "W1-02/immutable"):
            phase = S.PHASE_MAP.get(leaf.get("source_schema_phase"))
            scenario = leaf.get("source_scenario")
            if phase not in ("expand", "guards") or not scenario:
                S.group_gap(gap_groups, source, 1,
                            "leaf has no resolvable phase/scenario "
                            "(phase=%r scenario=%r)"
                            % (leaf.get("source_schema_phase"), scenario))
                continue
            names = ADEQUATE.get((source, leaf.get("source_schema_phase")), {}).get(scenario)
            if not names:
                reason = INADEQUATE.get(
                    scenario,
                    "no adequate W1-02 probe is declared for this plan "
                    "scenario in phase %s" % phase)
                S.group_gap(gap_groups, source, 1, reason)
                continue
            probes = phases[phase]["probes"]
            lines = [S.require_probe(probes, name, CASE_ID)
                     for name in names]
            assertions = [S.assertion(
                "probe_%s_passed" % name,
                "%s:%d name=%s pass=true"
                % (os.path.basename(probes["path"]), line_no, name))
                for name, line_no in zip(names, lines)]
            assertions.extend([
                S.assertion(
                    "probe_phase_summary",
                    "%s phase=%s ok=%d bad=0"
                    % (os.path.basename(probes["path"]), phase,
                       probes["summary"].get("ok"))),
                S.assertion(
                    "phase_replay",
                    "%s: replayed %d up files; executor %r"
                    % (phase, phases[phase]["replay"], phases[phase]["line"])),
            ])
            db = phases[phase]["db"]
            oracle = {
                "pre_db_state": "fresh %s with %d replayed up migrations in the "
                                "%s schema phase"
                                % (db, phases[phase]["replay"], phase),
                "pre_object_hash": "n/a: this plan leaf is a catalog/DDL/row "
                                   "probe; no provider object exists",
                "action": "psql probes %r on %s" % (names, db),
                "expected_http_semantics": "no HTTP call: the leaf's stated "
                                           "property is a database/catalog "
                                           "assertion",
                "expected_db_delta": "the named probe's expected value is "
                                     "asserted by the real psql run",
                "expected_object_state": ("guards reject the mutation; the "
                                          "row's storage identity and cipher "
                                          "stay unchanged"
                                          if source == "W1-02/immutable"
                                          else "no object mutation in this "
                                               "plan probe"),
                "proof_actor": "psql imboy_user on %s; probe artifact sha256=%s"
                               % (db, probes["sha256"]),
            }
            rows.append(S.C.build_row(
                leaf, phases[phase]["window"][0], phases[phase]["window"][1],
                {"exit_code": 0, "executed": len(names),
                 "passed": len(names), "failed": 0, "skipped": 0},
                oracle, assertions, S.db_actor(db), [db],
                ["w102:%s:%s" % (phase, name) for name in names]))
            continue
        if source in OBS_GROUPS:
            subdir = OBS_STEP_FOR[source]
            step = (obs_cache[subdir] if subdir in obs_cache
                    else _step_binding(case_dir, subdir, obs_cache))
            obs_started, obs_finished = step["started"], step["finished"]
            obs_index, obs_path = step["index"], step["path"]
            scenario = _obs_scenario(leaf)
            key = (source, leaf["entrypoint"], scenario, leaf["domain"],
                   leaf["driver"])
            obs = _match_observation(obs_index, key, leaf.get("ac_id"))
            if obs is None:
                S.group_gap(gap_groups, source, 1,
                            "no %s-step observation matches %r ac_id=%r"
                            % (subdir, key, leaf.get("ac_id")))
                continue
            measured = obs.get("measured") or {}
            emitted = obs.get("emitted_at_ms")
            t0_ms = obs_started.timestamp() * 1000
            t1_ms = obs_finished.timestamp() * 1000
            if not isinstance(emitted, (int, float)) \
                    or not t0_ms <= emitted <= t1_ms:
                S.die(CASE_ID, "observation %r emitted_at_ms=%r outside the "
                               "%s step window" % (key, emitted, subdir))
            resource = bucket if leaf["driver"] == "garage" else disk_root
            actor = (S.garage_actor(bucket) if leaf["driver"] == "garage"
                     else S.disk_actor(disk_root))
            digest = measured.get("bytes_sha256")
            status_keys = [k for k in sorted(measured)
                           if k.endswith(("status", "_result"))]
            msum = "; ".join("%s=%s" % (k, measured[k])
                             for k in sorted(measured) if k != "driver")
            if len(msum) > 800:
                msum = msum[:797] + "..."
            obj_keys = "; ".join(
                "%s=%s" % (k, measured[k]) for k in sorted(measured)
                if any(t in k for t in ("object", "bytes", "delete", "key")))
            assertions = [
                S.assertion(
                    "w403_observation_matched",
                    "%s: case_id=%s entrypoint=%s scenario=%s domain=%s "
                    "driver=%s measured(%d keys)"
                    % (os.path.basename(obs_path), source,
                       leaf["entrypoint"], scenario, leaf["domain"],
                       leaf["driver"], len(measured))),
                S.assertion(
                    "observation_in_step_window",
                    "emitted_at_ms=%d within [%d, %d] of %r"
                    % (emitted, t0_ms, t1_ms, step["record"]["line"])),
                S.assertion(
                    "measured_driver_bound",
                    "measured.driver=%s bound to the run-owned %s resource %s"
                    % (leaf["driver"], leaf["driver"], resource)),
                S.assertion(
                    "battery_step_green",
                    "%r; markers=%r; %d parsed units, summary=%r"
                    % (step["record"]["line"], sorted(step["markers"]),
                       len(step["log"]["parsed"]["tests"]),
                       step["log"]["parsed"]["summary_raw"])),
                S.assertion(
                    "integrated_phase_battery_db",
                    "the w403 step ran on the run-owned isolated config with "
                    "the full migration chain (marker DB attv5_<slot>_w601); "
                    "the suites emit their historical static phase label %r"
                    % obs.get("schema_phase")),
            ]
            oracle = {
                "pre_db_state": "real PG isolated config DB provisioned by "
                                "the full migration chain; the %s unit ran "
                                "against the real %s driver"
                                % (source, leaf["driver"]),
                "pre_object_hash": (
                    "the unit seeds its own fixture bytes; measured "
                    "post-action digest sha256=%s" % digest
                    if digest else
                    "n/a: queue/catalog scenario; the unit's assertions cover "
                    "the DB state transitions"),
                "action": "%s/%s (driver=%s) executed by the w403 battery; "
                          "measured: %s"
                          % (source, scenario, leaf["driver"], msum),
                "expected_http_semantics": (
                    "measured transport outcomes: %s"
                    % "; ".join("%s=%s" % (k, measured[k])
                                for k in status_keys)
                    if status_keys else
                    "no HTTP surface in this scenario; the unit asserts the "
                    "driver/queue semantics directly"),
                "expected_db_delta": "the unit asserts the scenario's DB/"
                                     "queue invariant before returning ok "
                                     "(w403 suite contract)",
                "expected_object_state": (
                    "measured object fields: %s" % obj_keys
                    if obj_keys else
                    "driver-state only; no provider object field in the "
                    "measured payload"),
                "proof_actor": "real driver %s resource %s; observations "
                               "sha256=%s"
                               % (leaf["driver"], resource,
                                  S.sha256_file(obs_path)),
            }
            rows.append(S.C.build_row(
                leaf, obs_started, obs_finished,
                {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
                 "skipped": 0},
                oracle, assertions, actor, [resource],
                ["w403-obs:%s#%s@%s" % (source, scenario, leaf["driver"])]))
            continue
        reason = GROUP_REASONS.get(source, DEFAULT_REASON)
        S.group_gap(
            gap_groups, source, 1,
            reason + "; oracle_observations.pre_db_state/pre_object_hash/"
                     "action/expected_http_semantics/expected_db_delta/"
                     "expected_object_state/proof_actor unmeasured at leaf "
                     "granularity and no real-driver actor receipt exists")

    if gap_groups:
        S.report_gaps(CASE_ID, len(rows), gap_groups)
    S.C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    S.run_adapter(NAME, build)
