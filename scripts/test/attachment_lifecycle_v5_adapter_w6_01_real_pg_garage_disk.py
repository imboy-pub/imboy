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

All other W6-01 leaves require a per-(domain, driver) rerun that the contract
itself sizes at ~39.8h of leaf time; the source batteries executed by the gate
emit per-entrypoint/aggregate units, not per-domain/per-driver receipts.  They
are reported as named evidence gaps (grouped by source case) and the adapter
exits 30 instead of relabelling an aggregate unit as 1271 leaf PASSes.
"""
import hashlib
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
    "W2-01/pending-failclosed":
        "w201 probes execute one SQL chain per (entrypoint, scenario); the W6 "
        "leaf requires domain x driver expansion with a real object operation, "
        "and no per-driver probe or HTTP receipt exists",
    "W2-01/pending-multipart":
        "w201 multipart probes execute one SQL chain per (entrypoint, "
        "scenario); no per-domain/per-driver multipart HTTP receipt exists",
    "W2-01/session-recovery-api":
        "renewal/revocation SQL probes are per-entrypoint; no per-domain/"
        "per-driver HTTP request ids or object bytes are recorded",
    "W2-01/upload-content-pin":
        "pin probes are per (entrypoint, scenario); no per-driver provider "
        "request receipt is produced for this leaf",
    "W2-02/confirm-idempotent":
        "the confirm suite emits aggregate eunit counts; no per-domain/"
        "per-driver confirm HTTP receipt exists",
    "W2-02/confirm-old-client":
        "the confirm-old-client suite emits aggregate eunit counts; no "
        "per-domain/per-driver legacy HTTP receipt exists",
    "W2-02/confirmed-content-immutable":
        "confirm-content immutability is asserted in an aggregate PG suite; no "
        "per-driver object-hash receipt is emitted for this domain",
    "W3-02/hold-all-entrypoints":
        "the hold protocol suite runs against the isolated PG DB once; no "
        "garage/disk per-driver object operation or receipt exists",
    "W3-02/hold-gc-race":
        "the hold/GC race barrier suite is a single real-PG run; no per-driver "
        "object receipt exists",
    "W4-01/hold-entrypoints":
        "the W4-01 entrypoint suite is an aggregate real-PG run; no per-domain/"
        "per-driver object receipt exists",
    "W4-02/worker-crash":
        "worker crash tests drive the isolated PG queue; no per-driver object "
        "receipt is emitted for this leaf",
    "W4-02/worker-db-fault":
        "worker DB-fault tests drive the isolated PG queue; no per-driver "
        "object receipt is emitted",
    "W4-02/queue-adoption":
        "queue-adoption tests are aggregate PG/eunit units; no per-domain/"
        "per-driver receipt exists",
    "W4-02/purged-session-recovery":
        "purged-session recovery is an aggregate PG/eunit run; no per-driver "
        "object receipt exists",
    "W4-03/storage-garage":
        "the W4-03 storage suite has no unit with entrypoint storage-garage; "
        "driver-matrix units are keyed surface/driver/scenario, not this leaf",
    "W4-03/storage-disk":
        "the W4-03 disk unit is one aggregate function (put/read/head or "
        "late-put) without per-scenario status lines",
    "W4-03/storage-switch":
        "the storage-switch units are per-scenario eunit aggregates without "
        "the W6 domain x driver expansion receipt",
    "W4-03/namespace-alias-integrity":
        "the namespace/alias suite is a single eunit aggregate; no per-driver "
        "namespace stat or locator-digest receipt exists",
    "W4-03/disk-predecessor-protocols":
        "the predecessor suite has one aggregate run/0 over representative "
        "intents; the 66 source scenarios have no individual status lines",
    "W4-03/late-put":
        "the late-put suite is an aggregate real-PG unit; no per-driver "
        "provider receipt exists",
    "W4-04/reconcile":
        "reconcile tests are process-level aggregates; no per-driver object "
        "receipt exists",
    "W4-04/no-infinite-loop":
        "the no-infinite-loop suite is an aggregate real-PG run; no "
        "per-driver receipt exists",
    "W4-04/guards-activation":
        "guards-activation is an aggregate activation-order test; no "
        "per-driver object receipt exists",
    "W4-04/all-writer-compat":
        "all-writer-compat is an aggregate writer test; no per-domain/"
        "per-driver receipt exists",
    "W4-04/activation-commit-unknown":
        "activation commit-unknown tests are aggregate catalog/DDL assertions; "
        "no per-driver object receipt exists",
    "W4-02/claim-capacity-and-liveness":
        "claim capacity/liveness tests are aggregate PG timing units; no "
        "per-driver receipt exists",
    "W4-02/last-attempt-crash":
        "last-attempt crash tests are aggregate PG queue units; no per-driver "
        "receipt exists",
    "W4-02/old-attempts-preserved":
        "old-attempt tests are aggregate PG queue units; no per-driver receipt "
        "exists",
    "W4-02/ordinary-queue-recovery":
        "ordinary queue recovery is an aggregate PG unit; no per-driver "
        "receipt exists",
    "W4-02/worker-lease":
        "worker lease tests are aggregate PG timing units; no per-driver "
        "receipt exists",
    "W4-02/cross-queue-identity":
        "cross-queue identity is an aggregate PG unit; no per-driver receipt "
        "exists",
    "W3-01/reference-gc-race":
        "reference-GC race barriers are single real-PG runs; no per-driver "
        "object receipt exists",
    "W3-01/shared-owner":
        "shared-owner tests are aggregate PG units; no per-driver receipt "
        "exists",
    "W4-01/reference-gc-race":
        "W4-01 reference-GC race barriers are single real-PG runs; no "
        "per-driver receipt exists",
    "W4-01/hold-gc-race":
        "W4-01 hold/GC race barriers are single real-PG runs; no per-driver "
        "receipt exists",
    "W4-01/confirm-gc-race":
        "confirm/GC race barriers are single real-PG runs; no per-driver "
        "receipt exists",
    "W4-01/intent-rollback":
        "intent-rollback tests are aggregate PG fault-injection units; no "
        "per-driver receipt exists",
    "W4-01/delete-entrypoints":
        "delete-entrypoint call-graph tests are aggregate static/PG units; no "
        "per-driver object receipt exists",
    "W3-03/cs-bind":
        "CS bind tests are aggregate PG assertions; no per-driver receipt "
        "exists",
    "W3-03/enterprise-bind":
        "enterprise bind tests are aggregate PG assertions; no per-driver "
        "receipt exists",
    "W2-03/confirm-gc-race":
        "the confirm/GC barrier suite is a single real-PG run; no per-driver "
        "receipt exists",
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
