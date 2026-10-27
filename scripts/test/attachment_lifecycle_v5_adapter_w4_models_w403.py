#!/usr/bin/env python3
"""W4-03 leaf models and the driver-content-immutable leaf binder."""
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from attachment_lifecycle_v5_adapter_w4_models_common import (  # noqa: E402
    DRIVER_SCENARIOS,
    SCENARIO_TEXT,
    S,
    _oracle,
    base,
)

def _w403_units(f):
    return [S.describe_test(t) for t in f.parsed["tests"]
            if t["module"].startswith("attachment_lifecycle_v5_w403_")]


def model_storage_garage(f):
    descs = [t["desc"] for t in f.parsed["tests"]
             if t["module"] == "attachment_lifecycle_v5_w403_driver_matrix_tests"]
    matrix = [d for d in descs if S.DRIVER_LEAF_RE.match(d)]
    roundtrips = [d for d in descs if d.count("/") == 1]
    env_bucket = os.environ.get("W202_GARAGE_BUCKET", "").strip()
    unit = f.test("attachment_lifecycle_v5_w403_storage_tests", func="storage_switch")

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        missing = []
        if scenario == "put_head_get_range_delete":
            missing.append(
                "no executed unit implements put_head_get_range_delete; "
                "driver-matrix roundtrip units (%s) perform register/put/read/"
                "confirm only, not HEAD/range/delete; storage_tests disk units "
                "are disk-only"
                % (", ".join(roundtrips) or "none parsed"))
        else:
            missing.append(
                "no executed unit implements unauthorized_prefix; storage_tests "
                "and driver_matrix contain no prefix-authorization denial, and "
                "attachment_lifecycle_v5_w403_gateway_protocol_tests is not in "
                "w403_case Mods")
        missing.append(
            "domain=%s entrypoint=storage-garage: no fixture with this identity "
            "(driver-matrix descriptions are <surface>/<driver>/<scenario>, not "
            "storage-garage)" % leaf["domain"])
        missing.append(
            "driver=garage provenance: W202_GARAGE_BUCKET env is %s; contract "
            "requires a real garage bucket operation and namespace stat for "
            "this leaf"
            % (repr(env_bucket) if env_bucket else "not exported"))
        missing.append(_oracle())
        return missing

    return gaps, [
        "parsed driver-matrix units: %d (%d leaf-shaped, %d roundtrips)"
        % (len(descs), len(matrix), len(roundtrips)),
        "storage-garage needs the gateway/HTTP fixture; w403_case exports only "
        "V5_DISK_ROOT and does not set W403_RESOURCE_MANIFEST/"
        "W403_CONFIG_PATH/W202_GARAGE_*",
    ]


def _w403_sources(f):
    rels = (
        "test/lib/attachment_lifecycle_v5_w403_storage_tests.erl",
        "test/lib/attachment_lifecycle_v5_w403_driver_matrix_tests.erl",
        "test/lib/attachment_lifecycle_v5_w403_predecessor_tests.erl",
    )
    return [f.src(rel)[1] for rel in rels]


def model_storage_disk(f):
    _path, text = f.src("test/lib/attachment_lifecycle_v5_w403_storage_tests.erl")
    combined = "\n".join(_w403_sources(f)).lower()
    path_line = S.line_of(text, "disk_path_validation() ->")
    immutable_line = S.line_of(text, "driver_content_immutable_disk() ->")
    late_line = S.line_of(text, "late_put_atomic_visible() ->")
    has_symlink = "symlink" in combined
    has_interrupted = "interrupt" in combined
    unit = f.test("attachment_lifecycle_v5_w403_storage_tests", func="disk_path_validation")

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        missing = []
        if scenario == "put_head_get_range_delete":
            missing.append(
                "no single executed unit performs put+head+range+delete: "
                "driver_content_immutable_disk/0 (%s:%d) does put/read/head and "
                "late_put_atomic_visible/0 (%s:%d) does put/read/delete"
                % ("test/lib/attachment_lifecycle_v5_w403_storage_tests.erl",
                   immutable_line,
                   "test/lib/attachment_lifecycle_v5_w403_storage_tests.erl",
                   late_line))
        elif scenario in ("path_traversal", "absolute_path"):
            missing.append(
                "disk_path_validation/0 (test/lib/"
                "attachment_lifecycle_v5_w403_storage_tests.erl:%d) asserts "
                "relative dot-dot, absolute segment and control-character "
                "rejection in one aggregate unit; no per-scenario unit"
                % path_line)
        elif scenario == "symlink_escape":
            missing.append(
                "no symlink fixture in the executed w403 sources "
                "(test/lib/attachment_lifecycle_v5_w403_*.erl symlink token=%s)"
                % has_symlink)
        else:
            missing.append(
                "no interrupted/torn-write fixture in the executed w403 sources "
                "(interrupt token=%s)" % has_interrupted)
        missing.append(
            "domain=%s: storage_tests sets one registry (disk-ns1/imboy-ns1) "
            "with no domain fixture; driver-matrix scenarios are for "
            "driver-content-immutable, not storage-disk" % leaf["domain"])
        missing.append(_oracle())
        return missing

    return gaps, [
        "storage_tests exposes per-file units but not the five contract "
        "scenario names; no per-domain dimension exists",
    ]


def model_storage_switch(f):
    _path, text = f.src("test/lib/attachment_lifecycle_v5_w403_storage_tests.erl")
    switch_line = S.line_of(text, "storage_switch() ->")

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        if scenario == "default_switch_same_key":
            detail = ("storage_switch/0 (%s:%d) only asserts resolve/"
                      "driver_module mapping; it never writes the same key "
                      "through both drivers"
                      % ("test/lib/attachment_lifecycle_v5_w403_storage_tests.erl",
                         switch_line))
        elif scenario == "unknown_driver":
            detail = ("storage_switch/0 asserts {error,unsupported_driver} for "
                      "'cos' inside the same multi-scenario unit; no per-driver "
                      "or per-domain status")
        else:
            detail = ("no fallback test exists (%s); neither storage_tests nor "
                      "driver_matrix records a 404/403 no-fallback response"
                      % scenario)
        return [
            detail,
            "driver=%s: the suite only checks module mapping, not a %s put/"
            "read/delete or namespace stat" % (leaf["driver"], leaf["driver"]),
            "domain=%s: no domain-specific fixture exists" % leaf["domain"],
            _oracle(),
        ]

    return gaps, ["storage-switch has one aggregate registry unit, not 16 "
                  "driver/domain leaves"]


def model_late_put(f):
    descs = [t["desc"] for t in f.parsed["tests"]
             if t["module"] == "attachment_lifecycle_v5_w403_driver_matrix_tests"]
    expiry = [d for d in descs
              if d.endswith("put_paused_before_dispatch_until_expiry")]

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        if scenario == "held_put_finishes_after_expiry":
            detail = (
                "driver-matrix units %s implement a paused PUT ending after "
                "expiry, but they are W4-03/driver-content-immutable leaves and "
                "carry a different leaf identity; storage_tests "
                "late_put_atomic_visible/0 only checks immediate put->read->"
                "delete" % (", ".join(expiry) or "none parsed"))
        elif scenario == "held_put_finishes_after_delete_attempt":
            detail = ("no fixture pauses a PUT across a delete attempt; the "
                      "executed suites never combine a held PUT with a delete "
                      "intent")
        else:
            detail = ("no drain-block/backpressure fixture exists; no unit "
                      "records an unbounded drain being bounded")
        return [
            detail,
            "entrypoint=late-put driver=%s domain=%s: no unit with this "
            "identity; driver-matrix scenarios are per-surface, not late-put"
            % (leaf["driver"], leaf["domain"]),
            _oracle(),
        ]

    return gaps, ["late-put scenarios are not implemented as contract leaves"]


def model_namespace_alias_integrity(f):
    _path, text = f.src("test/lib/attachment_lifecycle_v5_w403_storage_tests.erl")
    combined = "\n".join(_w403_sources(f))
    alias_line = S.line_of(text, "namespace_alias_integrity() ->")
    unknown_line = S.line_of(text, "unknown_namespace_fail_closed() ->")
    has_rotation = bool(re.search(r"rotat", combined, re.I))

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        if scenario == "undeclared_alias_sentinel":
            detail = (
                "unknown_namespace_fail_closed/0 "
                "(test/lib/attachment_lifecycle_v5_w403_storage_tests.erl:%d) "
                "asserts resolve/identity/canonical return {error,unknown} for "
                "an unregistered namespace; it does not exercise a declared-root "
                "alias sentinel" % unknown_line)
        elif scenario == "same_root_canonical_lock":
            detail = (
                "namespace_alias_integrity/0 "
                "(test/lib/attachment_lifecycle_v5_w403_storage_tests.erl:%d) "
                "asserts two aliases canonicalize to imboy-ns1 and share a "
                "locator digest in one aggregate unit" % alias_line)
        elif scenario == "locator_remap_rejected":
            detail = (
                "only attachment_lifecycle_v5_w403_gateway_protocol_tests:"
                "registry_remap_rejected/1 implements remap rejection; that "
                "module is not in w403_case Mods")
        else:
            detail = (
                "no credential-rotation fixture exists in the executed w403 "
                "sources (rotation token present=%s)" % has_rotation)
        return [
            detail,
            "driver=%s: identity is registry-level; no per-driver put/read or "
            "exact namespace stat is executed" % leaf["driver"],
            "domain=cross_domain: the executed units are single-domain "
            "registry checks; no cross-domain producer fixture exists",
            _oracle(),
        ]

    return gaps, ["namespace aliases are checked by two aggregate storage "
                  "units, not 8 driver-scoped leaves"]


def model_disk_predecessor_protocols(f):
    src_rel = "test/lib/attachment_lifecycle_v5_w403_predecessor_tests.erl"
    _path, text = f.src(src_rel)
    rollback_line = S.line_of(text, "p_intent_rollback_disk(Conn, DiskRoot) ->")
    done_line = S.line_of(text, "p_worker_disk_delete_done(Conn, DiskRoot) ->")
    crash_line = S.line_of(text, "p_worker_crash_reap_disk(Conn, DiskRoot) ->")
    entry_line = S.line_of(text, "p_entrypoint_intents_disk_env(Conn) ->")
    unit = f.test("attachment_lifecycle_v5_w403_predecessor_tests", func="run")

    def gaps(leaf):
        source_case = leaf.get("source_case_ref", "")
        source_scenario = leaf.get("source_scenario", "")
        if source_case == "W4-01/intent-rollback":
            if source_scenario == "sql_rollback":
                detail = (
                    "p_intent_rollback_disk/2 (%s:%d) injects "
                    "{error,{db,sql_rollback}} into "
                    "attachment_repo:intent_ordinary_delete_tx for one key and "
                    "asserts the disk file remains; entrypoint %r is not "
                    "invoked and there is no per-entrypoint unit"
                    % (src_rel, rollback_line, leaf["entrypoint"]))
            else:
                detail = (
                    "no disk-driver fault for %r: p_intent_rollback_disk/2 "
                    "(%s:%d) only injects sql_rollback; no audit/insert/"
                    "commit-response fault is executed for entrypoint %r"
                    % (source_scenario, src_rel, rollback_line,
                       leaf["entrypoint"]))
        elif source_case == "W4-02/worker-crash":
            detail = (
                "p_worker_crash_reap_disk/2 (%s:%d) exercises lease-expiry -> "
                "reap -> reclaim for one key; source scenario %r is not "
                "separately injected and has no own unit"
                % (src_rel, crash_line, source_scenario))
        elif source_case == "W4-02/worker-db-fault":
            detail = (
                "no database-fault injection exists in the predecessor suite "
                "for %r" % source_scenario)
        elif source_case == "W4-02/queue-adoption":
            detail = (
                "no queue-adoption scenario exists in the predecessor suite "
                "for %r" % source_scenario)
        else:
            detail = (
                "p_worker_disk_delete_done/2 (%s:%d) covers ready -> worker disk "
                "delete -> ack done for one key; source scenario %r is not "
                "implemented" % (src_rel, done_line, source_scenario))
        return [
            S.per_leaf_gap(unit, "predecessor disk aggregate run/0"),
            detail,
            "entrypoint_intents_disk_env/2 (%s:%d) covers only orphan/pending/"
            "empty-collect/not-found shapes, none of them this source scenario"
            % (src_rel, entry_line),
            _oracle(),
        ]

    return gaps, [
        "contract maps 66 W4-01/W4-02 source scenarios onto disk; the "
        "predecessor suite has 3 worker/rollback functions plus one entrypoint "
        "probe, all inside one aggregate run/0",
    ]

# --------------------------------------------------------------------------
# W4-03/driver-content-immutable: the only W4 case with one eunit unit and
# one W403_HTTP_EVIDENCE record per frozen leaf.
# --------------------------------------------------------------------------
BODY_RE = re.compile(r'-define\(BODY,\s*<<"(.*?)">>\)\.', re.S)


def _driver_test_map(f):
    mapping = {}
    for test in f.parsed["tests"]:
        if test["module"] != "attachment_lifecycle_v5_w403_driver_matrix_tests":
            continue
        match = S.DRIVER_LEAF_RE.match(test["desc"])
        if match:
            mapping[test["desc"]] = test
    return mapping


def _http_evidence_map(f):
    mapping = {}
    for entry in f.w403_evidence:
        if entry.get("kind") != "HTTP_EVIDENCE":
            continue
        payload = entry.get("payload")
        if isinstance(payload, dict) and isinstance(payload.get("leaf"), str):
            mapping[payload["leaf"]] = entry
    return mapping


def _value_marker(f, kind, desc):
    for entry in f.w403_evidence:
        if entry.get("kind") != kind:
            continue
        payload = entry.get("payload")
        if isinstance(payload, dict) and payload.get("leaf") == desc:
            return entry
    return None


def _is_int(value):
    return isinstance(value, int) and not isinstance(value, bool)


def _validate_requests(entry, desc):
    problems = []
    if entry is None:
        return ["no W403_HTTP_EVIDENCE record for leaf %r" % desc], []
    payload = entry.get("payload") if entry else None
    requests = payload.get("requests") if isinstance(payload, dict) else None
    if not isinstance(requests, list) or not requests:
        return ["W403_HTTP_EVIDENCE for %r has no requests array" % desc], []
    ids = []
    statuses = []
    routes = []
    for index, request in enumerate(requests, 1):
        if not isinstance(request, dict):
            problems.append("request %d is not an object" % index)
            continue
        request_id = request.get("request_id")
        if not (isinstance(request_id, str) and request_id.startswith("w403-")):
            problems.append("request %d has no w403- request_id (%r)"
                            % (index, request_id))
        else:
            ids.append(request_id)
        status = request.get("status")
        if not _is_int(status):
            problems.append("request %d has no integer status (%r)"
                            % (index, status))
        else:
            statuses.append(status)
        route = request.get("route")
        if not (isinstance(route, str) and route):
            problems.append("request %d has no route" % index)
        else:
            routes.append(route)
        if not isinstance(request.get("method"), str) or not request.get("method"):
            problems.append("request %d has no method" % index)
        if not isinstance(request.get("handler"), str) or not request.get("handler"):
            problems.append("request %d has no handler" % index)
    if statuses and not any(200 <= status < 300 for status in statuses):
        problems.append("no 2xx request in the leaf evidence")
    return problems, ids


def _scenario_line(f, text, scenario, driver):
    for needle in (
            "scenario(S, Surface, %s, %s) ->" % (driver, scenario),
            "scenario(S, Surface, Driver, %s) ->" % scenario):
        line = S.line_of(text, needle)
        if line:
            return line
    return None


def _body_sha256(f, text):
    match = BODY_RE.search(text)
    if not match:
        return None, None, "?BODY define not found in the driver-matrix source"
    body = match.group(1).encode("utf-8")
    line = text.count("\n", 0, match.start()) + 1
    return (hashlib_hex(body), body,
            "?BODY at test/lib/attachment_lifecycle_v5_w403_driver_matrix_tests.erl:%d"
            % line)


def hashlib_hex(data):
    return hashlib.sha256(data).hexdigest()


def _marker_problem(f, leaf, desc):
    scenario = leaf["ordering_or_fault"]
    selector = (leaf["entrypoint"], leaf["driver"], scenario)
    if scenario == "provider_bypass_writer_denied":
        entry = _value_marker(f, "PROVIDER_BYPASS", desc)
        if entry is None:
            return "W403_PROVIDER_BYPASS marker missing for %r" % desc, None
        payload = entry["payload"]
        if leaf["driver"] == "garage":
            status = payload.get("provider_http_status")
            if status not in (401, 403):
                return ("W403_PROVIDER_BYPASS provider_http_status=%r not in "
                        "401/403 for garage" % status), entry
        else:
            code = payload.get("unprivileged_writer_exit")
            owner = payload.get("owner_uid")
            writer = payload.get("writer_uid")
            if not _is_int(code) or code == 0:
                return ("W403_PROVIDER_BYPASS unprivileged_writer_exit=%r is "
                        "not a non-zero exit" % code), entry
            if not _is_int(owner) or not _is_int(writer) or owner == writer:
                return ("W403_PROVIDER_BYPASS uid separation invalid "
                        "(owner=%r writer=%r)" % (owner, writer)), entry
        return None, entry
    if scenario == "max_supported_file_gateway_budget":
        entry = _value_marker(f, "GATEWAY_BUDGET", desc)
        if entry is None:
            return "W403_GATEWAY_BUDGET marker missing for %r" % desc, None
        payload = entry["payload"]
        size = payload.get("payload_bytes")
        elapsed = payload.get("elapsed_ms")
        throughput = payload.get("throughput_bytes_per_second")
        peak = payload.get("peak_binary_delta_bytes")
        if not _is_int(size) or size <= 0:
            return "W403_GATEWAY_BUDGET payload_bytes=%r invalid" % size, entry
        if not _is_int(elapsed) or elapsed < 0:
            return "W403_GATEWAY_BUDGET elapsed_ms=%r invalid" % elapsed, entry
        if not _is_int(throughput) or throughput <= 0:
            return ("W403_GATEWAY_BUDGET throughput_bytes_per_second=%r "
                    "invalid" % throughput), entry
        if not _is_int(peak) or peak < 0:
            return "W403_GATEWAY_BUDGET peak_binary_delta_bytes=%r invalid" % peak, entry
        return None, entry
    if scenario == "disk_put_delete_lock_order_or_garage_expiry_fence" and \
            leaf["driver"] == "garage":
        entry = _value_marker(f, "GARAGE_EXPIRY", desc)
        if entry is None:
            return "W403_GARAGE_EXPIRY marker missing for %r" % desc, None
        payload = entry["payload"]
        if payload.get("status") != 400:
            return "W403_GARAGE_EXPIRY status=%r != 400" % payload.get("status"), entry
        if payload.get("provider_error_code") != "InvalidRequest":
            return ("W403_GARAGE_EXPIRY provider_error_code=%r != "
                    "InvalidRequest" % payload.get("provider_error_code")), entry
        if payload.get("provider_expiry_reason") != "signed_url_expired":
            return ("W403_GARAGE_EXPIRY provider_expiry_reason=%r != "
                    "signed_url_expired"
                    % payload.get("provider_expiry_reason")), entry
        return None, entry
    return None, None


def _provenance(f):
    problems = []
    pool = None
    try:
        slot = base.resolve_slot(f.ctx["run_dir"])
        pool = base.db_resource(f.ctx["allowlist"], slot, "pool")
    except base.AdapterGap as exc:
        problems.append("scratch_postgres pool unresolved: %s" % exc)
    allowed_garage = f.ctx["allowlist"].get("garage_bucket")
    bucket = os.environ.get("W202_GARAGE_BUCKET", "").strip()
    endpoint = os.environ.get("W202_GARAGE_ENDPOINT", "").strip()
    if not isinstance(allowed_garage, list) or not allowed_garage:
        problems.append("resource_allowlist.garage_bucket missing")
    elif bucket and bucket not in allowed_garage:
        problems.append("W202_GARAGE_BUCKET=%r outside allowlist %r"
                        % (bucket, allowed_garage))
    if not bucket:
        problems.append("W202_GARAGE_BUCKET is not exported; real garage "
                        "provenance cannot be proven")
    if not endpoint:
        problems.append("W202_GARAGE_ENDPOINT is not exported; real garage "
                        "provenance cannot be proven")
    allowed_disk = f.ctx["allowlist"].get("disk_root")
    disk_root = None
    if not isinstance(allowed_disk, str) or not allowed_disk:
        problems.append("resource_allowlist.disk_root missing")
    else:
        env_root = os.environ.get("V5_DISK_ROOT", "").strip()
        disk_root = os.path.abspath(env_root) if env_root else os.path.abspath(allowed_disk)
        allowed_clean = allowed_disk.rstrip("/")
        if disk_root != allowed_clean and not disk_root.startswith(
                allowed_clean + "/"):
            problems.append("disk root %s outside allowlist prefix %s"
                            % (disk_root, allowed_disk))
        if not os.path.isdir(disk_root):
            problems.append("disk root %s is not a directory" % disk_root)
    return {"pool": pool, "bucket": bucket, "endpoint": endpoint,
            "disk_root": disk_root}, problems


def _producer_carries_provenance(root):
    """True only if the frozen producer copies entrypoint_provenance into receipts."""
    path = os.path.join(root, "scripts", "attachment_lifecycle_v5_producer.py")
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            text = handle.read()
    except OSError:
        return False
    start = text.find("receipt = {")
    if start < 0:
        return "entrypoint_provenance" in text
    end = text.find("}", start)
    block = text[start:end if end > start else len(text)]
    return "entrypoint_provenance" in block


def binder_driver_content_immutable(f):
    _path, text = f.src("test/lib/attachment_lifecycle_v5_w403_driver_matrix_tests.erl")
    contract_descs = {
        "%s/%s/%s" % (leaf["entrypoint"], leaf["driver"],
                      leaf["ordering_or_fault"])
        for leaf in f.leaves}
    tests = _driver_test_map(f)
    evidence = _http_evidence_map(f)
    body_hash, body, body_note = _body_sha256(f, text)
    prov, prov_problems = _provenance(f)
    scenario_lines = {
        (scenario, driver): _scenario_line(f, text, scenario, driver)
        for scenario in DRIVER_SCENARIOS for driver in ("disk", "garage")}

    def gaps(leaf):
        desc = "%s/%s/%s" % (leaf["entrypoint"], leaf["driver"],
                             leaf["ordering_or_fault"])
        missing = list(f.health)
        test = tests.get(desc)
        if test is None:
            missing.append(
                "no parsed eunit unit with description %r in eunit-w403.log "
                "(parsed driver-matrix leaf descriptions: %s)"
                % (desc, ", ".join(sorted(tests)) or "none"))
        elif not test.get("ok"):
            missing.append("eunit unit %r is not ok" % desc)
        entry = evidence.get(desc)
        request_problems, request_ids = _validate_requests(entry, desc)
        missing.extend(request_problems)
        marker_problem, marker_entry = _marker_problem(f, leaf, desc)
        if marker_problem:
            missing.append(marker_problem)
        body_problem = None
        leaf_body_hash = body_hash
        if leaf["ordering_or_fault"] == "max_supported_file_gateway_budget":
            if marker_entry is None:
                body_problem = "max-size leaf has no GATEWAY_BUDGET payload_bytes"
            else:
                size = marker_entry["payload"].get("payload_bytes")
                if _is_int(size) and 0 < size <= 256 * 1024 * 1024:
                    leaf_body_hash = hashlib_hex(b"a" * size)
                else:
                    body_problem = ("max-size leaf payload_bytes=%r cannot be "
                                    "hashed" % size)
        elif body_hash is None:
            body_problem = body_note
        if body_problem:
            missing.append(body_problem)
        if leaf["driver"] == "garage":
            missing.extend(prov_problems)
        elif not prov.get("disk_root"):
            missing.append("disk root provenance missing")
        scenario_line = scenario_lines.get(
            (leaf["ordering_or_fault"], leaf["driver"]))
        if not scenario_line:
            missing.append("driver-matrix source has no scenario clause for %r"
                           % leaf["ordering_or_fault"])
        if missing:
            return missing
        return []

    producer_blocker = None
    if not _producer_carries_provenance(f.root):
        producer_blocker = (
            "frozen scripts/attachment_lifecycle_v5_producer.py builds "
            "receipts without entrypoint_provenance, but "
            "runner.receipt_errors requires it for upload_surface_contract "
            "case W4-03/driver-content-immutable; this measured leaf cannot "
            "be sealed through the frozen producer/seal path")
    gap_rows = []
    for leaf in f.leaves:
        missing = gaps(leaf)
        if producer_blocker:
            missing.append(producer_blocker)
        gap_rows.append(S.gap(leaf, missing))
    if any(row["missing"] for row in gap_rows):
        S.report_gaps(
            f.case_id, f.evidence, f.parsed, gap_rows,
            notes=f.health + [
                "cell requires one parsed unit and one W403_HTTP_EVIDENCE "
                "record per frozen leaf; %d/%d units parsed, %d evidence "
                "records parsed"
                % (len(contract_descs & set(tests)), len(contract_descs),
                   len(contract_descs & set(evidence))),
                "W403 driver matrix setup requires W403_RESOURCE_MANIFEST, "
                "W403_CONFIG_PATH and W202_GARAGE_*; w403_case exports only "
                "V5_DISK_ROOT",
                "?BODY source: %s (%s)" % (body_note, body_hash),
            ] + (["sealing blocker: " + producer_blocker]
                 if producer_blocker else []))
    # All leaves have runtime units + evidence; build measured rows.
    rows = []
    started, finished = S.measurement_window(f.evidence, f.parsed, f.case_id)
    min_deadline = min(
        leaf.get("observation_deadline_seconds", 1800) for leaf in f.leaves)
    if (finished - started).total_seconds() > min_deadline:
        S.die(f.case_id,
              "measured suite window %.3fs exceeds the contract observation "
              "deadline %ss" % ((finished - started).total_seconds(),
                                min_deadline))
    for leaf in f.leaves:
        desc = "%s/%s/%s" % (leaf["entrypoint"], leaf["driver"],
                             leaf["ordering_or_fault"])
        test = tests[desc]
        entry = evidence[desc]
        request_problems, request_ids = _validate_requests(entry, desc)
        marker_problem, marker_entry = _marker_problem(f, leaf, desc)
        if request_problems or marker_problem:
            S.die(f.case_id, "internal binder inconsistency for %r" % desc)
        requests = entry["payload"]["requests"]
        statuses = [request["status"] for request in requests
                    if _is_int(request.get("status"))]
        routes = sorted({request["route"] for request in requests
                         if isinstance(request.get("route"), str)})
        first_2xx = next((request for request in requests
                          if _is_int(request.get("status"))
                          and 200 <= request["status"] < 300), None)
        leaf_body_hash = body_hash
        leaf_body_note = body_note
        if leaf["ordering_or_fault"] == "max_supported_file_gateway_budget":
            size = marker_entry["payload"]["payload_bytes"]
            leaf_body_hash = hashlib_hex(b"a" * size)
            leaf_body_note = (
                "max-size fixture body b'a' x %d (payload_bytes measured by "
                "W403_GATEWAY_BUDGET)" % size)
        if leaf["driver"] == "garage":
            resource_id = prov["bucket"]
            actor = S.garage_actor(prov["bucket"])
            driver_detail = (
                "real garage bucket %s via attachment_storage_garage; "
                "W202_GARAGE_ENDPOINT=%s (W202_GARAGE_BUCKET matches the "
                "allowlist)" % (prov["bucket"], prov["endpoint"]))
            resource_ids = [prov["bucket"], prov["pool"]]
        else:
            resource_id = prov["disk_root"]
            actor = S.disk_actor(prov["disk_root"])
            driver_detail = (
                "real disk driver under V5_DISK_ROOT=%s (inside allowlisted "
                "disk_root; fixture creates attv5-w403-<hex> beneath it)"
                % prov["disk_root"])
            resource_ids = [prov["disk_root"], prov["pool"]]
        http_text, db_text, object_text = SCENARIO_TEXT[
            leaf["ordering_or_fault"]]
        assertions = [
            S.assertion(
                "named_eunit_unit_ok",
                "%s:%d %s" % (f.evidence["name"], test["line"],
                              S.describe_test(test))),
            S.assertion(
                "w403_http_evidence_requests",
                "%s:%d leaf=%r requests=%d statuses=%r routes=%r ids=%r"
                % (f.evidence["name"], entry["line"], desc, len(requests),
                   statuses, routes, request_ids[:3])),
            S.assertion("driver_provenance", driver_detail),
            S.assertion(
                "scenario_source_present",
                "test/lib/attachment_lifecycle_v5_w403_driver_matrix_tests.erl:"
                "%s defines scenario(..., %s); the named unit asserts the "
                "post-conditions in-source"
                % (scenario_lines[(leaf["ordering_or_fault"], leaf["driver"])],
                   leaf["ordering_or_fault"])),
        ]
        if marker_entry is not None:
            assertions.append(S.assertion(
                "measured_%s_marker" % marker_entry["kind"].lower(),
                "%s:%d payload=%s"
                % (f.evidence["name"], marker_entry["line"],
                   json.dumps(marker_entry["payload"], sort_keys=True))))
        oracle = {
            "pre_db_state": (
                "isolated scratch PostgreSQL pool %s; leaf registration "
                "requests recorded before the scenario action (first 2xx "
                "request %s status %s; all ids %r)"
                % (prov["pool"],
                   first_2xx["request_id"] if first_2xx else "none",
                   first_2xx["status"] if first_2xx else "n/a",
                   request_ids[:4])),
            "pre_object_hash": (
                "sha256=%s of the registered fixture body (%s); the named unit "
                "asserts read() returns exactly these bytes"
                % (leaf_body_hash, leaf_body_note)),
            "action": (
                "real HTTP through entrypoint %s plus real %s driver; eunit "
                "unit %s (log line %d)"
                % (leaf["entrypoint"], leaf["driver"],
                   S.describe_test(test), test["line"])),
            "expected_http_semantics": http_text,
            "expected_db_delta": db_text,
            "expected_object_state": object_text,
            "proof_actor": (
                "real driver + real PostgreSQL: %s, pool %s, runner pid "
                "evidence in W403_HTTP_EVIDENCE (request ids %r)"
                % (driver_detail, prov["pool"], request_ids[:2])),
        }
        provenance = {
            "entrypoint": leaf["entrypoint"],
            "route": first_2xx["route"],
            "method": first_2xx["method"],
            "handler": first_2xx["handler"],
            "request_id": first_2xx["request_id"],
        }
        rows.append(base.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, resource_ids, request_ids,
            provenance=provenance))
    base.assert_leaf_set(f.leaves, rows)
    base.emit(f.case_dir, rows, "w4_driver_content_immutable")

W403_MODELS = {
    "W4-03/storage-garage": model_storage_garage,
    "W4-03/storage-disk": model_storage_disk,
    "W4-03/storage-switch": model_storage_switch,
    "W4-03/late-put": model_late_put,
    "W4-03/namespace-alias-integrity": model_namespace_alias_integrity,
    "W4-03/disk-predecessor-protocols": model_disk_predecessor_protocols,
}
