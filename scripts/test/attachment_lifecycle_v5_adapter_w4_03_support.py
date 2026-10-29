#!/usr/bin/env python3
"""W4-03 storage-boundary adapter support.

Every W4-03 storage/predecessor leaf is sealed from one real suite observation
(case_id + entrypoint + scenario + domain + measured.driver), the parsed
named eunit unit, the clean suite summary and the driver's real resource
(run-owned disk root or the allowlisted garage bucket).  Nothing is
synthesized: measured fields are copied verbatim from the suite stream.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w3_support as W3  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as W4  # noqa: E402

SHORT = {
    "W4-03/storage-garage": "storage-garage",
    "W4-03/storage-disk": "storage-disk",
    "W4-03/storage-switch": "storage-switch",
    "W4-03/late-put": "late-put",
    "W4-03/namespace-alias-integrity": "namespace-alias-integrity",
    "W4-03/disk-predecessor-protocols": "disk-predecessor-protocols",
    "W4-04/reconcile": "reconcile",
    "W4-04/no-infinite-loop": "no-infinite-loop",
    "W4-04/guards-activation": "guards-activation",
    "W4-04/all-writer-compat": "all-writer-compat",
    "W4-04/activation-commit-unknown": "activation-commit-unknown",
}

STORAGE_SRC = "test/lib/attachment_lifecycle_v5_w403_storage_tests.erl"

HTTP_TEXT = {
    "put_head_get_range_delete": (
        "real provider PUT -> HEAD(size) -> full GET(hash) -> ranged GET -> "
        "DELETE -> exact not_found; no fallback path"
    ),
    "unauthorized_prefix": (
        "a signed capability for one key/prefix is rejected by the provider "
        "when the key path is altered (403) and an undeclared namespace never "
        "resolves; the authorized object stays intact"
    ),
    "path_traversal": (
        "no HTTP: disk keys containing dot-dot / dot / empty segments are "
        "rejected before any filesystem syscall (invalid_path)"
    ),
    "absolute_path": (
        "no HTTP: absolute keys and absolute container names are rejected "
        "before any filesystem syscall (invalid_path)"
    ),
    "symlink_escape": (
        "no HTTP: a symlink inside the root (as key segment or container) is "
        "rejected with path_escape; nothing is written to the link target"
    ),
    "interrupted_write": (
        "no HTTP: an injected copy failure aborts the private temp write and "
        "leaves no final object (no partial bytes published)"
    ),
    "default_switch_same_key": (
        "after the default root/namespace switch the frozen locator still "
        "addresses the object it was frozen for; the other root is untouched"
    ),
    "no_fallback_404": (
        "a provider miss returns not_found: the boundary never falls back to "
        "another root/driver"
    ),
    "no_fallback_403": (
        "a provider denial is surfaced as an error (garage 403 / disk EACCES): "
        "the boundary never falls back to another root/driver"
    ),
    "unknown_driver": (
        "a root whose driver is not implemented is refused at the boundary "
        "(unsupported_driver / upload_root_unavailable); no object is written"
    ),
    "held_put_finishes_after_expiry": (
        "a PUT dispatched before the persisted upload expiry still completes "
        "after it; the stored bytes are exactly the pinned body"
    ),
    "held_put_finishes_after_delete_attempt": (
        "a DELETE attempted while the per-identity OS lock is held fails "
        "closed (os_lock_failed) and cannot cross the in-flight PUT; the late "
        "PUT then lands, and garage refuses an expired late signature "
        "(400) instead of resurrecting a deleted object"
    ),
    "unbounded_drain_blocks": (
        "without a provable in-flight bound the capability is refused "
        "(invalid_signature_context) or the lock is unavailable "
        "(os_lock_failed); no reclamation success is recorded and the object "
        "is preserved"
    ),
    "undeclared_alias_sentinel": (
        "no HTTP: an undeclared namespace resolves to unknown and frozen "
        "location checks refuse it (location_changed)"
    ),
    "same_root_canonical_lock": (
        "no HTTP: declared aliases canonicalize to one root id/locator digest "
        "(one lock domain); a second root on the same physical locator is "
        "refused by registry validation"
    ),
    "locator_remap_rejected": (
        "no HTTP: remapping a root endpoint changes the locator digest, so a "
        "frozen digest is refused (location_changed)"
    ),
    "credential_rotation_same_digest": (
        "no HTTP: credential-only rotation leaves the locator digest unchanged "
        "(credentials are excluded from the locator record)"
    ),
}


def die(case_id, message):
    sys.stderr.write("adapter %s: %s\n" % (case_id, message))
    sys.exit(W4.EXIT_EVIDENCE)


def short(case_id):
    return SHORT[case_id]


def description(case_id, leaf):
    return "%s/%s/%s/%s" % (
        SHORT[case_id], leaf["driver"], leaf["ordering_or_fault"],
        leaf["domain"])


def match_observation(case_dir, leaf, case_id):
    entries = W3.read_observations(case_dir, case_id)
    want = (leaf["entrypoint"], leaf["ordering_or_fault"], leaf["domain"])
    hits = [
        entry for entry in entries
        if entry.get("case_id") == case_id
        and entry.get("entrypoint") == want[0]
        and entry.get("scenario") == want[1]
        and entry.get("domain") == want[2]
    ]
    if len(hits) > 1:
        # storage-switch/late-put share (entrypoint, scenario, domain) across
        # the two drivers; the measured driver disambiguates.
        hits = [
            entry for entry in hits
            if (entry.get("measured") or {}).get("driver") == leaf["driver"]
        ]
    if not hits:
        die(case_id, "no suite observation for %r (stream has %d events)"
            % (want, len(entries)))
    if len(hits) > 1:
        die(case_id, "%d duplicate suite observations for %r" % (len(hits), want))
    return hits[0]


def suite_health(evidence, parsed, marker, modules, log_names, case_id):
    problems = W4.suite_health(evidence, parsed, marker, modules, log_names)
    if problems:
        die(case_id, "suite run is not a clean measured run: %s"
            % "; ".join(problems))


def source_emit_assertion(source_path, source, leaf, case_id):
    scenario = leaf["ordering_or_fault"]
    block = W4.block(source, "leaf_unit(", case_id, "leaf_unit/2")
    emit_block = W4.block(source, "emit(", case_id, "emit/2")
    if "emit(Spec, Measured)" not in block["text"]:
        die(case_id, "%s:%d-%d does not emit the measured values of this unit"
            % (source_path, block["start"], block["end"]))
    if "attachment_lifecycle_v5_obs:emit" not in emit_block["text"]:
        die(case_id, "%s:%d-%d lacks the observation stream call"
            % (source_path, emit_block["start"], emit_block["end"]))
    literal = '<<"%s">>' % scenario
    if literal not in source:
        die(case_id, "suite source %s has no scenario literal %s"
            % (source_path, literal))
    line = source.count("\n", 0, source.index(literal)) + 1
    return W4.assertion(
        "suite_source_emits_scenario",
        "%s:%d-%d leaf_unit/2 emits the measured values via emit/2 "
        "(%s:%d-%d calls attachment_lifecycle_v5_obs:emit) after the asserts; "
        "scenario literal %s at line %d"
        % (os.path.basename(source_path), block["start"], block["end"],
           os.path.basename(source_path), emit_block["start"],
           emit_block["end"], json.dumps(scenario), line))


def measured_detail(obs):
    return json.dumps(obs.get("measured") or {}, sort_keys=True)


def bind(case_id, *, run_dir, case_dir, root, log_names, marker, modules,
         module, oracle_fn, source_rel=STORAGE_SRC):
    ctx = W4.context(run_dir, case_id)
    leaves = W4.leaves(ctx, case_id)
    evidence = W4.read_log(case_dir, log_names, case_id)
    if evidence is None:
        die(case_id, "missing case log (%s)" % ", ".join(log_names))
    parsed = W4.parse_verbose(evidence["text"])
    suite_health(evidence, parsed, marker, modules, log_names, case_id)
    source_path, source = W4.source(root, source_rel, case_id)
    pool = W4.resolve_pool(ctx, case_id)
    started, finished = W4.measurement_window(evidence, parsed, case_id)
    bucket, endpoint = W4.resolve_garage(ctx, case_id, require_env=False)
    disk_root = W4.resolve_disk_root(ctx, case_id)
    rows = []
    for leaf in leaves:
        leaf["_case"] = case_id
        desc = description(case_id, leaf)
        test = W4.find_test(parsed, module, desc=desc)
        if test is None:
            die(case_id, "no parsed eunit unit with description %r" % desc)
        obs = match_observation(case_dir, leaf, case_id)
        measured = obs.get("measured") or {}
        if measured.get("driver") != leaf["driver"]:
            die(case_id, "observation driver %r != leaf driver %r for %r"
                % (measured.get("driver"), leaf["driver"], desc))
        oracle, assertions = oracle_fn(
            leaf, obs, ctx, pool, bucket, disk_root, endpoint)
        if leaf["driver"] == "garage":
            actor = W4.garage_actor(bucket)
            resource_ids = [bucket, pool]
        elif leaf["driver"] == "none":
            actor = {
                "kind": "real",
                "driver": "none",
                "resource_kind": "scratch_postgres",
                "resource_id": pool,
            }
            resource_ids = [pool]
        else:
            actor = W4.disk_actor(disk_root)
            resource_ids = [disk_root, pool]
        request_ids = [
            "w403-%s-%s-%s" % (leaf["driver"], leaf["ordering_or_fault"],
                               obs.get("emitted_at_ms"))
        ]
        assertions = assertions + [
            W4.assertion(
                "named_eunit_unit_ok",
                "%s:%d %s" % (evidence["name"], test["line"],
                              W4.describe_test(test))),
            W4.assertion(
                "scenario_observation_emitted",
                "observations.jsonl:%d case=%s measured=%s emitted_at_ms=%s"
                % (obs["_line"], case_id, measured_detail(obs),
                   obs.get("emitted_at_ms"))),
            W4.assertion(
                "suite_summary_green",
                "%s: %s" % (evidence["name"], parsed["summary_raw"])),
            source_emit_assertion(source_path, source, leaf, case_id),
        ]
        rows.append(W3.build_observation(
            leaf, assertions=assertions, oracle=oracle,
            actor_provenance=actor, resource_ids=resource_ids,
            request_ids=request_ids,
            started_at=W4.base.iso(started), finished_at=W4.base.iso(finished)))
    W3.write_observations(case_dir, rows, leaves, case_id)
    sys.stdout.write("adapter %s: %d measured leaf row(s)\n"
                     % (case_id, len(rows)))


def storage_oracle(leaf, obs, ctx, pool, bucket, disk_root, endpoint):
    """Measured oracle row for one storage-boundary leaf."""
    measured = obs.get("measured") or {}
    scenario = leaf["ordering_or_fault"]
    driver = leaf["driver"]
    if driver == "garage":
        resource = "real garage bucket %s via attachment_storage_garage (endpoint %s)" % (
            bucket, endpoint or "W202_GARAGE_ENDPOINT")
        target = "object key %s in namespace %s (container %s)" % (
            measured.get("object_key", "?"), measured.get("namespace", "?"),
            measured.get("container", "?"))
    else:
        resource = "real disk driver rooted at %s (V5_DISK_ROOT, allowlisted)" % disk_root
        target = "disk object key %s in namespace %s (container %s)" % (
            measured.get("object_key", measured.get("symlink", "?")),
            measured.get("namespace", "?"), measured.get("container", "?"))
    digest = measured.get("bytes_sha256") or measured.get("digest_after") or ""
    oracle = {
        "pre_db_state": (
            "no PostgreSQL state is involved in this storage-boundary leaf "
            "(the attachment/session rows are covered by the sibling cases); "
            "scratch pool %s is the run-owned database the case executor "
            "opened" % pool),
        "pre_object_hash": (
            "sha256=%s measured by the suite for %s; ranged/short reads are "
            "compared byte-for-byte against the same body" % (digest or "n/a", target)),
        "action": (
            "direct erl eunit unit %r -> %s; measured=%s"
            % (description(leaf["_case"], leaf), resource, measured_detail(obs))),
        "expected_http_semantics": HTTP_TEXT[scenario],
        "expected_db_delta": (
            "no database delta: the leaf adjudicates the storage boundary; "
            "no attachment/session row is created or mutated"),
        "expected_object_state": (
            "the suite asserts the exact post-condition recorded in measured "
            "(hash/size/status/error); no partial or fallback object remains"),
        "proof_actor": (
            "%s; suite observations are appended by the same eunit VM after "
            "the asserts (emitted_at_ms=%s) and the adapter matches "
            "case/entrypoint/scenario/domain/driver uniquely"
            % (resource, obs.get("emitted_at_ms"))),
    }
    assertions = [
        W4.assertion("driver_provenance", resource),
        W4.assertion("measured_object_state", measured_detail(obs)),
    ]
    return oracle, assertions
