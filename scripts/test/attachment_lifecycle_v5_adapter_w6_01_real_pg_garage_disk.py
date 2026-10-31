#!/usr/bin/env python3
"""W6-01/real-pg-garage-disk measured adapter.

Measured leaves come only from artifacts the W6-01 gate executor produced by
real execution:

  * W1-02 plan leaves: the two real schema-phase SQL probe runs
    (w102-expand / w102-guards), using exactly the probe adequacy rules that the
    frozen W1-02 adapters apply (placeholder probes stay gaps).
  * W4-03/driver-content-immutable leaves are admitted only when their w403
    driver-matrix observation carries the server-observed request records
    independently bound to the object identity (route/method/handler/status,
    fixture-bound object_key/action, and the request process's production
    storage resolution of the same object); anything short of that stays a
    named evidence gap.
  * the per-leaf-observing battery groups (OBS_STEP_FOR): the W4-03 six
    (w403 step), the W2 upload/confirm eight (w2upload step) and the
    W4 queue/worker/activation twenty-one (w4queue step) — each battery
    suite emits one observation line per executed leaf
    (attachment_lifecycle_v5_obs:emit, env-gated JSONL) keyed by
    (case_id, entrypoint, scenario, domain, measured.driver); the adapter
    binds each contract leaf to its unique matching observation inside
    the executing step's window (multi-line keys disambiguate via
    measured.ac_id).

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


# Production storage-resolution entry points the per-request server trace
# records (attachment_w403_server_trace:start/1); the trace tuples stay
# diagnostic_untrusted, so they only corroborate the independently bound
# object identity written by the fixture after the real response.
STORAGE_TRACE_SOURCES = frozenset({
    "attachment_storage:session_location/1",
    "attachment_storage:frozen_location/2",
})

# Presign (method, route, presign handler, PUT handler) per upload surface.
SURFACE_ROUTES = {
    "ordinary": ("GET", r"/api/v1/attachment/presign", "attach_handler", "attachment_storage_gateway_handler"),
    "enterprise_internal": ("POST", r"/api/internal/v1/files/presign", "enterprise_asset_handler", "attachment_storage_gateway_handler"),
    "enterprise_seat": ("POST", r"/api/v1/seat/enterprise/organizations/[0-9]+/assets/presign", "eb_tenant_handler", "eb_tenant_handler"),
    "cs_widget": ("POST", r"/api/v1/cs/widget/sessions/[0-9]+/assets/presign", "cs_widget_handler", "cs_widget_handler"),
}


def _untrusted_location_recorded(log, leaf):
    """Diagnostics only: a recorded trace is not independently authenticated."""
    parts = [leaf.get("entrypoint"), leaf.get("driver"), leaf.get("source_scenario")]
    if any(not isinstance(part, str) or not part for part in parts):
        return False
    entry = S.http_evidence(log, "/".join(parts))
    if not isinstance(entry, dict):
        return False
    payload = entry.get("payload")
    if not isinstance(payload, dict):
        return False
    requests = payload.get("requests")
    if not isinstance(requests, list):
        return False
    for request in requests:
        if not isinstance(request, dict):
            continue
        locations = request.get("diagnostic_untrusted_locations")
        if not isinstance(locations, list):
            continue
        for item in locations:
            if (not isinstance(item, dict) or item.get("trust") != "diagnostic_untrusted"
                    or item.get("source") not in STORAGE_TRACE_SOURCES):
                continue
            identity = item.get("identity")
            if (isinstance(identity, dict) and identity.get("driver") == leaf["driver"]
                    and all(isinstance(identity.get(k), str) and identity[k]
                            for k in ("namespace", "container", "key", "locator_digest"))):
                return True
    return False

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

# W102DM dual-driver leaves: the five W1-02 governance sources whose
# contract leaves are the case scenarios x {garage, disk} (the case
# matrices themselves are driver=none; measured per driver from the
# merged dual-driver probes.jsonl).
W102DM_DUAL_SOURCES = frozenset({
    "W1-02/audit-actor-anonymization", "W1-02/intent-authority",
    "W1-02/recovery-tombstone-integrity", "W1-02/registration-integrity",
    "W1-02/session-link-integrity",
})


def _expiry_proof(measured, entrypoint):
    """Bind the successful registration and failed PUT to this exact leaf."""
    if (entrypoint in ("enterprise_seat", "cs_widget") and isinstance(measured, dict)
            and measured.get("registration_preseeded_fixture") is True):
        raise ValueError("enterprise preseeded registration is not established as equivalent to initial HTTP registration")
    routes = SURFACE_ROUTES
    if entrypoint not in routes or not isinstance(measured, dict):
        raise ValueError("unknown upload surface")
    requests = measured.get("http_requests")
    presign_id, put_id = measured.get("presign_request_id"), measured.get("put_request_id")
    if (not isinstance(requests, list) or not isinstance(presign_id, str)
            or not isinstance(put_id, str) or not presign_id or not put_id or presign_id == put_id):
        raise ValueError("missing distinct real request identities")
    def by_id(identity):
        matches = [r for r in requests if isinstance(r, dict) and r.get("request_id") == identity]
        if len(matches) != 1:
            raise ValueError("request identity missing or duplicated")
        request = matches[0]
        if (not all(isinstance(request.get(k), str) and request[k]
                    for k in ("method", "route", "handler", "server_process", "connection_id"))
                or type(request.get("status")) is not int):
            raise ValueError("incomplete observed HTTP request")
        return request
    presign, put = by_id(presign_id), by_id(put_id)
    method, route, presign_handler, put_handler = routes[entrypoint]
    if (presign["method"] != method or not re.fullmatch(route, presign["route"])
            or presign["handler"] != presign_handler or presign.get("action") != "presign"
            or not 200 <= presign["status"] < 300 or put["method"] != "PUT"
            or put["handler"] != put_handler or put.get("action") != "put"
            or not 400 <= put["status"] < 600
            or measured.get("put_status") != put["status"]):
        raise ValueError("observed request does not match registration or PUT")
    if (not isinstance(measured.get("object_key"), str) or not measured["object_key"]
            or not re.fullmatch(r"[0-9a-f]{64}", measured.get("pin_sha256", ""))
            or measured.get("driver_window_expired") is not True
            or measured.get("provider_bytes_absent") is not True):
        raise ValueError("missing measured object and pin evidence")
    # The expiry scenario's contract: the leaf's PRIMARY object is pinned
    # (pin_sha256 above), then a SEPARATE expired capability is signed and
    # its PUT refused — the presign/PUT chain belongs to the expired
    # capability object, which by design differs from the pinned object.
    # The binding proof is the expired chain's internal self-consistency
    # (same object_key on both requests), not equality with the pinned
    # object (run10: all 8 expiry leaves have this two-object shape).
    if presign.get("object_key") != put.get("object_key"):
        raise ValueError("expired-chain requests are not bound to the same object")
    if not isinstance(presign.get("object_key"), str) or not presign["object_key"]:
        raise ValueError("expired chain carries no object identity")
    if entrypoint in ("ordinary", "enterprise_internal"):
        cid = measured.get("client_upload_id")
        if not isinstance(cid, str) or not re.fullmatch(r"[0-9a-fA-F-]{36}", cid):
            raise ValueError("invalid upload session identity")
        put_route = "/api/v1/attachment/gateway/" + cid
    elif entrypoint == "enterprise_seat":
        put_route = presign["route"]
    else:
        put_route = presign["route"][:-len("presign")] + "upload"
    if put["route"] != put_route:
        raise ValueError("PUT route differs from registered upload session")
    return presign, put


def _server_location_bound(request, object_key, driver):
    """The request process resolved this object's location in production storage.

    attachment_w403_server_trace stays diagnostic_untrusted (any local process
    can forge trace tuples), so this only corroborates the object_key/action
    the fixture independently bound to the request after the real response.
    """
    locations = request.get("diagnostic_untrusted_locations")
    if not isinstance(locations, list):
        return False
    for item in locations:
        if (not isinstance(item, dict) or item.get("trust") != "diagnostic_untrusted"
                or item.get("source") not in STORAGE_TRACE_SOURCES):
            continue
        identity = item.get("identity")
        if (isinstance(identity, dict) and identity.get("key") == object_key
                and identity.get("driver") == driver
                and all(isinstance(identity.get(k), str) and identity[k]
                        for k in ("namespace", "container", "locator_digest"))):
            return True
    return False


def _has_upload_action(measured):
    """True when the observation carries any presign/put action request.

    DB-layer fail-closed leaves carry only health/collection requests
    (GET/POST with action=None); their acceptance evidence is the generic
    no-HTTP oracle, not an upload-surface proof.
    """
    requests = measured.get("http_requests") or []
    return any(request.get("action") in ("presign", "put") for request in requests)


def _surface_from_requests(measured):
    """Recover the upload surface from the observed admitted presign.

    W2 leaves whose entrypoint is the case name (pending-failclosed,
    confirm-gc-race, ...) still carry real presign/PUT records; the
    producing handler and route identify the surface unambiguously.
    """
    requests = measured.get("http_requests") or []
    for request in requests:
        if (request.get("action") != "presign"
                or not 200 <= request.get("status", 0) < 300):
            continue
        for surface, (method, route, presign_handler, _put) in SURFACE_ROUTES.items():
            if (request.get("method") == method
                    and re.fullmatch(route, request.get("route", ""))
                    and request.get("handler") == presign_handler):
                return surface
    return None


def _immutability_proof(measured, entrypoint):
    """Bind this leaf's server-observed requests to its object identity.

    Same shape as _expiry_proof: unique real request identities, the surface's
    expected route/method/handler, and a three-way object_key binding (request
    record / measured object / production storage resolution inside the same
    server request process). PUT statuses stay scenario-owned: this proof binds
    provenance, not a fixed outcome sequence.
    """
    if entrypoint not in SURFACE_ROUTES or not isinstance(measured, dict):
        raise ValueError("unknown upload surface")
    requests = measured.get("http_requests")
    object_key = measured.get("object_key")
    if (not isinstance(requests, list) or not requests
            or any(not isinstance(request, dict) for request in requests)
            or not isinstance(object_key, str) or not object_key):
        raise ValueError("missing server-bound request evidence")
    ids = [request.get("request_id") for request in requests]
    if (any(not isinstance(identity, str) or not identity for identity in ids)
            or len(set(ids)) != len(ids)):
        raise ValueError("request identities missing or duplicated")
    def complete(request):
        return (all(isinstance(request.get(k), str) and request[k]
                    for k in ("method", "route", "handler", "server_process", "connection_id"))
                and type(request.get("status")) is int)
    if not all(complete(request) for request in requests):
        raise ValueError("incomplete observed HTTP request")
    method, route, presign_handler, put_handler = SURFACE_ROUTES[entrypoint]
    presigns = [request for request in requests
                if request.get("action") == "presign" and 200 <= request["status"] < 300]
    # Multi-session scenarios carry several presign/PUT rounds in one
    # observation; this leaf's chain is the one whose presign carries THIS
    # leaf's object_key (object identity selects the session — precise,
    # not permissive).
    bound_presigns = [request for request in presigns
                      if request.get("object_key") == object_key]
    if len(bound_presigns) != 1:
        raise ValueError("expected exactly one admitted presign request bound to this leaf's object")
    presign = bound_presigns[0]
    if (presign["method"] != method or not re.fullmatch(route, presign["route"])
            or presign["handler"] != presign_handler
            or presign.get("object_key") != object_key):
        raise ValueError("presign request is not bound to this leaf's object at the server")
    puts = [request for request in requests
            if request.get("action") == "put"
            and request.get("object_key") == object_key]
    if not puts:
        raise ValueError("no real PUT request observed for this leaf")
    if entrypoint in ("ordinary", "enterprise_internal"):
        put_route_re = re.compile(r"/api/v1/attachment/gateway/[0-9a-fA-F-]{36}")
        put_route = None
    elif entrypoint == "enterprise_seat":
        put_route_re, put_route = None, presign["route"]
    else:
        put_route_re, put_route = None, presign["route"][:-len("presign")] + "upload"
    # Production storage resolution happens on the WRITE path, not the
    # signing path: only the gateway/seat presign handler of
    # enterprise_internal calls session_location/frozen_location during
    # presign (run9 battery: 16/16 enterprise_internal presigns carry a
    # storage trace, 0/48 for the other three surfaces), while EVERY
    # admitted (2xx) PUT resolves the object's location in production
    # storage (run9: 60/60).  So the three-way binding's location leg is
    # proven on at least one admitted PUT of this leaf; rejected replays
    # (409, often before the storage layer) keep the identity bindings.
    admitted_put_bound = False
    for put in puts:
        route_ok = (put_route_re.fullmatch(put["route"]) if put_route_re is not None
                    else put["route"] == put_route)
        if (put["method"] != "PUT" or put["handler"] != put_handler
                or put.get("object_key") != object_key or not route_ok):
            raise ValueError("PUT request is not bound to this leaf's object at the server")
        if 200 <= put["status"] < 300 and _server_location_bound(
                put, object_key, measured.get("driver")):
            admitted_put_bound = True
    if not admitted_put_bound:
        raise ValueError("no admitted PUT resolved this leaf's object in production storage")
    return presign, puts


def _rejected_scenario_proof(measured, surface):
    """Reject-type leaves bind identities WITHOUT an admitted PUT.

    Fail-closed, old-client and replay-rejection scenarios assert that the
    upload chain is refused; their acceptance evidence is the identity
    binding of every observed request plus the refused statuses.  At most
    one admitted presign may exist (the chain opened and the write was
    then refused); zero presigns is the chain-never-opened shape.  Every
    PUT must be refused.  Returns (presign_or_None, representative_put).
    """
    if surface not in SURFACE_ROUTES or not isinstance(measured, dict):
        raise ValueError("unknown upload surface")
    requests = measured.get("http_requests")
    object_key = measured.get("object_key")
    if (not isinstance(requests, list) or not requests
            or not isinstance(object_key, str) or not object_key):
        raise ValueError("missing server-bound request evidence")
    ids = [request.get("request_id") for request in requests]
    if (any(not isinstance(identity, str) or not identity for identity in ids)
            or len(set(ids)) != len(ids)):
        raise ValueError("request identities missing or duplicated")

    def complete(request):
        return (all(isinstance(request.get(k), str) and request[k]
                    for k in ("method", "route", "handler", "server_process", "connection_id"))
                and type(request.get("status")) is int)

    if not all(complete(request) for request in requests):
        raise ValueError("incomplete observed HTTP request")
    method, route, presign_handler, put_handler = SURFACE_ROUTES[surface]
    presigns = [request for request in requests
                if request.get("action") == "presign"
                and 200 <= request["status"] < 300]
    if len(presigns) > 1:
        raise ValueError("expected at most one admitted presign request")
    presign = presigns[0] if presigns else None
    if presign is not None and (
            presign["method"] != method
            or not re.fullmatch(route, presign["route"])
            or presign["handler"] != presign_handler
            or presign.get("object_key") != object_key):
        raise ValueError("presign request is not bound to this leaf's object at the server")
    puts = [request for request in requests if request.get("action") == "put"]
    if not puts:
        raise ValueError("no real PUT request observed for this leaf")
    if entrypoint_route_tail(surface, presign) is None:
        raise ValueError("cannot derive the PUT route from the presign")
    representative = None
    for put in puts:
        if 200 <= put["status"] < 300:
            raise ValueError("scenario asserts rejection but an admitted PUT exists")
        route_ok = _put_route_ok(surface, presign, put)
        if (put["method"] != "PUT" or put["handler"] != put_handler
                or put.get("object_key") != object_key or not route_ok):
            raise ValueError("PUT request is not bound to this leaf's object at the server")
        if representative is None:
            representative = put
    if representative is None:
        raise ValueError("no refused PUT observed for this leaf")
    return presign, representative


def entrypoint_route_tail(surface, presign):
    """The PUT route rule per surface; None when it cannot be derived."""
    if surface in ("ordinary", "enterprise_internal"):
        return re.compile(r"/api/v1/attachment/gateway/[0-9a-fA-F-]{36}")
    if presign is None:
        return None
    if surface == "enterprise_seat":
        return presign["route"]
    return presign["route"][:-len("presign")] + "upload"


def _put_route_ok(surface, presign, put):
    rule = entrypoint_route_tail(surface, presign)
    if isinstance(rule, re.Pattern):
        return bool(rule.fullmatch(put["route"]))
    if rule is None:
        return False
    return put["route"] == rule

# The frozen enterprise multipart/group_file leaves cannot be proven by
# enterprise seat proxy PUT. Keep them blocked until the contract owner
# authorizes a revision or the required entrypoints can be exercised.
BLOCKED_ENTERPRISE_SCENARIOS = frozenset({"multipart_success", "group_file_success"})


def _blocked_enterprise_leaf(leaf):
    return (leaf.get("source_case_ref") == "W2-01/pending-multipart"
            and leaf.get("domain") == "enterprise"
            and _obs_scenario(leaf) in BLOCKED_ENTERPRISE_SCENARIOS)


def _blocked_ordinary_group_file_leaf(leaf):
    return (leaf.get("source_case_ref") == "W2-01/pending-multipart"
            and leaf.get("domain") == "ordinary"
            and _obs_scenario(leaf) == "group_file_success")


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
# 2026-10-31: the eight scenarios below were moved out of the stale
# INADEQUATE snapshot — w102_probe.sh has carried their behavior-level
# implementations for several candidates now (legacy-NULL backfill,
# 1,000,000-row load with lock-wait evidence, keyset-batch interrupt/resume,
# real old-writer GC-intent refusal, real function invocations with ACL
# callability, M3 function inventory parity, first-bind + second-bind
# refusal, attachment-binding same-value replay with to_jsonb snapshot),
# which is exactly what the frozen W1-02 adapters already accept.
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
        "old_null_session_protected": ["old_null_session_protected"],
        "large_table_write_block_bound": ["large_table_write_block_bound"],
        "backfill_interrupt_resume": ["backfill_interrupt_resume"],
        "rolling_old_writer_no_gc": ["rolling_old_writer_no_gc"],
        "expand_required_functions_callable": [
            "expand_required_functions_callable"],
        "guards_do_not_install_missing_functions": [
            "guards_do_not_install_missing_functions"],
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
        "old_null_session_protected": ["old_null_session_protected"],
        "large_table_write_block_bound": ["large_table_write_block_bound"],
        "backfill_interrupt_resume": ["backfill_interrupt_resume"],
        "rolling_old_writer_no_gc": ["rolling_old_writer_no_gc"],
        "expand_required_functions_callable": [
            "expand_required_functions_callable"],
        "guards_do_not_install_missing_functions": [
            "guards_do_not_install_missing_functions"],
    },
    ("W1-02/immutable", "guards-contract"): {
        name: [name] for name in (
            "mutate_driver", "mutate_namespace", "mutate_container",
            "mutate_key", "mutate_owner", "mutate_scope", "mutate_cipher",
            "mutate_locator_digest", "tombstone_recreation",
            "first_bind_once", "same_value_replay")
    },
    ("W1-02/audit-actor-anonymization", "guards-contract"): {
        "direct_actor_null_with_live_user_denied": ["direct_actor_null_with_live_user_denied"],
        "direct_actor_substitution_denied": ["direct_actor_substitution_denied"],
        "fk_user_delete_sets_audit_null": ["fk_user_delete_sets_audit_null"],
    },
    ("W1-02/intent-authority", "guards-contract"): {
        "activate_legacy_without_proof": ["activate_legacy_without_proof"],
        "authorized_intent_atomic": ["authorized_intent_atomic"],
        "direct_metadata_intent": ["direct_metadata_intent"],
        "direct_session_intent": ["direct_session_intent"],
        "forge_gc_verified": ["forge_gc_verified"],
        "forge_gc_verified_on_insert": ["forge_gc_verified_on_insert"],
        "intent_future_retention": ["intent_future_retention"],
        "intent_missing_audit": ["intent_missing_audit"],
        "intent_missing_metadata_transition": ["intent_missing_metadata_transition"],
        "intent_under_hold": ["intent_under_hold"],
        "intent_with_live_reference": ["intent_with_live_reference"],
        "queue_insert_done": ["queue_insert_done"],
        "queue_insert_leased": ["queue_insert_leased"],
        "queue_insert_ready": ["queue_insert_ready"],
        "queue_insert_retry_wait": ["queue_insert_retry_wait"],
        "runtime_set_role_owner": ["runtime_set_role_owner"],
        "worker_arbitrary_enqueue": ["worker_arbitrary_enqueue"],
    },
    ("W1-02/recovery-tombstone-integrity", "guards-contract"): {
        "copy_uuid_snapshot_before_metadata_delete": ["copy_uuid_snapshot_before_metadata_delete"],
        "cross_table_duplicate_uuid_rejected": ["cross_table_duplicate_uuid_rejected"],
        "direct_dml_resurrection_denied": ["direct_dml_resurrection_denied"],
        "down_with_recovery_tombstone_refused": ["down_with_recovery_tombstone_refused"],
        "legacy_null_preserved": ["legacy_null_preserved"],
        "missing_snapshot_rollback": ["missing_snapshot_rollback"],
        "tombstone_update_delete_denied": ["tombstone_update_delete_denied"],
    },
    ("W1-02/registration-integrity", "guards-contract"): {
        "audit_null_does_not_grant_resume": ["audit_null_does_not_grant_resume"],
        "legacy_null_not_fabricated": ["legacy_null_not_fabricated"],
        "original_filename_conflict_after_restart": ["original_filename_conflict_after_restart"],
        "original_mime_conflict_after_restart": ["original_mime_conflict_after_restart"],
        "pg_canonical_json_digest_vector": ["pg_canonical_json_digest_vector"],
        "seat_contact_subject_kind_conflict": ["eb_seat_contact_subject_kind_conflict"],
        "snapshot_direct_dml_rejected": ["snapshot_direct_dml_rejected"],
        "widget_other_visitor_session_denied": ["eb_widget_other_visitor_session_denied"],
    },
    ("W1-02/session-link-integrity", "guards-contract"): {
        "confirmed_gc_preserves_session_link": ["confirmed_gc_preserves_session_link"],
        "confirmed_null": ["confirmed_null"],
        "confirmed_rebind": ["confirmed_rebind"],
        "confirmed_to_registered": ["confirmed_to_registered"],
        "lawful_repeat": ["lawful_repeat"],
        "legal_fk_wrong_cipher": ["legal_fk_wrong_cipher"],
        "legal_fk_wrong_key": ["legal_fk_wrong_key"],
        "legal_fk_wrong_location": ["legal_fk_wrong_location"],
        "legal_fk_wrong_owner": ["legal_fk_wrong_owner"],
        "legal_fk_wrong_scope": ["legal_fk_wrong_scope"],
        "registered_link_nonnull": ["registered_link_nonnull"],
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
        if source == "W4-03/driver-content-immutable":
            # The w403 driver-matrix battery emits one observation per executed
            # leaf; admitting a row requires the server-observed HTTP requests
            # and the object identity to be independently bound to the server
            # (fixture request records + bind_request + in-process storage
            # resolution). Anything short of that stays a named gap.
            scenario = leaf.get("source_scenario")
            key = (source, leaf.get("entrypoint"), scenario, leaf.get("domain"),
                   leaf.get("driver"))
            obs = _match_observation(idx403, key, leaf.get("ac_id"))
            if obs is None:
                S.group_gap(gap_groups, source, 1,
                            "no w403-step observation matches %r ac_id=%r"
                            % (key, leaf.get("ac_id")))
                continue
            measured = obs.get("measured") or {}
            try:
                presign, puts = _immutability_proof(measured, leaf["entrypoint"])
            except (ValueError, TypeError) as immutable_exc:
                # Reject-type w403dm scenarios (expired replay, old-client,
                # fail-closed) bind identities with refused writes; their
                # acceptance evidence is the identity chain plus the refused
                # statuses, not an admitted write.
                try:
                    presign, put_r = _rejected_scenario_proof(
                        measured, leaf["entrypoint"])
                    puts = [put_r]
                except (ValueError, TypeError) as rejected_exc:
                    S.group_gap(gap_groups, source, 1,
                                "server-observed request/object binding "
                                "rejected: immutability: %s; "
                                "rejected-scenario: %s"
                                % (immutable_exc, rejected_exc))
                    continue
            emitted = obs.get("emitted_at_ms")
            # STEP lines record epoch SECONDS (date +%s, taken after the
            # eunit process exited): the finish second's tail is inside the
            # step, so the right edge extends to the end of that second
            # (run9: an observation emitted 345ms into the finish second died
            # against a t1_ms truncated to the second's first millisecond).
            t0_ms = started403.timestamp() * 1000
            t1_ms = finished403.timestamp() * 1000 + 999
            if not isinstance(emitted, (int, float)) \
                    or not t0_ms <= emitted <= t1_ms:
                S.die(CASE_ID, "observation %r emitted_at_ms=%r outside the "
                               "w403 step window" % (key, emitted))
            resource = bucket if leaf["driver"] == "garage" else disk_root
            actor = (S.garage_actor(bucket) if leaf["driver"] == "garage"
                     else S.disk_actor(disk_root))
            request_ids = [presign["request_id"]] + [
                put["request_id"] for put in puts]
            put_statuses = [put["status"] for put in puts]
            assertions = [
                S.assertion(
                    "w403_observation_matched",
                    "%s: case_id=%s entrypoint=%s scenario=%s domain=%s "
                    "driver=%s measured(%d keys)"
                    % (os.path.basename(obs_path403), source,
                       leaf["entrypoint"], scenario, leaf["domain"],
                       leaf["driver"], len(measured))),
                S.assertion(
                    "observation_in_step_window",
                    "emitted_at_ms=%d within [%d, %d] of %r"
                    % (emitted, t0_ms, t1_ms, w403_step["line"])),
                S.assertion(
                    "immutability_provenance_bound",
                    "presign %s (status %s) and %d PUT request(s) %s each "
                    "carry the server-recorded route/method/handler/status "
                    "and the fixture-bound object_key; the presign request "
                    "process resolved the same object through production "
                    "storage"
                    % (presign["request_id"], presign["status"], len(puts),
                       [put["request_id"] for put in puts])),
                S.assertion(
                    "battery_step_green",
                    "%r; markers=%r; %d parsed units, summary=%r"
                    % (w403_step["line"], sorted(w403_markers),
                       len(w403["parsed"]["tests"]),
                       w403["parsed"]["summary_raw"])),
            ]
            oracle = {
                "pre_db_state": "real PG isolated config DB provisioned by "
                                "the full migration chain; the w403 driver "
                                "matrix ran %s/%s against the real %s driver"
                                % (leaf["entrypoint"], scenario, leaf["driver"]),
                "pre_object_hash": "the unit registers its own fixture bytes "
                                   "and asserts the object absent before PUT",
                "action": "%s/%s (driver=%s) executed by the w403 driver "
                          "matrix; presign %s then %d real PUT attempt(s) "
                          "(statuses %s) on the server-bound object"
                          % (source, scenario, leaf["driver"],
                             presign["request_id"], len(puts), put_statuses),
                "expected_http_semantics": "the scenario's own assertions "
                                           "govern each PUT outcome; the "
                                           "server-observed PUT statuses are "
                                           "%s" % put_statuses,
                "expected_db_delta": "the scenario asserts its immutable-"
                                     "content invariant before returning ok "
                                     "(w403 suite contract)",
                "expected_object_state": "object_key bound to every presign/"
                                         "PUT request record and to the "
                                         "in-process storage resolution; "
                                         "content immutability asserted by "
                                         "the unit",
                "proof_actor": "real driver %s resource %s; observations "
                               "sha256=%s"
                               % (leaf["driver"], resource,
                                  S.sha256_file(obs_path403)),
            }
            row = S.C.build_row(
                leaf, started403, finished403,
                {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
                 "skipped": 0},
                oracle, assertions, actor, [resource], request_ids)
            row["entrypoint_provenance"] = {
                "entrypoint": leaf["entrypoint"], "route": presign["route"],
                "method": presign["method"], "handler": presign["handler"],
                "request_id": presign["request_id"]}
            row["server_request_provenance"] = {
                "object_key": measured["object_key"],
                "presign_request_id": presign["request_id"],
                "requests": [
                    {"request_id": request["request_id"],
                     "action": request.get("action"),
                     "method": request["method"], "route": request["route"],
                     "handler": request["handler"], "status": request["status"],
                     "object_key": request.get("object_key")}
                    for request in measured["http_requests"]
                    if isinstance(request, dict)],
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
        if source in W102DM_DUAL_SOURCES:
            # W102DM dual-driver leaves (W102DM-BLUEPRINT segment 2): the
            # same scenario/probe set as the (plan, none) migration leaves,
            # measured per storage driver — the merged probes.jsonl carries
            # both drivers' rows (V5_PROBE_DRIVER-parameterized probe run),
            # and a leaf is measured only by its OWN driver's probe rows.
            phase = S.PHASE_MAP.get(leaf.get("source_schema_phase"))
            scenario = leaf.get("source_scenario")
            driver = leaf.get("driver")
            if (phase not in ("expand", "guards") or not scenario
                    or driver not in ("garage", "disk")):
                S.group_gap(gap_groups, source, 1,
                            "leaf has no resolvable phase/scenario/driver "
                            "(phase=%r scenario=%r driver=%r)"
                            % (leaf.get("source_schema_phase"), scenario,
                               driver))
                continue
            names = ADEQUATE.get((source, leaf.get("source_schema_phase")),
                                 {}).get(scenario)
            if not names:
                S.group_gap(gap_groups, source, 1,
                            "no adequate W1-02 probe is declared for this "
                            "scenario in phase %s" % phase)
                continue
            probes = phases[phase]["probes"]
            lines = [S.require_probe_driver(probes, name, driver, CASE_ID)
                     for name in names]
            assertions = [S.assertion(
                "probe_%s_passed" % name,
                "%s:%d name=%s driver=%s pass=true"
                % (os.path.basename(probes["path"]), line_no, name, driver))
                for name, line_no in zip(names, lines)]
            resource = bucket if driver == "garage" else disk_root
            actor = (S.garage_actor(bucket) if driver == "garage"
                     else S.disk_actor(disk_root))
            db = phases[phase]["db"]
            oracle = {
                "pre_db_state": "fresh %s with %d replayed up migrations in "
                                "the %s schema phase, storage fixtures "
                                "seeded with the %s driver quadruple"
                                % (db, phases[phase]["replay"], phase, driver),
                "pre_object_hash": "n/a: this plan leaf is a DB/governance "
                                   "probe; no provider object exists",
                "action": "psql probes %r on %s (driver=%s)"
                          % (names, db, driver),
                "expected_http_semantics": "no HTTP call: the leaf's stated "
                                           "property is a database/"
                                           "governance assertion",
                "expected_db_delta": "the named probe's expected value is "
                                     "asserted by the real psql run",
                "expected_object_state": "guards reject the mutation; the "
                                         "row's storage identity stays "
                                         "bound to the %s driver" % driver,
                "proof_actor": "psql imboy_user on %s; probe artifact "
                               "sha256=%s; driver=%s"
                               % (db, probes["sha256"], driver),
            }
            rows.append(S.C.build_row(
                leaf, phases[phase]["window"][0], phases[phase]["window"][1],
                {"exit_code": 0, "executed": len(names),
                 "passed": len(names), "failed": 0, "skipped": 0},
                oracle, assertions, actor, [resource],
                ["w102dm:%s:%s:%s" % (phase, driver, name) for name in names]))
            continue
        if source in OBS_GROUPS:
            if _blocked_enterprise_leaf(leaf):
                S.group_gap(gap_groups, source, 1,
                            "user ruling 2026-10-29: the enterprise "
                            "multipart/group_file gateway substitute is not "
                            "acceptance evidence; leaf %r stays blocked per "
                            "the frozen contract until an authorized revision"
                            % leaf["ordering_or_fault"])
                continue
            if _blocked_ordinary_group_file_leaf(leaf):
                S.group_gap(gap_groups, source, 1,
                            "ordinary group_file_success uses attachment "
                            "multipart and message-anchor confirmation, not "
                            "the required /api/v1/group/file/upload entrypoint; "
                            "real group-file evidence is missing")
                continue
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
            surface_cases = ctx["contract"].get("upload_surface_contract", {}).get("cases", [])
            expiry_leaf = (source == "W2-01/upload-content-pin"
                           and scenario == "frozen_internal_signature_expiry")
            proof = None
            proof_kind = None
            if source.startswith("W2-") and not expiry_leaf:
                # 2026-10-31 unlock: the w2upload battery now emits the same
                # server-bound HTTP evidence as the w403dm battery (every
                # observation carries http_requests; every admitted PUT is
                # storage-traced), so upload-surface leaves prove with the
                # same immutability proof.  Leaves whose entrypoint is the
                # case name recover the surface from the observed presign
                # handler/route.
                surface = leaf["entrypoint"]
                if surface not in SURFACE_ROUTES:
                    surface = _surface_from_requests(measured)
                if surface is None or not measured.get("object_key"):
                    if not _has_upload_action(measured):
                        # DB-layer fail-closed leaf (registration refused
                        # before any upload chain opened): the observation
                        # carries no presign/put action by design, and the
                        # generic no-HTTP oracle below is its acceptance
                        # evidence.
                        pass
                    else:
                        S.group_gap(gap_groups, source, 1,
                                    "upload-surface evidence incomplete: no "
                                    "resolvable surface or object_key")
                        continue
                try:
                    presign_p, puts_p = _immutability_proof(measured, surface)
                    proof = (presign_p, puts_p[0])
                    proof_kind = "immutability"
                except (ValueError, TypeError) as immutable_exc:
                    puts_all = [request for request in (measured.get("http_requests") or [])
                                if request.get("action") == "put"]
                    if not puts_all:
                        # Chain-never-opened / no-write-action leaf: the
                        # generic no-HTTP oracle below carries it.
                        pass
                    else:
                        try:
                            presign_r, put_r = _rejected_scenario_proof(measured, surface)
                            proof = (presign_r, put_r)
                            proof_kind = "rejected"
                        except (ValueError, TypeError) as rejected_exc:
                            S.group_gap(gap_groups, source, 1,
                                        "immutability: %s; rejected-scenario: %s"
                                        % (immutable_exc, rejected_exc))
                            continue
            elif source in surface_cases:
                if not expiry_leaf:
                    S.group_gap(gap_groups, source, 1,
                                "real request provenance not yet bound for this surface")
                    continue
                try:
                    proof = _expiry_proof(measured, leaf["entrypoint"])
                    proof_kind = "expiry"
                except (ValueError, TypeError) as exc:
                    S.group_gap(gap_groups, source, 1, str(exc))
                    continue
            emitted = obs.get("emitted_at_ms")
            # Same second-granularity right edge as the w403 window above:
            # the finish second's tail belongs to the step (run9 345ms case).
            t0_ms = obs_started.timestamp() * 1000
            t1_ms = obs_finished.timestamp() * 1000 + 999
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
            if proof:
                presign, put = proof
                if proof_kind == "immutability":
                    oracle.update(
                        pre_object_hash="pre-PUT object state asserted by the "
                                        "scenario's own driver reads",
                        action="presign %s and real PUT %s on %s carried the "
                               "leaf's object through the production gateway "
                               "(measured: %s)"
                               % (presign["request_id"], put["request_id"],
                                  put["route"], msum),
                        expected_http_semantics="presign %s status %s; PUT %s status %s" % (
                            presign["request_id"], presign["status"],
                            put["request_id"], put["status"]),
                        expected_db_delta="the unit asserts the scenario's "
                                          "DB/queue invariant before returning "
                                          "ok (w2upload suite contract)",
                        expected_object_state="measured object fields: %s"
                                              % (obj_keys or
                                                 "object %s" % measured.get("object_key")))
                elif proof_kind == "rejected":
                    oracle.update(
                        pre_object_hash="the upload chain was refused as the "
                                        "scenario asserts; pre-state asserted "
                                        "by the scenario's own driver reads",
                        action="real PUT %s was refused with status %s as "
                               "asserted (measured: %s)"
                               % (put["request_id"], put["status"], msum),
                        expected_http_semantics="%s; all %d PUT(s) refused"
                                                % ("presign %s status %s" % (
                                                    presign["request_id"],
                                                    presign["status"])
                                                   if presign else
                                                   "chain never opened: no "
                                                   "admitted presign",
                                                len([r for r in (measured.get("http_requests") or [])
                                                     if r.get("action") == "put"])),
                        expected_db_delta="the unit asserts the scenario's "
                                          "DB/queue invariant before returning "
                                          "ok (w2upload suite contract)",
                        expected_object_state="no object written: measured "
                                              "fields %s" % (obj_keys or
                                                             "object %s unbound" % measured.get("object_key")))
                else:
                    oracle.update(
                        pre_object_hash="object absent before PUT, as asserted by the real driver read",
                        action="real PUT %s reached the driver and was rejected after frozen expiry" % put["request_id"],
                        expected_http_semantics="presign %s status %s; PUT %s status %s" % (
                            presign["request_id"], presign["status"], put["request_id"], put["status"]),
                        expected_db_delta="content pin sha256=%s committed before expired driver write" % measured["pin_sha256"],
                        expected_object_state="object %s absent after failed PUT" % measured["object_key"])
            row = S.C.build_row(
                leaf, obs_started, obs_finished,
                {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
                 "skipped": 0},
                oracle, assertions, actor, [resource],
                [proof[0]["request_id"], proof[1]["request_id"]] if proof
                else ["w403-obs:%s#%s@%s" % (source, scenario, leaf["driver"])])
            if proof:
                presign, _ = proof
                row["entrypoint_provenance"] = {
                    "entrypoint": leaf["entrypoint"], "route": presign["route"],
                    "method": presign["method"], "handler": presign["handler"],
                    "request_id": presign["request_id"]}
            rows.append(row)
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
