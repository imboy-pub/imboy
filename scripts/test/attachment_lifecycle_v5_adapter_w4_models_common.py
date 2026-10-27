#!/usr/bin/env python3
"""Shared W4 model configuration, resource context and gap helpers.

The per-wave model modules (W4-01..W4-04) import this module; it contains no
case-specific runtime logic beyond Facts/timestamps and the frozen constants
copied into the contract mapping tables.
"""
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as base  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_support as S  # noqa: E402

W401_MODULES = (
    "attachment_lifecycle_v5_pg_tests",
    "attachment_lifecycle_v5_hold_protocol_tests",
    "attachment_lifecycle_v5_w401_entrypoints_tests",
    "attachment_lifecycle_v5_w401_hold_entrypoints_tests",
    "attachment_lifecycle_v5_w401_rollback_tests",
)
W402_MODULES = (
    "attachment_lifecycle_v5_w402_worker_tests",
    "attachment_lifecycle_v5_w402_purged_recovery_tests",
)
W403_MODULES = (
    "attachment_lifecycle_v5_w403_storage_tests",
    "attachment_lifecycle_v5_w403_driver_matrix_tests",
    "attachment_lifecycle_v5_w403_predecessor_tests",
)
W404_MODULES = ("attachment_lifecycle_v5_w404_activation_tests",)

CASE_CONFIG = {
    "W4-01/delete-entrypoints": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-01/intent-rollback": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-01/confirm-gc-race": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-01/reference-gc-race": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-01/hold-gc-race": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-01/hold-entrypoints": dict(
        log=("eunit-w401.log",), marker="W401_DIRECT_OK", modules=W401_MODULES),
    "W4-02/worker-crash": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/worker-lease": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/worker-db-fault": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/queue-adoption": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/last-attempt-crash": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/old-attempts-preserved": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/cross-queue-identity": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/ordinary-queue-recovery": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/claim-capacity-and-liveness": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-02/purged-session-recovery": dict(
        log=("eunit-w402.log",), marker="W402_DIRECT_OK", modules=W402_MODULES),
    "W4-03/storage-garage": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/storage-disk": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/storage-switch": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/late-put": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/namespace-alias-integrity": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/disk-predecessor-protocols": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES),
    "W4-03/driver-content-immutable": dict(
        log=("eunit-w403.log",), marker="W403_DIRECT_OK", modules=W403_MODULES,
        binder=True),
    "W4-04/reconcile": dict(
        log=("eunit-w404.log",), marker="W404_DIRECT_OK", modules=W404_MODULES),
    "W4-04/no-infinite-loop": dict(
        log=("eunit-w404.log",), marker="W404_DIRECT_OK", modules=W404_MODULES),
    "W4-04/guards-activation": dict(
        log=("eunit-w404.log",), marker="W404_DIRECT_OK", modules=W404_MODULES),
    "W4-04/all-writer-compat": dict(
        log=("eunit-w404.log",), marker="W404_DIRECT_OK", modules=W404_MODULES),
    "W4-04/activation-commit-unknown": dict(
        log=("eunit-w404.log",), marker="W404_DIRECT_OK", modules=W404_MODULES),
}

ROLLBACK_ENTRYPOINT = {
    "pending-cleanup": "pending_cleanup",
    "orphan-cleanup": "orphan_cleanup",
    "account-deletion": "account_deletion",
    "eb-pending-cleanup": "eb_pending_cleanup",
    "app-retention-purge": "app_retention_purge",
    "eb-private-delete": "eb_private_delete",
}
FAULT_SUFFIX = {
    "intent_insert_error": "intent_insert_error",
    "audit_insert_error": "audit_insert_error",
    "sql_rollback": "rollback",
    "commit_response_lost": "commit_response_lost",
}
HOLD_RACE_TOKENS = {
    "hold_commits_first": "v5e2e/w302/race/hold-first",
    "intent_before_asset_hold": "v5e2e/w302/race/intent-first",
    "intent_before_retention_extension": None,
    "workspace_hold_exclusion_receipt": None,
}
HOLD_RACE_DOMAIN = {
    "hold_commits_first": "ordinary",
    "intent_before_asset_hold": "enterprise",
}
HOLD_ENTRYPOINT_CALLS = {
    "orphan-cleanup": ("attachment_ds:orphan_cleanup", "orphan_cleanup"),
    "pending-cleanup": ("attachment_ds:pending_cleanup", "pending_cleanup"),
    "account-deletion": (
        "user_deletion_executor:collect_attachment_keys_tx", "collect+intent"),
    "app-retention-purge": (
        "enterprise_asset_retention_logic:governance_tx", "governance_tx purge"),
    "eb-private-delete": ("eb_asset_store:delete_private", "delete_private"),
    "eb-pending-cleanup": ("eb_asset_app:cleanup_pending", "cleanup_pending"),
}
PURGED_SCENARIOS = {
    "pending_purge_register_response_lost": (
        "w402p_pending_register_replay",
        "same-UUID register replay returns the same key/state"),
    "confirmed_purge_register_same_uuid": (
        "w402p_confirmed_purge_tombstone",
        "purge copies the queue tombstone and a same-UUID register is rejected"),
    "foreign_actor_no_disclosure": (
        "w402p_foreign_actor_no_disclosure",
        "a fresh foreign-actor UUID is accepted with the ordinary shape"),
    "done_job_same_uuid_no_new_asset": (
        "w402p_done_job_same_uuid",
        "after a done job the same UUID register is still rejected"),
}
DRIVER_SURFACE_DOMAIN = {
    "ordinary": "ordinary",
    "enterprise_internal": "ordinary",
    "enterprise_seat": "enterprise",
    "cs_widget": "enterprise",
}
DRIVER_SCENARIOS = (
    "confirm_old_put_different_bytes",
    "stat_then_different_put",
    "inflight_same_bytes_after_confirm",
    "put_paused_before_dispatch_until_expiry",
    "same_key_different_worker_payload",
    "provider_bypass_writer_denied",
    "max_supported_file_gateway_budget",
    "disk_put_delete_lock_order_or_garage_expiry_fence",
)
SCENARIO_TEXT = {
    "confirm_old_put_different_bytes": (
        "register/put/confirm succeed; a post-confirm PUT with different bytes "
        "is rejected and a same-content PUT performs no provider write",
        "session stays confirmed and no second content pin/metadata row commits",
        "the stored object remains the originally confirmed bytes"),
    "stat_then_different_put": (
        "register+put succeed; while confirm has observed the bytes a "
        "different-byte PUT is rejected; confirm then succeeds",
        "only one confirm/content-pin transaction commits",
        "read returns the original bytes; the competing request writes nothing"),
    "inflight_same_bytes_after_confirm": (
        "an in-flight same-byte PUT overlaps confirm and afterwards returns "
        "success or 409/410; read remains original",
        "confirm commits while the same-byte PUT is in flight",
        "the object bytes never change"),
    "put_paused_before_dispatch_until_expiry": (
        "the registration expires while the PUT is paused before driver "
        "dispatch; the PUT and the following confirm are rejected",
        "the pending session expires and no confirm/metadata commit occurs",
        "the late PUT does not persist; read returns not_found"),
    "same_key_different_worker_payload": (
        "two workers PUT the same key with different payloads; the second is "
        "rejected, the first completes and confirm succeeds",
        "only the first content pin/confirm commits",
        "read returns the original bytes, never the competing payload"),
    "provider_bypass_writer_denied": (
        "after confirm a direct provider write that bypasses the application "
        "is denied (garage 401/403; disk unprivileged principal gets EACCES)",
        "confirmed metadata is unchanged",
        "read returns the original bytes"),
    "max_supported_file_gateway_budget": (
        "a payload exactly at the surface maximum is uploaded through the real "
        "gateway and confirmed successfully",
        "content pin/confirm commits for the maximum-size payload",
        "read returns the exact maximum-size bytes"),
    "disk_put_delete_lock_order_or_garage_expiry_fence": (
        "disk: DELETE waits behind the writer's OS lock until the PUT lock is "
        "released; garage: the unchanged signed URL is rejected after expiry "
        "with InvalidRequest/Date too old",
        "the queue job reaches done only after the owned object is deleted and "
        "no other key is touched",
        "disk object is absent after the drain; garage late PUT does not "
        "persist and read returns not_found"),
}


class Facts(object):
    def __init__(self, case_id):
        self.case_id = case_id
        cfg = CASE_CONFIG[case_id]
        run_dir, case_dir, root = base.argv_paths()
        self.run_dir = run_dir
        self.case_dir = case_dir
        self.root = root
        self.ctx = S.context(run_dir, case_id)
        self.leaves = S.leaves(self.ctx, case_id)
        self.evidence = S.read_log(case_dir, cfg["log"], case_id)
        text = self.evidence["text"] if self.evidence else ""
        self.parsed = S.parse_verbose(text)
        self.w403_evidence = (
            S.parse_w403_evidence(text) if case_id.startswith("W4-03/") else [])
        self.health = S.suite_health(
            self.evidence, self.parsed, cfg["marker"], cfg["modules"],
            cfg["log"])
        self._sources = {}

    def src(self, relpath):
        if relpath not in self._sources:
            self._sources[relpath] = S.source(self.root, relpath, self.case_id)
        return self._sources[relpath]

    def test(self, module, func=None):
        return S.find_test(self.parsed, module, func=func)

    def unit(self, module, func):
        test = self.test(module, func)
        return S.describe_test(test) if test else "no parsed %s:%s unit" % (
            module, func)


def _health_note(f):
    return ["suite health: %s" % "; ".join(f.health)] if f.health else []


def _oracle():
    return S.oracle_gap()
