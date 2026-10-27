#!/usr/bin/env python3
"""W1-02/down-block measured adapter (expand-queue + guards-contract leaves).

The frozen probe script never invokes an up/down/up replay or a down guard:
each ordering only inserts a fixture row and evaluates a local CASE predicate.
None of the five orderings can support the contract's "rejects down" oracle,
so this adapter reports the exact gaps instead of sealing simulated PASS rows.
"""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/down-block"
PHASES = ("expand-queue", "guards-contract")

GAPS = {
    "empty_up_down_up": (
        [("empty_up_down_up", 0)],
        "probe SQL is the constant SELECT 'ok'; no upgrade, down or re-up "
        "migration is executed"),
    "nondefault_identity": (
        [("nondefault_identity", 0), ("nondefault_identity_predicate", 0)],
        "probe inserts a located attachment row and evaluates a local "
        "'blocked'/'down' CASE predicate; no down migration or down guard is "
        "executed"),
    "confirmed_session": (
        [("confirmed_session", 0)],
        "probe evaluates a CASE over existing attach_pending rows "
        "(confirmed_attachment_id OR client_upload_id); no down is executed "
        "and no confirmed-session fixture is created"),
    "attempted_job": (
        [("attempted_job", 0)],
        "probe inserts an attachment_object_deletion row and evaluates a local "
        "CASE predicate; no down is executed"),
    "tombstone": (
        [("tombstone", 0)],
        "probe inserts an enterprise_asset_delete_queue tombstone row and "
        "evaluates a local CASE predicate; no down is executed"),
}

MAPPING = {}
for _ordering, (_probes, _reason) in GAPS.items():
    for _phase in PHASES:
        MAPPING[("plan", _ordering, _phase)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=True)
