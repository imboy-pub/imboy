#!/usr/bin/env python3
"""W1-02/down-block measured adapter (expand-queue + guards-contract leaves).

The probe replays the real frozen 00000178/00000179 down files through
psql (--single-transaction) per ordering, asserts the V5_DOWN_BLOCKED_
USE_FORWARD_FIX refusal, and proves attribution by removing the single
fixture and watching the same down apply.  Each leaf below is bound to the
probe row that executed that action; no ordering relies on a local CASE
predicate any more.
"""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/down-block"
PHASES = ("expand-queue", "guards-contract")

ADEQUATE = {
    # real down179+down178 replay on the controlled rollback-cycle database,
    # absence of every V5 column/function verified between down and re-up,
    # then real up178+up179 replay restoring the schema.
    "empty_up_down_up": [("empty_up_down_up", 0)],
    # real 00000178.down.sql replay refused with V5_DOWN_BLOCKED_USE_FORWARD_
    # FIX while a located attachment row exists; columns and row unchanged.
    "nondefault_identity": [("nondefault_identity", 0)],
    # real 00000178.down.sql refused for a persisted client_upload_id session.
    "confirmed_session": [("confirmed_session", 0)],
    # real 00000179.down.sql refused for attempts=2; job row unchanged.
    "attempted_job": [("attempted_job", 0)],
    # real 00000179.down.sql refused for an enterprise recovery tombstone;
    # attribution proven by tombstone_block_attributable (same down applies
    # once the tombstone row is removed).
    "tombstone": [("tombstone", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    for _phase in PHASES:
        MAPPING[("plan", _ordering, _phase)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=True)
