#!/usr/bin/env python3
"""W1-02/immutable measured adapter (guards-contract leaves)."""
from attachment_lifecycle_v5_adapter_w1_support import run_w102_case

CASE_ID = "W1-02/immutable"
PHASE = "guards-contract"

ADEQUATE = {
    "mutate_driver": [("mutate_driver", 0)],
    "mutate_namespace": [("mutate_namespace", 0)],
    "mutate_container": [("mutate_container", 0)],
    "mutate_key": [("mutate_key", 0)],
    "mutate_owner": [("mutate_owner", 0)],
    "mutate_scope": [("mutate_scope", 0)],
    "mutate_cipher": [("mutate_cipher", 0)],
    "mutate_locator_digest": [("mutate_locator_digest", 0)],
    "tombstone_recreation": [("tombstone_recreation", 0)],
    # real first bind executed: NULL -> 7001 accepted once, 7001 -> 7002
    # refused (23510), replay of the identical value idempotent.
    "first_bind_once": [("first_bind_once", 0)],
    # real attachment group_file_id same-value replay (immutable_probes
    # occurrence 0; registration_probes emits a same-named row later).
    "same_value_replay": [("same_value_replay", 0)],
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("plan", _ordering, PHASE)] = {"probes": _probes}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
