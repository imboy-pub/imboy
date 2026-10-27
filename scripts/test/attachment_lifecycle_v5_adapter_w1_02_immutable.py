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
}

GAPS = {
    "first_bind_once": (
        [("first_bind_once", 0)],
        "probe executes UPDATE attachment SET group_file_id=NULL ... then "
        "SELECT 'ok'; it creates no first bind and makes no second-bind "
        "attempt, so once-only binding is not measured"),
    "same_value_replay": (
        [("same_value_replay", 0)],
        "the only row with this name is emitted by registration_probes "
        "(fn_v5_register_pending replay on v5p/reg1); immutable_probes has no "
        "attachment-binding replay, so this immutable leaf would read an "
        "ordinary upload-registration measurement"),
}

MAPPING = {}
for _ordering, _probes in ADEQUATE.items():
    MAPPING[("plan", _ordering, PHASE)] = {"probes": _probes}
for _ordering, (_probes, _reason) in GAPS.items():
    MAPPING[("plan", _ordering, PHASE)] = {"probes": _probes, "gap": _reason}

if __name__ == "__main__":
    run_w102_case(CASE_ID, MAPPING, two_phase=False)
