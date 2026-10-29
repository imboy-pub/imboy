#!/usr/bin/env python3
"""W4-03/namespace-alias-integrity adapter: measured leaves from the W4-03 storage suite.

Each leaf is sealed from its real suite observation (case/entrypoint/
scenario/domain + measured driver), the parsed named eunit unit, the clean
suite summary, the real driver resource and the suite source emit check.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w4_03_support as S

CASE = "W4-03/namespace-alias-integrity"
LOG_NAMES = ("eunit-w403.log",)
MARKER = "W403_DIRECT_OK"
MODULES = ["attachment_lifecycle_v5_w403_storage_tests"]
MODULE = "attachment_lifecycle_v5_w403_storage_tests"


def main():
    run_dir, case_dir, root = (os.path.abspath(arg) for arg in sys.argv[1:4])
    S.bind(
        CASE,
        run_dir=run_dir,
        case_dir=case_dir,
        root=root,
        log_names=LOG_NAMES,
        marker=MARKER,
        modules=MODULES,
        module=MODULE,
        oracle_fn=S.storage_oracle,
    )


if __name__ == "__main__":
    main()
