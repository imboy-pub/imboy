#!/usr/bin/env python3
"""W4-04/activation-commit-unknown adapter: activation commit-unknown and rollback evidence.

The adapter parses only real CASE_DIR/state.json/contract inputs.  When the
executed suite does not emit one measured unit per frozen leaf it names every
missing field and exits 30; it never relabels an aggregate unit as a leaf PASS.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w4_models as models  # noqa: E402

CASE = "W4-04/activation-commit-unknown"

if __name__ == "__main__":
    models.run(CASE)
