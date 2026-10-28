#!/usr/bin/env python3
"""W4 adapter dispatcher.

The wave modules own the leaf models; this module only resolves the frozen
case context and dispatches to the assessment model or the W4-03 leaf binder.
Every path is fail-closed with exit 30 and a named evidence gap.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as base  # noqa: E402
import attachment_lifecycle_v5_adapter_w4_models_common as common  # noqa: E402
from attachment_lifecycle_v5_adapter_w4_models_w401 import W401_MODELS  # noqa: E402
from attachment_lifecycle_v5_adapter_w4_models_w402 import W402_MODELS  # noqa: E402
from attachment_lifecycle_v5_adapter_w4_models_w403 import (  # noqa: E402
    W403_MODELS,
    binder_driver_content_immutable,
)
from attachment_lifecycle_v5_adapter_w4_models_w404 import W404_MODELS  # noqa: E402

MODELS = {}
MODELS.update(W401_MODELS)
MODELS.update(W402_MODELS)
MODELS.update(W403_MODELS)
MODELS.update(W404_MODELS)


def run(case_id):
    if case_id not in common.CASE_CONFIG:
        common.S.die(case_id, "case is not in the W4 adapter registry")
    cfg = common.CASE_CONFIG[case_id]
    try:
        facts = common.Facts(case_id)
        if cfg.get("binder"):
            binder_driver_content_immutable(facts)
            return
        if case_id not in MODELS:
            common.S.die(case_id, "no leaf model registered")
        gap_fn, notes = MODELS[case_id](facts)
        if case_id.startswith("W4-04/"):
            notes = notes + [
                "suite setup_storage_fixture/0 creates its disk root under "
                "/tmp (test/repo/attachment_lifecycle_v5_w404_activation_tests"
                ".erl:69-73), not under V5_DISK_ROOT; W4-04 leaves are "
                "driver=none, so no storage receipt is claimed"]
        gaps = common.S.model_gaps(facts.leaves, gap_fn)
        common.S.report_gaps(
            case_id, facts.evidence, facts.parsed, gaps,
            notes=common._health_note(facts) + notes)
    except base.AdapterGap as exc:
        common.S.die(case_id, str(exc))
