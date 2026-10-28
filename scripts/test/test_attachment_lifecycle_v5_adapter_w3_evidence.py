"""W3 adapters must not turn named EUnit output into measured receipts."""
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPTS = ROOT / "scripts" / "test"


class W3EvidenceTests(unittest.TestCase):
    def test_all_adapters_report_missing_evidence_as_nogo(self):
        plan = ROOT / "docs" / "architecture" / "attachment-lifecycle-v5"
        with tempfile.TemporaryDirectory(prefix="w3-missing-") as tmp:
            run = Path(tmp)
            (run / "state.json").write_text(json.dumps(
                {"plan_dir": str(plan), "resource_allowlist": {"scratch_postgres": ["fake"]}}))
            case = run / "case"
            case.mkdir()
            adapters = sorted(SCRIPTS.glob("attachment_lifecycle_v5_adapter_w3_0*.py"))
            self.assertEqual({path.name for path in adapters}, {
                "attachment_lifecycle_v5_adapter_w3_01_history_unknown.py",
                "attachment_lifecycle_v5_adapter_w3_01_reference_gc_race.py",
                "attachment_lifecycle_v5_adapter_w3_01_shared_owner.py",
                "attachment_lifecycle_v5_adapter_w3_02_hold_all_entrypoints.py",
                "attachment_lifecycle_v5_adapter_w3_02_hold_gc_race.py",
                "attachment_lifecycle_v5_adapter_w3_03_cs_bind.py",
                "attachment_lifecycle_v5_adapter_w3_03_enterprise_bind.py",
            })
            for adapter in adapters:
                with self.subTest(adapter=adapter.name):
                    result = subprocess.run([sys.executable, str(adapter), str(run),
                                             str(case), str(ROOT)],
                                            capture_output=True, text=True, check=False)
                    self.assertEqual(30, result.returncode, result.stderr)
                    self.assertIn("no case evidence log", result.stderr)
                    self.assertFalse((case / "observations.json").exists())
                    self.assertFalse((case / "measured-receipts.json").exists())

    def run_adapter(self, suffix, case_id, scenarios, tests):
        with tempfile.TemporaryDirectory(prefix="w3-evidence-") as tmp:
            root = Path(tmp)
            plan = root / "plan"
            plan.mkdir()
            case = root / "case"
            case.mkdir()
            leaves = [{"ac_id": "AC04", "domain": "enterprise", "driver": "none",
                       "entrypoint": case_id.split("/")[1], "ordering_or_fault": name,
                       "schema_phase": "expand-queue", "observation_deadline_seconds": 1800}
                      for name in scenarios]
            (plan / "execution-contract.json").write_text(json.dumps(
                {"expected_leaf_matrix": {case_id: leaves}}))
            (root / "state.json").write_text(json.dumps(
                {"plan_dir": str(plan), "resource_allowlist": {"scratch_postgres": ["fake"]}}))
            lines = ["  eb_message_assets_projection_tests: %s...[0.001 s] ok" % name
                     for name in tests]
            lines.extend(["  All %d tests passed." % len(tests), "W303_DIRECT_OK"])
            (case / "eunit-w303.log").write_text("\n".join(lines) + "\n")
            result = subprocess.run(
                [sys.executable, str(SCRIPTS / ("attachment_lifecycle_v5_adapter_w3_03_" + suffix + ".py")),
                 str(root), str(case), str(ROOT)], capture_output=True, text=True, check=False)
            self.assertEqual(30, result.returncode, result.stderr)
            self.assertIn("EVIDENCE_GAP", result.stderr)
            self.assertIn("runtime database/actor provenance", result.stderr)
            for scenario in scenarios:
                self.assertIn(scenario, result.stderr)
            self.assertFalse((case / "observations.json").exists())
            self.assertFalse((case / "measured-receipts.json").exists())
            self.assertFalse(list(case.glob("leaf-*.json")))

    def test_enterprise_named_green_is_not_a_receipt(self):
        self.run_adapter("enterprise_bind", "W3-03/enterprise-bind",
                         ["same_message_replay"], ["enterprise_replay_echoes_same_assets"])

    def test_joint_green_is_not_two_receipts(self):
        self.run_adapter("cs_bind", "W3-03/cs-bind",
                         ["wrong_org", "wrong_workspace"],
                         ["cross_org_and_workspace_bind_rejected"])


if __name__ == "__main__":
    unittest.main()
