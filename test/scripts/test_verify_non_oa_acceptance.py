"""Integrity gates must bind external plans and distinguish complete from partial."""
import csv
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("verifier", ROOT / "scripts/verify_non_oa_acceptance.py")
VERIFIER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VERIFIER)


class NonOaVerifierTest(unittest.TestCase):
    def create_run(self, directory, pending=False):
        run = Path(directory)
        plan = run / "external-plan.md"
        plan.write_text("External execution contract")
        evidence = run / "proof.json"
        evidence.write_text('{"result":"recorded"}')
        rows = []
        for index, acceptance_id in enumerate(sorted(VERIFIER.IDS)):
            rows.append({
                "id": acceptance_id,
                "status": "PARTIAL" if pending and index == 0 else "PASS_EXECUTED",
                "plan_sha256": VERIFIER.digest(plan),
                "evidence_sha256": VERIFIER.digest(evidence),
                "evidence_path": evidence.name,
                "reason": "unit fixture", "next_action": "review",
                "source_sha": json.dumps({"backend": "a" * 40}),
                "command": "fixture runner", "oracle": "fixture result", "exit_code": "0",
            })
        with (run / "acceptance.tsv").open("w") as handle:
            writer = csv.DictWriter(handle, fieldnames=list(rows[0]), delimiter="\t")
            writer.writeheader()
            writer.writerows(rows)
        (run / "artifacts.json").write_text(json.dumps([
            {"path": evidence.name, "sha256": VERIFIER.digest(evidence)}
        ]))
        (run / "state.json").write_text(json.dumps({
            "acceptance_ids": {row["id"]: row["status"] for row in rows},
            "pending": [row["id"] for row in rows if row["status"] == "PARTIAL"],
            "status": "PARTIAL" if pending else "NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS",
        }))
        (run / "independent-review.json").write_text(json.dumps({
            "verdict": "APPROVE", "reviewer": "unit-fixture-reviewer",
            "ledger_sha256": VERIFIER.digest(run / "acceptance.tsv"),
        }))
        return run, plan

    def test_complete_integrity_returns_true(self):
        with tempfile.TemporaryDirectory() as directory:
            self.assertTrue(VERIFIER.verify(*self.create_run(directory)))

    def test_partial_integrity_returns_false(self):
        with tempfile.TemporaryDirectory() as directory:
            self.assertFalse(VERIFIER.verify(*self.create_run(directory, pending=True)))

    def test_changed_external_plan_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            run, plan = self.create_run(directory)
            plan.write_text("Changed contract")
            with self.assertRaisesRegex(ValueError, "plan drift"):
                VERIFIER.verify(run, plan)


if __name__ == "__main__":
    unittest.main()
