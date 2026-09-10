#!/usr/bin/env python3

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WRITER_PATH = ROOT / "scripts/write_agent_hub_e2e_evidence.py"
VERIFIER_PATH = ROOT / "scripts/verify_agent_hub_task_evidence.py"


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


WRITER = load("write_agent_hub_e2e_evidence", WRITER_PATH)
VERIFIER = load("verify_agent_hub_task_evidence", VERIFIER_PATH)


class EvidenceWriterTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.base = Path(self.tmp.name)
        self.repo = self.base / "repo"
        (self.repo / "scripts").mkdir(parents=True)
        (self.repo / "docs/operations").mkdir(parents=True)
        (self.repo / "docs/runbooks").mkdir(parents=True)
        (self.repo / "scripts/agent_hub_golden_flow.sh").write_text(
            "#!/bin/sh\n", encoding="utf-8")
        (self.repo / "docs/operations/agent-hub-local-golden-flow.md").write_text(
            "runbook\n", encoding="utf-8")
        (self.repo / "docs/runbooks/bot-webhook-aead-key.md").write_text(
            "aead runbook\n", encoding="utf-8")
        self.evidence = self.base / "E2E-01"
        self.evidence.mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def run_writer(self, status, trace_exit=2, failed_step=""):
        if status == "PARTIAL":
            (self.evidence / "trace-verifier.json").write_text(
                '{"decision":"VIOLATION"}\n', encoding="utf-8")
        code = WRITER.main([
            "--evidence-dir", str(self.evidence),
            "--repo-root", str(self.repo),
            "--status", status,
            "--imboy-sha", "a" * 40,
            "--imboyapp-sha", "b" * 40,
            "--imboyadmin-sha", "c" * 40,
            "--suites-passed", "13" if status == "PARTIAL" else "2",
            "--trace-exit", str(trace_exit),
            "--cleanup-passed", "1",
            "--sensitive-scan-passed", "1",
            "--failed-step", failed_step,
            "--failed-code", "7",
        ])
        self.assertEqual(code, 0)
        self.assertFalse((self.evidence / "evidence.json.tmp").exists())
        return VERIFIER.verify_task_file(str(self.evidence / "evidence.json"))

    def test_partial_is_valid_but_cannot_pass(self):
        result = self.run_writer("PARTIAL")
        self.assertEqual(result["decision"], "PARTIAL")
        evidence = VERIFIER._load_evidence_json(self.evidence / "evidence.json")
        by_id = {row["acceptance_id"]: row for row in evidence["acceptance"]}
        self.assertEqual(by_id["E2E-01-A01"]["status"], "FAIL")
        self.assertEqual(by_id["E2E-01-A06"]["status"], "PASS")

    def test_failure_is_valid_but_cannot_pass(self):
        result = self.run_writer("FAIL", failed_step="make app")
        self.assertEqual(result["decision"], "FAIL")


if __name__ == "__main__":
    unittest.main()
