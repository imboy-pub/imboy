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

    def run_writer(self, status, trace_exit=2, failed_step="",
                   http_smoke=1, channel_webhook=1, agent_dialog=1,
                   bot_dialog=1, protocol_negatives=1, restart=1,
                   final_integrated=0):
        if status != "FAIL":
            (self.evidence / "runtime-correlation-trace.json").write_text(
                '{"schema_version":1,"records":[]}\n', encoding="utf-8")
            (self.evidence / "trace-verifier.json").write_text(
                '{"decision":"OK"}\n', encoding="utf-8")
        if http_smoke:
            (self.evidence / "ext01-a02-runtime.json").write_text(
                '{"passed":11,"failed":0}\n', encoding="utf-8")
        if channel_webhook:
            (self.evidence / "channel-webhook-a02-runtime.json").write_text(
                '{"passed":4,"failed":0}\n', encoding="utf-8")
            (self.evidence / "channel-webhook-a02-db.json").write_text(
                '{"message_count":1}\n', encoding="utf-8")
        if agent_dialog:
            (self.evidence / "agent-dialog-a02-runtime.log").write_text(
                "runtime evidence\n", encoding="utf-8")
            (self.evidence / "agent-dialog-a02-db.json").write_text(
                '{"human_match":1,"agent_match":1}\n', encoding="utf-8")
        if bot_dialog:
            (self.evidence / "bot-dialog-a02-runtime.json").write_text(
                '{"passed":12,"failed":0}\n', encoding="utf-8")
            (self.evidence / "bot-dialog-a02-db.json").write_text(
                '{"bot_reply_count":1}\n', encoding="utf-8")
        if protocol_negatives:
            for name in [
                "webhook-5xx-a02-runtime.json",
                "webhook-5xx-a02-db.json",
                "webhook-4xx-a02-runtime.json",
                "webhook-4xx-a02-db.json",
                "delivery-replay-a02-runtime.json",
                "delivery-replay-a02-db.json",
                "agent-task-e2ee-a02-runtime.txt",
                "agent-task-e2ee-a02-db.json",
            ]:
                (self.evidence / name).write_text("runtime evidence\n", encoding="utf-8")
        if restart:
            for name in [
                "restart-before.json",
                "restart-after.json",
                "restart-logic-read.txt",
                "runtime-backend-before-restart.log",
                "runtime-backend-after-restart.log",
            ]:
                (self.evidence / name).write_text("runtime evidence\n", encoding="utf-8")
        code = WRITER.main([
            "--evidence-dir", str(self.evidence),
            "--repo-root", str(self.repo),
            "--status", status,
            "--imboy-sha", "a" * 40,
            "--imboyapp-sha", "b" * 40,
            "--imboyadmin-sha", "c" * 40,
            "--suites-passed", "15" if status == "PARTIAL" else "2",
            "--trace-exit", str(trace_exit),
            "--http-smoke-passed", str(http_smoke),
            "--channel-webhook-passed", str(channel_webhook),
            "--agent-dialog-passed", str(agent_dialog),
            "--bot-dialog-passed", str(bot_dialog),
            "--protocol-negatives-passed", str(protocol_negatives),
            "--restart-passed", str(restart),
            "--final-integrated-base", str(final_integrated),
            "--cleanup-passed", "1",
            "--sensitive-scan-passed", "1",
            "--failed-step", failed_step,
            "--failed-code", "7",
        ])
        self.assertEqual(code, 0)
        self.assertFalse((self.evidence / "evidence.json.tmp").exists())
        return VERIFIER.verify_task_file(str(self.evidence / "evidence.json"))

    def test_partial_has_a01_a02_pass_but_a07_still_open(self):
        result = self.run_writer("PARTIAL", trace_exit=0)
        self.assertEqual(result["decision"], "PARTIAL")
        evidence = VERIFIER._load_evidence_json(self.evidence / "evidence.json")
        self.assertIn(
            "failed_step=none", (self.evidence / "run-summary.txt").read_text())
        by_id = {row["acceptance_id"]: row for row in evidence["acceptance"]}
        self.assertEqual(by_id["E2E-01-A01"]["status"], "PASS")
        self.assertEqual(by_id["E2E-01-A02"]["status"], "PASS")
        self.assertIn("cmd-08", by_id["E2E-01-A02"]["command_ids"])
        self.assertIn(
            "artifact-channel-webhook-db", by_id["E2E-01-A02"]["artifact_ids"])
        self.assertIn("cmd-09", by_id["E2E-01-A02"]["command_ids"])
        self.assertIn("cmd-10", by_id["E2E-01-A02"]["command_ids"])
        self.assertIn(
            "artifact-agent-dialog-db", by_id["E2E-01-A02"]["artifact_ids"])
        self.assertIn(
            "artifact-bot-dialog-db", by_id["E2E-01-A02"]["artifact_ids"])
        self.assertEqual(by_id["E2E-01-A03"]["status"], "PASS")
        self.assertEqual(by_id["E2E-01-A06"]["status"], "PASS")
        self.assertEqual(by_id["E2E-01-A07"]["status"], "SKIP")

    def test_final_integrated_marks_a07_pass(self):
        result = self.run_writer(
            "PASS", trace_exit=0, final_integrated=1)
        self.assertEqual(result["decision"], "PASS")
        evidence = VERIFIER._load_evidence_json(self.evidence / "evidence.json")
        by_id = {row["acceptance_id"]: row for row in evidence["acceptance"]}
        self.assertEqual(by_id["E2E-01-A07"]["status"], "PASS")
        self.assertEqual(by_id["E2E-01-A07"]["command_ids"], ["cmd-12"])
        self.assertNotIn(
            "Final integrated Base rerun remains open.", evidence["residual_risks"])

    def test_pass_without_final_integrated_flag_is_rejected(self):
        with self.assertRaises(ValueError):
            self.run_writer("PASS", trace_exit=0)

    def test_protocol_negative_flag_without_artifacts_cannot_pass(self):
        result = self.run_writer("PARTIAL", trace_exit=0, protocol_negatives=0)
        self.assertEqual(result["decision"], "PARTIAL")
        evidence = VERIFIER._load_evidence_json(self.evidence / "evidence.json")
        by_id = {row["acceptance_id"]: row for row in evidence["acceptance"]}
        self.assertEqual(by_id["E2E-01-A02"]["status"], "FAIL")

    def test_restart_flag_without_artifacts_cannot_pass(self):
        result = self.run_writer("PARTIAL", http_smoke=0, restart=0)
        self.assertEqual(result["decision"], "PARTIAL")
        evidence = VERIFIER._load_evidence_json(self.evidence / "evidence.json")
        by_id = {row["acceptance_id"]: row for row in evidence["acceptance"]}
        self.assertEqual(by_id["E2E-01-A03"]["status"], "FAIL")

    def test_failure_is_valid_but_cannot_pass(self):
        result = self.run_writer("FAIL", failed_step="make app")
        self.assertEqual(result["decision"], "FAIL")
        self.assertIn(
            "failed_step=make app", (self.evidence / "run-summary.txt").read_text())


if __name__ == "__main__":
    unittest.main()
