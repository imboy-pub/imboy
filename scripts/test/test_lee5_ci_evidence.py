"""Offline guard regressions; synthetic XML/JSON, not real PostgreSQL evidence."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("lee5_guard", ROOT / "scripts/ci/verify_lee5_evidence.py")
GUARD = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GUARD)
SHA = "a" * 40


class EvidenceTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "commit.txt").write_text(SHA)
        (self.root / "runner-exit.txt").write_text("0")
        (self.root / "acceptance-mode.txt").write_text("1")
        for module in GUARD.MODULES:
            path = self.root / "raw" / module
            path.mkdir(parents=True)
            suite = ET.Element("testsuite", name=f"module '{module}'", tests="1",
                               failures="0", errors="0", skipped="0")
            ET.SubElement(suite, "testcase", name="synthetic")
            ET.ElementTree(suite).write(path / f"TEST-{module}.xml")
        self.dcheck = {
            "status": "PASS", "cases": 3, "failed": 0, "skipped": 0,
            "fixture_rows_remaining": 0, "authority": "real PG",
            "backend_nodes": ["a@127.0.0.1", "b@127.0.0.1"],
            "scenarios": [{"entries": list(pair), "synchronized_pg_waiters": 2,
                           "credentials": 1, "winner_identity_checked": True,
                           "consumers": [{"node": node, "entry": entry,
                                          "credential_count": int(index == 0), "compatible": True}
                                         for index, (node, entry) in enumerate(zip(
                                             ["a@127.0.0.1", "b@127.0.0.1"], pair))]}
                          for pair in GUARD.PAIRS],
        }
        self.write_dcheck()

    def write_dcheck(self):
        (self.root / "dcheck.json").write_text(json.dumps(self.dcheck))

    def validate(self):
        return GUARD.validate(self.root, SHA, "success")

    def first_report(self):
        module = GUARD.MODULES[0]
        return self.root / "raw" / module / f"TEST-{module}.xml"

    def test_valid_evidence_and_output_allowlist(self):
        # Raw failure output must never be copied into the sanitized summary.
        path = self.first_report()
        suite = ET.parse(path).getroot()
        ET.SubElement(suite, "system-out").text = "DO_NOT_PUBLISH_RAW_TOKEN"
        ET.ElementTree(suite).write(path)
        summary = self.validate()
        self.assertEqual("PASS", summary["status"])
        self.assertNotIn("DO_NOT_PUBLISH", json.dumps(summary))
        self.assertEqual(set(GUARD.MODULES), set(summary["modules"]))

    def test_missing_or_stale_run_metadata(self):
        for filename, invalid in (("runner-exit.txt", "2"), ("acceptance-mode.txt", "0"),
                                  ("commit.txt", "b" * 40)):
            path = self.root / filename
            original = path.read_text()
            with self.subTest(filename=filename):
                path.write_text(invalid)
                with self.assertRaises(ValueError):
                    self.validate()
                path.unlink()
                with self.assertRaises(OSError):
                    self.validate()
                path.write_text(original)

    def test_unsuccessful_or_skipped_step_cannot_pass(self):
        for outcome in ("failure", "skipped", "cancelled", ""):
            with self.subTest(outcome=outcome), self.assertRaises(ValueError):
                GUARD.validate(self.root, SHA, outcome)

    def test_missing_report(self):
        self.first_report().unlink()
        with self.assertRaises(ValueError):
            self.validate()

    def test_extra_report(self):
        path = self.first_report()
        path.with_name("TEST-unexpected.xml").write_bytes(path.read_bytes())
        with self.assertRaises(ValueError):
            self.validate()

    def test_report_counters_and_empty_selection(self):
        path = self.first_report()
        original = path.read_bytes()
        for key, value in (("tests", "0"), ("tests", "2"), ("failures", "1"),
                           ("errors", "1"), ("skipped", "1"), ("errors", "-1")):
            with self.subTest(key=key):
                suite = ET.fromstring(original)
                suite.set(key, value)
                ET.ElementTree(suite).write(path)
                with self.assertRaises(ValueError):
                    self.validate()
        path.write_bytes(original)

    def test_hidden_case_failure_or_skip(self):
        path = self.first_report()
        original = path.read_bytes()
        for tag in ("failure", "error", "skipped"):
            with self.subTest(tag=tag):
                suite = ET.fromstring(original)
                ET.SubElement(suite.find("testcase"), tag)
                ET.ElementTree(suite).write(path)
                with self.assertRaises(ValueError):
                    self.validate()

    def test_wrong_suite_or_malformed_xml(self):
        path = self.first_report()
        suite = ET.parse(path).getroot()
        suite.set("name", "wrong module")
        ET.ElementTree(suite).write(path)
        with self.assertRaises(ValueError):
            self.validate()
        path.write_text("<malformed")
        with self.assertRaises(ET.ParseError):
            self.validate()

    def test_two_peer_requires_actual_nodes_waiters_identity_and_single_winner(self):
        original = copy.deepcopy(self.dcheck)
        mutations = [
            lambda d: d.update(backend_nodes=["a", "a"]),
            lambda d: d.update(skipped=1),
            lambda d: d.update(fixture_rows_remaining=1),
            lambda d: d["scenarios"].pop(),
            lambda d: d["scenarios"][0].update(winner_identity_checked=False),
            lambda d: d["scenarios"][0].update(synchronized_pg_waiters=1),
            lambda d: d["scenarios"][0]["consumers"][1].update(credential_count=1),
            lambda d: d["scenarios"][0]["consumers"][0].update(compatible=False),
            lambda d: d["scenarios"][0]["consumers"][0].update(credential_count=True),
            lambda d: d["scenarios"][0]["consumers"][0].update(node="other"),
        ]
        for index, mutate in enumerate(mutations):
            with self.subTest(index=index):
                self.dcheck = copy.deepcopy(original)
                mutate(self.dcheck)
                self.write_dcheck()
                with self.assertRaises(ValueError):
                    self.validate()

    def test_cli_writes_failure_not_stale_pass(self):
        (self.root / "summary.json").write_text('{"status":"PASS"}')
        result = subprocess.run(["python3", str(ROOT / "scripts/ci/verify_lee5_evidence.py"),
                                 str(self.root), "--commit", SHA, "--outcome", "failure"],
                                capture_output=True, text=True)
        self.assertEqual(1, result.returncode)
        self.assertEqual("FAIL", json.loads((self.root / "summary.json").read_text())["status"])

    def test_ci_runner_refuses_unapproved_environment(self):
        env = os.environ.copy()
        env.pop("LEE5_CI_DISPOSABLE", None)
        result = subprocess.run(["bash", str(ROOT / "scripts/ci/lee5-acceptance.sh")],
                                env=env, capture_output=True, text=True)
        self.assertEqual(2, result.returncode)


if __name__ == "__main__":
    unittest.main()
