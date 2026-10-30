"""Attempt path isolation for lifecycle evidence publication."""
import importlib.util
import hashlib
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts"))
SPEC = importlib.util.spec_from_file_location(
    "v5_attempt_producer", ROOT / "scripts/attachment_lifecycle_v5_producer.py")
producer = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(producer)


class AttemptPathTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.run = Path(self.tmp.name).resolve()
        self.case = self.run / "evidence/W6-01/example"
        self.first = self.case / "attempt-1"
        self.second = self.case / "attempt-2"
        self.first.mkdir(parents=True)
        self.second.mkdir()

    def open_attempt(self, path, attempt):
        return producer._open_case_dir(str(self.run), "W6-01", "W6-01/example", str(path), attempt)

    def test_current_attempt_is_distinct_and_old_artifacts_survive(self):
        (self.first / "measured-receipts.json").write_text("old receipt")
        fd = self.open_attempt(self.second, 2)
        try:
            producer._write_exclusive(fd, "measured-receipts.json", b"new receipt")
        finally:
            os.close(fd)
        self.assertEqual("old receipt", (self.first / "measured-receipts.json").read_text())
        self.assertEqual("new receipt", (self.second / "measured-receipts.json").read_text())
        with self.assertRaises(ValueError):
            self.open_attempt(self.first, 2)
        with self.assertRaises(ValueError):
            self.open_attempt(self.case, 2)

    def test_symlinked_attempt_cannot_redirect_publication(self):
        self.second.rmdir()
        outside = self.run / "outside"
        outside.mkdir()
        self.second.symlink_to(outside, target_is_directory=True)
        with self.assertRaises(OSError):
            self.open_attempt(self.second, 2)
        self.assertEqual([], list(outside.iterdir()))

    def test_attempt_must_be_positive_integer(self):
        for attempt in (0, -1, True, "1"):
            with self.subTest(attempt=attempt), self.assertRaises(ValueError):
                self.open_attempt(self.first, attempt)

    def test_reconciliation_rejects_new_attempt_candidate_or_changed_raw(self):
        raw = self.first / "raw.json"
        raw.write_bytes(b"first observation")
        receipt = {"task_attempt": 1, "candidate_heads": {"imboy": "a" * 40},
                   "raw_output_path": str(raw),
                   "raw_output_sha256": hashlib.sha256(raw.read_bytes()).hexdigest()}
        published = self.first / "measured-receipts.json"
        published.write_text(json.dumps([receipt]))
        self.assertTrue(producer._same_published(str(self.run), str(published), [receipt]))
        other_attempt = dict(receipt, task_attempt=2)
        self.assertFalse(producer._same_published(str(self.run), str(published), [other_attempt]))
        other_candidate = dict(receipt, candidate_heads={"imboy": "b" * 40})
        self.assertFalse(producer._same_published(str(self.run), str(published), [other_candidate]))
        raw.write_bytes(b"altered observation")
        self.assertFalse(producer._same_published(str(self.run), str(published), [receipt]))


if __name__ == "__main__":
    unittest.main()
