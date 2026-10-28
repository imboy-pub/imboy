import os
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_runner as runner


class RawArtifactTest(unittest.TestCase):
    def test_disappearing_case_directory_returns_contract_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp).resolve()
            case_dir = run / "evidence" / "W1-01" / "contract"
            case_dir.mkdir(parents=True)
            receipt = case_dir / "measured-receipts.json"
            row = {key: "test" for key in runner.LEAF_FIELDS}
            row["command"] = "command"
            receipt.write_text(json.dumps([row]))
            args = SimpleNamespace(case_dir=str(case_dir), receipts=str(receipt), command="command",
                                   task_id="W1-01", case_id="W1-01/contract")
            state = {"tasks": {"W1-01": {"state": "RUNNING"}}}
            contract = {"tasks": [{"task_id": "W1-01", "case_ids": ["W1-01/contract"]}],
                        "expected_leaf_matrix": {"W1-01/contract": [row]}}
            with patch.object(runner, "receipt_errors", return_value=[]), \
                 patch.object(runner.os, "listdir", side_effect=FileNotFoundError):
                _, errors, _, _ = runner._measured_case_rows(args, state, contract, str(run))
            self.assertEqual(["case directory unavailable while inspecting leaves"], errors)

    def test_missing_case_directory_is_a_contract_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp).resolve()
            case_dir = run / "evidence" / "W1-01" / "contract"
            args = SimpleNamespace(case_dir=str(case_dir), receipts=str(case_dir / "measured-receipts.json"),
                                   task_id="W1-01", case_id="W1-01/contract")
            _, errors, _, _ = runner._measured_case_rows(args, {}, {}, str(run))
            self.assertEqual(["case directory unavailable"], errors)

    def test_fifo_receipts_fail_without_blocking(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp).resolve()
            case_dir = run / "evidence" / "W1-01" / "contract"
            case_dir.mkdir(parents=True)
            receipt = case_dir / "measured-receipts.json"
            os.mkfifo(receipt)
            args = SimpleNamespace(case_dir=str(case_dir), receipts=str(receipt),
                                   task_id="W1-01", case_id="W1-01/contract")
            state = {"tasks": {"W1-01": {"state": "RUNNING"}}}
            contract = {"tasks": [{"task_id": "W1-01", "case_ids": ["W1-01/contract"]}]}
            _, errors, _, _ = runner._measured_case_rows(args, state, contract, str(run))
            self.assertEqual(["measured receipts are not a regular, run-owned JSON file"], errors)

    def test_only_regular_files_inside_run_without_symlinks(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            run = root / "run"
            run.mkdir()
            outside = root / "outside"
            outside.write_bytes(b"outside")
            valid = run / "raw.json"
            valid.write_bytes(b"inside")
            self.assertEqual(b"inside", runner._read_run_artifact(str(run), str(valid)))
            for name, target in (("file-link", outside), ("dir-link", root)):
                link = run / name
                link.symlink_to(target, target_is_directory=target.is_dir())
                path = link if name == "file-link" else link / "outside"
                with self.subTest(path=path), self.assertRaises((OSError, ValueError)):
                    runner._read_run_artifact(str(run), str(path))
            with self.assertRaises((OSError, ValueError)):
                runner._read_run_artifact(str(run), str(outside))
            os.link(outside, run / "hard-link")
            with self.assertRaises(ValueError):
                runner._read_run_artifact(str(run), str(run / "hard-link"))
            fifo = run / "fifo"
            os.mkfifo(fifo)
            with self.assertRaises(ValueError):
                runner._read_run_artifact(str(run), str(fifo))

    def test_sparse_oversized_evidence_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp).resolve()
            evidence = run / "raw.json"
            with evidence.open("wb") as stream:
                stream.truncate(runner.MAX_STRUCTURED_EVIDENCE_BYTES + 1)
            with self.assertRaises(ValueError):
                runner._read_run_artifact(str(run), str(evidence))


if __name__ == "__main__":
    unittest.main()
