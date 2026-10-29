import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_case_store as store
from scripts import attachment_lifecycle_v5_runner as runner


class CaseStoreTests(unittest.TestCase):
    def test_oversized_artifacts_fail_closed_before_publication(self):
        with tempfile.TemporaryDirectory() as tmp:
            fd = os.open(tmp, os.O_RDONLY | os.O_DIRECTORY)
            try:
                with patch.object(store, "MAX_LEAVES", 1):
                    with self.assertRaises(ValueError):
                        store.publish(fd, [{"n": 1}, {"n": 2}])
                with patch.object(store, "MAX_ARTIFACT_BYTES", 8):
                    with self.assertRaises(ValueError):
                        store.publish(fd, [{"value": "too large"}])
                    (Path(tmp) / "case-commit.json").write_bytes(b"x" * 9)
                    with self.assertRaises(ValueError):
                        store.read(fd)
                self.assertFalse(list(Path(tmp).glob("sealed-*")))
            finally:
                os.close(fd)

    def test_runner_idempotent_commit_and_conflict(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            case = root / "case"
            case.mkdir()
            info = case.stat()
            identity = (info.st_dev, info.st_ino)
            self.assertTrue(runner._write_case_rows(str(root), str(case), [{"n": 1}], identity))
            self.assertTrue(runner._write_case_rows(str(root), str(case), [{"n": 1}], identity))
            self.assertEqual(1, len(list(case.glob("sealed-*"))))
            self.assertFalse(runner._write_case_rows(str(root), str(case), [{"n": 2}], identity))
            fd = os.open(str(case), os.O_RDONLY | os.O_DIRECTORY)
            try:
                digest = store.manifest_digest(fd)
                self.assertEqual([("leaf-001.json", {"n": 1})], runner._read_case_rows(str(root), str(case), digest))
            finally:
                os.close(fd)

    def test_commit_is_all_or_nothing_and_leaves_are_hash_checked(self):
        with tempfile.TemporaryDirectory() as tmp:
            fd = os.open(tmp, os.O_RDONLY | os.O_DIRECTORY)
            try:
                original = store._write_file
                def broken(directory, name, data):
                    if name == "leaf-002.json":
                        raise OSError("injected failure")
                    return original(directory, name, data)
                with patch.object(store, "_write_file", side_effect=broken):
                    with self.assertRaises(OSError):
                        store.publish(fd, [{"n": 1}, {"n": 2}])
                with self.assertRaises(FileNotFoundError):
                    store.read(fd)
                store.publish(fd, [{"n": 1}, {"n": 2}])
                self.assertEqual([{"n": 1}, {"n": 2}], [row for _, row in store.read(fd)])
                manifest = json.loads((Path(tmp) / "case-commit.json").read_text())
                (Path(tmp) / manifest["generation"] / "leaf-001.json").write_text('{"n":3}')
                with self.assertRaises(ValueError):
                    store.read(fd)
            finally:
                os.close(fd)

    def test_fifo_rejected_and_manifest_binding_detects_rewritten_leaf(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            fd = os.open(tmp, os.O_RDONLY | os.O_DIRECTORY)
            try:
                os.mkfifo(root / "case-commit.json")
                with self.assertRaises(ValueError):
                    store.read(fd)
                (root / "case-commit.json").unlink()
                store.publish(fd, [{"n": 1}])
                digest = store.manifest_digest(fd)
                path = root / "case-commit.json"
                manifest = json.loads(path.read_text())
                leaf = root / manifest["generation"] / "leaf-001.json"
                leaf.write_text('{"n":2}')
                manifest["entries"][0]["sha256"] = store._digest(leaf.read_bytes())
                path.write_text(json.dumps(manifest))
                with self.assertRaises(ValueError):
                    store.read(fd, digest)
            finally:
                os.close(fd)

    def test_w603_rejects_legacy_receipts_and_reads_bound_case(self):
        import importlib.util
        import sys
        from unittest import mock
        scripts = Path(__file__).resolve().parents[1]
        sys.path.insert(0, str(scripts / "test"))
        sys.path.insert(0, str(scripts))
        spec = importlib.util.spec_from_file_location("ledger_adapter_test", scripts / "test" / "attachment_lifecycle_v5_adapter_w6_03_ledger_verify.py")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as tmp:
            case = Path(tmp) / "evidence" / "W1-01" / "case"
            case.mkdir(parents=True)
            (case / "leaf-001.json").write_text('{"n":1}')
            contract = {"tasks": [{"task_id": "W1-01", "case_ids": ["W1-01/case"]}]}
            import attachment_lifecycle_v5_runner as ledger_runner
            with mock.patch.object(ledger_runner, "_require_state", return_value={}):
                with self.assertRaises(SystemExit):
                    module._sealed_receipts(tmp, "test", contract)
            fd = os.open(str(case), os.O_RDONLY | os.O_DIRECTORY)
            try:
                store.publish(fd, [{"n": 1}])
                binding = store.manifest_digest(fd)
            finally:
                os.close(fd)
            with mock.patch.object(ledger_runner, "_require_state", return_value={"case_manifest_digests": {"W1-01/case": binding}}):
                self.assertEqual({"n": 1}, module._sealed_receipts(tmp, "test", contract)["W1-01/case"][0][1])

    def test_existing_commit_and_symlink_are_not_overwritten(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            target = root / "sentinel"
            target.write_text("preserve")
            (root / "case-commit.json").symlink_to(target)
            fd = os.open(tmp, os.O_RDONLY | os.O_DIRECTORY)
            try:
                with self.assertRaises(OSError):
                    store.publish(fd, [{"n": 1}])
                self.assertEqual("preserve", target.read_text())
                with self.assertRaises(OSError):
                    store.read(fd)
            finally:
                os.close(fd)
