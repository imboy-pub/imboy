"""Admission-negative tests only; no databases, credentials or infrastructure.
仅验证准入拒绝边界；不创建数据库、凭据或基础设施。
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import socket
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("native_v5", ROOT / "scripts/ci/native-v5-fixture.py")
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)


class NativeV5AdmissionTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.directory = Path(self.tmp.name)
        self.proposal = {"container_id": socket.gethostname().ljust(64, "0"),
                         "container_image_id": "synthetic-image-no-runtime",
                         "pg": {"host": "127.0.0.1", "port": 55432,
                                "user": "imboy_ci", "system_identifier": "synthetic",
                                "exact_database_allowlist": {"V5_INTTEST": "attv5_slot_case"}}}
        self.write("proposal.json", self.proposal)
        digest = hashlib.sha256((self.directory / "proposal.json").read_bytes()).hexdigest()
        self.approval = {"decision": "APPROVED_BY_USER", "proposal_sha256": digest}
        self.write("approval.json", self.approval)
        names = ["scripts/ci/native-v5-fixture.py", "test/common/native_v5_fixture.erl",
                 "test/common/inttest_marker_db.erl", "test/ds/moment_delete_atomic_pg_tests.erl"]
        pid = os.getpid()
        self.receipt = {"proposal_path": str(self.directory / "proposal.json"),
                        "approval_path": str(self.directory / "approval.json"),
                        "proposal_sha256": digest, "mode": "EUNIT_ONLY", "owner_token": "unit-only",
                        "lease_until": time.time() + 60, "controller_pid": pid,
                        "controller_start": Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()[19],
                        "container_id": self.proposal["container_id"],
                        "container_image_id": self.proposal["container_image_id"],
                        "pg_system_identifier": "synthetic", "source_root": str(ROOT),
                        "source_files": {n: hashlib.sha256((ROOT / n).read_bytes()).hexdigest() for n in names}}
        self.write("receipt.json", self.receipt)

    def write(self, name, value):
        path = self.directory / name
        path.write_text(json.dumps(value))
        path.chmod(0o600)

    def check(self, **target):
        return guard.admission(str(self.directory / "receipt.json"), **target)

    def test_exact_admission_metadata(self):
        self.assertEqual("attv5_slot_case", self.check(prefix="V5_INTTEST")[2])

    def test_prefix_is_not_database_authorization(self):
        with self.assertRaises(ValueError):
            self.check(database="attv5_slot_case_other")

    def test_changed_proposal_rejected(self):
        self.proposal["pg"]["port"] = 5432
        self.write("proposal.json", self.proposal)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_unapproved_rejected(self):
        self.approval["decision"] = "PENDING"
        self.write("approval.json", self.approval)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_expired_controller_rejected(self):
        self.receipt["lease_until"] = time.time() - 1
        self.write("receipt.json", self.receipt)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_recycled_pid_rejected(self):
        self.receipt["controller_start"] += "0"
        self.write("receipt.json", self.receipt)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_cluster_identity_rejected(self):
        self.receipt["pg_system_identifier"] = "another-cluster"
        self.write("receipt.json", self.receipt)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_source_change_rejected(self):
        self.receipt["source_files"]["test/common/native_v5_fixture.erl"] = "0" * 64
        self.write("receipt.json", self.receipt)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_missing_guard_manifest_rejected(self):
        self.receipt["source_files"] = {}
        self.write("receipt.json", self.receipt)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")

    def test_public_receipt_rejected(self):
        (self.directory / "receipt.json").chmod(0o644)
        with self.assertRaises(ValueError):
            self.check(prefix="V5_INTTEST")


if __name__ == "__main__":
    unittest.main()
