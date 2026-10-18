import importlib.util
import json
import os
from pathlib import Path
import secrets
import sys
import tempfile
from types import SimpleNamespace
import unittest


SPEC = importlib.util.spec_from_file_location(
    "textguard_dual", Path(__file__).resolve().parents[2] / "scripts/verify_textguard_dual.py")
DUAL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(DUAL)


class PrivacyTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        root = Path(self.temporary.name)
        base = SimpleNamespace(Run=object,
            save=lambda path, value: path.write_text(json.dumps(value)))
        runtime = DUAL.run_type(base)
        self.run = runtime.__new__(runtime)
        self.run.root = self.run.out = self.run.rust = root
        self.run.env = dict(os.environ)
        self.secret = secrets.token_urlsafe(32)
        self.run.redactions = [self.secret]
        self.run.raw_privacy = []
        self.run.commands = []
        self.run.identity = lambda: {"test": "runner privacy harness"}
        self.run.identity_before = self.run.identity()
        self.run.rust_before = self.run.shared_before = {}
        self.run.tests = ["test-harness-only"]
        self.run.execution = {"passed": True, "cases": [
            {"id": "test-harness-only", "acceptance": DUAL.ACCEPTANCE, "outcome": "passed"}]}
        self.run.dependency_proof = {"registry_proven": True}
        self.run.liveness = self.run.proto_proof = None
        self.run.args = SimpleNamespace(cache_source=root, prepare_only=False,
            expected_sha="test-only", expected_tree="test-only", rust_sha="test-only", rust_tree="test-only")

    def assert_gate(self, expected):
        self.assertEqual(expected, self.run.finish(None, True))
        for path in self.run.out.iterdir():
            self.assertNotIn(self.secret.encode(), path.read_bytes())

    def test_clean_output_passes(self):
        self.run.command("clean", [sys.executable, "-c", "print(123)"])
        self.assert_gate(0)

    def test_command_secret_fails_before_redaction(self):
        self.run.env["TG_UNIT_SECRET"] = self.secret
        self.run.command("injected", [sys.executable, "-c",
            "import os; print(os.environ[\"TG_UNIT_SECRET\"])"])
        self.assertIn(b"<REDACTED>", (self.run.out / "injected.log").read_bytes())
        self.assert_gate(1)

    def test_rust_log_secret_fails_before_redaction(self):
        path = self.run.out / "server.log"
        path.write_bytes(self.secret.encode())
        self.run.private_log("rust-server", path)
        self.assertEqual(b"<REDACTED>", path.read_bytes())
        self.assert_gate(1)

    def test_plaintext_canary_fails(self):
        path = self.run.out / "server.log"
        path.write_bytes(b"TG20-CANARY-NO-LOG")
        self.run.private_log("rust-server", path)
        self.assert_gate(1)


if __name__ == "__main__":
    unittest.main()
