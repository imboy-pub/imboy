import json
import os
import signal
import subprocess
import tempfile
import time
from pathlib import Path
import unittest
from unittest.mock import patch

from scripts.test import attachment_lifecycle_v5_pg_provision as provision


class ProvisionTest(unittest.TestCase):
    def test_private_write_never_overwrites(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "receipt"
            provision.private_write(path, b"original")
            with self.assertRaises(FileExistsError):
                provision.private_write(path, b"replacement")
            self.assertEqual(b"original", path.read_bytes())

    def test_cleanup_refuses_foreign_container(self):
        cid = "a" * 64
        with patch.object(provision, "inspect", return_value={
            "Id": cid, "Config": {"Labels": {"codex.run": "other", "codex.resource_nonce": "nonce"}}
        }), patch.object(provision, "command") as command:
            with self.assertRaises(RuntimeError):
                provision.remove_created(cid, "myrun", "nonce")
            command.assert_not_called()

    def test_cleanup_refuses_even_matching_container_without_exclusive_ownership(self):
        cid = "a" * 64
        with patch.object(provision, "inspect", return_value={
            "Id": cid, "Config": {"Labels": {"codex.run": "myrun", "codex.resource_nonce": "nonce"}}
        }), patch.object(provision, "command") as command:
            with self.assertRaises(RuntimeError):
                provision.remove_created(cid, "myrun", "nonce")
            command.assert_not_called()

    def test_noncanonical_run_path_is_rejected_before_docker(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            actual = root / "actual"
            actual.mkdir()
            (actual / "resources").mkdir()
            alias = root / "alias"
            alias.symlink_to(actual, target_is_directory=True)
            with patch.object(provision, "command") as command:
                with self.assertRaises(ValueError):
                    provision.provision(str(alias), "unused")
                command.assert_not_called()

    def test_remote_docker_environment_is_rejected(self):
        with patch.dict(os.environ, {"DOCKER_HOST": "tcp://remote.example:2375"}), \
             patch.object(provision.subprocess, "run") as process:
            with self.assertRaises(ValueError):
                provision.local_docker()
            process.assert_not_called()

    def test_reconcile_is_read_only_and_rejects_ambiguous_containers(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve() / "myrun" / "resources"
            root.mkdir(parents=True)
            nonce = "a" * 32
            resource = root / ("pg-prepared-" + nonce)
            resource.mkdir()
            (resource / "intent.json").write_text(json.dumps({
                "run_id": "myrun", "nonce": nonce, "container": "attv5-" + nonce}))
            with patch.object(provision, "local_docker"), \
                 patch.object(provision, "command", return_value=("b" * 64) + "\n" + ("c" * 64)) as command:
                with self.assertRaises(RuntimeError):
                    provision.reconcile(str(resource))
                self.assertEqual("ps", command.call_args.args[0][1])

    def test_secret_cleanup_requires_identity_and_preserves_data(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            identities = {}
            for name in ("password", "ca-signing.key", "server.key"):
                path = root / name
                provision.private_write(path, b"synthetic")
                info = path.stat()
                identities[name] = [info.st_dev, info.st_ino]
            (root / "ownership.json").write_text(json.dumps({
                "run_id": "myrun", "nonce": "nonce", "files": identities}))
            (root / "pg-data").mkdir()
            (root / "pg-data" / "keep").write_text("owned-data")
            outcome = {"run_id": "myrun", "nonce": "nonce", "container_ids": []}
            with self.assertRaises(RuntimeError):
                provision.cleanup_secrets(str(root), outcome)
            self.assertEqual("owned-data", (root / "pg-data" / "keep").read_text())
            for name in identities:
                self.assertTrue((root / name).exists())

    def test_secret_cleanup_rejects_replaced_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            identities = {}
            for name in ("password", "ca-signing.key", "server.key"):
                path = root / name
                provision.private_write(path, b"synthetic")
                info = path.stat()
                identities[name] = [info.st_dev, info.st_ino]
            (root / "ownership.json").write_text(json.dumps({
                "run_id": "myrun", "nonce": "nonce", "files": identities}))
            (root / "password").unlink()
            (root / "password").symlink_to(root / "server.key")
            with self.assertRaises(RuntimeError):
                provision.cleanup_secrets(str(root), {"run_id": "myrun", "nonce": "nonce", "container_ids": []})
            self.assertEqual(b"synthetic", (root / "server.key").read_bytes())

    def test_provision_rejects_before_creating_resources(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve() / "test-run"
            root.mkdir()
            (root / "resources").mkdir()
            with self.assertRaises(RuntimeError):
                provision.provision(str(root), "unused")
            self.assertEqual([], list((root / "resources").iterdir()))


if __name__ == "__main__":
    unittest.main()
