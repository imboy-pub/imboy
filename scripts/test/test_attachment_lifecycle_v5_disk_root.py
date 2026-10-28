import contextlib
import io
import os
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from scripts.test import attachment_lifecycle_v5_adapter_w4_support as support
from scripts.test import attachment_lifecycle_v5_adapter_w4_models_w403 as models


class DiskRootTest(unittest.TestCase):
    def test_resolved_root_stays_inside_allowlist(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve() / "owned"
            root.mkdir()
            child = root / "child"
            child.mkdir()
            outside = Path(tmp) / "outside"
            outside.mkdir()
            (root / "escape").symlink_to(outside, target_is_directory=True)
            ctx = {"allowlist": {"disk_root": str(root)}}
            with patch.dict(os.environ, {"V5_DISK_ROOT": str(child)}):
                self.assertEqual(str(child.resolve()), support.resolve_disk_root(ctx, "W4-03"))
            with patch.dict(os.environ, {"V5_DISK_ROOT": str(root / "escape")}):
                with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                    support.resolve_disk_root(ctx, "W4-03")
            alias = Path(tmp).resolve() / "root-alias"
            alias.symlink_to(outside, target_is_directory=True)
            with patch.dict(os.environ, {"V5_DISK_ROOT": str(alias)}):
                with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                    support.resolve_disk_root({"allowlist": {"disk_root": str(alias)}}, "W4-03")
                fixture = SimpleNamespace(ctx={"allowlist": {"disk_root": str(alias)}, "run_dir": tmp})
                with patch.object(models.base, "resolve_slot", return_value="slot1"), \
                     patch.object(models.base, "db_resource", return_value="pool"):
                    provenance, problems = models._provenance(fixture)
                self.assertIsNone(provenance["disk_root"])
                self.assertIn("allowlisted disk_root contains a symbolic link", problems)
            fixture = SimpleNamespace(ctx={"allowlist": {"disk_root": str(root)}, "run_dir": tmp})
            for invalid in (outside, root / "missing"):
                with self.subTest(invalid=invalid), \
                     patch.dict(os.environ, {"V5_DISK_ROOT": str(invalid)}), \
                     patch.object(models.base, "resolve_slot", return_value="slot1"), \
                     patch.object(models.base, "db_resource", return_value="pool"):
                    provenance, problems = models._provenance(fixture)
                    self.assertIsNone(provenance["disk_root"])
                    self.assertTrue(problems)


if __name__ == "__main__":
    unittest.main()
