import importlib.util
import os
from pathlib import Path
import tempfile
import unittest

MODULE = Path(__file__).with_name("attachment_lifecycle_v5_adapter_w6_03_ledger_verify.py")
spec = importlib.util.spec_from_file_location("ledger_actor_test", MODULE)
ledger = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ledger)


class LedgerActorTests(unittest.TestCase):
    def test_ac08_requires_physical_device_but_not_browser(self):
        allow = {"worker": ["worker-1"], "device_provenance": ["phone-1"],
                 "browser_provenance": ["browser-1"]}
        row = {"actor_provenance": {"kind": "real", "resource_kind": "worker",
                                    "resource_id": "worker-1"},
               "resource_ids": ["worker-1"],
               "device_provenance": {"kind": "physical", "resource_id": "phone-1", "process_id": 1}}
        def errors(cid):
            return ledger._actor_provenance_errors({cid: [("leaf", row, "path")]}, allow)
        self.assertEqual([], errors("W6-02/ac08"))
        allow["device_provenance"] = "phone-1"
        self.assertTrue(errors("W6-02/ac08"))
        allow["device_provenance"] = ["phone-1"]
        self.assertTrue(errors("W6-02/e2e-compat"))
        del row["device_provenance"]
        self.assertTrue(errors("W6-02/ac08"))
        row["browser_provenance"] = {"kind": "real-browser", "resource_id": "browser-1", "process_id": 1}
        self.assertEqual([], errors("W6-02/admin-recovery-real"))
        row["browser_provenance"]["process_id"] = 0
        self.assertTrue(errors("W6-02/admin-recovery-real"))

    def test_illegal_allowlist_types_cannot_authorize_actor(self):
        row = {"actor_provenance": {"kind": "real", "resource_kind": "worker",
                                    "resource_id": "worker-1"},
               "resource_ids": ["worker-1"]}
        sealed = {"case": [("leaf", row, "path")]}
        for invalid in (17, True, {}, None, [], "", ["worker-1", 1]):
            with self.subTest(invalid=invalid):
                self.assertTrue(ledger._actor_provenance_errors(sealed, {"worker": invalid}))

    def test_non_disk_prefix_strings_do_not_authorize_actor(self):
        for kind in ("scratch_postgres", "garage_bucket", "device_provenance", "worker"):
            with self.subTest(kind=kind):
                row = {"actor_provenance": {"kind": "real", "resource_kind": kind,
                                            "resource_id": "owned-1"},
                       "resource_ids": ["owned-1"]}
                sealed = {"case": [("leaf", row, "path")]}
                self.assertTrue(ledger._actor_provenance_errors(sealed, {kind: "owned"}))
                self.assertTrue(ledger._actor_provenance_errors(sealed, {kind: ["owned"]}))
                self.assertEqual([], ledger._actor_provenance_errors(sealed, {kind: ["owned-1"]}))

    def test_disk_root_realpath_is_required(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            root = base / "owned"
            root.mkdir()
            child = root / "run"
            child.mkdir()
            outside = base / "outside"
            outside.mkdir()
            (root / "escape").symlink_to(outside, target_is_directory=True)
            allow = {"disk_root": str(root)}
            row = {"actor_provenance": {"kind": "real", "driver": "disk",
                                        "resource_kind": "disk_root", "resource_id": str(child)},
                   "driver": "disk", "resource_ids": [str(child)]}
            def errors():
                return ledger._actor_provenance_errors({"case": [("leaf", row, "path")]}, allow)
            self.assertEqual([], errors())
            for path in (outside, root / "escape", base / "owned-other"):
                row["actor_provenance"]["resource_id"] = str(path)
                row["resource_ids"] = [str(path)]
                self.assertTrue(errors())


if __name__ == "__main__":
    unittest.main()
