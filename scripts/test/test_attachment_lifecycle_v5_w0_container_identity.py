import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
ADAPTERS = ("w0_02_schema", "w0_02_history_unknown", "w0_03_strict_ledger_bootstrap")


class ContainerIdentityTest(unittest.TestCase):
    def test_exec_uses_verified_full_id_not_replaceable_name(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp) / "run"
            (run / "resources").mkdir(parents=True)
            container_id = "a" * 64
            receipt = {"container": "attv5-owned", "container_id": container_id,
                       "owner_run_id": "run"}
            (run / "resources/postgres.json").write_text(json.dumps(receipt))
            inspected = {"Id": container_id, "Running": True,
                         "Labels": {"codex.run": "run", "codex.role": "attachment-lifecycle-pg"}}
            for name in ADAPTERS:
                with self.subTest(adapter=name), patch.object(sys, "argv", ["adapter", str(run), tmp, str(ROOT)]):
                    path = ROOT / "scripts/test" / ("attachment_lifecycle_v5_adapter_" + name + ".py")
                    spec = importlib.util.spec_from_file_location("tested_" + name, path)
                    module = importlib.util.module_from_spec(spec)
                    spec.loader.exec_module(module)
                    with patch.object(module.subprocess, "check_output", return_value=json.dumps(inspected)):
                        target = module.owned_container({"postgres_container": ["attv5-owned"]})
                    self.assertEqual(container_id, target)
                    with patch.object(module.subprocess, "run") as command:
                        command.return_value.returncode = 0
                        command.return_value.stdout = "ok\n"
                        command.return_value.stderr = ""
                        module.psql(target, "postgres", "fixture-password", "SELECT 1")
                    self.assertEqual(container_id, command.call_args.args[0][5])
                    with patch.object(module.subprocess, "check_output", return_value=json.dumps(dict(inspected, Id="a" * 12))):
                        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                            module.owned_container({"postgres_container": ["attv5-owned"]})


if __name__ == "__main__":
    unittest.main()
