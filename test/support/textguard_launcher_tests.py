import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


class LauncherTests(unittest.TestCase):
    def test_prepared_build_is_rejected_before_startup(self):
        launcher = Path(__file__).resolve().parents[2] / "scripts/verify_textguard_dual.py"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            output = root / "evidence"
            arguments = [sys.executable, str(launcher)]
            for option in ["cwd", "rust-cwd", "cache-source", "base-launcher",
                           "cargo-cache", "rustup-home", "prepared"]:
                arguments.extend(["--" + option, str(root)])
            arguments.extend(["--output", str(output), "--resource-clearance", "unit-test"])
            result = subprocess.run(arguments, capture_output=True, text=True, check=False)
            self.assertEqual(result.returncode, 2)
            self.assertIn("unrecognized arguments: --prepared", result.stderr)
            self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
