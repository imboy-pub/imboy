import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class NativeCaptureTest(unittest.TestCase):
    def test_file_credentials_redacted_and_child_exit_preserved(self):
        script = Path(__file__).resolve().parents[2] / "scripts/ci/native-eunit-capture.py"
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            key = "synthetic-textguard-test-credential"
            secret = root / "check.secret"
            secret.write_text(key)
            raw, public = root / "raw.log", root / "public.log"
            env = os.environ.copy()
            env["TEXTGUARD_CHECK_SECRET_FILE"] = str(secret)
            env.pop("TEXTGUARD_PUBLISH_SECRET_FILE", None)
            result = subprocess.run(
                [sys.executable, str(script), str(raw), str(public), sys.executable,
                 "-c", "import sys; print(sys.argv[1]); sys.exit(17)", key],
                env=env, capture_output=True, text=True, check=False,
            )
            self.assertEqual(17, result.returncode)
            self.assertIn(key, raw.read_text())
            self.assertNotIn(key, public.read_text())
            self.assertNotIn(key, result.stdout)
            self.assertIn("[REDACTED_SYNTHETIC_CREDENTIAL]", result.stdout)
            self.assertEqual("17\n", Path(str(raw) + ".exit").read_text())


if __name__ == "__main__":
    unittest.main()
