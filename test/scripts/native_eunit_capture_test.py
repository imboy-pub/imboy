import os
import select
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class NativeCaptureTest(unittest.TestCase):
    def test_progress_is_published_before_child_exits(self):
        script = Path(__file__).resolve().parents[2] / "scripts/ci/native-eunit-capture.py"
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            env = os.environ.copy()
            env.pop("TEXTGUARD_CHECK_SECRET_FILE", None)
            env.pop("TEXTGUARD_PUBLISH_SECRET_FILE", None)
            child = subprocess.Popen(
                [sys.executable, str(script), str(root / "raw.log"),
                 str(root / "public.log"), sys.executable, "-c",
                 "import sys; print('PROGRESS_READY\\n' + 'x' * 4096, flush=True); "
                 "sys.stdin.readline(); sys.exit(17)"],
                env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            try:
                ready, _, _ = select.select([child.stdout], [], [], 5)
                self.assertTrue(ready, "capture buffered progress until child exit")
                self.assertEqual(b"PROGRESS_READY\n", child.stdout.readline())
                self.assertIsNone(child.poll())
            finally:
                child.stdin.write(b"finish\n")
                child.stdin.close()
                child.wait(timeout=5)
                child.stdout.close()
                child.stderr.close()
            self.assertEqual(17, child.returncode)
            self.assertEqual("17\n", (root / "raw.log.exit").read_text())

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
