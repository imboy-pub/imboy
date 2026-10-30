import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
HELPER = ROOT / "scripts/test/attachment_lifecycle_v5_flutter_command.sh"


class FlutterCommandTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        flutter = root / "flutter"
        flutter.write_text('#!/usr/bin/env python3\nimport json,os,sys\n'
                           'open(os.environ["ARGV_CAPTURE"], "w").write(json.dumps(sys.argv[1:]))\n')
        flutter.chmod(0o700)
        self.capture = root / "argv.json"
        self.env = dict(os.environ, PATH=f"{root}:{os.environ['PATH']}",
                        ARGV_CAPTURE=str(self.capture),
                        W602_BACKEND_URL="http://127.0.0.1:9800",
                        W602_E2E_RUN_ID="v5-e2e-test-1",
                        W602_E2E_UPLOAD_ID="a1234567-1234-4123-8123-123456789012",
                        W602_E2E_UID="synthetic-user")

    def run_command(self, driver="garage"):
        return subprocess.run(["bash", "-c", 'source "$1"; v5_w602_flutter_command "$2" "$3"',
                               "test", str(HELPER), driver, "synthetic-device"],
                              env=self.env, capture_output=True, text=True)

    def test_actual_argv_contains_all_separate_defines(self):
        result = self.run_command()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "")
        args = json.loads(self.capture.read_text())
        self.assertEqual(args[:6], ["test", "integration_test/attachment_lifecycle_v5_test.dart",
                                    "-d", "synthetic-device", "--reporter", "json"])
        self.assertEqual(args[6:], ["--dart-define=APP_ENV=local",
                                    "--dart-define=V5_E2E_OPT_IN=true",
                                    "--dart-define=API_BASE_URL_OVERRIDE=http://127.0.0.1:9800",
                                    "--dart-define=V5_E2E_RUN_ID=v5-e2e-test-1",
                                    "--dart-define=V5_E2E_UPLOAD_ID=a1234567-1234-4123-8123-123456789012",
                                    "--dart-define=V5_E2E_UID=synthetic-user",
                                    "--dart-define=V5_E2E_DRIVER=garage"])

    def test_missing_or_invalid_inputs_fail_without_launch_or_values_in_error(self):
        for key, value in [("W602_BACKEND_URL", "https://other.example/secret"),
                           ("W602_E2E_RUN_ID", "wrong-run"),
                           ("W602_E2E_UPLOAD_ID", "not-uuid"),
                           ("W602_E2E_UID", "")]:
            old = self.env[key]
            self.env[key] = value
            result = self.run_command()
            self.assertEqual(result.returncode, 75)
            self.assertFalse(self.capture.exists())
            self.assertEqual(result.stderr, "BLOCKED_EXTERNAL: missing or invalid W6-02 Flutter inputs\n")
            self.env[key] = old
        self.assertEqual(self.run_command("other").returncode, 75)
        self.assertFalse(self.capture.exists())


if __name__ == "__main__":
    unittest.main()
