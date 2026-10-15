"""Known failures and retries must not green a failed full business suite."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class FullEunitGateTest(unittest.TestCase):
    def test_raw_exit_and_summary(self):
        cases = [('All 2 tests passed.', 0, True),
                 ('[organization_admin_invitation_cancelled]\nexpected request timed out\nAll 2 tests passed.', 0, True),
                 ('suite:case...*timed out*\nAll 2 tests passed.', 0, False),
                 ('All 2 tests passed.', 2, False),
                 ('Failed: 1. Skipped: 0. Passed: 2.', 0, False),
                 ('All 2 tests passed.\nSkipped: 1.', 0, False),
                 ('All 2 tests passed.\nOne or more tests were cancelled.', 0, False),
                 ('All 2 tests passed.\nSIGTERM received - shutting down', 0, False),
                 ('All 2 tests passed.\nABORTED_BY_RESOURCE_GUARD=1', 0, False),
                 ('All 2 tests passed.\ngmake: *** [eunit] Terminated: 15', 0, False),
                 ('', 0, False)]
        with tempfile.TemporaryDirectory() as tmp:
            for raw, rc, succeeds in cases:
                with self.subTest(raw=raw, rc=rc):
                    producer = Path(tmp) / 'producer.sh'
                    producer.write_text("printf '%s\\n' '" + raw + "'\nexit " + str(rc) + '\n')
                    result = subprocess.run(['bash', str(ROOT / '.qa/nightly-gate.sh'), 'full-eunit',
                                             str(Path(tmp) / 'raw.log'), '--', 'bash', str(producer)],
                                            cwd=tmp, capture_output=True, text=True)
                    self.assertEqual(result.returncode == 0, succeeds, result.stdout + result.stderr)
                    self.assertFalse((Path(tmp) / 'raw.log.retry').exists())
