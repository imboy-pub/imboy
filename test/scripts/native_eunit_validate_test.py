#!/usr/bin/env python3
"""Official small-suite summaries stay compatible with fail-closed validation."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[2] / 'scripts/ci/native-eunit-validate.py'


class NativeValidateTests(unittest.TestCase):
    def validate(self, text, rc=0):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            log = root / 'test.log'
            log.write_text(text)
            Path(str(log) + '.exit').write_text(str(rc))
            output = root / 'summary.json'
            result = subprocess.run([sys.executable, str(SCRIPT), str(log), str(output)],
                                    capture_output=True, text=True)
            return result.returncode, json.loads(output.read_text())

    def test_official_summary_forms(self):
        for line, count in [('  Test passed.\n', 1), ('  2 tests passed.\n', 2),
                            ('  All 26 tests passed.\n', 26)]:
            with self.subTest(line=line):
                rc, summary = self.validate(line)
                self.assertEqual(rc, 0)
                self.assertTrue(summary['clean'])
                self.assertEqual(summary['success_summaries'], [count])

    def test_nonzero_exit_or_framework_error_still_fails(self):
        for marker in ['*failed*', '*timed out*', 'context setup failed',
                       'context cleanup failed', 'instantiation of subtests failed',
                       'One or more tests were cancelled', 'unexpected termination of test process',
                       'Failed: 1', 'Skipped: 1', 'There were no tests to run']:
            with self.subTest(marker=marker):
                rc, summary = self.validate('  Test passed.\n' + marker + '\n')
                self.assertNotEqual(rc, 0)
                self.assertFalse(summary['clean'])
        self.assertNotEqual(self.validate('  Test passed.\n', rc=2)[0], 0)

    def test_no_summary_or_embedded_claim_is_not_complete(self):
        for text in ['', 'RAW_EXIT=0\n', '  0 tests passed.\n', 'log says All 26 tests passed.\n']:
            with self.subTest(text=text):
                self.assertNotEqual(self.validate(text)[0], 0)


if __name__ == '__main__':
    unittest.main()
