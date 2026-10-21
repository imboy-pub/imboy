#!/usr/bin/env python3
"""Exercise the CLI boundary without running Erlang or other shared resources."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

VALIDATOR = Path(__file__).with_name('native-eunit-validate.py')


class NativeEunitSummaryTests(unittest.TestCase):
    def validate(self, text, raw_exit=0):
        with tempfile.TemporaryDirectory() as directory:
            log = Path(directory) / 'eunit.log'
            result = Path(directory) / 'result.json'
            log.write_text(text)
            Path(str(log) + '.exit').write_text(str(raw_exit))
            proc = subprocess.run([sys.executable, str(VALIDATOR), str(log),
                                   str(result)], capture_output=True, text=True)
            payload = json.loads(result.read_text())
            self.assertEqual(proc.returncode, 0 if payload['clean'] else 1)
            return payload

    def test_standard_single(self):
        self.assertEqual(self.validate('module output\nTest passed.\n')['success_summaries'], [1])
        self.assertTrue(self.validate('Test passed.')['clean'])

    def test_standard_plural(self):
        self.assertTrue(self.validate('All 12 tests passed.\n')['clean'])
        self.assertEqual(self.validate('All 12 tests passed.\n')['success_summaries'], [12])

    def test_standard_indentation_and_crlf(self):
        for text, expected in [('  Test passed.\n', [1]),
                               ('\tTest passed. \t\r\n', [1]),
                               ('  All 12 tests passed.\r\n', [12])]:
            with self.subTest(text=text):
                payload = self.validate(text)
                self.assertTrue(payload['clean'])
                self.assertEqual(payload['success_summaries'], expected)

    def test_nonzero_exit_veto(self):
        for summary in ['Test passed.', 'All 12 tests passed.']:
            with self.subTest(summary=summary):
                self.assertFalse(self.validate(summary + '\n', 2)['clean'])

    def test_all_existing_vetoes(self):
        markers = ['*failed*', '*timed out*', 'context setup failed',
                   'context cleanup failed', 'instantiation of subtests failed',
                   'One or more tests were cancelled',
                   'unexpected termination of test process',
                   'result from instantiator example is not a test',
                   'test aborted', 'ABORTED_BY_RESOURCE_GUARD=1',
                   'NOT_A_COMPLETE_FULL_RESULT', 'SIGTERM received - shutting down',
                   'SIGINT received - shutting down', 'There were no tests to run',
                   'make: ***', 'gmake[1]: ***', 'Failed: 1', 'Skipped: 1']
        for summary in ['Test passed.', 'All 12 tests passed.']:
            for marker in markers:
                with self.subTest(summary=summary, marker=marker):
                    payload = self.validate(marker + '\n' + summary + '\n')
                    self.assertFalse(payload['clean'])
                    self.assertTrue(any(payload['framework_markers'].values()))

    def test_no_or_forged_summary(self):
        for text in ['', 'test output only\n', 'All 0 tests passed.',
                     'prefix Test passed.', 'Test passed. suffix',
                     'prefix All 12 tests passed.', 'All 12 tests passed. suffix',
                     'Test passedX', 'test passed.']:
            with self.subTest(text=text):
                payload = self.validate(text)
                self.assertFalse(payload['clean'])
                self.assertEqual(payload['success_summaries'], [])


if __name__ == '__main__':
    unittest.main()
