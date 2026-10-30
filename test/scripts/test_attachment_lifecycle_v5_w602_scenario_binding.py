import contextlib
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts' / 'test'))
import attachment_lifecycle_v5_adapter_w6_02_support as support


class ScenarioBindingTests(unittest.TestCase):
    def report(self, specs):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        path = Path(tmp.name) / 'report.json'
        path.write_text(json.dumps({'suites': [{'title': 'suite', 'specs': specs}],
                                    'stats': {'duration': 100}}))
        with contextlib.redirect_stderr(io.StringIO()):
            return support.playwright_report(tmp.name, path.name, 'case', 'role')

    @staticmethod
    def spec(title, results=None):
        return {'title': title, 'tests': [{'status': 'expected', 'results':
            [{'status': 'passed', 'duration': 10}] if results is None else results}]}

    def test_exact_scenario_title_not_substrings_or_suite_names(self):
        suite = self.report([self.spec('widget_timeout: seat_timeout'),
                             self.spec('seat_timeout_extra')])
        self.assertEqual(1, len(support.find_playwright_test(suite, 'widget_timeout')))
        self.assertEqual([], support.find_playwright_test(suite, 'seat_timeout'))
        self.assertEqual([], support.find_playwright_test(suite, 'suite'))
        flutter = {'tests': [{'name': 'widget_timeout: seat_timeout'}]}
        self.assertEqual([], support.find_flutter_test(flutter, 'seat_timeout'))

    def test_empty_result_is_not_executed_test(self):
        with self.assertRaises(SystemExit):
            self.report([self.spec('widget_timeout', [])])

    def run_rows(self, suite, leaves):
        with mock.patch.object(support.S5, 'window', return_value=('start', 'finish')), \
             mock.patch.object(support.S6, 'assertion', return_value={}), \
             mock.patch.object(support.C, 'build_row', side_effect=lambda leaf, *args: {'leaf': leaf}), \
             mock.patch.object(support.C, 'assert_leaf_set'), \
             contextlib.redirect_stderr(io.StringIO()):
            return support.rows_for({}, 'case', leaves, '/tmp',
                {('playwright', 'disk'): suite}, {}, None, None,
                lambda leaf: ({}, 'resource'), lambda *args: {}, 'playwright')

    def test_duplicate_scenario_tests_are_rejected(self):
        suite = self.report([self.spec('widget_timeout'), self.spec('widget_timeout')])
        with self.assertRaises(SystemExit):
            self.run_rows(suite, [{'driver': 'disk', 'ordering_or_fault': 'x/widget_timeout'}])

    def test_same_executed_test_cannot_cover_two_leaves(self):
        suite = self.report([self.spec('widget_timeout')])
        leaves = [{'driver': 'disk', 'ordering_or_fault': 'x/widget_timeout'},
                  {'driver': 'disk', 'ordering_or_fault': 'y/widget_timeout'}]
        with self.assertRaises(SystemExit):
            self.run_rows(suite, leaves)

    def test_distinct_explicit_scenarios_match(self):
        suite = self.report([self.spec('widget_timeout: retries safely'),
                             self.spec('seat_timeout')])
        leaves = [{'driver': 'disk', 'ordering_or_fault': 'x/widget_timeout'},
                  {'driver': 'disk', 'ordering_or_fault': 'x/seat_timeout'}]
        self.assertEqual(2, len(self.run_rows(suite, leaves)))


if __name__ == '__main__':
    unittest.main()
