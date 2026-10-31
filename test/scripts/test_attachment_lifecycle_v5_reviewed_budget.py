import copy
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_runner as runner


class ReviewedBudgetTest(unittest.TestCase):
    def setUp(self):
        self.contract = {
            'tasks': [{'task_id': 'W6-01', 'wave': 'W6'}, {'task_id': 'W6-02', 'wave': 'W6'}],
            'timeouts': {'run_seconds': 20, 'wave_seconds': 10,
                         'task_seconds': 5, 'task_overrides': {'W6-01': 5}},
            'retry_policy': {'task_attempts_total_max': 5, 'task_attempts_overrides': {'W6-01': 8}},
            'reviewed_readmission': {'task_id': 'W6-01', 'attempt_limit': 8,
                                     'deadlines': {'task': 110, 'wave': 120, 'run': 130}},
        }
        self.state = {
            'reviewed_readmission': 'a' * 64,
            'tasks': {'W6-01': {'task_started_at': 1, 'task_deadline': 110, 'attempt': 5},
                      'W6-02': {'task_started_at': 1, 'task_deadline': 110, 'attempt': 5}},
            'time_budgets': {'run': {'started_at': 1, 'deadline': 130},
                             'waves': {'W6': {'started_at': 1, 'deadline': 120},
                                       'W5': {'started_at': 1, 'deadline': 11}}},
        }

    def test_missing_provenance_fails_closed_even_for_non_enforcing_check(self):
        with self.assertRaises(SystemExit) as err:
            runner._budget_check(self.state, self.contract, enforce=False)
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)
        with self.assertRaises(SystemExit) as err:
            runner._task_attempt_limit(self.state, self.contract, 'W6-01')
        self.assertEqual(runner.EXIT_BLOCKED, err.exception.code)

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_reviewed_limits_are_task_specific_and_exhaustion_is_preserved(self, _verify):
        with patch.object(runner.time, 'time', return_value=100):
            runner._budget_check(self.state, self.contract, 'W6-01')
            self.assertEqual(5, runner._task_attempt_limit(self.state, self.contract, 'W6-02'))
            for attempt in (5, 6, 7, 8):
                self.state['tasks']['W6-01']['attempt'] = attempt
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.state['tasks']['W6-01']['attempt'] = 9
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)
            self.state['tasks']['W6-02']['attempt'] = 6
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-02')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_deadline_and_scope_tampering_fails(self, _verify):
        for mutation in (
                lambda s, c: s['time_budgets']['run'].update(deadline=131),
                lambda s, c: s['time_budgets']['waves']['W6'].update(deadline=121),
                lambda s, c: s['time_budgets']['waves']['W5'].update(deadline=12),
                lambda s, c: s['tasks']['W6-01'].update(task_deadline=111),
                lambda s, c: c['retry_policy']['task_attempts_overrides'].update({'W6-02': 8}),
                lambda s, c: c['reviewed_readmission'].update(attempt_limit=9),
                lambda s, c: s.update(reviewed_readmission='invalid')):
            with self.subTest(mutation=mutation):
                state, contract = copy.deepcopy(self.state), copy.deepcopy(self.contract)
                mutation(state, contract)
                with self.assertRaises(SystemExit) as err:
                    runner._budget_check(state, contract, enforce=False)
                self.assertEqual(runner.EXIT_CONTRACT, err.exception.code)

    @patch.object(runner, '_verify_reviewed_readmission', return_value=True)
    def test_expired_reviewed_deadlines_remain_enforced(self, _verify):
        with patch.object(runner.time, 'time', return_value=120):
            with self.assertRaises(SystemExit) as err:
                runner._budget_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)
        with patch.object(runner.time, 'time', return_value=110):
            with self.assertRaises(SystemExit) as err:
                runner._task_attempt_check(self.state, self.contract, 'W6-01')
            self.assertEqual(runner.EXIT_NOGO, err.exception.code)


if __name__ == '__main__':
    unittest.main()
