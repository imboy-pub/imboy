import importlib.util
import json
import pathlib
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[2]
SCRIPT_DIR = ROOT / "scripts" / "test"
sys.path.insert(0, str(SCRIPT_DIR))
SPEC = importlib.util.spec_from_file_location(
    "w603_ledger_guard", SCRIPT_DIR / "attachment_lifecycle_v5_adapter_w6_03_ledger_verify.py")
adapter = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(adapter)


class LedgerGuardTests(unittest.TestCase):
    def setUp(self):
        self.contract = {"tasks": [{"task_id": "W0-01"}, {"task_id": "W6-03"}]}
        self.state = {"tasks": {"W0-01": {"state": "PASS"},
                                "W6-03": {"state": "RUNNING"}}}

    def test_prior_tasks_require_exact_nonempty_contract_and_states(self):
        self.assertEqual([], adapter._prior_task_errors(self.contract, self.state))
        for state in ({}, {"tasks": {}}, {"tasks": {"W6-03": {"state": "RUNNING"}}},
                      {"tasks": {"W0-01": {"state": "PASS"}, "W6-03": {}, "extra": {}}},
                      {"tasks": {"W0-01": {"state": "RUNNING"}, "W6-03": {}}}):
            with self.subTest(state=state):
                self.assertTrue(adapter._prior_task_errors(self.contract, state))
        self.assertTrue(adapter._prior_task_errors({"tasks": []}, self.state))
        self.assertTrue(adapter._prior_task_errors({"tasks": self.contract["tasks"] * 2}, self.state))

    def test_parallel_tests_require_every_key_and_true(self):
        good = {key: True for key in adapter.PARALLEL_REQUIRED}
        self.assertEqual([], adapter._parallel_selftest_errors(good))
        missing = dict(good)
        missing.pop(next(iter(missing)))
        for value in ({}, missing, {**good, "extra": True},
                      {**good, "second-writer-refused": 1}):
            with self.subTest(value=value):
                self.assertTrue(adapter._parallel_selftest_errors(value))

    def test_review_file_cannot_self_attest_independence_or_candidate(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = pathlib.Path(tmp) / "review.json"
            self.assertTrue(adapter._independent_review_errors(str(path)))
            for value in ({}, {"verdict": "FAIL"},
                          {"verdict": "PASS", "candidate": "claimed", "reviewer": "independent"}):
                path.write_text(json.dumps(value), encoding="utf-8")
                with self.subTest(value=value):
                    self.assertTrue(adapter._independent_review_errors(str(path)))
            path.write_text("{", encoding="utf-8")
            self.assertTrue(adapter._independent_review_errors(str(path)))


if __name__ == "__main__":
    unittest.main()
