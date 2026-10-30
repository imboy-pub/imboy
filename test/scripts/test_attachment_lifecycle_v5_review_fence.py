"""Untrusted review claims must never authorize W6-03 or G6."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import types
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "review_fence_runner", ROOT / "scripts/attachment_lifecycle_v5_runner.py")
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class ReviewFenceTests(unittest.TestCase):
    def test_self_declared_review_cannot_accept_task_or_gate(self):
        with tempfile.TemporaryDirectory() as directory:
            run = Path(directory)
            resources = run / "resources"
            resources.mkdir()
            (resources / "independent-review.json").write_text(json.dumps({
                "verdict": "PASS", "reviewer": "independent",
                "candidate": "claimed-current"}))
            state = {"plan_dir": directory,
                     "tasks": {"W6-03": {"state": "COMMITTING"}}}
            lock = mock.Mock()
            lock.acquire_nb.return_value = True
            commands = (
                (runner.cmd_accept_task,
                 types.SimpleNamespace(run_dir=directory, task_id="W6-03")),
                (runner.cmd_verify,
                 types.SimpleNamespace(run_dir=directory, gate="G6")),
            )
            for command, args in commands:
                with self.subTest(command=command.__name__), \
                     mock.patch.object(runner, "RunLock", return_value=lock), \
                     mock.patch.object(runner, "_require_state", return_value=state), \
                     mock.patch.object(runner, "load_contract", return_value={
                         "wave_gates": {"G6": {}}}), \
                     contextlib.redirect_stderr(io.StringIO()), \
                     self.assertRaises(SystemExit) as raised:
                    command(args)
                self.assertEqual(runner.EXIT_BLOCKED, raised.exception.code)
                self.assertEqual("COMMITTING", state["tasks"]["W6-03"]["state"])
            self.assertFalse((run / "events.jsonl").exists())


if __name__ == "__main__":
    unittest.main()
