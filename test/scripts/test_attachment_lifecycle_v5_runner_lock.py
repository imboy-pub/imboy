"""Lock acquisition must not modify redirected or replaced filesystem objects."""
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "v5_runner_lock", ROOT / "scripts/attachment_lifecycle_v5_runner.py")
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class LockIdentityTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.lock_path = self.root / ".runner.lock"

    def test_normal_lock_excludes_second_writer(self):
        first = runner.RunLock(str(self.root), wait_budget=0)
        second = runner.RunLock(str(self.root), wait_budget=0)
        self.assertTrue(first.acquire_nb())
        try:
            self.assertFalse(second.acquire_nb())
        finally:
            first.release()
        self.assertTrue(second.acquire_nb())
        second.release()

    def test_symlink_and_hardlink_are_not_modified(self):
        target = self.root / "unrelated"
        target.write_text("preserve")
        self.lock_path.symlink_to(target)
        self.assertFalse(runner.RunLock(str(self.root), wait_budget=0).acquire_nb())
        self.assertEqual("preserve", target.read_text())
        self.lock_path.unlink()
        os.link(target, self.lock_path)
        self.assertFalse(runner.RunLock(str(self.root), wait_budget=0).acquire_nb())
        self.assertEqual("preserve", target.read_text())

    def test_replacement_during_acquisition_is_rejected_before_truncate(self):
        self.lock_path.write_text("old")
        original_flock = runner.fcntl.flock
        def replace_after_lock(fd, operation):
            result = original_flock(fd, operation)
            if operation & runner.fcntl.LOCK_EX:
                self.lock_path.unlink()
                self.lock_path.write_text("new")
            return result
        with mock.patch.object(runner.fcntl, "flock", side_effect=replace_after_lock):
            self.assertFalse(runner.RunLock(str(self.root), wait_budget=0).acquire_nb())
        self.assertEqual("new", self.lock_path.read_text())

    def test_fifo_is_rejected_without_blocking_or_replacement(self):
        os.mkfifo(self.lock_path)
        script = ("import importlib.util,sys; "
                  "s=importlib.util.spec_from_file_location('r',sys.argv[1]); "
                  "m=importlib.util.module_from_spec(s); s.loader.exec_module(m); "
                  "sys.exit(1 if m.RunLock(sys.argv[2],wait_budget=0).acquire_nb() else 0)")
        proc = subprocess.run([sys.executable, "-c", script,
                               str(ROOT / "scripts/attachment_lifecycle_v5_runner.py"),
                               str(self.root)], capture_output=True, timeout=5)
        self.assertEqual(0, proc.returncode, proc.stderr)
        self.assertTrue(self.lock_path.is_fifo())

    def test_fdopen_failure_cannot_close_reused_descriptor(self):
        # A wrapper may close its input on failure, after which another thread
        # can immediately acquire that descriptor number. RunLock never hands
        # ownership to fdopen, so that ambiguous error path cannot occur.
        lock = runner.RunLock(str(self.root), wait_budget=0)
        with mock.patch.object(runner.os, "fdopen", side_effect=OSError("closed and reused")) as wrap:
            self.assertTrue(lock.acquire_nb())
            wrap.assert_not_called()
        lock_fd = lock.fd
        original_close = os.close
        reopened = []
        unrelated = self.root / "unrelated"
        unrelated.write_text("preserve")
        def close_then_reuse(fd):
            original_close(fd)
            if fd == lock_fd:
                reopened.append(os.open(unrelated, os.O_RDONLY))
        with mock.patch.object(runner.os, "close", side_effect=close_then_reuse):
            lock.release()
        self.assertEqual([lock_fd], reopened)
        try:
            self.assertEqual(b"preserve", os.read(reopened[0], 8))
        finally:
            original_close(reopened[0])


if __name__ == "__main__":
    unittest.main()
