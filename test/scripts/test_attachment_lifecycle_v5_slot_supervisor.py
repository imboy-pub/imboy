"""Local lock checks; these do not establish DB connection provenance."""
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "v5_slot", ROOT / "scripts/test/attachment_lifecycle_v5_slot_supervisor.py")
slot = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(slot)


class ResourceLockTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name).resolve()
        self.identity = "a" * 64

    def test_resource_collision_cross_run_and_independent_resources(self):
        first = slot.acquire_resource(self.identity, self.root)
        self.addCleanup(os.close, first)
        with self.assertRaises(BlockingIOError):
            slot.acquire_resource(self.identity, self.root)
        second = slot.acquire_resource("b" * 64, self.root)
        os.close(second)

    def test_child_inherits_lock_after_parent_closes_descriptor(self):
        fd = slot.acquire_resource(self.identity, self.root)
        child = subprocess.Popen(
            [sys.executable, "-c", "import os,sys; os.fstat(int(sys.argv[1])); "
             "print('ready',flush=True); sys.stdin.read()", str(fd)],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, pass_fds=(fd,))
        try:
            self.assertEqual("ready", child.stdout.readline().strip())
            os.close(fd)
            fd = None
            with self.assertRaises(BlockingIOError):
                slot.acquire_resource(self.identity, self.root)
            child.stdin.close()
            self.assertEqual(0, child.wait(timeout=5))
            acquired = slot.acquire_resource(self.identity, self.root)
            os.close(acquired)
        finally:
            if fd is not None:
                os.close(fd)
            if child.poll() is None:
                child.kill()
                child.wait(timeout=5)
            child.stdout.close()

    def test_reject_symlink_and_hardlink(self):
        lock_dir = self.root / ("imboy-v5-pg-locks-%d" % os.getuid())
        lock_dir.mkdir(mode=0o700)
        target = self.root / "target"
        target.write_text("keep")
        lock = lock_dir / (self.identity + ".lock")
        lock.symlink_to(target)
        with self.assertRaises(OSError):
            slot.acquire_resource(self.identity, self.root)
        self.assertEqual("keep", target.read_text())
        lock.unlink()
        os.link(target, lock)
        with self.assertRaises(ValueError):
            slot.acquire_resource(self.identity, self.root)

    def test_reject_insecure_directory_and_bad_id(self):
        lock_dir = self.root / ("imboy-v5-pg-locks-%d" % os.getuid())
        lock_dir.mkdir(mode=0o755)
        with self.assertRaises(ValueError):
            slot.acquire_resource(self.identity, self.root)
        with self.assertRaises(ValueError):
            slot.acquire_resource("../../wrong", self.root)

    def test_missing_receipt_never_runs_child(self):
        run = self.root / "run"
        (run / "resources").mkdir(parents=True)
        marker = self.root / "child-ran"
        result = subprocess.run([
            sys.executable, str(ROOT / "scripts/test/attachment_lifecycle_v5_slot_supervisor.py"),
            "--run-dir", str(run), "--slot", "slot1", "--", sys.executable, "-c",
            "from pathlib import Path; Path(%r).touch()" % str(marker)],
            capture_output=True, text=True, timeout=5)
        self.assertEqual(75, result.returncode)
        self.assertIn("receipt missing", result.stderr)
        self.assertFalse(marker.exists())

    def test_replaced_lock_cannot_authorize_exclusive_resource(self):
        original = slot.acquire_resource(self.identity, self.root)
        self.addCleanup(os.close, original)
        lock_dir = self.root / ("imboy-v5-pg-locks-%d" % os.getuid())
        lock = lock_dir / (self.identity + ".lock")
        lock.rename(lock_dir / "replaced")
        # Advisory lock files are replaceable by the same UID.  A second
        # process may hold the replacement; neither lock proves exclusivity.
        replacement = slot.acquire_resource(self.identity, self.root)
        self.addCleanup(os.close, replacement)
        self.assertNotEqual(os.fstat(original).st_ino, os.fstat(replacement).st_ino)
        with self.assertRaises(RuntimeError):
            slot.require_exclusive()

    def test_verify_refuses_real_and_forged_descriptors(self):
        run = self.root / "run"
        (run / "resources").mkdir(parents=True)
        held = slot.acquire(str(run), "slot1")
        self.addCleanup(os.close, held)
        path = run / "resources/slot-locks/slot1.lock"
        forged = os.open(path, os.O_RDONLY)
        self.addCleanup(os.close, forged)
        for descriptor in (held, forged):
            with self.subTest(descriptor=descriptor):
                with self.assertRaisesRegex(ValueError, "ownership cannot be proven"):
                    slot.verify(str(run), "slot1", descriptor)

    def test_resource_lock_released_when_holder_process_dies(self):
        code = ("import importlib.util,sys,time; "
                "s=importlib.util.spec_from_file_location('slot',sys.argv[1]); "
                "m=importlib.util.module_from_spec(s); s.loader.exec_module(m); "
                "m.acquire_resource(sys.argv[2],sys.argv[3]); "
                "print('ready',flush=True); time.sleep(60)")
        child = subprocess.Popen(
            [sys.executable, "-c", code, str(ROOT / "scripts/test/attachment_lifecycle_v5_slot_supervisor.py"),
             self.identity, str(self.root)], stdout=subprocess.PIPE, text=True)
        try:
            self.assertEqual("ready", child.stdout.readline().strip())
            with self.assertRaises(BlockingIOError):
                slot.acquire_resource(self.identity, self.root)
            child.kill()
            child.wait(timeout=5)
            fd = slot.acquire_resource(self.identity, self.root)
            os.close(fd)
        finally:
            if child.poll() is None:
                child.kill()
                child.wait(timeout=5)
            child.stdout.close()


if __name__ == "__main__":
    unittest.main()
