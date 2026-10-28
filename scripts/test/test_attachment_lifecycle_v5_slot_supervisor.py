import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from scripts.test import attachment_lifecycle_v5_slot_supervisor as slot


class SlotSupervisorTest(unittest.TestCase):
    def test_lock_replaced_during_acquisition_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            lock_path = root / "resources" / "slot-locks" / "slot1.lock"
            actual_flock = slot.fcntl.flock

            def replace_after_lock(fd, operation):
                actual_flock(fd, operation)
                lock_path.rename(lock_path.with_suffix(".old"))
                lock_path.touch(mode=0o600)

            with patch.object(slot.fcntl, "flock", side_effect=replace_after_lock):
                with self.assertRaisesRegex(ValueError, "changed while acquiring"):
                    slot.acquire(str(root), "slot1")

    def test_lock_survives_supervisor_death_while_gate_holds_fd(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            program = Path(slot.__file__)
            worker = "import os,time; print(os.getpid(),flush=True); time.sleep(30)"
            owner = subprocess.Popen([sys.executable, str(program), "--run-dir", str(root),
                                      "--slot", "slot1", "--", sys.executable, "-c", worker],
                                     stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            child = None
            try:
                child = int(owner.stdout.readline().strip())
                with self.assertRaises(BlockingIOError):
                    slot.acquire(str(root), "slot1")
                owner.kill()
                owner.wait(timeout=5)
                with self.assertRaises(BlockingIOError):
                    slot.acquire(str(root), "slot1")
            finally:
                if owner.poll() is None:
                    owner.kill()
                    owner.wait(timeout=5)
                if child:
                    os.kill(child, signal.SIGTERM)
                owner.stdout.close()
                owner.stderr.close()

    def test_symlink_lock_is_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            locks = root / "resources" / "slot-locks"
            locks.mkdir(parents=True, mode=0o700)
            foreign = root / "foreign"
            foreign.write_text("keep")
            (locks / "slot1.lock").symlink_to(foreign)
            with self.assertRaises(OSError):
                slot.acquire(str(root), "slot1")
            self.assertEqual("keep", foreign.read_text())

    def test_verify_rejects_replaced_inode(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            fd = slot.acquire(str(root), "slot1")
            try:
                with self.assertRaisesRegex(ValueError, "ownership cannot be proven"):
                    slot.verify(str(root), "slot1", fd)
                path = root / "resources" / "slot-locks" / "slot1.lock"
                path.rename(path.with_suffix(".old"))
                path.touch(mode=0o600)
                with self.assertRaises(ValueError):
                    slot.verify(str(root), "slot1", fd)
            finally:
                os.close(fd)

    def test_verify_rejects_matching_but_unlocked_descriptor(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            fd = slot.acquire(str(root), "slot1")
            os.close(fd)
            path = root / "resources" / "slot-locks" / "slot1.lock"
            fd = os.open(path, os.O_RDWR | os.O_NOFOLLOW)
            try:
                with self.assertRaisesRegex(ValueError, "ownership cannot be proven"):
                    slot.verify(str(root), "slot1", fd)
                other = os.open(path, os.O_RDWR | os.O_NOFOLLOW)
                try:
                    slot.fcntl.flock(other, slot.fcntl.LOCK_EX | slot.fcntl.LOCK_NB)
                finally:
                    os.close(other)
            finally:
                os.close(fd)

    def test_cross_run_locks_do_not_prove_shared_resource_exclusivity(self):
        with tempfile.TemporaryDirectory() as tmp:
            roots = [Path(tmp).resolve() / name for name in ("run-a", "run-b")]
            for root in roots:
                (root / "resources").mkdir(parents=True)
            fds = [slot.acquire(str(root), "slot1") for root in roots]
            try:
                with self.assertRaises(RuntimeError):
                    slot.require_exclusive()
            finally:
                for fd in fds:
                    os.close(fd)

    def test_replaced_inode_cannot_release_resource_gate(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            first = slot.acquire(str(root), "slot1")
            path = root / "resources" / "slot-locks" / "slot1.lock"
            path.rename(path.with_suffix(".old"))
            second = slot.acquire(str(root), "slot1")
            try:
                with self.assertRaises(RuntimeError):
                    slot.require_exclusive()
            finally:
                os.close(first)
                os.close(second)

    def test_descendant_without_fd_cannot_release_resource_gate(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            (root / "resources").mkdir()
            code = ("import subprocess,sys; "
                    "p=subprocess.Popen([sys.executable,'-c','import time;time.sleep(30)'],close_fds=True); "
                    "print(p.pid,flush=True)")
            owner = subprocess.Popen([sys.executable, str(Path(slot.__file__)), "--run-dir", str(root),
                                      "--slot", "slot1", "--", sys.executable, "-c", code],
                                     stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            child = None
            try:
                child = int(owner.stdout.readline().strip())
                self.assertEqual(0, owner.wait(timeout=5))
                fd = slot.acquire(str(root), "slot1")
                try:
                    with self.assertRaises(RuntimeError):
                        slot.require_exclusive()
                finally:
                    os.close(fd)
            finally:
                if owner.poll() is None:
                    owner.kill()
                    owner.wait(timeout=5)
                if child:
                    os.kill(child, signal.SIGTERM)
                owner.stdout.close()
                owner.stderr.close()


if __name__ == "__main__":
    unittest.main()
