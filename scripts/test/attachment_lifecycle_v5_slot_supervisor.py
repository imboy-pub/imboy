#!/usr/bin/env python3
"""Hold a cooperative per-run slot lock for a gate and its inheriting children."""
import argparse
import fcntl
import os
from pathlib import Path
import re
import stat
import subprocess
import sys


def acquire(run_dir, slot):
    root = Path(run_dir)
    if not root.is_absolute() or root.resolve() != root or not root.is_dir():
        raise ValueError("run directory must be canonical and existing")
    if not re.fullmatch(r"[a-z][a-z0-9_]{0,31}", slot):
        raise ValueError("invalid slot")
    resources = root / "resources"
    if resources.resolve() != resources or not resources.is_dir():
        raise ValueError("canonical resources directory required")
    lock_dir = resources / "slot-locks"
    lock_dir.mkdir(mode=0o700, exist_ok=True)
    info = lock_dir.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("slot lock directory not private")
    directory = os.open(lock_dir, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        fd = os.open(slot + ".lock", os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600, dir_fd=directory)
        try:
            held = os.fstat(fd)
            path = os.stat(slot + ".lock", dir_fd=directory, follow_symlinks=False)
            if (not stat.S_ISREG(held.st_mode) or held.st_uid != os.getuid()
                    or held.st_nlink != 1 or held.st_mode & 0o077
                    or (held.st_dev, held.st_ino) != (path.st_dev, path.st_ino)):
                raise ValueError("slot lock identity invalid")
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            path = os.stat(slot + ".lock", dir_fd=directory, follow_symlinks=False)
            if (held.st_dev, held.st_ino) != (path.st_dev, path.st_ino):
                raise ValueError("slot lock identity changed while acquiring")
            return fd
        except BaseException:
            os.close(fd)
            raise
    finally:
        os.close(directory)


def verify(run_dir, slot, fd):
    root = Path(run_dir)
    if (not root.is_absolute() or root.resolve() != root
            or not re.fullmatch(r"[a-z][a-z0-9_]{0,31}", slot)):
        raise ValueError("invalid run or slot")
    path = root / "resources" / "slot-locks" / (slot + ".lock")
    held = os.fstat(fd)
    found = path.lstat()
    if (not stat.S_ISREG(found.st_mode) or found.st_nlink != 1
            or found.st_uid != os.getuid() or found.st_mode & 0o077
            or (held.st_dev, held.st_ino) != (found.st_dev, found.st_ino)):
        raise ValueError("inherited slot lock identity changed")
    # flock on an arbitrary descriptor would acquire the lock, not prove who
    # originally acquired it. Do not authorize until ownership is attestable.
    raise ValueError("supervisor lock ownership cannot be proven")


def require_exclusive():
    """A per-run flock alone cannot prove exclusive ownership of resources."""
    raise RuntimeError("resource identity and all worker lifetimes are not bound to the slot lock")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", required=True)
    parser.add_argument("--slot", required=True)
    parser.add_argument("--verify-fd", type=int)
    parser.add_argument("--require-exclusive", action="store_true")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if args.require_exclusive:
        try:
            require_exclusive()
        except RuntimeError as exc:
            print("BLOCKED_EXTERNAL: %s" % exc, file=sys.stderr)
            return 75
    if args.verify_fd is not None:
        try:
            verify(args.run_dir, args.slot, args.verify_fd)
            return 0
        except (OSError, ValueError) as exc:
            print("CONTRACT_CONFLICT: %s" % exc, file=sys.stderr)
            return 20
    if not command:
        return 2
    try:
        fd = acquire(args.run_dir, args.slot)
    except BlockingIOError:
        print("BLOCKED_EXTERNAL: slot already in use", file=sys.stderr)
        return 75
    except (OSError, ValueError) as exc:
        print("CONTRACT_CONFLICT: %s" % exc, file=sys.stderr)
        return 20
    try:
        env = dict(os.environ, V5_SLOT_LOCK_FD=str(fd))
        return subprocess.call(command, env=env, pass_fds=(fd,))
    finally:
        # Closing rather than LOCK_UN preserves the lock in inheriting children.
        os.close(fd)


if __name__ == "__main__":
    sys.exit(main())
