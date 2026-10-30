#!/usr/bin/env python3
"""Hold a cooperative per-run slot lock for a gate and its inheriting children."""
import argparse
import fcntl
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys


class ResourceUnavailable(Exception):
    """The supervisor cannot establish a resource identity for this run."""


def acquire_resource(container_id, lock_root=None):
    """Serialize cooperating runs that name the same physical PG container.

    This lock is not proof of the TCP peer, authorization, or child lifetime.
    The database guard must remain closed until those checks are implemented.
    """
    if not isinstance(container_id, str) or not re.fullmatch(r"[0-9a-f]{64}", container_id):
        raise ValueError("invalid resource identity")
    parent = Path("/tmp").resolve() if lock_root is None else Path(lock_root)
    if not parent.is_absolute() or parent.resolve() != parent or not parent.is_dir():
        raise ValueError("invalid resource lock parent")
    lock_dir = parent / ("imboy-v5-pg-locks-%d" % os.getuid())
    lock_dir.mkdir(mode=0o700, exist_ok=True)
    info = lock_dir.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("resource lock directory not private")
    directory = os.open(lock_dir, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        opened_dir = os.fstat(directory)
        if (opened_dir.st_dev, opened_dir.st_ino) != (info.st_dev, info.st_ino):
            raise ValueError("resource lock directory changed")
        name = container_id + ".lock"
        fd = os.open(name, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600, dir_fd=directory)
        try:
            held = os.fstat(fd)
            found = os.stat(name, dir_fd=directory, follow_symlinks=False)
            if (not stat.S_ISREG(held.st_mode) or held.st_uid != os.getuid()
                    or held.st_nlink != 1 or held.st_mode & 0o077
                    or (held.st_dev, held.st_ino) != (found.st_dev, found.st_ino)):
                raise ValueError("resource lock identity invalid")
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            found = os.stat(name, dir_fd=directory, follow_symlinks=False)
            current_dir = lock_dir.lstat()
            if ((held.st_dev, held.st_ino) != (found.st_dev, found.st_ino)
                    or (opened_dir.st_dev, opened_dir.st_ino) != (current_dir.st_dev, current_dir.st_ino)):
                raise ValueError("resource lock identity changed while acquiring")
            return fd
        except BaseException:
            os.close(fd)
            raise
    finally:
        os.close(directory)


def receipt_container_id(run_dir):
    """Read the existing receipt without following a final-component symlink."""
    path = Path(run_dir) / "resources" / "postgres.json"
    try:
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    except FileNotFoundError as exc:
        raise ResourceUnavailable("PostgreSQL resource receipt missing") from exc
    with os.fdopen(fd, "rb") as stream:
        if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
            raise ValueError("resource receipt is not a regular file")
        raw = stream.read(65537)
    if len(raw) > 65536:
        raise ValueError("resource receipt too large")
    receipt = json.loads(raw)
    container_id = receipt.get("container_id") if isinstance(receipt, dict) else None
    if not isinstance(container_id, str) or not re.fullmatch(r"[0-9a-f]{64}", container_id):
        raise ValueError("resource receipt container identity invalid")
    return container_id


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
    fd = resource_fd = None
    try:
        fd = acquire(args.run_dir, args.slot)
        container_id = receipt_container_id(args.run_dir)
        resource_fd = acquire_resource(container_id)
    except ResourceUnavailable as exc:
        if fd is not None:
            os.close(fd)
        print("BLOCKED_EXTERNAL: %s" % exc, file=sys.stderr)
        return 75
    except BlockingIOError:
        if fd is not None:
            os.close(fd)
        print("BLOCKED_EXTERNAL: slot already in use", file=sys.stderr)
        return 75
    except (OSError, ValueError) as exc:
        if fd is not None:
            os.close(fd)
        print("CONTRACT_CONFLICT: %s" % exc, file=sys.stderr)
        return 20
    try:
        env = dict(os.environ, V5_SLOT_LOCK_FD=str(fd))
        descriptors = [fd]
        if resource_fd is not None:
            env["V5_RESOURCE_LOCK_FD"] = str(resource_fd)
            descriptors.append(resource_fd)
        return subprocess.call(command, env=env, pass_fds=tuple(descriptors))
    finally:
        # Closing rather than LOCK_UN preserves the lock in inheriting children.
        os.close(fd)
        if resource_fd is not None:
            os.close(resource_fd)


if __name__ == "__main__":
    sys.exit(main())
