"""Atomic publication of a case's complete receipt set."""
import hashlib
import json
import os
import stat
import uuid

MAX_ARTIFACT_BYTES = 8 * 1024 * 1024
MAX_CASE_BYTES = 64 * 1024 * 1024
MAX_LEAVES = 10000


def _digest(data):
    return hashlib.sha256(data).hexdigest()


def _read_file(directory, name):
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
    info = os.fstat(fd)
    if not stat.S_ISREG(info.st_mode) or info.st_size > MAX_ARTIFACT_BYTES:
        os.close(fd)
        raise ValueError("case artifact must be a bounded regular file")
    with os.fdopen(fd, "rb") as stream:
        data = stream.read(MAX_ARTIFACT_BYTES + 1)
        if len(data) > MAX_ARTIFACT_BYTES:
            raise ValueError("case artifact exceeds size limit")
        return data


def _write_file(directory, name, data):
    fd = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                 0o600, dir_fd=directory)
    with os.fdopen(fd, "wb") as stream:
        stream.write(data)
        stream.flush()
        os.fsync(stream.fileno())


def publish(directory, rows):
    """Write an immutable generation and expose it with one no-replace link."""
    if not isinstance(rows, list) or not 0 < len(rows) <= MAX_LEAVES:
        raise ValueError("invalid case leaf count")
    encoded, total = [], 0
    for row in rows:
        data = json.dumps(row, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()
        total += len(data)
        if len(data) > MAX_ARTIFACT_BYTES or total > MAX_CASE_BYTES:
            raise ValueError("case receipts exceed size limit")
        encoded.append(data)
    # Never accumulate generations on a normal retry. Uncommitted generations
    # are retained for inspection; a bounded count forces explicit reconciliation.
    try:
        _read_file(directory, "case-commit.json")
    except FileNotFoundError:
        pass
    else:
        raise FileExistsError("case already committed")
    if sum(name.startswith("sealed-") for name in os.listdir(directory)) >= 32:
        raise ValueError("too many abandoned generations; reconcile manually")
    generation = "sealed-" + uuid.uuid4().hex
    os.mkdir(generation, 0o700, dir_fd=directory)
    fd = os.open(generation, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
    try:
        entries = []
        for n, data in enumerate(encoded, 1):
            name = "leaf-%03d.json" % n
            _write_file(fd, name, data)
            entries.append({"name": name, "sha256": _digest(data)})
        os.fsync(fd)
    finally:
        os.close(fd)
    manifest = json.dumps({"generation": generation, "entries": entries}, sort_keys=True,
                          separators=(",", ":")).encode()
    temporary = ".commit-" + uuid.uuid4().hex
    _write_file(directory, temporary, manifest)
    try:
        os.link(temporary, "case-commit.json", src_dir_fd=directory, dst_dir_fd=directory,
                follow_symlinks=False)
        os.fsync(directory)
    finally:
        os.unlink(temporary, dir_fd=directory)


def manifest_digest(directory):
    return _digest(_read_file(directory, "case-commit.json"))


def read(directory, expected_digest=None):
    """Read only a fully committed generation and verify every leaf digest."""
    raw = _read_file(directory, "case-commit.json")
    if expected_digest is not None and _digest(raw) != expected_digest:
        raise ValueError("case manifest binding mismatch")
    manifest = json.loads(raw)
    generation = manifest.get("generation")
    if not isinstance(generation, str) or len(generation) != 39 or not generation.startswith("sealed-") or any(c not in "0123456789abcdef" for c in generation[7:]):
        raise ValueError("invalid generation")
    entries = manifest.get("entries")
    if not isinstance(entries, list) or not 0 < len(entries) <= MAX_LEAVES:
        raise ValueError("invalid manifest leaf count")
    fd = os.open(generation, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
    try:
        rows, total = [], 0
        for n, entry in enumerate(entries, 1):
            name = "leaf-%03d.json" % n
            if not isinstance(entry, dict) or entry.get("name") != name:
                raise ValueError("invalid leaf entry")
            data = _read_file(fd, name)
            total += len(data)
            if total > MAX_CASE_BYTES:
                raise ValueError("case receipts exceed size limit")
            if _digest(data) != entry.get("sha256"):
                raise ValueError("leaf digest mismatch")
            rows.append((name, json.loads(data)))
        return rows
    finally:
        os.close(fd)
