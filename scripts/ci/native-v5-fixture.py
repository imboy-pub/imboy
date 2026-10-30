#!/usr/bin/env python3
"""Verify a user-admitted native cloud fixture; never provision or relax guards.
验证用户明确准入的原生云夹具；本程序不供给资源、不放松已有守卫。
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import socket
import stat
import subprocess
import time


def private_json(path):
    path = Path(path)
    if not path.is_absolute() or path.is_symlink():
        raise ValueError("receipt must be an absolute regular private file")
    info = path.stat()
    if not stat.S_ISREG(info.st_mode) or stat.S_IMODE(info.st_mode) != 0o600:
        raise ValueError("receipt mode must be 0600")
    parent = path.parent.stat()
    if not stat.S_ISDIR(parent.st_mode) or stat.S_IMODE(parent.st_mode) != 0o700:
        raise ValueError("receipt directory mode must be 0700")
    if info.st_uid != parent.st_uid or (os.geteuid() != 0 and info.st_uid != os.geteuid()):
        raise ValueError("receipt ownership mismatch")
    raw = path.read_bytes()
    value = json.loads(raw)
    if not isinstance(value, dict):
        raise ValueError("receipt must be an object")
    return value, hashlib.sha256(raw).hexdigest()


def admission(receipt_path, prefix=None, database=None):
    receipt, _ = private_json(receipt_path)
    proposal, digest = private_json(receipt["proposal_path"])
    approval, _ = private_json(receipt["approval_path"])
    if digest != receipt["proposal_sha256"] or digest != approval["proposal_sha256"]:
        raise ValueError("approved proposal digest mismatch")
    if approval["decision"] != "APPROVED_BY_USER":
        raise ValueError("fixture has not been approved")
    if receipt["mode"] != "EUNIT_ONLY" or not receipt["owner_token"]:
        raise ValueError("invalid controller ownership")
    if not time.time() < receipt["lease_until"] <= time.time() + 14400:
        raise ValueError("controller lease expired or unbounded")
    pid = receipt["controller_pid"]
    if not isinstance(pid, int) or isinstance(pid, bool) or pid <= 1:
        raise ValueError("invalid controller PID")
    # /proc start-time prevents a recycled PID from adopting the lease.
    # /proc 启动时刻阻止复用 PID 接管租约。
    actual_start = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()[19]
    if actual_start != receipt["controller_start"]:
        raise ValueError("controller process changed")
    if receipt["container_id"] != proposal["container_id"]:
        raise ValueError("container not allowlisted")
    if receipt["container_image_id"] != proposal["container_image_id"]:
        raise ValueError("container image changed")
    if socket.gethostname() != receipt["container_id"][:12]:
        raise ValueError("not executing in the admitted container")
    pg = proposal["pg"]
    if receipt["pg_system_identifier"] != pg["system_identifier"]:
        raise ValueError("cluster identity changed")
    if pg["host"] != "127.0.0.1" or pg["port"] != 55432 or pg["user"] != "imboy_ci":
        raise ValueError("unexpected native connection boundary")
    allowed = pg["exact_database_allowlist"]
    if prefix is not None:
        database = allowed[prefix]
    if database not in allowed.values() or not re.fullmatch(r"attv5_[a-z0-9_]+", database):
        raise ValueError("database is not exactly allowlisted")
    source_root = Path(receipt["source_root"]).resolve()
    if source_root != Path(__file__).resolve().parents[2]:
        raise ValueError("receipt belongs to another checkout")
    required_sources = {"scripts/ci/native-v5-fixture.py",
                        "test/common/native_v5_fixture.erl",
                        "test/common/inttest_marker_db.erl",
                        "test/ds/moment_delete_atomic_pg_tests.erl"}
    if not required_sources.issubset(receipt["source_files"]):
        raise ValueError("incomplete frozen guard sources")
    for name, expected in receipt["source_files"].items():
        source = (source_root / name).resolve()
        if not source.is_relative_to(source_root):
            raise ValueError("source path escapes checkout")
        if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
            raise ValueError("frozen source changed")
    return receipt, pg, database


def verify(receipt_path, prefix=None, database=None):
    receipt, pg, database = admission(receipt_path, prefix, database)
    if hashlib.sha256(Path(receipt["psql"]).read_bytes()).hexdigest() != receipt["psql_sha256"]:
        raise ValueError("PostgreSQL client changed")
    # Exact DB name was validated before SQL construction. Do not inherit any
    # PG service/connection overrides, or emit credentials/connection strings.
    # SQL 构造前已核精确库名；不继承 PG 服务覆盖，不输出凭据或连接串。
    sql = ("SELECT current_database(), current_user, "
           "(SELECT system_identifier::text FROM pg_control_system()), "
           "shobj_description(oid,'pg_database') FROM pg_database "
           "WHERE datname=current_database()")
    env = {k: v for k, v in os.environ.items() if not k.startswith("PG")}
    env["PGCONNECT_TIMEOUT"] = "5"
    command = [receipt["psql"], "-X", "-w", "-A", "-t", "-F", "\t",
               "-h", pg["host"], "-p", str(pg["port"]), "-U", pg["user"],
               "-d", database, "-v", "ON_ERROR_STOP=1", "-c", sql]
    result = subprocess.run(command, env=env, capture_output=True, text=True, timeout=10)
    expected_marker = "imboy-native-v5:" + receipt["owner_token"] + ":" + receipt["proposal_sha256"]
    expected = [database, pg["user"], pg["system_identifier"], expected_marker]
    if result.returncode != 0 or result.stdout.strip().split("\t") != expected:
        raise ValueError("live database identity or ownership marker mismatch")
    return {"database": database, "host": pg["host"], "port": pg["port"],
            "user": pg["user"], "slot": receipt["slot"]}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("receipt")
    target = parser.add_mutually_exclusive_group(required=True)
    target.add_argument("--prefix")
    target.add_argument("--database")
    args = parser.parse_args()
    try:
        result = verify(args.receipt, args.prefix, args.database)
    except Exception as error:
        # Exception values can contain raw SQL/provider diagnostics. Keep only
        # the error type; detailed private controller evidence remains on disk.
        # 异常值可能含 SQL/provider 数据；这里只输出类型，详情留在私有回执。
        raise SystemExit("native fixture admission refused: " + type(error).__name__)
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
