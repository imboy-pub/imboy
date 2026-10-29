#!/usr/bin/env python3
"""Prepare a new local PG resource; admission and slot ownership are separate."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import secrets
import socket
import ssl
import stat
import struct
import subprocess
import sys
import time
import uuid

DOCKER_SOCKET = None
DOCKER_SOCKET_IDENTITY = None


def local_docker():
    """Use only the current user's Docker Desktop Unix socket on this host."""
    global DOCKER_SOCKET, DOCKER_SOCKET_IDENTITY
    if any(os.environ.get(key) for key in ("DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_TLS_VERIFY", "DOCKER_CERT_PATH")):
        raise ValueError("external Docker endpoint override is forbidden")
    result = subprocess.run(["docker", "context", "inspect", "--format", "{{json .Endpoints.docker.Host}}"],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=10, check=False)
    if result.returncode:
        raise RuntimeError("local Docker context unavailable")
    endpoint = json.loads(result.stdout)
    socket_path = Path.home().resolve() / ".docker/run/docker.sock"
    if endpoint != "unix://" + str(socket_path):
        raise ValueError("Docker context is not the local Desktop socket")
    identity = socket_path.stat()
    if not stat.S_ISSOCK(identity.st_mode):
        raise ValueError("Docker endpoint is not a Unix socket")
    DOCKER_SOCKET = str(socket_path)
    DOCKER_SOCKET_IDENTITY = (identity.st_dev, identity.st_ino)


def command(args, *, env=None, timeout=20):
    if args[0] == "docker":
        if DOCKER_SOCKET is None:
            raise RuntimeError("Docker endpoint not pinned")
        identity = Path(DOCKER_SOCKET).stat()
        if (identity.st_dev, identity.st_ino) != DOCKER_SOCKET_IDENTITY or not stat.S_ISSOCK(identity.st_mode):
            raise RuntimeError("Docker socket changed")
        env = dict(os.environ if env is None else env)
        for key in ("DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_TLS_VERIFY", "DOCKER_CERT_PATH"):
            env.pop(key, None)
        env["DOCKER_HOST"] = "unix://" + DOCKER_SOCKET
    result = subprocess.run(args, env=env, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, timeout=timeout, check=False)
    if result.returncode:
        raise RuntimeError("resource command failed: " + args[0])
    return result.stdout.decode().strip()


def private_write(path, data):
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(fd, "wb") as stream:
        stream.write(data)
        stream.flush()
        os.fsync(stream.fileno())


def inspect(container_id):
    data = json.loads(command(["docker", "inspect", container_id]))
    if not isinstance(data, list) or len(data) != 1:
        raise RuntimeError("unexpected container inspection")
    return data[0]


def remove_created(container_id, run_id, nonce):
    """Fail closed until exclusive resource ownership is established."""
    raise RuntimeError("container cleanup requires verified exclusive ownership")


def verify_tls_identity(port, ca_bytes):
    """Use the already-hashed CA bytes rather than reopening a mutable path."""
    context = ssl.create_default_context(cadata=ca_bytes.decode("ascii"))
    with socket.create_connection(("127.0.0.1", port), timeout=3) as connection:
        connection.sendall(struct.pack("!II", 8, 80877103))
        if connection.recv(1) != b"S":
            raise RuntimeError("PostgreSQL TLS unavailable")
        with context.wrap_socket(connection, server_hostname="localhost"):
            pass


def reconcile(resource_dir):
    root = Path(resource_dir)
    if not root.is_absolute() or root.resolve() != root or not root.is_dir():
        raise ValueError("resource directory must be canonical and existing")
    if root.parent.name != "resources":
        raise ValueError("resource is outside a run resources directory")
    intent = json.loads((root / "intent.json").read_bytes())
    nonce = intent.get("nonce") if isinstance(intent, dict) else None
    run_id = intent.get("run_id") if isinstance(intent, dict) else None
    if (not isinstance(nonce, str) or not re.fullmatch(r"[0-9a-f]{32}", nonce)
            or root.name != "pg-prepared-" + nonce or run_id != root.parent.parent.name
            or intent.get("container") != "attv5-" + nonce):
        raise ValueError("resource creation intent is invalid")
    local_docker()
    listing = command(["docker", "ps", "-a", "--no-trunc", "--filter", "label=codex.run=" + run_id,
                       "--filter", "label=codex.resource_nonce=" + nonce, "--format", "{{.ID}}"])
    candidates = listing.splitlines() if listing else []
    if len(candidates) > 1:
        raise RuntimeError("multiple matching containers; manual reconciliation required")
    for cid in candidates:
        if not re.fullmatch(r"[0-9a-f]{64}", cid):
            raise RuntimeError("invalid container identity")
        actual = inspect(cid)
        labels = actual.get("Config", {}).get("Labels", {})
        if (actual.get("Id") != cid or labels.get("codex.run") != run_id
                or labels.get("codex.resource_nonce") != nonce):
            raise RuntimeError("container ownership changed")
    return {"run_id": run_id, "nonce": nonce, "container_ids": candidates,
            "ca_signing_key_present": (root / "ca-signing.key").exists(),
            "state": "PREPARED_UNAUTHORIZED"}


def cleanup_secrets(resource_dir, outcome):
    """Fail closed until exclusive ownership makes deletion safe."""
    raise RuntimeError("secret cleanup requires verified exclusive ownership")


def provision(run_dir, image):
    root = Path(run_dir)
    if not root.is_absolute() or root.resolve() != root or not root.is_dir():
        raise ValueError("run directory must be an existing canonical absolute path")
    run_id = root.name
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]{0,48}", run_id):
        raise ValueError("invalid run identifier")
    resources = root / "resources"
    if not resources.is_dir() or resources.resolve() != resources:
        raise ValueError("existing canonical resources directory required")
    raise RuntimeError("provisioning requires verified exclusive resource ownership")
    local_docker()
    nonce = uuid.uuid4().hex
    dest = resources / ("pg-prepared-" + nonce)
    dest.mkdir(mode=0o700)
    container = "attv5-" + nonce
    private_write(dest / "intent.json", (json.dumps({"state": "CREATING", "run_id": run_id,
                                                   "nonce": nonce, "container": container},
                                                  sort_keys=True) + "\n").encode())
    empty_init = dest / "empty-init"
    empty_init.mkdir(mode=0o700)
    data_dir = dest / "pg-data"
    data_dir.mkdir(mode=0o700)
    password = secrets.token_urlsafe(32)
    private_write(dest / "password", password.encode())
    private_write(dest / "ca-signing.key", b"")
    private_write(dest / "server.key", b"")
    files = {}
    for name in ("password", "ca-signing.key", "server.key"):
        info = (dest / name).stat()
        files[name] = [info.st_dev, info.st_ino]
    private_write(dest / "ownership.json", (json.dumps({"run_id": run_id, "nonce": nonce,
                                                        "files": files}, sort_keys=True) + "\n").encode())
    try:
        ca_key = dest / "ca-signing.key"
        try:
            command(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
                     "-keyout", str(ca_key), "-out", str(dest / "ca.pem"),
                     "-days", "7", "-subj", "/CN=IMBoy-V5-" + nonce,
                     "-addext", "basicConstraints=critical,CA:TRUE"])
            command(["openssl", "req", "-newkey", "rsa:2048", "-nodes",
                     "-keyout", str(dest / "server.key"), "-out", str(dest / "server.csr"),
                     "-subj", "/CN=localhost"])
            private_write(dest / "extensions", b"subjectAltName=DNS:localhost\nbasicConstraints=critical,CA:FALSE\nextendedKeyUsage=serverAuth\n")
            command(["openssl", "x509", "-req", "-in", str(dest / "server.csr"),
                     "-CA", str(dest / "ca.pem"), "-CAkey", str(ca_key),
                     "-CAcreateserial", "-out", str(dest / "server.crt"), "-days", "7",
                     "-extfile", str(dest / "extensions")])
        finally:
            pass
        # The root entrypoint copies the private key and gives it to PostgreSQL.
        os.chmod(dest / "server.key", 0o600)
        image_id = command(["docker", "image", "inspect", image, "--format", "{{.Id}}"])
        if not re.fullmatch(r"sha256:[0-9a-f]{64}", image_id):
            raise RuntimeError("image identity unavailable")
        bootstrap = ("cp /proof-server.key /tmp/server.key && cp /proof-server.crt /tmp/server.crt && "
                     "chown postgres:postgres /tmp/server.key /tmp/server.crt && chmod 600 /tmp/server.key && "
                     "exec docker-entrypoint.sh postgres -c ssl=on -c ssl_cert_file=/tmp/server.crt "
                     "-c ssl_key_file=/tmp/server.key -c "
                     "shared_preload_libraries=pgcrypto,pg_jieba,timescaledb,vector,pg_stat_statements")
        env = dict(os.environ, POSTGRES_PASSWORD=password)
        cid = command(["docker", "create", "--name", container, "--label", "codex.run=" + run_id,
                       "--label", "codex.role=attachment-lifecycle-pg",
                       "--label", "codex.resource_nonce=" + nonce, "--network", "bridge",
                       "-p", "127.0.0.1::5432", "-v", str(dest / "server.key") + ":/proof-server.key:ro",
                       "-v", str(dest / "server.crt") + ":/proof-server.crt:ro",
                       "-v", str(data_dir) + ":/var/lib/postgresql",
                       "-v", str(empty_init) + ":/docker-entrypoint-initdb.d:ro",
                       "-e", "POSTGRES_USER=imboy_user", "-e", "POSTGRES_PASSWORD",
                       "--entrypoint", "sh", image_id, "-c", bootstrap], env=env)
        if not re.fullmatch(r"[0-9a-f]{64}", cid):
            raise RuntimeError("invalid container identity")
        try:
            # This is an output receipt, not an authorization or an admission record.
            ca_bytes = (dest / "ca.pem").read_bytes()
            descriptor = {"state": "PREPARED_UNAUTHORIZED", "run_id": run_id, "nonce": nonce,
                          "container_id": cid, "container": container, "image_id": image_id,
                          "ca_sha256": hashlib.sha256(ca_bytes).hexdigest(),
                          "server_name": "localhost"}
            private_write(dest / "resource.json", (json.dumps(descriptor, sort_keys=True) + "\n").encode())
            command(["docker", "start", cid])
            actual = inspect(cid)
            labels = actual.get("Config", {}).get("Labels", {})
            ports = actual.get("NetworkSettings", {}).get("Ports", {}).get("5432/tcp")
            if (actual.get("Id") != cid or actual.get("Image") != image_id
                    or labels.get("codex.run") != run_id or labels.get("codex.resource_nonce") != nonce
                    or not isinstance(ports, list) or len(ports) != 1
                    or ports[0].get("HostIp") != "127.0.0.1"):
                raise RuntimeError("resource identity or binding mismatch")
            port = int(ports[0]["HostPort"])
            if not 1024 <= port <= 65535:
                raise RuntimeError("invalid published port")
            deadline = time.monotonic() + 60
            while True:
                try:
                    verify_tls_identity(port, ca_bytes)
                    break
                except (OSError, RuntimeError):
                    pass
                if time.monotonic() >= deadline:
                    raise RuntimeError("TLS handshake observation failed")
                time.sleep(0.5)
            descriptor["host_binding"] = ports
            descriptor["observation"] = "TLS_HANDSHAKE_ONLY_NO_DATABASE_AUTHENTICATION"
            # This is not readiness, admission, or database ownership evidence.
            private_write(dest / "tls-observed.json", (json.dumps(descriptor, sort_keys=True) + "\n").encode())
            return dest, descriptor
        except BaseException:
            remove_created(cid, run_id, nonce)
            raise
    finally:
        pass


def main():
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--run-dir")
    mode.add_argument("--inspect-resource")
    mode.add_argument("--cleanup-resource")
    mode.add_argument("--cleanup-secrets")
    parser.add_argument("--container-id")
    args = parser.parse_args()
    try:
        if args.run_dir:
            if args.container_id:
                raise ValueError("container-id is only valid for cleanup")
            path, descriptor = provision(args.run_dir, "imboy/pg18:3.6.1-2")
            print(json.dumps({"resource_dir": str(path), "state": descriptor["state"],
                              "container_id": descriptor["container_id"]}))
        else:
            outcome = reconcile(args.inspect_resource or args.cleanup_resource or args.cleanup_secrets)
            if args.cleanup_resource:
                if (not isinstance(args.container_id, str)
                        or outcome["container_ids"] != [args.container_id]):
                    raise ValueError("exact unique container ID required for cleanup")
                remove_created(args.container_id, outcome["run_id"], outcome["nonce"])
                outcome["container_removed"] = True
            elif args.cleanup_secrets:
                if args.container_id:
                    raise ValueError("container-id is only valid for cleanup")
                outcome = cleanup_secrets(args.cleanup_secrets, outcome)
            elif args.container_id:
                raise ValueError("container-id is only valid for cleanup")
            print(json.dumps(outcome))
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError):
        print("resource provisioning failed; inspect this run's prepared-resource directory", file=sys.stderr)
        return 20
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
