#!/usr/bin/env python3
"""Own one real TextGuard child for a full-EUnit peer, never an external service."""
import json
import os
from pathlib import Path
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.request


def emit(value):
    print(json.dumps(value), flush=True)


def owns_listeners(pid, ports):
    # Full-suite peers use the Linux cloud executor. Prove PID/port ownership,
    # rather than accepting a response from an unrelated loopback listener.
    descriptors = Path(f"/proc/{pid}/fd")
    inodes = set()
    for descriptor in descriptors.iterdir():
        try:
            target = os.readlink(descriptor)
        except FileNotFoundError:
            continue
        if target.startswith("socket:["):
            inodes.add(target[8:-1])
    found = set()
    for row in Path("/proc/net/tcp").read_text().splitlines()[1:]:
        fields = row.split()
        address, port = fields[1].split(":")
        if address == "0100007F" and fields[3] == "0A" and fields[9] in inodes:
            found.add(int(port, 16))
    return set(ports) <= found


def interrupted(_signum, _frame):
    raise SystemExit(1)


def main():
    for signum in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
        signal.signal(signum, interrupted)
    if sys.platform != "linux":
        emit({"error": "owned_service_requires_linux_proc"})
        return 1
    child = None
    reservations = []
    try:
        binary = Path(os.environ["TEXTGUARD_TEST_BINARY"]).resolve(strict=True)
        if not binary.is_file() or not os.access(binary, os.X_OK):
            raise RuntimeError("binary_not_executable")
        with tempfile.TemporaryDirectory(prefix="imboy-textguard-test-") as root:
            root = Path(root)
            try:
                for _ in range(4):
                    handle = socket.socket()
                    handle.bind(("127.0.0.1", 0))
                    reservations.append(handle)
                ports = [handle.getsockname()[1] for handle in reservations]
                runtime = dict(os.environ)
                for name, port in zip(("TEXTGUARD_HTTP_ADDR", "TEXTGUARD_GRPC_ADDR",
                                       "TEXTGUARD_MANAGE_ADDR"), ports):
                    runtime[name] = f"127.0.0.1:{port}"
                runtime.update(TEXTGUARD_DATA_DIR=str(root / "data"),
                               TEXTGUARD_T2S_TABLE_ID="test-t2s-v1")
                # Reuse the two already-authorized secret FILE variables unchanged.
                for handle in reservations[:3]:
                    handle.close()
                with (root / "service.log").open("xb") as log:
                    child = subprocess.Popen([str(binary)], env=runtime, stdin=subprocess.DEVNULL,
                                             stdout=log, stderr=subprocess.STDOUT,
                                             start_new_session=True)
                    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
                    url = f"http://127.0.0.1:{ports[0]}"
                    deadline = time.monotonic() + 15
                    while True:
                        if child.poll() is not None:
                            raise RuntimeError("service_exited_before_ready")
                        try:
                            with opener.open(url + "/health/live", timeout=0.2) as response:
                                live = response.status == 200
                        except (OSError, urllib.error.URLError):
                            live = False
                        if live and owns_listeners(child.pid, ports[:3]):
                            break
                        if time.monotonic() >= deadline:
                            raise RuntimeError("owned_service_readiness_timeout")
                        time.sleep(0.02)
                    reservations[3].close()
                    emit({"ready": True, "http": url,
                          "manage": f"http://127.0.0.1:{ports[2]}",
                          "grpc_port": ports[1], "fault_port": ports[3],
                          "enhancement_file": str(root / "enhancement.json")})
                    # Closing the Erlang port also closes stdin: peer crashes still clean up.
                    sys.stdin.readline()
            finally:
                for handle in reservations:
                    handle.close()
                if child is not None:
                    if child.poll() is None:
                        child.terminate()
                    try:
                        child.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        child.kill()
                        child.wait()
                # TemporaryDirectory removes only this owner's private directory.
        return 0
    except Exception as error:
        # Never print runtime configuration, credential contents or service logs.
        emit({"error": type(error).__name__})
        return 1


if __name__ == "__main__":
    sys.exit(main())
