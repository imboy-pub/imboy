"""Resolve only the explicitly authorized, run-owned PostgreSQL container."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys


def owned_container(run_dir, slot):
    root = Path(run_dir).resolve()
    resource = json.loads((root / "resources/postgres.json").read_text())
    state = json.loads((root / "state.json").read_text())
    auth_path = Path(state["authorization_path"])
    auth_bytes = auth_path.read_bytes()
    if hashlib.sha256(auth_bytes).hexdigest() != state["authorization_sha256"]:
        raise ValueError("authorization changed")
    auth = json.loads(auth_bytes)
    if (not re.fullmatch(r"[a-z][a-z0-9_]{0,31}", slot)
            or slot not in resource["allowed_slots"]
            or resource["owner_run_id"] != root.name):
        raise ValueError("slot or resource owner mismatch")
    container = resource["container"]
    allowed = auth.get("resource_allowlist", {}).get("postgres_container", [])
    if (not isinstance(container, str) or not re.fullmatch(r"attv5-[a-z0-9-]+", container)
            or container not in allowed):
        raise ValueError("container is not explicitly authorized")
    fmt = ('{"id":{{json .Id}},"labels":{{json .Config.Labels}},'
           '"image":{{json .Image}},"running":{{json .State.Running}},"ports":{{json .NetworkSettings.Ports}}}')
    actual = json.loads(subprocess.check_output(
        ["docker", "inspect", "--format", fmt, container], text=True,
        stderr=subprocess.DEVNULL, timeout=5))
    labels = actual.get("labels") or {}
    if (actual.get("id") != resource["container_id"]
            or actual.get("image") != resource["image_id"]
            or actual.get("running") is not True
            or labels.get("codex.run") != root.name
            or labels.get("codex.role") != "attachment-lifecycle-pg"):
        raise ValueError("live container does not match owned resource receipt")
    bindings = (actual.get("ports") or {}).get("5432/tcp")
    if bindings != resource.get("host_binding") or not isinstance(bindings, list) or len(bindings) != 1:
        raise ValueError("PostgreSQL host binding changed")
    binding = bindings[0]
    if binding.get("HostIp") != "127.0.0.1" or not str(binding.get("HostPort", "")).isdigit():
        raise ValueError("PostgreSQL must bind only loopback")
    port = int(binding["HostPort"])
    if not 1024 <= port <= 65535:
        raise ValueError("invalid dedicated PostgreSQL port")
    return actual["id"], binding["HostIp"], str(port)


if __name__ == "__main__":
    try:
        if len(sys.argv) != 3:
            raise ValueError("run directory and slot required")
        print("\t".join(owned_container(sys.argv[1], sys.argv[2])))
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError):
        print("CONTRACT_CONFLICT: dedicated owned PostgreSQL resource required", file=sys.stderr)
        raise SystemExit(20)
