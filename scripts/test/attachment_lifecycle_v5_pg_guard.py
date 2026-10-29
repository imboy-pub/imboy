"""Resolve only the explicitly authorized, run-owned PostgreSQL container."""
import hashlib
import json
import math
from pathlib import Path
import re
import subprocess
import sys
import time


def validate_slot(allowed_slots, slot):
    if (not isinstance(allowed_slots, list) or not allowed_slots
            or not all(isinstance(item, str) and re.fullmatch(r"[a-z][a-z0-9_]{0,31}", item)
                       for item in allowed_slots)
            or len(allowed_slots) != len(set(allowed_slots)) or slot not in allowed_slots):
        raise ValueError("invalid fixture slot")


def authorized_database(allowlist, allowed_slots, slot, database):
    """Check the exact receipt entry; callers must separately bind resource identity and lock."""
    validate_slot(allowed_slots, slot)
    if (not isinstance(database, str) or len(database) > 63
            or not re.fullmatch(r"[a-z][a-z0-9_]*", database)):
        raise ValueError("invalid database name")
    matches = [item for item in allowed_slots
               if any(database.startswith(prefix) and len(database) > len(prefix)
                      for prefix in ("attv5_%s_" % item, "attv5_chain_%s_" % item))]
    if matches != [slot]:
        raise ValueError("database slot ownership is missing or ambiguous")
    if not isinstance(allowlist, dict):
        raise ValueError("resource allowlist missing")
    databases = allowlist.get("scratch_postgres")
    if (not isinstance(databases, list) or not all(isinstance(item, str) for item in databases)
            or len(databases) != len(set(databases)) or database not in databases):
        raise ValueError("database is not exactly authorized")
    return database


def owned_container(run_dir, slot, database=None):
    root = Path(run_dir).resolve()
    resource = json.loads((root / "resources/postgres.json").read_text())
    state = json.loads((root / "state.json").read_text())
    if not isinstance(resource, dict) or not isinstance(state, dict):
        raise ValueError("resource receipt and run state must be objects")
    auth_path = Path(state["authorization_path"])
    auth_bytes = auth_path.read_bytes()
    if hashlib.sha256(auth_bytes).hexdigest() != state["authorization_sha256"]:
        raise ValueError("authorization changed")
    auth = json.loads(auth_bytes)
    if not isinstance(auth, dict):
        raise ValueError("authorization must be an object")
    validate_slot(resource.get("allowed_slots"), slot)
    if database is not None:
        authorized_database(auth.get("resource_allowlist"), resource.get("allowed_slots"), slot, database)
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from attachment_lifecycle_v5_runner import authorization_unexpired, verify_plan_manifest
    if not authorization_unexpired(auth):
        raise ValueError("authorization issuance or expiry invalid")
    if (auth.get("implementation_allowed") is not True
            or state.get("mode") != "IMPLEMENTATION"
            or state.get("run_id") != root.name
            or not isinstance(state.get("owner_token"), str)
            or not state["owner_token"]
            or not isinstance(state.get("lease_until"), (int, float))
            or isinstance(state["lease_until"], bool)
            or not math.isfinite(state["lease_until"])
            or state["lease_until"] <= time.time()):
        raise ValueError("implementation authorization or run lease invalid")
    # Reuse the runner's full manifest verification, including every frozen input.
    ok, manifest_digest = verify_plan_manifest(state["plan_dir"])
    if (not ok or manifest_digest != state.get("plan_manifest_sha256")
            or manifest_digest != auth.get("plan_manifest_sha256")):
        raise ValueError("plan manifest changed since authorization")
    if resource["owner_run_id"] != root.name:
        raise ValueError("slot or resource owner mismatch")
    container = resource["container"]
    container_id = resource.get("container_id")
    allowlist = auth.get("resource_allowlist")
    if not isinstance(allowlist, dict):
        raise ValueError("resource allowlist missing")
    allowed = allowlist.get("postgres_container", [])
    if (not isinstance(container, str) or not re.fullmatch(r"attv5-[a-z0-9-]+", container)
            or not isinstance(container_id, str) or not re.fullmatch(r"[0-9a-f]{64}", container_id)
            or not isinstance(allowed, list) or container not in allowed):
        raise ValueError("container is not explicitly authorized")
    fmt = ('{"id":{{json .Id}},"labels":{{json .Config.Labels}},'
           '"image":{{json .Image}},"running":{{json .State.Running}},"ports":{{json .NetworkSettings.Ports}}}')
    actual = json.loads(subprocess.check_output(
        ["docker", "inspect", "--format", fmt, container], text=True,
        stderr=subprocess.DEVNULL, timeout=5))
    if not isinstance(actual, dict) or not isinstance(actual.get("labels"), dict):
        raise ValueError("invalid Docker inspection result")
    labels = actual["labels"]
    if (actual.get("id") != container_id
            or actual.get("image") != resource["image_id"]
            or actual.get("running") is not True
            or labels.get("codex.run") != root.name
            or labels.get("codex.role") != "attachment-lifecycle-pg"):
        raise ValueError("live container does not match owned resource receipt")
    ports = actual.get("ports")
    if not isinstance(ports, dict):
        raise ValueError("invalid Docker port inspection")
    bindings = ports.get("5432/tcp")
    if bindings != resource.get("host_binding") or not isinstance(bindings, list) or len(bindings) != 1:
        raise ValueError("PostgreSQL host binding changed")
    binding = bindings[0]
    if not isinstance(binding, dict):
        raise ValueError("invalid PostgreSQL host binding")
    if binding.get("HostIp") != "127.0.0.1" or not str(binding.get("HostPort", "")).isdigit():
        raise ValueError("PostgreSQL must bind only loopback")
    port = int(binding["HostPort"])
    if not 1024 <= port <= 65535:
        raise ValueError("invalid dedicated PostgreSQL port")
    return actual["id"], binding["HostIp"], str(port)


if __name__ == "__main__":
    try:
        args = sys.argv[1:]
        if len(args) == 2 and not args[0].startswith("-"):
            owned_container(args[0], args[1])
            # Bare container discovery stays refused by contract even on a
            # fully valid resource: the callers also open TCP connections and
            # create databases; container inspection alone cannot pin those
            # connections or authorize their names.
            print("CONTRACT_CONFLICT: scratch database allowlist and connection identity "
                  "are not enforced by the fixture", file=sys.stderr)
            raise SystemExit(20)
        elif len(args) == 3 and not args[0].startswith("-"):
            # Database-scoped resolution for gate executors and probes: the
            # full audit chain (receipt, authorization, manifest, live
            # container, loopback binding) plus the exact scratch-database
            # receipt entry, then the identity tuple for the caller to bind.
            container_id, host, port = owned_container(args[0], args[1], args[2])
            print("%s\t%s\t%s" % (container_id, host, port))
        else:
            raise ValueError("expected RUN_DIR SLOT [DATABASE]")
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError):
        print("CONTRACT_CONFLICT: dedicated owned PostgreSQL resource required", file=sys.stderr)
        raise SystemExit(20)
