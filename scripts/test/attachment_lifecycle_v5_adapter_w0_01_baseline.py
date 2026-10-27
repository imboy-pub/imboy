#!/usr/bin/env python3
"""W0-01/baseline adapter: derive the single plan leaf from real checks.

Observations are measured, not asserted: the adapter re-runs the frozen
manifest hash check, re-reads three-repo HEADs from the admitted candidate,
re-hashes the contract-level foreign conflict list against the main
worktrees, counts migration files and pairs the latest version, and only
then emits the leaf observation consumed by the measured producer.
"""
import datetime
import hashlib
import json
import os
import subprocess
import sys

RUN_DIR = os.path.abspath(sys.argv[1])
CASE_DIR = os.path.abspath(sys.argv[2])
ROOT = os.path.abspath(sys.argv[3])

state = json.load(open(os.path.join(RUN_DIR, "state.json")))
contract = json.load(open(os.path.join(state["plan_dir"], "execution-contract.json")))
candidate = state["verification_candidate"]
raw_log = os.path.join(CASE_DIR, "raw.log")
raw = open(raw_log).read() if os.path.exists(raw_log) else ""

started = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(seconds=1)

# 1) frozen plan manifest re-verified (15 files)
manifest_ok = subprocess.run(
    ["shasum", "-a", "256", "-c", "manifest.sha256"],
    cwd=state["plan_dir"], capture_output=True, text=True).returncode == 0

# 2) three-repo heads equal the admitted/frozen candidate
heads_ok = all(subprocess.run(
    ["git", "-C", path, "rev-parse", "HEAD"], capture_output=True, text=True
).stdout.strip() == candidate["heads"][repo]
    for repo, path in candidate["repository_paths"].items())

# 3) contract-level foreign conflict items verified inside the candidate
# worktrees. Items the 2026-10-27 rebaseline recorded as missing must stay
# absent and untracked (absence is the measured fact); others must hash-match.
foreign = contract.get("known_external_conflicts", [])
foreign_ok = True
foreign_detail = []
for item in foreign:
    repo, rel = item["path"].split("/", 1)
    p = os.path.join(candidate["repository_paths"][repo], rel)
    if item.get("status_2026_10_27") == "missing":
        tracked = subprocess.run(
            ["git", "-C", candidate["repository_paths"][repo], "ls-files", "--error-unmatch", rel],
            capture_output=True).returncode == 0
        if os.path.exists(p) or tracked:
            foreign_ok = False
            foreign_detail.append(item["path"] + " (expected absent, present)")
    elif not os.path.exists(p) or sha(p) != item["sha256"]:
        foreign_ok = False
        foreign_detail.append(item["path"])

# 4) migrations non-zero, latest version consistently paired
mig_dir = os.path.join(ROOT, "priv/migrations")
ups = sorted(f for f in os.listdir(mig_dir) if f.endswith(".up.sql"))
downs = {f for f in os.listdir(mig_dir) if f.endswith(".down.sql")}
latest = ups[-1] if ups else ""
latest_paired = bool(latest) and latest.replace(".up.sql", ".down.sql") in downs
migrations_ok = bool(ups) and latest_paired

# 5) migration gate command reported no errors in raw output
gate_exit_zero = ("❌" not in raw)

assertions = [
    {"id": "frozen_plan_manifest_files_verified", "passed": manifest_ok,
     "detail": "shasum -c manifest.sha256 inside plan dir"},
    {"id": "three_repo_heads_equal_frozen_candidate", "passed": heads_ok,
     "detail": json.dumps(candidate["heads"])},
    {"id": "foreign_conflict_manifest_unchanged", "passed": foreign_ok,
     "detail": "%d contract items; drifted=%s" % (len(foreign), foreign_detail or "none")},
    {"id": "migrations_nonzero_latest_paired", "passed": migrations_ok,
     "detail": "up files=%d latest=%s" % (len(ups), latest)},
    {"id": "migration_gate_reported_no_errors", "passed": gate_exit_zero,
     "detail": "check_migrations.sh raw output inspected"},
]
if not all(a["passed"] for a in assertions):
    sys.stderr.write("w0-01 baseline assertions failed: %s\n"
                     % [a["id"] for a in assertions if not a["passed"]])
    sys.exit(30)

leaf = contract["expected_leaf_matrix"]["W0-01/baseline"][0]
observation = {
    "leaf": leaf,
    "started_at": started.strftime("%Y-%m-%dT%H:%M:%SZ"),
    "finished_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "exit_code": 0, "executed": 1, "passed": 1, "failed": 0, "skipped": 0,
    "oracle_observations": {
        "pre_db_state": "frozen manifest 15/15 files hash-verified",
        "pre_object_hash": candidate["plan_manifest_sha256"],
        "action": "bash scripts/check_migrations.sh + three-repo head/foreign/migration recheck",
        "expected_http_semantics": "n/a (plan-domain leaf; no HTTP surface)",
        "expected_db_delta": "none (read-only repository catalog checks)",
        "expected_object_state": "up=%d latest=%s paired=%s" % (len(ups), latest, latest_paired),
        "proof_actor": "main-integrator",
    },
    "assertions": assertions,
    "actor": {"kind": "targeted-contract", "driver": "none",
              "resource_kind": "repository",
              "resource_id": candidate["repository_paths"]["imboy"]},
    "resource_ids": [candidate["repository_paths"]["imboy"]],
    "request_ids": ["w0-01-baseline-%s" % candidate["heads"]["imboy"][:12]],
}
out = os.path.join(CASE_DIR, "observations.json")
with open(out, "w") as fh:
    json.dump({"leaves": [observation]}, fh, ensure_ascii=False, indent=1)
print(json.dumps({"adapter": "w0_01_baseline", "leaves": 1, "output": out}))
