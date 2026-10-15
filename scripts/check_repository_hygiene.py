#!/usr/bin/env python3
"""Reject tracked execution artifacts without excluding test source or product contracts."""
import argparse
from pathlib import Path, PurePosixPath
import hashlib
import json
import subprocess
import re


def forbidden_reason(path):
    parts = PurePosixPath(path).parts
    if path == "test/auto_test/task_state.json":
        return "historical test execution state"
    if any(part in {".otp", ".dart_tool", "__pycache__", "node_modules", "_build"} for part in parts):
        return "generated build cache"
    if path.endswith((".beam", ".pyc")):
        return "compiled artifact"
    if any(part in {"evidence", "test-results", "playwright-report"} for part in parts):
        return "execution evidence"
    # Formal versioned contracts are deliverables; ad-hoc plans/reports still fail.
    if path.startswith(("docs/design/", "docs/architecture/", "docs/plans/")) and re.fullmatch(
        r"(?:\d{4}-\d{2}-\d{2}-.+-v\d+|(?:.+-)?plan-v\d+(?:\.\d+)*|\d+-main-plan)\.md",
        PurePosixPath(path).name,
    ):
        return None
    if "docs" in parts and any(
        part in {"plans", "planning", "qa", "review", "analysis"} for part in parts
    ):
        return "execution documentation"
    if path.startswith("docs/archive/design/"):
        return "historical execution run"
    if ".claude" in parts and "plans" in parts:
        return "AI execution plan"
    name = parts[-1] if parts else ""
    if name.endswith('.md') and not any(part in {'macos', 'ios', 'plugin'} for part in parts):
        if re.search(r'(^|[-_])(plan|prompt)([-_]|$)', PurePosixPath(path).stem.lower()):
            return "named AI execution plan"
    if path.startswith("integration_test/full_acceptance/") and any(
        re.fullmatch(r"F\d{2}-S\d{2}", part) for part in parts
    ):
        return "execution coordination snapshot"
    if path.startswith("docs/compliance/") and any(
        token in name for token in (
            "Execution Record", "Execution Prompt", "Completion Audit",
            "Implementation Plan", "Gap Analysis", "Gap Matrix", "Self-Audit",
        )
    ):
        return "compliance execution material"
    if name.startswith("I18N_NATIVE_REVIEW") or name == "I18N_AUDIT_REPORT.md":
        return "translation execution report"
    return None


def tracked_paths(repo, revision=None):
    command = ["git", "-C", str(repo)]
    if revision:
        command += ["ls-tree", "-r", "--name-only", "-z", revision]
    else:
        command += ["ls-files", "-z"]
    return subprocess.check_output(command).decode().strip("\0").split("\0")


def preserved_artifacts(repo, revision=None):
    registry = "docs/reference/contracts/repository-artifacts.json"
    if revision:
        raw = subprocess.check_output(["git", "-C", str(repo), "show", f"{revision}:{registry}"])
    else:
        path = Path(repo) / registry
        if not path.exists():
            return set()
        raw = path.read_bytes()
    data = json.loads(raw)
    if data.get("schema_version") != 1:
        raise ValueError("invalid delivery artifact registry")
    preserved = set()
    tracked = set(tracked_paths(repo, revision))
    for item in data["artifacts"]:
        path = item["path"]
        if (path in preserved or not path.startswith("docs/") or ".." in PurePosixPath(path).parts
                or forbidden_reason(path) not in {"execution evidence", "execution documentation", "named AI execution plan"}
                or item["source_contract"] not in tracked or not item["purpose"].strip()):
            raise ValueError(f"invalid preserved delivery artifact: {path}")
        raw = subprocess.check_output(["git", "-C", str(repo), "show", f"{revision or ''}:{path}"])
        if hashlib.sha256(raw).hexdigest() != item["sha256"]:
            raise ValueError(f"preserved delivery artifact changed: {path}")
        preserved.add(path)
    return preserved


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=".", help="Git repository to inspect")
    parser.add_argument("--revision", help="Inspect a commit instead of the current index")
    args = parser.parse_args()
    try:
        preserved = preserved_artifacts(args.repo, args.revision)
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError) as error:
        print(f"Repository hygiene FAIL: {error}")
        return 1
    violations = [(path, forbidden_reason(path)) for path in tracked_paths(args.repo, args.revision)
                  if path not in preserved]
    violations = [(path, reason) for path, reason in violations if reason]
    for path, reason in violations:
        print(f"FAIL: {reason}: {path}")
    if violations:
        print(f"Repository hygiene failed: {len(violations)} tracked execution artifacts")
        return 1
    print(f"Repository hygiene PASS: {len(preserved)} registered delivery materials preserved")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
