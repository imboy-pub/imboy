#!/usr/bin/env python3
"""Reject tracked execution artifacts without excluding test source or product contracts."""
import argparse
from pathlib import PurePosixPath
import subprocess
import re


def forbidden_reason(path):
    parts = PurePosixPath(path).parts
    if any(part in {"evidence", "test-results", "playwright-report"} for part in parts):
        return "execution evidence"
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=".", help="Git repository to inspect")
    parser.add_argument("--revision", help="Inspect a commit instead of the current index")
    args = parser.parse_args()
    violations = [(path, forbidden_reason(path)) for path in tracked_paths(args.repo, args.revision)]
    violations = [(path, reason) for path, reason in violations if reason]
    for path, reason in violations:
        print(f"FAIL: {reason}: {path}")
    if violations:
        print(f"Repository hygiene failed: {len(violations)} tracked execution artifacts")
        return 1
    print("Repository hygiene PASS: no tracked execution artifacts in checked categories")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
