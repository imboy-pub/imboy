#!/usr/bin/env python3
"""Fail-closed LEE-5 evidence check; publish counts only, never raw logs/config/XML."""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
import xml.etree.ElementTree as ET

MODULES = (
    "qr_login_handler_tests", "qr_login_sse_handler_tests", "qr_login_uid_contract_tests",
    "qr_login_pg_failure_tests", "qr_login_cleanup_worker_tests", "qr_login_session_repo_tests",
    "qr_login_claim_acceptance_tests",
)
PAIRS = (("status", "status"), ("sse", "status"), ("sse", "sse"))


def validate(root: Path, commit: str, outcome: str) -> dict:
    if outcome != "success":
        raise ValueError("runner or prerequisites did not succeed")
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise ValueError("invalid checkout SHA")
    if (root / "commit.txt").read_text().strip() != commit:
        raise ValueError("evidence belongs to another checkout")
    if (root / "runner-exit.txt").read_text().strip() != "0":
        raise ValueError("runner did not exit successfully")
    if (root / "acceptance-mode.txt").read_text().strip() != "1":
        raise ValueError("formal no-skip mode was not enabled")
    reports = {}
    for module in MODULES:
        paths = list((root / "raw" / module).glob("TEST-*.xml"))
        if len(paths) != 1 or paths[0].name != f"TEST-{module}.xml":
            raise ValueError(f"missing, duplicate or unexpected report: {module}")
        suite = ET.parse(paths[0]).getroot()
        if suite.tag != "testsuite" or suite.get("name") != f"module '{module}'":
            raise ValueError(f"wrong EUnit suite: {module}")
        counts = {key: int(suite.attrib[key]) for key in ("tests", "failures", "errors", "skipped")}
        cases = suite.findall("testcase")
        if counts["tests"] < 1 or len(cases) != counts["tests"]:
            raise ValueError(f"empty or incomplete case evidence: {module}")
        if any(counts[key] != 0 for key in ("failures", "errors", "skipped")) or any(
            case.find(tag) is not None for case in cases for tag in ("failure", "error", "skipped")
        ):
            raise ValueError(f"failed, cancelled or skipped cases: {module}")
        reports[module] = counts
    dcheck = json.loads((root / "dcheck.json").read_text())
    if not (dcheck.get("status") == "PASS" and dcheck.get("cases") == 3
            and dcheck.get("failed") == 0 and dcheck.get("skipped") == 0
            and dcheck.get("fixture_rows_remaining") == 0 and dcheck.get("authority") == "real PG"):
        raise ValueError("two-peer summary failed")
    nodes = dcheck.get("backend_nodes", [])
    if len(nodes) != 2 or len(set(nodes)) != 2:
        raise ValueError("two distinct backend nodes were not recorded")
    scenarios = dcheck.get("scenarios", [])
    if len(scenarios) != 3 or [tuple(s.get("entries", [])) for s in scenarios] != list(PAIRS):
        raise ValueError("missing or duplicate two-peer scenario")
    for scenario in scenarios:
        consumers = scenario.get("consumers", [])
        if (scenario.get("synchronized_pg_waiters") != 2 or scenario.get("credentials") != 1
                or scenario.get("winner_identity_checked") is not True or len(consumers) != 2):
            raise ValueError("two-peer concurrency or identity evidence missing")
        if [c.get("entry") for c in consumers] != scenario["entries"]:
            raise ValueError("consumer transport mismatch")
        if {c.get("node") for c in consumers} != set(nodes):
            raise ValueError("consumer node mismatch")
        if any(c.get("compatible") is not True or type(c.get("credential_count")) is not int
               or c["credential_count"] not in (0, 1) for c in consumers):
            raise ValueError("unexpected two-peer consumer result")
        if sum(c["credential_count"] for c in consumers) != 1:
            raise ValueError("two-peer credential count is not one")
    return {"commit": commit, "status": "PASS", "runner_exit": 0, "formal_acceptance": True,
            "scope": "LEE-5 real PostgreSQL / Cowboy callback observers / two peer nodes",
            "modules": reports, "two_peer": {"cases": 3, "failed": 0, "skipped": 0,
                "pg_waiters_per_case": 2, "credentials_per_case": 1, "winner_identity_checked": True},
            "not_covered": ["browser end-to-end", "database failover durability", "production rollout"]}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("root", type=Path)
    parser.add_argument("--commit", required=True)
    parser.add_argument("--outcome", required=True)
    args = parser.parse_args()
    args.root.mkdir(parents=True, exist_ok=True)
    try:
        summary = validate(args.root, args.commit, args.outcome)
    except (OSError, ValueError, KeyError, TypeError, ET.ParseError) as exc:
        # Exception class only: XML/log/config contents may contain credentials.
        summary = {"commit": args.commit, "status": "FAIL", "reason": type(exc).__name__,
                   "scope": "LEE-5 acceptance evidence incomplete or unsuccessful"}
    (args.root / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary))
    return 0 if summary["status"] == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main())
