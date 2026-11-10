#!/usr/bin/env python3
"""Reconcile verified Semgrep exceptions for GitHub's SARIF consumer.

The JSON findings gate is unchanged. Active SARIF findings must match it,
including multiplicity, severity and coordinates. Exact-rule source exceptions
recognized by Semgrep are retained separately in a sanitized audit inventory.
"""
import argparse
from collections import Counter
import copy
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import tokenize
from urllib.parse import unquote, urlsplit

LEVELS = {
    "CRITICAL": "error", "HIGH": "error", "ERROR": "error",
    "MEDIUM": "warning", "WARNING": "warning", "LOW": "note", "INFO": "note",
}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def relative_path(value, *, uri=False):
    require(isinstance(value, str) and bool(value), "Invalid finding path")
    if uri:
        parsed = urlsplit(value)
        require(not any((parsed.scheme, parsed.netloc, parsed.query, parsed.fragment)),
                "Finding URI must be repository-relative")
        value = unquote(parsed.path)
    path = PurePosixPath(value)
    require(not path.is_absolute() and ".." not in path.parts
            and "\\" not in value and not any(ord(char) < 32 for char in value),
            "Unsafe finding path")
    return path.as_posix()


def coordinates(start_line, start_column, end_line, end_column):
    values = (start_line, start_column, end_line, end_column)
    require(all(type(value) is int and value > 0 for value in values)
            and (end_line, end_column) >= (start_line, start_column),
            "Invalid finding coordinates")
    return values


def comments(path, source):
    if path.suffix == ".py":
        return [(token.start[0], token.string)
                for token in tokenize.generate_tokens(io.StringIO(source).readline)
                if token.type == tokenize.COMMENT]
    if path.suffix in (".yaml", ".yml"):
        # Installed by the pinned Semgrep package. Scanner comment tokens exclude
        # quoted strings and block scalars; do not regex-match arbitrary source.
        from ruamel.yaml import YAML
        from ruamel.yaml.tokens import CommentToken
        found = []

        def visit(value):
            if isinstance(value, CommentToken):
                found.append((value.start_mark.line + 1, value.value))
            elif isinstance(value, (list, tuple)):
                for child in value:
                    visit(child)

        for token in YAML(typ="rt").scan(source):
            visit(getattr(token, "comment", None))
        return found
    if "dockerfile" in path.name.lower():
        return [(number, line) for number, line in enumerate(source.splitlines(), 1)
                if line.lstrip().startswith("#")]
    raise ValueError("Unsupported in-source annotation language")


def source_proof(root, path, line, rule_id):
    source_path = root / path
    resolved = source_path.resolve(strict=True)
    require(resolved.is_relative_to(root) and resolved == source_path,
            "Suppressed source must be a regular repository path")
    data = source_path.read_bytes()
    for first_line, comment in comments(source_path, data.decode("utf-8")):
        for offset, text in enumerate(comment.splitlines()):
            if first_line + offset not in (line - 1, line):
                continue
            match = re.match(r"^\s*#\s*nosemgrep:\s*([^#]+)", text)
            if match and rule_id in re.split(r"[\s,]+", match.group(1).strip()):
                return hashlib.sha256(data).hexdigest()
    raise ValueError("Suppressed result has no exact-rule source comment")


def prepare(report, sarif, root):
    root = Path(root).resolve(strict=True)
    require(sarif["version"] == "2.1.0" and len(sarif["runs"]) == 1,
            "Expected one SARIF 2.1.0 run")
    run = sarif["runs"][0]
    driver = run["tool"]["driver"]
    require(driver["name"] == "Semgrep OSS"
            and driver["semanticVersion"] == report["version"],
            "Semgrep JSON/SARIF scanner identity mismatch")
    rules = {rule["id"]: rule for rule in driver["rules"]}
    require(len(rules) == len(driver["rules"]), "Duplicate SARIF rule definitions")
    expected = Counter()
    for item in report["results"]:
        require(not item["extra"].get("is_ignored", False),
                "The verified JSON must contain active findings only")
        key = (item["check_id"], relative_path(item["path"]),
               *coordinates(item["start"]["line"], item["start"]["col"],
                            item["end"]["line"], item["end"]["col"]),
               LEVELS[item["extra"]["severity"]])
        expected[key] += 1
    active, suppressed, actual = [], [], Counter()
    for result in run["results"]:
        rule_id = result["ruleId"]
        require(rule_id in rules, "Unknown SARIF finding rule")
        require(len(result["locations"]) == 1, "Expected one primary location")
        location = result["locations"][0]["physicalLocation"]
        require(location["artifactLocation"].get("uriBaseId") in (None, "%SRCROOT%"),
                "Unknown SARIF source-root identifier")
        path = relative_path(location["artifactLocation"]["uri"], uri=True)
        region = location["region"]
        position = coordinates(region["startLine"], region["startColumn"],
                               region["endLine"], region["endColumn"])
        level = result.get("level", rules[rule_id]["defaultConfiguration"]["level"])
        require(level in ("error", "warning", "note"), "Unknown SARIF finding level")
        key = (rule_id, path, *position, level)
        suppressions = result.get("suppressions", [])
        require(isinstance(suppressions, list), "Invalid SARIF suppressions")
        if suppressions:
            require(len(suppressions) == 1 and isinstance(suppressions[0], dict)
                    and suppressions[0].get("kind") == "inSource"
                    and suppressions[0].get("status", "accepted") == "accepted"
                    and set(suppressions[0]) <= {"kind", "status"},
                    "Unknown or unaccepted SARIF suppression")
            digest = source_proof(root, path, position[0], rule_id)
            suppressed.append({
                "rule_id": rule_id, "path": path,
                "start_line": position[0], "start_column": position[1],
                "end_line": position[2], "end_column": position[3],
                "level": level, "source_sha256": digest,
                "reason": "Semgrep inSource with an exact-rule source comment",
            })
        else:
            active.append(result)
            actual[key] += 1
    require(actual == expected, "Active SARIF findings do not match verified JSON")
    output = copy.deepcopy(sarif)
    output["runs"][0]["results"] = copy.deepcopy(active)
    return output, suppressed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", required=True)
    parser.add_argument("--sarif", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--inventory", required=True)
    args = parser.parse_args()
    output, inventory = Path(args.output), Path(args.inventory)
    output.unlink(missing_ok=True)
    inventory.unlink(missing_ok=True)
    try:
        report = json.loads(Path(args.json).read_text())
        upload, suppressed = prepare(report, json.loads(Path(args.sarif).read_text()), Path.cwd())
        payload = {
            "schema_version": "semgrep-suppressions/v1",
            "scanner_version": report["version"],
            "repository": os.environ["GITHUB_REPOSITORY"],
            "git_sha": os.environ["GITHUB_SHA"],
            "run_id": os.environ["GITHUB_RUN_ID"],
            "run_attempt": os.environ["GITHUB_RUN_ATTEMPT"],
            "suppressions": suppressed,
        }
        inventory.write_text(json.dumps(payload, indent=2) + "\n")
        output.write_text(json.dumps(upload, indent=2) + "\n")
    except Exception:
        # Never emit raw findings, source text, secrets or parser diagnostics.
        output.unlink(missing_ok=True)
        inventory.unlink(missing_ok=True)
        print("::error::Semgrep SARIF/source reconciliation failed; no upload prepared.")
        return 2
    print(f"Verified SARIF upload: {len(upload['runs'][0]['results'])} active findings; "
          f"{len(suppressed)} exact-rule in-source exceptions inventoried.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
