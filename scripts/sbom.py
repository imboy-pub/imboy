#!/usr/bin/env python3
"""Validate a source-manifest SBOM; never claim a resolved release/image inventory."""

import argparse
import copy
import hashlib
import json
from pathlib import Path
import re
import sys
from urllib.parse import quote, urlsplit


COVERAGE = "imboy:sbom:coverage"
SCOPE = "source-manifests-v1"
MANIFESTS = ("Makefile", "include/deps.mk", "erlang.mk", "rebar.lock",
             "docs-site/package.json", "docs-site/bun.lock")
LIMITATIONS = (
    "Source declaration union (runtime/build/test/local), not an active runtime graph; "
    "Git refs are not resolved build revisions. "
    "Erlang transitive dependencies, DOC_DEPS (including unresolved edown), OTP "
    "and external build tools are incomplete. Container images, OS packages, "
    "Dockerfile archive overrides and externally built bundles are not scanned."
)
NAME = r"[A-Za-z0-9_.-]+"
NPM_NAME = rf"(?:@{NAME}/)?{NAME}"
VERSION = r"[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?"


def require(condition, message):
    if not condition:
        raise ValueError(message)


def unique_object(pairs):
    result = {}
    for name, value in pairs:
        require(name not in result, f"Duplicate JSON key: {name}")
        result[name] = value
    return result


def json_loads(text):
    def reject_constant(value):
        raise ValueError("Invalid JSON constant: " + value)
    return json.loads(text, object_pairs_hook=unique_object, parse_constant=reject_constant)


def jsonc(text):
    """Remove comments/trailing commas outside strings; JSON still validates syntax."""
    strings = r'"(?:\\.|[^"\\])*"'
    text = re.sub(strings + r"|//[^\n]*|/\*[\s\S]*?\*/",
                  lambda m: m[0] if m[0].startswith('"') else " ", text)
    text = re.sub(strings + r"|,(?=\s*[}\]])",
                  lambda m: m[0] if m[0].startswith('"') else "", text)
    return json_loads(text)


def digest(value):
    return hashlib.sha256(value).hexdigest()


def inventory(values):
    return {"count": len(values), "sha256": digest("\n".join(sorted(values)).encode()),
            "status": "covered" if values else "none"}


def properties(value):
    entries = value.get("properties", [])
    require(isinstance(entries, list), "Properties must be an array")
    result = {}
    for entry in entries:
        require(isinstance(entry, dict) and isinstance(entry.get("name"), str)
                and isinstance(entry.get("value"), str), "Invalid property")
        require(entry["name"] not in result, "Duplicate property: " + entry["name"])
        result[entry["name"]] = entry["value"]
    return result


def dependency_fields(manifest):
    result = {}
    for field in ("dependencies", "devDependencies", "optionalDependencies", "peerDependencies"):
        values = manifest.get(field, {})
        require(isinstance(values, dict), f"Invalid {field}")
        for name, spec in values.items():
            require(re.fullmatch(NPM_NAME, name) and isinstance(spec, str) and spec
                    and not re.search(r"[:/#@$\\]", spec),
                    f"Unsupported npm declaration: {name}")
        result[field] = values
    return result


def coverage_inputs(root):
    """Return expected npm purls, evidenced Erlang components and source digests."""
    root = Path(root)
    try:
        sources = {name: (root / name).read_bytes() for name in MANIFESTS}
        texts = {name: content.decode("utf-8") for name, content in sources.items()}
    except (OSError, UnicodeError) as error:
        raise ValueError(f"Cannot read SBOM source manifests: {error}") from error
    hashes = {name: digest(content) for name, content in sources.items()}
    require(re.fullmatch(r"\s*\[\s*\]\s*\.\s*", re.sub(r"%[^\n]*", "", texts["rebar.lock"])),
            "Nonempty/unsupported rebar.lock requires extending the coverage contract")
    package = json_loads(texts["docs-site/package.json"])
    lock = jsonc(texts["docs-site/bun.lock"])
    require(isinstance(package, dict) and isinstance(lock, dict), "Invalid Bun manifests")
    require(set(lock) == {"lockfileVersion", "configVersion", "workspaces", "packages"}
            and not any(package.get(key) for key in ("workspaces", "overrides", "resolutions", "patchedDependencies")),
            "Unsupported Bun manifest/lock fields require extending coverage")
    require(type(lock.get("lockfileVersion")) is int and lock["lockfileVersion"] == 1
            and type(lock.get("configVersion")) is int and lock["configVersion"] == 1,
            "Unsupported Bun lock schema")
    require(isinstance(lock.get("workspaces"), dict) and set(lock["workspaces"]) == {""},
            "Unsupported Bun workspace layout")
    workspace = lock["workspaces"][""]
    require(isinstance(workspace, dict) and workspace.get("name") == package.get("name"),
            "Bun root workspace does not match package.json")
    requested = dependency_fields(package)
    require(requested == dependency_fields(workspace), "Bun lock root declarations are stale")
    packages = lock.get("packages")
    require(isinstance(packages, dict), "Bun lock packages must be an object")
    expected, package_names = set(), {}
    for key, entry in packages.items():
        require(isinstance(entry, list) and len(entry) == 4 and isinstance(entry[0], str)
                and isinstance(entry[1], str) and isinstance(entry[2], dict)
                and isinstance(entry[3], str) and entry[3].startswith(("sha256-", "sha512-")),
                f"Unsupported Bun package tuple: {key}")
        match = re.fullmatch(rf"({NPM_NAME})@({VERSION})", entry[0])
        require(match is not None, f"Unsupported non-registry Bun package: {key}")
        require(not entry[1] or entry[1].startswith("https://"),
                f"Unsupported Bun package source: {key}")
        name, version = match.groups()
        package_names[key] = name
        expected.add(f"pkg:npm/{quote(name, safe='/')}@{quote(version, safe='')}")
    for values in requested.values():
        for name in values:
            require(name in packages, f"Bun lock is missing root dependency: {name}")
            require(package_names[name] == name, f"Bun root dependency identity mismatch: {name}")

    # This is a union of literal declarations, including local/test roles. Do not
    # execute Make or silently interpret a future expression as an empty graph.
    graph, declarations, conditions, graph_conditions = {}, set(), [], {}
    unconditional_runtime = set()
    source_include = False
    makefile = texts["Makefile"].replace("\\\n", " ")
    for line in makefile.splitlines():
        line = line.split("#", 1)[0].strip()
        if line == "include include/deps.mk" and not conditions:
            source_include = True
        require(not re.match(r"^(?:(?:override|export)\s+)?dep_\w+\s*[:+?!]?=", line),
                "Makefile dependency overrides require extending SBOM coverage")
        if re.match(r"^(ifeq|ifneq|ifdef|ifndef)\b", line):
            conditions.append(line)
        elif line == "endif" and conditions:
            conditions.pop()
        elif line.startswith("else") and conditions:
            conditions[-1] = "else: " + conditions[-1]
        match = re.match(r"^(?:(?:override|export)\s+)?(DEPS|BUILD_DEPS|TEST_DEPS)\s*([:+?!]?=)(.*)$", line)
        if not match:
            require(not re.search(r"\b(?:DEPS|BUILD_DEPS|TEST_DEPS)\s*[:+?!]?=", line),
                    "Unsupported indirect Make dependency declaration")
            continue
        role, operator, value = match.groups()
        require(operator in ("=", ":=", "+=") and (operator == "+=" or role not in declarations),
                "Unsupported repeated/conditional Make assignment: " + role)
        declarations.add(role)
        for name in value.split():
            require(re.fullmatch(NAME, name), f"Unsupported dynamic {role} declaration")
            graph.setdefault(name, set()).add(role)
            if role == "DEPS" and not conditions:
                unconditional_runtime.add(name)
            if conditions:
                graph_conditions.setdefault(name, set()).add("; ".join(conditions))
    require(source_include and declarations == {"DEPS", "BUILD_DEPS", "TEST_DEPS"},
            "Missing critical Make dependency declarations; absence is not an empty graph")
    definitions = {}
    for line in texts["include/deps.mk"].splitlines():
        line = line.split("#", 1)[0].strip()
        match = re.fullmatch(r"dep_(\w+)\s*=\s*(.*)", line)
        require(match or not re.match(r"^(?:(?:override|export)\s+)?dep_", line),
                "Unsupported Erlang source assignment")
        if match:
            require(match[1] not in definitions, "Ambiguous dependency definition: " + match[1])
            definitions[match[1]] = match[2].split()
    if any((definitions.get(name) or [None])[0] == "hex" for name in graph):
        graph.setdefault("hex_core", set()).add("AUTO_BUILD_DEPS")
    require(not any(name + "_commit" in definitions for name in graph),
            "Erlang commit overrides require extending SBOM coverage")
    erlang = []
    for name, roles in sorted(graph.items()):
        parts = definitions.get(name, [])
        require(parts and parts[0] in ("git", "hex"), "Unidentified Erlang dependency: " + name)
        props = {"imboy:sbom:domain": "erlang", "imboy:sbom:declaration": name,
                 "imboy:sbom:roles": ",".join(sorted(roles)),
                 "imboy:sbom:manifest": "Makefile;include/deps.mk"}
        component = {"type": "library", "name": name}
        if name not in unconditional_runtime:
            component["scope"] = "excluded"
        if name in graph_conditions:
            props["imboy:sbom:condition"] = " | ".join(sorted(graph_conditions[name]))
        if parts[0] == "hex":
            require(len(parts) in (2, 3) and re.fullmatch(VERSION, parts[1]),
                    "Unsupported Hex dependency: " + name)
            package_name = parts[2] if len(parts) == 3 else name
            require(re.fullmatch(NAME, package_name), "Invalid Hex package name")
            component.update(name=package_name, version=parts[1],
                             purl=f"pkg:hex/{quote(package_name, safe='')}@{parts[1]}")
            props["imboy:sbom:resolution"] = "declared-version"
        else:
            require(len(parts) == 3, "Unsupported Git dependency: " + name)
            source, ref = parts[1:]
            parsed = urlsplit(source)
            require(parsed.scheme == "https" and parsed.hostname and not parsed.username
                    and not parsed.query and not parsed.fragment
                    and re.fullmatch(r"[A-Za-z0-9._/-]+", ref), "Unsupported Git source/ref: " + name)
            pinned = re.fullmatch(rf"(?:v?{VERSION}|[0-9a-fA-F]{{40}})", ref)
            if pinned:
                component["version"] = ref
            props.update({"imboy:sbom:source": source, "imboy:sbom:declared-ref": ref,
                          "imboy:sbom:resolution": "declared-ref" if pinned else "unresolved"})
            suffix = "@" + quote(ref, safe="") if pinned else ""
            component["purl"] = (f"pkg:generic/{quote(name, safe='')}{suffix}?vcs_url="
                                 + quote("git+" + source + "@" + ref, safe=""))
            component["externalReferences"] = [{"type": "vcs", "url": source}]
        component["bom-ref"] = component["purl"]
        component["properties"] = [{"name": key, "value": value} for key, value in props.items()]
        erlang.append(component)
    return expected, erlang, hashes


def basic(bom):
    require(isinstance(bom, dict) and bom.get("bomFormat") == "CycloneDX"
            and bom.get("specVersion") in ("1.5", "1.6", "1.7")
            and type(bom.get("version")) is int and bom["version"] > 0,
            "Invalid CycloneDX document")
    require(isinstance(bom.get("metadata"), dict) and isinstance(bom.get("components"), list),
            "CycloneDX metadata/components are missing or invalid")
    properties(bom["metadata"])
    root_component = bom["metadata"].get("component")
    if root_component is not None:
        require(isinstance(root_component, dict) and isinstance(root_component.get("bom-ref"), str)
                and root_component["bom-ref"] and isinstance(root_component.get("name"), str)
                and root_component["name"] and root_component.get("type") == "application",
                "Invalid CycloneDX root component")
    seen = set()
    for component in bom["components"]:
        require(isinstance(component, dict) and isinstance(component.get("name"), str)
                and component["name"] and component.get("type") in (
                    "application", "framework", "library", "container", "platform", "operating-system",
                    "device", "device-driver", "firmware", "file", "machine-learning-model", "data", "cryptographic-asset"),
                "Invalid CycloneDX component")
        properties(component)
        ref = component.get("bom-ref")
        require(isinstance(ref, str) and ref and ref not in seen,
                "Missing or duplicate component bom-ref")
        seen.add(ref)
        require("version" not in component or isinstance(component["version"], str),
                "Invalid component version")
        require("group" not in component or isinstance(component["group"], str), "Invalid component group")
        require("purl" not in component or (isinstance(component["purl"], str)
                and re.fullmatch(r"pkg:[a-z][a-z0-9.+-]*/[^\s]+", component["purl"])),
                "Invalid component purl")
    if root_component is not None:
        require(root_component["bom-ref"] not in seen, "Root/component bom-ref collision")
    graph = bom.get("dependencies", [])
    require(isinstance(graph, list), "Invalid CycloneDX dependency graph")
    graph_refs = set()
    for entry in graph:
        require(isinstance(entry, dict) and isinstance(entry.get("ref"), str) and entry["ref"]
                and entry["ref"] not in graph_refs and isinstance(entry.get("dependsOn", []), list)
                and all(isinstance(ref, str) and ref for ref in entry.get("dependsOn", [])),
                "Invalid CycloneDX dependency graph entry")
        graph_refs.add(entry["ref"])
    return bom["metadata"]


def domains(bom):
    result = {"npm": set(), "erlang": set()}
    for component in bom["components"]:
        require(not component.get("components"), "Nested components require extending coverage")
        purl = component.get("purl")
        props = properties(component)
        if purl is None:
            require(component["type"] == "application" and component["name"] == "docs-site/bun.lock",
                    "Unidentified component requires extending SBOM coverage")
            continue
        require(isinstance(purl, str) and purl.startswith("pkg:"), "Invalid component purl")
        require(component["type"] == "library", "Package identity must identify a library")
        if purl.startswith("pkg:npm/"):
            domain = "npm"
            name = (component.get("group", "") + "/" if component.get("group") else "") + component["name"]
            version = component.get("version", "")
            require(re.fullmatch(NPM_NAME, name) and re.fullmatch(VERSION, version)
                    and purl == f"pkg:npm/{quote(name, safe='/')}@{quote(version, safe='')}",
                    "npm purl does not match component name/version")
        else:
            require(props.get("imboy:sbom:domain") == "erlang"
                    and purl.startswith(("pkg:hex/", "pkg:generic/")),
                    "Uncovered package ecosystem: " + purl)
            domain = "erlang"
            if purl.startswith("pkg:hex/"):
                require(re.fullmatch(NAME, component["name"]) and re.fullmatch(VERSION, component.get("version", ""))
                        and purl == f"pkg:hex/{quote(component['name'], safe='')}@{component['version']}",
                        "Hex purl does not match component name/version")
            else:
                source, ref = props.get("imboy:sbom:source", ""), props.get("imboy:sbom:declared-ref", "")
                pinned = re.fullmatch(rf"(?:v?{VERSION}|[0-9a-fA-F]{{40}})", ref)
                suffix = "@" + quote(ref, safe="") if pinned else ""
                require(source.startswith("https://") and re.fullmatch(r"[A-Za-z0-9._/-]+", ref)
                        and component.get("version") == (ref if pinned else None)
                        and purl == f"pkg:generic/{quote(component['name'], safe='')}{suffix}?vcs_url="
                        + quote("git+" + source + "@" + ref, safe=""),
                        "Git purl does not match declared source/ref")
        require(purl not in result[domain], "Duplicate dependency identity: " + purl)
        result[domain].add(purl)
    return result


def contract(expected, erlang, hashes):
    return {"version": 1, "scope": SCOPE, "manifests": hashes,
            "domains": {"npm": inventory(expected),
                        "erlang": inventory({item["purl"] for item in erlang})}}


def prepare(root, raw_bom):
    bom = copy.deepcopy(raw_bom)
    metadata = basic(bom)
    require(COVERAGE not in properties(metadata), "Input SBOM already has a coverage contract")
    expected, erlang, hashes = coverage_inputs(root)
    actual = domains(bom)
    require(actual == {"npm": expected, "erlang": set()},
            "Trivy npm coverage differs from Bun lock (include dev dependencies; no omissions allowed)")
    bom["components"].extend(erlang)
    root_component = metadata.get("component", {})
    require(isinstance(root_component, dict) and isinstance(root_component.get("bom-ref"), str)
            and root_component["bom-ref"]
            and root_component["bom-ref"] not in {item["bom-ref"] for item in bom["components"]},
            "Trivy metadata root component is missing")
    root_ref = root_component["bom-ref"]
    dependencies = bom.setdefault("dependencies", [])
    require(isinstance(dependencies, list) and all(isinstance(item, dict) for item in dependencies),
            "Invalid dependency graph")
    roots = [item for item in dependencies if item.get("ref") == root_ref]
    require(len(roots) <= 1, "Duplicate root dependency graph entry")
    if not roots:
        roots = [{"ref": root_ref, "dependsOn": []}]
        dependencies.extend(roots)
    require(isinstance(roots[0].get("dependsOn"), list), "Invalid root dependsOn")
    roots[0]["dependsOn"] = sorted(set(roots[0]["dependsOn"]) | {item["bom-ref"] for item in erlang})
    # No empty leaf edges are invented for Erlang: its transitive graph is unknown.
    metadata.setdefault("properties", []).extend([
        {"name": COVERAGE, "value": json.dumps(contract(expected, erlang, hashes), sort_keys=True)},
        {"name": "imboy:sbom:limitations", "value": LIMITATIONS},
    ])
    metadata["lifecycles"] = [{"phase": "pre-build"}]
    bom["compositions"] = [{"aggregate": "incomplete", "assemblies": [root_ref]}]
    validate(bom, root)
    return bom


def validate(bom, root=None):
    metadata = basic(bom)
    value = properties(metadata).get(COVERAGE)
    require(value is not None, "Legacy SBOM has no coverage contract; explicit rebaseline is required")
    coverage = json_loads(value)
    require(isinstance(coverage, dict) and set(coverage) == {"version", "scope", "manifests", "domains"}
            and type(coverage.get("version")) is int and coverage["version"] == 1
            and coverage.get("scope") == SCOPE, "Unsupported coverage contract")
    hashes = coverage["manifests"]
    require(isinstance(hashes, dict) and set(hashes) == set(MANIFESTS)
            and all(isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value)
                    for value in hashes.values()), "Invalid manifest fingerprints")
    require(isinstance(coverage["domains"], dict) and set(coverage["domains"]) == {"npm", "erlang"}
            and all(isinstance(value, dict) and type(value.get("count")) is int
                    for value in coverage["domains"].values()), "Invalid coverage domains")
    require(properties(metadata).get("imboy:sbom:limitations") == LIMITATIONS
            and metadata.get("lifecycles") == [{"phase": "pre-build"}]
            and isinstance(metadata.get("component"), dict)
            and bom.get("compositions") == [{"aggregate": "incomplete", "assemblies": [metadata["component"].get("bom-ref")]}],
            "Missing or inconsistent SBOM completeness declaration")
    actual = {key: inventory(values) for key, values in domains(bom).items()}
    require(coverage["domains"] == actual, "SBOM coverage count/digest mismatch")
    if root is not None:
        require(coverage == contract(*coverage_inputs(root)),
                "SBOM no longer matches source manifests or expected dependency coverage")
    return coverage


def identities(bom):
    return {item["purl"] for item in bom["components"] if "purl" in item}


def versions(bom):
    result = {}
    for item in bom["components"]:
        if "purl" not in item:
            continue
        props = properties(item)
        git = item["purl"].startswith("pkg:generic/")
        key = (item["purl"].split("/", 1)[0], item.get("group", "") if item["purl"].startswith("pkg:npm/") else "",
               item["name"], props["imboy:sbom:source"] if git else "")
        result.setdefault(key, set()).add(props["imboy:sbom:declared-ref"] if git else item.get("version", ""))
    return result


def full_inventory(bom):
    return ["", "<details><summary>完整依赖列表 / Full dependency identities</summary>", "",
            *["- `" + value.replace("`", "%60") + "`" for value in sorted(identities(bom))],
            "", "</details>"]


def summary(root, current, previous=None, rebaseline_legacy=False):
    coverage = validate(current, root)
    lines = ["## SBOM 源码依赖报告 / Source dependency report", "",
             "| 覆盖域 / Domain | 状态 / Status | 依赖数 / Dependencies |",
             "|---|---|---|"]
    for domain, details in coverage["domains"].items():
        lines.append(f"| {domain} | {details['status']} | {details['count']} |")
    lines.extend(["", "**整体完整性 / Overall completeness: incomplete.** " + LIMITATIONS, ""])
    if previous is None:
        lines.append("首次生成：确认没有历史 artifact，建立已校验的源码清单基线。 / First verified baseline.")
        lines.extend(full_inventory(current))
        return "\n".join(lines) + "\n", {}
    metadata = basic(previous)
    if COVERAGE not in properties(metadata):
        require(rebaseline_legacy, "Legacy baseline is incomparable; explicit rebaseline is required")
        lines.append("旧基线不可比：经显式批准重新建立基线，不产生增删数字。 / "
                     "Legacy baseline is incomparable; explicitly re-established, without change counts.")
        lines.extend(full_inventory(current))
        return "\n".join(lines) + "\n", {}
    validate(previous)
    old, new = identities(previous), identities(current)
    before, after = versions(previous), versions(current)
    changed = [key for key in before.keys() & after.keys() if before[key] != after[key]]
    added, removed = sorted(new - old), sorted(old - new)
    counts = {"sbom_added": str(len(added)), "sbom_removed": str(len(removed)), "sbom_changed": str(len(changed))}
    lines.extend(["| 类型 / Change | 数量 / Count |", "|---|---|",
                  f"| 新增身份 / Added identities | {len(added)} |",
                  f"| 删除身份 / Removed identities | {len(removed)} |",
                  f"| 版本集合变更 / Changed version sets | {len(changed)} |", "",
                  "版本更新同时计入身份新增/删除；按来源区分多版本集合。 / "
                  "Version updates also appear as added/removed identities; versions are compared as sets.", ""])
    for label, values in (("新增 / Added", added), ("删除 / Removed", removed)):
        if values:
            lines.extend([f"### {label}", ""] + ["- `" + value.replace("`", "%60") + "`" for value in values] + [""])
    if not added and not removed and not changed:
        lines.append("无依赖变更；源码覆盖合同均已校验。 / No dependency changes; both coverage contracts validated.")
    return "\n".join(lines) + "\n", counts


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    build = commands.add_parser("prepare")
    build.add_argument("--root", required=True, type=Path)
    build.add_argument("--input", required=True, type=Path)
    build.add_argument("--output", required=True, type=Path)
    report = commands.add_parser("report")
    report.add_argument("--root", required=True, type=Path)
    report.add_argument("--current", required=True, type=Path)
    report.add_argument("--previous", type=Path)
    report.add_argument("--found", required=True, choices=("true", "false"))
    report.add_argument("--rebaseline-legacy", default="false", choices=("true", "false"))
    report.add_argument("--summary", required=True, type=Path)
    report.add_argument("--outputs", required=True, type=Path)
    args = parser.parse_args()
    try:
        if args.command == "prepare":
            bom = prepare(args.root, json_loads(args.input.read_text()))
            args.output.write_text(json.dumps(bom, ensure_ascii=False, indent=2) + "\n")
        else:
            current = json_loads(args.current.read_text())
            previous = None
            if args.found == "true":
                require(args.previous is not None, "Previous SBOM path is required")
                previous = json_loads(args.previous.read_text())
            markdown, counts = summary(args.root, current, previous, args.rebaseline_legacy == "true")
            with args.summary.open("a") as stream:
                stream.write(markdown)
            with args.outputs.open("a") as stream:
                stream.writelines(f"{key}={value}\n" for key, value in counts.items())
    except (ValueError, OSError) as error:
        print(f"SBOM coverage error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
