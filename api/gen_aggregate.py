#!/usr/bin/env python3
"""Generate the OpenAPI bridge from the three audience roots (requires PyYAML).

Run from the repository root: python3 api/gen_aggregate.py [--check].
Path references retain the two-line bridge consumed by existing route gates.
"""
import copy
from pathlib import Path
import sys

import yaml

API_DIR = Path(__file__).resolve().parent
ROOTS = ("openapi-api.yaml", "openapi-adm.yaml", "openapi-internal.yaml")
AGG = "openapi.yaml"
METHODS = {"get", "post", "put", "patch", "delete", "head", "options", "trace"}


class UniqueKeyLoader(yaml.SafeLoader):
    """Reject duplicate keys instead of silently losing a path or component."""

    _MERGE_KEY = object()

    def __init__(self, stream):
        super().__init__(stream)
        self._checked_mappings = set()

    def flatten_mapping(self, node):
        # SafeLoader recursively flattens inline merge sources without calling
        # construct_mapping. Check here before inherited keys enter the node,
        # and only once because aliases can reuse an already flattened mapping.
        if node not in self._checked_mappings:
            seen = set()
            for key_node, _ in node.value:
                key = (self._MERGE_KEY if key_node.tag == "tag:yaml.org,2002:merge"
                       else self.construct_object(key_node))
                if key in seen:
                    raise ValueError("duplicate YAML mapping key")
                seen.add(key)
            self._checked_mappings.add(node)
        return super().flatten_mapping(node)


class Reference(str):
    """Keep file references quoted for existing shell contract consumers."""


class Dumper(yaml.SafeDumper):
    def ignore_aliases(self, data):
        return True


Dumper.add_representer(Reference, lambda dumper, value: dumper.represent_scalar(
    "tag:yaml.org,2002:str", value, style="'"))


def load(path):
    document = yaml.load(Path(path).read_text(encoding="utf-8"), Loader=UniqueKeyLoader)
    if not isinstance(document, dict):
        raise ValueError(f"{path}: expected a YAML mapping")
    return document


def add_paths(aggregate, root, name, api_dir):
    if not isinstance(root.get("paths"), dict):
        raise ValueError(f"{name}: paths must be an explicit mapping")
    for path, item in root["paths"].items():
        if path in aggregate["paths"]:
            raise ValueError(f"duplicate path across roots: {path} ({name})")
        if (not isinstance(item, dict) or set(item) != {"$ref"}
                or not isinstance(item["$ref"], str)
                or not item["$ref"].startswith("./paths/")):
            raise ValueError(f"{name}: {path} must reference a local path file")
        leaf = load(api_dir / item["$ref"])
        # A shared Path Item inherits the aggregate's default security. Require
        # explicit security whenever its audience has another default.
        for method, operation in leaf.items():
            if method not in METHODS:
                continue
            if not isinstance(operation, dict):
                raise ValueError(f"{name}: {method.upper()} {path} must be a mapping")
            expected = operation.get("security", root.get("security", []))
            actual = operation.get("security", aggregate.get("security", []))
            if expected != actual:
                raise ValueError(f"{name}: {method.upper()} {path} needs explicit security")
        aggregate["paths"][path] = {"$ref": Reference(item["$ref"])}


def merge_components(aggregate, root, name):
    for section, entries in root.get("components", {}).items():
        target = aggregate["components"].setdefault(section, {})
        for key, value in entries.items():
            if key in target and target[key] != value:
                raise ValueError(f"components/{section}/{key} differs in {name}")
            target[key] = copy.deepcopy(value)


def build(api_dir=None):
    api_dir = API_DIR if api_dir is None else Path(api_dir)
    first = load(api_dir / ROOTS[0])
    info = copy.deepcopy(first["info"])
    info.update(title="IMBoy REST API", summary="IMBoy HTTP / REST 三受众聚合入口",
                description="Generated from openapi-api.yaml, openapi-adm.yaml and "
                "openapi-internal.yaml. Edit the audience roots and path files, "
                "then run python3 api/gen_aggregate.py. Authentication and wire "
                "formats are defined separately for each audience.")
    aggregate = {"openapi": first["openapi"], "info": info,
                 "servers": first.get("servers", []), "tags": [], "paths": {},
                 "components": {}, "security": first.get("security", [])}
    tags = set()
    for name in ROOTS:
        root = load(api_dir / name)
        if root["openapi"] != aggregate["openapi"]:
            raise ValueError(f"OpenAPI version differs in {name}")
        add_paths(aggregate, root, name, api_dir)
        merge_components(aggregate, root, name)
        for tag in root.get("tags", []):
            if tag["name"] not in tags:
                aggregate["tags"].append(tag)
                tags.add(tag["name"])
    header = "# GENERATED by api/gen_aggregate.py; edit the three audience roots instead.\n"
    return header + yaml.dump(aggregate, Dumper=Dumper, allow_unicode=True,
                              sort_keys=False, width=100)


def main():
    try:
        content = build()
    except (ValueError, OSError, yaml.YAMLError) as error:
        print(f"aggregate: {error}", file=sys.stderr)
        return 1
    target = API_DIR / AGG
    if "--check" in sys.argv:
        if target.read_text(encoding="utf-8") != content:
            print(f"{AGG}: OUT OF SYNC; run python3 api/gen_aggregate.py")
            return 1
        print(f"{AGG}: up to date with all three audience roots")
        return 0
    target.write_text(content, encoding="utf-8")
    print(f"generated {AGG}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
