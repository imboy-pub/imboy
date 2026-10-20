#!/usr/bin/env python3
"""FULL-07 §8：企业 internal 面**发布候选**机械断言（12 项，fail-closed）。

把四方真源一次性对齐，任何一侧漂移即 exit 1：

  ① 冻结路由表      src/api/enterprise_internal_routes.erl        （合同源）
  ② cowboy 注册表   src/imboy_router.erl  enterprise_internal_routes/0
  ③ 边界规格表      src/api/enterprise_internal_boundary.erl      （授权源）
  ④ OpenAPI 合同    .contract/api_contract.json endpoints.api_internal
  ⑤ A0 冻结 manifest  control/internal-api-manifest.yaml（经 --manifest / env 传入）
  ⑥ 接线测试        test/api/enterprise_internal_wiring_http_tests.erl（计数钉）

外加 §8 硬边界：仓库里 `/api/open/v1` 生产路由数量为 **0**（含 alias / handler /
migration / OpenAPI 全谱）。

设计口径与 A0 control/assert_manifest.py 一致（注释行不算生产面、`{name}` 与
`:name` 占位符归一后比对）；本脚本是 FULL-07 自己的 RC 门禁，不改 A0 的脚本。

用法：
  python3 scripts/check_enterprise_release_manifest.py \
      [--repo .] [--manifest <control/internal-api-manifest.yaml>]
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
from pathlib import Path

import yaml

FROZEN_TABLE = "src/api/enterprise_internal_routes.erl"
ROUTER = "src/imboy_router.erl"
AUTH_MW = "src/api/auth_middleware.erl"
BOUNDARY = "src/api/enterprise_internal_boundary.erl"
CONTRACT = ".contract/api_contract.json"
WIRING_TEST = "test/api/enterprise_internal_wiring_http_tests.erl"
DEFAULT_MANIFEST = os.environ.get("IMBOY_INTERNAL_MANIFEST", str(Path(__file__).resolve().parents[1] / "api/internal/v1/manifest.yaml"))

PREFIX = "/api/internal/v1/"
# V2.1 只读扩面（INT-24..31）：路由 23→31、cowboy path 19→25（2026-09-24）
MANIFEST_ROUTE_COUNT = 47
ROUTER_PATH_COUNT = 33
# 代码类扫描根（§8「含 alias / handler / migration / OpenAPI 全谱」）
CODE_ROOTS = ("src", "config", "priv")
CODE_SUFFIXES = {".erl", ".hrl", ".config", ".example", ".json", ".yaml", ".sql"}

failures: list[str] = []
checks = 0


def check(ok: bool, label: str, detail: str = "") -> None:
    global checks
    checks += 1
    print(("PASS " if ok else "FAIL ") + label + (f"  {detail}" if detail else ""))
    if not ok:
        failures.append(label)


def code_lines(text: str) -> str:
    """去掉整行注释（Erlang % / SQL -- / YAML #）——注释里提到 token 不算生产面。"""
    keep = []
    for ln in text.splitlines():
        s = ln.lstrip()
        if s.startswith("%") or s.startswith("--") or s.startswith("#"):
            continue
        keep.append(ln)
    return "\n".join(keep)


def cowboy_to_frozen(path: str) -> str:
    return re.sub(r":([a-z_]+)", r"{\1}", path)


def frozen_routes(repo: Path) -> list[tuple[str, str]]:
    src = (repo / FROZEN_TABLE).read_text(encoding="utf-8")
    return re.findall(r'method\s*=>\s*<<"([A-Z]+)">>.*?path\s*=>\s*<<"([^"]+)">>', src, re.S)


def router_paths(repo: Path) -> list[str]:
    src = (repo / ROUTER).read_text(encoding="utf-8")
    m = re.search(r"^-spec enterprise_internal_routes\(\) ->.*$", src, re.M)
    if not m:
        return []
    return re.findall(r'\{\s*"(/api/internal/v1/[^"]*)"\s*,', src[m.start() :])


class UniqueKeyLoader(yaml.SafeLoader):
    def construct_mapping(self, node, deep=False):
        mapping = super().construct_mapping(node, deep=deep)
        if len(mapping) != len(node.value):
            raise ValueError("duplicate YAML mapping key")
        return mapping


def manifest_operations(path: Path) -> list[tuple[str, str, str]]:
    document = yaml.load(path.read_text(encoding="utf-8"), Loader=UniqueKeyLoader)
    if not isinstance(document, dict) or not isinstance(document.get("routes"), list):
        raise ValueError("manifest.routes must be an array")
    operations = []
    ids = set()
    for route in document["routes"]:
        if not isinstance(route, dict) or not all(
            isinstance(route.get(key), str) and route[key] for key in ("id", "method", "path")
        ):
            raise ValueError("route requires string id/method/path")
        ident = route["id"]
        if ident in ids:
            raise ValueError("duplicate route id: " + ident)
        ids.add(ident)
        if ident == "HUMAN-SSO-01":
            if (route["method"], route["path"]) != ("POST", "/api/v1/oa/sso/code"):
                raise ValueError("invalid human SSO counterpart")
            continue
        if not re.fullmatch(r"INT-\d+", ident):
            raise ValueError("unknown route id: " + ident)
        operations.append((ident, route["method"], route["path"]))
    return operations


def contract_metadata(repo: Path, manifest: Path) -> tuple[dict, dict, dict, set, dict]:
    """Read semantic metadata in addition to the legacy route-count assertions."""
    registry = {}
    source = (repo / FROZEN_TABLE).read_text(encoding="utf-8")
    for ident, block in re.findall(r'#\{\s*id\s*=>\s*<<"(INT-\d+)">>(.*?)\n\s*}', source, re.S):
        def value(key, binary=True):
            pattern = key + (r'\s*=>\s*<<"([^"]+)"' if binary else r'\s*=>\s*([a-z_]+)')
            found = re.search(pattern, block)
            return found[1] if found else None
        scope = value("scope") or "dynamic:messages_send"
        registry[ident] = (value("method"), value("path"), scope,
                           value("rate_bucket", False), value("idempotency", False))
    document = yaml.load(manifest.read_text(encoding="utf-8"), Loader=UniqueKeyLoader)
    declared = {}
    for row in document["routes"]:
        if row["id"].startswith("INT-"):
            scope = row["scope"]
            if scope.startswith("dynamic "):
                scope = "dynamic:messages_send"
            declared[row["id"]] = (row["method"], row["path"], scope, row["rate"], row["idempotency"])
    root = yaml.load((repo / "api/openapi-internal.yaml").read_text(), Loader=UniqueKeyLoader)
    operations = {}
    schemas = {}
    for path, ref in root["paths"].items():
        leaf = yaml.load((repo / "api" / ref["$ref"]).read_text(), Loader=UniqueKeyLoader)
        for method, operation in leaf.items():
            if method.upper() in {"GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"}:
                pair = (method.upper(), path)
                operations[pair] = (operation.get("x-imboy-scope"),
                    operation.get("x-imboy-rate-bucket"), operation.get("x-imboy-idempotency"))
                if pair == ("POST", PREFIX + "oa/sso/exchange"):
                    schemas["sso"] = operation["responses"]["200"]["content"]["application/json"]["schema"]
    collection = json.loads((repo / "api/internal/v1/IMBoy-Internal-API-v1.postman_collection.json").read_text())
    requests = set()
    def visit(items):
        for item in items:
            if "item" in item:
                visit(item["item"])
            elif "request" in item:
                request = item["request"]
                url = request["url"]
                raw = url["raw"] if isinstance(url, dict) else url
                path = raw.replace("{{base_url}}", "").split("?")[0]
                path = re.sub(r'\{\{([^}]+)}}', r'{\1}', path)
                requests.add((request["method"], path))
    visit(collection["item"])
    return registry, declared, operations, requests, schemas


def metadata_errors(registry: dict, declared: dict, operations: dict,
                    requests: set, schemas: dict) -> list[str]:
    errors = []
    if registry != declared:
        errors.append("manifest scope/rate/idempotency differs from registry")
    expected = {(row[0], row[1]) for row in registry.values()}
    if expected != set(operations):
        errors.append("OpenAPI operation set differs from registry")
    for ident, (method, path, scope, rate, idem) in registry.items():
        if operations.get((method, path)) != (scope, rate, idem):
            errors.append(ident + " OpenAPI scope/rate/idempotency mismatch")
    internal = {(m, p) for m, p in requests if p.startswith(PREFIX)}
    if internal != expected:
        errors.append("Postman Internal operation set differs from registry")
    human = {(m, p) for m, p in requests if not p.startswith(PREFIX)}
    if human != {("POST", "/api/v1/oa/login/" + action) for action in ("exchange", "link", "register")}:
        errors.append("unexpected/missing Human OA companion requests")
    sso = schemas.get("sso", {})
    org = sso.get("properties", {}).get("organization_id", {})
    if ("organization_id" not in sso.get("required", [])
            or set(org.get("type", [])) != {"integer", "null"}
            or org.get("format") != "int64" or "nullable" in org):
        errors.append("SSO organization_id must remain required int64-or-null under OAS 3.1")
    return errors


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default=".")
    ap.add_argument("--manifest", default=DEFAULT_MANIFEST)
    args = ap.parse_args()
    repo = Path(args.repo).resolve()

    # ---------------- ① §8 硬边界：/api/open/v1 生产面 = 0 ----------------
    hits: list[str] = []
    for base in CODE_ROOTS:
        d = repo / base
        if not d.is_dir():
            continue
        for f in sorted(d.rglob("*")):
            if not f.is_file() or f.suffix not in CODE_SUFFIXES:
                continue
            if "_test" in f.name:
                continue
            if "open/v1" in code_lines(f.read_text(encoding="utf-8", errors="ignore")):
                hits.append(str(f.relative_to(repo)))
    # OpenAPI 合同（JSON，不能按行去注释）
    contract_text = (repo / CONTRACT).read_text(encoding="utf-8")
    if "open/v1" in contract_text:
        hits.append(CONTRACT)
    check(not hits, "1) open_v1_surface=0 (src/config/priv/.contract 全谱)", f"hits={hits}")

    # ---------------- ② 冻结表条数 + 前缀 ----------------
    routes = frozen_routes(repo)
    bad_prefix = [p for _m, p in routes if not p.startswith(PREFIX)]
    check(
        len(routes) == MANIFEST_ROUTE_COUNT and not bad_prefix,
        f"2) frozen_table_route_count={MANIFEST_ROUTE_COUNT} 且前缀 {PREFIX}",
        f"got={len(routes)} offenders={bad_prefix}",
    )

    # ---------------- ③ cowboy 注册表条数 ----------------
    registered = router_paths(repo)
    check(
        len(registered) == ROUTER_PATH_COUNT,
        f"3) router_internal_path_count={ROUTER_PATH_COUNT}",
        f"got={len(registered)}",
    )

    # ---------------- ④ 冻结表路径集 == 注册表路径集 ----------------
    frozen_paths = sorted({cowboy_to_frozen(p) for _m, p in routes})
    registered_norm = sorted({cowboy_to_frozen(p) for p in registered})
    check(
        frozen_paths == registered_norm,
        "4) frozen_table_paths == router_paths",
        ""
        if frozen_paths == registered_norm
        else f"missing={set(frozen_paths)-set(registered_norm)} "
        f"extra={set(registered_norm)-set(frozen_paths)}",
    )

    # ---------------- ⑤ 每个 internal 路由的 handler 模块文件存在 ----------------
    rsrc = (repo / ROUTER).read_text(encoding="utf-8")
    m = re.search(r"^-spec enterprise_internal_routes\(\) ->.*$", rsrc, re.M)
    tail = rsrc[m.start() :] if m else ""
    handlers = sorted(
        set(re.findall(r'\{\s*"/api/internal/v1/[^"]*"\s*,\s*([a-z_][a-z0-9_]*)\s*,', tail))
    )
    missing = [h for h in handlers if not (repo / "src" / "api" / f"{h}.erl").is_file()]
    check(not missing, "5) internal_handler_modules_exist", f"missing={missing}")

    # ---------------- ⑥ internal 前缀不得进匿名白名单 ----------------
    aw = (repo / AUTH_MW).read_text(encoding="utf-8")
    open_lists = re.findall(r'<<"(/api/internal/v1/[^"]*)">>', aw)
    check(not open_lists, "6) internal_prefix_not_in_open_whitelist", f"offenders={open_lists}")

    # ---------------- ⑦ auth 中间件：internal 分支在 /api/v1/ 之前且委托 ----------------
    i_internal = aw.find('<<"/api/internal/v1/", _Tail/binary>>')
    i_v1 = aw.find('<<"/api/v1/", _Tail/binary>>')
    check(
        i_internal != -1 and i_v1 != -1 and i_internal < i_v1,
        "7) auth_middleware_internal_branch_before_v1 + delegate",
        f"internal@{i_internal} v1@{i_v1} delegate="
        f"{'yes' if 'enterprise_internal_middleware:execute' in aw else 'NO'}",
    )

    # ---------------- ⑧ A0 冻结 manifest 的 INT id 序列 == 冻结表 ----------------
    mf = Path(args.manifest)
    if mf.is_file():
        table_ids = re.findall(
            r'id\s*=>\s*<<"(INT-\d+)"', (repo / FROZEN_TABLE).read_text(encoding="utf-8")
        )
        expected = [(ident, method, path) for ident, (method, path) in zip(table_ids, routes)]
        try:
            actual = manifest_operations(mf)
            check(actual == expected and len(actual) == MANIFEST_ROUTE_COUNT,
                  f"8) manifest.routes == frozen_table ID/method/path ({MANIFEST_ROUTE_COUNT}, 有序)",
                  "" if actual == expected else f"manifest={len(actual)} table={len(expected)}; operation mismatch")
        except (ValueError, yaml.YAMLError, OSError) as exc:
            check(False, "8) manifest.routes schema", str(exc))
    else:
        check(False, "8) manifest_ids == frozen_table_ids", f"manifest 缺失: {mf}")

    # ---------------- ⑨ 边界规格表 id 与冻结表双向一致 ----------------
    bsrc = (repo / BOUNDARY).read_text(encoding="utf-8")
    b_ids = sorted(set(re.findall(r'spec\s*\(\s*<<"(INT-\d+)">>\s*\)', bsrc)))
    t_ids = sorted(
        set(
            re.findall(
                r'id\s*=>\s*<<"(INT-\d+)"', (repo / FROZEN_TABLE).read_text(encoding="utf-8")
            )
        )
    )
    check(
        b_ids == t_ids and len(b_ids) == MANIFEST_ROUTE_COUNT,
        f"9) boundary_ids == frozen_table_ids ({MANIFEST_ROUTE_COUNT}, 双向)",
        "" if b_ids == t_ids else f"boundary={len(b_ids)} table={len(t_ids)}",
    )

    # ---------------- ⑩ 动态 scope 只允许 INT-09/10 ----------------
    fsrc = (repo / FROZEN_TABLE).read_text(encoding="utf-8")
    dyn = re.findall(
        r'id\s*=>\s*<<"(INT-\d+)">>\s*,\s*method\s*=>\s*<<"[A-Z]+">>\s*,'
        r'\s*path\s*=>\s*<<"[^"]+">>\s*,\s*scope\s*=>\s*\{dynamic,',
        fsrc,
        re.S,
    )
    check(
        sorted(dyn) == ["INT-09", "INT-10"],
        "10) dynamic_scope_only_INT09_INT10",
        f"got={dyn}",
    )

    # ---------------- ⑪ OpenAPI 合同的 internal 路径集 == 冻结表路径集 ----------------
    contract = json.loads((repo / CONTRACT).read_text(encoding="utf-8"))
    ci = contract.get("endpoints", {}).get("api_internal", [])
    contract_paths = sorted({cowboy_to_frozen(e["path"]) for e in ci})
    contract_handlers = sorted({e["handler"] for e in ci})
    check(
        contract_paths == frozen_paths
        and len(contract_paths) == ROUTER_PATH_COUNT
        and contract_handlers == handlers,
        f"11) openapi_contract_internal_paths == frozen_table_paths ({ROUTER_PATH_COUNT})"
        " 且 handler 集一致",
        ""
        if contract_paths == frozen_paths
        else f"contract={len(contract_paths)} frozen={len(frozen_paths)} "
        f"hdiff={set(contract_handlers) ^ set(handlers)}",
    )

    # ---------------- ⑫ 接线测试必须把 31 / 25 / open=0 钉成常量 ----------------
    # 路径数钉的变量名在接线测试里为 WiredPaths（路由列表为 Manifest），
    # 两种历史命名（ManifestPaths / WiredPaths）均接受。
    wsrc = (repo / WIRING_TEST).read_text(encoding="utf-8")
    path_pins = [
        f"?assertEqual({ROUTER_PATH_COUNT}, length(ManifestPaths))",
        f"?assertEqual({ROUTER_PATH_COUNT}, length(WiredPaths))",
    ]
    pins = [
        f"?assertEqual({MANIFEST_ROUTE_COUNT}, length(Manifest))",
        "?assertEqual(0, length([P || P <- Paths, lists:prefix(\"/api/open/v1/\", P)]))",
    ]
    if not any(p in wsrc for p in path_pins):
        pins.insert(1, " OR ".join(path_pins))
    unpinned = [p for p in pins if p not in wsrc]
    check(
        not unpinned,
        f"12) wiring_test_pins({MANIFEST_ROUTE_COUNT}/{ROUTER_PATH_COUNT}/open=0)",
        f"unpinned={unpinned}",
    )

    try:
        drift = metadata_errors(*contract_metadata(repo, mf))
        check(not drift, "13) semantic metadata + Postman + OAS 3.1 nullable", "; ".join(drift))
    except (KeyError, ValueError, TypeError, OSError, yaml.YAMLError) as error:
        check(False, "13) semantic contract readable", str(error))

    print()
    if failures:
        print(f"RC MANIFEST ASSERT FAILED: {len(failures)}/{checks} 项 -> {failures}")
        return 1
    print(f"RC MANIFEST ASSERT PASS ({checks}/{checks} 项全过)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
