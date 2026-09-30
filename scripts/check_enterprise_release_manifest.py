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

FROZEN_TABLE = "src/api/enterprise_internal_routes.erl"
ROUTER = "src/imboy_router.erl"
AUTH_MW = "src/api/auth_middleware.erl"
BOUNDARY = "src/api/enterprise_internal_boundary.erl"
CONTRACT = ".contract/api_contract.json"
WIRING_TEST = "test/api/enterprise_internal_wiring_http_tests.erl"
DEFAULT_MANIFEST = os.environ.get("IMBOY_INTERNAL_MANIFEST", str(Path(__file__).resolve().parents[1] / "api/internal/v1/manifest.yaml"))

PREFIX = "/api/internal/v1/"
# V2.1 只读扩面（INT-24..31）：路由 23→31、cowboy path 19→25（2026-09-24）
MANIFEST_ROUTE_COUNT = 36
ROUTER_PATH_COUNT = 28
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
        ids = re.findall(r"^\s*-\s*id:\s*(INT-\d+)", mf.read_text(encoding="utf-8"), re.M)
        table_ids = re.findall(
            r'id\s*=>\s*<<"(INT-\d+)"', (repo / FROZEN_TABLE).read_text(encoding="utf-8")
        )
        check(
            ids == table_ids and len(ids) == MANIFEST_ROUTE_COUNT,
            f"8) manifest_ids == frozen_table_ids ({MANIFEST_ROUTE_COUNT}, 有序)",
            "" if ids == table_ids else f"manifest={len(ids)} table={len(table_ids)}",
        )
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

    print()
    if failures:
        print(f"RC MANIFEST ASSERT FAILED: {len(failures)}/{checks} 项 -> {failures}")
        return 1
    print(f"RC MANIFEST ASSERT PASS ({checks}/12 项全过)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
