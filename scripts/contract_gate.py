#!/usr/bin/env python3
"""P3-C2 API Contract Gate (Golden Gates §2.3 C2) — 导出器 + 双端校验器。

真源（Source of Truth，计划 §2.1「真源永远在后端」）：
  - 端点清单：src/imboy_router.erl（可执行路由实况，静态提取，不可漂移）
  - 枚举值域：priv/migrations/*.up.sql 的 CHECK ... = ANY(...) 约束
    （按迁移序号取最新定义，模拟迁移顺序应用）
  - 错误码：include/error_code.hrl（-define(ERR_*, N) 段摘要；
    flutter 全量契约由 C1 的 imboyapp/scripts/generate_error_code.dart --check 承担）

子命令：
  export            生成 .contract/api_contract.json（确定性输出：内容不变则文件不变，
                    无时间戳等不稳定字段——C1 教训）
  check             ①重导出与落仓产物 diff（router/迁移漂移自检）
                    ②--admin DIR：枚举注册表 vs admin TS 实际值集（bindings 比对）
                    ③--flutter DIR：flutter 注释值域 vs 枚举注册表
                    ④--admin 时附带 EntityId/TSID 规则子集检查
                    退出码：0=无漂移 / 1=漂移或违规（打印明细）

用法：
  python3 scripts/contract_gate.py export
  python3 scripts/contract_gate.py check [--admin <dir>] [--flutter <dir>]
  缺省自动探测同级目录 ../imboyadmin、../imboyapp（存在则启用）。

自动提取 vs 人工维护：
  自动：端点/免鉴权白名单（router）、枚举值域（迁移 CHECK）、错误码摘要（hrl）
  人工：CLIENT_BINDINGS（客户端在哪个文件/接口字段/变量里镜像了哪个枚举）
        与 must 子集（见 binding 的 mode/must 字段及文件尾 TODO 注释）。
"""
import argparse
import difflib
import json
import re
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CONTRACT_PATH = REPO / ".contract" / "api_contract.json"
FORMAT_VERSION = 1

# ---------------------------------------------------------------------------
# 后端真源：DB CHECK 枚举约束白名单（约束名 → 契约枚举 key）
# 人工维护。新增带 = ANY 枚举约束的迁移时，在此登记才会进契约物。
# ---------------------------------------------------------------------------
ENUM_SOURCES = {
    "chk_wallet_status": "wallet_status",
    "chk_wallet_tx_status": "wallet_tx_status",
    "chk_wallet_tx_type": "wallet_tx_type",
    "chk_recharge_order_status": "recharge_order_status",
    "chk_payment_tx_biz_type": "payment_tx_biz_type",
    "chk_payment_tx_status": "payment_tx_status",
    "chk_billing_plan_period": "billing_plan_period",
    "chk_billing_plan_status": "billing_plan_status",
    "chk_billing_sub_status": "billing_subscription_status",
    "chk_billing_invoice_status": "billing_invoice_status",
}

# ---------------------------------------------------------------------------
# 客户端镜像 bindings（人工维护的声明式清单，校验器自动按 kind 提取值集）
# mode=exact:  提取集 == 后端 canonical（label map / 全量值域注释）
# mode=subset: 提取集 ⊆ canonical 且 ⊇ must（客户端注释只列子集的现状妥协，
#              演进目标全部转 exact，见文件尾 TODO）
# ---------------------------------------------------------------------------
def _b(file, kind, enum, mode, **kw):
    d = {"file": file, "kind": kind, "enum": enum, "mode": mode}
    d.update(kw)
    return d

ADMIN_ENUM_BINDINGS = [
    _b("src/types/billing.ts", "ts_interface_field", "wallet_status", "exact",
       interface="Wallet", field="status"),
    _b("src/types/billing.ts", "ts_interface_field", "wallet_tx_type", "exact",
       interface="WalletTransaction", field="type"),
    _b("src/types/billing.ts", "ts_interface_field", "wallet_tx_status", "exact",
       interface="WithdrawalTransaction", field="status"),
    _b("src/types/billing.ts", "ts_interface_field", "recharge_order_status", "exact",
       interface="RechargeOrder", field="status"),
    _b("src/types/billing.ts", "ts_interface_field", "payment_tx_biz_type", "exact",
       interface="PaymentTransaction", field="biz_type"),
    _b("src/types/billing.ts", "ts_interface_field", "payment_tx_status", "exact",
       interface="PaymentTransaction", field="status"),
    _b("src/types/billing.ts", "ts_literal_union", "billing_plan_period", "exact",
       interface="BillingPlan", field="billing_period"),
    _b("src/types/billing.ts", "ts_interface_field", "billing_plan_status", "exact",
       interface="BillingPlan", field="status"),
    _b("src/types/billing.ts", "ts_interface_field", "billing_subscription_status", "exact",
       interface="BillingSubscription", field="status"),
    _b("src/types/billing.ts", "ts_interface_field", "billing_invoice_status", "exact",
       interface="BillingInvoice", field="status"),
    _b("src/pages/payment-transactions/PaymentTransactionListPage.tsx",
       "ts_record_map", "payment_tx_biz_type", "exact", var="BIZ_TYPE_LABELS"),
    _b("src/pages/payment-transactions/PaymentTransactionListPage.tsx",
       "ts_record_map", "payment_tx_status", "exact", var="TX_STATUS_LABELS"),
]

FLUTTER_ENUM_BINDINGS = [
    _b("lib/page/wallet/wallet_provider.dart", "dart_comment_ints", "wallet_tx_type",
       "exact", anchor="chk_wallet_tx_type"),
]

# EntityId/TSID 契约（计划 §2.1）：canonical = JSON integer (TSID 64-bit)，
# admin 网络层 safeParseBigIntJson 转 string，TS 类型一律 EntityId。
ENTITY_ID_RULES = {
    "safeParseBigIntJson 文件存在": ("src/lib/safeParseBigIntJson.ts", None),
    "admin 网络层接入 safeParseBigIntJson": (
        "src/services/api/client.ts", r"safeParseBigIntJson"),
    "EntityId 类型定义 (export type EntityId = string)": (
        "src/types/common.ts", r"export\s+type\s+EntityId\s*=\s*string"),
}
# ID 字段禁裸 number（grep 级可稳定判定子集；AST 级全量检查见 TODO）
ENTITY_ID_FORBIDDEN_FIELDS = [
    "id", "user_id", "wallet_id", "plan_id", "tenant_id",
    "subscription_id", "group_id", "channel_id",
]

# ---------------------------------------------------------------------------
# 端点提取（src/imboy_router.erl 静态解析）
# ---------------------------------------------------------------------------
ROUTE_RE = re.compile(r'\{\s*"(/[^"]*)"\s*,\s*([a-z_][a-z0-9_]*)\s*,\s*(.*?)\}\s*\}', re.S)
ACTION_RE = re.compile(r"action\s*=>\s*([a-z0-9_]+)")
BIN_PATH_RE = re.compile(r'<<("([^"]+)")>>')

SCOPE_MARKERS = [
    ("main", re.compile(r"^\s*MainRoutes\s*=", re.M)),
    ("api_v1", re.compile(r"^\s*ApiV1Routes\s*=", re.M)),
    ("adm", re.compile(r"^\s*AdmRoutes\s*=\s*\[", re.M)),
]


def _slice_between(text, start_re, end_re):
    """取 start_re 匹配起点到 end_re 匹配起点之间的文本段。"""
    m = start_re.search(text)
    if m is None:
        return ""
    rest = text[m.start():]
    m2 = end_re.search(rest[len(m.group(0)):])
    if m2 is None:
        return rest
    return rest[: len(m.group(0)) + m2.start()]


def extract_routes(router_src: str) -> dict:
    """静态提取 cowboy 路由三元组，按 scope 分组返回排序后的端点清单。"""
    seg_main = _slice_between(router_src, SCOPE_MARKERS[0][1], SCOPE_MARKERS[1][1])
    seg_v1 = _slice_between(router_src, SCOPE_MARKERS[1][1], SCOPE_MARKERS[2][1])
    seg_adm = _slice_between(
        router_src, SCOPE_MARKERS[2][1], re.compile(r"^\s*CoreRoutes\s*=", re.M))
    seg_test = _slice_between(
        router_src, re.compile(r"^test_routes_v1\(\)\s*->", re.M),
        re.compile(r"^-spec\s+test_open_routes", re.M))
    scopes = {
        "main": seg_main, "api_v1": seg_v1, "adm": seg_adm,
        "test_dev_only": seg_test,
    }
    out = {}
    for scope, seg in scopes.items():
        eps = []
        for m in ROUTE_RE.finditer(seg):
            path, handler, opts = m.group(1), m.group(2), m.group(3)
            am = ACTION_RE.search(opts)
            eps.append({
                "path": path, "handler": handler,
                "action": am.group(1) if am else None,
                "static": handler == "cowboy_static",
            })
        eps.sort(key=lambda e: (e["path"], e["handler"], e["action"] or ""))
        _assert_no_duplicate_paths(scope, eps)
        out[scope] = eps
    return out


def _assert_no_duplicate_paths(scope, eps):
    seen = {}
    for e in eps:
        if e["path"] in seen:
            raise SystemExit(f"contract export: scope={scope} 重复路由 {e['path']}（cowboy 不允许）")
        seen[e["path"]] = e


def extract_open_whitelist(router_src: str) -> list:
    """提取 open()/option() 免鉴权白名单（不含 test_open_routes 动态部分）。"""
    seg = _slice_between(
        router_src, re.compile(r"^open\(\)\s*->", re.M),
        re.compile(r"\]\s*\+\+\s*test_open_routes\(\)", re.M))
    paths = sorted({m.group(2) for m in BIN_PATH_RE.finditer(seg)})
    _assert_no_duplicate_binaries(paths)
    return paths


def _assert_no_duplicate_binaries(paths):
    if len(paths) != len(set(paths)):
        raise SystemExit("contract export: open() 白名单存在重复项（提取正则异常）")


# ---------------------------------------------------------------------------
# 枚举提取（priv/migrations CHECK 约束，按迁移序号取最新定义）
# ---------------------------------------------------------------------------
CHECK_RE = re.compile(
    r"CONSTRAINT\s+(\w+)\s+CHECK\s*\(+\s*\(?\s*(\w+)\s*\)?(?:::\w+)?\s*=\s*ANY"
    r"\s*\(\s*ARRAY\s*\[([^\]]*)\]", re.I)


def extract_db_enums(migrations_dir: Path) -> dict:
    latest = {}  # constraint_name -> (seq, column, values, source_migration)
    for f in sorted(migrations_dir.glob("*.up.sql")):
        seq = f.name.split("_", 1)[0]
        text = f.read_text(encoding="utf-8")
        for m in CHECK_RE.finditer(text):
            name, column, raw = m.group(1), m.group(2), m.group(3)
            if name not in ENUM_SOURCES:
                continue
            vals = _parse_array_values(raw)
            if vals:
                latest[name] = (seq, column, vals, f.name)
    out = {}
    for name, key in ENUM_SOURCES.items():
        if name not in latest:
            raise SystemExit(f"contract export: 枚举约束 {name} 未在任何迁移中找到")
        seq, column, vals, fname = latest[name]
        out[key] = {
            "source": f"priv/migrations/{fname} ({name}, column {column})",
            "kind": "str" if any(isinstance(v, str) for v in vals) else "int",
            "values": vals,
        }
    return dict(sorted(out.items()))


def _parse_array_values(raw: str):
    vals = []
    for tok in raw.split(","):
        tok = tok.strip()
        tok = re.sub(r"::\w+$", "", tok).strip().strip("'\"")
        if not tok:
            continue
        if re.fullmatch(r"-?\d+", tok):
            vals.append(int(tok))
        elif re.fullmatch(r"[A-Za-z_][\w.-]*", tok):
            vals.append(tok)
    return sorted(set(vals), key=lambda v: (str(type(v)), v))


# ---------------------------------------------------------------------------
# 错误码摘要（include/error_code.hrl）
# ---------------------------------------------------------------------------
ERR_DEFINE_RE = re.compile(r"-define\((ERR_\w+),\s*(\d+)\)")


def extract_error_code_summary(hrl_path: Path) -> dict:
    text = hrl_path.read_text(encoding="utf-8")
    defines = ERR_DEFINE_RE.findall(text)
    codes = sorted({int(c) for _, c in defines})
    segments = {}
    for c in codes:
        seg = (c // 100) * 100
        segments[str(seg)] = segments.get(str(seg), 0) + 1
    return {
        "source": "include/error_code.hrl",
        "defines": len(defines),
        "distinct_codes": codes,
        "segments": dict(sorted(segments.items())),
        "note": "flutter 全量契约由 C1 imboyapp/scripts/generate_error_code.dart --check 承担",
    }


# ---------------------------------------------------------------------------
# 契约物构建与导出
# ---------------------------------------------------------------------------
def build_contract() -> dict:
    router = REPO / "src" / "imboy_router.erl"
    router_src = router.read_text(encoding="utf-8")
    return {
        "format_version": FORMAT_VERSION,
        "generator": "scripts/contract_gate.py",
        "sources": {
            "router": "src/imboy_router.erl",
            "migrations_dir": "priv/migrations",
            "error_code": "include/error_code.hrl",
        },
        "notes": {
            "method": "cowboy 路由无 method 维度（handler 内分派），method 契约不在第一版范围",
            "plugin_routes": "插件路由为运行时 ETS 注册，静态导出不含（plugin_routes/0）",
            "entity_id_tsid": "canonical: JSON integer (TSID 64-bit)；admin 网络层"
                              " safeParseBigIntJson 转 string；TS 类型一律 EntityId",
            "server_ts": "13 位毫秒 number（< 2^53，JSON number 安全）",
        },
        "endpoints": extract_routes(router_src),
        "auth_open_whitelist": extract_open_whitelist(router_src),
        "enums": extract_db_enums(REPO / "priv" / "migrations"),
        "error_code_summary": extract_error_code_summary(REPO / "include" / "error_code.hrl"),
    }


def render_contract(contract: dict) -> str:
    return json.dumps(contract, ensure_ascii=False, indent=2, sort_keys=True) + "\n"


def cmd_export() -> int:
    CONTRACT_PATH.parent.mkdir(parents=True, exist_ok=True)
    CONTRACT_PATH.write_text(render_contract(build_contract()), encoding="utf-8")
    n = sum(len(v) for v in build_contract()["endpoints"].values())
    print(f"contract exported: {CONTRACT_PATH.relative_to(REPO)} "
          f"(endpoints={n}, enums={len(ENUM_SOURCES)})")
    return 0


# ---------------------------------------------------------------------------
# 客户端值集提取器（TS / Dart 源码正则级解析）
# ---------------------------------------------------------------------------
def _ts_interface_block(text: str, interface: str):
    m = re.search(rf"export\s+interface\s+{interface}\s*\{{", text)
    if m is None:
        return None
    end = text.find("\n}", m.end())
    return text[m.end(): end if end != -1 else len(text)]


def _jsdoc_before(block: str, field: str):
    lines = block.splitlines()
    idx = next((i for i, ln in enumerate(lines)
                if re.match(rf"^\s*{field}\??\s*:", ln)), None)
    if idx is None:
        return None
    docs = []
    for ln in reversed(lines[:idx]):
        s = ln.strip()
        if not s:
            continue
        if s.startswith("/*") or s.startswith("*") or s.startswith("//"):
            docs.append(s)
            if s.startswith("/*"):  # /** 开始（含单行 /** ... */ 形态）= 块边界
                break
        else:
            break
    return "\n".join(reversed(docs))


def extract_ts_interface_field(text: str, interface: str, field: str):
    """提取 interface 字段 JSDoc 注释中的 `N=label` 整数值集。"""
    block = _ts_interface_block(text, interface)
    if block is None:
        return None, f"interface {interface} 未找到"
    docs = _jsdoc_before(block, field)
    if docs is None:
        return None, f"字段 {interface}.{field} 未找到"
    return sorted({int(v) for v in re.findall(r"(\d+)=", docs)}), None


def extract_ts_literal_union(text: str, interface: str, field: str):
    """提取 interface 字段的 `'a' | 'b'` 字面量联合值集。"""
    block = _ts_interface_block(text, interface)
    if block is None:
        return None, f"interface {interface} 未找到"
    m = re.search(rf"^\s*{field}\??\s*:\s*([^\n]+)", block, re.M)
    if m is None:
        return None, f"字段 {interface}.{field} 未找到"
    return sorted(set(re.findall(r"'([^']+)'", m.group(1)))), None


def extract_ts_record_map(text: str, var: str):
    """提取 `const VAR: Record<..> = { '1': 'x', ... }` 的数字键值集。"""
    m = re.search(rf"const\s+{var}\s*:\s*Record[^=]*=\s*\{{", text)
    if m is None:
        return None, f"常量 {var} 未找到"
    end = text.find("}", m.end())
    if end == -1:
        return None, f"常量 {var} 未闭合"
    body = text[m.end(): end]
    return sorted({int(v) for v in re.findall(r"'(\d+)'\s*:", body)}), None


def extract_dart_comment_ints(text: str, anchor: str):
    """提取 Dart 连续 // 注释块中的 `1充值 20 agent` 式整数值集。"""
    lines = text.splitlines()
    idx = next((i for i, ln in enumerate(lines) if anchor in ln and ln.strip().startswith("//")), None)
    if idx is None:
        return None, f"锚点注释（含 {anchor}）未找到"
    vals = set()
    for ln in lines[idx:]:
        s = ln.strip()
        if not s.startswith("//"):
            break
        vals.update(int(v) for v in re.findall(r"(\d+)(?:\s*[^\s\d])", s[len("//"):]))
    return sorted(vals), None


EXTRACTORS = {
    "ts_interface_field": lambda text, b: extract_ts_interface_field(
        text, b["interface"], b["field"]),
    "ts_literal_union": lambda text, b: extract_ts_literal_union(
        text, b["interface"], b["field"]),
    "ts_record_map": lambda text, b: extract_ts_record_map(text, b["var"]),
    "dart_comment_ints": lambda text, b: extract_dart_comment_ints(text, b["anchor"]),
}


# ---------------------------------------------------------------------------
# 校验逻辑
# ---------------------------------------------------------------------------
def _diff_enum(enum_key, canonical, extracted, mode, must, where, failures):
    if extracted is None:
        failures.append(f"{where}: 枚举 {enum_key} 提取失败（binding 失配，文件被改？）")
        return
    exp, got = set(canonical), set(extracted)
    if mode == "exact" and got != exp:
        missing, extra = sorted(exp - got), sorted(got - exp)
        failures.append(
            f"{where}: 枚举 {enum_key} 漂移（exact）：后端={sorted(exp)} "
            f"客户端={sorted(got)}；缺失={missing} 多余={extra}")
    elif mode == "subset":
        if got - exp:
            failures.append(
                f"{where}: 枚举 {enum_key} 漂移：客户端出现后端不存在的值 "
                f"{sorted(got - exp)}（后端={sorted(exp)}）")
        lack = sorted(set(must or []) - got)
        if lack:
            failures.append(
                f"{where}: 枚举 {enum_key} 关键值缺失 {lack}（must ⊆ 客户端断言失败）")


def check_client_bindings(root: Path, bindings, contract, repo_label, failures):
    enums = contract["enums"]
    per_file = {}
    for b in bindings:
        per_file.setdefault(b["file"], []).append(b)
    for rel, bs in sorted(per_file.items()):
        path = root / rel
        if not path.is_file():
            failures.append(f"[{repo_label}] {rel}: 文件不存在（binding 过期？）")
            continue
        text = path.read_text(encoding="utf-8")
        for b in bs:
            got, err = EXTRACTORS[b["kind"]](text, b)
            if err:
                failures.append(f"[{repo_label}] {rel}: {err}")
                continue
            _diff_enum(b["enum"], enums[b["enum"]]["values"], got,
                       b["mode"], b.get("must"), f"[{repo_label}] {rel}", failures)


def check_entity_id_rules(admin_dir: Path, failures):
    for desc, (rel, pattern) in ENTITY_ID_RULES.items():
        path = admin_dir / rel
        if not path.is_file():
            failures.append(f"[admin][EntityId] {desc}: 文件 {rel} 不存在")
            continue
        if pattern and not re.search(pattern, path.read_text(encoding="utf-8")):
            failures.append(f"[admin][EntityId] {desc}: 模式未命中（{rel}）")
    field_re = re.compile(
        rf"^\s*({'|'.join(ENTITY_ID_FORBIDDEN_FIELDS)})\??\s*:\s*number", re.M)
    for path in sorted((admin_dir / "src" / "types").rglob("*.ts*")):
        for m in field_re.finditer(path.read_text(encoding="utf-8")):
            failures.append(
                f"[admin][EntityId] {path.relative_to(admin_dir)}: "
                f"ID 字段 `{m.group(1)}: number` 违规——TSID 必须用 EntityId(string)")


def check_openapi_coverage(contract):
    """informational：router 实况 vs api/openapi.yaml 手工契约覆盖差异（不阻塞）。"""
    openapi = REPO / "api" / "openapi.yaml"
    if not openapi.is_file():
        return
    seg = openapi.read_text(encoding="utf-8")
    m = re.search(r"^paths:\n(.*?)^\w", seg, re.M | re.S)
    declared = set(re.findall(r"^\s{2}(/\S+):\s*$", m.group(1), re.M)) if m else set()
    router_paths = set()
    for scope in ("main", "api_v1", "adm"):
        for e in contract["endpoints"][scope]:
            if not e["static"]:
                router_paths.add(re.sub(r":(\w+)", r"{\1}", e["path"]))
    print(f"[info] OpenAPI 覆盖（informational，不阻塞）："
          f"router 端点={len(router_paths)} openapi 声明={len(declared)} "
          f"交集={len(router_paths & declared)} "
          f"router有openapi无={len(router_paths - declared)} "
          f"openapi有router无={len(declared - router_paths)}")


def _self_check(contract) -> list:
    with tempfile.NamedTemporaryFile("w+", suffix=".json", delete=False,
                                     encoding="utf-8") as tmp:
        tmp.write(render_contract(contract))
        tmp.flush()
        fresh = Path(tmp.name).read_text(encoding="utf-8")
    if not CONTRACT_PATH.is_file():
        return [f"契约产物未落仓：{CONTRACT_PATH.relative_to(REPO)} 不存在；先运行 make contract-export"]
    committed = CONTRACT_PATH.read_text(encoding="utf-8")
    if committed == fresh:
        return []
    diff = list(difflib.unified_diff(
        committed.splitlines(), fresh.splitlines(),
        fromfile=".contract/api_contract.json (落仓)", tofile="(重导出)", lineterm=""))
    head = "\n".join(diff[:60])
    return ["后端真源已变更但契约产物未同步（非法漂移）。"
            "合法变更流程（计划 §2.2）：同一 PR 内运行 make contract-export 并提交产物。\n" + head]


def cmd_check(admin_dir, flutter_dir) -> int:
    contract = build_contract()
    failures = _self_check(contract)
    if failures:
        print("== 自检 FAIL：落仓契约与后端真源不一致 ==")
    else:
        print("== 自检 PASS：落仓契约与 router/迁移/错误码真源一致 ==")
    if admin_dir:
        check_client_bindings(admin_dir, ADMIN_ENUM_BINDINGS, contract, "admin", failures)
        check_entity_id_rules(admin_dir, failures)
    else:
        print("[skip] admin diff（未指定 --admin 且 ../imboyadmin 不存在）")
    if flutter_dir:
        check_client_bindings(flutter_dir, FLUTTER_ENUM_BINDINGS, contract, "flutter", failures)
    else:
        print("[skip] flutter diff（未指定 --flutter 且 ../imboyapp 不存在）")
    check_openapi_coverage(contract)
    if failures:
        print(f"\n== Contract Gate FAIL：{len(failures)} 处漂移/违规 ==")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("== Contract Gate PASS ==")
    return 0


def _autodetect(name):
    for cand in (REPO.parent / name, REPO / name):
        if (cand / "src").is_dir() or (cand / "lib").is_dir():
            return cand
    return None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("export", help="导出契约产物 .contract/api_contract.json")
    ck = sub.add_parser("check", help="契约校验（漂移非零退出）")
    ck.add_argument("--admin", default=None, help="imboyadmin 仓根目录")
    ck.add_argument("--flutter", default=None, help="imboyapp 仓根目录")
    ck.add_argument("--no-admin", action="store_true")
    ck.add_argument("--no-flutter", action="store_true")
    args = ap.parse_args()
    if args.cmd == "export":
        return cmd_export()
    admin = None if args.no_admin else (Path(args.admin) if args.admin else _autodetect("imboyadmin"))
    flutter = None if args.no_flutter else (Path(args.flutter) if args.flutter else _autodetect("imboyapp"))
    return cmd_check(admin, flutter)


if __name__ == "__main__":
    sys.exit(main())
