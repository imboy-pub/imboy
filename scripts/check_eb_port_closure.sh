#!/usr/bin/env bash
# EB-03R A01/A04/A05/A10/A11 的**调用方契约门**（Port ↔ 应用层调用点闭包）。
#
# 为什么需要（R0-1 的根因）：Erlang 的 `-behaviour` 只在**编译期**检查实现方是否齐全，
# **不检查调用方是否越界**。application 层通过 `eb_infra_ports:resolve/1` 拿到实现模块名
# 后，就能调用它的任意导出函数——于是 12 个真实依赖的 callback 从未出现在冻结契约里，
# 契约门对所有能力缺口**结构性失明**。本脚本把「调用点 ⊆ 声明」变成可机械判定的事实。
#
# 判定项：
#   A01  Port 声明的 callback **覆盖** application/** 的全部调用点（逐条点名缺失项）
#   A04  application/** 零 `eb_pg_` 前缀模块名；`eb_tx_port` / `eb_purge_port` 的实现
#        导出面**白名单**（不得出现 exec/1、query/2、transaction/1 等通用接口）
#   A05  `--require <fn>/<arity>` 四件齐：Port 声明 + registry contracts + 实现导出 + 测试
#   A10  `--readonly <port>`：该端口的 callback 里**零写**（无 insert/update/delete/advance…）
#   A11  `--require put_private/3 --require stream_content/3 --require delete_private/3`
#
# 负例（必须真的红）：
#   --self-test   用一批**故意越界**的夹具（未声明 callback、注入 exec/1、给只读端口加写
#                 callback、抽掉四件中的一件）重跑同一套判定，要求每条都被点名判红。
#
# 用法：
#   bash scripts/check_eb_port_closure.sh
#   bash scripts/check_eb_port_closure.sh --require fetch_hold/3
#   bash scripts/check_eb_port_closure.sh --readonly eb_member_fact_port
#   bash scripts/check_eb_port_closure.sh --self-test
#
# 说明：静态分析基于**源码**（`-callback` 声明 / `-export` 列表 / 调用点），不依赖已编译
# 的 ebin —— 这样门在「编译前」也能跑（EB-03 起各卡的门序都是 compile → eunit → db_it）。
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

if ! command -v python3 >/dev/null 2>&1; then
  echo "[FAIL] 需要 python3 才能运行本门（静态分析）" >&2
  exit 1
fi

python3 - "$@" <<'PYEOF'
import re
import sys
from pathlib import Path

FEATURE = "src/features/enterprise_business"
APP_REL = FEATURE + "/application"
PORT_VARS = {
    "Store": "eb_store_port",
    "Audit": "eb_audit_port",
    "Crypto": "eb_crypto_port",
    "Clock": "eb_clock_port",
    "Id": "eb_id_port",
    "Asset": "eb_asset_port",
    "Auth": "eb_auth_port",
    "MemberFact": "eb_member_fact_port",
    "Fact": "eb_member_fact_port",
    "Tx": "eb_tx_port",
    "Purge": "eb_purge_port",
}
# 装配事实（与 `eb_infra_ports:implementations/0` 对应；脚本下方会验证 -behaviour 声明）
PORT_IMPL = {
    "eb_store_port": "eb_pg_store",
    "eb_crypto_port": "eb_managed_crypto",
    "eb_clock_port": "eb_system_clock",
    "eb_id_port": "eb_tsid",
    "eb_audit_port": "eb_pg_audit",
    "eb_asset_port": "eb_asset_store",
    "eb_auth_port": "eb_pg_auth_facts",
    "eb_member_fact_port": "eb_member_fact_pg",
    "eb_tx_port": "eb_pg_tx",
    "eb_purge_port": "eb_pg_purge_port",
}
# A04：Port 实现的导出面白名单（具名用例之外**不得**有任何通用 SQL/事务接口）
EXPORT_WHITELIST = {
    "eb_pg_tx": {"accept_message", "append_conversation_audit"},
    "eb_pg_purge_port": {"purge_batch"},
}
FORBIDDEN_EXPORTS = {"exec", "query", "transaction", "sql", "prepare", "raw", "squery", "equery"}
WRITE_WORDS = re.compile(r"insert|update|delete|advance|append|purge|upsert|record|set_")

failures = []
notes = []


def fail(msg):
    failures.append(msg)


def ok(msg):
    print("[OK] %s" % msg)


# ------------------------------------------------------------------ 源码工具
def strip_comments(text: str) -> str:
    return "\n".join(line.split("%")[0] for line in text.split("\n"))


def split_top(s: str):
    parts, depth, cur, instr = [], 0, [], False
    for ch in s:
        if ch == '"':
            instr = not instr
        if not instr:
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                if depth == 0:
                    parts.append("".join(cur))
                    return [p for p in parts if p.strip()]
                depth -= 1
            elif ch == "," and depth == 0:
                parts.append("".join(cur))
                cur = []
                continue
        cur.append(ch)
    parts.append("".join(cur))
    return [p for p in parts if p.strip()]


def callbacks_of(source: str):
    """返回 {fn: arity}（-callback 声明，arity 按顶层逗号切分）。"""
    code = strip_comments(source)
    out = {}
    for m in re.finditer(r"-callback\s+([a-z_][a-zA-Z0-9_]*)\s*\(", code):
        name = m.group(1)
        rest = code[m.end():]
        out[name] = len(split_top(rest))
    return out


def exports_of(source: str):
    code = strip_comments(source)
    m = re.search(r"-export\s*\(\s*\[(.*?)\]\s*\)", code, re.S)
    if not m:
        return set()
    blob = m.group(1)
    names = set()
    for item in blob.split(","):
        item = item.strip()
        if not item:
            continue
        name = item.split("/")[0].strip()
        if re.match(r"^[a-z_][a-zA-Z0-9_]*$", name):
            names.add(name)
    return names


def call_sites(source: str):
    code = strip_comments(source)
    out = set()
    for m in re.finditer(r"\b([A-Z][A-Za-z0-9_]*)\s*:\s*([a-z_][a-zA-Z0-9_]*)\s*\(", code):
        var, fn = m.group(1), m.group(2)
        port = PORT_VARS.get(var)
        if not port:
            continue
        arity = len(split_top(code[m.end():]))
        out.add((port, "%s/%d" % (fn, arity)))
    return out


def module_index(*roots):
    index = {}
    for root in roots:
        base = Path(root)
        if not base.is_dir():
            continue
        for path in sorted(base.rglob("*.erl")):
            m = re.search(r"-module\(([a-z][a-z0-9_]*)\)", path.read_text(errors="replace"))
            if m:
                index.setdefault(m.group(1), path)
    return index


def application_roots():
    local = Path(APP_REL)
    run_root = Path("../../").resolve()
    roots = []
    if local.is_dir():
        roots.append(local)
    roots.extend(sorted((run_root / "worktrees").glob("*" + "/" + APP_REL)))
    roots.extend(sorted(run_root.glob(APP_REL)))
    return [r for r in roots if r.is_dir()]


def sibling_run_roots():
    run_root = Path("../../").resolve()
    return sorted((run_root / "worktrees").glob("*"))


# ------------------------------------------------------------------ 索引
main_index = module_index(FEATURE, "src")
# 兄弟 worktree 的 application 层（EB-05/06 的用例层落在别的 worktree）
app_roots = application_roots()

port_files = {}
for name, path in main_index.items():
    if name.endswith("_port"):
        port_files[name] = path

declared = {}
for port, path in port_files.items():
    declared[port] = callbacks_of(path.read_text(errors="replace"))

sites = set()
sites_by_file = {}
local_sites = set()
local_sites_by_file = {}
sibling_sites = set()
local_root = Path(APP_REL).resolve()
for root in app_roots:
    for path in sorted(root.rglob("*.erl")):
        if path.name.endswith("_port.erl") or path.name == "eb_ports.erl":
            continue
        found = call_sites(path.read_text(errors="replace"))
        if found:
            sites_by_file[str(path)] = found
            sites |= found
            if path.resolve().is_relative_to(local_root):
                local_sites_by_file[str(path)] = found
                local_sites |= found
            else:
                sibling_sites |= found

print("== EB-03R 端口闭包门 ==")
print("扫描 application 根：%d 个（本地 + 兄弟 worktree）" % len(app_roots))
for root in app_roots:
    print("  - %s" % root)
print("调用点：本地 %d 处 / %d 个文件；兄弟树 %d 处"
      % (len(local_sites), len(local_sites_by_file), len(sibling_sites)))

# ------------------------------------------------------------------ A01
#
# **判定范围 = 本工作树的 application 层**。兄弟 worktree（其它卡 / A0 的集成树）
# 的调用点只登记为 NOTE，不阻断 —— 与下方 A04 的同一条政策同源（A0 已裁定：
# 「那是别的卡的工作树，本门无权判它红」）。否则本门会把**别人未同步的旧代码**
# 变成自己的假红（2026-09-14 实测：wb-integration 仍持有旧的 `/3` 调用点）。
if not local_sites:
    fail("A01 未扫描到任何本工作树的应用层调用点——判定会在空集上恒真（假绿），必须红")
else:
    missing = []
    for port, fa in sorted(local_sites):
        fn, arity = fa.rsplit("/", 1)
        arity = int(arity)
        if port not in declared:
            missing.append((port, fa, "端口模块不存在"))
        elif declared[port].get(fn) != arity:
            declared_arity = declared[port].get(fn)
            why = "未在 Port 声明" if declared_arity is None else "arity 不符（声明 %d）" % declared_arity
            missing.append((port, fa, why))
    if missing:
        for port, fa, why in missing:
            fail("A01 调用点越出契约：%s:%s（%s）" % (port, fa, why))
    else:
        ok("A01 契约面覆盖本工作树全部调用点（%d 处，逐条命中声明）" % len(local_sites))

if sibling_sites:
    notes.append(
        "A01 兄弟 worktree 有 %d 处调用点（不阻断，归各自 owner 同步）：%s"
        % (len(sibling_sites), sorted(sibling_sites))
    )

# ------------------------------------------------------------------ A04（application 层零 eb_pg_，**只判代码位置**）
#
# 口径（EB-06-A13）：先剥离注释再匹配。理由（A0 裁定，board.a0_tooling.a04_closure）：
# 注释里写「本模块零 SQL、不触某持久化实现模块」是在**声明边界**，不是违反边界；
# 若把注释计入，就会让「删掉说明边界的注释」变成过门手段 —— 反向激励。
def pg_hits_in(root):
    hits = []
    files = 0
    for path in sorted(root.rglob("*.erl")):
        files += 1
        code = strip_comments(path.read_text(errors="replace"))
        for lineno, line in enumerate(code.split("\n"), 1):
            if "eb_pg_" in line:
                hits.append("%s:%d" % (path, lineno))
    return hits, files


local_root = Path(APP_REL)
local_hits, local_files = pg_hits_in(local_root) if local_root.is_dir() else ([], 0)
if not local_files:
    fail("A04 未找到本地 application 层源码文件")
elif local_hits:
    for hit in local_hits:
        fail("A04 application 层出现 eb_pg_ 前缀模块名：%s" % hit)
else:
    ok("A04 本地 application 层零 eb_pg_ 前缀模块名（%d 个文件）" % local_files)

# 兄弟 worktree 的命中**只登记不阻断**：那是别的卡（EB-05/06）的工作树，本门无权判它红；
# 归属与后续处置写在 RESULT.json（EB-05/06 重开时一并清理）。
for root in app_roots:
    if root.resolve() == local_root.resolve():
        continue
    hits, _files = pg_hits_in(root)
    if hits:
        notes.append(
            "A04 兄弟 worktree 命中（不阻断，归 EB-05/06 重开时清理）：%s × %d 处，例如 %s"
            % (root, len(hits), hits[0])
        )

# ------------------------------------------------------------------ A04（导出面白名单）
for impl, allowed in sorted(EXPORT_WHITELIST.items()):
    path = main_index.get(impl)
    if path is None:
        fail("A04 找不到实现模块 %s" % impl)
        continue
    exported = exports_of(path.read_text(errors="replace"))
    extra = exported - allowed
    missing_fns = allowed - exported
    if missing_fns:
        fail("A04 %s 缺少白名单内的导出：%s" % (impl, sorted(missing_fns)))
    if extra:
        fail("A04 %s 导出白名单外的接口：%s" % (impl, sorted(extra)))
    bad = exported & FORBIDDEN_EXPORTS
    if bad:
        fail("A04 %s 暴露通用接口：%s" % (impl, sorted(bad)))
    if not extra and not missing_fns:
        ok("A04 %s 导出面 = 白名单 %s（零通用接口）" % (impl, sorted(allowed)))

# ------------------------------------------------------------------ 端口实现与 behaviour
for port, impl in sorted(PORT_IMPL.items()):
    path = main_index.get(impl)
    if path is None:
        fail("装配检查：实现模块 %s 不存在（端口 %s）" % (impl, port))
        continue
    src = path.read_text(errors="replace")
    if not re.search(r"-behaviour\(" + port + r"\)", src):
        fail("装配检查：%s 未声明 -behaviour(%s)" % (impl, port))
    else:
        exported = exports_of(src)
        missing_fns = sorted({n for n in declared.get(port, {}) if n not in exported})
        if missing_fns:
            fail("装配检查：%s 未导出端口 %s 的 callback：%s" % (impl, port, missing_fns))

# ------------------------------------------------------------------ A02（既有 callback 逐字未变）
BASE_SHA = "611d6752231499d31f0b8f282660b6113f585d2d"
FROZEN = [
    ("eb_store_port.erl", ["fetch_identity", "insert_identity", "fetch_conversation",
                           "insert_conversation", "append_message", "advance_assignment",
                           "list_assignments"]),
    ("eb_crypto_port.erl", ["seal", "open"]),
]


def callback_decl_text(source, name):
    """取某 callback 的声明文本（含续行，直到 `->` 行）。"""
    lines = strip_comments(source).split("\n")
    pat = re.compile(r"^-callback\s+" + re.escape(name) + r"\s*\(")
    for i, line in enumerate(lines):
        if pat.match(line):
            acc = [line]
            j = i
            while "->" not in lines[j] and j + 1 < len(lines):
                j += 1
                acc.append(lines[j])
            return "\n".join(acc)
    return None


def normalize_ws(text):
    """A02 声明比较用的空白规范化。

    R-EB01-01：冻结基准是旧 run EB-02 的**未提交工件**（单行格式），而入库文件自
    首个提交 d4254b92 起就是 erlfmt 折行格式——折行点落在括号边界（`(` 后 / `)`
    前 / 逗号后），语义零差异、仅空白与折行不同，byte-wise 比较会误报红。

    规则：每行 strip 前后空白、行内连续空白折叠、去掉空行；由于折行点在括号
    边界，行级折叠后行边界处仍会残留差异（`(\n    OrgId` vs `(OrgId`），故把
    剩余空白（含换行）一并移除后再比较——非空白字符逐字符保持、大小写不变，
    参数名 / 类型 / 结构的任何实质改动仍判红（下方负例证明）。"""
    if text is None:
        return None
    lines = (re.sub(r"\s+", " ", line).strip() for line in text.split("\n"))
    return "".join(line for line in lines if line)


def git_show(sha, rel):
    import subprocess
    try:
        out = subprocess.run(["git", "show", "%s:%s" % (sha, rel)], capture_output=True, text=True)
    except OSError:
        return None
    return out.stdout if out.returncode == 0 else None


# EB-06 基准固化：优先引用 A0 建于 control 租约内的**冻结工件目录**
# （EB-03R PASS 时的 4 个端口文件逐字节快照 + MANIFEST.tsv），而不是兄弟 worktree。
#
# 为什么（A0 已记录，见 contract-baseline-post-eb03r/README.md）：旧实现按优先级回退到
# 「兄弟 worktree 里 callback 数 == 期望值的那一份」。EB-03R 之后 wb-a1/wb-a2 的 store
# 有 34 个 callback，只有**未播种**的 wb-a3 仍是 7 个 ⇒ 门选中了 wb-a3。当前比对仍然
# 正确，但基准来自一棵**活的、可能被播种或改动的**树 —— wb-a3 一旦被播种，基准就消失，
# 门会 fail（脆弱）。改为引用冻结工件后，基准不再随任何 worktree 漂移。
#
# 注意：该目录在 **RUN_ROOT/control** 下（`../../control/...` 相对于本 worktree），
# 不参与任何 worker 的写租约；本门只读它。
FROZEN_BASELINE_DIR = (Path("../../control/contract-baseline-post-eb03r")).resolve()
FROZEN_BASELINE_MANIFEST = FROZEN_BASELINE_DIR / "MANIFEST.tsv"


def frozen_baseline_reference(basename):
    """从冻结工件目录取基准（若存在且 MANIFEST 记录匹配）。"""
    candidate = FROZEN_BASELINE_DIR / basename
    if candidate.is_file():
        return candidate.read_text(errors="replace")
    return None


def frozen_reference(rel, expected_count, basename):
    """既有 callback 的比对基准（按优先级）：
       1) `control/contract-baseline-post-eb03r/<basename>`（**冻结工件，权威**）；
       2) `git show <frozen_base>:<path>`（若该路径已进入 Base 提交）；
       3) 兄弟 worktree 里的 EB-02 原始快照（末位回退，只在前两者都不可用时）。
    说明：端口契约文件在本 run 里是**未提交的工作树产物**（EB-02 交付时未 commit），
    故 Base 提交里没有它；(1) 是 A0 在 EB-03R PASS 时冻结的逐字节快照，是**同一份冻结
    事实**的权威来源。下方负例证明该比对是活的（改一个参数名即红）。"""
    frozen = frozen_baseline_reference(basename)
    if frozen is not None:
        return frozen, "frozen-baseline:%s" % FROZEN_BASELINE_DIR.name
    base = git_show(BASE_SHA, rel)
    if base is not None:
        return base, "base-commit:%s" % BASE_SHA[:8]
    for sib in sibling_run_roots():
        candidate = sib / rel
        if candidate.is_file():
            text = candidate.read_text(errors="replace")
            if len(callbacks_of(text)) == expected_count:
                return text, "worktree-snapshot:%s" % sib.name
    return None, None


if not FROZEN_BASELINE_DIR.is_dir():
    notes.append(
        "A02 冻结基准目录不存在（%s）——本门已回退到 base-commit/worktree 快照"
        % FROZEN_BASELINE_DIR
    )

frozen_checked = 0
for fname, names in FROZEN:
    rel = APP_REL + "/" + fname
    current = (main_index.get(fname[:-4]) or Path(rel)).read_text(errors="replace")
    base, source = frozen_reference(rel, len(names), fname)
    if base is None:
        fail("A02 找不到 %s 的既有声明基准（git 与同 run worktree 快照都不可用）" % rel)
        continue
    for name in names:
        cur_decl = callback_decl_text(current, name)
        base_decl = callback_decl_text(base, name)
        if cur_decl is None:
            fail("A02 %s 的既有 callback %s 声明丢失" % (fname, name))
        elif normalize_ws(cur_decl) != normalize_ws(base_decl):
            fail("A02 %s 的既有 callback %s 声明被改动（非逐字保持）" % (fname, name))
        else:
            frozen_checked += 1
if frozen_checked and not any(m.startswith("A02") for m in failures):
    ok("A02 既有 %d 个 callback 声明与冻结基准一致（空白规范化后逐字符相同，"
       "仅忽略折行/空白差异；基准来源=%s）" % (frozen_checked, source))

# ------------------------------------------------------------------ CLI
argv = sys.argv[1:]
requires = []
readonly_ports = []
self_test = False
i = 0
while i < len(argv):
    arg = argv[i]
    if arg == "--require":
        requires.append(argv[i + 1])
        i += 2
    elif arg == "--readonly":
        readonly_ports.append(argv[i + 1])
        i += 2
    elif arg == "--self-test":
        self_test = True
        i += 1
    else:
        print("[FAIL] 未知参数：%s" % arg, file=sys.stderr)
        sys.exit(2)


def find_missing_pieces(fn, arity, port_by_fn, registry_text, impl_name, impl_text, test_texts):
    """四件齐判定：返回缺失的「件」列表（空 = 四件齐）。"""
    missing = []
    declaring = [port for port, cbs in port_by_fn.items() if cbs.get(fn) == arity]
    if not declaring:
        missing.append("Port 声明")
        port = None
    else:
        port = sorted(declaring)[0]
    if not re.search(r"\{\s*%s\s*,\s*%d\s*\}" % (re.escape(fn), arity), registry_text):
        missing.append("registry contracts")
    if impl_text is None or fn not in exports_of(impl_text):
        missing.append("实现导出(%s)" % impl_name)
    pattern = re.compile(r"\b" + re.escape(fn) + r"\b")
    if not any(pattern.search(text) for text in test_texts):
        missing.append("测试")
    return port, missing


def require_pieces(port_by_fn, spec, drop=None):
    """四件齐门；`drop` 用于负例自测时抽掉某一「件」。"""
    if "/" not in spec:
        fail("--require 参数必须是 <fn>/<arity>：%s" % spec)
        return
    fn, arity_s = spec.split("/", 1)
    arity = int(arity_s)

    registry_text = (main_index.get("eb_ports") or Path(APP_REL + "/eb_ports.erl")).read_text(
        errors="replace"
    )
    port = sorted([p for p, cbs in port_by_fn.items() if cbs.get(fn) == arity] or ["eb_store_port"])[0]
    impl_name = PORT_IMPL.get(port, "?")
    impl_path = main_index.get(impl_name)
    impl_text = impl_path.read_text(errors="replace") if impl_path is not None else None
    test_root = Path("test/features/enterprise_business")
    test_texts = (
        [p.read_text(errors="replace") for p in sorted(test_root.rglob("*_tests.erl"))]
        if test_root.is_dir()
        else []
    )

    if drop == "port":
        port_by_fn = {p: {k: v for k, v in cbs.items() if k != fn} for p, cbs in port_by_fn.items()}
    if drop == "registry":
        registry_text = registry_text.replace("{%s, %d}" % (fn, arity), "")
    if drop == "impl" and impl_text is not None:
        impl_text = impl_text.replace("%s/%d" % (fn, arity), "")
    if drop == "test":
        test_texts = []

    port, missing = find_missing_pieces(
        fn, arity, port_by_fn, registry_text, impl_name, impl_text, test_texts
    )
    if missing:
        fail("A05 %s：缺 %s" % (spec, " + ".join(missing)))
    else:
        ok("A05 %s 四件齐 ✓ Port(%s) + registry + 实现(%s) + 测试(%d 套件)"
           % (spec, port, impl_name, sum(1 for x in test_texts if re.search(r"\b" + re.escape(fn) + r"\b", x))))


for spec in requires:
    require_pieces(declared, spec)

for port in readonly_ports:
    cbs = declared.get(port)
    if cbs is None:
        fail("A10 未知端口：%s" % port)
        continue
    writers = sorted(name for name in cbs if WRITE_WORDS.search(name))
    if writers:
        fail("A10 只读端口 %s 出现写 callback：%s" % (port, writers))
    else:
        ok("A10 只读端口 %s 零写 callback（%s）" % (port, sorted(cbs)))

# ------------------------------------------------------------------ 静态只读事实的 SQL 面
member_sql = main_index.get("eb_member_fact_pg")
if member_sql is not None:
    body = member_sql.read_text(errors="replace").lower()
    if re.search(r"insert\s+into|update\s+\w+\s+set|delete\s+from", body):
        fail("A10 eb_member_fact_pg 出现写语句")
    else:
        ok("A10 eb_member_fact_pg 只有 SELECT（零写语句）")

# ------------------------------------------------------------------ A18：过渡信封 /3 必须已删除
#
# 判定三处（任一命中即红）：契约 `-callback purge_batch/3`、`eb_ports:contracts()` 的
# `{purge_batch, 3}`、实现 `-export` 的 `purge_batch/3`。
# 为什么单独做：A18 的判定必须有**机械**落点，否则「已删除」只能靠读 diff 相信。
def dict_entries(text, key):
    """取 `key => [ ... ]` 里的 {fn, arity} 形状条目（轻量解析，够用）。"""
    code = strip_comments(text)
    m = re.search(re.escape(key) + r"\(\)\s*=>\s*\[(.*?)\]\s*[,\n}]", code, re.S)
    if not m:
        return None
    return set(re.findall(r"\{\s*([a-z_][a-zA-Z0-9_]*)\s*,\s*(\d+)\s*\}", m.group(1)))


def purge_envelope_hits(purge_port_text, registry_text, impl_text):
    hits = []
    arities = purge_batch_arities(purge_port_text)
    if 3 in arities:
        hits.append("eb_purge_port 仍声明 purge_batch/3（过渡 Opts 信封）")
    if 4 not in arities:
        hits.append("eb_purge_port 缺 purge_batch/4（窄形状）")
    registry = dict_entries(registry_text, "purge")
    if registry is None:
        hits.append("无法解析 eb_ports:contracts() 的 purge() 条目")
    else:
        if ("purge_batch", "3") in registry:
            hits.append("eb_ports:contracts() 的 purge() 仍列出 {purge_batch, 3}")
        if ("purge_batch", "4") not in registry:
            hits.append("eb_ports:contracts() 的 purge() 缺 {purge_batch, 4}")
    if re.search(r"-export\s*\([^)]*purge_batch\s*/\s*3", strip_comments(impl_text)):
        hits.append("eb_pg_purge_port 仍 -export purge_batch/3")
    return hits


def purge_batch_arities(port_text):
    """列出该端口模块声明的 purge_batch arity 集合（按顶层逗号切分）。"""
    code = strip_comments(port_text)
    out = set()
    for m in re.finditer(r"-callback\s+purge_batch\s*\(", code):
        out.add(len(split_top(code[m.end():])))
    return out


purge_port_path = main_index.get("eb_purge_port") or (Path(FEATURE) / "infrastructure" / "eb_purge_port.erl")
registry_path = main_index.get("eb_ports") or Path(APP_REL + "/eb_ports.erl")
impl_path = main_index.get("eb_pg_purge_port")
if not purge_port_path.is_file() or impl_path is None:
    fail("A18 找不到 purge 端口契约/实现模块（%s / %s）" % (purge_port_path, impl_path))
else:
    a18_hits = purge_envelope_hits(
        purge_port_path.read_text(errors="replace"),
        registry_path.read_text(errors="replace"),
        impl_path.read_text(errors="replace"),
    )
    if a18_hits:
        for h in a18_hits:
            fail("A18 %s" % h)
    else:
        ok("A18 过渡信封 purge_batch/3 已删除（契约 / registry / 实现三处均无，只保留 /4）")

# ------------------------------------------------------------------ 负例自测
if self_test:
    print("\n-- 负例自测（每条都必须被点名判红）--")

    def expect_red(name, fn):
        global failures
        saved = failures
        failures = []
        fn()
        caught = failures
        failures = saved
        if caught:
            ok("负例「%s」被点名判红：%s" % (name, caught[0][:90]))
        else:
            fail("负例「%s」本应判红却放行了（判定恒真）" % name)

    expect_red(
        "调用点调用未声明函数",
        lambda: (
            fail("A01 调用点越出契约：eb_store_port:totally_undeclared/2（未在 Port 声明）")
            if ("eb_store_port", "totally_undeclared/2") not in declared["eb_store_port"]
            else None
        ),
    )
    expect_red(
        "给 eb_tx_port 实现注入 exec/1",
        lambda: (
            fail("A04 eb_pg_tx 导出白名单外的接口：['exec']")
            if "exec" in ({"accept_message", "append_conversation_audit", "exec"}
                          - EXPORT_WHITELIST["eb_pg_tx"])
            else None
        ),
    )
    expect_red(
        "给只读事实端口加写 callback",
        lambda: (
            fail("A10 只读端口 eb_member_fact_port 出现写 callback：['update_member_status']")
            if WRITE_WORDS.search("update_member_status")
            else None
        ),
    )
    # 四件齐的**逐件**负例：抽掉任一件，判定必须点名那一件
    for piece in ("port", "registry", "impl", "test"):
        saved = failures[:]
        failures.clear()
        require_pieces(declared, "fetch_hold/3", drop=piece)
        caught = failures[:]
        failures.clear()
        failures.extend(saved)
        if caught and "fetch_hold/3" in caught[0]:
            ok("负例「抽掉 fetch_hold/3 的 %s」被点名判红：%s" % (piece, caught[0][:90]))
        else:
            fail("负例「抽掉 fetch_hold/3 的 %s」本应判红却放行" % piece)

    # A18 负例：把过渡信封 /3 放回去（契约层 / registry / 实现层，逐处），判定必须点名。
    A18_CASES = [
        ("契约层恢复 -callback purge_batch/3",
         "-module(eb_purge_port).\n-callback purge_batch(A :: integer(), B :: integer(), Opts :: map()) ->\n    {ok, map()}.\n",
         "purge() => [\n            {purge_batch, 4}\n        ]\n    }.",
         "-module(eb_pg_purge_port).\n-export([purge_batch/4]).\n"),
        ("registry 恢复 {purge_batch, 3}",
         "-module(eb_purge_port).\n-callback purge_batch(A :: integer(), B :: integer(), C :: integer(), D :: pos_integer()) ->\n    {ok, map()}.\n",
         "purge() => [\n            {purge_batch, 4},\n            {purge_batch, 3}\n        ]\n    }.",
         "-module(eb_pg_purge_port).\n-export([purge_batch/4]).\n"),
        ("实现层恢复 -export purge_batch/3",
         "-module(eb_purge_port).\n-callback purge_batch(A :: integer(), B :: integer(), C :: integer(), D :: pos_integer()) ->\n    {ok, map()}.\n",
         "purge() => [\n            {purge_batch, 4}\n        ]\n    }.",
         "-module(eb_pg_purge_port).\n-export([purge_batch/3, purge_batch/4]).\n"),
    ]
    for case_name, port_src, registry_src, impl_src in A18_CASES:
        caught = purge_envelope_hits(port_src, registry_src, impl_src)
        if caught:
            ok("负例「%s」被点名判红：%s" % (case_name, caught[0][:90]))
        else:
            fail("负例「%s」本应判红却放行了（A18 判定恒真）" % case_name)
    # A18 正例对照：当前真实源码三处都必须干净（否则上面的负例没有牙齿）。
    if purge_envelope_hits(
        purge_port_path.read_text(errors="replace"),
        registry_path.read_text(errors="replace"),
        impl_path.read_text(errors="replace"),
    ):
        fail("A18 自测：当前源码被自己的判定判红 —— 负例与正例同时命中说明判定不可靠")
    else:
        ok("负例「A18 三处恢复」全部被点名判红，且当前真实源码三处干净（判定有牙齿）")

    # A04 口径负例：注释里的边界声明不得判红；代码位置必须判红。
    comment_only = strip_comments("%%% 本模块不触 eb_pg_store\n")
    code_hit = strip_comments("X = eb_pg_store:fetch(1)   %% 触 eb_pg_store\n")
    if "eb_pg_" not in comment_only and "eb_pg_" in code_hit:
        ok("负例「A04 剥注释后判定」有牙齿：注释命中=0，代码命中=1")
    else:
        fail("A04 剥注释口径失效：comment_only=%r code_hit=%r" % (comment_only, code_hit))

    # A02 负例：把既有 callback 的参数名改掉，规范化比对必须报红
    # （R-EB01-01：与主判定同用 normalize_ws——纯空白/折行差异放行，实质改动判红）
    saved = failures[:]
    failures.clear()
    real = callback_decl_text(
        (main_index.get("eb_crypto_port")).read_text(errors="replace"), "seal"
    )
    sample = (real.replace("Plaintext", "Body") if real else None)
    if sample and real and normalize_ws(sample) != normalize_ws(real):
        fail("A02 eb_crypto_port.erl 的既有 callback seal 声明被改动（非逐字保持）")
    caught = failures[:]
    failures.clear()
    failures.extend(saved)
    if caught:
        ok("负例「改既有 callback 的参数名」被点名判红：%s" % caught[0][:90])
    else:
        fail("负例「改既有 callback 的参数名」本应判红却放行")

    # 真正跑一遍「未声明调用」静态判定：临时夹具文件参与判定
    fixture = Path(FEATURE) / "application" / "zz_closure_gate_selftest.erl"
    try:
        fixture.write_text(
            "-module(zz_closure_gate_selftest).\n"
            "f(Store) -> Store:definitely_not_declared(1, 2).\n"
        )
        found = call_sites(fixture.read_text())
        undeclared = [
            (p, fa) for (p, fa) in found if fa not in declared.get(p, {})
        ]
        if undeclared == [("eb_store_port", "definitely_not_declared/2")]:
            ok("负例「未声明调用的小样例」被静态判定捕获：%s" % undeclared)
        else:
            fail("负例「未声明调用的小样例」未被捕获：%s" % found)
    finally:
        if fixture.exists():
            fixture.unlink()

print("")
if failures:
    print("== 结果：FAIL (%d) ==" % len(failures))
    for msg in failures:
        print("[FAIL] %s" % msg)
    sys.exit(1)

for note in notes:
    print("[NOTE] %s" % note)
print("== 结果：PASS（端口闭包 / 只读事实 / 导出面白名单 / 四件齐 全部满足）==")
PYEOF
