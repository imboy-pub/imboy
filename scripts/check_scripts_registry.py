#!/usr/bin/env python3
"""S2-07 护栏 / Scripts registry guardrail.

以 scripts/INVENTORY.yml 为登记源，校验：
  1. 每个 git 跟踪的 scripts/ 资产都已登记，登记簿条目在磁盘上存在；
  2. scripts/archive/ 归档工具不被 Makefile / include/*.mk / workflows / deploy
     安装链 / Compose / Helm / cron / Dockerfile 等默认流程引用，也不被
     非归档脚本调用（归档退出默认路径）；
  3. docs/、deploy/、根级 *.md 中 scripts/... 文件引用指向真实存在的文件
     （抓失效文档引用）。

用法：python3 scripts/check_scripts_registry.py
退出码：0 通过；1 有 error。warning 不影响退出码。
"""
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INVENTORY = ROOT / "scripts" / "INVENTORY.yml"

REF_RX = re.compile(
    r"(?<![A-Za-z0-9_.-]/)"  # 排除 imboyapp/scripts、test/scripts、/opt/imboy/scripts 等他仓/服务器路径
    r"(?:scripts/[A-Za-z0-9_./-]+\.(?:sh|py|erl|escript|sql|mjs|dart)|scripts/imboy_ctl)"
)

# 历史档案：交付计划/历史计划/冻结证据不重写，豁免引用存在性检查
HISTORY_RX = re.compile(
    r"^docs/design/|^docs/plans/|^docs/architecture/.*/evidence/|/materials-index\.json$"
    r"|^docs/engineering/.*(audit|gap-analysis)"
)

# 已核实为"他仓工作目录内的正确相对命令"或"文档内联定义脚本"的引用：
# (file, ref) → 原因。新增条目须附原因，定期复查是否仍成立。
EXPECTED_REL = {
    # CONTRIBUTING 的命令在 imboyapp 仓内执行（并排双仓工作目录上下文）
    ("CONTRIBUTING.md", "scripts/generate_error_code.dart"):
        "imboyapp 仓内命令，文档上下文为并排双仓工作目录",
    # hird POC 报告的历史步骤记录：旧目录名是有意保留的叙述，同句已附归档路径
    ("docs/architecture/hird-poc/IMBOY_HIRD_AUTH_REPLAY_POC_V1.md", "scripts/hird_poc_v1"):
        "历史步骤记录保留旧目录名，同句已注明 2026-11-27 归档至 scripts/archive/hird-poc-v1/",
}


def tracked(prefix: str):
    out = subprocess.run(["git", "-C", str(ROOT), "ls-files", "-z", prefix],
                         capture_output=True, check=True).stdout
    return [p.decode("utf-8", "replace") for p in out.split(b"\0") if p]


def parse_inventory() -> dict:
    """解析 INVENTORY.yml 的严格子集（- path: X + 两空格缩进字段）。"""
    entries = {}
    cur = None
    for line in INVENTORY.read_text(encoding="utf-8").splitlines():
        line = line.rstrip()
        if line.startswith("- path: "):
            cur = line[len("- path: "):].strip().strip('"')
            entries[cur] = {}
        elif cur is not None and line.startswith("  ") and ":" in line:
            key, _, val = line.strip().partition(": ")
            entries[cur][key] = val.strip().strip('"')
    return entries


def glob_tracked(pattern: str):
    out = subprocess.run(["git", "-C", str(ROOT), "ls-files", "-z", "--", pattern],
                         capture_output=True, check=True).stdout
    return [p.decode("utf-8", "replace") for p in out.split(b"\0") if p]


def main() -> int:
    errors = []
    warnings = []

    if not INVENTORY.exists():
        print(f"FAIL: {INVENTORY.relative_to(ROOT)} 不存在")
        return 1
    inv = parse_inventory()

    # ── 1) 登记完整性 ────────────────────────────────────────────
    for f in tracked("scripts/"):
        if f == "scripts/INVENTORY.yml":
            continue
        if f not in inv:
            errors.append(f"未登记脚本资产：{f}（登记到 scripts/INVENTORY.yml，含 category/disposition）")
    for p in inv:
        if not (ROOT / p).exists():
            errors.append(f"登记簿条目在磁盘上不存在：{p}（归档/删除后须同步登记簿）")

    # ── 2) 归档退出默认流程 ──────────────────────────────────────
    flow_files = set()
    for pat in ("Makefile", "include/*.mk", ".github/workflows/*.yml",
                ".github/workflows/*.yaml", "deploy/install.sh", "deploy/preflight.sh",
                "deploy/cron/*", "deploy/helm/templates/*.yaml",
                "Dockerfile*", "docker/*_Dockerfile", "docker-compose*.yml"):
        flow_files.update(glob_tracked(pat))
    for f in sorted(flow_files):
        try:
            text = (ROOT / f).read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        if "scripts/archive/" in text:
            errors.append(f"默认流程引用归档工具：{f} 出现 scripts/archive/")
    for f in tracked("scripts/"):
        if f.startswith("scripts/archive/") or f == "scripts/INVENTORY.yml":
            continue
        if f in ("scripts/check_scripts_registry.py", "scripts/test/test_scripts_registry.py"):
            continue  # 本文件与自测的说明/断言文本可提及 archive 路径
        p = ROOT / f
        if p.suffix in (".sh", ".py", ".escript", ".erl") or f.endswith("imboy_ctl"):
            try:
                text = p.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue
            if "scripts/archive/" in text:
                errors.append(f"非归档脚本引用归档工具：{f} 出现 scripts/archive/")

    # ── 3) 文档/部署引用存在性 ───────────────────────────────────
    scan_files = set(tracked("docs/")) | set(tracked("deploy/"))
    scan_files.update(glob_tracked("*.md"))
    scan_files.update(glob_tracked("scripts/*.md"))
    bare_rx = re.compile(r"(?<![A-Za-z0-9_.-]/)scripts/[A-Za-z0-9_*./-]+")
    script_suffixes = (".sh", ".py", ".escript", ".erl", ".sql", ".mjs", ".dart", ".bash")
    for f in sorted(scan_files):
        if HISTORY_RX.search(f):
            continue  # 历史档案不重写，引用保持归档时点原貌
        p = ROOT / f
        try:
            text = p.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        for m in REF_RX.findall(text):
            ref = m.rstrip(".")
            if (f, ref) in EXPECTED_REL:
                continue
            if not (ROOT / ref).exists():
                errors.append(f"失效脚本引用：{f} → {ref}")
        # 3b) 无扩展名形态 scripts/xxx：须能解析为存在路径或已知后缀脚本
        for raw in bare_rx.findall(text):
            ref = raw.rstrip(".,;:）)]、）/")
            if not ref or "*" in ref or ref.endswith(("_", "-")):
                continue  # glob 前缀（scripts/cache_*）与散文列举不判
            if ref.count("/") > 1:
                continue  # 顶层单名以外（连写目录列举/子目录树）不判，带扩展名由 3a 管
            if ref.rsplit("/", 1)[-1].startswith("."):
                continue  # dotfile（如 scripts/.env.deploy 运行时实例）非脚本，不判
            if (f, ref) in EXPECTED_REL:
                continue
            cand = ROOT / ref
            if cand.exists():
                continue
            if any((ROOT / (ref + suf)).exists() for suf in script_suffixes):
                continue  # 解析到已知后缀脚本（如 erl_crashdump_analyzer → .sh）
            errors.append(f"无法解析的无扩展名脚本引用：{f} → {ref}")

    # 汇总
    for w in warnings:
        print(f"WARN: {w}")
    if errors:
        for e in errors:
            print(f"FAIL: {e}")
        print(f"\ncheck_scripts_registry: {len(errors)} error(s)")
        return 1
    print(f"check_scripts_registry: OK（登记 {len(inv)} 项，流程文件 {len(flow_files)} 个，"
          f"文档引用扫描 {len(scan_files)} 个文件）")
    return 0


if __name__ == "__main__":
    sys.exit(main())
