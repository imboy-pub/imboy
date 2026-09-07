#!/usr/bin/env python3
"""校验第三方与数据流清单（Overseas Compliance Plan Task V-01）。

与 validate_retention_policy.py 同思路：清单不能只是一份愿望文档，
必须与实际构建/运行时互证。四道门：

  1. schema     —— active 行字段齐备、枚举值合法；no_data 行必须给理由
  2. 覆盖       —— REQUIRED_CATEGORIES 每类至少一行（active/no_data/self-hosted 均算）
  3. 实证双向   —— EVIDENCE 特征表与源码/配置互证：
                   (a) 源特征必须映射到清单行（防 provider 私接不入册）
                   (b) active 行 endpoint host 必须在源文本中命中（防清单漂移/编造）
  4. 海外发布门 --overseas-gate：任何真实出站行（非 dev-only、非 self-hosted）
                   若 DPA/SCC 未签署或 region=unknown 则报错退出——
                   unknown DPA/region blocks operated overseas release。

用法：
  python3 scripts/validate_third_party_inventory.py                 # 常规校验
  python3 scripts/validate_third_party_inventory.py --overseas-gate # 海外发布门
测试直调 validate(...，全部显式传参，不依赖全局常量）。
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

DEFAULT_INVENTORY = Path("docs/compliance/third-party-data-inventory.yml")
DEFAULT_ROOT = Path(".")

# 计划点名的数据流类别：每类至少一行（含 no_data 行——显式理由即披露）
REQUIRED_CATEGORIES = frozenset(
    {
        "sms",
        "auth",
        "map",
        "location",
        "crash",
        "payment",
        "email",
        "llm",
        "storage",
        "rtc",
        "turn",
        "push",
        "cdn",
        "webhooks",
        "analytics",
    }
)

ACTIVE_REQUIRED_FIELDS = (
    "provider",
    "category",
    "version",
    "purpose",
    "data_fields",
    "endpoints",
    "region",
    "necessity",
    "consent",
    "dpa_status",
    "retention",
    "deletion_api",
    "build_flavors",
    "evidence",
)

ALLOWED_REGION = frozenset({"cn", "global", "self-hosted", "unknown"})
ALLOWED_NECESSITY = frozenset({"necessary", "optional", "dev-only"})
ALLOWED_DPA = frozenset({"executed", "pending-owner", "unknown", "not-applicable"})

# 实证特征表：(host_marker, 相对 root 路径, 命中正则, optional)
# optional=True 的条目在 CI 等只检出本仓的环境下降级为警告（跨仓/app 仓文件）；
# marker 映射到清单行的判定见 evidence_row_text()。
EVIDENCE = (
    ("api.sms.jpush.cn", "src/lib/imboy_sms.erl", r"api\.sms\.jpush\.cn", False),
    ("api.verification.jpush.cn", "src/lib/imboy_sms.erl", r"api\.verification\.jpush\.cn", False),
    ("jverification:3.2.8", "../imboyapp/android/app/build.gradle.kts", r"jverification:3\.2\.8", True),
    ("restapi.amap.com", "../imboyapp/lib/component/location/amap_helper.dart", r"restapi\.amap\.com", True),
    ("com.amap.api:location", "../imboyapp/android/app/build.gradle.kts", r"com\.amap\.api:location", True),
    ("com.amap.api:3dmap", "../imboyapp/android/app/build.gradle.kts", r"com\.amap\.api:3dmap", True),
    ("sentry_flutter 9.29.0", "../imboyapp/pubspec.yaml", r"sentry_flutter:\s*9\.29\.0", True),
    ("o436562.ingest.sentry.io", "../imboyapp/.env", r"ingest\.sentry\.io", True),
    ("openapi.alipay.com", "../erlang_pay/src", r"openapi\.alipay\.com", True),
    ("api.mch.weixin.qq.com", "../erlang_pay/src", r"api\.mch\.weixin\.qq\.com", True),
    ("api.stripe.com", "../erlang_pay/src", r"api\.stripe\.com", True),
    ("smtp.qq.com", "config/sys.pro.config", r"smtp\.qq\.com", False),
    ("ark.cn-beijing.volces.com", "config/sys.local.config", r"ark\.cn-beijing\.volces\.com", False),
    ("maas.aliyuncs.com", "config/sys.pro.config", r"maas\.aliyuncs\.com", False),
    ("openapi-sandbox.dl.alipaydev.com", "config/sys.local.config", r"openapi-sandbox\.dl\.alipaydev\.com", False),
    ("s3.imboy.pub", "config/sys.pro.config", r"s3\.imboy\.pub", False),
    ("pro.imboy.pub/livekit", "config/sys.pro.config", r"pro\.imboy\.pub/livekit", False),
    ("turn.imboy.pub", "config/sys.pro.config", r"turn\.imboy\.pub", False),
)

HOST_RE = re.compile(r"[a-z0-9][a-z0-9.-]*\.(?:com|cn|io|net|org|pub|dev)")


def parse_inventory(path: Path) -> tuple[dict, dict]:
    """解析受限 YAML 子集：meta 段 + providers 段（两空格块名/四空格字段）。

    抛 ValueError 即结构非法（含「值中出现冒号+空格」）。
    """
    meta: dict[str, str] = {}
    providers: dict[str, dict[str, str]] = {}
    section = None
    current = None
    with path.open(encoding="utf-8") as fh:
        for lineno, raw in enumerate(fh, 1):
            line = raw.rstrip("\n")
            if not line.strip() or line.strip().startswith("#"):
                continue
            if not line.startswith(" "):
                key = line.rstrip(":")
                if key not in ("meta", "providers"):
                    raise ValueError(f"line {lineno}: unknown top-level section {key!r}")
                section = key
                current = None
                continue
            indent = len(line) - len(line.lstrip(" "))
            body = line.strip()
            if indent == 2 and body.endswith(":") and ":" not in body[:-1]:
                if section != "providers":
                    raise ValueError(f"line {lineno}: provider block outside providers section")
                current = body[:-1]
                if current in providers:
                    raise ValueError(f"line {lineno}: duplicate provider {current!r}")
                providers[current] = {}
                continue
            if ":" not in body:
                raise ValueError(f"line {lineno}: expected 'key: value', got {body!r}")
            key, _, value = body.partition(":")
            if value.startswith(" ") is False and value != "":
                raise ValueError(f"line {lineno}: expected space after colon")
            value = value.strip()
            if ": " in value:
                raise ValueError(
                    f"line {lineno}: field value must not contain ': ' (restricted YAML subset)"
                )
            target = meta if section == "meta" else providers.get(current, None)
            if target is None:
                raise ValueError(f"line {lineno}: field outside any provider block")
            target[key.strip()] = value
    return meta, providers


def is_active_row(row: dict) -> bool:
    return "no_data" not in row


def evidence_row_text(name: str, row: dict) -> str:
    """特征 marker 与清单行的映射判定：块名+全部字段值拼接后做子串匹配。"""
    parts = [name] + [f"{k}={v}" for k, v in row.items()]
    return " | ".join(parts)


def load_evidence_texts(root: Path, evidence) -> list[dict]:
    """读取每条特征的源文件文本；optional 文件缺失降级为警告。"""
    loaded = []
    for marker, rel_path, pattern, optional in evidence:
        path = root / rel_path
        entry = {"marker": marker, "path": path, "pattern": re.compile(pattern), "optional": optional}
        if path.is_dir():
            entry["text"] = "\n".join(
                p.read_text(encoding="utf-8", errors="replace") for p in sorted(path.glob("*.erl"))
            )
        else:
            entry["text"] = path.read_text(encoding="utf-8", errors="replace") if path.exists() else None
        loaded.append(entry)
    return loaded


def validate(
    inventory_path: Path,
    root: Path = DEFAULT_ROOT,
    evidence=EVIDENCE,
    required_categories: frozenset = REQUIRED_CATEGORIES,
    overseas_gate: bool = False,
) -> tuple[bool, list[str], list[str]]:
    """四道门校验。返回 (ok, errors, warnings)。全部参数显式传入（可测试性）。"""
    errors: list[str] = []
    warnings: list[str] = []

    if not inventory_path.exists():
        return False, [f"inventory not found: {inventory_path}"], []

    try:
        meta, providers = parse_inventory(inventory_path)
    except ValueError as exc:
        return False, [f"parse error: {exc}"], []
    if int(meta.get("schema_version", "0")) != 1:
        errors.append("meta.schema_version must be 1")
    if not providers:
        errors.append("no provider rows")

    categories = set()
    for name, row in providers.items():
        if is_active_row(row):
            for field in ACTIVE_REQUIRED_FIELDS:
                if not row.get(field):
                    errors.append(f"{name}: active row missing field '{field}'")
            if row.get("region") not in ALLOWED_REGION:
                errors.append(f"{name}: region must be one of {sorted(ALLOWED_REGION)}")
            if row.get("necessity") not in ALLOWED_NECESSITY:
                errors.append(f"{name}: necessity must be one of {sorted(ALLOWED_NECESSITY)}")
            if row.get("dpa_status") not in ALLOWED_DPA:
                errors.append(f"{name}: dpa_status must be one of {sorted(ALLOWED_DPA)}")
            privacy = row.get("privacy_url", "")
            if privacy != "unknown" and not privacy.startswith("https://"):
                errors.append(f"{name}: privacy_url must be https URL or 'unknown'")
        else:
            if not row.get("no_data") or not row.get("provider") or not row.get("category"):
                errors.append(f"{name}: no_data row requires provider/category/no_data rationale")
            if row.get("necessity", "optional") not in ALLOWED_NECESSITY:
                errors.append(f"{name}: necessity must be one of {sorted(ALLOWED_NECESSITY)}")
        cat = row.get("category")
        if cat:
            categories.add(cat)

    missing = required_categories - categories
    if missing:
        errors.append(f"missing category rows: {sorted(missing)}")

    # 门 3：实证双向
    loaded = load_evidence_texts(root, evidence)
    for entry in loaded:
        marker = entry["marker"]
        if entry["text"] is None:
            msg = f"evidence source missing: {marker} ({entry['path']})"
            (warnings if entry["optional"] else errors).append(msg)
            continue
        if not entry["pattern"].search(entry["text"]):
            # 文件在但特征消失：清单的 evidence 声明已失效（无论是否还有行映射，
            # 源头变了就必须人工复核清单）——fail loud 而非静默放行
            errors.append(
                f"evidence feature {marker!r} no longer present in {entry['path']} — "
                "inventory evidence stale, re-verify the provider row"
            )
            continue
        mapped = any(marker in evidence_row_text(n, r) for n, r in providers.items())
        if not mapped:
            errors.append(
                f"source feature {marker!r} is not mapped to any inventory row (unregistered data flow?)"
            )

    all_source_text = "\n".join(e["text"] for e in loaded if e["text"])
    for name, row in providers.items():
        if not is_active_row(row):
            continue
        for host in HOST_RE.findall(row.get("endpoints", "")):
            if not all_source_text:
                warnings.append(f"{name}: no evidence sources readable; endpoint {host} unverified")
                continue
            if host not in all_source_text:
                errors.append(
                    f"{name}: endpoint host {host!r} not found in any evidence source "
                    "(stale or fabricated inventory row?)"
                )

    # 门 4：海外发布门
    blocked = []
    if overseas_gate:
        for name, row in providers.items():
            if not is_active_row(row):
                continue
            if row.get("region") == "self-hosted":
                continue
            if row.get("necessity") == "dev-only":
                continue
            if row.get("dpa_status") != "executed" or row.get("region") == "unknown":
                blocked.append(f"{name} (region={row.get('region')}, dpa={row.get('dpa_status')})")
        if blocked:
            errors.append(
                "overseas release BLOCKED — DPA/SCC not executed for: " + "; ".join(blocked)
            )

    return (not errors), errors, warnings


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--inventory", type=Path, default=DEFAULT_INVENTORY)
    parser.add_argument("--root", type=Path, default=DEFAULT_ROOT)
    parser.add_argument(
        "--overseas-gate",
        action="store_true",
        help="block when any real outbound row lacks executed DPA/SCC (overseas release check)",
    )
    args = parser.parse_args(argv)

    ok, errors, warnings = validate(args.inventory, root=args.root, overseas_gate=args.overseas_gate)
    for w in warnings:
        print(f"WARN: {w}")
    for e in errors:
        print(f"ERROR: {e}")
    print(("PASS" if ok else "FAIL") + f": {args.inventory}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
