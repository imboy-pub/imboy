#!/usr/bin/env python3
"""T-01 保留策略注册表校验 / Retention policy registry validator.

校验 docs/compliance/retention-policy.yml：
1. schema：必填字段、duration 形态、action/status 枚举；
2. 覆盖：验收要求的九大数据类必须全部登记；
3. 红线：禁止 per-country 分支（不得出现 country 字段）；
   legal_hold 必须显式 unsupported（未经 counsel 设计不得开启）；
4. 实证一致性：注册表里 evidence-backed 条目的 duration 必须与
   deploy/loki/loki.yml、Prometheus 启动参数、backup_pg.sh 的实际配置一致。

用法:
    python3 scripts/validate_retention_policy.py [--root PATH]
退出码 0=PASS，1=FAIL。
"""
import argparse
import re
import sys
from pathlib import Path

try:
    import yaml
except ImportError:  # pragma: no cover
    print("PyYAML is required: pip install pyyaml", file=sys.stderr)
    sys.exit(1)

ROOT = Path(__file__).resolve().parents[1]
POLICY = ROOT / "docs/compliance/retention-policy.yml"

LOKI_YML = ROOT / "deploy/loki/loki.yml"
PROMETHEUS_COMPOSE = ROOT / "deploy/docker-compose.community.yml"
BACKUP_PG_SH = ROOT / "scripts/backup_pg.sh"

REQUIRED_CLASSES = {
    "messages_e2ee",
    "messages_channel",
    "moments",
    "attachments",
    "logs_observability",
    "metrics_prometheus",
    "audit_security_moderation",
    "sessions_tokens",
    "payments_billing",
    "backups_pg",
    "backups_imboy_db",
    "vendors",
}

REQUIRED_FIELDS = ["id", "description", "storage", "duration", "trigger", "action", "status", "owner"]
DURATION_RE = re.compile(r"^\d+\s*(h|d|w|mo|y)$")
DURATIONS = {"indefinite", "event-driven", "per-vendor", "10-copies"}
ACTIONS = {"delete", "anonymize", "archive", "retain"}
STATUSES = {"evidence-backed", "pending-owner", "legal-review"}


class PolicyError(ValueError):
    pass


def load_policy(path: Path):
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise PolicyError("policy root must be a mapping")
    return data


def validate_schema(policy: dict) -> list:
    errors = []
    if policy.get("legal_hold") != "unsupported":
        errors.append("legal_hold 必须显式为 unsupported（counsel 设计落地前不支持）")

    classes = policy.get("classes")
    if not isinstance(classes, list) or not classes:
        raise PolicyError("classes 必须为非空列表")

    seen = set()
    for idx, cls in enumerate(classes):
        cid = cls.get("id", f"<index {idx}>")
        for field in REQUIRED_FIELDS:
            if not cls.get(field):
                errors.append(f"{cid}: 缺少必填字段 {field}")
        if cls.get("country") is not None or "country" in cls:
            errors.append(f"{cid}: 禁止 per-country 分支（用 profiles 承载口径）")
        duration = str(cls.get("duration", ""))
        if duration not in DURATIONS and not DURATION_RE.match(duration):
            errors.append(f"{cid}: duration 形态非法: {duration!r}")
        if cls.get("action") not in ACTIONS:
            errors.append(f"{cid}: action 非法: {cls.get('action')!r}")
        if cls.get("status") not in STATUSES:
            errors.append(f"{cid}: status 非法: {cls.get('status')!r}")
        seen.add(cid)

    missing = REQUIRED_CLASSES - seen
    if missing:
        errors.append(f"覆盖缺失：必备数据类未登记: {sorted(missing)}")
    return errors


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace") if path.exists() else ""


def validate_consistency(policy: dict, root: Path = ROOT) -> list:
    """evidence-backed 条目与 deploy/脚本实际配置比对（天数；10-copies 单独比）。"""
    errors = []
    by_id = {c.get("id"): c for c in policy.get("classes", [])}

    loki_text = _read(root / "deploy/loki/loki.yml")
    m = re.search(r"retention_period:\s*(\d+)h", loki_text)
    if not m:
        errors.append("deploy/loki/loki.yml 未找到 retention_period（实证缺失）")
    else:
        expect = duration_days(by_id, "logs_observability")
        actual = int(m.group(1)) // 24
        if expect is not None and expect != actual:
            errors.append(f"logs_observability 注册 {expect}d 与 Loki 实配 {actual}d 不一致")
        if re.search(r"30\s*天|30\s*days", loki_text):
            errors.append("deploy/loki/loki.yml 注释仍含与 180d 冲突的“30 天”表述（T-01 已修，防回归）")

    prom_text = _read(root / "deploy/docker-compose.community.yml")
    m = re.search(r"--storage\.tsdb\.retention\.time=(\d+)d", prom_text)
    if not m:
        errors.append("docker-compose.community.yml 未找到 prometheus retention（实证缺失）")
    else:
        expect = duration_days(by_id, "metrics_prometheus")
        actual = int(m.group(1))
        if expect is not None and expect != actual:
            errors.append(f"metrics_prometheus 注册 {expect}d 与 Prometheus 实配 {actual}d 不一致")

    backup_text = _read(root / "scripts/backup_pg.sh")
    m = re.search(r"RETENTION_DAYS:?\s*-?\s*(\d+)", backup_text)
    if not m:
        errors.append("scripts/backup_pg.sh 未找到 RETENTION_DAYS 默认值（实证缺失）")
    else:
        expect = duration_days(by_id, "backups_pg")
        actual = int(m.group(1))
        if expect is not None and expect != actual:
            errors.append(f"backups_pg 注册 {expect}d 与 backup_pg.sh 默认 {actual}d 不一致")

    copies_text = _read(root / "scripts/backup_imboy_db.sh")
    if by_id.get("backups_imboy_db", {}).get("duration") == "10-copies":
        if not re.search(r"10", copies_text):
            errors.append("backups_imboy_db 注册 10 份轮换，但 backup_imboy_db.sh 未体现")

    return errors


def duration_days(by_id: dict, class_id: str):
    entry = by_id.get(class_id, {})
    duration = str(entry.get("duration", ""))
    if entry.get("status") != "evidence-backed":
        return None
    m = re.match(r"^(\d+)\s*d$", duration)
    return int(m.group(1)) if m else None


def validate(path: Path = POLICY, check_files: bool = True, root: Path = ROOT) -> list:
    policy = load_policy(path)
    errors = validate_schema(policy)
    if check_files:
        errors.extend(validate_consistency(policy, root))
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=str(ROOT), help="仓库根目录")
    parser.add_argument("--no-files", action="store_true", help="跳过 deploy/脚本实证一致性检查")
    args = parser.parse_args()

    root = Path(args.root)
    policy_path = root / "docs/compliance/retention-policy.yml"
    if not policy_path.exists():
        print(f"FAIL: 未找到 {policy_path}")
        return 1

    try:
        errors = validate(policy_path, check_files=not args.no_files, root=root)
    except (PolicyError, yaml.YAMLError) as exc:
        print(f"FAIL: {exc}")
        return 1

    if errors:
        print("FAIL:")
        for err in errors:
            print(f"  - {err}")
        return 1
    print("retention-policy registry: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
