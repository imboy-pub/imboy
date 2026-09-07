import importlib.util
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/validate_retention_policy.py"
SPEC = importlib.util.spec_from_file_location("validate_retention_policy", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)

FIXTURES = Path(__file__).resolve().parents[1] / "scripts"  # 根下脚本实证文件所在层


def base_class(cid="messages_e2ee", **changes):
    value = {
        "id": cid,
        "description": "d",
        "storage": "pg",
        "duration": "180d",
        "trigger": "created_at",
        "action": "delete",
        "status": "pending-owner",
        "owner": "product",
    }
    value.update(changes)
    return value


def write_policy(tmp: Path, policy: dict) -> Path:
    import yaml

    path = tmp / "retention-policy.yml"
    path.write_text(yaml.safe_dump(policy, allow_unicode=True), encoding="utf-8")
    return path


class RetentionPolicySchemaTest(unittest.TestCase):
    def test_real_policy_passes(self):
        real = Path(__file__).resolve().parents[2] / "docs/compliance/retention-policy.yml"
        self.assertEqual([], MODULE.validate(real, check_files=False))

    def test_missing_required_field(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(Path(tmp), {"legal_hold": "unsupported", "classes": [base_class()]})
            policy = MODULE.load_policy(path)
            policy["classes"][0].pop("owner")
            path.write_text(__import__("yaml").safe_dump(policy), encoding="utf-8")
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("owner" in e for e in errors))

    def test_country_branch_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(
                Path(tmp),
                {
                    "legal_hold": "unsupported",
                    "classes": [base_class(country="DE", duration="30d")],
                },
            )
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("per-country" in e for e in errors))

    def test_legal_hold_must_be_unsupported(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(
                Path(tmp),
                {"legal_hold": "enabled", "classes": [base_class()]},
            )
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("legal_hold" in e for e in errors))

    def test_bad_duration_and_action_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(
                Path(tmp),
                {
                    "legal_hold": "unsupported",
                    "classes": [base_class(duration="forever", action="wipe")],
                },
            )
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("duration" in e for e in errors))
            self.assertTrue(any("action" in e for e in errors))

    def test_required_classes_coverage(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(
                Path(tmp),
                {
                    "legal_hold": "unsupported",
                    "classes": [base_class(cid="messages_e2ee")],
                },
            )
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("覆盖缺失" in e and "vendors" in e for e in errors))

    def test_bad_status_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_policy(
                Path(tmp),
                {
                    "legal_hold": "unsupported",
                    "classes": [base_class(status="approved-by-me")],
                },
            )
            errors = MODULE.validate(path, check_files=False)
            self.assertTrue(any("status" in e for e in errors))


class RetentionPolicyConsistencyTest(unittest.TestCase):
    """实证一致性：注册表 evidence-backed 值必须与 deploy/脚本实配一致。"""

    def policy_with(self, loki="180d", prom="180d", backup="7d"):
        return {
            "legal_hold": "unsupported",
            "classes": [
                base_class(
                    cid="logs_observability",
                    storage="loki",
                    duration=loki,
                    status="evidence-backed",
                    owner="ops",
                ),
                base_class(
                    cid="metrics_prometheus",
                    storage="prometheus",
                    duration=prom,
                    status="evidence-backed",
                    owner="ops",
                ),
                base_class(
                    cid="backups_pg",
                    storage="backup",
                    duration=backup,
                    status="evidence-backed",
                    owner="ops",
                ),
            ],
        }

    def test_consistency_against_real_repo_files(self):
        real = Path(__file__).resolve().parents[2] / "docs/compliance/retention-policy.yml"
        # 真仓三处实证应与注册表一致（deploy 注释冲突已在 T-01 修复）
        self.assertEqual([], MODULE.validate(real, check_files=True))

    def test_mismatch_detected(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "docs/compliance").mkdir(parents=True)
            (root / "deploy/loki").mkdir(parents=True)
            (root / "deploy").mkdir(exist_ok=True)
            (root / "scripts").mkdir(exist_ok=True)
            write_policy(root / "docs/compliance", self.policy_with())
            (root / "deploy/loki/loki.yml").write_text(
                "limits_config:\n  retention_period: 720h\n", encoding="utf-8"
            )
            (root / "deploy/docker-compose.community.yml").write_text(
                "      - '--storage.tsdb.retention.time=180d'\n", encoding="utf-8"
            )
            (root / "scripts/backup_pg.sh").write_text(
                "RETENTION_DAYS=\"${RETENTION_DAYS:-7}\"\n", encoding="utf-8"
            )
            errors = MODULE.validate(
                root / "docs/compliance/retention-policy.yml", check_files=True, root=root
            )
            self.assertTrue(any("logs_observability" in e and "30d" in e for e in errors))

    def test_loki_30d_comment_regression_detected(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "docs/compliance").mkdir(parents=True)
            (root / "deploy/loki").mkdir(parents=True)
            (root / "scripts").mkdir(exist_ok=True)
            (root / "deploy").mkdir(exist_ok=True)
            write_policy(root / "docs/compliance", self.policy_with())
            (root / "deploy/loki/loki.yml").write_text(
                "# 保留 30 天日志\nlimits_config:\n  retention_period: 4320h\n",
                encoding="utf-8",
            )
            (root / "deploy/docker-compose.community.yml").write_text(
                "      - '--storage.tsdb.retention.time=180d'\n", encoding="utf-8"
            )
            (root / "scripts/backup_pg.sh").write_text(
                "RETENTION_DAYS=\"${RETENTION_DAYS:-7}\"\n", encoding="utf-8"
            )
            errors = MODULE.validate(
                root / "docs/compliance/retention-policy.yml", check_files=True, root=root
            )
            self.assertTrue(any("30 天" in e for e in errors))


if __name__ == "__main__":
    unittest.main()
