#!/usr/bin/env python3
"""test_validate_third_party_inventory —— V-01 校验脚本单元测试。

真仓正向（清单与三端源码互证 PASS）+ 负向 fixture（缺字段/缺类别/
源特征未映射/endpoint 无源/证据失效/枚举非法/受限子集违规/
海外门阻断与豁免）。validate 全参数显式传入（可测试性，吸取
validate_retention_policy 全局变量不生效的教训）。
"""

import tempfile
import unittest
from pathlib import Path

import sys

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "scripts"))

from validate_third_party_inventory import (  # noqa: E402
    EVIDENCE,
    REQUIRED_CATEGORIES,
    parse_inventory,
    validate,
)

INVENTORY = REPO / "docs/compliance/third-party-data-inventory.yml"


def write(tmp: Path, rel: str, content: str) -> None:
    path = tmp / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


ACTIVE_ROW = """providers:
  example_provider:
    provider: Example
    category: payment
    version: v1
    purpose: 测试用途
    data_fields: 字段A, 字段B
    endpoints: https://api.example-test-host.com
    region: cn
    necessity: optional
    consent: 用户主动触发
    privacy_url: https://example.com/privacy
    dpa_status: unknown
    retention: unknown
    deletion_api: unknown
    build_flavors: backend
    evidence: 测试证据
"""


class ParseTests(unittest.TestCase):
    def test_value_with_colon_space_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            inv = Path(td) / "inv.yml"
            inv.write_text(
                "meta:\n  schema_version: 1\nproviders:\n  a:\n    purpose: bad: value\n",
                encoding="utf-8",
            )
            with self.assertRaises(ValueError):
                parse_inventory(inv)


class ValidateTests(unittest.TestCase):
    def test_real_inventory_passes(self) -> None:
        ok, errors, _ = validate(INVENTORY, root=REPO)
        self.assertTrue(ok, msg="\n".join(errors))

    def test_real_inventory_parse_meta(self) -> None:
        meta, providers = parse_inventory(INVENTORY)
        self.assertEqual(meta.get("schema_version"), "1")
        self.assertIn("jpush_sms", providers)
        self.assertIn("fcm", providers)

    def test_overseas_gate_blocks_unknown_dpa(self) -> None:
        ok, errors, _ = validate(INVENTORY, root=REPO, overseas_gate=True)
        self.assertFalse(ok)
        blocked_line = next(e for e in errors if "BLOCKED" in e)
        # 真实出站行（dpa=unknown）必须被点名
        self.assertIn("jpush_sms", blocked_line)
        self.assertIn("sentry_flutter", blocked_line)
        # self-hosted / dev-only / no_data 行必须豁免
        self.assertNotIn("garage_s3", blocked_line)
        self.assertNotIn("livekit", blocked_line)
        self.assertNotIn("eturnal_turn", blocked_line)
        self.assertNotIn("alipay_sandbox", blocked_line)
        self.assertNotIn("fcm", blocked_line)

    def test_missing_required_field_fails(self) -> None:
        row = ACTIVE_ROW.replace("    privacy_url: https://example.com/privacy\n", "")
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + row)
            write(tmp, "src/evidence.txt", "api.example-test-host.com\n")
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),),
                required_categories=frozenset({"payment"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("privacy_url" in e for e in errors), msg=str(errors))

    def test_missing_category_row_fails(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + ACTIVE_ROW)
            write(tmp, "src/evidence.txt", "api.example-test-host.com\n")
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),),
                required_categories=frozenset({"payment", "crash"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("missing category rows" in e and "crash" in e for e in errors))

    def test_unmapped_source_feature_fails(self) -> None:
        """源码出现新出站特征而清单无对应行 = 未登记数据流，必须失败。"""
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + ACTIVE_ROW)
            write(
                tmp,
                "src/evidence.txt",
                "api.example-test-host.com\nhttps://sneaky.new-vendor.cn/api\n",
            )
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(
                    ("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),
                    ("sneaky.new-vendor.cn", "src/evidence.txt", r"sneaky\.new-vendor\.cn", False),
                ),
                required_categories=frozenset({"payment"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("sneaky.new-vendor.cn" in e and "not mapped" in e for e in errors))

    def test_endpoint_without_source_fails(self) -> None:
        """清单声称的 endpoint 在任何源中都找不到 = 漂移/编造，必须失败。"""
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + ACTIVE_ROW)
            write(tmp, "src/evidence.txt", "unrelated content\n")
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),),
                required_categories=frozenset({"payment"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("api.example-test-host.com" in e and "not found" in e for e in errors))

    def test_stale_evidence_fails(self) -> None:
        """文件存在但特征已移除 = 清单证据失效，必须失败（fail loud）。"""
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + ACTIVE_ROW)
            write(tmp, "src/evidence.txt", "feature was removed from source\n")
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),),
                required_categories=frozenset({"payment"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("no longer present" in e for e in errors), msg=str(errors))

    def test_no_data_row_minimal_schema(self) -> None:
        row = (
            "providers:\n"
            "  push_x:\n"
            "    provider: X Push\n"
            "    category: push\n"
            "    no_data: 服务端推送未接入\n"
            "    build_flavors: backend\n"
        )
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + row)
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(),
                required_categories=frozenset({"push"}),
            )
            self.assertTrue(ok, msg=str(errors))

    def test_bad_enum_fails(self) -> None:
        row = ACTIVE_ROW.replace("region: cn", "region: mars")
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td)
            write(tmp, "docs/inv.yml", "meta:\n  schema_version: 1\n" + row)
            write(tmp, "src/evidence.txt", "api.example-test-host.com\n")
            ok, errors, _ = validate(
                tmp / "docs/inv.yml",
                root=tmp,
                evidence=(("api.example-test-host.com", "src/evidence.txt", r"api\.example-test-host\.com", False),),
                required_categories=frozenset({"payment"}),
            )
            self.assertFalse(ok)
            self.assertTrue(any("region" in e for e in errors))

    def test_real_evidence_table_covers_required_categories_sources(self) -> None:
        """内建证据表的每条特征在真仓都必须命中——防止脚本证据表自身过期。"""
        from validate_third_party_inventory import load_evidence_texts

        loaded = load_evidence_texts(REPO, EVIDENCE)
        missing = [
            e["marker"]
            for e in loaded
            if e["text"] is None
        ]
        self.assertEqual(missing, [], msg=f"local run should find all evidence sources: {missing}")
        unmatched = [e["marker"] for e in loaded if not e["pattern"].search(e["text"])]
        self.assertEqual(unmatched, [], msg=f"evidence patterns not matched in source: {unmatched}")


if __name__ == "__main__":
    unittest.main()
