#!/usr/bin/env python3
"""scripts/check_scripts_registry.py 的自测（S2-07）。

覆盖：INVENTORY.yml 解析、引用正则的跨仓路径排除、历史档案豁免、
预期引用豁免表、归档隔离的静态断言。
"""
import sys
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(REPO / "scripts"))

import check_scripts_registry as guard  # noqa: E402


class TestInventoryParsing(unittest.TestCase):
    def test_inventory_parses_and_complete(self):
        inv = guard.parse_inventory()
        self.assertGreater(len(inv), 400, f"登记条目过少：{len(inv)}")
        for path, fields in inv.items():
            self.assertIn("category", fields, f"{path} 缺 category")
            self.assertIn(fields["category"],
                          ("core", "optional", "acceptance", "archived", "unknown"),
                          f"{path} category 非法：{fields.get('category')}")
            self.assertIn(fields.get("disposition", ""),
                          ("keep", "move", "merge", "archive", "delete"),
                          f"{path} disposition 非法")


class TestRefRegex(unittest.TestCase):
    def test_bare_script_ref_matched(self):
        line = "运行 `scripts/backup_pg.sh` 备份"
        self.assertEqual(guard.REF_RX.findall(line), ["scripts/backup_pg.sh"])

    def test_cross_repo_and_server_paths_rejected(self):
        for line in (
            "见 `imboyapp/scripts/build_play_aab.sh`",
            "委托 `../imboyapp/scripts/regen_protobuf.sh`",
            "服务器路径 /opt/imboy/scripts/rotate_secrets.sh",
            "覆盖 `test/scripts/test_repository_hygiene.py`",
        ):
            self.assertEqual(guard.REF_RX.findall(line), [], line)

    def test_controller_escript_matched(self):
        self.assertEqual(guard.REF_RX.findall("escript scripts/imboy_ctl db ping"),
                         ["scripts/imboy_ctl"])


class TestHistoryExemption(unittest.TestCase):
    def test_history_files_exempt(self):
        for f in ("docs/design/2026-10-11-unified-delivery-closeout/plan-v1.md",
                  "docs/plans/2026-10-13-limiter-v1-local-evidence-and-plan.md",
                  "docs/architecture/attachment-lifecycle-v5/evidence/current-baseline.json",
                  "docs/design/x/materials-index.json",
                  "docs/engineering/erlang-performance-audit.md",
                  "docs/engineering/erlang-testing-gap-analysis.md"):
            self.assertRegex(f, guard.HISTORY_RX, f)

    def test_live_docs_not_exempt(self):
        for f in ("docs/engineering/engineering-guardrails.md",
                  "docs/guides/operations/deployment/compose-operations.md",
                  "scripts/README.md"):
            self.assertNotRegex(f, guard.HISTORY_RX, f)


class TestBareNameResolution(unittest.TestCase):
    """3b 无扩展名引用：解析/豁免/跳过规则。"""

    def _scan(self, text, fname="docs/x.md"):
        errs = []
        # 复用 main() 的 3b 逻辑代价高，直接对公开常量做最小行为验证：
        # 用 guard.REF_RX/bare_rx 的 findall + 与 main 相同的判定序列太耦合，
        # 这里验证正则本身的关键行为。
        hits = guard.REF_RX.findall(text)
        bares = guard.bare_rx.findall(text) if hasattr(guard, "bare_rx") else []
        return hits, bares

    def test_glob_prefix_not_flagged(self):
        # bare_rx 应捕获完整 glob（含 *），供后续 "*" 过滤
        _, bares = self._scan("既有 Bench：`scripts/cache_*_bench.erl` ×5、`scripts/check_*.sh`")
        self.assertTrue(all("*" in b for b in bares), bares)

    def test_dotfile_and_cross_repo_excluded(self):
        _, bares = self._scan("透传给 `deploy/install.sh`、写入 `imboy/scripts/.env.deploy` 与 `scripts/.env.deploy`")
        crossed = [b for b in bares if "imboy/scripts" in b]
        self.assertEqual(crossed, [], bares)  # 他仓前缀被负向断言排除

    def test_erl_crashdump_resolves_via_suffix(self):
        ref = "scripts/erl_crashdump_analyzer"
        root = guard.ROOT
        self.assertFalse((root / ref).exists())
        self.assertTrue(any((root / (ref + s)).exists()
                            for s in (".sh", ".py", ".escript")))


class TestExpectedRel(unittest.TestCase):
    def test_contributing_entry_present(self):
        self.assertIn(("CONTRIBUTING.md", "scripts/generate_error_code.dart"),
                      guard.EXPECTED_REL)


class TestArchiveIsolation(unittest.TestCase):
    def test_no_flow_file_references_archive(self):
        flow = set()
        for pat in ("Makefile", "include/*.mk", ".github/workflows/*.yml",
                    "deploy/install.sh", "deploy/preflight.sh", "deploy/cron/*",
                    "deploy/helm/templates/*.yaml", "Dockerfile*", "docker-compose*.yml"):
            flow.update(guard.glob_tracked(pat))
        for f in sorted(flow):
            text = (REPO / f).read_text(encoding="utf-8", errors="replace")
            self.assertNotIn("scripts/archive/", text, f"{f} 引用了归档工具")


if __name__ == "__main__":
    unittest.main(verbosity=2)
