"""Verify artifact classification and the actual Git-index boundary."""
import importlib.util
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "hygiene", Path(__file__).resolve().parents[2] / "scripts/check_repository_hygiene.py"
)
HYGIENE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(HYGIENE)


class RepositoryHygieneTests(unittest.TestCase):
    def test_rejects_execution_materials_at_any_depth(self):
        for path in [
            ".otp/imboy/priv", ".dart_tool/package_config.json",
            "lib/__pycache__/module.pyc", "ebin/module.beam",
            "nested/node_modules/cache.json", "_build/default/app.beam",
            "docs/plans/evidence/2026-10-14-fake-v2.md",
            "docs/plans/task.md.log", "docs/plans/__pycache__/plan-v1.md",
            "docs/qa/report.md", "docs/plans/task.md", "docs/archive/design/run/plan.md",
            "docs/archive/review/code.md", "docs/archive/analysis/report.md",
            "docs/reference/evidence/screenshot.png", "test-results/result.json",
            ".claude/PRPs/plans/work.md", "I18N_NATIVE_REVIEW_ja-JP.md",
            "integration_test/full_acceptance/F00-S01/state.json",
            "docs/compliance/IMBoy Feature Composition F07 Execution Record.md",
            "test/auto_test/LOOP_PROMPT.md",
            "test/auto_test/task_state.json",
        ]:
            with self.subTest(path=path):
                self.assertIsNotNone(HYGIENE.forbidden_reason(path))

    def test_preserves_source_contracts_and_reusable_fixtures(self):
        for path in [
            "docs/plans/2026-10-14-three-platform-ci-micro-community-closeout-v2.md",
            "docs/design/2026-10-08-micro-community/plan-v1.md",
            "docs/architecture/attachment-lifecycle-v5/02-main-plan.md",
            "test/fixtures/message.json", "test/unit_test/message_test.dart",
            "tests/e2e/widget.spec.ts", "docs/testing/README.md",
            "docs/api-contracts/report_center_3end_api_contract.md",
            "docs/adr/0001-four-layer-architecture.md", ".github/ISSUE_TEMPLATE/bug_report.md",
        ]:
            with self.subTest(path=path):
                self.assertIsNone(HYGIENE.forbidden_reason(path))

    def test_preserved_materials_require_exact_hash_and_tracked_contract(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            subprocess.run(["git", "init", "-q", directory], check=True)
            for path in ["docs/qa/frozen.log", "docs/testing/contract.md"]:
                (root / path).parent.mkdir(parents=True, exist_ok=True)
                (root / path).write_text("synthetic")
                subprocess.run(["git", "-C", directory, "add", path], check=True)
            registry = root / "docs/reference/contracts/repository-artifacts.json"
            registry.parent.mkdir(parents=True)
            entry = dict(path="docs/qa/frozen.log", sha256=hashlib.sha256(b"synthetic").hexdigest(),
                         source_contract="docs/testing/contract.md", purpose="Required synthetic proof")
            def save(item):
                registry.write_text(json.dumps(dict(schema_version=1, artifacts=[item])))
            save(entry)
            self.assertEqual({entry["path"]}, HYGIENE.preserved_artifacts(root))
            subprocess.run(["git", "-C", directory, "add", str(registry.relative_to(root))], check=True)
            tree = subprocess.check_output(["git", "-C", directory, "write-tree"], text=True).strip()
            self.assertEqual({entry["path"]}, HYGIENE.preserved_artifacts(root, tree))
            for bad in [dict(entry, sha256="0"*64), dict(entry, source_contract="absent.md"),
                        dict(entry, path="docs/__pycache__/module.pyc"), dict(entry, path="docs/../secret")]:
                save(bad)
                with self.assertRaises(ValueError): HYGIENE.preserved_artifacts(root)
            save(entry)
            (root / entry["path"]).write_text("changed")
            subprocess.run(["git", "-C", directory, "add", entry["path"]], check=True)
            with self.assertRaisesRegex(ValueError, "changed"):
                HYGIENE.preserved_artifacts(root)

    def test_staged_removal_and_untracked_artifacts_are_not_delivery_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            artifact = root / "evidence/result.json"
            artifact.parent.mkdir()
            artifact.write_text("{}")
            self.assertNotIn("evidence/result.json", HYGIENE.tracked_paths(root))
            subprocess.run(["git", "-C", directory, "add", "evidence/result.json"], check=True)
            self.assertIn("evidence/result.json", HYGIENE.tracked_paths(root))
            subprocess.run(["git", "-C", directory, "rm", "--cached", "-q", "evidence/result.json"], check=True)
            self.assertNotIn("evidence/result.json", HYGIENE.tracked_paths(root))


if __name__ == "__main__":
    unittest.main()
