"""Verify artifact classification and the actual Git-index boundary."""
import importlib.util
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
            "docs/qa/report.md", "docs/plans/task.md", "docs/archive/design/run/plan.md",
            "docs/archive/review/code.md", "docs/archive/analysis/report.md",
            "docs/reference/evidence/screenshot.png", "test-results/result.json",
            ".claude/PRPs/plans/work.md", "I18N_NATIVE_REVIEW_ja-JP.md",
            "integration_test/full_acceptance/F00-S01/state.json",
            "docs/compliance/IMBoy Feature Composition F07 Execution Record.md",
            "docs/architecture/2026-09-25-storage-phase1-plan.md",
            "test/auto_test/LOOP_PROMPT.md",
        ]:
            with self.subTest(path=path):
                self.assertIsNotNone(HYGIENE.forbidden_reason(path))

    def test_preserves_source_contracts_and_reusable_fixtures(self):
        for path in [
            "test/fixtures/message.json", "test/unit_test/message_test.dart",
            "tests/e2e/widget.spec.ts", "docs/testing/README.md",
            "docs/api-contracts/report_center_3end_api_contract.md",
            "docs/adr/0001-four-layer-architecture.md", ".github/ISSUE_TEMPLATE/bug_report.md",
        ]:
            with self.subTest(path=path):
                self.assertIsNone(HYGIENE.forbidden_reason(path))

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
