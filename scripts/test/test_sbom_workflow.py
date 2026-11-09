"""Exercise the actual SBOM summary shell with isolated artifact fixtures."""

import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github/workflows/sbom-diff.yml"


def step(name):
    """Read one workflow step, following the existing nightly test convention."""
    lines = WORKFLOW.read_text().splitlines()
    start = lines.index(f"      - name: {name}")
    end = next((i for i in range(start + 1, len(lines))
                if lines[i].startswith("      - name:")), len(lines))
    return lines[start:end]


def summary_script():
    lines = step("Diff SBOM and write summary")
    start = lines.index("        run: |") + 1
    body = []
    for line in lines[start:]:
        if line and not line.startswith("          "):
            break
        body.append(line[10:])
    return "\n".join(body) + "\n"


def sbom(**versions):
    return json.dumps({"components": [
        {"name": name, "version": version, "type": "library"}
        for name, version in versions.items()
    ]})


class SbomWorkflowTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not shutil.which("jq"):
            raise RuntimeError("SBOM workflow regression requires jq")

    def test_previous_run_selection_and_error_propagation(self):
        download = "\n".join(step("Download previous SBOM artifact"))
        inputs = dict(re.findall(r"^          ([a-z_]+): (.+)$", download, re.M))
        self.assertEqual(inputs["branch"], "main")
        self.assertEqual(inputs["workflow"], "sbom-diff.yml")
        self.assertEqual(inputs["workflow_conclusion"], "success")
        self.assertEqual(inputs["search_artifacts"], "true")
        self.assertEqual(inputs["if_no_artifact_found"], "warn")
        self.assertEqual(inputs["github_token"], "${{ github.token }}")
        for key in ("run_id", "pr", "commit", "ref"):
            self.assertNotIn(key, inputs)
        self.assertNotIn("continue-on-error:", download)
        self.assertRegex(WORKFLOW.read_text(), r"(?m)^  actions: read$")
        self.assertIn("${{ steps.prev-sbom.outputs.found_artifact }}",
                      "\n".join(step("Diff SBOM and write summary")))

    def run_summary(self, found, previous=None, current=None):
        with tempfile.TemporaryDirectory(prefix="imboy-sbom-test-") as temporary:
            root = Path(temporary)
            (root / "sbom-prev").mkdir()
            if previous is not None:
                (root / "sbom-prev/sbom-current.json").write_text(previous)
            (root / "sbom-current.json").write_text(
                sbom(unchanged="1", updated="2", added="1") if current is None else current)
            summary = root / "summary"
            output = root / "output"
            script = summary_script().replace("/tmp/", str(root) + "/")
            result = subprocess.run(
                ["bash", "-c", script], text=True, capture_output=True, timeout=10,
                env={"PATH": os.environ["PATH"], "PREVIOUS_SBOM_FOUND": found,
                     "GITHUB_STEP_SUMMARY": str(summary), "GITHUB_OUTPUT": str(output)},
            )
            return (result, summary.read_text() if summary.exists() else "",
                    dict(line.split("=", 1) for line in output.read_text().splitlines())
                    if output.exists() else {})

    def test_no_artifact_is_a_valid_first_run(self):
        result, summary, output = self.run_summary("false")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("首次生成", summary)
        self.assertIn("| added | 1 | library |", summary)
        self.assertEqual(output, {})

    def test_downloaded_baseline_reports_real_changes(self):
        result, summary, output = self.run_summary(
            "true", sbom(unchanged="1", updated="1", removed="1"))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("首次生成", summary)
        self.assertIn("| updated | 1 | 2 |", summary)
        self.assertEqual(output, {"sbom_added": "2", "sbom_removed": "2", "sbom_changed": "1"})

    def test_unchanged_baseline_reports_zero_changes(self):
        same = sbom(package="1")
        result, summary, output = self.run_summary("true", same, same)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("无依赖变更", summary)
        self.assertEqual(output, {"sbom_added": "0", "sbom_removed": "0", "sbom_changed": "0"})

    def assert_rejected(self, found, previous=None, current=None):
        result, summary, output = self.run_summary(found, previous, current)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("首次生成", summary)
        self.assertEqual(output, {})

    def test_reported_download_without_expected_file_is_rejected(self):
        self.assert_rejected("true")

    def test_missing_or_unknown_download_status_is_rejected(self):
        for value in ("", "unknown", "TRUE"):
            with self.subTest(value=value):
                self.assert_rejected(value, sbom(package="1"))

    def test_corrupt_downloaded_baseline_is_rejected(self):
        self.assert_rejected("true", "not json")

    def test_corrupt_current_sbom_is_rejected(self):
        self.assert_rejected("true", sbom(package="1"), "not json")


if __name__ == "__main__":
    unittest.main()
