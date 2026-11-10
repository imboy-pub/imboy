"""Synthetic report data only: upload reconciliation must fail closed."""
import copy
import json
from pathlib import Path
import tempfile
import unittest
from prepare_semgrep_sarif import prepare

RULE = "python.lang.security.fixture.exact-rule"


class SemgrepSarifTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        self.source = self.root / "source.py"
        self.source.write_text(f"# reviewed local fixture\ncall()  # nosemgrep: {RULE}\n")
        self.finding = {
            "ruleId": RULE,
            "locations": [{"physicalLocation": {
                "artifactLocation": {"uri": "source.py"},
                "region": {"startLine": 2, "startColumn": 1, "endLine": 2,
                           "endColumn": 7, "snippet": {"text": "private fixture"}},
            }}],
            "message": {"text": "private fixture"},
        }
        self.json_result = {
            "check_id": RULE, "path": "source.py",
            "start": {"line": 2, "col": 1}, "end": {"line": 2, "col": 7},
            "extra": {"severity": "ERROR"},
        }
        self.report = {"version": "1.184.0", "results": [self.json_result]}
        self.sarif = {"version": "2.1.0", "runs": [{
            "tool": {"driver": {"name": "Semgrep OSS", "semanticVersion": "1.184.0",
                               "rules": [{"id": RULE, "defaultConfiguration": {"level": "error"}}]}},
            "results": [self.finding], "automationDetails": {"id": "semgrep/"},
        }]}

    def suppress(self):
        self.finding["suppressions"] = [{"kind": "inSource"}]
        self.report["results"] = []

    def test_active_findings_and_metadata_are_unchanged(self):
        original = copy.deepcopy(self.sarif)
        output, inventory = prepare(self.report, self.sarif, self.root)
        self.assertEqual(original, output)
        self.assertEqual(original, self.sarif)
        self.assertEqual([], inventory)

    def test_exception_is_audited_without_private_fields(self):
        self.suppress()
        output, inventory = prepare(self.report, self.sarif, self.root)
        self.assertEqual([], output["runs"][0]["results"])
        self.assertEqual(1, len(self.sarif["runs"][0]["results"]))
        self.assertEqual(RULE, inventory[0]["rule_id"])
        self.assertEqual(64, len(inventory[0]["source_sha256"]))
        self.assertNotIn("private fixture", json.dumps(inventory))

    def test_previous_line_comment_is_accepted(self):
        self.suppress()
        self.source.write_text(f"# nosemgrep: {RULE}\ncall()\n")
        self.assertEqual(1, len(prepare(self.report, self.sarif, self.root)[1]))

    def test_broad_wrong_missing_or_string_annotations_fail(self):
        self.suppress()
        for source in ("# nosemgrep\ncall()\n", "# nosemgrep: other.rule\ncall()\n",
                       "call()\ncall()\n", f'example = "# nosemgrep: {RULE}"\ncall()\n',
                       f'"""# nosemgrep: {RULE}\ncall()\n"""\n'):
            with self.subTest(source=source):
                self.source.write_text(source)
                with self.assertRaises(ValueError):
                    prepare(self.report, self.sarif, self.root)

    def test_unknown_external_mixed_or_unreviewed_suppressions_fail(self):
        self.suppress()
        for value in ([{"kind": "external"}], [{"kind": "inSource", "status": "underReview"}],
                      [{"kind": "inSource"}, {"kind": "external"}], "invalid"):
            with self.subTest(value=value):
                self.finding["suppressions"] = value
                with self.assertRaises(ValueError):
                    prepare(self.report, self.sarif, self.root)

    def test_duplicate_and_location_mismatch_fail(self):
        for mutation in ("duplicate", "line", "column", "severity", "missing"):
            with self.subTest(mutation=mutation):
                sarif = copy.deepcopy(self.sarif)
                results = sarif["runs"][0]["results"]
                if mutation == "duplicate":
                    results.append(copy.deepcopy(results[0]))
                elif mutation == "missing":
                    results.clear()
                elif mutation == "severity":
                    results[0]["level"] = "warning"
                else:
                    key = "endLine" if mutation == "line" else "endColumn"
                    results[0]["locations"][0]["physicalLocation"]["region"][key] += 1
                with self.assertRaises(ValueError):
                    prepare(self.report, sarif, self.root)

    def test_matching_duplicates_are_preserved(self):
        self.report["results"].append(copy.deepcopy(self.json_result))
        self.sarif["runs"][0]["results"].append(copy.deepcopy(self.finding))
        output, _ = prepare(self.report, self.sarif, self.root)
        self.assertEqual(2, len(output["runs"][0]["results"]))

    def test_suppression_cannot_remove_an_active_json_finding(self):
        self.finding["suppressions"] = [{"kind": "inSource"}]
        with self.assertRaises(ValueError):
            prepare(self.report, self.sarif, self.root)

    def test_bad_paths_and_scanner_identity_fail(self):
        self.suppress()
        for path in ("../source.py", "/source.py", "https://example.invalid/source.py"):
            with self.subTest(path=path):
                self.finding["locations"][0]["physicalLocation"]["artifactLocation"]["uri"] = path
                with self.assertRaises(ValueError):
                    prepare(self.report, self.sarif, self.root)
        self.finding["locations"][0]["physicalLocation"]["artifactLocation"]["uri"] = "source.py"
        self.sarif["runs"][0]["tool"]["driver"]["semanticVersion"] = "0.0.0"
        with self.assertRaises(ValueError):
            prepare(self.report, self.sarif, self.root)

    def test_yaml_comments_are_distinct_from_scalar_text(self):
        self.suppress()
        self.finding["locations"][0]["physicalLocation"]["artifactLocation"]["uri"] = "source.yaml"
        source = self.root / "source.yaml"
        source.write_text(f'name: fixture\nvalue: "fixture" # nosemgrep: {RULE}\n')
        self.assertEqual(1, len(prepare(self.report, self.sarif, self.root)[1]))
        for value in (f'name: "# nosemgrep: {RULE}"\nvalue: fixture\n',
                      f"name: |\n  # nosemgrep: {RULE}\nvalue: fixture\n"):
            source.write_text(value)
            with self.assertRaises(ValueError):
                prepare(self.report, self.sarif, self.root)

    def test_dockerfile_comment_is_recognized(self):
        self.suppress()
        self.finding["locations"][0]["physicalLocation"]["artifactLocation"]["uri"] = "service_Dockerfile"
        (self.root / "service_Dockerfile").write_text(f"# nosemgrep: {RULE}\nCMD []\n")
        self.assertEqual(1, len(prepare(self.report, self.sarif, self.root)[1]))

    def test_unknown_source_root_fails(self):
        self.finding["locations"][0]["physicalLocation"]["artifactLocation"]["uriBaseId"] = "unknown"
        with self.assertRaises(ValueError):
            prepare(self.report, self.sarif, self.root)


if __name__ == "__main__":
    unittest.main()
