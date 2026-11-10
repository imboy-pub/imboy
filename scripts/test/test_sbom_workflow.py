"""Exercise SBOM coverage, real scanner output, and the actual workflow shell."""

import copy
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest
from urllib.parse import quote, unquote


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github/workflows/sbom-diff.yml"
SPEC = importlib.util.spec_from_file_location("imboy_sbom", ROOT / "scripts/sbom.py")
inventory = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(inventory)
DEFAULT_PACKAGES = {"vitepress": "1.6.4", "@vue/shared": "3.5.13"}
HIRD_COMMIT = "2bb12f35a8209915849b5e103b44349c5c2a5595"


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


def raw_bom(packages):
    """Independent CycloneDX fixture with real package names and versions."""
    components = []
    for name, version in packages.items():
        purl = f"pkg:npm/{quote(name, safe='/')}@{version}"
        component = {"type": "library", "name": name.rsplit("/", 1)[-1],
                     "version": version, "purl": purl, "bom-ref": purl}
        if name.startswith("@"):
            component["group"] = name.split("/", 1)[0]
        components.append(component)
    return {"bomFormat": "CycloneDX", "specVersion": "1.6", "version": 1,
            "metadata": {"component": {"type": "application", "name": "imboy", "bom-ref": "imboy:test"},
                         "tools": {"components": [{"type": "application", "group": "aquasecurity",
                                                  "name": "trivy", "version": "0.72.0"}]}},
            "components": components}


class SbomWorkflowTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="imboy-sbom-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        for path in ("docs-site", "include", "scripts"):
            (self.root / path).mkdir()
        shutil.copyfile(ROOT / "scripts/sbom.py", self.root / "scripts/sbom.py")
        self.write_source()

    def write_source(self, packages=None, erlang=True):
        self.packages = dict(DEFAULT_PACKAGES if packages is None else packages)
        manifest = {"name": "imboy-docs-site", "private": True,
                    "devDependencies": {name: f"^{version}" for name, version in self.packages.items()}}
        (self.root / "docs-site/package.json").write_text(json.dumps(manifest))
        lock = {"lockfileVersion": 1, "configVersion": 1, "workspaces": {"": manifest},
                "packages": {name: [f"{name}@{version}", "", {}, "sha512-test-fixture"]
                             for name, version in self.packages.items()}}
        (self.root / "docs-site/bun.lock").write_text(json.dumps(lock))
        (self.root / "Makefile").write_text(
            "PROJECT = imboy\ninclude include/deps.mk\nDEPS = "
            + ("cowboy chatterbox hird_runtime uid" if erlang else "")
            + "\nBUILD_DEPS =\nTEST_DEPS =\nDOC_DEPS =\nLOCAL_DEPS = crypto\n")
        (self.root / "include/deps.mk").write_text(
            "dep_cowboy = git https://github.com/ninenines/cowboy.git 2.19.0\n"
            "dep_chatterbox = hex 0.16.0 ts_chatterbox\n"
            "dep_hex_core = git https://github.com/hexpm/hex_core.git v0.19.0\n"
            f"dep_hird_runtime = git https://github.com/no-materials/hird.git {HIRD_COMMIT}\n"
            "dep_uid = git https://github.com/fogfish/uid.git master\n"
            "dep_pg_types = ln ../../pg_types\n")
        (self.root / "erlang.mk").write_text("# Fixture build tool fingerprint.\n")
        (self.root / "rebar.lock").write_text("[].\n")

    def current(self):
        return inventory.prepare(self.root, raw_bom(self.packages))

    def run_summary(self, found, previous=None, current=None, rebaseline=False):
        with tempfile.TemporaryDirectory(dir=self.root, prefix="artifacts-") as temporary:
            root = Path(temporary)
            (root / "sbom-prev").mkdir()
            def write(path, value):
                path.write_text(value if isinstance(value, str) else json.dumps(value))
            if previous is not None:
                write(root / "sbom-prev/sbom-current.json", previous)
            write(root / "sbom-current.json", self.current() if current is None else current)
            summary, output = root / "summary", root / "output"
            result = subprocess.run(
                ["bash", "-c", summary_script().replace("/tmp/", str(root) + "/")],
                cwd=self.root, text=True, capture_output=True, timeout=10,
                env={"PATH": os.environ["PATH"], "PREVIOUS_SBOM_FOUND": found,
                     "REBASELINE_LEGACY": str(rebaseline).lower(),
                     "GITHUB_STEP_SUMMARY": str(summary), "GITHUB_OUTPUT": str(output)},
            )
            return (result, summary.read_text() if summary.exists() else "",
                    dict(line.split("=", 1) for line in output.read_text().splitlines())
                    if output.exists() else {})

    def assert_rejected(self, found, previous=None, current=None, rebaseline=False):
        result, summary, output = self.run_summary(found, previous, current, rebaseline)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("首次生成", summary)
        self.assertEqual(output, {})

    def test_previous_run_selection_and_error_propagation(self):
        download = "\n".join(step("Download previous SBOM artifact"))
        inputs = dict(re.findall(r"^          ([a-z_]+): (.+)$", download, re.M))
        expected = {"branch": "main", "workflow": "sbom-diff.yml", "workflow_conclusion": "success",
                    "search_artifacts": "true", "if_no_artifact_found": "warn", "github_token": "${{ github.token }}"}
        for key, value in expected.items():
            self.assertEqual(inputs[key], value)
        for key in ("run_id", "pr", "commit", "ref"):
            self.assertNotIn(key, inputs)
        self.assertNotIn("continue-on-error:", download)
        self.assertRegex(WORKFLOW.read_text(), r"(?m)^  actions: read$")
        report = "\n".join(step("Diff SBOM and write summary"))
        self.assertIn("${{ steps.prev-sbom.outputs.found_artifact }}", report)
        self.assertIn("github.event_name == 'workflow_dispatch' && inputs.rebaseline_legacy", report)
        self.assertIn("--include-dev-deps", "\n".join(step("Generate SBOM (CycloneDX JSON)")))
        self.assertIn("SBOM_TRIVY_BINARY: trivy", "\n".join(step("Check SBOM completeness regressions")))
        workflow = WORKFLOW.read_text()
        positions = [workflow.index("      - name: " + name) for name in (
            "Check SBOM completeness regressions", "Generate SBOM (CycloneDX JSON)",
            "Diff SBOM and write summary", "Upload current SBOM as artifact")]
        self.assertEqual(positions, sorted(positions))
        upload = "\n".join(step("Upload current SBOM as artifact"))
        self.assertNotIn("continue-on-error:", upload)
        self.assertNotRegex(upload, r"if:.*always\(")
        self.assertIn("if-no-files-found: error", upload)
        self.assertRegex(workflow, r"(?s)rebaseline_legacy:.*?type: boolean.*?default: false")

    def test_zero_file_only_or_partial_npm_inventory_is_rejected(self):
        for packages in ({}, {"vitepress": "1.6.4"}):
            for file_only in (False, True):
                with self.subTest(packages=packages, file_only=file_only):
                    bom = raw_bom(packages)
                    if file_only:
                        bom["components"].append({"type": "application", "name": "docs-site/bun.lock", "bom-ref": "lock"})
                    with self.assertRaises(ValueError):
                        inventory.prepare(self.root, bom)

    def test_missing_or_tampered_erlang_component_is_rejected(self):
        for mutation in ("remove", "version"):
            bom = self.current()
            component = next(c for c in bom["components"] if c["name"] == "cowboy")
            if mutation == "remove":
                bom["components"].remove(component)
            else:
                component["version"] = "0.0.0"
            with self.subTest(mutation=mutation), self.assertRaises(ValueError):
                inventory.validate(bom, self.root)

    def test_erlang_alias_commit_and_floating_ref_are_preserved(self):
        _, components, _ = inventory.coverage_inputs(self.root)
        self.assertEqual({c["name"] for c in components}, {"cowboy", "ts_chatterbox", "hex_core", "hird_runtime", "uid"})
        by_name = {c["name"]: c for c in components}
        self.assertEqual(by_name["ts_chatterbox"]["purl"], "pkg:hex/ts_chatterbox@0.16.0")
        self.assertIn(HIRD_COMMIT, json.dumps(by_name["hird_runtime"]))
        self.assertNotIn("version", by_name["uid"])
        properties = {p["name"]: p["value"] for p in by_name["uid"]["properties"]}
        self.assertEqual(properties["imboy:sbom:declared-ref"], "master")
        self.assertEqual(properties["imboy:sbom:resolution"], "unresolved")

    def test_missing_inputs_and_manifest_lock_mismatch_fail_closed(self):
        for filename in ("Makefile", "include/deps.mk", "erlang.mk", "rebar.lock", "docs-site/package.json", "docs-site/bun.lock"):
            self.write_source()
            (self.root / filename).unlink()
            with self.subTest(filename=filename), self.assertRaises(ValueError):
                self.current()
        self.write_source()
        manifest = self.root / "docs-site/package.json"
        manifest.write_text(manifest.read_text().replace("^1.6.4", "^2.0.0"))
        with self.assertRaises(ValueError):
            self.current()

    def test_unknown_erlang_dependency_and_unsupported_syntax_are_rejected(self):
        for line in ("DEPS += undefined_dependency\n", "DEPS += $(EXTRA_DEPS)\n"):
            self.write_source()
            with (self.root / "Makefile").open("a") as stream:
                stream.write(line)
            with self.subTest(line=line), self.assertRaises(ValueError):
                self.current()
        self.write_source()
        (self.root / "include/deps.mk").write_text("dep_cowboy = $(COWBOY_SOURCE)\n")
        with self.assertRaises(ValueError):
            self.current()

    def test_invalid_lockfile_is_not_no_dependencies(self):
        for content in ("not json", '{"lockfileVersion":999,"workspaces":{},"packages":{}}'):
            (self.root / "docs-site/bun.lock").write_text(content)
            with self.subTest(content=content), self.assertRaises(ValueError):
                self.current()

    def test_unhandled_erlang_overrides_or_missing_include_fail_closed(self):
        changes = (("Makefile", "dep_cowboy = git https://github.com/ninenines/cowboy.git 2.18.0\n"),
                   ("include/deps.mk", "dep_cowboy_commit = 2.18.0\n"),
                   ("Makefile", None))
        for filename, extra in changes:
            self.write_source()
            path = self.root / filename
            text = path.read_text()
            path.write_text(text + extra if extra else text.replace("include include/deps.mk\n", ""))
            with self.subTest(filename=filename, extra=extra), self.assertRaises(ValueError):
                self.current()

    def test_demonstrably_empty_declarations_can_have_zero_packages(self):
        self.write_source({}, erlang=False)
        bom = self.current()
        inventory.validate(bom, self.root)
        self.assertEqual([c for c in bom["components"] if c.get("purl")], [])

    def test_no_artifact_is_a_valid_first_run(self):
        result, summary, _ = self.run_summary("false")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("首次生成", summary)
        self.assertIn("vitepress", summary)

    def test_unchanged_baseline_and_rebaseline_flag_report_zero_changes(self):
        same = self.current()
        for reset in (False, True):
            result, summary, output = self.run_summary("true", same, same, reset)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("无依赖变更", summary)
            self.assertEqual(output, {"sbom_added": "0", "sbom_removed": "0", "sbom_changed": "0"})

    def test_real_lock_removal_and_version_change_are_reported(self):
        previous = self.current()
        self.write_source({"vitepress": "1.6.3"})
        result, summary, output = self.run_summary("true", previous)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("@vue/shared", unquote(summary))
        self.assertIn("vitepress", summary)
        self.assertEqual(output, {"sbom_added": "1", "sbom_removed": "2", "sbom_changed": "1"})

    def test_npm_metadata_cannot_override_validated_version(self):
        previous, current = self.current(), self.current()
        component = next(c for c in current["components"] if c["name"] == "vitepress")
        component.setdefault("properties", []).append(
            {"name": "imboy:sbom:declared-ref", "value": "irrelevant-git-metadata"})
        result, _, output = self.run_summary("true", previous, current)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output, {"sbom_added": "0", "sbom_removed": "0", "sbom_changed": "0"})

    def test_missing_scan_with_unchanged_lock_is_not_reported_as_deletion(self):
        previous, current = self.current(), self.current()
        current["components"] = [c for c in current["components"] if c["name"] != "shared"]
        self.assert_rejected("true", previous, current)

    def test_comment_only_source_changes_do_not_invalidate_comparison(self):
        previous = self.current()
        for name in ("Makefile", "include/deps.mk"):
            with (self.root / name).open("a") as stream:
                stream.write("# Documentation-only edit.\n")
        result, _, output = self.run_summary("true", previous)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output, {"sbom_added": "0", "sbom_removed": "0", "sbom_changed": "0"})

    def test_scoped_packages_with_same_basename_keep_their_identity(self):
        self.write_source({"@vue/shared": "3.5.13", "@vueuse/shared": "12.0.0"})
        previous = self.current()
        self.write_source({"@vue/shared": "3.5.14", "@vueuse/shared": "12.0.0"})
        result, summary, output = self.run_summary("true", previous)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("@vue/shared", unquote(summary))
        self.assertEqual(output, {"sbom_added": "1", "sbom_removed": "1", "sbom_changed": "1"})

    def test_coverage_schema_scope_and_domains_cannot_drift(self):
        for key, value in (("version", 2), ("scope", "different-scope"), ("domains", {})):
            previous = self.current()
            prop = next(p for p in previous["metadata"]["properties"] if p["name"] == "imboy:sbom:coverage")
            coverage = json.loads(prop["value"])
            coverage[key] = value
            prop["value"] = json.dumps(coverage)
            with self.subTest(key=key):
                self.assert_rejected("true", previous, rebaseline=True)

    def test_removing_one_of_multiple_versions_keeps_other_version(self):
        path = self.root / "docs-site/bun.lock"
        lock = json.loads(path.read_text())
        lock["packages"]["vitepress/@vue/shared"] = ["@vue/shared@3.5.12", "", {}, "sha512-test-fixture"]
        path.write_text(json.dumps(lock))
        raw = raw_bom(self.packages)
        raw["components"].extend(raw_bom({"@vue/shared": "3.5.12"})["components"])
        previous = inventory.prepare(self.root, raw)
        self.write_source()
        result, summary, output = self.run_summary("true", previous)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("3.5.12", summary)
        self.assertEqual(output["sbom_removed"], "1")
        self.assertEqual(output["sbom_added"], "0")

    def test_bad_or_duplicate_contract_is_not_legacy(self):
        for value in ("", "not json", "{}", "null", None):
            previous = self.current()
            prop = next(p for p in previous["metadata"]["properties"] if p["name"] == "imboy:sbom:coverage")
            if value is None:
                previous["metadata"]["properties"].append(copy.deepcopy(prop))
            else:
                prop["value"] = value
            with self.subTest(value=value):
                self.assert_rejected("true", previous, rebaseline=True)

    def test_legacy_empty_baseline_requires_explicit_reset_without_normal_diff(self):
        legacy = raw_bom({})
        self.assert_rejected("true", legacy)
        result, summary, output = self.run_summary("true", legacy, rebaseline=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("基线", summary)
        self.assertEqual(output, {})

    def test_reported_download_without_expected_file_is_rejected(self):
        self.assert_rejected("true")

    def test_missing_or_unknown_download_status_is_rejected(self):
        for value in ("", "unknown", "TRUE"):
            with self.subTest(value=value):
                self.assert_rejected(value, self.current())

    def test_corrupt_downloaded_baseline_cannot_be_reset(self):
        for previous in ("not json", {}, {"components": []}, {"bomFormat": "CycloneDX", "components": "bad"}):
            for reset in (False, True):
                with self.subTest(previous=previous, reset=reset):
                    self.assert_rejected("true", previous, rebaseline=reset)

    def test_corrupt_current_sbom_is_rejected(self):
        self.assert_rejected("true", self.current(), "not json")

    @unittest.skipUnless("SBOM_TRIVY_BINARY" in os.environ, "set SBOM_TRIVY_BINARY to exercise the installed scanner")
    def test_real_repository_scanner_covers_every_locked_package(self):
        binary = os.environ["SBOM_TRIVY_BINARY"]
        self.assertTrue(binary, "SBOM_TRIVY_BINARY must identify a runnable scanner")
        lock = json.loads(re.sub(r",(\s*[}\]])", r"\1", (ROOT / "docs-site/bun.lock").read_text()))
        expected = {"pkg:npm/" + package[0] for package in lock["packages"].values()}
        for include_dev in (False, True):
            output = self.root / ("dev.json" if include_dev else "default.json")
            command = [binary, "fs", str(ROOT), "--format", "cyclonedx", "--output", str(output),
                       "--include-dev-deps=" + str(include_dev).lower()]
            result = subprocess.run(command, text=True, capture_output=True, timeout=120)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            bom = json.loads(output.read_text())
            packages = {unquote(c["purl"]) for c in bom.get("components", []) if c.get("purl", "").startswith("pkg:npm/")}
            if include_dev:
                self.assertEqual(packages, expected)
                self.assertTrue(any(p.startswith("pkg:npm/vitepress@") for p in packages))
                inventory.validate(inventory.prepare(ROOT, bom), ROOT)
            else:
                self.assertFalse(any(p.startswith("pkg:npm/vitepress@") for p in packages))
                with self.assertRaises(ValueError):
                    inventory.prepare(ROOT, bom)


if __name__ == "__main__":
    unittest.main()
