"""Guard API generation against dropped definitions and audience auth drift."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import yaml

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("api_aggregate", ROOT / "api/gen_aggregate.py")
GEN = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GEN)


class AggregateTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.api = Path(self.directory.name)
        (self.api / "paths").mkdir()
        for audience in ("api", "adm", "internal"):
            self.write_root(audience)

    def write_root(self, audience, **changes):
        root = {"openapi": "3.1.0", "info": {"title": audience, "version": "1"},
                "paths": {}, "components": {}, "security": [{"jwt": []}]}
        root.update(changes)
        (self.api / f"openapi-{audience}.yaml").write_text(yaml.safe_dump(root))

    def add_leaf(self, name, operation):
        (self.api / "paths" / name).write_text(yaml.safe_dump({"get": operation}))
        return {"$ref": f"./paths/{name}"}

    def test_yaml_styles_and_individual_components_survive(self):
        self.add_leaf("one.yaml", {"operationId": "one"})
        self.write_root("api", components={"schemas": {"First": {"type": "string"},
                                                       "Second": {"type": "integer"}}})
        path = self.api / "openapi-api.yaml"
        original = path.read_text().replace("paths: {}", "paths:\n  /one:\n    $ref: ./paths/one.yaml")
        for ref in ("./paths/one.yaml", "'./paths/one.yaml'", '"./paths/one.yaml"'):
            with self.subTest(ref=ref):
                path.write_text(original.replace("$ref: ./paths/one.yaml", "$ref: " + ref))
                result = yaml.safe_load(GEN.build(self.api))
                self.assertEqual({"First", "Second"}, set(result["components"]["schemas"]))
                self.assertIn("/one", result["paths"])

    def test_conflicting_component_rejected(self):
        self.write_root("api", components={"schemas": {"Id": {"type": "integer"}}})
        self.write_root("adm", components={"schemas": {"Id": {"type": "string"}}})
        with self.assertRaisesRegex(ValueError, "components/schemas/Id"):
            GEN.build(self.api)

    def test_duplicate_yaml_key_rejected(self):
        path = self.api / "openapi-api.yaml"
        path.write_text(path.read_text() + "paths: {}\n")
        with self.assertRaisesRegex(ValueError, "duplicate YAML"):
            GEN.build(self.api)

    def test_yaml_merge_override_is_supported(self):
        path = self.api / "merge.yaml"
        path.write_text("base: &base {type: string}\noverride: &override\n"
                        "  <<: *base\n  type: integer\n"
                        "reused: {<<: *override}\n"
                        "sequence: {<<: [*override, *base]}\n")
        document = GEN.load(path)
        for key in ("override", "reused", "sequence"):
            self.assertEqual("integer", document[key]["type"])

    def test_duplicate_keys_in_yaml_merge_sources_rejected(self):
        path = self.api / "merge.yaml"
        for source in ("schema: {<<: {type: string, type: integer}}\n",
                       "schema: {<<: [{type: string, type: integer}]}\n",
                       "schema: {<<: {<<: {type: string, type: integer}}}\n",
                       "schema: {<<: {type: string}, <<: {format: int64}}\n"):
            with self.subTest(source=source):
                path.write_text(source)
                with self.assertRaisesRegex(ValueError, "duplicate YAML"):
                    GEN.load(path)

    def test_missing_or_invalid_paths_cannot_drop_an_audience(self):
        for value in (None, [], "not-a-path-map"):
            with self.subTest(value=value):
                self.write_root("adm", paths=value)
                with self.assertRaisesRegex(ValueError, "paths must be an explicit mapping"):
                    GEN.build(self.api)
        self.write_root("adm")
        path = self.api / "openapi-adm.yaml"
        document = yaml.safe_load(path.read_text())
        document.pop("paths")
        path.write_text(yaml.safe_dump(document))
        with self.assertRaisesRegex(ValueError, "paths must be an explicit mapping"):
            GEN.build(self.api)

    def test_invalid_root_leaf_and_operation_rejected(self):
        for value in (None, [], "not-a-map"):
            with self.subTest(value=value):
                self.write_root("adm")
                (self.api / "openapi-adm.yaml").write_text(yaml.safe_dump(value))
                with self.assertRaisesRegex(ValueError, "expected a YAML mapping"):
                    GEN.build(self.api)
                leaf = self.add_leaf("invalid.yaml", {"operationId": "one"})
                self.write_root("adm", paths={"/invalid": leaf})
                (self.api / "paths/invalid.yaml").write_text(yaml.safe_dump(value))
                with self.assertRaisesRegex(ValueError, "expected a YAML mapping"):
                    GEN.build(self.api)
                self.add_leaf("invalid.yaml", value)
                with self.assertRaisesRegex(ValueError, "must be a mapping"):
                    GEN.build(self.api)

    def test_check_reports_staleness_without_writing(self):
        with patch.object(GEN, "API_DIR", self.api), patch.object(GEN.sys, "argv", ["gen", "--check"]):
            target = self.api / GEN.AGG
            expected = GEN.build(self.api)
            for content, code in ((expected, 0), ("stale\n", 1)):
                with self.subTest(code=code):
                    target.write_text(content)
                    before = target.read_bytes()
                    self.assertEqual(code, GEN.main())
                    self.assertEqual(before, target.read_bytes())

    def test_duplicate_path_across_audiences_rejected(self):
        leaf = self.add_leaf("one.yaml", {"operationId": "one"})
        for audience in ("api", "adm"):
            self.write_root(audience, paths={"/one": leaf})
        with self.assertRaisesRegex(ValueError, "duplicate path"):
            GEN.build(self.api)

    def test_inherited_wrong_authentication_rejected(self):
        leaf = self.add_leaf("internal.yaml", {"operationId": "one"})
        self.write_root("internal", paths={"/internal": leaf}, security=[{"hmac": []}])
        with self.assertRaisesRegex(ValueError, "needs explicit security"):
            GEN.build(self.api)

    def test_explicit_auth_and_public_override_survive(self):
        paths = {}
        for name, security in (("private", [{"cookieId": [], "cookieSig": []}]),
                               ("public", [])):
            paths["/" + name] = self.add_leaf(name + ".yaml", {"security": security})
        self.write_root("adm", paths=paths, security=[{"cookieId": [], "cookieSig": []}])
        content = GEN.build(self.api)
        self.assertIn("    $ref: './paths/private.yaml'", content)
        self.assertEqual(set(paths), set(yaml.safe_load(content)["paths"]))

    def test_current_candidate_has_unique_operations_and_preserves_security(self):
        # Build also checks inherited security across the three audience roots.
        aggregate = yaml.safe_load(GEN.build())
        ids = {}
        for path, item in aggregate["paths"].items():
            leaf = GEN.load(ROOT / "api" / item["$ref"])
            for method, operation in leaf.items():
                if method not in GEN.METHODS:
                    continue
                operation_id = operation["operationId"]
                self.assertNotIn(operation_id, ids, (operation_id, path, ids.get(operation_id)))
                ids[operation_id] = (method, path)


if __name__ == "__main__":
    unittest.main()
