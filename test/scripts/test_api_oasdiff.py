"""Smoke-test the actual diff binary's OpenAPI 3.1 boolean-schema semantics.

Run with OASDIFF_BIN=/path/to/oasdiff. The breaking-change CI job sets this
explicitly; local schema-only tests skip when the binary is unavailable.
"""
import copy
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

OASDIFF = os.environ.get("OASDIFF_BIN") or shutil.which("oasdiff")


@unittest.skipUnless(OASDIFF, "set OASDIFF_BIN to run the real OpenAPI diff smoke tests")
class BooleanSchemaDiffTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.base = Path(self.directory.name) / "base.json"
        self.head = Path(self.directory.name) / "head.json"
        self.spec = {
            "openapi": "3.1.0",
            "info": {"title": "Boolean schema regression", "version": "1.0.0"},
            "paths": {"/messages": {"post": {
                "operationId": "sendMessage",
                "requestBody": {"required": True, "content": {"application/json": {"schema": {
                    "type": "object",
                    "properties": {"allowed": True, "forbidden_alias": False},
                }}}},
                "responses": {"200": {"description": "Accepted"}},
            }}},
        }
        self.base.write_text(json.dumps(self.spec), encoding="utf-8")

    def breaking(self, head):
        self.head.write_text(json.dumps(head), encoding="utf-8")
        return subprocess.run(
            [OASDIFF, "breaking", str(self.base), str(self.head), "--fail-on", "ERR", "--format", "json"],
            capture_output=True, text=True, timeout=30, check=False,
        )

    def test_unchanged_boolean_schemas_load_and_pass(self):
        result = self.breaking(self.spec)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_newly_forbidden_request_property_is_blocked(self):
        head = copy.deepcopy(self.spec)
        schema = head["paths"]["/messages"]["post"]["requestBody"]["content"]["application/json"]["schema"]
        schema["properties"]["allowed"] = False
        result = self.breaking(head)
        self.assertEqual(1, result.returncode, result.stdout + result.stderr)
        # A parsing failure also exits nonzero; require the specific semantic
        # breaking-change finding so a broken parser cannot masquerade as a gate.
        self.assertIn("request-property-schema-became-false", result.stdout)


if __name__ == "__main__":
    unittest.main()
