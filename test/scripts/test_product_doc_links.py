"""Product links must handle URI schemes and percent-encoded filenames."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "links", Path(__file__).resolve().parents[2]
    / "docs/documentation-system/scripts/check-doc-links.py",
)
LINKS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(LINKS)


class ProductDocLinksTests(unittest.TestCase):
    def test_encoded_local_names_and_application_uris(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "Protocol Spec.md").write_text("# Protocol")
            (root / "README.md").write_text(
                "[spec](Protocol%20Spec.md#wire)\n"
                "[app](imboy://imboy.pub/workspace/demo/docs/example)\n"
                "[anchor](#section)\n"
            )
            self.assertEqual((1, []), LINKS.check_dir(root))

    def test_missing_local_document_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "README.md").write_text("[missing](retired.md)")
            self.assertEqual((1, [("README.md", "retired.md")]), LINKS.check_dir(root))


if __name__ == "__main__":
    unittest.main()
