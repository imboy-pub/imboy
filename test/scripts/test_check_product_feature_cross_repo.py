import importlib.util
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class CrossRepoPathTest(unittest.TestCase):
    def test_custom_stale_repositories_cannot_pass(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            for relative in (
                'app/lib/app_core/feature_flags/generated_product_features.dart',
                'admin/src/generated/productFeatures.ts',
            ):
                path = root / relative
                path.parent.mkdir(parents=True)
                path.write_text('deliberately stale fixture\n')
            result = subprocess.run(
                [sys.executable, str(ROOT / 'scripts/check_product_feature_cross_repo.py'),
                 '--app-dir', str(root / 'app'), '--admin-dir', str(root / 'admin')],
                capture_output=True, text=True, cwd=ROOT,
            )
            self.assertEqual(1, result.returncode, result.stdout + result.stderr)
            self.assertIn('FAIL', result.stdout)

    def test_custom_valid_repositories_and_admin_drift(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            spec = importlib.util.spec_from_file_location(
                'product_generator', ROOT / 'scripts/generate_product_features.py')
            generator = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(generator)
            manifest = generator.read_manifest(ROOT / 'config/product-feature-manifest.json')
            contract = generator.validate(manifest, generator.runtime_catalog(ROOT))
            artifacts = generator.render(contract, app_dir=root / 'app', admin_dir=root / 'admin')
            for destination, content in artifacts.items():
                if destination.is_relative_to(root):
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    destination.write_text(content)
            command = [sys.executable, str(ROOT / 'scripts/check_product_feature_cross_repo.py'),
                       '--app-dir', str(root / 'app'), '--admin-dir', str(root / 'admin')]
            valid = subprocess.run(command, capture_output=True, text=True, cwd=ROOT)
            self.assertEqual(0, valid.returncode, valid.stdout + valid.stderr)
            (root / 'admin/src/generated/productFeatures.ts').write_text('stale admin\n')
            stale = subprocess.run(command, capture_output=True, text=True, cwd=ROOT)
            self.assertEqual(1, stale.returncode, stale.stdout + stale.stderr)
            self.assertIn(str(root / 'admin'), stale.stdout)


if __name__ == '__main__':
    unittest.main()
