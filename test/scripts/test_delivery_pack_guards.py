"""Exercise delivery guards in disposable repositories with synthetic bytes."""
import base64
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[2] / 'scripts/deliver/make_delivery_pack.sh'


class DeliveryPackGuardsTests(unittest.TestCase):
    def test_rejects_repository_output_without_creating_it(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'repo'
            root.mkdir()
            self.initialize(root)
            link = Path(directory) / 'link'
            link.symlink_to(root, target_is_directory=True)
            for output in ['.', 'new-output', str(root / 'new-output'),
                           str(link / 'new-output'), '../repo/new-output']:
                with self.subTest(output=output):
                    result = self.run_pack(root, output)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn('outside the repository', result.stderr)
                    self.assertFalse((root / 'new-output').exists())

    def test_wrapped_private_key_rejected_and_placeholder_allowed(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'repo'
            root.mkdir()
            self.initialize(root)
            body = base64.b64encode(bytes(range(256))).decode()
            wrapped = '\n'.join(body[i:i + 64] for i in range(0, len(body), 64))
            fixture = root / 'sample data.txt'
            for label in ['PRIVATE KEY', 'RSA PRIVATE KEY', 'EC PRIVATE KEY',
                          'OPENSSH PRIVATE KEY', 'ENCRYPTED PRIVATE KEY']:
                with self.subTest(label=label):
                    fixture.write_text(f'-----BEGIN {label}-----\n{wrapped}\n-----END {label}-----\n')
                    subprocess.run(['git', 'add', '.'], cwd=root, check=True)
                    result = self.run_pack(root, str(Path(directory) / 'output'))
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn('[real-pem] sample data.txt', result.stderr)
            fixture.write_text('-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----\n')
            result = self.run_pack(root, str(Path(directory) / 'output'))
            self.assertEqual(result.returncode, 0, result.stderr)

    @staticmethod
    def initialize(root):
        subprocess.run(['git', 'init', '-q'], cwd=root, check=True)
        (root / 'README.md').write_text('Synthetic package fixture\n')
        subprocess.run(['git', 'add', '.'], cwd=root, check=True)
        subprocess.run(['git', '-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
                        'commit', '-qm', 'fixture'], cwd=root, check=True)

    @staticmethod
    def run_pack(root, output):
        return subprocess.run(['bash', str(SCRIPT), '--out-dir', output, '--no-tar'],
                              cwd=root, text=True, capture_output=True)


if __name__ == '__main__':
    unittest.main()
