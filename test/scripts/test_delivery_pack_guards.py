"""Exercise delivery guards in disposable repositories with synthetic bytes."""
import base64
from pathlib import Path
import subprocess
import tempfile
import tarfile
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
                    self.commit_fixture(root)
                    result = self.run_pack(root, str(Path(directory) / ('output-' + label.replace(' ', '-'))))
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn('[real-pem] sample data.txt', result.stderr)
            fixture.write_text('-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----\n')
            self.commit_fixture(root)
            result = self.run_pack(root, str(Path(directory) / 'output'))
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_pack_uses_head_bytes_and_excludes_staged_additions(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'repo'
            root.mkdir()
            self.initialize(root)
            (root / 'README.md').write_text('Dirty replacement\n')
            (root / 'staged.txt').write_text('Uncommitted file\n')
            subprocess.run(['git', 'add', '.'], cwd=root, check=True)
            output = Path(directory) / 'output'
            result = subprocess.run(['bash', str(SCRIPT), '--out-dir', str(output)],
                                    cwd=root, text=True, capture_output=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            with tarfile.open(next(output.glob('*.tar'))) as archive:
                self.assertEqual(archive.getnames(), ['README.md'])
                self.assertEqual(archive.extractfile('README.md').read(),
                                 b'Synthetic package fixture\n')
            self.assertEqual((root / 'README.md').read_text(), 'Dirty replacement\n')

    def test_symlink_must_reference_an_included_snapshot_file(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'repo'
            root.mkdir()
            self.initialize(root)
            (root / 'safe-link').symlink_to('README.md')
            self.commit_fixture(root)
            result = self.run_pack(root, str(Path(directory) / 'safe-output'))
            self.assertEqual(result.returncode, 0, result.stderr)
            (root / 'unsafe-link').symlink_to('../outside.txt')
            self.commit_fixture(root)
            result = self.run_pack(root, str(Path(directory) / 'unsafe-output'))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('[symlink] unsafe-link', result.stderr)

    @staticmethod
    def commit_fixture(root):
        subprocess.run(['git', 'add', '.'], cwd=root, check=True)
        subprocess.run(['git', '-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
                        'commit', '-qm', 'synthetic fixture update'], cwd=root, check=True)

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
