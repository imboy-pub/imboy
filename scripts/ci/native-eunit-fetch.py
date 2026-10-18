#!/usr/bin/env python3
"""Fetch only immutable, approved public build inputs; do not fetch user data."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tarfile

ROOT = Path(os.environ['NATIVE_CI_ROOT']).resolve()
PINS = json.loads(Path(__file__).with_name('native-eunit-pins.json').read_text())
SRC = ROOT / 'sources'
SRC.mkdir(parents=True, exist_ok=True)

def run(*args):
    subprocess.run(args, check=True)

def fetch(url, target, digest):
    if not url.startswith('https://'):
        raise SystemExit('Only HTTPS public source URLs are allowed')
    if not target.exists():
        part = target.with_name(target.name + '.part')
        run('curl', '--proto', '=https', '--tlsv1.2', '-fsSL', '--retry', '3',
            '--connect-timeout', '30', '--max-time', '600', '-o', str(part), url)
        part.rename(target)
    if hashlib.file_digest(target.open('rb'), 'sha256').hexdigest() != digest:
        raise SystemExit('SHA256 mismatch: ' + target.name)

def archive(entry):
    target = SRC / entry['file']
    fetch(entry['url'], target, entry['sha256'])
    with tarfile.open(target) as tar:
        tar.extractall(SRC, filter='data')

for entry in PINS['sources']:
    if entry['kind'] == 'archive':
        archive(entry)
    else:
        target = SRC / entry['directory']
        run('git', 'init', '-q', str(target))
        run('git', '-C', str(target), 'remote', 'add', 'origin', entry['url'])
        run('git', '-C', str(target), 'fetch', '--depth=1', 'origin', entry['commit'])
        run('git', '-C', str(target), 'checkout', '--detach', entry['commit'])
        run('git', '-C', str(target), 'submodule', 'update', '--init', '--recursive')
        actual = subprocess.check_output(['git', '-C', str(target), 'rev-parse', 'HEAD'], text=True).strip()
        if actual != entry['commit']:
            raise SystemExit('Git commit mismatch: ' + target.name)
        for subdir, commit in entry.get('submodule_commits', {}).items():
            actual = subprocess.check_output(['git', '-C', str(target / subdir), 'rev-parse', 'HEAD'], text=True).strip()
            if actual != commit:
                raise SystemExit('Submodule commit mismatch: ' + subdir)
for entry in PINS['rust']:
    archive(entry)
(ROOT / 'prefix/bin').mkdir(parents=True, exist_ok=True)
garage = ROOT / 'prefix/bin/garage'
fetch(PINS['garage']['url'], garage, PINS['garage']['sha256'])
garage.chmod(0o755)
print('PINNED_PUBLIC_BUILD_INPUTS_VERIFIED')
