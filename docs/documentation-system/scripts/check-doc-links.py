#!/usr/bin/env python3
"""Check local Markdown links in tracked product documentation."""
import argparse
from pathlib import Path
import re
import subprocess
from urllib.parse import unquote, urlsplit

LINK_RE = re.compile(r'\[[^\]]*\]\(([^)]+)\)')


def markdown_files(root):
    try:
        repo = Path(subprocess.check_output(
            ['git', '-C', str(root), 'rev-parse', '--show-toplevel'],
            text=True, stderr=subprocess.DEVNULL,
        ).strip())
        paths = subprocess.check_output(
            ['git', '-C', str(repo), 'ls-files', '-z', '--', str(root)],
        ).decode().split('\0')
        return [repo / path for path in paths if path.endswith('.md')]
    except subprocess.CalledProcessError:
        return list(root.rglob('*.md'))


def check_dir(root):
    root = Path(root).resolve()
    dead, checked = [], 0
    for source in markdown_files(root):
        for match in LINK_RE.finditer(source.read_text(encoding='utf-8')):
            link = match.group(1).strip().strip('<>')
            parsed = urlsplit(link)
            if parsed.scheme or not parsed.path:
                continue
            target = unquote(parsed.path)
            checked += 1
            if not (source.parent / target).exists():
                dead.append((str(source.relative_to(root)), link))
    return checked, dead


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('roots', nargs='*', type=Path, help='Documentation directories')
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[3]
    roots = args.roots or [repo / 'docs']
    total = 0
    for root in roots:
        checked, dead = check_dir(root)
        print(f'{root}: checked={checked}, dead={len(dead)}')
        for source, link in dead:
            print(f'  [{source}] -> {link}')
        total += len(dead)
    return int(total > 0)


if __name__ == '__main__':
    raise SystemExit(main())
