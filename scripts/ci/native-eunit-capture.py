#!/usr/bin/env python3
"""Keep private raw output; redact only explicitly injected credential values.

This does not detect unknown credentials or prove an output safe to publish.
"""
import os
from pathlib import Path
import re
import subprocess
import sys

REPLACEMENT = b'[REDACTED_SYNTHETIC_CREDENTIAL]'
SECRET_NAMES = (
    'W202_GARAGE_ACCESS_KEY',
    'W202_GARAGE_SECRET_KEY',
    'NATIVE_GARAGE_RPC_SECRET',
    'IMBOY_TEST_ACCESS',
    'IMBOY_TEST_SECRET',
    'PGPASSWORD',
)
SECRET_FILES = ('TEXTGUARD_CHECK_SECRET_FILE', 'TEXTGUARD_PUBLISH_SECRET_FILE')


def injected_secrets(environment):
    """Read known secret aliases and explicitly configured secret files only."""
    names = set(SECRET_NAMES) | {n for n in environment if n.endswith('_PG_PASSWORD')}
    values = [environment.get(name, '').encode() for name in names]
    for name in SECRET_FILES:
        if environment.get(name):
            values.append(Path(environment[name]).read_bytes().strip())
    return sorted({value for value in values if value}, key=len, reverse=True)


def redact_chunks(chunks, secrets):
    """Replace longest matching literal values, including across read boundaries."""
    values = sorted({value for value in secrets if value}, key=len, reverse=True)
    if not values:
        yield from chunks
        return
    pattern = re.compile(b'|'.join(re.escape(value) for value in values))
    reserve = max(map(len, values)) - 1
    pending = b''
    for chunk in chunks:
        pending += chunk
        cutoff = max(0, len(pending) - reserve)
        consumed = cutoff
        for match in pattern.finditer(pending):
            if match.start() >= cutoff:
                break
            consumed = max(consumed, match.end())
        if consumed:
            yield pattern.sub(REPLACEMENT, pending[:consumed])
            pending = pending[consumed:]
    if pending:
        yield pattern.sub(REPLACEMENT, pending)


def main():
    raw_path, public_path, *command = sys.argv[1:]
    secrets = injected_secrets(os.environ)
    with open(raw_path, 'wb') as raw, open(public_path, 'wb') as public:
        os.chmod(raw_path, 0o600)
        os.chmod(public_path, 0o600)
        child = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if child.stdout is None:
            raise RuntimeError('child output unavailable')
        def chunks():
            with child.stdout:
                while True:
                    chunk = child.stdout.read(65536)
                    if not chunk:
                        break
                    raw.write(chunk)
                    yield chunk
        for safe in redact_chunks(chunks(), secrets):
            public.write(safe)
            public.flush()
            sys.stdout.buffer.write(safe)
            sys.stdout.buffer.flush()
        rc = child.wait()
        footer = f'\nRAW_EXIT={rc}\n'.encode()
        raw.write(footer)
        public.write(footer)
    Path(raw_path + '.exit').write_text(str(rc) + '\n')
    return rc if rc >= 0 else 128 - rc


if __name__ == '__main__':
    sys.exit(main())
