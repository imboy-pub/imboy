#!/usr/bin/env python3
"""Keep private raw output, stream only credential-redacted output, preserve rc."""
import os
from pathlib import Path
import subprocess
import sys

raw_path, public_path, *command = sys.argv[1:]
secrets = [os.environ.get(name, '') for name in
           ('W202_GARAGE_ACCESS_KEY',
            'W202_GARAGE_SECRET_KEY',
            'NATIVE_GARAGE_RPC_SECRET')]
for name in ('TEXTGUARD_CHECK_SECRET_FILE', 'TEXTGUARD_PUBLISH_SECRET_FILE'):
    if os.environ.get(name):
        secrets.append(Path(os.environ[name]).read_text().strip())
secrets = sorted((value for value in secrets if value), key=len, reverse=True)
with open(raw_path, 'wb') as raw, open(public_path, 'wb') as public:
    child = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    assert child.stdout is not None
    for line in child.stdout:
        raw.write(line)
        safe = line
        for secret in secrets:
            safe = safe.replace(secret.encode(), b'[REDACTED_SYNTHETIC_CREDENTIAL]')
        public.write(safe)
        public.flush()
        sys.stdout.buffer.write(safe)
        sys.stdout.buffer.flush()
    rc = child.wait()
    footer = f'\nRAW_EXIT={rc}\n'.encode()
    raw.write(footer)
    public.write(footer)
Path(raw_path + '.exit').write_text(str(rc) + '\n')
sys.exit(rc if rc >= 0 else 128 - rc)
