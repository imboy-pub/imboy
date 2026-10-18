#!/usr/bin/env python3
"""Fail closed on raw exit, incomplete output, framework errors and nonzero skips."""
import hashlib
import json
from pathlib import Path
import re
import sys

log = Path(sys.argv[1])
raw = log.read_text(errors='replace')
rc = int(Path(str(log) + '.exit').read_text().strip())
patterns = {
    'assertions': r'\*failed\*',
    'timeouts': r'\*timed out\*',
    'setup': r'context setup failed',
    'cleanup': r'context cleanup failed',
    'instantiation': r'instantiation of subtests failed',
    'cancelled': r'One or more tests were cancelled',
    'termination': r'unexpected termination of test process',
    'non_test': r'result from instantiator .* is not a test',
    'aborted': r'tests? aborted|ABORTED_BY_RESOURCE_GUARD=1|NOT_A_COMPLETE_FULL_RESULT',
    'signals': r'SIG(?:TERM|INT) received - shutting down',
    'empty_suite': r'There were no tests to run',
    'make_failure': r'(?:g?make)(?:\[\d+\])?: \*\*\*',
    'nonzero_failed': r'Failed:\s*[1-9]',
    'nonzero_skipped': r'Skipped:\s*[1-9]',
}
counts = {name: len(re.findall(pattern, raw, re.I)) for name, pattern in patterns.items()}
complete = [int(value) for value in re.findall(r'All ([1-9][0-9]*) tests passed\.', raw)]
result = {'raw_exit': rc, 'success_summaries': complete, 'framework_markers': counts,
          'private_raw_sha256': hashlib.sha256(log.read_bytes()).hexdigest(),
          'public_log_is_credential_redacted': True,
          'clean': rc == 0 and bool(complete) and not any(counts.values())}
Path(sys.argv[2]).write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result, indent=2))
raise SystemExit(0 if result['clean'] else 1)
