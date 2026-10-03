#!/usr/bin/env python3
"""Read-only non-OA ledger validation; integrity is separate from acceptance."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import sys
import tempfile
IDS = {'N0-A01', 'N5-A01'} | {f'N{card}-A{number:02}' for (card, count) in ((1, 3), (2, 4), (3, 4), (4, 3)) for number in range(1, count + 1)}
STATUSES = {'PASS_EXECUTED', 'PASS_REQUALIFIED', 'PARTIAL', 'NOT_PROVEN', 'BLOCKED_ENV', 'IN_PROGRESS'}
ROOT = Path(__file__).resolve().parents[1]
PLAN = ROOT / 'docs/plans/2026-10-02-enterprise-non-oa-final-acceptance-plan-v1.md'

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def validate_rows(rows, plan_sha):
    if not (len(rows) == len(IDS) and {row['id'] for row in rows} == IDS):
        raise ValueError('ID set')
    for row in rows:
        if row['status'] not in STATUSES:
            raise ValueError(f"{row['id']}: status")
        if not row['plan_sha256'] == plan_sha:
            raise ValueError(f"{row['id']}: plan drift")
        if not re.fullmatch('[0-9a-f]{64}', row['evidence_sha256']):
            raise ValueError(row['id'])
        if not (row['reason'] and row['next_action']):
            raise ValueError(f"{row['id']}: rationale")
        if row['status'].startswith('PASS'):
            sources = json.loads(row['source_sha'])
            if not (isinstance(sources, dict) and sources and all(isinstance(s, str) and re.fullmatch('[0-9a-f]{40}', s) for s in sources.values())):
                raise ValueError(row['id'])
            if not (row['command'] and row['oracle']):
                raise ValueError(row['id'])
            if row['exit_code'] not in ('0', 'manual'):
                raise ValueError(row['id'])
            if any(x in row['command'] for x in ('...', '(+7', '同上')):
                raise ValueError(row['id'])

def validate_artifacts(run):
    manifest = json.loads((run / 'artifacts.json').read_text())
    if not manifest:
        raise ValueError('empty artifact manifest')
    for ref in manifest:
        path = (run / ref['path']).resolve()
        if not path.is_relative_to(run.resolve()):
            raise ValueError('artifact escapes run directory')
        if not path.is_file():
            raise ValueError(f"missing artifact: {ref['path']}")
        if not digest(path) == ref['sha256']:
            raise ValueError(f"artifact mismatch: {ref['path']}")
    return {ref['path']: ref['sha256'] for ref in manifest}

def verify(run):
    with (run / 'acceptance.tsv').open() as ledger:
        rows = list(csv.DictReader(ledger, delimiter='\t'))
    validate_rows(rows, digest(PLAN))
    artifacts = validate_artifacts(run)
    for row in rows:
        if not artifacts.get(row['evidence_path']) == row['evidence_sha256']:
            raise ValueError(row['id'])
    state = json.loads((run / 'state.json').read_text())
    if not state['acceptance_ids'] == {r['id']: r['status'] for r in rows}:
        raise ValueError('state drift')
    pending = [r['id'] for r in rows if not r['status'].startswith('PASS')]
    if not state['pending'] == pending:
        raise ValueError('pending set')
    if pending:
        if not state['status'] == 'PARTIAL':
            raise ValueError('false completion')
    else:
        review = json.loads((run / 'independent-review.json').read_text())
        if not (review['verdict'] == 'APPROVE' and review['reviewer']):
            raise ValueError('final review')
        if not review['ledger_sha256'] == digest(run / 'acceptance.tsv'):
            raise ValueError('review drift')
        if not state['status'] == 'NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS':
            raise ValueError('completion state')
    print(f'INTEGRITY_PASS: {len(rows)} IDs; acceptance requires external independent review')
    if pending:
        print('Pending: ' + ', '.join(pending))
    return False

def self_test():
    with tempfile.TemporaryDirectory() as directory:
        run = Path(directory)
        artifact = run / 'proof.json'
        artifact.write_text('{"result": "recorded"}')
        (run / 'artifacts.json').write_text(json.dumps([{'path': artifact.name, 'sha256': digest(artifact)}]))
        validate_artifacts(run)
        artifact.write_text('{"result": "changed"}')
        try:
            validate_artifacts(run)
        except ValueError:
            pass
        else:
            raise AssertionError('tampered evidence was accepted')
    print('SELF_TEST_PASS: altered evidence is rejected')

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-root', type=Path)
    parser.add_argument('--require-complete', action='store_true')
    parser.add_argument('--self-test', action='store_true')
    args = parser.parse_args()
    if args.require_complete and not args.run_root:
        parser.error("--require-complete requires --run-root")
    if args.self_test:
        self_test()
    if args.run_root:
        complete = verify(args.run_root)
        return 2 if args.require_complete and (not complete) else 0
    if not args.self_test:
        parser.error('provide --run-root or --self-test')
    return 0
if __name__ == '__main__':
    try:
        sys.exit(main())
    except (AssertionError, OSError, ValueError, KeyError, TypeError, AttributeError) as error:
        print(f'INTEGRITY_FAIL: {error}', file=sys.stderr)
        sys.exit(1)
