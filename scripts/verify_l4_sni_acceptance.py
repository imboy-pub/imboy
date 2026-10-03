#!/usr/bin/env python3
"""Read-only L4 SNI evidence verification; integrity never implies completion."""

import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

FIELDS = (
    'acceptance_id scope required status backend_candidate_sha app_candidate_sha '
    'command exit_code oracle evidence_path evidence_sha256 timestamp note'
).split()
STATUSES = {'PASS', 'PARTIAL', 'PENDING', 'BLOCKED_ENV', 'BLOCKED_EVIDENCE', 'FAIL'}
LOCAL_IDS = '''L4-T0-WS L4-T1-DETECT L4-T1-TEST L4-T2-METRICS
L4-T3-INSTALLER L4-T3-SANDBOX L4-T3-PREFLIGHT L4-T4-RELAY L4-T4-STATS
L4-T4-ANALYZE L4-T5-CRON L4-T5-RULES L4-T5-DOCS L4-T6-FREEZE
L4-A7-REVIEW L4-A8-VERIFY L4-T7-PREP L4-T7A-SAMPLE L4-T7B-UPLOAD
L4-WB-B1 L4-WB-B2 L4-T7B-V2 L4-OB-CRON L4-P1-PREFLIGHT
L4-P2-INSTALLER L4-P3-APPSPLIT L4-F1F2F3-REVIEWFIX L4-F4-REVIEWFIX
L4-POLISH-MERGE L4-SERVER-SYNC-V4 L4-T8-RECOVERY L4-T9-ALERT-PIPELINE
L4-T10-TLS443 L4-T11-MATRIX'''.split()
EXPECTED_IDS = set(LOCAL_IDS) | {
    f'CL-{scope}-01-A{number:02}'
    for scope, count in [('TEST', 6), ('DEVICE', 7)]
    for number in range(1, count + 1)
}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def git(repo, *args):
    return subprocess.check_output(['git', '-C', str(repo), *args], text=True).strip()


def read_rows(path):
    with path.open(newline='') as stream:
        reader = csv.reader(stream, delimiter='\t')
        if next(reader) != FIELDS:
            raise ValueError('ledger header mismatch')
        rows = []
        for number, values in enumerate(reader, 2):
            if len(values) != len(FIELDS):
                raise ValueError(f'ledger line {number}: expected 13 columns')
            rows.append(dict(zip(FIELDS, values)))
    return rows


def verify(run, backend, app, plan):
    contract = json.loads((run / 'control/acceptance-contract.json').read_text())
    if contract['plan_sha256'] != digest(plan):
        raise ValueError('plan hash drift')
    rows = read_rows(run / 'control/acceptance.tsv')
    ids = [row['acceptance_id'] for row in rows]
    if (len(ids) != len(set(ids)) or set(ids) != EXPECTED_IDS
            or set(contract['acceptance_ids']) != EXPECTED_IDS):
        raise ValueError('duplicate or missing acceptance IDs')
    heads = {'backend': git(backend, 'rev-parse', 'HEAD'),
             'app': git(app, 'rev-parse', 'HEAD')}
    for row in rows:
        ident = row['acceptance_id']
        if row['status'] not in STATUSES or row['required'] not in {'yes', 'no'}:
            raise ValueError(f'{ident}: invalid status/required')
        if row['required'] != ('no' if ident == 'L4-T7-PREP' else 'yes'):
            raise ValueError(f'{ident}: required contract changed')
        if row['status'] != 'PASS':
            if not row['note']:
                raise ValueError(f'{ident}: missing reason')
            continue
        for name, repo in [('backend', backend), ('app', app)]:
            sha = row[f'{name}_candidate_sha']
            if not re.fullmatch(r'[0-9a-f]{40}', sha):
                raise ValueError(f'{ident}: invalid {name} SHA')
            if sha != heads[name]:
                raise ValueError(f'{ident}: {name} evidence is not current HEAD')
            subprocess.run(['git', '-C', str(repo), 'merge-base', '--is-ancestor',
                            contract[f'{name}_base_sha'], sha], check=True)
        if row['exit_code'] != '0' or not all(row[k] for k in
                ('command', 'oracle', 'timestamp', 'evidence_path')):
            raise ValueError(f'{ident}: incomplete PASS binding')
        if not re.fullmatch(r'[0-9a-f]{64}', row['evidence_sha256']):
            raise ValueError(f'{ident}: invalid evidence hash')
        path = (run / row['evidence_path']).resolve()
        if not path.is_relative_to(run.resolve()) or not path.is_file():
            raise ValueError(f'{ident}: evidence missing or outside run')
        if digest(path) != row['evidence_sha256']:
            raise ValueError(f'{ident}: evidence hash mismatch')
        record = json.loads(path.read_text())
        for field in ('acceptance_id', 'backend_candidate_sha', 'app_candidate_sha',
                      'command', 'oracle', 'timestamp'):
            if record[field] != row[field]:
                raise ValueError(f'{ident}: evidence binding mismatch: {field}')
        if (record['exit_code'] != 0 or record['oracle_pass'] is not True
                or record['skipped'] is not False
                or record['plan_sha256'] != contract['plan_sha256']):
            raise ValueError(f'{ident}: evidence result is not PASS')
        log = (run / record['log_path']).resolve()
        if not log.is_relative_to(run.resolve()) or not log.is_file():
            raise ValueError(f'{ident}: raw log missing or outside run')
        if digest(log) != record['log_sha256']:
            raise ValueError(f'{ident}: raw log hash mismatch')
    pending = [row['acceptance_id'] for row in rows
               if row['required'] == 'yes' and row['status'] != 'PASS']
    state = json.loads((run / 'control/state.json').read_text())
    if state['acceptance_statuses'] != {r['acceptance_id']: r['status'] for r in rows}:
        raise ValueError('state/ledger drift')
    if state['pending'] != pending or state['plan_status'] != ('PARTIAL' if pending else 'PASS'):
        raise ValueError('false completion state')
    return {'integrity': 'PASS', 'plan_status': 'PARTIAL' if pending else 'PASS',
            'pending': pending, 'heads': heads, 'count': len(rows)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-root', type=Path, required=True)
    parser.add_argument('--backend', type=Path, required=True)
    parser.add_argument('--app', type=Path, required=True)
    parser.add_argument('--plan', type=Path, required=True)
    parser.add_argument('--require-complete', action='store_true')
    args = parser.parse_args()
    try:
        result = verify(args.run_root.resolve(), args.backend, args.app, args.plan)
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        print(f'VERIFY_FAIL: {error}', file=sys.stderr)
        return 1
    print(json.dumps(result, ensure_ascii=False, indent=2))
    return 2 if args.require_complete and result['pending'] else 0


if __name__ == '__main__':
    sys.exit(main())
