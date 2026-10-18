#!/usr/bin/env python3
"""Fail closed on missing, failed, duplicate or mismatched UID reporter proof."""
import json
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

SUITE = 'native_uid_preflight'
EXPECTED = {surface + '/disk/provider_bypass_writer_denied' for surface in
            ('ordinary', 'enterprise_internal', 'enterprise_seat', 'cs_widget')}
FIELDS = {'leaf', 'owner_uid', 'writer_uid', 'unprivileged_writer_exit'}
MARKER = 'W403_PROVIDER_BYPASS '


def require(condition, message):
    if not condition:
        raise ValueError(message)


def extract(directory):
    paths = sorted(Path(directory).glob('*.xml'))
    require(len(paths) == 1 and paths[0].name == 'TEST-' + SUITE + '.xml',
            'Expected exactly one named UID suite XML report')
    suite = ET.parse(paths[0]).getroot()
    require(suite.tag == 'testsuite' and suite.get('name') == SUITE,
            'Unexpected UID reporter suite')
    require(all(suite.get(key) == value for key, value in
                {'tests': '4', 'failures': '0', 'errors': '0', 'skipped': '0'}.items()),
            'UID reporter counters are incomplete or nonzero')
    cases = list(suite)
    require(len(cases) == 4 and all(case.tag == 'testcase' for case in cases),
            'Expected exactly four UID testcases')
    proof = []
    for case in cases:
        outputs = list(case)
        require(len(outputs) == 1 and outputs[0].tag == 'system-out' and not list(outputs[0]),
                'UID testcase must have only one captured output and no failure/skip')
        records = [line[len(MARKER):] for line in (outputs[0].text or '').splitlines()
                   if line.startswith(MARKER)]
        require(len(records) == 1, 'Expected exactly one provider denial proof per testcase')
        row = json.loads(records[0])
        require(isinstance(row, dict) and set(row) == FIELDS, 'Unexpected proof fields')
        require(isinstance(row['leaf'], str) and row['leaf'] in EXPECTED,
                'Unexpected provider denial leaf')
        require(case.get('name', '').endswith(' (' + row['leaf'] + ')'),
                'Provider denial proof does not match its testcase')
        require(type(row['owner_uid']) is int and row['owner_uid'] == 0,
                'Owner UID must be root')
        require(type(row['writer_uid']) is int and row['writer_uid'] > 0,
                'Writer UID must be a different unprivileged principal')
        require(type(row['unprivileged_writer_exit']) is int and
                1 <= row['unprivileged_writer_exit'] <= 255,
                'Unprivileged write must exit unsuccessfully')
        proof.append(row)
    require({row['leaf'] for row in proof} == EXPECTED, 'Missing or duplicate UID proof leaves')
    return sorted(proof, key=lambda row: row['leaf'])


def main():
    try:
        proof = extract(sys.argv[1])
    except (ValueError, OSError, ET.ParseError):
        # Do not expose captured test output or arbitrary JSON in diagnostics.
        print('UID proof validation failed: require one clean named suite and four exact permission-denial rows', file=sys.stderr)
        return 1
    Path(sys.argv[2]).write_text(json.dumps(proof, indent=2) + '\n')
    return 0


if __name__ == '__main__':
    sys.exit(main())
