#!/usr/bin/env python3
"""Read-only unified-closeout evidence checks; integrity is not business acceptance.

The expected contract and candidate are separate, externally hash-pinned inputs.
A source reviewer must independently enumerate the contract's five-field leaves;
copying requirements into that file does not establish completeness. PARTIAL mode
permits an explicitly incomplete source review but never a PASS on an unreviewed
leaf. Historical logs without execution bindings stay outside command_receipts.
No commands from evidence are executed. FULL also requires all final gates and
independent review; the checker cannot authenticate a human review or an oracle.

Contract schema 2 requires every leaf to freeze execution_repo (backend/app/admin)
and execution_cwd, a repository-relative directory ('.' means the root). Candidate
schema 2 requires index_sha256 (raw ls-files --stage -z), staged_sha256 (binary
diff --cached HEAD), and unstaged_sha256 (binary diff), including empty hashes.
Historical schema 1 inputs must be explicitly re-frozen and independently reviewed;
the verifier neither upgrades them nor infers execution policy from receipts.
"""
import argparse
import csv
from collections import defaultdict
from datetime import datetime
import hashlib
import json
import os
import re
from pathlib import Path
import subprocess
import sys

from verify_non_oa_acceptance import digest

KEY = ('source_plan', 'source_id', 'subcase', 'platform', 'role')
STATES = {'PASS', 'FAIL', 'UNVERIFIED', 'BLOCKED_SOURCE', 'BLOCKED_CONTRACT',
          'BLOCKED_RESOURCE', 'BLOCKED_DEPENDENCY', 'DEFERRED_BY_USER', 'NOT_RUN'}
GATES = {'BE-COMPILE', 'BE-EUNIT-R1', 'BE-EUNIT-R2', 'BE-DIALYZE',
         'ADMIN-TEST', 'ADMIN-LINT', 'ADMIN-TYPECHECK', 'ADMIN-DEADCODE',
         'ADMIN-BUILD', 'APP-ANALYZE', 'APP-TEST'}
BINDINGS = ('source', 'build', 'runner', 'config', 'fixture')


def require(ok, rule):
    if not ok:
        raise ValueError(rule)


def identity(row):
    require(all(isinstance(row.get(k), str) and row[k] for k in KEY), 'LEAF_KEY')
    return tuple(row[k] for k in KEY)


def cell_id(row):
    raw = json.dumps(identity(row), ensure_ascii=False, separators=(',', ':'))
    return hashlib.sha256(raw.encode('utf-8')).hexdigest()


def checked_rows(rows):
    result = {}
    for row in rows:
        key = identity(row)
        require(key not in result, 'REQ_LEAF_DUPLICATE')
        require(row.get('cell_id') == cell_id(row), 'CELL_SHA_MISMATCH')
        result[key] = row
    return result


def artifact(ref, root=None):
    path = Path(ref['path']).resolve()
    if root is not None:
        require(path.is_relative_to(root.resolve()), 'ARTIFACT_OUTSIDE_RUN')
    require(path.is_file() and digest(path) == ref['sha256'], 'ARTIFACT_SHA_MISMATCH')
    return path


def source_contract(contract):
    require(contract.get('schema_version') == 2, 'CONTRACT_SCHEMA')
    artifact(contract['plan'])
    sources = {s['id']: s for s in contract['sources']}
    require(len(sources) == len(contract['sources']) and sources, 'SOURCE_IDS')
    lines = {key: artifact(s).read_text().splitlines() for key, s in sources.items()}
    expected = checked_rows(contract['leaves'])
    require(bool(expected), 'EMPTY_SOURCE_SET')
    for row in expected.values():
        cwd = row.get('execution_cwd')
        require(row.get('execution_repo') in {'backend', 'app', 'admin'}
                and isinstance(cwd, str) and bool(cwd) and not Path(cwd).is_absolute()
                and '..' not in Path(cwd).parts, 'LEAF_EXECUTION_SCHEMA')
        loc = row['source_locator']
        start, end = loc['start'], loc['end']
        require(loc['source'] in lines and type(start) is int and type(end) is int
                and 1 <= start <= end <= len(lines[loc['source']]), 'SOURCE_LOCATOR')
        require(loc['quote'] == '\n'.join(lines[loc['source']][start - 1:end]), 'SOURCE_QUOTE')
        require(bool(row.get('oracle')) and type(row.get('required_local')) is bool,
                'LEAF_ORACLE')
    return expected


def git(repo, *args):
    command = ['git', '--no-pager', '-c', 'core.fsmonitor=false',
               '-c', 'core.hooksPath=/dev/null', '-C', repo]
    configuration = subprocess.run(
        [*command, 'config', '--null', '--get-regexp', r'^filter\..*\.(clean|process)$'],
        env=dict(os.environ, GIT_OPTIONAL_LOCKS='0'), capture_output=True, check=False)
    require(configuration.returncode in {0, 1}, 'GIT_CONFIG_UNAVAILABLE')
    require(not any(entry.partition(b'\n')[2].strip()
                    for entry in configuration.stdout.split(b'\0') if entry),
            'GIT_EXECUTABLE_FILTER')
    if args and args[0] == 'diff':
        command.extend(['diff', '--no-ext-diff', '--no-textconv', *args[1:]])
    else:
        command.extend(args)
    return subprocess.check_output(command, env=dict(os.environ, GIT_OPTIONAL_LOCKS='0'))


def visible_index(repo):
    flags = git(repo, 'ls-files', '-v', '-z').split(b'\0')
    require(all(not entry or entry[:1] == b'H' for entry in flags), 'HIDDEN_INDEX_STATE')
    for entry in git(repo, 'ls-files', '--stage', '-z').split(b'\0'):
        if not entry:
            continue
        meta, name = entry.split(b'\t', 1)
        mode, head, stage = meta.split()
        require(stage == b'0', 'UNMERGED_INDEX')
        if mode == b'160000':
            child = str(Path(repo) / name.decode())
            require(git(child, 'rev-parse', 'HEAD').strip() == head, 'SUBMODULE_HEAD')
            require(not git(child, 'status', '--porcelain', '--untracked-files=all').strip(),
                    'SUBMODULE_DIRTY')
            visible_index(child)


def candidate_snapshot(candidate, contract):
    require(candidate.get('schema_version') == 2, 'CANDIDATE_SCHEMA')
    require(set(candidate['sources']) == {'backend', 'app', 'admin'}, 'CANDIDATE_REPOS')
    for name, src in candidate['sources'].items():
        repo = src['repo_path']
        visible_index(repo)
        require(git(repo, 'rev-parse', 'HEAD').decode().strip() == src['head'], 'CANDIDATE_HEAD')
        require(git(repo, 'rev-parse', 'HEAD^{tree}').decode().strip() == src['tree'], 'CANDIDATE_TREE')
        patch = git(repo, 'diff', '--binary', 'HEAD')
        require(hashlib.sha256(patch).hexdigest() == src['patch_sha256'], 'DIRTY_UNBOUND')
        require(all(isinstance(src.get(key), str) and re.fullmatch(r'[0-9a-f]{64}', src[key])
                    for key in ('index_sha256', 'staged_sha256', 'unstaged_sha256')),
                'INDEX_UNBOUND')
        require(hashlib.sha256(git(repo, 'ls-files', '--stage', '-z')).hexdigest()
                == src['index_sha256'], 'INDEX_UNBOUND')
        require(hashlib.sha256(git(repo, 'diff', '--binary', '--cached', 'HEAD')).hexdigest()
                == src['staged_sha256'], 'INDEX_UNBOUND')
        require(hashlib.sha256(git(repo, 'diff', '--binary')).hexdigest()
                == src['unstaged_sha256'], 'DIRTY_UNBOUND')
        untracked = git(repo, 'ls-files', '--others', '--exclude-standard', '-z')
        names = [n.decode() for n in untracked.split(b'\0') if n]
        actual = {n: digest(Path(repo) / n) for n in names}
        require(actual == src['untracked'], 'DIRTY_UNBOUND')
        changed = git(repo, 'diff', '--name-only', 'HEAD', '-z')
        paths = set(names) | {n.decode() for n in changed.split(b'\0') if n}
        for arguments in [('diff', '--name-only', '--cached', 'HEAD', '-z'),
                          ('diff', '--name-only', '-z')]:
            paths.update(n.decode() for n in git(repo, *arguments).split(b'\0') if n)
        require(paths <= set(contract['owned_paths'].get(name, [])), 'FOREIGN_WIP')
    for refs in candidate.get('inputs', {}).values():
        for ref in refs:
            artifact(ref)


def time_value(raw):
    value = datetime.fromisoformat(raw)
    require(value.tzinfo is not None, 'RECEIPT_TIME')
    return value


def command_record(record, candidate_sha, candidate, root):
    require(record['candidate_sha256'] == candidate_sha, 'RECEIPT_CANDIDATE')
    argv = record['argv']
    require(isinstance(argv, list) and argv and all(isinstance(v, str) and v for v in argv),
            'RECEIPT_ARGV')
    require(type(record['exit_code']) is int, 'RECEIPT_EXIT')
    require(record['kind'] in {'quality_gate', 'business', 'negative_probe'}, 'RECEIPT_KIND')
    require(record['status'] in {'PASS', 'FAIL'}, 'RECEIPT_STATUS')
    if record['status'] == 'PASS':
        require(record['exit_code'] == 0, 'GATE_NONZERO_NOT_PASS')
        require(all(record.get(k) == 0 for k in ('skipped', 'cancelled', 'timed_out')),
                'RECEIPT_SKIP_CANCEL')
    require(time_value(record['started_at']) < time_value(record['ended_at']), 'RECEIPT_TIME')
    for key in BINDINGS:
        refs = record['bindings'][key]
        require(refs and refs == candidate['inputs'][key], 'RECEIPT_BINDING')
        for ref in refs:
            artifact(ref)
    path = artifact(record['log'], root)
    lines = path.read_text().splitlines()
    require(len(lines) >= 3 and lines[0].startswith('UC_COMMAND_START ')
            and lines[-1].startswith('UC_COMMAND_END '), 'EXECUTION_ENVELOPE')
    start = json.loads(lines[0].removeprefix('UC_COMMAND_START '))
    end = json.loads(lines[-1].removeprefix('UC_COMMAND_END '))
    require(all(start[k] == record[k] for k in ('id', 'argv', 'cwd', 'started_at',
                                                'candidate_sha256', 'bindings')),
            'EXECUTION_START')
    require(all(start.get(key) == record.get(key) for key in
                ('platform', 'role', 'round', 'device', 'execution_device')),
            'EXECUTION_START')
    if record.get('platform') in DEVICE_PLATFORMS:
        require(all(key in start and key in record for key in
                    ('platform', 'role', 'round', 'device')), 'EXECUTION_START')
    require(all(end[k] == record[k] for k in ('id', 'exit_code', 'ended_at',
                                              'skipped', 'cancelled', 'timed_out')),
            'EXECUTION_END')


DEVICE_PLATFORMS = {'app_android': 'app_android', 'android': 'app_android',
                    'app_macos': 'app_macos', 'macOS': 'app_macos',
                    'admin_browser': 'admin_browser', 'browser': 'admin_browser',
                    'admin': 'admin_browser'}


def selected_device(receipt, platform, device_id):
    argv = receipt.get('argv', [])
    selected = []
    for index, argument in enumerate(argv):
        executable = Path(argument).name
        if executable not in {'flutter', 'adb'}:
            continue
        flags = {'-d', '--device-id'} if executable == 'flutter' else {'-s'}
        for offset in range(index + 1, len(argv)):
            value = argv[offset]
            if value in flags:
                require(offset + 1 < len(argv), 'WRONG_DEVICE')
                selected.append(argv[offset + 1])
            elif any(value.startswith(flag + '=') for flag in flags):
                selected.append(value.split('=', 1)[1])
            elif value.startswith('-d' if executable == 'flutter' else '-s'):
                selected.append(value[2:])
    execution = receipt.get('execution_device')
    if execution is not None:
        require(isinstance(execution, dict)
                and DEVICE_PLATFORMS.get(execution.get('platform')) == platform,
                'WRONG_DEVICE')
        selected.append(execution.get('id'))
    require(bool(selected) and all(isinstance(value, str) and value == device_id
                and 'emulator' not in value.lower() and 'simulator' not in value.lower()
                for value in selected), 'WRONG_DEVICE')


def checked_device(receipt, expected, root):
    require(receipt.get('platform') == expected['platform'], 'WRONG_PLATFORM_ROLE')
    platform = DEVICE_PLATFORMS.get(expected['platform'])
    if platform is None:
        return
    device = receipt.get('device', {})
    require(isinstance(device, dict)
            and DEVICE_PLATFORMS.get(device.get('platform')) == platform and device.get('physical') is True
            and isinstance(device.get('id'), str) and bool(device['id'])
            and not device['id'].startswith('emulator'), 'WRONG_DEVICE')
    if 'device_id' in expected:
        require(device['id'] == expected['device_id'], 'WRONG_DEVICE')
    selected_device(receipt, platform, device['id'])
    discovery = json.loads(artifact(device['discovery'], root).read_text())
    require(device['id'] in discovery.get(platform, []), 'WRONG_DEVICE')


def passing_leaf(row, expected, records, root, candidate):
    require(expected.get('source_reviewed') is True, 'SOURCE_UNREVIEWED_PASS')
    cid = row.get('command_id')
    require(cid in records, 'LEAF_RECEIPT_MISSING')
    receipt = records[cid]
    repo = Path(candidate['sources'][expected['execution_repo']]['repo_path']).resolve()
    permitted = (repo / expected['execution_cwd']).resolve()
    require(permitted.is_relative_to(repo) and Path(receipt['cwd']).resolve() == permitted,
            'LEAF_EXECUTION_CWD')
    require(receipt['kind'] == 'business' and receipt['status'] == 'PASS', 'SUITE_NOT_ORACLE')
    require(row['oracle'] == expected['oracle'], 'LEAF_ORACLE_MISMATCH')
    require(receipt['platform'] == expected['platform'] and receipt['role'] == expected['role'],
            'WRONG_PLATFORM_ROLE')
    checked_device(receipt, expected, root)
    assertion = json.loads(artifact(row['assertion'], root).read_text())
    require(assertion['log_sha256'] == receipt['log']['sha256'], 'ASSERTION_LOG_BINDING')
    raw = artifact(receipt['log'], root).read_text().splitlines()
    require(type(assertion['line']) is int and 1 < assertion['line'] < len(raw)
            and bool(assertion['observed']) and raw[assertion['line'] - 1] == assertion['observed'], 'ASSERTION_OBSERVATION')
    require(assertion['cell_id'] == row['cell_id'] and assertion['command_id'] == cid
            and assertion['oracle'] == row['oracle'] and assertion['result'] == 'PASS', 'ORACLE_ASSERTION')
    require(row.get('reviewer') and row.get('review_note'), 'ORACLE_REVIEW')
    return receipt['log']['sha256'], assertion['line'], assertion['observed']


def builtin_gate(gate, rec, raw):
    if gate.startswith('BE-EUNIT'):
        require(len(rec['argv']) >= 2 and rec['argv'][:2] in (['make', 'eunit'], ['make', 'eunit-local'])
                and not any(arg.startswith('t=') for arg in rec['argv'][2:]), 'GATE_COMMAND')
        require(re.search(r'All [1-9][0-9]* tests passed\.', raw), 'GATE_RESULT')
        require(not re.search(r'Failed: [1-9]|Skipped: [1-9]|cancelled', raw), 'GATE_RESULT')
        return
    commands = {'BE-COMPILE': ['make', 'compile'], 'BE-DIALYZE': ['make', 'dialyze'],
                'APP-ANALYZE': ['flutter', 'analyze'], 'APP-TEST': ['flutter', 'test']}
    commands.update({g: ['bun', 'run', g[6:].lower()] for g in GATES if g.startswith('ADMIN-')})
    if gate not in commands:
        return
    require(rec['argv'] == commands[gate], 'GATE_COMMAND')
    patterns = {'BE-COMPILE': r'(?m)^\s*(?:APP\s+imboy|ERLC\s)',
                'BE-DIALYZE': r'done', 'APP-ANALYZE': r'No issues found!',
                'APP-TEST': r'\+[1-9][0-9]*: All tests passed!',
                'ADMIN-TEST': r'(?m)^\s*[1-9][0-9]* pass\s*$'}
    if gate in patterns:
        require(re.search(patterns[gate], raw), 'GATE_RESULT')
    if gate == 'BE-DIALYZE':
        require('warnings were emitted' not in raw and 'DIALYZER BASELINE GATE' not in raw,
                'DIALYZE_RAW_REQUIRED')
    if gate == 'ADMIN-TEST':
        require(re.search(r'(?m)^\s*0 fail\s*$', raw), 'GATE_RESULT')
        require(not re.search(r'(?m)^\s*[1-9][0-9]* (?:skip|todo)\s*$', raw), 'GATE_RESULT')


def gate_journeys(spec, rec, package, records, contract, root):
    required = spec.get('required_cells')
    require(isinstance(required, list) and required
            and all(isinstance(cell, str) for cell in required)
            and len(set(required)) == len(required), 'GATE_JOURNEY_CONTRACT')
    require(isinstance(spec.get('platform'), str) and spec['platform'], 'GATE_JOURNEY_CONTRACT')
    if spec['platform'] in DEVICE_PLATFORMS:
        require(isinstance(spec.get('device_id'), str) and spec['device_id'], 'GATE_DEVICE_CONTRACT')
    checked_device(rec, spec, root)
    expected = {row['cell_id']: row for row in contract['leaves']}
    actual = {row['cell_id']: row for row in package['leaf_rows']}
    for cell in required:
        require(cell in expected and expected[cell]['required_local']
                and expected[cell]['platform'] == spec['platform'], 'GATE_JOURNEY_CONTRACT')
        require(cell in actual and actual[cell]['status'] == 'PASS', 'GATE_JOURNEY_MISSING')
        journey = records[actual[cell]['command_id']]
        checked_device(journey, spec, root)
        require(journey.get('round') == spec['round']
                and time_value(rec['started_at']) <= time_value(journey['started_at'])
                and time_value(journey['ended_at']) <= time_value(rec['ended_at']),
                'GATE_JOURNEY_BINDING')


def final_gates(package, records, contract, candidate, root):
    gates, specs = package['quality_gates'], contract['final_gates']
    require(GATES <= specs.keys() and set(gates) == set(specs), 'FINAL_GATES_MISSING')
    require({'integration', 'recovery'} <= {s['category'] for s in specs.values()},
            'FINAL_BUSINESS_GATES_MISSING')
    passed = [entry['command_id'] for entry in gates.values() if entry['status'] == 'PASS']
    require(len(set(passed)) == len(passed) and all(cid in records for cid in passed),
            'GATE_RECEIPT_REUSE_OR_MISSING')
    for gate, entry in gates.items():
        require(entry['status'] in {'PASS', 'FAIL', 'NOT_RUN', 'BLOCKED_RESOURCE'}, 'GATE_STATUS')
        if entry['status'] != 'PASS':
            require(entry.get('reason'), 'GATE_REASON')
            continue
        spec = specs[gate]
        require(spec.get('execution_parameters_status') == 'FROZEN', 'GATE_PARAMETERS_NOT_FROZEN')
        cid = entry['command_id']
        rec = records[cid]
        require(rec['kind'] == 'quality_gate' and rec['status'] == 'PASS', 'GATE_KIND')
        require(rec['argv'] == spec['argv'] and rec['gate_id'] == gate
                and rec['round'] == spec['round'], 'GATE_COMMAND')
        require(Path(rec['cwd']).resolve() == Path(candidate['sources'][spec['repo']]['repo_path']).resolve(),
                'GATE_CWD')
        if spec['category'] in {'integration', 'recovery'}:
            gate_journeys(spec, rec, package, records, contract, root)
        if gate == 'BE-DIALYZE':
            require(rec['argv'] == ['make', 'dialyze'], 'DIALYZE_RAW_REQUIRED')
        text = '\n'.join(artifact(rec['log']).read_text().splitlines()[1:-1])
        builtin_gate(gate, rec, text)
        require(spec['success_pattern'] and re.search(spec['success_pattern'], text), 'GATE_RESULT')
    if all(gates[g]['status'] == 'PASS' for g in ('BE-EUNIT-R1', 'BE-EUNIT-R2')):
        a, b = (records[gates[g]['command_id']] for g in ('BE-EUNIT-R1', 'BE-EUNIT-R2'))
        require(a['bindings'] == b['bindings'] and a['candidate_sha256'] == b['candidate_sha256']
                and time_value(a['ended_at']) <= time_value(b['started_at']), 'EUNIT_ROUND_BINDING')


PROBES = {'missing_leaf': {'REQ_LEAF_MISSING'}, 'extra_leaf': {'REQ_LEAF_EXTRA'},
          'duplicate_leaf': {'REQ_LEAF_DUPLICATE'},
          'wrong_sha': {'CELL_SHA_MISMATCH', 'ARTIFACT_SHA_MISMATCH'},
          'log_reuse': {'LOG_REUSE', 'OBSERVATION_REUSE'},
          'nonzero_gate_marked_pass': {'GATE_NONZERO_NOT_PASS'},
          'parent_pass_without_children': {'PARENT_CHILD_INCONSISTENT'},
          'wrong_device': {'WRONG_DEVICE', 'WRONG_PLATFORM_ROLE'},
          'dirty_unbound': {'DIRTY_UNBOUND'}, 'foreign_wip_mixed': {'FOREIGN_WIP'}}


def independent_review(review, package, contract_sha, candidate_sha, records):
    require(isinstance(review, dict) and review.get('decision') == 'APPROVE'
            and review.get('reviewer') and review.get('independent') is True, 'INDEPENDENT_REVIEW_REQUIRED')
    content = json.dumps(package, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()
    require(review['package_canonical_sha256'] == hashlib.sha256(content).hexdigest()
            and review['contract_sha256'] == contract_sha
            and review['candidate_sha256'] == candidate_sha, 'REVIEW_BINDING')
    require(review['unresolved_p0_p1'] == [], 'UNRESOLVED_P0_P1')
    probes = review['negative_probes']
    require(set(probes) == set(PROBES), 'NEGATIVE_PROBE_SET')
    for name, probe in probes.items():
        require(probe['expected_rule'] in PROBES[name], 'NEGATIVE_PROBE_RULE')
        require(probe['command_id'] in records, 'NEGATIVE_PROBE_RECEIPT')
        rec = records[probe['command_id']]
        require(rec['kind'] == 'negative_probe' and rec['status'] == 'PASS'
                and probe['rejected_rule'] == probe['expected_rule'] and probe['single_fault'] is True,
                'NEGATIVE_PROBE_NOT_REJECTED')
        raw = artifact(rec['log']).read_text()
        require(f"UC_PROBE {name} {probe['expected_rule']}" in raw, 'NEGATIVE_PROBE_LOG')


def synchronized_ledgers(package, root):
    fields = (*KEY, 'cell_id', 'status', 'oracle')
    wanted = {r['cell_id']: tuple(r[k] for k in fields) for r in package['leaf_rows']}
    for key, relative in (('requirements', 'control/requirements.tsv'), ('ledger', 'final/leaf-receipts.tsv')):
        path = artifact(package[key], root)
        require(path == (root / relative).resolve(), 'LEDGER_PATH')
        with path.open() as stream:
            rows = list(csv.DictReader(stream, delimiter='\t'))
        require(len(rows) == len(wanted) and len({r['cell_id'] for r in rows}) == len(rows), 'LEDGER_SET')
        require({r['cell_id']: tuple(r[k] for k in fields) for r in rows} == wanted, 'LEDGER_DIVERGENCE')


def validate(package, contract, candidate, root, contract_sha, candidate_sha, mode='PARTIAL', review=None):
    require(all(isinstance(x, dict) for x in (package, contract, candidate)), 'INPUT_OBJECT')
    require(package.get('schema_version') == 1, 'PACKAGE_SCHEMA')
    require(package['contract_sha256'] == contract_sha, 'CONTRACT_SHA_MISMATCH')
    require(package['candidate_sha256'] == candidate_sha, 'CANDIDATE_SHA_MISMATCH')
    expected = source_contract(contract)
    actual = checked_rows(package['leaf_rows'])
    require(not expected.keys() - actual.keys(), 'REQ_LEAF_MISSING')
    require(not actual.keys() - expected.keys(), 'REQ_LEAF_EXTRA')
    candidate_snapshot(candidate, contract)
    records, hashes, paths = {}, set(), set()
    for rec in package['command_receipts']:
        require(rec['id'] not in records, 'RECEIPT_DUPLICATE')
        require(rec['log']['sha256'] not in hashes and rec['log']['path'] not in paths, 'LOG_REUSE')
        command_record(rec, candidate_sha, candidate, root)
        records[rec['id']] = rec
        hashes.add(rec['log']['sha256']); paths.add(rec['log']['path'])
    parents = defaultdict(list)
    observations, observed_lines = set(), set()
    for key, row in actual.items():
        require(row['status'] in STATES, 'LEAF_STATUS')
        if row['status'] == 'PASS':
            log_sha, line, observed = passing_leaf(row, expected[key], records, root, candidate)
            require((log_sha, line) not in observed_lines
                    and (log_sha, observed) not in observations, 'OBSERVATION_REUSE')
            observed_lines.add((log_sha, line))
            observations.add((log_sha, observed))
        else:
            require(row.get('reason') and row.get('next_unlock'), 'LEAF_REASON')
        require(row['status'] != 'DEFERRED_BY_USER' or expected[key].get('disposition') == 'deferred',
                'UNAUTHORIZED_DEFER')
        parents[json.dumps(key[:2], ensure_ascii=False)].append(row['status'])
    calculated = {p: 'PASS' if all(s == 'PASS' for s in states) else 'PARTIAL'
                  for p, states in parents.items()}
    require(package['parent_status'] == calculated, 'PARENT_CHILD_INCONSISTENT')
    final_gates(package, records, contract, candidate, root)
    synchronized_ledgers(package, root)
    source_review = contract['source_review']
    require(source_review['status'] in {'COMPLETE', 'INCOMPLETE'} and source_review.get('reviewer'), 'SOURCE_REVIEW')
    if source_review['status'] == 'INCOMPLETE':
        require(source_review.get('blockers'), 'SOURCE_REVIEW_BLOCKERS')
    if mode == 'FULL':
        require(source_review['status'] == 'COMPLETE', 'SOURCE_SET_INCOMPLETE')
        require(all(actual[k]['status'] == 'PASS' for k in expected if expected[k]['required_local']),
                'REQUIRED_NOT_PASS')
        require(all(g['status'] == 'PASS' for g in package['quality_gates'].values()), 'FINAL_GATE_NOT_PASS')
        require(package['overall'] == 'UNIFIED_LOCAL_CANDIDATE_PASS', 'FULL_VERDICT')
        independent_review(review, package, contract_sha, candidate_sha, records)
    else:
        require(package['overall'] in {'PARTIAL', 'BLOCKED'}, 'PARTIAL_NOT_FUNCTIONAL_PASS')
    return {'integrity': 'PASS', 'overall': package['overall'], 'source_coverage': source_review['status'],
            'known_leaves': len(actual), 'pass_leaves': sum(r['status'] == 'PASS' for r in actual.values())}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('package', 'contract', 'candidate', 'contract-sha256', 'candidate-sha256'):
        parser.add_argument('--' + name, required=True)
    parser.add_argument('--mode', choices=('PARTIAL', 'FULL'), default='PARTIAL')
    parser.add_argument('--review')
    parser.add_argument('--review-sha256')
    args = parser.parse_args()
    try:
        require(digest(Path(args.contract)) == args.contract_sha256, 'CONTRACT_SHA_MISMATCH')
        require(digest(Path(args.candidate)) == args.candidate_sha256, 'CANDIDATE_SHA_MISMATCH')
        review = None
        if args.review:
            require(digest(Path(args.review)) == args.review_sha256, 'REVIEW_SHA_MISMATCH')
            review = json.loads(Path(args.review).read_text())
        data = [json.loads(Path(p).read_text()) for p in (args.package, args.contract, args.candidate)]
        result = validate(*data, Path(args.package).resolve().parent.parent,
                          args.contract_sha256, args.candidate_sha256, args.mode, review)
        print(json.dumps(result, ensure_ascii=False))
        return 0
    except (ValueError, KeyError, TypeError, AttributeError, IndexError, OSError, subprocess.SubprocessError) as exc:
        print(json.dumps({'integrity': 'FAIL', 'rule': str(exc)}, ensure_ascii=False))
        return 1


if __name__ == '__main__':
    sys.exit(main())
