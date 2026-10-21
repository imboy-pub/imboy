#!/usr/bin/env python3
"""Read-only structural checks; does not prove runtime locks or business acceptance."""
import argparse
import hashlib
import json
import re
from datetime import datetime, timezone
from pathlib import Path

ACTIVE = {'claimed', 'running'}


def validate(state, contract, manifest, root, check_files=True):
    errors = []
    tasks = state.get('tasks', [])
    by_id = {t['id']: t for t in tasks}
    if len(by_id) != len(tasks) or set(by_id) != set(contract['required_tasks']):
        errors.append('task IDs missing, duplicated or unexpected')
    if state.get('plan_id') != contract['plan_id']:
        errors.append('plan identity mismatch')
    if str(state.get('plan_revision')) != str(contract['revision']):
        errors.append('contract revision mismatch')
    if str(manifest.get('revision')) != str(contract['revision']):
        errors.append('manifest revision mismatch')
    if state.get('max_parallel_workers') != contract['max_parallel_workers']:
        errors.append('parallel limit mismatch')
    for field in ('state_version', 'controller_epoch'):
        if type(state.get(field)) is not int or state[field] < 0:
            errors.append(f'invalid {field}')
    expected_files = set(contract['manifest_required_files'])
    if set(manifest.get('files', {})) != expected_files:
        errors.append('manifest required file set incomplete or unexpected')
    if any(not isinstance(h, str) or not re.fullmatch(r'[0-9a-f]{64}', h)
           for h in manifest.get('files', {}).values()):
        errors.append('invalid manifest hash')
    reached = set()
    while len(reached) < len(by_id):
        ready = {k for k, t in by_id.items() if k not in reached
                 and set(t.get('dependencies', [])) <= reached}
        if not ready:
            errors.append('development dependency graph has cycle or missing ID')
            break
        reached.update(ready)
    active = [t for t in tasks if t.get('status') in ACTIVE]
    capacity = state.get('runtime_capacity')
    if active and (type(capacity) is not int or capacity < 1):
        errors.append('active tasks require observed runtime capacity')
    limit = min(8, capacity) if type(capacity) is int and capacity > 0 else 0
    for permission, granted in state.get('permissions', {}).items():
        if granted and not state.get('permission_grants', {}).get(permission, {}).get('trusted_source'):
            errors.append(f'permission lacks trusted grant: {permission}')
    if active:
        for field in ('design_approval', 'implementation_authorization'):
            approval = state.get(field, {})
            if approval.get('status') != 'approved' or not approval.get('trusted_source'):
                errors.append(f'active work lacks trusted {field}')
    if len(active) > limit:
        errors.append('active tasks exceed capacity')
    for t in tasks:
        errors.extend(validate_task(t, by_id, state, contract, check_files))
    locks = [v for t in active for v in t.get('resource_locks', [])]
    if len(locks) != len(set(locks)):
        errors.append('active tasks share exclusive resource lock')
    paths = [(t['id'], str(Path(v).resolve())) for t in active for v in t.get('allowed_paths', [])]
    if len([p for _, p in paths]) != len(set(p for _, p in paths)):
        errors.append('active tasks share owned path')
    for owner_id, a in paths:
        if any(owner_id != other_id and a != b and Path(a) in Path(b).parents
               for other_id, b in paths):
            errors.append('active tasks overlap directory ownership')
            break
    if state.get('status') == 'COMPLETE' and any(t['status'] != 'accepted' for t in tasks):
        errors.append('COMPLETE contains unaccepted tasks')
    if state.get('status') == 'COMPLETE' and not state.get('final_candidate'):
        errors.append('COMPLETE lacks final candidate binding')
    if check_files:
        for name, digest in manifest.get('files', {}).items():
            path = (root / name).resolve()
            if not path.is_relative_to(root.resolve()) or not path.is_file():
                errors.append(f'invalid manifest path: {name}')
            elif hashlib.sha256(path.read_bytes()).hexdigest() != digest:
                errors.append(f'plan file drift: {name}')
    return errors


def validate_task(t, by_id, state, contract, check_files):
    errors, task_id = [], t['id']
    status = t.get('status')
    for field, expected_value in contract['task_definitions'].get(task_id, {}).items():
        if t.get(field) != expected_value:
            errors.append(f'{task_id}: immutable task definition drift: {field}')
    if status not in contract['allowed_statuses']:
        errors.append(f'{task_id}: unknown status')
    if status == 'component_ready' and task_id not in contract['component_tasks']:
        errors.append(f'{task_id}: component_ready not allowed')
    expected = contract['component_tasks'].get(task_id, [])
    if t.get('acceptance_dependencies', []) != expected:
        errors.append(f'{task_id}: acceptance dependencies changed')
    if status in ACTIVE or status in {'review', 'component_ready', 'accepted'}:
        for dep in t.get('dependencies', []):
            acceptable = {'accepted'}
            if status != 'accepted' and dep in t.get('allow_component_dependencies', []):
                acceptable.add('component_ready')
            if by_id.get(dep, {}).get('status') not in acceptable:
                errors.append(f'{task_id}: dependency not ready: {dep}')
    if status in ACTIVE:
        if any(any(ch in str(p) for ch in '*?[') for p in t.get('allowed_paths', [])):
            errors.append(f'{task_id}: claim paths must be expanded concrete paths')
        try:
            lease = datetime.fromisoformat(str(t.get('lease_until')).replace('Z', '+00:00'))
            if lease.tzinfo is None or lease <= datetime.now(timezone.utc):
                errors.append(f'{task_id}: expired or timezone-free lease')
        except ValueError:
            errors.append(f'{task_id}: malformed lease')
        if t.get('controller_epoch') != state.get('controller_epoch'):
            errors.append(f'{task_id}: stale controller epoch')
        if not all(t.get(k) for k in ('owner', 'allowed_paths', 'worktree', 'lease_until')):
            errors.append(f'{task_id}: incomplete claim')
    if status == 'component_ready':
        if not all(t.get(k) for k in ('owner', 'candidate', 'base_sha', 'evidence')):
            errors.append(f'{task_id}: component candidate/evidence incomplete')
        errors.extend(validate_evidence(t, contract, check_files))
    if status == 'accepted':
        for decision in contract.get('decision_gates', {}).get(task_id, []):
            record = state.get('decisions', {}).get(decision, {})
            if record.get('status') != 'approved' or not record.get('value') or not record.get('trusted_source'):
                errors.append(f'{task_id}: unresolved decision: {decision}')
        if not all(t.get(k) for k in contract['acceptance_required_fields']):
            errors.append(f'{task_id}: acceptance record incomplete')
        if t.get('reviewer') == t.get('owner'):
            errors.append(f'{task_id}: independent reviewer equals owner')
        for dep in expected:
            if by_id.get(dep, {}).get('status') != 'accepted':
                errors.append(f'{task_id}: integration prerequisite not accepted: {dep}')
        errors.extend(validate_evidence(t, contract, check_files))
    return errors


def validate_evidence(t, contract, check_files):
    errors, current_pass = [], False
    required = (contract['component_checks'].get(t['id'], [])
                if t.get('status') == 'component_ready'
                else contract['required_checks'].get(t['id'], []))
    passed_checks = set()
    for e in t.get('evidence', []):
        if not isinstance(e, dict) or not all(k in e for k in contract['evidence_required_fields']):
            errors.append(f'{t["id"]}: evidence fields missing')
            continue
        if not all(e.get(k) for k in ('path', 'sha256', 'command', 'candidate', 'created_at', 'scope')):
            errors.append(f'{t["id"]}: empty evidence metadata')
        if type(e['exit_code']) is not int:
            errors.append(f'{t["id"]}: invalid evidence exit code')
        result = e['result']
        if result not in contract['evidence_results']:
            errors.append(f'{t["id"]}: unknown evidence result')
        current = e['candidate'] == t.get('candidate')
        binding = t.get('test_bindings', {}).get(e['check_id'], {})
        frozen = contract.get('frozen_test_bindings', {}).get(t['id'], {}).get(e['check_id'])
        if current and e['check_id'] in required and frozen != binding:
            errors.append(f'{t["id"]}: binding differs from frozen contract')
        if not isinstance(e['runner_sha256'], str) or not re.fullmatch(r'[0-9a-f]{64}', e['runner_sha256']):
            errors.append(f'{t["id"]}: invalid runner hash')
        bound = (frozen == binding and bool(frozen)
                 and binding.get('command') == e['command']
                 and binding.get('runner_sha256') == e['runner_sha256']
                 and binding.get('cwd') == e['cwd']
                 and binding.get('expected_exit') == e['exit_code'])
        if current and e['check_id'] in required and not bound:
            errors.append(f'{t["id"]}: check command/runner binding missing or changed')
        if current and bound and e['result'] == 'PASS' and e['exit_code'] == 0:
            passed_checks.add(e['check_id'])
        if current and result == 'PASS' and e['exit_code'] == 0:
            current_pass = True
        if current and (e.get('required', True) or e['check_id'] in required) and (result != 'PASS' or e['exit_code'] != 0):
            errors.append(f'{t["id"]}: required evidence not PASS')
        if check_files:
            path = Path(e['path'])
            if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != e['sha256']:
                errors.append(f'{t["id"]}: evidence missing or changed')
    if set(required) - passed_checks:
        errors.append(f'{t["id"]}: required check coverage missing: {sorted(set(required) - passed_checks)}')
    if not current_pass:
        errors.append(f'{t["id"]}: no current candidate PASS evidence')
    return errors


def parse_arguments(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--directory', type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument('--state', type=Path,
                        help='Read execution state from an external run directory; never creates state')
    return parser.parse_args(argv)


def main():
    args = parse_arguments()
    root = args.directory
    read = lambda n: json.loads((root / n).read_text())
    state_path = args.state if args.state is not None else root / 'execution-state.json'
    errors = validate(json.loads(state_path.read_text()), read('execution-contract.json'),
                      read('plan-manifest.json'), root)
    if errors:
        print('\n'.join(errors))
        raise SystemExit(1)
    print('PASS: plan structure, state prerequisites and available file hashes; not runtime acceptance')


if __name__ == '__main__':
    main()
