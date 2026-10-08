#!/usr/bin/env python3
"""Read-only NO_FULL_EUNIT micro-community evidence integrity verifier.

Input contract (all evidence paths relative to --run-root):
* acceptance.tsv: id/status/candidate_manifest_sha256/plan_sha256/policy_sha256/
  command_id/oracle/evidence_path/evidence_sha256/reviewer/blocker; exact MC-AC-01..26.
* candidate-manifest.json: plan_sha256, policy_sha256, execution_policy,
  protected_wip_sha256, sources={backend,admin,app:{repo_path,head,tree,
  diff_sha256,untracked:{relative_path:sha256}}}, device={targetPlatform:'android',isPhysicalDevice:true,
  resource_id:<nonempty>}. External expected candidate hash binds the entire file.
* commands.jsonl: unique id, argv (nonempty list), cwd, exit_code (integer),
  test_count (positive integer for tests), log_path, log_sha256,
  candidate_manifest_sha256. Records attest execution; they do not execute commands.
* quality-gates.json: backend_full_eunit='NOT_EXECUTED_BY_USER_POLICY',
  gates={backend_compile,admin_test,admin_lint,admin_typecheck,admin_deadcode,
  admin_build,app_analyze,app_test:<command id>}, backend_scoped_rounds=[[],[]]
  containing one command per suite in focused-suites.txt, each command.suite matching
  argv's t=<suite>. No empty whitelist, no wildcard or hidden full-suite invocation.
* protected-wip.sha256 and current-protected-wip.sha256: identical frozen bytes.
  protected-wip.json: list of {path:absolute baseline file,sha256,repo:backend/admin/app};
  manifest.protected_wip_manifest_sha256 binds this list; sources also carry
  baseline_repo_path and baseline_head. Actual baseline bytes must match, and
  candidate diffs/untracked paths must exclude protected paths. No implicit handoff.
* device.discovery_command_id points to the recorded flutter devices --machine
  command in App cwd; its raw output must equal the discovery JSON.
  device.discovery_path/discovery_sha256: raw `flutter devices --machine` JSON;
  selected id must be supported android-* with emulator=false. Discovery evidence
  is integrity checked, not cryptographically attested hardware identity.
* Command raw logs start with MC_COMMAND_START <JSON id/argv/cwd/started_at> and
  end with MC_COMMAND_END <JSON id/exit_code/ended_at>, captured by the recorder.
  commands also carry matching timezone-aware started_at/ended_at. Test counts
  are parsed from EUnit/bun/flutter summaries, quiet gates require canonical runner
  output and zero-exit envelope. Unknown output formats fail closed. Each scoped
  round uses distinct log paths/hashes and non-overlapping chronological execution.
  Gates accept only make compile, bun run test/lint/typecheck/deadcode/build,
  flutter analyze/test in their respective frozen repository roots.
Recorder logs and discovery files can be manually forged: these checks validate
structure and consistency only, not trusted external execution or hardware
attestation. Independent reviewers must establish provenance and business oracle
semantics separately; INTEGRITY_PASS cannot establish business PASS.
Every PASS row needs a successful command, nonblank oracle, reviewer and hashed
nonempty evidence. MC-AC-25 remains PARTIAL with a blocker; overall PARTIAL or
SCOPED_IMPLEMENTATION_PASS only. Integrity approval is never business acceptance.
Reuse digest from the existing verifier; no DB, shell execution or generic framework.
"""
import argparse
import csv
import json
from pathlib import Path
import re
import sys
import hashlib
import subprocess
from datetime import datetime

from verify_non_oa_acceptance import digest

IDS = {f'MC-AC-{number:02}' for number in range(1, 27)}
GATES = {'backend_compile', 'admin_test', 'admin_lint', 'admin_typecheck',
         'admin_deadcode', 'admin_build', 'app_analyze', 'app_test'}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def evidence(run, name, sha):
    path = (run / name).resolve()
    require(path.is_relative_to(run.resolve()), 'evidence escapes run root')
    require(path.is_file() and path.stat().st_size > 0, 'missing/empty evidence')
    require(digest(path) == sha, 'evidence hash mismatch')


def command_record(run, command, candidate):
    require(command['candidate_manifest_sha256'] == candidate, 'command candidate drift')
    argv = command['argv']
    require(isinstance(argv, list) and argv and all(isinstance(a, str) and a.strip() for a in argv), 'empty argv')
    require(command['cwd'].strip(), 'empty command cwd')
    require(type(command['exit_code']) is int and command['exit_code'] == 0, 'nonzero exit')
    require(not command.get('cancelled', False) and not command.get('timed_out', False)
            and not command.get('skipped', False), 'cancel/timeout/skip')
    require(not any(a in {'check', '*'} for a in argv), 'indirect full EUnit')
    if any(a in {'eunit', 'eunit-local'} for a in argv):
        require(argv[0] == 'make' and len(argv) == 3 and re.fullmatch(r't=[A-Za-z0-9_]+_tests', argv[2]), 'full EUnit forbidden')
    evidence(run, command['log_path'], command['log_sha256'])
    parse_execution_log(run, command)


def parse_execution_log(run, command):
    """Require recorder envelopes; raw runner summaries remain mandatory for tests."""
    lines = (run / command['log_path']).read_text().splitlines()
    require(len(lines) >= 2 and lines[0].startswith('MC_COMMAND_START ')
            and lines[-1].startswith('MC_COMMAND_END '), 'missing execution recorder envelope')
    start = json.loads(lines[0][17:])
    end = json.loads(lines[-1][15:])
    require(start['id'] == end['id'] == command['id']
            and start['argv'] == command['argv'] and start['cwd'] == command['cwd'], 'execution envelope identity')
    require(end['exit_code'] == command['exit_code'] == 0, 'execution envelope exit')
    require(start['started_at'] == command['started_at']
            and end['ended_at'] == command['ended_at'], 'execution envelope time')
    begin, finish = datetime.fromisoformat(start['started_at']), datetime.fromisoformat(end['ended_at'])
    require(begin.tzinfo is not None and finish.tzinfo is not None and finish > begin, 'execution timestamps')
    raw = re.sub(r'\x1b\[[0-9;]*[a-zA-Z]', '', '\n'.join(lines[1:-1]))
    require(not re.search(r'(?im)\bFAILED\b|\bFAILURE\b|\bERROR(?: TS\d+|:| •)|'
                          r'\bskipped\b|\bcancelled\b|\btimed out\b|'
                          r'\b[1-9]\d* (?:fail|skip|error)s?\b|\bexit [1-9]\d*\b|'
                          r'\bsyntax error\b|make(?:\[\d+\])?: \*\*\*|\berlc:|'
                          r'\btests? aborted\b|\bbuild failed\b', raw),
            'runner failure/skip/cancel log')
    return raw


def quality_result(run, command, gate, sources):
    repo_name = 'backend' if gate == 'backend_compile' else ('admin' if gate.startswith('admin_') else 'app')
    require(Path(command['cwd']).resolve() == Path(sources[repo_name]['repo_path']).resolve(), 'quality repository cwd')
    expected = {'backend_compile': ['make', 'compile'], 'app_analyze': ['flutter', 'analyze'],
                'app_test': ['flutter', 'test']}.get(gate)
    if gate.startswith('admin_'):
        expected = ['bun', 'run', gate.removeprefix('admin_')]
        scripts = json.loads((Path(command['cwd']) / 'package.json').read_text())['scripts']
        canonical = {'test': 'bun test --isolate', 'lint': 'eslint .',
                     'typecheck': 'tsc --noEmit -p tsconfig.app.json', 'deadcode': 'knip',
                     'build': 'tsc -b && vite build'}
        require(scripts[gate.removeprefix('admin_')] == canonical[gate.removeprefix('admin_')], 'unsupported quality script')
    require(command['argv'] == expected, 'quality gate argv')
    raw = parse_execution_log(run, command)
    if gate == 'admin_test':
        match = re.search(r'(?m)^\s*(\d+) pass\s*$', raw)
        require(match is not None and int(match[1]) > 0, 'missing bun test result')
        require(re.search(r'(?m)^\s*0 fail\s*$', raw), 'missing bun zero failures')
        count = int(match[1])
    elif gate == 'app_test':
        match = re.search(r'\+(\d+)(?:[^\n]*?): All tests passed!', raw)
        require(match is not None and int(match[1]) > 0, 'missing flutter test result')
        count = int(match[1])
    else:
        patterns = {'backend_compile': r'(?m)^(?:ERLC|APP)\s|Nothing to be done for .compile.',
                    'admin_lint': r'(?m)^\$ eslint \.$',
                    'admin_typecheck': r'(?m)^\$ tsc --noEmit -p tsconfig\.app\.json$',
                    'admin_deadcode': r'(?m)^\$ knip$',
                    'admin_build': r'\d+ modules transformed\.[\s\S]*built in ',
                    'app_analyze': r'No issues found!'}
        require(re.search(patterns[gate], raw), 'unsupported/missing quality runner result')
        return
    require(type(command['test_count']) is int and command['test_count'] == count, 'runner test count mismatch')


def scoped_result(run, record, repo):
    require(Path(record['cwd']).resolve() == Path(repo).resolve(), 'suite repository cwd')
    raw = parse_execution_log(run, record)
    match = re.search(r'All (\d+) tests passed\.', raw)
    count = int(match[1]) if match else (1 if re.search(r'(?m)^\s*Test passed\.$', raw) else 0)
    require(count > 0 and type(record['test_count']) is int and record['test_count'] == count, 'missing EUnit runner result/count')


def verify_protected_wip(run, manifest):
    path = run / 'protected-wip.json'
    require(digest(path) == manifest['protected_wip_manifest_sha256'], 'protected WIP manifest drift')
    protected = json.loads(path.read_text())
    require(isinstance(protected, list), 'protected WIP list')
    for item in protected:
        actual = Path(item['path']).resolve()
        require(actual.is_file() and digest(actual) == item['sha256'], 'actual protected WIP drift')
        source = manifest['sources'][item['repo']]
        origin = Path(source['baseline_repo_path']).resolve()
        require(actual.is_relative_to(origin), 'protected WIP baseline path')
        relative = actual.relative_to(origin).as_posix()
        repo = Path(source['repo_path']).resolve()
        changed = subprocess.check_output(['git', '-C', str(repo), 'diff', '--name-only', '-z',
                                           source['baseline_head']]).decode().split('\0')
        require(relative not in changed and relative not in source['untracked'], 'protected WIP mixed into candidate')


def verify_device(run, device, commands, sources):
    require(device['targetPlatform'] == 'android' and device['isPhysicalDevice'] is True
            and device['resource_id'].strip(), 'Android physical device required')
    evidence(run, device['discovery_path'], device['discovery_sha256'])
    command = commands[device['discovery_command_id']]
    require(command['argv'] == ['flutter', 'devices', '--machine']
            and Path(command['cwd']).resolve() == Path(sources['app']['repo_path']).resolve(), 'device discovery command')
    raw_discovery = (run / device['discovery_path']).read_text()
    require(json.loads(parse_execution_log(run, command)) == json.loads(raw_discovery), 'device discovery command output mismatch')
    discovered = json.loads(raw_discovery)
    require(isinstance(discovered, list), 'unsupported device discovery format')
    matching = [d for d in discovered if d.get('id') == device['resource_id']]
    require(len(matching) == 1, 'device discovery identity')
    actual = matching[0]
    require(actual.get('targetPlatform', '').startswith('android-')
            and actual.get('isSupported') is True and actual.get('emulator') is False,
            'discovery is not supported physical Android')


def verify_sources(sources):
    require(set(sources) == {'backend', 'admin', 'app'}, 'source repository set')
    for source in sources.values():
        require(all(re.fullmatch(r'[0-9a-f]{40}', source[key]) for key in ('head', 'tree', 'baseline_head')), 'invalid source git SHA')
        repo = Path(source['repo_path']).resolve()
        def git(*args):
            return subprocess.check_output(['git', '-C', str(repo), *args])
        require(Path(git('rev-parse', '--show-toplevel').decode().strip()).resolve() == repo, 'repository root mismatch')
        require(git('rev-parse', 'HEAD').decode().strip() == source['head'], 'source HEAD drift')
        require(git('rev-parse', 'HEAD^{tree}').decode().strip() == source['tree'], 'source tree drift')
        require(hashlib.sha256(git('diff', '--binary', 'HEAD')).hexdigest() == source['diff_sha256'], 'source patch drift')
        paths = git('ls-files', '--others', '--exclude-standard', '-z').decode().split('\0')
        actual = {name: digest(repo / name) for name in paths if name}
        require(actual == source['untracked'], 'source untracked/WIP drift')


def verify(run, plan, policy, candidate):
    manifest_path = run / 'candidate-manifest.json'
    require(digest(manifest_path) == candidate, 'candidate drift')
    manifest = json.loads(manifest_path.read_text())
    plan_sha, policy_sha = digest(plan), digest(policy)
    require(manifest['plan_sha256'] == plan_sha, 'plan drift')
    require(manifest['policy_sha256'] == policy_sha, 'policy drift')
    require(manifest['execution_policy'] == 'NO_FULL_EUNIT', 'wrong policy')
    original, current = run / 'protected-wip.sha256', run / 'current-protected-wip.sha256'
    require(original.read_bytes() == current.read_bytes(), 'protected WIP drift')
    require(digest(original) == manifest['protected_wip_sha256'], 'WIP binding drift')
    verify_sources(manifest['sources'])
    verify_protected_wip(run, manifest)
    commands = [json.loads(line) for line in (run / 'commands.jsonl').read_text().splitlines() if line.strip()]
    require(commands and len({c['id'] for c in commands}) == len(commands), 'duplicate/empty command IDs')
    indexed = {c['id']: c for c in commands}
    for command in commands:
        command_record(run, command, candidate)
    verify_device(run, manifest['device'], indexed, manifest['sources'])
    with (run / 'acceptance.tsv').open() as ledger:
        rows = list(csv.DictReader(ledger, delimiter='\t'))
    require(len(rows) == 26 and {r['id'] for r in rows} == IDS, 'exact ID set')
    for row in rows:
        require(row['candidate_manifest_sha256'] == candidate, 'row candidate drift')
        require(row['plan_sha256'] == plan_sha and row['policy_sha256'] == policy_sha, 'row plan/policy drift')
        require(row['status'] in {'PASS', 'PARTIAL', 'BLOCKED', 'NOT_EXECUTED'}, 'unsupported status')
        if row['id'] == 'MC-AC-25':
            require(row['status'] == 'PARTIAL' and row['blocker'].strip(), 'MC-AC-25 must remain PARTIAL')
        if row['status'] == 'PASS':
            require(row['oracle'].strip() and row['reviewer'].strip(), 'false PASS empty oracle/reviewer')
            require(row['command_id'] in indexed, 'missing PASS command')
            evidence(run, row['evidence_path'], row['evidence_sha256'])
    quality = json.loads((run / 'quality-gates.json').read_text())
    require(quality['backend_full_eunit'] == 'NOT_EXECUTED_BY_USER_POLICY', 'false full EUnit claim')
    require(set(quality['gates']) == GATES, 'quality gate set')
    for gate, command_id in quality['gates'].items():
        require(command_id in indexed, 'missing quality command')
        quality_result(run, indexed[command_id], gate, manifest['sources'])
    suites = (run / 'focused-suites.txt').read_text().splitlines()
    require(suites and len(set(suites)) == len(suites)
            and all(re.fullmatch(r'[A-Za-z0-9_]+_tests', s) for s in suites), 'invalid whitelist')
    rounds = quality['backend_scoped_rounds']
    require(len(rounds) == 2, 'two rounds required')
    require(len(set(rounds[0] + rounds[1])) == 2 * len(suites), 'round commands reused')
    round_records = [indexed[c] for ids in rounds for c in ids]
    require(len({c['log_path'] for c in round_records}) == len(round_records)
            and len({c['log_sha256'] for c in round_records}) == len(round_records), 'round log reuse')
    previous_end = None
    for round_ids in rounds:
        records = [indexed[c] for c in round_ids]
        require(len(records) == len(suites) and {c['suite'] for c in records} == set(suites), 'round whitelist mismatch')
        for record in records:
            require(record['argv'] in [['make', 'eunit', 't=' + record['suite']],
                    ['make', 'eunit-local', 't=' + record['suite']]], 'suite argv mismatch')
            scoped_result(run, record, manifest['sources']['backend']['repo_path'])
            begin = datetime.fromisoformat(record['started_at'])
            end = datetime.fromisoformat(record['ended_at'])
            require(previous_end is None or begin >= previous_end, 'suite rounds overlap/reuse time')
            previous_end = end
    state = json.loads((run / 'state.json').read_text())
    require(state['status'] in {'PARTIAL', 'SCOPED_IMPLEMENTATION_PASS'}, 'forbidden completion state')
    require(state['acceptance_ids'] == {r['id']: r['status'] for r in rows}, 'state ID drift')
    if state['status'] == 'SCOPED_IMPLEMENTATION_PASS':
        require(all(r['status'] == 'PASS' for r in rows if r['id'] != 'MC-AC-25'), 'false scoped completion')
    return state['status']


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-root', type=Path, required=True)
    parser.add_argument('--plan-path', type=Path, required=True)
    parser.add_argument('--policy-path', type=Path, required=True)
    parser.add_argument('--expected-candidate-sha256', required=True)
    args = parser.parse_args()
    result = verify(args.run_root, args.plan_path, args.policy_path, args.expected_candidate_sha256)
    print(f'INTEGRITY_PASS: 26 IDs; {result}; full EUnit not executed; business acceptance not implied')
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (OSError, ValueError, KeyError, TypeError, AttributeError, subprocess.SubprocessError) as error:
        print(f'INTEGRITY_FAIL: {error}', file=sys.stderr)
        sys.exit(1)
