"""Synthetic tool-rule tests; these never count toward business acceptance."""
import copy
import csv
import hashlib
import json
import os
from pathlib import Path
import sys
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
import verify_unified_delivery_acceptance as V
REAL_GIT = V.git


class VerifierTest(unittest.TestCase):
    def setUp(self):
        self.prepare_environment()
        source = self.ref('source.md', 'frozen source: foreground delivery and deduplication\n')
        self.rows = []
        for sub in ('arrives', 'deduplicates'):
            row = dict(source_plan='phase1', source_id='AC23', subcase=sub,
                       platform='app_android', role='normal_user', oracle=sub,
                       required_local=True, source_reviewed=True,
                       execution_repo='app', execution_cwd='.')
            row['cell_id'] = V.cell_id(row)
            row['source_locator'] = {'source': 'phase1', 'start': 1, 'end': 1,
                                    'quote': 'frozen source: foreground delivery and deduplication'}
            self.rows.append(row)
        self.contract = {'schema_version': 2, 'plan': source,
                         'sources': [dict(source, id='phase1')], 'leaves': self.rows,
                         'source_review': {'status': 'COMPLETE', 'reviewer': 'independent-fixture'},
                         'owned_paths': {name: [] for name in ('backend', 'app', 'admin')}}
        self.contract['final_gates'] = {g: {'category': 'quality'} for g in V.GATES}
        self.contract['final_gates'].update({'NATIVE': {'category': 'integration'},
                                             'RESTORE': {'category': 'recovery'}})
        self.candidate = {'schema_version': 2, 'sources': {name: {'repo_path': str(self.root / name),
                          'head': 'a' * 40, 'tree': 'b' * 40,
                          'patch_sha256': hashlib.sha256(b'').hexdigest(), 'untracked': {},
                          'index_sha256': hashlib.sha256(b'').hexdigest(),
                          'staged_sha256': hashlib.sha256(b'').hexdigest(),
                          'unstaged_sha256': hashlib.sha256(b'').hexdigest()}
                          for name in ('backend', 'app', 'admin')},
                          'inputs': {key: [source] for key in V.BINDINGS}}
        self.contract_sha, self.candidate_sha = 'c' * 64, 'd' * 64
        self.record = {'id': 'android-1', 'kind': 'business', 'status': 'PASS',
                       'exit_code': 0, 'round': 1, 'argv': ['flutter', 'test', '-d', 'fixture-device'],
                       'cwd': str(self.root / 'app'), 'platform': 'app_android', 'role': 'normal_user',
                       'started_at': '2026-10-14T00:00:00+00:00',
                       'ended_at': '2026-10-14T00:01:00+00:00',
                       'skipped': 0, 'cancelled': 0, 'timed_out': 0,
                       'candidate_sha256': self.candidate_sha, 'bindings': self.candidate['inputs'],
                       'device': {'id': 'fixture-device', 'platform': 'app_android', 'physical': True,
                                  'discovery': self.ref('devices.json', json.dumps({'app_android': ['fixture-device']}))}}
        self.log(self.record)
        leaves = []
        for index, row in enumerate(self.rows):
            assertion = self.ref(row['subcase'] + '.json', json.dumps({
                'cell_id': row['cell_id'], 'command_id': self.record['id'],
                'oracle': row['oracle'], 'result': 'PASS', 'log_sha256': self.record['log']['sha256'],
                'line': index + 2, 'observed': 'synthetic assertion ' + row['subcase']}))
            leaves.append(dict(row, status='PASS', command_id=self.record['id'], assertion=assertion,
                               reviewer='independent-fixture', review_note='synthetic behavior only'))
        self.package = {'schema_version': 1, 'contract_sha256': self.contract_sha,
                        'candidate_sha256': self.candidate_sha, 'leaf_rows': leaves,
                        'command_receipts': [self.record], 'overall': 'PARTIAL',
                        'parent_status': {json.dumps(['phase1', 'AC23']): 'PASS'},
                        'quality_gates': {g: {'status': 'NOT_RUN', 'reason': 'synthetic'} for g in self.contract['final_gates']}}
        self.sync_ledgers()

    def prepare_environment(self):
        self.git_environment = patch.dict(os.environ, {'GIT_CONFIG_GLOBAL': os.devnull,
                                                       'GIT_CONFIG_NOSYSTEM': '1'})
        self.git_environment.start()
        self.addCleanup(self.git_environment.stop)
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.mock_git = patch.object(V, 'git', self.git)
        self.mock_git.start()
        self.addCleanup(self.mock_git.stop)

    def sync_ledgers(self):
        for key, name in [('requirements', 'control/requirements.tsv'), ('ledger', 'final/leaf-receipts.tsv')]:
            path = self.root / name
            path.parent.mkdir(exist_ok=True)
            fields = [*V.KEY, 'cell_id', 'status', 'oracle']
            with path.open('w') as f:
                writer = csv.DictWriter(f, fields, delimiter='\t', extrasaction='ignore')
                writer.writeheader()
                writer.writerows(self.package['leaf_rows'])
            self.package[key] = {'path': str(path), 'sha256': V.digest(path)}

    def git(self, repo, *args):
        if args == ('rev-parse', 'HEAD'):
            return b'a' * 40 + b'\n'
        if args == ('rev-parse', 'HEAD^{tree}'):
            return b'b' * 40 + b'\n'
        return b''

    def ref(self, name, contents):
        path = self.root / name
        path.write_text(contents)
        return {'path': str(path), 'sha256': V.digest(path)}

    def log(self, rec):
        start = {k: rec[k] for k in ('id', 'argv', 'cwd', 'started_at', 'candidate_sha256', 'bindings')}
        start.update({key: rec[key] for key in ('platform', 'role', 'round', 'device',
                                               'execution_device') if key in rec})
        end = {k: rec[k] for k in ('id', 'exit_code', 'ended_at', 'skipped', 'cancelled', 'timed_out')}
        observed = ('\n'.join('synthetic assertion ' + row['subcase'] for row in self.rows)
                    if rec['kind'] == 'business' else 'synthetic assertion')
        rec['log'] = self.ref(rec['id'] + '.log', 'UC_COMMAND_START ' + json.dumps(start)
                              + '\n' + observed + '\nUC_COMMAND_END ' + json.dumps(end) + '\n')

    def validate(self, mode='PARTIAL'):
        return V.validate(self.package, self.contract, self.candidate, self.root,
                          self.contract_sha, self.candidate_sha, mode)

    def rejected(self, rule):
        with self.assertRaisesRegex(ValueError, '^' + rule + '$'):
            self.validate()

    def test_partial_integrity_is_not_full_acceptance(self):
        self.assertEqual(self.validate()['overall'], 'PARTIAL')
        with self.assertRaisesRegex(ValueError, 'FINAL_GATE_NOT_PASS'):
            self.validate('FULL')

    def test_missing_child_preserves_parent_but_is_rejected(self):
        self.package['leaf_rows'].pop()
        self.rejected('REQ_LEAF_MISSING')

    def test_extra_leaf(self):
        row = dict(self.package['leaf_rows'][0], subcase='invented')
        row['cell_id'] = V.cell_id(row)
        self.package['leaf_rows'].append(row)
        self.rejected('REQ_LEAF_EXTRA')

    def test_duplicate_leaf(self):
        self.package['leaf_rows'].append(self.package['leaf_rows'][0])
        self.rejected('REQ_LEAF_DUPLICATE')

    def test_wrong_cell_hash(self):
        self.package['leaf_rows'][0]['cell_id'] = '0' * 64
        self.rejected('CELL_SHA_MISMATCH')

    def test_wrong_log_hash(self):
        self.record['log']['sha256'] = '0' * 64
        self.rejected('ARTIFACT_SHA_MISMATCH')

    def test_log_bytes_changed(self):
        Path(self.record['log']['path']).write_text('tampered')
        self.rejected('ARTIFACT_SHA_MISMATCH')

    def test_log_reuse(self):
        self.package['command_receipts'].append(dict(self.record, id='second'))
        self.rejected('LOG_REUSE')

    def test_same_observation_cannot_sign_two_leaves(self):
        ref = self.package['leaf_rows'][1]['assertion']
        assertion = json.loads(Path(ref['path']).read_text())
        assertion.update(line=2, observed='synthetic assertion arrives')
        Path(ref['path']).write_text(json.dumps(assertion))
        ref['sha256'] = V.digest(Path(ref['path']))
        self.rejected('OBSERVATION_REUSE')

    def test_copied_observation_on_another_line_is_rejected(self):
        path = Path(self.record['log']['path'])
        path.write_text(path.read_text().replace('synthetic assertion deduplicates',
                                                 'synthetic assertion arrives'))
        self.record['log']['sha256'] = V.digest(path)
        for row in self.package['leaf_rows']:
            ref = row['assertion']
            assertion = json.loads(Path(ref['path']).read_text())
            assertion.update(log_sha256=self.record['log']['sha256'],
                             observed='synthetic assertion arrives')
            Path(ref['path']).write_text(json.dumps(assertion))
            ref['sha256'] = V.digest(Path(ref['path']))
        self.rejected('OBSERVATION_REUSE')

    def test_distinct_observations_in_one_execution_are_allowed(self):
        self.assertEqual(self.validate()['pass_leaves'], 2)

    def test_nonzero_pass(self):
        self.record['exit_code'] = 2
        self.rejected('GATE_NONZERO_NOT_PASS')

    def test_parent_pass_with_nonpass_child(self):
        self.package['leaf_rows'][0].update(status='FAIL', reason='failed', next_unlock='fix')
        self.rejected('PARENT_CHILD_INCONSISTENT')

    def test_wrong_platform(self):
        self.record['device']['platform'] = 'app_macos'
        self.log(self.record)
        self.rejected('WRONG_DEVICE')

    def test_other_physical_device(self):
        self.record['device']['id'] = 'other-device'
        self.log(self.record)
        self.rejected('WRONG_DEVICE')

    def test_dirty_bytes_unbound(self):
        self.candidate['sources']['backend']['patch_sha256'] = '0' * 64
        self.rejected('DIRTY_UNBOUND')

    def test_foreign_wip_even_without_foreign_in_filename(self):
        repo = self.root / 'backend'
        repo.mkdir()
        (repo / 'account.erl').write_text('unrelated WIP')
        self.candidate['sources']['backend']['untracked'] = {'account.erl': V.digest(repo / 'account.erl')}
        def changed_git(path, *args):
            if path == str(repo) and args == ('ls-files', '--others', '--exclude-standard', '-z'):
                return b'account.erl\0'
            return self.git(path, *args)
        with patch.object(V, 'git', changed_git):
            self.rejected('FOREIGN_WIP')

    def test_suite_cannot_sign_business_leaf(self):
        self.record['kind'] = 'quality_gate'
        self.rejected('SUITE_NOT_ORACLE')

    def test_unreviewed_source_cannot_pass(self):
        self.rows[0]['source_reviewed'] = False
        self.rejected('SOURCE_UNREVIEWED_PASS')

    def test_incomplete_source_blocks_full(self):
        self.contract['source_review'] = {'status': 'INCOMPLETE', 'reviewer': 'reviewer',
                                          'blockers': ['recursive contract not expanded']}
        self.assertEqual(self.validate()['source_coverage'], 'INCOMPLETE')
        with self.assertRaisesRegex(ValueError, 'SOURCE_SET_INCOMPLETE'):
            self.validate('FULL')

    def test_five_field_key_not_normalized(self):
        row = dict(self.rows[0], subcase=' arrives')
        self.assertNotEqual(V.cell_id(row), self.rows[0]['cell_id'])

    def test_execution_exit_must_match_raw_envelope(self):
        self.record['exit_code'] = 1
        self.record['status'] = 'FAIL'
        self.rejected('EXECUTION_END')

    def test_wrong_candidate_binding(self):
        self.record['candidate_sha256'] = '0' * 64
        self.rejected('RECEIPT_CANDIDATE')

    def test_skip_not_pass(self):
        self.record['skipped'] = 1
        self.rejected('RECEIPT_SKIP_CANCEL')

    def test_hidden_git_flags_with_real_index(self):
        repo = self.root / 'real-index'
        subprocess.run(['git', 'init', '-q', str(repo)], check=True)
        (repo / 'source.txt').write_text('baseline')
        subprocess.run(['git', '-C', str(repo), 'add', 'source.txt'], check=True)
        with patch.object(V, 'git', REAL_GIT):
            V.visible_index(str(repo))
            for flag, undo in [('assume-unchanged', 'no-assume-unchanged'),
                               ('skip-worktree', 'no-skip-worktree')]:
                subprocess.run(['git', '-C', str(repo), 'update-index', '--' + flag, 'source.txt'], check=True)
                with self.assertRaisesRegex(ValueError, 'HIDDEN_INDEX_STATE'):
                    V.visible_index(str(repo))
                subprocess.run(['git', '-C', str(repo), 'update-index', '--' + undo, 'source.txt'], check=True)

    def real_callback_repo(self, setting):
        repo = self.root / 'callback-repo'
        subprocess.run(['git', 'init', '-q', str(repo)], check=True)
        (repo / 'source.txt').write_text('baseline\n')
        (repo / '.gitattributes').write_text('source.txt diff=callback\n')
        subprocess.run(['git', '-C', str(repo), 'add', 'source.txt', '.gitattributes'], check=True)
        subprocess.run(['git', '-c', 'core.hooksPath=/dev/null', '-C', str(repo),
                        '-c', 'user.name=leeyi', '-c', 'user.email=leeyisoft@qq.com',
                        '-c', 'commit.gpgsign=false', 'commit', '-qm', 'synthetic baseline'], check=True)
        marker = self.root / 'callback-invoked'
        helper = self.root / 'git-callback'
        helper.write_text('#!' + sys.executable + '\nfrom pathlib import Path\n'
                          + 'Path(' + repr(str(marker)) + ').write_text("invoked")\n')
        helper.chmod(0o700)
        subprocess.run(['git', '-C', str(repo), 'config', setting, str(helper)], check=True)
        (repo / 'source.txt').write_text('dirty candidate\n')
        return repo, marker

    def reject_real_callback_empty_hash(self, setting):
        repo, marker = self.real_callback_repo(setting)
        with patch.object(V, 'git', REAL_GIT):
            source = {'repo_path': str(repo),
                      'head': V.git(str(repo), 'rev-parse', 'HEAD').decode().strip(),
                      'tree': V.git(str(repo), 'rev-parse', 'HEAD^{tree}').decode().strip(),
                      'patch_sha256': hashlib.sha256(b'').hexdigest(), 'untracked': {}}
            candidate = {'schema_version': 2,
                         'sources': {name: dict(source) for name in ('backend', 'app', 'admin')}}
            with self.assertRaisesRegex(ValueError, '^DIRTY_UNBOUND$'):
                V.candidate_snapshot(candidate, {'owned_paths': {}})
            for arguments in [('diff', '--name-only', 'HEAD', '-z'),
                              ('status', '--porcelain', '--untracked-files=all'),
                              ('ls-files', '--others', '--exclude-standard', '-z')]:
                V.git(str(repo), *arguments)
        self.assertFalse(marker.exists(), 'read-only verification executed a Git callback')

    def test_external_diff_cannot_execute_or_hide_dirty_patch(self):
        self.reject_real_callback_empty_hash('diff.external')

    def test_textconv_cannot_execute_or_hide_dirty_patch(self):
        self.reject_real_callback_empty_hash('diff.callback.textconv')

    def test_fsmonitor_cannot_execute_during_candidate_or_status_reads(self):
        self.reject_real_callback_empty_hash('core.fsmonitor')

    def test_clean_filter_is_rejected_without_execution(self):
        self.reject_real_filter('clean')

    def test_process_filter_is_rejected_without_execution(self):
        self.reject_real_filter('process')

    def reject_real_filter(self, kind):
        repo, marker = self.real_callback_repo('filter.callback.' + kind)
        (repo / '.gitattributes').write_text('source.txt filter=callback\n')
        with patch.object(V, 'git', REAL_GIT):
            for arguments in [('diff', '--binary', 'HEAD'),
                              ('status', '--porcelain'), ('ls-files', '--stage', '-z')]:
                with self.assertRaisesRegex(ValueError, '^GIT_EXECUTABLE_FILTER$'):
                    V.git(str(repo), *arguments)
        self.assertFalse(marker.exists())

    def real_index_candidate(self):
        repo, marker = self.real_callback_repo('diff.external')
        (repo / 'source.txt').write_text('baseline\n')
        source = {'repo_path': str(repo), 'untracked': {}}
        for key, arguments in {
            'head': ('rev-parse', 'HEAD'), 'tree': ('rev-parse', 'HEAD^{tree}'),
            'patch_sha256': ('diff', '--binary', 'HEAD'),
            'index_sha256': ('ls-files', '--stage', '-z'),
            'staged_sha256': ('diff', '--binary', '--cached', 'HEAD'),
            'unstaged_sha256': ('diff', '--binary')}.items():
            value = REAL_GIT(str(repo), *arguments)
            source[key] = value.decode().strip() if key in {'head', 'tree'} else hashlib.sha256(value).hexdigest()
        return repo, marker, {'schema_version': 2, 'sources': {
            name: dict(source) for name in ('backend', 'app', 'admin')}}

    def test_real_clean_index_candidate_is_accepted(self):
        repo, marker, candidate = self.real_index_candidate()
        with patch.object(V, 'git', REAL_GIT):
            V.candidate_snapshot(candidate, {'owned_paths': {}})
        self.assertFalse(marker.exists())

    def test_staged_change_hidden_by_restored_worktree_is_rejected(self):
        repo, marker, candidate = self.real_index_candidate()
        (repo / 'source.txt').write_text('staged A\n')
        subprocess.run(['git', '-C', str(repo), 'add', 'source.txt'], check=True)
        (repo / 'source.txt').write_text('baseline\n')
        with patch.object(V, 'git', REAL_GIT):
            self.assertEqual(V.git(str(repo), 'diff', '--binary', 'HEAD'), b'')
            with self.assertRaisesRegex(ValueError, '^INDEX_UNBOUND$'):
                V.candidate_snapshot(candidate, {'owned_paths': {}})
            source = candidate['sources']['backend']
            for key, arguments in {
                'index_sha256': ('ls-files', '--stage', '-z'),
                'staged_sha256': ('diff', '--binary', '--cached', 'HEAD'),
                'unstaged_sha256': ('diff', '--binary')}.items():
                value = hashlib.sha256(V.git(str(repo), *arguments)).hexdigest()
                for item in candidate['sources'].values():
                    item[key] = value
            with self.assertRaisesRegex(ValueError, '^FOREIGN_WIP$'):
                V.candidate_snapshot(candidate, {'owned_paths': {}})
            V.candidate_snapshot(candidate, {'owned_paths': {
                name: ['source.txt'] for name in candidate['sources']}})
        self.assertFalse(marker.exists())

    def test_missing_index_binding_is_rejected(self):
        del self.candidate['sources']['app']['index_sha256']
        self.rejected('INDEX_UNBOUND')

    def test_full_missing_index_binding_is_rejected(self):
        review = self.full_review()
        del self.candidate['sources']['app']['index_sha256']
        with self.assertRaisesRegex(ValueError, '^INDEX_UNBOUND$'):
            V.validate(self.package, self.contract, self.candidate, self.root,
                       self.contract_sha, self.candidate_sha, 'FULL', review)

    def test_historical_candidate_schema_is_not_inherited(self):
        self.candidate['schema_version'] = 1
        self.rejected('CANDIDATE_SCHEMA')

    def test_leaf_execution_cwd_cannot_escape_repo(self):
        self.rows[0]['execution_cwd'] = '../backend'
        self.rejected('LEAF_EXECUTION_SCHEMA')

    def test_historical_contract_schema_is_not_inherited(self):
        self.contract['schema_version'] = 1
        self.rejected('CONTRACT_SCHEMA')

    def test_leaf_requires_frozen_execution_policy(self):
        del self.rows[0]['execution_repo']
        self.rejected('LEAF_EXECUTION_SCHEMA')

    def test_full_business_receipt_cannot_self_sign_foreign_cwd(self):
        review = self.full_review()
        self.record['cwd'] = str(self.root / 'backend')
        self.log(self.record)
        for row in self.package['leaf_rows']:
            assertion = json.loads(Path(row['assertion']['path']).read_text())
            assertion['log_sha256'] = self.record['log']['sha256']
            row['assertion'] = self.ref(row['subcase'] + '.json', json.dumps(assertion))
        with self.assertRaisesRegex(ValueError, '^LEAF_EXECUTION_CWD$'):
            V.validate(self.package, self.contract, self.candidate, self.root,
                       self.contract_sha, self.candidate_sha, 'FULL', review)

    def test_gate_command_and_cwd_are_not_free_form(self):
        self.record.update(kind='quality_gate', gate_id='BE-COMPILE', round=1)
        self.package['leaf_rows'] = [dict(r, status='UNVERIFIED', reason='no oracle', next_unlock='test')
                                     for r in self.package['leaf_rows']]
        self.package['parent_status'][json.dumps(['phase1', 'AC23'])] = 'PARTIAL'
        self.contract['final_gates']['BE-COMPILE'].update(argv=['make', 'compile'], repo='backend',
                                                          round=1, success_pattern='APP +imboy', execution_parameters_status='FROZEN')
        self.package['quality_gates']['BE-COMPILE'] = {'status': 'PASS', 'command_id': self.record['id']}
        self.record['argv'] = ['echo', 'ok']
        self.log(self.record)
        self.rejected('GATE_COMMAND')
        self.record['argv'] = ['make', 'compile']
        self.log(self.record)
        self.rejected('GATE_CWD')

    def test_final_business_gates_cannot_be_omitted(self):
        for name in ('NATIVE', 'RESTORE'):
            del self.contract['final_gates'][name]
            del self.package['quality_gates'][name]
        self.rejected('FINAL_BUSINESS_GATES_MISSING')

    def test_independent_review_required(self):
        with self.assertRaisesRegex(ValueError, 'INDEPENDENT_REVIEW_REQUIRED'):
            V.independent_review(None, self.package, self.contract_sha, self.candidate_sha, {})

    def test_empty_complete_source_set(self):
        self.contract['leaves'] = []
        self.package['leaf_rows'] = []
        self.rejected('EMPTY_SOURCE_SET')

    def test_cli_malformed_input_is_structured_failure(self):
        package = self.ref('package.json', '[]')
        contract = self.ref('contract.json', '{}')
        candidate = self.ref('candidate.json', '{}')
        result = subprocess.run([sys.executable, '-B', str(ROOT / 'scripts/verify_unified_delivery_acceptance.py'),
                                 '--package', package['path'], '--contract', contract['path'],
                                 '--candidate', candidate['path'], '--contract-sha256', contract['sha256'],
                                 '--candidate-sha256', candidate['sha256']], capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(json.loads(result.stdout)['rule'], 'INPUT_OBJECT')
        self.assertEqual(result.stderr, '')

    def complete_gate_receipts(self):
        payload = {'BE-COMPILE': 'APP imboy', 'BE-DIALYZE': 'done',
                   'BE-EUNIT-R1': 'All 1 tests passed.', 'BE-EUNIT-R2': 'All 1 tests passed.',
                   'ADMIN-TEST': '1 pass\n0 fail', 'APP-ANALYZE': 'No issues found!',
                   'APP-TEST': '00:01 +1: All tests passed!'}
        for gate, spec in self.contract['final_gates'].items():
            repo = 'backend' if gate.startswith('BE-') else 'admin' if gate.startswith('ADMIN-') else 'app'
            argv = (['make', 'eunit'] if gate.startswith('BE-EUNIT') else ['make', gate[3:].lower()]) if repo == 'backend' else ['bun', 'run', gate[6:].lower()] if repo == 'admin' else ['flutter', gate[4:].lower()]
            spec.update(repo=repo, argv=argv, round=1, success_pattern='\\S', execution_parameters_status='FROZEN')
            if spec['category'] in {'integration', 'recovery'}:
                spec.update(platform='app_android', device_id='fixture-device',
                            required_cells=[row['cell_id'] for row in self.rows])
                argv.extend(['-d', 'fixture-device'])
            rec = copy.deepcopy(self.record)
            rec.update(id=gate, kind='quality_gate', gate_id=gate, round=1, argv=argv,
                       cwd=self.candidate['sources'][repo]['repo_path'])
            if gate == 'BE-EUNIT-R2':
                rec.update(started_at='2026-10-14T00:02:00+00:00', ended_at='2026-10-14T00:03:00+00:00')
            self.log(rec)
            path = Path(rec['log']['path'])
            path.write_text(path.read_text().replace('synthetic assertion', payload.get(gate, 'integration oracle passed')))
            rec['log']['sha256'] = V.digest(path)
            self.package['command_receipts'].append(rec)
            self.package['quality_gates'][gate] = {'status': 'PASS', 'command_id': gate}

    def full_review(self, log_reuse_rule='LOG_REUSE'):
        self.complete_gate_receipts()
        probes = {}
        for name in sorted(V.PROBES):
            rule = log_reuse_rule if name == 'log_reuse' else sorted(V.PROBES[name])[0]
            rec = copy.deepcopy(self.record)
            rec.update(id='probe-' + name, kind='negative_probe', argv=['python3', 'test.py'])
            self.log(rec)
            path = Path(rec['log']['path'])
            path.write_text(path.read_text().replace('synthetic assertion', 'UC_PROBE ' + name + ' ' + rule))
            rec['log']['sha256'] = V.digest(path)
            self.package['command_receipts'].append(rec)
            probes[name] = {'command_id': rec['id'], 'expected_rule': rule,
                            'rejected_rule': rule, 'single_fault': True}
        self.package['overall'] = 'UNIFIED_LOCAL_CANDIDATE_PASS'
        content = json.dumps(self.package, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()
        return {'decision': 'APPROVE', 'reviewer': 'independent-fixture', 'independent': True,
                  'contract_sha256': self.contract_sha, 'candidate_sha256': self.candidate_sha,
                  'package_canonical_sha256': hashlib.sha256(content).hexdigest(),
                  'unresolved_p0_p1': [], 'negative_probes': probes}

    def test_full_requires_external_review_and_probe_receipts(self):
        review = self.full_review()
        probes = review['negative_probes']
        with self.assertRaisesRegex(ValueError, 'INDEPENDENT_REVIEW_REQUIRED'):
            self.validate('FULL')
        result = V.validate(self.package, self.contract, self.candidate, self.root,
                            self.contract_sha, self.candidate_sha, 'FULL', review)
        self.assertEqual(result['integrity'], 'PASS')
        probes['missing_leaf'].update(expected_rule='INPUT_OBJECT', rejected_rule='INPUT_OBJECT')
        with self.assertRaisesRegex(ValueError, 'NEGATIVE_PROBE_RULE'):
            V.validate(self.package, self.contract, self.candidate, self.root,
                       self.contract_sha, self.candidate_sha, 'FULL', review)

        probes['missing_leaf'].update(expected_rule='REQ_LEAF_MISSING', rejected_rule='REQ_LEAF_MISSING')
        review['unresolved_p0_p1'] = ['unresolved critical issue']
        with self.assertRaisesRegex(ValueError, 'UNRESOLVED_P0_P1'):
            V.validate(self.package, self.contract, self.candidate, self.root,
                       self.contract_sha, self.candidate_sha, 'FULL', review)

    def test_full_accepts_actual_observation_reuse_negative_probe(self):
        ref = self.package['leaf_rows'][1]['assertion']
        path = Path(ref['path'])
        original = path.read_text()
        assertion = json.loads(original)
        assertion.update(line=2, observed='synthetic assertion arrives')
        path.write_text(json.dumps(assertion))
        ref['sha256'] = V.digest(path)
        with self.assertRaisesRegex(ValueError, '^OBSERVATION_REUSE$') as rejected:
            self.validate()
        path.write_text(original)
        ref['sha256'] = V.digest(path)
        review = self.full_review(log_reuse_rule=str(rejected.exception))
        result = V.validate(self.package, self.contract, self.candidate, self.root,
                            self.contract_sha, self.candidate_sha, 'FULL', review)
        self.assertEqual(result['integrity'], 'PASS')

    def test_real_contract_platform_aliases_require_physical_discovery(self):
        for alias in ('android', 'macOS', 'browser', 'admin'):
            with self.subTest(platform=alias):
                expected = dict(self.rows[0], platform=alias)
                original_identity = V.identity(expected)
                original_cell = V.cell_id(expected)
                canonical = V.DEVICE_PLATFORMS[alias]
                receipt = copy.deepcopy(self.record)
                receipt['platform'] = alias
                receipt['execution_device'] = {'platform': canonical, 'id': 'fixture-device'}
                receipt['device'].update(platform=canonical, discovery=self.ref(
                    alias + '-devices.json', json.dumps({canonical: ['fixture-device']})))
                V.checked_device(receipt, expected, self.root)
                for change in ({'physical': False}, {'platform': 'backend'},
                               {'discovery': self.ref(alias + '-missing.json', '{}')}):
                    bad = copy.deepcopy(receipt)
                    bad['device'].update(change)
                    with self.assertRaisesRegex(ValueError, 'WRONG_DEVICE'):
                        V.checked_device(bad, expected, self.root)
                del receipt['device']
                with self.assertRaisesRegex(ValueError, 'WRONG_DEVICE'):
                    V.checked_device(receipt, expected, self.root)
                self.assertEqual(V.identity(expected), original_identity)
                self.assertEqual(V.cell_id(expected), original_cell)

    def test_full_rejects_simulator_argv_with_physical_receipt(self):
        self.full_device_selection(['flutter', 'test', '-d', 'emulator-5554'], 'WRONG_DEVICE')

    def full_device_selection(self, argv, rule=None):
        review = self.full_review()
        self.record['argv'] = argv
        self.record['execution_device'] = {'platform': 'app_android', 'id': 'fixture-device'}
        self.log(self.record)
        for leaf in self.package['leaf_rows']:
            reference = leaf['assertion']
            path = Path(reference['path'])
            assertion = json.loads(path.read_text())
            assertion['log_sha256'] = self.record['log']['sha256']
            path.write_text(json.dumps(assertion))
            reference['sha256'] = V.digest(path)
        content = json.dumps(self.package, sort_keys=True, ensure_ascii=False,
                             separators=(',', ':')).encode()
        review['package_canonical_sha256'] = hashlib.sha256(content).hexdigest()
        if rule is None:
            result = V.validate(self.package, self.contract, self.candidate, self.root,
                                self.contract_sha, self.candidate_sha, 'FULL', review)
            self.assertEqual(result['integrity'], 'PASS')
            return
        with self.assertRaisesRegex(ValueError, '^' + rule + '$'):
            V.validate(self.package, self.contract, self.candidate, self.root,
                       self.contract_sha, self.candidate_sha, 'FULL', review)

    def test_full_rejects_compact_emulator_selector_despite_runner(self):
        self.full_device_selection(['flutter', 'test', '-demulator-5554'], 'WRONG_DEVICE')

    def test_full_rejects_compact_other_device_despite_runner(self):
        self.full_device_selection(['flutter', 'test', '-dOTHER_DEVICE'], 'WRONG_DEVICE')

    def test_full_rejects_equals_selector_despite_runner(self):
        self.full_device_selection(['flutter', 'test', '--device-id=emulator-5554'], 'WRONG_DEVICE')

    def test_full_accepts_matching_compact_physical_selector(self):
        self.full_device_selection(['flutter', 'test', '-dfixture-device'])

    def test_full_accepts_matching_equals_physical_selector(self):
        self.full_device_selection(['flutter', 'test', '--device-id=fixture-device'])

    def test_execution_envelope_binds_device_context(self):
        for key in ('platform', 'role', 'round', 'device'):
            with self.subTest(field=key):
                receipt = copy.deepcopy(self.record)
                receipt[key] = 'changed'
                with self.assertRaisesRegex(ValueError, 'EXECUTION_START'):
                    V.command_record(receipt, self.candidate_sha, self.candidate, self.root)

    def test_device_selection_argv_and_explicit_runner(self):
        for argv in (['adb', '-s', 'emulator-5554', 'shell'],
                     ['flutter', 'test', '--device-id=simulator-123'],
                     ['flutter', 'test', '-d', 'another-device'], ['runner']):
            with self.subTest(argv=argv):
                receipt = dict(self.record, argv=argv)
                with self.assertRaisesRegex(ValueError, 'WRONG_DEVICE'):
                    V.checked_device(receipt, self.rows[0], self.root)
        receipt = dict(self.record, argv=['runner'], execution_device={
            'platform': 'app_android', 'id': 'fixture-device'})
        V.checked_device(receipt, self.rows[0], self.root)

    def test_reused_receipt_cannot_sign_two_gates(self):
        self.complete_gate_receipts()
        self.package['quality_gates']['BE-EUNIT-R2']['command_id'] = 'BE-EUNIT-R1'
        self.rejected('GATE_RECEIPT_REUSE_OR_MISSING')

    def test_integration_gate_requires_device_and_exact_journeys(self):
        for field, value, rule in [('device', None, 'WRONG_DEVICE'),
                                   ('platform', 'app_macos', 'WRONG_PLATFORM_ROLE')]:
            with self.subTest(field=field):
                self.complete_gate_receipts()
                records = {rec['id']: rec for rec in self.package['command_receipts']}
                gate = copy.deepcopy(records['NATIVE'])
                if value is None:
                    gate.pop(field)
                else:
                    gate[field] = value
                with self.assertRaisesRegex(ValueError, '^' + rule + '$'):
                    V.gate_journeys(self.contract['final_gates']['NATIVE'], gate,
                                    self.package, records, self.contract, self.root)
                self.package['command_receipts'] = [self.record]

    def test_integration_gate_contract_cannot_omit_journeys_or_device(self):
        self.complete_gate_receipts()
        records = {rec['id']: rec for rec in self.package['command_receipts']}
        for field, rule in [('required_cells', 'GATE_JOURNEY_CONTRACT'),
                            ('device_id', 'GATE_DEVICE_CONTRACT')]:
            with self.subTest(field=field):
                spec = dict(self.contract['final_gates']['NATIVE'])
                spec.pop(field)
                with self.assertRaisesRegex(ValueError, '^' + rule + '$'):
                    V.gate_journeys(spec, records['NATIVE'], self.package,
                                    records, self.contract, self.root)

    def test_final_device_gates_reject_other_discovered_device_and_simulator(self):
        self.complete_gate_receipts()
        discovery = self.ref('gate-devices.json', json.dumps(
            {'app_android': ['fixture-device', 'other-device']}))
        for gate_id in ('NATIVE', 'RESTORE'):
            gate = next(rec for rec in self.package['command_receipts'] if rec['id'] == gate_id)
            original = copy.deepcopy(gate['device'])
            for change in ({'id': 'other-device', 'discovery': discovery}, {'physical': False}):
                with self.subTest(gate=gate_id, change=change):
                    gate['device'] = dict(original, **change)
                    self.log(gate)
                    self.rejected('WRONG_DEVICE')
            gate['device'] = original
            self.log(gate)

    def test_integration_gate_rejects_missing_journey(self):
        self.complete_gate_receipts()
        self.package['leaf_rows'][1].update(status='NOT_RUN', reason='not executed', next_unlock='run')
        self.package['parent_status'][json.dumps(['phase1', 'AC23'])] = 'PARTIAL'
        self.sync_ledgers()
        self.rejected('GATE_JOURNEY_MISSING')

    def test_integration_gate_rejects_wrong_journey_round_or_window(self):
        self.complete_gate_receipts()
        records = {rec['id']: rec for rec in self.package['command_receipts']}
        for field, value in [('round', 2), ('ended_at', '2026-10-14T00:02:00+00:00')]:
            with self.subTest(field=field):
                journey = dict(self.record)
                journey[field] = value
                changed = dict(records, **{journey['id']: journey})
                with self.assertRaisesRegex(ValueError, '^GATE_JOURNEY_BINDING$'):
                    V.gate_journeys(self.contract['final_gates']['NATIVE'], records['NATIVE'],
                                    self.package, changed, self.contract, self.root)

    def test_raw_admin_skip_cannot_be_hidden_by_zero_receipt(self):
        self.complete_gate_receipts()
        rec = next(r for r in self.package['command_receipts'] if r['id'] == 'ADMIN-TEST')
        path = Path(rec['log']['path'])
        original = path.read_text()
        for result in ('3 skip', '2 todo'):
            with self.subTest(result=result):
                path.write_text(original.replace('0 fail', '0 fail\n' + result))
                rec['log']['sha256'] = V.digest(path)
                self.rejected('GATE_RESULT')

    def test_ledger_drift_rejected_even_with_updated_hash(self):
        ref = self.package['ledger']
        path = Path(ref['path'])
        path.write_text(path.read_text().replace('PASS', 'FAIL', 1))
        ref['sha256'] = V.digest(path)
        self.rejected('LEDGER_DIVERGENCE')


if __name__ == '__main__':
    unittest.main()
