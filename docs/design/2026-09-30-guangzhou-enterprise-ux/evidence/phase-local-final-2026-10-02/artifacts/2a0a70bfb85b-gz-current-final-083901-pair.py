from pathlib import Path
import subprocess,json,re,hashlib,sys,os,signal,uuid
repo=Path('/Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-webhook-transport')
os.umask(0o077)
owner='gz-final-'+uuid.uuid4().hex
os.environ['IMBOY_GATE_OWNER']=owner
cache=json.loads(Path('/tmp/imboy-internal-audit.zRSdsy/result.json').read_text())
head=subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()
assert head=='083901a60f49b25ed5abf6109af9c577bed1bc3d'
recovery=json.loads((repo/'evidence/full-regression-test-recovery/result.json').read_text())
for item in recovery['source_files']:
 assert hashlib.sha256((repo/item['path']).read_bytes()).hexdigest()==item['sha256'] or item['path']=='test/repo/msg_store_repo_tests.erl'
 cache['sources'][item['path']]=item['sha256']
for name in ['src/lib/elib_tsid_catalog.erl', 'test/lib/elib_tsid_catalog_tests.erl', 'test/lib/elib_tsid_rebind_tests.erl', 'test/lib/elib_tsid_guard_tests.erl', 'test/lib/data_disposition_tests.erl', 'src/api/workspace_handler.erl', 'src/ds/attachment_ds.erl', 'src/ds/group_file_ds.erl', 'src/ds/workspace_ds.erl', 'src/imboy_router.erl', 'src/logic/attach_logic.erl', 'src/logic/enterprise_message_logic.erl', 'src/logic/group_logic.erl', 'src/repo/attachment_repo.erl', 'src/repo/enterprise_audit_event_repo.erl', 'src/repo/enterprise_message_repo.erl', 'src/repo/group_file_repo.erl', 'src/repo/group_repo.erl', 'src/repo/msg_store_repo.erl', 'test/api/enterprise_cursor_domain_http_checks.erl', 'test/api/enterprise_human_file_binding_pg_checks.erl', 'test/api/enterprise_legacy_group_file_pg_checks.erl', 'test/api/enterprise_message_authorization_http_checks.erl', 'test/api/workspace_unread_http_checks.erl', 'test/ds/group_file_ds_tests.erl', 'test/ds/workspace_archive_closure_tests.erl', 'test/logic/attach_logic_tests.erl', 'test/logic/enterprise_msg_asset_webhook_pg_tests.erl', 'test/performance/msg_send_performance_tests.erl', 'test/repo/group_file_atomic_pg_tests.erl', 'test/repo/msg_store_repo_tests.erl', 'test/repo/workspace_member_groups_pg_tests.erl']:
 cache['sources'][name]=hashlib.sha256((repo/name).read_bytes()).hexdigest()
assert not subprocess.check_output(['git','status','--porcelain'],cwd=repo,text=True).strip()
paths=[Path('/tmp/gz-current-final-083901-backend.sh'),Path('/tmp/gz-current-final-083901-garage.sh')]
paths.append(Path('/tmp/gz-current-final-083901-pair.py'))
helpers={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
tracked=subprocess.check_output(['git','ls-files','-z'],cwd=repo).split(b'\0')
def fingerprint(p):
 if p.is_symlink():return {'kind':'symlink','target':os.readlink(p)}
 return {'kind':'file','sha256':hashlib.sha256(p.read_bytes()).hexdigest()}
frozen={n.decode():fingerprint(repo/n.decode()) for n in tracked if n}
def assert_frozen():
 assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()==head
 assert not subprocess.check_output(['git','status','--porcelain'],cwd=repo,text=True).strip()
 assert frozen=={n:fingerprint(repo/n) for n in frozen}
 assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in helpers.items())
 for n,h in cache['sources'].items():
  p=Path(n) if Path(n).is_absolute() else repo/n
  assert p.is_file() and hashlib.sha256(p.read_bytes()).hexdigest()==h, n
 for n,h in cache['beams'].items():
  assert hashlib.sha256((Path('/tmp/imboy-internal-audit.zRSdsy/imboy/ebin')/n).read_bytes()).hexdigest()==h, n
state={'owner':owner,'coordinator_pid':os.getpid(),'status':'RUNNING_NOT_PASS','candidate':head,'runs':[],'helper_hashes':helpers,'required_pass_runs':2,'whole_goal_complete':False}
def save():Path('/tmp/gz-current-final-083901-pair-terminal.json').write_text(json.dumps(state,indent=2)+'\n')
save()
for n in (1,2):
 log=Path(f'/tmp/gz-current-final-083901-pair-r{n}.log')
 entry={'run':n,'driver_log':str(log)}
 try:
  assert_frozen()
  with log.open('w') as f:
   process=subprocess.Popen(['bash',str(paths[1])],stdout=f,stderr=subprocess.STDOUT,start_new_session=True)
   entry['driver_pid']=process.pid;state['active_run']=entry;save()
   try:
    rc=process.wait(timeout=1900)
   except subprocess.TimeoutExpired:
    os.killpg(process.pid,signal.SIGTERM)
    try:process.wait(timeout=15)
    except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
    try:os.killpg(process.pid,signal.SIGKILL)
    except ProcessLookupError:pass
    raise RuntimeError('full run timed out; owned process group stopped')
  entry.update(exit=rc,driver_log_sha256=hashlib.sha256(log.read_bytes()).hexdigest())
  t=log.read_text(errors='replace');dirs=re.findall(r'^Evidence: (.+)$',t,re.M)
  assert_frozen()
  assert rc==0 and len(dirs)==1,'driver failed or missing run directory'
  out=Path(dirs[0]);b=json.loads((out/'source-binding.json').read_text());report=(out/'http.log').read_text(errors='replace')
  assert b['candidate']==head
  assert all(hashlib.sha256((out/'imboy/ebin'/f).read_bytes()).hexdigest()==h for f,h in b['beams'].items())
  assert all(hashlib.sha256((repo/f).read_bytes()).hexdigest()==h for f,h in b['sources'].items())
  assert 'FROZEN_FULL_RESULT=ok' in report.splitlines() and '*failed*' not in report and 'One or more tests were cancelled' not in report
  counts=re.findall(r'All ([0-9]+) tests passed\.',report);assert counts and int(counts[-1])>10000
  entry.update(status='PASS_FROZEN_FULL',run_dir=str(out),passed=int(counts[-1]),source_binding_verified=True,log_sha256=hashlib.sha256((out/'http.log').read_bytes()).hexdigest(),discovery=json.loads((out/'full-discovery.json').read_text()))
 except Exception as exc:
  entry.update(status='FAIL',reason=str(exc));state['runs'].append(entry);state['status']='FAILED_NOT_PASS';save()
  try:
   containers=subprocess.check_output(['docker','ps','-aq','--filter','label=imboy.gate.owner='+owner],text=True).split()
   if containers:subprocess.run(['docker','rm','-f','-v',*containers],check=True,stdout=subprocess.DEVNULL)
  except Exception as cleanup_error:
   state['cleanup_error']=str(cleanup_error);save()
  print(f'FULL_RUN_{n}_FAIL',flush=True);sys.exit(1)
 state['runs'].append(entry);save();print(f'FULL_RUN_{n}_PASS tests={entry["passed"]}',flush=True)
state['status']='PASS_BACKEND_FROZEN_PAIR';save();print('BACKEND_FROZEN_PAIR_PASS; whole delivery remains incomplete',flush=True)
