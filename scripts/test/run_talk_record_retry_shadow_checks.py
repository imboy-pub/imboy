#!/usr/bin/env python3
"""Explicit owned PG execution. Never called by unit tests or normal migrations.
Each method runs in an independent psql transaction/process. A mutant alters only
one function inside that transaction; process exit always rolls it back.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

ROOT=Path(__file__).resolve().parents[2]
PSQL='/opt/homebrew/opt/postgresql@18/bin/psql'
SYSID='7698912704598417449'
FUNCTIONS={'claim':'fn_v5_claim_ordinary','ack':'fn_v5_ack_ordinary','reaper':'fn_v5_reaper_ordinary'}


def script(case, mutant):
    text=(ROOT/f'test/sql/talk_record_retry_worker_temp_shadow_{case}.sql').read_text()
    if not mutant:
        return text
    source=(ROOT/'priv/migrations/00000221_talk_record_cleanup_retry.up.sql').read_text()
    name=FUNCTIONS[case]
    match=re.search(r'CREATE OR REPLACE FUNCTION public\.'+name+r'\(.*?\$\$;',source,re.S)
    assert match, 'Exact current protocol definition required'
    definition=match.group().replace('search_path=pg_catalog,public,pg_temp','search_path=public')
    for table in ['attachment_object_deletion','attachment_lifecycle_v5_audit']:
        definition=definition.replace('public.'+table,table)
    # Only this method is mutated, and only within this case's rollback transaction.
    return text.replace('BEGIN;','BEGIN;\n'+definition,1)


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--execute',action='store_true')
    parser.add_argument('--database',required=True)
    parser.add_argument('--lease',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    if not args.execute:
        raise SystemExit('PREPARED_ONLY; explicit controller lease required')
    lease=json.loads(args.lease.read_text())
    assert lease['approved'] is True and lease['owner']=='codex-relay-20261021'
    assert lease['database']==args.database and lease['system_identifier']==SYSID
    assert lease['host']=='127.0.0.1' and lease['port']==65497 and lease['exclusive'] is True
    assert datetime.datetime.now(datetime.timezone.utc)<datetime.datetime.fromisoformat(lease['expires_at'].replace('Z','+00:00'))
    assert args.database.startswith('talk_record_') and args.database not in ['talk_record_full'], 'Owned isolated DB required'
    assert os.environ.get('PGHOST')=='127.0.0.1' and os.environ.get('PGPORT')=='65497'
    assert os.environ.get('PGDATABASE')==args.database
    assert os.environ.get('T20_SHADOW_OWNED_DATABASE')==args.database, 'Controller ownership declaration required'
    assert not args.output.exists(), 'Never overwrite prior evidence'
    args.output.mkdir(mode=0o700,parents=True)
    env={k:v for k,v in os.environ.items() if k in ['PATH','HOME','LANG','PGHOST','PGPORT','PGDATABASE','PGUSER','PGPASSWORD']}
    env['PGOPTIONS']='-c statement_timeout=20000 -c lock_timeout=3000'
    check=subprocess.run([PSQL,'-X','-w','-At','-v','ON_ERROR_STOP=1','-c',
       'SELECT current_database()||\'|\'||system_identifier::text FROM pg_control_system()'],
       env=env,capture_output=True,text=True)
    assert check.returncode==0 and check.stdout.strip()==args.database+'|'+SYSID,'Physical identity gate refused'
    receipts=[]
    # No short-circuit: every hardened and individual mutant case has a terminal.
    for case in FUNCTIONS:
        for mutant in [False,True]:
            label=case+('-mutant' if mutant else '-hardened')
            source=script(case,mutant)
            try:
                proc=subprocess.run([PSQL,'-X','-w','-v','ON_ERROR_STOP=1'],
                    input=source,env=env,capture_output=True,text=True,timeout=45)
            except subprocess.TimeoutExpired:
                receipts.append({'case':case,'variant':'single-method-mutant' if mutant else 'hardened',
                    'result':'TEST_FAILURE','reason':'timeout','ok':False})
                continue
            private=args.output/(label+'.private.log')
            private.write_text(proc.stdout+proc.stderr);private.chmod(0o600)
            rejected='SHADOW_ORACLE:' in proc.stderr
            ok=proc.returncode!=0 and rejected if mutant else proc.returncode==0
            receipts.append({'case':case,'variant':'single-method-mutant' if mutant else 'hardened',
              'raw_exit':proc.returncode,'expected':'oracle rejection' if mutant else 'pass',
              'result':'EXPECTED_REJECTION' if mutant and ok else ('PASS' if ok else 'TEST_FAILURE'),
              'ok':ok,'script_sha256':hashlib.sha256(source.encode()).hexdigest(),
              'private_log_sha256':hashlib.sha256(private.read_bytes()).hexdigest()})
    (args.output/'summary.json').write_text(json.dumps(receipts,indent=2)+'\n')
    raise SystemExit(0 if all(r['ok'] for r in receipts) else 1)


if __name__=='__main__':
    os.umask(0o077)
    main()
