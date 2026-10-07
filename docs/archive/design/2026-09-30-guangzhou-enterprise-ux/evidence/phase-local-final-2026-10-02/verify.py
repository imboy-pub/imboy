"""Read-only qualification of recorded oracles; never reruns external writes."""
from pathlib import Path
import csv,hashlib,json,re,sys
ROOT=Path(__file__).resolve().parent

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def load(path):
    return decode(json.loads(path.read_text()))

def decode(value):
    if isinstance(value,list):return [decode(item) for item in value]
    if not isinstance(value,dict):return value
    if set(value)=={'__digest_map_v1__'}:
        return {name:sha for sha,name in value['__digest_map_v1__']}
    return {name:decode(item) for name,item in value.items()}

def records_check(state,pair):
    ids={f'INT-{i:02}' for i in range(1,43)}
    policy={f'INT-{i:02}' for i in [2,4,5,6,8,9,10,11,12,13,14,15,19,20,21,22,32,35,36,37,38,39,40,41,42]}
    scenarios={'zero_grant','missing_scope','revoked_grant','expired_grant'}
    expected={(aid,scenario,mode) for aid in ids for scenario in scenarios for mode in (['application','human'] if aid in {'INT-09','INT-10'} else ['application'])}
    assert pair['candidate']==state['heads']['backend']
    records=load(ROOT/'backend-pair-records.json')
    assert len(records)==2 and {item['run'] for item in records}=={1,2}
    assert {run['run'] for run in pair['runs']}=={1,2}
    for record in records:
        assert len(record['records'])==5
        op,negative,audit=[load(ROOT/ref['archive']) if ref['archive'].endswith('.json') else [json.loads(line) for line in (ROOT/ref['archive']).read_text().splitlines()] for ref in record['records'][:3]]
        assert len(op['operations'])==42 and {item['id'] for item in op['operations']}==ids
        assert set(op['covered_operation_ids'])==ids and all(item['behavior_assertions_passed'] for item in op['operations'])
        assert len(negative)==176 and {(item['id'],item['scenario'],item['sender_mode']) for item in negative}==expected
        assert all(item['status']==403 and item['code']=='insufficient_scope' for item in negative)
        assert len(audit)==25 and {item['id'] for item in audit}==policy
        assert all(item['rollback'] and item['replay_no_write'] and item['audit_failure_status']==500 and item['retry_status']==200 for item in audit)

def check():
    state=load(ROOT/'state.json')
    rows=list(csv.DictReader((ROOT/'acceptance.tsv').open(),delimiter='\t'))
    parents=[row for row in rows if not row['sub_endpoint']]
    children=[row for row in rows if row['sub_endpoint']]
    expected={f'{prefix}-{i:02}' for prefix in ['G0','CS','ORG','FILE','UX','OA','INTG','QA'] for i in [1,2,3]}
    expected|={f'INT-{domain}{i:02}' for domain in ['A','B'] for i in [1,2,3]}
    assert len(parents)==30 and {row['acceptance_id'] for row in parents}==expected
    assert len(children)==42 and {row['sub_endpoint'] for row in children}=={f'INT-{i:02}' for i in range(1,43)}
    for row in rows:
        assert json.loads(row['source_heads'])==state['heads']
        assert row['plan_sha256']==state['plan_sha256'] and row['command'] and row['oracle']
        if row['status'].startswith('PASS'):assert row['exit']=='0'
        for ref in json.loads(row['evidence']):assert digest(ROOT/ref['archive'])==ref['sha256']
    for ref in load(ROOT/'artifact-origins.json'):
        assert digest(ROOT/ref['archive'])==ref['sha256']
        original=Path(ref['original'])
        if original.exists():
            assert digest(original)==ref['original_sha256']
            if original.suffix=='.json':assert load(ROOT/ref['archive'])==json.loads(original.read_text())
        if ref.get('original_semantic_sha256'):
            semantic=json.dumps(load(ROOT/ref['archive']),ensure_ascii=False,sort_keys=True,separators=(',',':')).encode()
            assert hashlib.sha256(semantic).hexdigest()==ref['original_semantic_sha256']
    pair=next(load(ROOT/ref['archive']) for ref in load(ROOT/'artifact-origins.json') if ref['original'].endswith('gz-current-final-083901-pair-terminal.json'))
    assert pair['status']=='PASS_BACKEND_FROZEN_PAIR' and len(pair['runs'])==2
    assert all(run['exit']==0 and run['passed']==10788 and run['source_binding_verified'] for run in pair['runs'])
    records_check(state,pair)
    print('PASS_RECORDED_LEDGER_INTEGRITY:30 parents,42 children,2x10788; skip/external/production are separate')

if __name__=='__main__':
    check()
