"""Static source-to-contract checks; no HTTP/DB/model acceptance claims."""
from pathlib import Path
import re
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
from contract_gate import ROUTE_RE, extract_routes
from contract_route_sources import uncomment


def routes():
    source = uncomment((ROOT / 'src/api/talk_record_routes.erl').read_text())
    handler = (ROOT / 'src/api/talk_record_handler.erl').read_text()
    dedicated = {'talk_record_cleanup_retry_handler': 'cleanup_retry',
                 'talk_record_recovery_handler': 'recovery',
                 'talk_record_participant_handler': 'participants',
                 'talk_record_governance_handler': 'governance',
                 'talk_record_playback_handler': 'stream'}
    for match in ROUTE_RE.finditer(source):
        path, module, opts = match.groups()
        action = re.search(r'action\s*=>\s*(\w+)', opts)
        if action:
            name = action.group(1)
            block = re.search(r'^handle_action\(' + name + r',.*?(?=^handle_action\(|\Z)', handler, re.M | re.S).group()
        else:
            name = dedicated[module]
            block = uncomment((ROOT / 'src/api' / (module + '.erl')).read_text())
        methods = set(re.findall(r'<<"(GET|POST|PUT|PATCH|DELETE|HEAD)">>', block))
        yield path, name, methods


class TalkRecordContractTests(unittest.TestCase):
    def test_all_38_paths_49_real_methods_are_registered_and_documented(self):
        root = (ROOT / 'api/openapi-api.yaml').read_text()
        exported = extract_routes((ROOT / 'src/imboy_router.erl').read_text())
        registered = {e['path'] for e in exported['api_v1']}
        total = 0
        for path, action, methods in routes():
            with self.subTest(path=path):
                self.assertIn(path, registered)
                key = re.sub(r':(\w+)', r'{\1}', path)
                self.assertIn('  ' + key + ':', root)
                document = ROOT / 'api/paths/api/v1/talk-records' / (action + '.yaml')
                actual = set(re.findall(r'^(get|post|put|patch|delete|head):', document.read_text(), re.M))
                self.assertEqual({m.lower() for m in methods}, actual)
                total += len(methods)
        self.assertEqual(len(list(routes())), 38)
        self.assertEqual(total, 49)

    def test_all_new_local_schema_refs_resolve(self):
        files = list((ROOT / 'api/paths/api/v1/talk-records').glob('*.yaml'))
        files += list((ROOT / 'api/components/schemas/talk-record').glob('*.yaml'))
        files.append(ROOT / 'api/paths/api/v1/organization/attachments.yaml')
        for file in files:
            for ref in re.findall(r'^\s*(?:- )?\$ref:\s*[\'"]?([^\s\'"#]+)', file.read_text(), re.M):
                self.assertTrue((file.parent / ref).resolve().is_file(), (file, ref))

    def test_binary_and_raw_receipt_contracts_are_distinct(self):
        stream = (ROOT / 'api/paths/api/v1/talk-records/stream.yaml').read_text()
        for term in ['get:', 'head:', 'audio/aac:', "'206':", "'416':", 'Content-Range:', 'Range']:
            self.assertIn(term, stream)
        receipt = (ROOT / 'api/paths/api/v1/attachment/gateway-put.yaml').read_text()
        for term in ['security: []', 'x-device-signature-required: false', 'pinned:', 'sha256:', 'size:', 'const: true']:
            self.assertIn(term, receipt)
        self.assertNotIn('Envelope.yaml', receipt)

    def test_recovery_completion_and_id_types_are_explicit(self):
        capture = (ROOT / 'api/components/schemas/talk-record/Capture.yaml').read_text()
        self.assertIn('completion_manifest_hash:', capture)
        complete = (ROOT / 'api/paths/api/v1/talk-records/complete.yaml').read_text()
        for term in ['completion_already_submitted', 'Idempotency-Key', 'If-Match', 'PV/PCV']:
            self.assertIn(term, complete)
        recovery = (ROOT / 'api/paths/api/v1/talk-records/recovery.yaml').read_text()
        self.assertIn('client_recording_id', recovery)
        self.assertIn('creator AND current owner', ' '.join(recovery.split()))
        entity = (ROOT / 'api/components/schemas/talk-record/Record.yaml').read_text()
        self.assertRegex(entity, r'owner_user_id:\n\s+type: integer\n\s+format: int64')


if __name__ == '__main__':
    unittest.main()
