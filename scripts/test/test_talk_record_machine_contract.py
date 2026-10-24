"""Static source-to-contract checks; no HTTP/DB/model acceptance claims."""
from pathlib import Path
import re
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
from contract_gate import ROUTE_RE, extract_routes
from contract_route_sources import uncomment


HTTP_METHOD = r'<<"(GET|POST|PUT|PATCH|DELETE|HEAD)">>'


def action_methods(module, action, source):
    """Support the actual dispatch forms; an unrecognized form must fail closed."""
    if module == 'talk_record_handler':
        block = re.search(r'^handle_action\(' + re.escape(action) +
                          r',.*?(?=^handle_action\(|\Z)', source, re.M | re.S)
        if block is None:
            raise ValueError(f'Missing action {module}:{action}')
        methods = set(re.findall(HTTP_METHOD, block.group()))
    elif module == 'talk_record_client_handler':
        pairs = re.findall(r'^method\((\w+)\)\s*->\s*' + HTTP_METHOD, source, re.M)
        mapping = dict(pairs)
        if '_' not in mapping:
            raise ValueError('Missing client method fallback')
        methods = {mapping.get(action, mapping['_'])}
    elif module == 'talk_record_provider_handler':
        block = re.search(r'\bMethod\s*=\s*case A of(.*?)end', source, re.S)
        if block is None:
            raise ValueError('Unsupported provider method dispatch')
        pairs = re.findall(r'(\w+)\s*->\s*' + HTTP_METHOD, block.group(1))
        mapping = dict(pairs)
        if '_' not in mapping:
            raise ValueError('Missing provider method fallback')
        methods = {mapping.get(action, mapping['_'])}
    elif action is None:
        methods = set(re.findall(HTTP_METHOD, source))
    else:
        raise ValueError(f'Unsupported handler {module}')
    if not methods:
        raise ValueError(f'No methods found for {module}:{action}')
    return methods


def routes():
    source = uncomment((ROOT / 'src/api/talk_record_routes.erl').read_text())
    for match in ROUTE_RE.finditer(source):
        path, module, opts = match.groups()
        action = re.search(r'action\s*=>\s*(\w+)', opts)
        handler = uncomment((ROOT / 'src/api' / (module + '.erl')).read_text())
        name = action.group(1) if action else None
        yield path, module, action_methods(module, name, handler)


class TalkRecordContractTests(unittest.TestCase):
    def test_all_60_paths_actual_handler_methods_are_registered_and_documented(self):
        root = (ROOT / 'api/openapi-api.yaml').read_text()
        exported = extract_routes((ROOT / 'src/imboy_router.erl').read_text())
        registered = {e['path'] for e in exported['api_v1']}
        documents = dict(re.findall(
            r'^  (/api/v1/talk-records[^\n]*):\n\s+\$ref: [\'"]?([^\s\'"]+)',
            root, re.M))
        total = 0
        actual_routes = list(routes())
        self.assertEqual(len(actual_routes), 60)
        self.assertEqual(len({path for path, _, _ in actual_routes}), 60)
        self.assertEqual(set(documents), {
            re.sub(r':(\w+)', r'{\1}', path) for path, _, _ in actual_routes})
        for path, module, methods in actual_routes:
            with self.subTest(path=path):
                self.assertIn(path, registered)
                key = re.sub(r':(\w+)', r'{\1}', path)
                document = ROOT / 'api' / documents[key]
                actual = set(re.findall(r'^(get|post|put|patch|delete|head):', document.read_text(), re.M))
                self.assertEqual({m.lower() for m in methods}, actual)
                total += len(methods)
        self.assertGreater(total, len(actual_routes))

    def test_split_handler_method_dispatch_is_not_borrowed_from_other_actions(self):
        provider = (ROOT / 'src/api/talk_record_provider_handler.erl').read_text()
        client = (ROOT / 'src/api/talk_record_client_handler.erl').read_text()
        for action, expected in [('latest', 'GET'), ('prepare', 'POST')]:
            self.assertEqual({expected}, action_methods('talk_record_provider_handler', action, provider))
        for action, expected in [('get', 'GET'), ('plan', 'PUT'), ('claim', 'POST')]:
            self.assertEqual({expected}, action_methods('talk_record_client_handler', action, client))

    def test_missing_or_unsupported_dispatch_fails_instead_of_skipping_routes(self):
        with self.assertRaisesRegex(ValueError, 'Missing action'):
            action_methods('talk_record_handler', 'missing', '')
        with self.assertRaisesRegex(ValueError, 'Unsupported provider'):
            action_methods('talk_record_provider_handler', 'get', '')
        with self.assertRaisesRegex(ValueError, 'fallback'):
            action_methods('talk_record_client_handler', 'claim', 'method(get) -> <<"GET">>.')

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
