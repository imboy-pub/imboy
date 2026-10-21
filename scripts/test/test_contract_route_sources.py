import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS))
from contract_route_sources import talk_record_delegate
import contract_gate


def registered_router():
    return """get_routes() ->
    Host = '_',
    MainRoutes = [],
    ApiV1Routes = [],
    AdmRoutes = [],
    CompiledApiRoutes = customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes()))),
    CompiledAdmRoutes = [],
    CompiledPluginRoutes = [],
    CoreRoutes = MainRoutes ++ CompiledApiRoutes ++ CompiledAdmRoutes ++ enterprise_internal_routes(),
    [{Host, CoreRoutes ++ CompiledPluginRoutes}].
"""


class DelegateTests(unittest.TestCase):
    def test_real_delegate_is_exactly_37_routes(self):
        repo = SCRIPTS.parent
        body = talk_record_delegate((repo / 'src/imboy_router.erl').read_text(), repo)
        self.assertEqual(len(list(contract_gate.ROUTE_RE.finditer(body))), 37)
        paths = contract_gate.extract_routes((repo / 'src/imboy_router.erl').read_text())['api_v1']
        self.assertEqual(sum(e['path'].startswith('/api/v1/talk-records') for e in paths), 37)

    def test_unregistered_and_commented_calls_do_not_read_delegate(self):
        with tempfile.TemporaryDirectory() as directory:
            for source in ('OtherRoutes ++ talk_record_routes:routes()',
                           '% imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes())',
                           'ok. % imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes())',
                           '"imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes())"', ''):
                self.assertEqual(talk_record_delegate(source, Path(directory)), '')

    def test_missing_or_nonliteral_registered_delegate_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            (repo / 'src/api').mkdir(parents=True)
            with self.assertRaises(FileNotFoundError):
                talk_record_delegate(registered_router(), repo)
            (repo / 'src/api/talk_record_routes.erl').write_text('-module(talk_record_routes).\nroutes() -> computed().')
            with self.assertRaises(ValueError):
                talk_record_delegate(registered_router(), repo)

    def test_comments_and_other_functions_are_not_routes(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            (repo / 'src/api').mkdir(parents=True)
            (repo / 'src/api/talk_record_routes.erl').write_text('''-module(talk_record_routes).
routes() -> [
% {"/comment", bad_handler, #{}},
{"/registered", real_handler, #{}}
].
other() -> [{"/unregistered", bad_handler, #{}}].
''')
            body = talk_record_delegate(registered_router(), repo)
            self.assertIn('/registered', body)
            self.assertNotIn('/comment', body)
            self.assertNotIn('/unregistered', body)

    def test_unused_function_and_unreachable_registration_fail_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            fake = registered_router().replace('get_routes()', 'unused_contract_example()')
            self.assertEqual(talk_record_delegate('get_routes() -> [].\n' + fake, repo), '')
            for source in [
                registered_router().replace('customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes())))', 'fun() -> customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes()))) end'),
                registered_router().replace('customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes())))', 'case false of true -> customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes ++ talk_record_routes:routes()))); false -> [] end'),
                registered_router().replace('MainRoutes ++ CompiledApiRoutes ++ CompiledAdmRoutes', 'MainRoutes ++ ApiV1Routes ++ CompiledAdmRoutes'),
                registered_router().replace('[{Host, CoreRoutes ++ CompiledPluginRoutes}]', '[{Host, MainRoutes}]'),
            ]:
                with self.assertRaises(ValueError):
                    talk_record_delegate(source, repo)


class CoverageGateTests(unittest.TestCase):
    def fixture(self, root):
        import shutil
        for f in ['scripts/check_rest_contract_coverage.sh','scripts/contract_route_sources.py']:
            target=root/f;target.parent.mkdir(parents=True,exist_ok=True)
            shutil.copy2(SCRIPTS.parent/f,target)
        for d in ['src/api','test/rest','api/paths']:(root/d).mkdir(parents=True,exist_ok=True)
        (root/'src/imboy_router.erl').write_text(registered_router() + """
open() ->
    [<<"/other">>].
option() ->
    [<<"/optional">>].
test_open_routes() ->
    [].
""")
        (root/'src/api/talk_record_routes.erl').write_text('''-module(talk_record_routes).
routes() -> [{"/api/v1/talk-records/:id", talk_record_handler, #{action=>item}}].
''')
        (root/'api/openapi.yaml').write_text("paths:\n  /api/v1/talk-records/{id}:\n    $ref: './paths/item.yaml'\n")
        (root/'api/paths/item.yaml').write_text('get:\n  operationId: example\n')
        (root/'test/rest/example.md').write_text('### CONTRACT-1\n')
        (root/'test/rest/example.erl').write_text('id() -> <<"CONTRACT-1">>.\n')
        row=['GET','/api/v1/talk-records/:id','talk_record_handler','jwt','test/rest/example.md','test/rest/example.erl','CONTRACT-1','api/paths/item.yaml']
        (root/'test/rest/contracts.tsv').write_text('\t'.join(row)+'\n')

    def test_registered_multiline_scope_covers_gate_and_inventory(self):
        import subprocess
        with tempfile.TemporaryDirectory() as d:
            root=Path(d);self.fixture(root)
            gate=subprocess.run(['bash',str(root/'scripts/check_rest_contract_coverage.sh')],capture_output=True,text=True)
            self.assertEqual(gate.returncode,0,gate.stdout+gate.stderr)
            inventory=root/'inventory.tsv'
            result=subprocess.run(['bash',str(root/'scripts/check_rest_contract_coverage.sh'),'--inventory',str(inventory)],capture_output=True,text=True)
            self.assertEqual(result.returncode,0,result.stdout+result.stderr)
            self.assertIn('\tget\tjwt\tOK',inventory.read_text())

    def test_missing_delegate_or_operation_or_suite_still_fails(self):
        import subprocess
        for failure in ['registration','unused_function','fun','branch','core','return','operation','suite']:
            with self.subTest(failure=failure),tempfile.TemporaryDirectory() as d:
                root=Path(d);self.fixture(root)
                target='api/paths/item.yaml' if failure=='operation' else 'test/rest/example.erl' if failure=='suite' else 'src/imboy_router.erl'
                path=root/target
                source=path.read_text()
                if failure=='registration':source=source.replace('ApiV1Routes ++ talk_record_routes:routes()','ApiV1Routes')
                elif failure=='unused_function':source=source.replace('ApiV1Routes ++ talk_record_routes:routes()','ApiV1Routes') + registered_router().replace('get_routes()', 'unused_contract_example()')
                elif failure=='fun':source=source.replace('CompiledApiRoutes = customer_service_wire(', 'CompiledApiRoutes = fun() -> customer_service_wire(').replace('talk_record_routes:routes()))),','talk_record_routes:routes()))) end,')
                elif failure=='branch':source=source.replace('CompiledApiRoutes = customer_service_wire(', 'CompiledApiRoutes = case false of true -> customer_service_wire(').replace('talk_record_routes:routes()))),','talk_record_routes:routes()))); false -> [] end,')
                elif failure=='core':source=source.replace('MainRoutes ++ CompiledApiRoutes ++ CompiledAdmRoutes','MainRoutes ++ ApiV1Routes ++ CompiledAdmRoutes')
                elif failure=='return':source=source.replace('[{Host, CoreRoutes ++ CompiledPluginRoutes}]','[{Host, MainRoutes}]')
                else:source=''
                path.write_text(source)
                result=subprocess.run(['bash',str(root/'scripts/check_rest_contract_coverage.sh')],capture_output=True,text=True)
                self.assertNotEqual(result.returncode,0)
                if failure in ['registration','unused_function','operation','suite']:
                    self.assertIn('NO-GO',result.stdout)
                inventory=root/'negative-inventory.tsv'
                scan=subprocess.run(['bash',str(root/'scripts/check_rest_contract_coverage.sh'),'--inventory',str(inventory)],capture_output=True,text=True)
                if failure in ['registration','unused_function']:
                    self.assertEqual(scan.returncode,0,scan.stdout+scan.stderr)
                    self.assertNotIn('/api/v1/talk-records/:id',inventory.read_text())
                elif failure in ['fun','branch','core','return']:
                    self.assertNotEqual(scan.returncode,0)



if __name__ == '__main__':
    unittest.main()
