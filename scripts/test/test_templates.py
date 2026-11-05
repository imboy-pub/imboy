import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
MAKE = shutil.which("gmake") or shutil.which("make")
TEMPLATES = {"rest_handler": "api", "logic": "logic", "ds": "ds", "repository": "repo"}


class TemplateTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temporary = tempfile.TemporaryDirectory(prefix="imboy-template-test-")
        cls.addClassCleanup(cls.temporary.cleanup)
        cls.root = Path(cls.temporary.name)
        cls.environment = {
            name: value for name, value in os.environ.items()
            if name in ("PATH", "HOME", "TMPDIR", "LANG")
        }
        cls.environment["ERL_CRASH_DUMP"] = str(cls.root / "erl_crash.dump")
        for name in ("erlang.mk", "include/tpl.mk", "include/log.hrl", "include/error_code.hrl",
                     "src/lib/elib_req.erl"):
            cls.write(name, (ROOT / name).read_text())
        cls.write("Makefile", "PROJECT = template_probe\ninclude erlang.mk\ninclude include/tpl.mk\n")
        for template, layer in TEMPLATES.items():
            (cls.root / "src" / layer).mkdir(parents=True, exist_ok=True)
            suffix = "repo" if template == "repository" else "handler" if template == "rest_handler" else template
            cls.command(MAKE, "new", f"t=imboy.{template}", f"n=sample_{suffix}")
        fixtures = {
            "cowboy_handler": '''-callback init(map(), map()) -> {ok, map(), map()}.
''',
            "cowboy_req": '''-export([method/1, set_resp_header/3, read_body/1, header/3]).
-export_type([req/0]).
-type req() :: map().
method(Req) -> maps:get(method, Req).
set_resp_header(Name, Value, Req) -> Req#{Name => Value}.
read_body(Req) -> {ok, maps:get(body, Req, <<>>), Req#{body_read => true}}.
header(Name, Req, Default) -> maps:get(Name, Req, Default).
''',
            "elib_response": '''-export([handle_logic_result/2, error_with_status/4]).
handle_logic_result(Req, Result) -> Req#{result => Result}.
error_with_status(Req, Status, Msg, Code) -> Req#{status => Status, result => {error, Msg, Code}}.
''',
            "elib_pg": '''-export([query/2, execute/2]).
query(Sql, Params) ->
    put(last_query, {Sql, Params}),
    case get(query_result) of crash -> error(synthetic_failure); Result -> Result end.
execute(Sql, Params) -> put(last_execute, {Sql, Params}), get(execute_result).
''',
            "elib_pg_sql": '''-export([public_tablename_quoted/1]).
public_tablename_quoted(Table) -> <<"public.", 34, Table/binary, 34>>.
''',
            "elib_tsid": '''-export([generate/1]).
generate(Namespace) -> put(tsid_namespace, Namespace), 123456789.
''',
            "elib_log": '''-export([internal_log/5]).
internal_log(_Level, _Format, _Args, _Module, _Line) -> ok.
''',
            "jsone": '''-export([encode/1, decode/1, decode/2]).
encode(Data) -> iolist_to_binary(json:encode(Data)).
decode(Data) -> json:decode(Data).
decode(Data, _Options) -> decode(Data).
''',
        }
        for module, body in fixtures.items():
            cls.write(f"fixtures/{module}.erl", f"-module({module}).\n{body}")
        (cls.root / "ebin").mkdir()
        cls.command("erlc", "-Werror", "-o", "ebin", "fixtures/cowboy_handler.erl")
        sources = [str(path.relative_to(cls.root)) for path in (cls.root / "fixtures").glob("*.erl")]
        sources += ["src/lib/elib_req.erl"]
        sources += [str(path.relative_to(cls.root)) for path in (cls.root / "src").glob("*/*.erl")
                    if path.parent.name != "lib"]
        cls.command("erlc", "-Werror", "-DTEST=1", "-pa", "ebin", "-I", "include", "-o", "ebin", *sources)

    @classmethod
    def write(cls, name, content):
        path = cls.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path

    @classmethod
    def command(cls, *arguments, success=True):
        result = subprocess.run(arguments, cwd=cls.root, env=cls.environment, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=60)
        if (result.returncode == 0) != success:
            raise AssertionError(f"{arguments}: exit={result.returncode}\n{result.stdout}")
        return result

    def erl(self, expression):
        return self.command("erl", "-noshell", "-pa", "ebin", "-eval", expression + ", halt(0).")

    def test_all_templates_generate_into_layers_and_preserve_existing_files(self):
        for template, layer in TEMPLATES.items():
            suffix = "repo" if template == "repository" else "handler" if template == "rest_handler" else template
            path = self.root / "src" / layer / f"sample_{suffix}.erl"
            before = path.read_bytes()
            self.command(MAKE, "new", f"t=imboy.{template}", f"n=sample_{suffix}", success=False)
            self.assertEqual(path.read_bytes(), before)

    def test_only_trailing_layer_suffix_is_removed(self):
        self.command(MAKE, "new", "t=imboy.rest_handler", "n=nested_handler_sample_handler")
        text = (self.root / "src/api/nested_handler_sample_handler.erl").read_text()
        self.assertIn("nested_handler_sample_logic:demo", text)

    def test_production_compiles_with_only_public_layer_interfaces(self):
        (self.root / "production").mkdir()
        paths = [str(path.relative_to(self.root)) for path in (self.root / "src").glob("*/*.erl")
                 if path.parent.name != "lib"]
        self.command("erlc", "-Werror", "-pa", "ebin", "-I", "include", "-o", "production", *paths)
        self.command("erl", "-noshell", "-pa", "production", "-eval", '''
            lists:foreach(fun({Module, Expected}) ->
                Exports = [{Name, Arity} || {Name, Arity} <- Module:module_info(exports),
                    Name =/= module_info],
                true = lists:sort(Expected) =:= lists:sort(Exports)
            end, [{sample_handler, [{init, 2}]},
                {sample_logic, [{demo, 3}]}, {sample_ds, [{demo, 3}]},
                {sample_repo, [{tablename, 0}, {demo, 3}, {create, 2},
                    {find_by_id, 2}, {update, 3}, {delete, 2}]}]), halt(0).''')

    def test_generated_eunit_runs_with_native_test_define(self):
        result = self.erl('''
            {module, sample_logic} = code:ensure_loaded(sample_logic),
            true = erlang:function_exported(sample_logic, demo_invalid_params_test, 0),
            ok = eunit:test([sample_logic, sample_ds, sample_repo], [verbose])''')
        self.assertIn("All 3 tests passed", result.stdout)

    def test_four_layer_flow_preserves_sql_parameters_and_read_request(self):
        self.erl('''
            put(query_result, {ok, [#{<<"id">> => 7}]}),
            Req = #{method => <<"POST">>, body => <<"{\\"val1\\":\\"same\\",\\"val2\\":\\"same\\"}">>},
            {ok, #{body_read := true, result := {ok, [#{<<"id">> := 7}]}}, #{current_uid := 42}} =
                sample_handler:init(Req, #{action => demo_action, current_uid => 42}),
            {Sql, [42, <<"same">>, <<"same">>]} = get(last_query),
            {_, _} = binary:match(Sql, <<"created_by = $1">>),
            {_, _} = binary:match(Sql, <<"data->>'val1' = $2">>),
            {_, _} = binary:match(Sql, <<"data->>'val2' = $3">>),
            {_, _} = binary:match(Sql, <<"ORDER BY id DESC LIMIT 20">>),
            <<"public.", 34, "sample", 34>> = sample_repo:tablename()''')

    def test_http_auth_method_and_unknown_action_stop_before_database(self):
        self.erl('''
            lists:foreach(fun(Uid) ->
                {ok, #{status := 401}, _} =
                    sample_handler:init(#{}, #{action => demo_action, current_uid => Uid})
            end, [0, -1, <<"1">>, undefined]),
            {ok, #{status := 401}, _} = sample_handler:init(#{}, #{action => demo_action}),
            {ok, #{status := 405, <<"allow">> := <<"POST">>}, _} =
                sample_handler:init(#{method => <<"GET">>}, #{action => demo_action, current_uid => 1}),
            {ok, #{status := 404}, _} = sample_handler:init(#{}, #{action => unknown}),
            {ok, #{status := 404}, _} = sample_handler:init(#{}, #{}),
            undefined = get(last_query)''')

    def test_validation_is_shared_and_errors_do_not_leak_database_details(self):
        self.erl('''
            Value = binary:copy(<<"a">>, 255),
            lists:foreach(fun({Uid, Val1, Val2}) ->
                {error, invalid_parameters} = sample_ds:demo(Uid, Val1, Val2),
                {error, _, 400} = sample_logic:demo(Uid, Val1, Val2)
            end, [{0, Value, Value}, {-1, Value, Value}, {<<"1">>, Value, Value},
                {1, 123, <<"b">>}, {1, <<>>, Value}, {1, Value, <<>>},
                {1, binary:copy(<<"a">>, 256), Value}, {1, Value, binary:copy(<<"b">>, 256)}]),
            undefined = get(last_query),
            put(query_result, {ok, []}),
            {ok, []} = sample_logic:demo(1, Value, Value),
            {_, [1, Value, Value]} = get(last_query),
            put(query_result, {error, <<"synthetic secret SQL detail">>}),
            {error, Msg, 500} = sample_logic:demo(1, <<"a">>, <<"b">>),
            nomatch = binary:match(Msg, <<"secret">>),
            put(query_result, crash),
            Req = #{method => <<"POST">>, body => <<"{\\"val1\\":\\"a\\",\\"val2\\":\\"b\\"}">>},
            {ok, #{status := 500, body_read := true, result := {error, <<"internal server error">>, 500}}, _} =
                sample_handler:init(Req, #{action => demo_action, current_uid => 1})''')

    def test_crud_preserves_placeholders_tsid_and_write_results(self):
        self.erl('''
            put(query_result, {ok, [#{<<"id">> => 123456789}]}),
            {ok, 123456789} = sample_repo:create(#{<<"val1">> => <<"a">>}, 42),
            sample = get(tsid_namespace),
            {Insert, [123456789, Encoded, 42]} = get(last_query),
            #{<<"val1">> := <<"a">>} = jsone:decode(Encoded),
            {_, _} = binary:match(Insert, <<"VALUES ($1, $2, $3, NOW()) RETURNING id">>),
            put(execute_result, {ok, 1}), ok = sample_repo:update(7, #{}, 42),
            {Update, [_, 42, 7]} = get(last_execute),
            {_, _} = binary:match(Update, <<"WHERE id = $3 AND created_by = $2">>),
            ok = sample_repo:delete(7, 42),
            {Delete, [7, 42]} = get(last_execute),
            {_, _} = binary:match(Delete, <<"WHERE id = $1 AND created_by = $2">>),
            put(execute_result, {ok, 0}),
            {error, not_found} = sample_repo:update(7, #{}, 42),
            {error, not_found} = sample_repo:delete(7, 42),
            put(execute_result, {error, synthetic_db_error}),
            {error, synthetic_db_error} = sample_repo:delete(7, 42),
            put(query_result, {error, synthetic_db_error}),
            {error, synthetic_db_error} = sample_repo:create(#{}, 42)''')

    def test_find_handles_missing_rows_errors_and_json_codec_variants(self):
        self.erl('''
            put(query_result, {ok, []}), {error, not_found} = sample_repo:find_by_id(7, 42),
            {Select, [7, 42]} = get(last_query),
            {_, _} = binary:match(Select, <<"WHERE id = $1 AND created_by = $2">>),
            lists:foreach(fun(Data) ->
                put(query_result, {ok, [#{<<"id">> => 7, <<"data">> => Data,
                    <<"created_by">> => 42, <<"created_at">> => synthetic_timestamp}]}),
                {ok, #{<<"id">> := 7, <<"data">> := #{<<"val1">> := <<"a">>},
                    <<"created_by">> := 42, <<"created_at">> := synthetic_timestamp}} =
                    sample_repo:find_by_id(7, 42)
            end, [#{<<"val1">> => <<"a">>}, <<"{\\"val1\\":\\"a\\"}">>]),
            put(query_result, {error, synthetic_db_error}),
            {error, synthetic_db_error} = sample_repo:find_by_id(7, 42)''')

    def test_invalid_crud_inputs_never_reach_database_or_allocate_id(self):
        self.erl('''
            lists:foreach(fun(Invalid) ->
                {error, invalid_parameters} = sample_repo:find_by_id(Invalid, 42),
                {error, invalid_parameters} = sample_repo:find_by_id(7, Invalid),
                {error, invalid_parameters} = sample_repo:delete(Invalid, 42),
                {error, invalid_parameters} = sample_repo:delete(7, Invalid),
                {error, invalid_parameters} = sample_repo:update(Invalid, #{}, 42),
                {error, invalid_parameters} = sample_repo:update(7, #{}, Invalid),
                {error, invalid_parameters} = sample_repo:create(#{}, Invalid)
            end, [0, -1, <<"1">>, undefined]),
            {error, invalid_parameters} = sample_repo:create([], 42),
            {error, invalid_parameters} = sample_repo:update(7, [], 42),
            undefined = get(last_query), undefined = get(last_execute),
            undefined = get(tsid_namespace)''')


if __name__ == "__main__":
    unittest.main()
