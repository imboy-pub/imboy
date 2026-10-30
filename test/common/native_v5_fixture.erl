-module(native_v5_fixture).
-export([enabled/0, configure/1, assert_database/1, entry/1]).

%% The controller opts into serial fresh-VM execution; no global V5 environment
%% escapes into unrelated roots or the guard's negative unit tests.
%% 控制器显式选择逐模块新 VM 串行执行，V5 环境不外溢至无关根及守卫负例。
entry(Module) ->
    case {os:getenv("IMBOY_NATIVE_V5_CONTROL"), prefix(Module)} of
        {false, _} -> talk_record_owned_eunit_fixture:entry(Module);
        {_, undefined} -> talk_record_owned_eunit_fixture:entry(Module);
        {Receipt, Prefix} ->
            {"native-v5/" ++ atom_to_list(Module), isolated_pg_fixture:test(fun() ->
                true = os:putenv("IMBOY_NATIVE_V5_RECEIPT", Receipt),
                true = os:putenv("TEST_HTTP_PORT", "0"),
                ok = configure(Prefix),
                Info = verify("--prefix", Prefix),
                {ok, Conf} = application:get_env(imboy, pg_conf),
                {epgsql, connect, [Args]} = maps:get(start_mfa, Conf),
                Bound = Args#{host => binary_to_list(maps:get(<<"host">>, Info)),
                    port => maps:get(<<"port">>, Info),
                    username => binary_to_list(maps:get(<<"user">>, Info)),
                    password => "", database => binary_to_list(maps:get(<<"database">>, Info))},
                ok = application:set_env(imboy, pg_conf, Conf#{start_mfa => {epgsql, connect, [Bound]}}),
                ok = observations(Module),
                {module, Module}
            end, 18000)}
    end.

observations(Module) ->
    case os:getenv("IMBOY_NATIVE_V5_OBSERVATIONS_DIR") of
        false -> ok;
        Dir ->
            true = filename:pathtype(Dir) =:= absolute,
            true = filelib:is_dir(Dir),
            Path = filename:join(Dir, atom_to_list(Module) ++ "-" ++ os:getpid() ++ ".jsonl"),
            {ok, F} = file:open(Path, [write, exclusive, binary]),
            ok = file:close(F),
            ok = file:change_mode(Path, 8#600),
            true = os:putenv("V5_OBSERVATIONS_FILE", Path),
            ok
    end.

prefix(attachment_lifecycle_v5_pg_tests) -> <<"V5_INTTEST">>;
prefix(attachment_lifecycle_v5_hold_protocol_tests) -> <<"V5_INTTEST_W302">>;
prefix(attachment_lifecycle_v5_w401_entrypoints_tests) -> <<"V5_INTTEST_W401">>;
prefix(attachment_lifecycle_v5_w402_worker_tests) -> <<"V5_INTTEST_W402">>;
prefix(attachment_lifecycle_v5_w402_purged_recovery_tests) -> <<"V5_INTTEST_W402P">>;
prefix(attachment_lifecycle_v5_w403_predecessor_tests) -> <<"V5_INTTEST_W403P">>;
prefix(attachment_lifecycle_v5_w404_activation_tests) -> <<"V5_INTTEST_W404">>;
prefix(attachment_lifecycle_v5_w601_w3_bind_tests) -> <<"V5_INTTEST_W404">>;
prefix(attachment_lifecycle_v5_w601_w3_hold_tests) -> <<"V5_INTTEST_W601H">>;
prefix(attachment_lifecycle_v5_w601_w3_race_tests) -> <<"V5_INTTEST_W601R">>;
prefix(attachment_lifecycle_v5_w601_w4_activation_tests) -> <<"V5_INTTEST_W601W4ACT">>;
prefix(attachment_lifecycle_v5_w601_w4_hold_tests) -> <<"V5_INTTEST_W601W4HOLD">>;
prefix(attachment_lifecycle_v5_w601_w4_intent_tests) -> <<"V5_INTTEST_W601W4INT">>;
prefix(attachment_lifecycle_v5_w601_w4_purged_tests) -> <<"V5_INTTEST_W601W4PRG">>;
prefix(attachment_lifecycle_v5_w601_w4_queue_tests) -> <<"V5_INTTEST_W601W4QUEUE">>;
prefix(attachment_lifecycle_v5_w601_w4_race_tests) -> <<"V5_INTTEST_W601W4RACE">>;
prefix(attachment_lifecycle_v5_w601_w4_worker_tests) -> <<"V5_INTTEST_W601W4WORK">>;
prefix(moment_delete_atomic_pg_tests) -> <<"V5_INTTEST_MOMENT">>;
prefix(_) -> undefined.

%% An explicit private receipt activates this path; legacy V5 gates remain intact.
%% 仅显式私有回执启用本入口；原有 V5 守卫保持原样。
enabled() -> os:getenv("IMBOY_NATIVE_V5_RECEIPT") =/= false.

configure(Prefix) ->
    Info = verify("--prefix", Prefix),
    putenv(Prefix, "_DB", maps:get(<<"database">>, Info)),
    putenv(Prefix, "_PG_HOST", maps:get(<<"host">>, Info)),
    putenv(Prefix, "_PG_PORT", integer_to_binary(maps:get(<<"port">>, Info))),
    putenv(Prefix, "_PG_USER", maps:get(<<"user">>, Info)),
    putenv(Prefix, "_PG_PASSWORD", <<>>),
    putenv(Prefix, "_MAINT_DB", <<"postgres">>),
    true = os:putenv("V5_SLOT", binary_to_list(maps:get(<<"slot">>, Info))),
    true = os:putenv("V5_RUN_DIR", filename:dirname(os:getenv("IMBOY_NATIVE_V5_RECEIPT"))),
    ok.

assert_database(Database) ->
    _ = verify("--database", Database),
    ok.

putenv(Prefix, Suffix, Value) ->
    true = os:putenv(binary_to_list(<<Prefix/binary, (list_to_binary(Suffix))/binary>>),
        binary_to_list(Value)),
    ok.

verify(Flag, Value) when is_list(Value) -> verify(Flag, list_to_binary(Value));
verify(Flag, Value) when is_binary(Value) ->
    Receipt = os:getenv("IMBOY_NATIVE_V5_RECEIPT"),
    true = is_list(Receipt),
    Script = filename:join([filename:dirname(filename:dirname(code:which(?MODULE))),
        "scripts", "ci", "native-v5-fixture.py"]),
    Python = os:find_executable("python3"),
    true = is_list(Python),
    Port = open_port({spawn_executable, Python}, [binary, exit_status, use_stdio,
        stderr_to_stdout, {args, [Script, Receipt, Flag, binary_to_list(Value)]}]),
    receive_result(Port, <<>>).

receive_result(Port, Acc) ->
    receive
        {Port, {data, Bytes}} when byte_size(Acc) + byte_size(Bytes) < 16384 ->
            receive_result(Port, <<Acc/binary, Bytes/binary>>);
        {Port, {exit_status, 0}} -> jsone:decode(Acc);
        {Port, {exit_status, Code}} -> error({native_fixture_admission_refused, Code})
    after 15000 ->
        catch port_close(Port),
        error(native_fixture_admission_timeout)
    end.
