-module(textguard_suite_fixture).
-export([test/2, preflight/1, require_owned_pool/0]).

%% Official launchers already own a fresh VM and need the original leaf receipts.
%% Full EUnit instead executes the entire lifecycle in a fresh peer.
test(Kind, Generator) ->
    Fixture = fun() ->
        {setup, fun() -> setup(Kind) end,
            fun cleanup/1,
            fun(_) -> Generator() end}
    end,
    case os:getenv("TEXTGUARD_TEST_OWN_VM") of
        "1" -> Fixture();
        false -> isolated_pg_fixture:test(Fixture);
        Other -> error({invalid_textguard_own_vm, Other})
    end.

setup(Kind) ->
    preflight_resources(Kind),
    false = lists:keymember(imboy, 1, application:which_applications()),
    undefined = whereis(pgsql),
    Service = start_service(),
    try
        preflight(Kind),
        Options = case Kind of
            integration -> #{};
            _ -> #{schema_mode => empty,
                connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}}
        end,
        Pg = textguard_export_tests:owned_pg_setup(Options),
        try
            case Kind of
                integration -> textguard_export_tests:trunc();
                _ -> ok
            end,
            #{pg => Pg, service => Service}
        catch
            Class0:Reason0:Stack0 ->
                textguard_export_tests:owned_pg_teardown(Pg),
                erlang:raise(Class0, Reason0, Stack0)
        end
    catch
        Class:Reason:Stack ->
            stop_service(Service),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{pg := Pg, service := Service}) ->
    try stop_service(Service)
    after textguard_export_tests:owned_pg_teardown(Pg)
    end.

%% Read-only checks precede application/pool changes and database provisioning.
preflight(Kind) ->
    preflight_resources(Kind),
    [required(Name) || Name <- ["TG19_HTTP_URL", "TG19_MANAGE_URL", "TG19_ENH_FILE"]],
    [port(Name) || Name <- ["TG19_GRPC_PORT", "TG19_FAULT_GRPC_PORT"]],
    directory("TG19_ENH_FILE", filename:dirname(required("TG19_ENH_FILE"))),
    ok.

preflight_resources(Kind) ->
    [required(Name) || Name <- ["TG16_PG_HOST", "TG16_PG_USER", "TG19_EVIDENCE"]],
    %% An explicitly empty password is valid for a task-owned trust-auth PG.
    present("TG16_PG_PASSWORD"),
    port("TG16_PG_PORT"),
    [readable(Name) || Name <- ["TEXTGUARD_CHECK_SECRET_FILE", "TEXTGUARD_PUBLISH_SECRET_FILE"]],
    directory("TG19_EVIDENCE", required("TG19_EVIDENCE")),
    case Kind of
        integration -> ok;
        _ ->
            [readable(Name) || Name <- ["TG20_SCHEMA_FILE", "TG20_MIGRATION_UP",
                "TG20_MIGRATION_DOWN"]],
            ok
    end.

required(Name) ->
    case os:getenv(Name) of
        false -> error({textguard_missing_env, Name});
        "" -> error({textguard_missing_env, Name});
        Value -> Value
    end.

port(Name) ->
    case string:to_integer(required(Name)) of
        {N, []} when N > 0, N < 65536 -> ok;
        _ -> error({textguard_invalid_port, Name})
    end.

readable(Name) ->
    case file:read_file(required(Name)) of
        {ok, Bytes} when byte_size(Bytes) > 0 -> ok;
        {ok, _} -> error({textguard_empty_file, Name});
        {error, Reason} -> error({textguard_unreadable_file, Name, Reason})
    end.

directory(Name, Path) ->
    case filelib:is_dir(Path) of
        true -> ok;
        false -> error({textguard_missing_directory, Name})
    end.

require_owned_pool() ->
    case {application:get_env(imboy, tg16_fixture_conn_opts),
        application:get_env(imboy, tg16_fixture_pool_owner), whereis(pgsql),
        lists:keymember(imboy, 1, application:which_applications())} of
        {{ok, #{database := Db}}, {ok, #{pid := Pid, db := Db}}, Pid, false}
            when is_pid(Pid) ->
            case Db of
                <<"inttest_tg16_", _/binary>> -> ok;
                _ -> error(textguard_owned_pool_required)
            end;
        _ -> error(textguard_owned_pool_required)
    end.

present(Name) ->
    case os:getenv(Name) of
        false -> error({textguard_missing_env, Name});
        Value -> Value
    end.

%% The official launcher owns its service. Automatic full-suite peers must own a
%% separate real process and data directory so generation-zero assertions remain real.
start_service() ->
    case os:getenv("TEXTGUARD_TEST_OWN_VM") of
        "1" -> launcher_owned;
        false ->
            Binary = required("TEXTGUARD_TEST_BINARY"),
            case filelib:is_regular(Binary) of
                true -> ok;
                false -> error({textguard_binary_unavailable, "TEXTGUARD_TEST_BINARY"})
            end,
            Python = case os:find_executable("python3") of
                false -> error(textguard_python3_required);
                Path -> Path
            end,
            Script = filename:absname("test/support/textguard_service_owner.py"),
            true = filelib:is_regular(Script),
            Parent = self(),
            Ref = make_ref(),
            {Owner, Monitor} = spawn_monitor(fun() -> service_owner(Parent, Ref, Python, Script) end),
            receive
                {Ref, {ready, Config}} ->
                    erlang:demonitor(Monitor, [flush]),
                    configure_service(Config),
                    Owner;
                {Ref, {error, Reason}} ->
                    erlang:demonitor(Monitor, [flush]),
                    error({textguard_owned_service_failed, Reason});
                {'DOWN', Monitor, process, Owner, _} -> error(textguard_service_owner_exited)
            after 20000 ->
                Owner ! {stop, self(), Ref},
                error(textguard_owned_service_start_timeout)
            end
    end.

configure_service(Config) ->
    [true = os:putenv(Name, binary_to_list(maps:get(Key, Config))) || {Name, Key} <- [
        {"TG19_HTTP_URL", <<"http">>}, {"TG19_MANAGE_URL", <<"manage">>},
        {"TG19_ENH_FILE", <<"enhancement_file">>}]],
    [true = os:putenv(Name, integer_to_list(maps:get(Key, Config))) || {Name, Key} <- [
        {"TG19_GRPC_PORT", <<"grpc_port">>}, {"TG19_FAULT_GRPC_PORT", <<"fault_port">>}]],
    ok.

service_owner(Parent, Ref, Python, Script) ->
    Port = open_port({spawn_executable, Python}, [binary, exit_status, use_stdio,
        {line, 65536}, {args, [Script]}]),
    try
        receive
            {Port, {data, {eol, Line}}} ->
                case jsone:decode(Line) of
                    #{<<"ready">> := true} = Config ->
                        Parent ! {Ref, {ready, Config}},
                        service_wait(Port);
                    _ -> error(service_start_failed)
                end;
            {Port, {exit_status, _}} -> error(service_start_failed)
        after 18000 -> error(service_start_timeout)
        end
    catch
        _:_ -> Parent ! {Ref, {error, service_process_failed}}
    after
        catch port_close(Port)
    end.

service_wait(Port) ->
    receive
        {stop, Caller, Ref} ->
            true = port_command(Port, <<"stop\n">>),
            Result = receive
                {Port, {exit_status, 0}} -> ok;
                {Port, {exit_status, _}} -> {error, service_owner_failed}
            after 10000 -> {error, service_stop_timeout}
            end,
            Caller ! {Ref, Result};
        {Port, {exit_status, _}} -> ok
    end.

stop_service(launcher_owned) -> ok;
stop_service(Owner) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Owner),
    Owner ! {stop, self(), Ref},
    receive
        {Ref, ok} -> erlang:demonitor(Monitor, [flush]), ok;
        {Ref, {error, Reason}} -> erlang:demonitor(Monitor, [flush]), error(Reason);
        {'DOWN', Monitor, process, Owner, _} -> error(textguard_owned_service_exited)
    after 12000 -> error(textguard_owned_service_stop_timeout)
    end.
