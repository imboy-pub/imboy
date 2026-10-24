%%% Task-owned real PG entrypoint. Rebinds fixture only; original test bodies,
%%% instantiators and timeouts remain intact. Never starts imboy/eunit_runner.
-module(personal_order_owned_pg_tests).
%% Explicit fresh-VM entry; deliberately not auto-discovered in full App EUnit.
-export([checks/0]).
-include_lib("eunit/include/eunit.hrl").

checks() ->
    {timeout, 1000,
        {setup, fun setup/0, fun cleanup/1, fun(S) ->
            {inorder, acceptance(maps:get(conn, S)) ++ [delivery(maps:get(conn, S))]}
        end}}.

acceptance(C) ->
    Mod = personal_c2c_acceptance_order_db_tests,
    Generators = [
        acceptance_order_test_,
        rollback_does_not_consume_sequence_test_,
        unrelated_unique_constraint_remains_error_test_,
        cross_identity_conflict_test_,
        stored_identity_conflict_test_,
        same_identity_concurrent_retry_test_,
        cross_conversation_insert_race_test_
    ],
    Raw = [apply(Mod, G, []) || G <- Generators],
    ?assertEqual(8, fixture_count(Raw)),
    [rebind(D, {ok, C}, 1) || D <- Raw].

delivery(C) ->
    rebind(personal_order_delivery_pg_tests:delivery_order_test_(), C, 15).

%% Only the known original descriptor shapes are accepted. No generic skip or
%% arbitrary fallback rewriting: source changes require a reviewed adapter.
rebind(List, State, Count) when is_list(List), List =/= [] ->
    [rebind(D, State, Count) || D <- List];
rebind({timeout, T, D}, State, Count) ->
    {timeout, T, rebind(D, State, Count)};
rebind({setup, OldSetup, OldCleanup, Instantiate}, State, Count) when
    is_function(OldSetup, 0), is_function(OldCleanup, 1), is_function(Instantiate, 1)
->
    {setup, fun() -> State end, fun(_) -> ok end, fun(Value) ->
        Tests = Instantiate(Value),
        ?assertEqual(Count, test_count(Tests)),
        Tests
    end};
rebind(Other, _, _) ->
    erlang:error({unrecognized_original_fixture, Other}).

fixture_count(L) when is_list(L) -> lists:sum([fixture_count(D) || D <- L]);
fixture_count({setup, _, _, _}) -> 1;
fixture_count(Other) -> erlang:error({unexpected_acceptance_descriptor, Other}).

test_count(L) when is_list(L), L =/= [] -> lists:sum([test_count(D) || D <- L]);
test_count({inorder, D}) -> test_count(D);
test_count({timeout, _, D}) -> test_count(D);
test_count({Label, D}) when is_integer(Label); is_list(Label) -> test_count(D);
test_count(F) when is_function(F, 0) -> 1;
test_count(Other) -> erlang:error({empty_or_unrecognized_original_tests, Other}).

setup() ->
    assert_unowned(),
    ok = ensure_loaded(),
    Before = [
        {K, application:get_env(imboy, K)}
     || K <-
            [sql_driver, pg_conf, super_account, env, msg_store_worker_tick_ms]
    ],
    put(owned_order_setup, #{before => Before, apps => []}),
    try
        Opts = connection_options(),
        start_apps([crypto, inets, epgsql, pooler, depcache, jsone]),
        {ok, C} = epgsql:connect(Opts),
        own(conn, C),
        verify_target(C),
        start_pool(Opts),
        ok = application:set_env(imboy, super_account, Opts),
        ok = application:set_env(imboy, sql_driver, pgsql),
        ok = application:set_env(imboy, env, test),
        ok = application:set_env(imboy, msg_store_worker_tick_ms, 0),
        ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
        own(tsid, true),
        {ok, Cache} = imboy_cache:start_link([]),
        unlink(Cache),
        own(cache, Cache),
        %% init only CREATE TABLE/INDEX IF NOT EXISTS on the verified current
        %% schema; no ALTER/DROP. Required for real asynchronous unstage.
        {ok, Producer} = msg_store_ds:start_link(),
        unlink(Producer),
        own(producer, Producer),
        {ok, Worker} = msg_store_worker:start_link(),
        unlink(Worker),
        own(worker, Worker),
        erase(owned_order_setup)
    catch
        Class:Reason:Stack ->
            %% Cleanup diagnostics must never replace the original setup failure.
            Errors = cleanup_errors(erase(owned_order_setup)),
            case Errors of
                [] -> ok;
                _ -> io:format("OWNED_ORDER_SETUP_CLEANUP ~p~n", [Errors])
            end,
            erlang:raise(Class, Reason, Stack)
    end.

assert_unowned() ->
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())),
    lists:foreach(
        fun(Name) -> ?assertEqual(undefined, whereis(Name)) end,
        [
            pgsql,
            pooler_pgsql_pool_sup,
            imboy_cache,
            msg_store_ds,
            msg_store_worker,
            eunit_boot_coordinator
        ]
    ),
    ?assertEqual(not_initialized, elib_tsid:readiness()).

ensure_loaded() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end.

connection_options() ->
    Host = required("HOST"),
    Port = list_to_integer(required("PORT")),
    Db = required("DB"),
    Run = required("RUN_ID"),
    ?assertEqual("127.0.0.1", Host),
    ?assertEqual("imboy-delivery-v3", Run),
    ?assert(Port > 0 andalso Port =< 65535),
    ?assert(Db =:= "delivery_v3_order" orelse lists:prefix("delivery_v3_order_", Db)),
    Password =
        case os:getenv("OWNED_ORDER_PG_PASSWORD") of
            false -> error({required_owned_order_env, "PASSWORD"});
            Value -> Value
        end,
    #{
        host => Host,
        port => Port,
        username => required("USER"),
        password => Password,
        database => Db,
        timeout => 5000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

verify_target(C) ->
    Db = list_to_binary(required("DB")),
    SysId = list_to_binary(required("SYSTEM_ID")),
    Comment = <<"Owned by imboy-delivery-v3; synthetic personal order test data only">>,
    {ok, _, [{Db, SysId, Comment}]} = epgsql:equery(
        C,
        "SELECT current_database(), (SELECT system_identifier::text FROM pg_control_system()),"
        " shobj_description(oid, 'pg_database') FROM pg_database WHERE datname=current_database()",
        []
    ),
    ?assertEqual(
        {ok, inttest_marker_db:migration_head(), false},
        erlang_migrate:version(#{conn => C, dir => "priv/migrations"})
    ),
    {ok, _, [{0}]} = epgsql:equery(
        C,
        "SELECT (SELECT count(*) FROM public.msg_store_staging) +"
        " (SELECT count(*) FROM public.msg_store) + (SELECT count(*) FROM public.msg_c2c) +"
        " (SELECT count(*) FROM public.msg_store_seq)",
        []
    ),
    io:format("OWNED_ORDER_TARGET ~s~n", [
        jsone:encode(#{
            database => Db,
            system_id => SysId,
            run_id => <<"imboy-delivery-v3">>,
            expected_tests => 23
        })
    ]).

start_pool(Opts) ->
    Conf = #{
        name => pgsql,
        init_count => 8,
        max_count => 16,
        start_mfa => {epgsql, connect, [Opts]}
    },
    ok = application:set_env(imboy, pg_conf, Conf),
    {ok, Sup} = pooler:new_pool(Conf),
    own(pool_sup, Sup),
    [{pooler, Worker, worker, [pooler]}] =
        [Ch || Ch = {pooler, _, _, _} <- supervisor:which_children(Sup)],
    own(pool_worker, Worker),
    ?assertEqual(Sup, whereis(pooler_pgsql_pool_sup)),
    ?assertEqual(Worker, whereis(pgsql)).

start_apps(Apps) ->
    lists:foreach(
        fun(App) ->
            {ok, Started} = application:ensure_all_started(App),
            S = get(owned_order_setup),
            put(owned_order_setup, S#{apps => lists:reverse(Started) ++ maps:get(apps, S)})
        end,
        Apps
    ).

own(Key, Value) ->
    put(owned_order_setup, (get(owned_order_setup))#{Key => Value}),
    ok.

required(Suffix) ->
    case os:getenv("OWNED_ORDER_PG_" ++ Suffix) of
        false -> error({required_owned_order_env, Suffix});
        "" -> error({required_owned_order_env, Suffix});
        Value -> Value
    end.

cleanup(S) ->
    case cleanup_errors(S) of
        [] -> ok;
        Errors -> erlang:error({owned_order_cleanup_failed, Errors})
    end.

cleanup_errors(S) when is_map(S) ->
    Actions =
        [
            {worker, fun() -> stop_owned(msg_store_worker, maps:get(worker, S, undefined)) end},
            {producer, fun() -> stop_owned(msg_store_ds, maps:get(producer, S, undefined)) end},
            {cache, fun() -> stop_owned(imboy_cache, maps:get(cache, S, undefined)) end},
            {pool, fun() -> stop_pool(S) end},
            {connection, fun() -> close_conn(maps:get(conn, S, undefined)) end},
            {tsid, fun() ->
                case maps:get(tsid, S, false) of
                    true -> elib_tsid:reset_for_test();
                    false -> ok
                end
            end}
        ] ++ [{{env, K}, fun() -> restore(K, V) end} || {K, V} <- maps:get(before, S, [])],
    Errors = run_cleanup(Actions),
    %% If ownership changed, preserve dependency apps rather than shutting down
    %% a replacement/foreign resource through its application supervisor.
    case Errors of
        [] ->
            run_cleanup([
                {{app, App}, fun() -> application:stop(App) end}
             || App <- maps:get(apps, S, [])
            ]);
        _ ->
            Errors ++ [{dependency_apps_retained, maps:get(apps, S, [])}]
    end;
cleanup_errors(_) ->
    [].

run_cleanup(Actions) ->
    lists:filtermap(
        fun({Name, F}) ->
            try F() of
                ok -> false;
                {error, {not_started, _}} -> false;
                Other -> {true, {Name, unexpected_cleanup_result, Other}}
            catch
                Class:Reason -> {true, {Name, Class, Reason}}
            end
        end,
        Actions
    ).

stop_pool(S) ->
    case maps:get(pool_sup, S, undefined) of
        Sup when is_pid(Sup) ->
            Worker = maps:get(pool_worker, S, undefined),
            ?assertEqual(Sup, whereis(pooler_pgsql_pool_sup)),
            ?assert(is_pid(Worker)),
            ?assertEqual(Worker, whereis(pgsql)),
            ?assert(
                lists:member(
                    {pooler, Worker, worker, [pooler]},
                    supervisor:which_children(Sup)
                )
            ),
            pooler:rm_pool(pgsql);
        _ ->
            ok
    end.

close_conn(C) when is_pid(C) -> epgsql:close(C);
close_conn(_) -> ok.

stop_owned(Name, Pid) when is_pid(Pid) ->
    case whereis(Name) of
        Pid -> gen:stop(Pid, normal, 5000);
        undefined -> ok;
        _ -> error({resource_owner_changed, Name})
    end;
stop_owned(_, _) ->
    ok.
restore(K, {ok, V}) -> application:set_env(imboy, K, V);
restore(K, undefined) -> application:unset_env(imboy, K).
