%% Shared Garage/PG owner extracted from the pre-retirement fixture at
%% 41dac488bcf310b5cb72ece89d72579cd9463e69^:
%% test/lib/attachment_lifecycle_v5_w403_gateway_protocol_tests.erl.
%% 共享 Garage/PG 夹具：保留资源身份检查与归属清理，不恢复已退役协议。
%% The retired gateway listener, disk roots and protocol tests are not restored.
%% Callers own their current HTTP routes; this module has no EUnit entry point.
-module(attachment_w403_owned_fixture).
-export([setup/0, cleanup/1, assert_fixture_database/4, assert_chain_fixture_database/4]).
-include_lib("kernel/include/file.hrl").

setup() ->
    %% EUnit may already have booted imboy. Journal only resources we create,
    %% including partial setup, and leave pre-existing pool/cache/TSID intact.
    %% Load before snapshotting: otherwise cleanup would unset config-file env
    %% (e.g. sql_driver) that a later setup in the same VM still needs.
    ok = ensure_loaded(imboy),
    Before = #{env => application:get_all_env(imboy)},
    erlang:put(w403_owned_fixture_setup, #{w403_before => Before, owned_apps => []}),
    try
        S = setup_owned(),
        Resources = erlang:erase(w403_owned_fixture_setup),
        maps:merge(Resources, S)
    catch
        Class:Reason:Stack ->
            cleanup(erlang:erase(w403_owned_fixture_setup)),
            erlang:raise(Class, Reason, Stack)
    end.

setup_owned() ->
    {ok, ManifestBytes} = file:read_file(required("W403_RESOURCE_MANIFEST")),
    #{<<"database">> := Db} = Manifest = jsone:decode(ManifestBytes),
    {ok, [Config]} = file:consult(required("W403_CONFIG_PATH")),
    Env = proplists:get_value(imboy, Config),
    ok = assert_fixture_database(Db, Manifest, Env, required("W403_RESOURCE_MANIFEST")),
    %% elib_pg uses the global pgsql name. Reuse only the exact manifest DB;
    %% never redirect, remove or replace a pre-existing pool.
    case whereis(pgsql) of
        undefined -> ok;
        _ -> assert_database(Db)
    end,
    [application:set_env(imboy, K, V) || {K, V} <- Env],
    {ok, Pg0} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [Connect0]} = maps:get(start_mfa, Pg0),
    Connect = Connect0#{database => binary_to_list(Db)},
    %% Standalone t= VM：eunit 只把 ebin 放进 code path，不 load/启动 app；
    %% config_ds:env(sql_driver) 依赖 application env，不 load 会返回 undefined
    %% → with_tx 取池名 undefined（r4 standalone 实证 24 例 502）。
    ok = ensure_loaded(imboy),
    ensure_started(inets),
    ensure_started(crypto),
    application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
    ensure_started(epgsql),
    ensure_started(pooler),
    case whereis(pgsql) of
        undefined ->
            {ok, Pool} = pooler:new_pool(Pg0#{
                name => pgsql,
                start_mfa => {epgsql, connect, [Connect]}
            }),
            own_resource(owned_pool, Pool);
        _ ->
            ok
    end,
    assert_database(Db),
    case whereis(imboy_cache) of
        undefined ->
            {ok, Cache} = imboy_cache:start_link([]),
            unlink(Cache),
            own_resource(owned_cache, Cache);
        _ ->
            ok
    end,
    EP = list_to_binary(required("W202_GARAGE_ENDPOINT")),
    Bucket = list_to_binary(required("W202_GARAGE_BUCKET")),
    application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => list_to_binary(required("W202_GARAGE_ACCESS_KEY")),
        secret_key => list_to_binary(required("W202_GARAGE_SECRET_KEY"))
    }),
    %% V3-20：disk 驱动退役，disk root 与 run-owned 目录随驱动删除，registry 只装 garage 根。
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"w403-garage-root">> => #{
                driver => <<"garage">>,
                namespace => <<"w403-garage">>,
                endpoint => EP,
                container => Bucket
            }
        }
    }),
    ensure_started(cowboy),
    case elib_tsid:readiness() of
        not_initialized ->
            ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
            own_resource(owned_tsid, true);
        _ ->
            ok
    end,
    Uid = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    {ok, _} = elib_pg:query(
        <<"INSERT INTO public.\"user\"(id,account,nickname,password,status,account_type,reg_ip,reg_cosv,created_at) VALUES($1,$2,'fixture','',1,0,'127.0.0.1','w403',now()) RETURNING id">>,
        [Uid, <<"w403-", (integer_to_binary(Uid))/binary>>]
    ),
    #{
        uid => Uid,
        bucket => Bucket,
        run => binary:encode_hex(crypto:strong_rand_bytes(12), lowercase)
    }.

own_resource(Key, Value) ->
    erlang:put(w403_owned_fixture_setup, (erlang:get(w403_owned_fixture_setup))#{Key => Value}),
    ok.
ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok
    end.

ensure_started(App) ->
    {ok, Started} = application:ensure_all_started(App),
    S = erlang:get(w403_owned_fixture_setup),
    %% ensure_all_started returns dependency-first order; stop dependents first.
    own_resource(owned_apps, lists:reverse(Started) ++ maps:get(owned_apps, S)).

%% Ordinary native-CI bases have their own namespace. Unlike legacy V5 review
%% manifests, they require private provenance and a live read-only identity check.
%% 原生 CI 普通主库使用独立命名空间；必须先核私有清单与实时身份，不只认前缀。
assert_chain_fixture_database(<<"attv5_chain_", _/binary>>, _, _, _) ->
    ok;
assert_chain_fixture_database(<<"imboy_chain_ci_", _/binary>> = Db, Manifest, Env, Path) ->
    assert_fixture_database(Db, Manifest, Env, Path);
assert_chain_fixture_database(_, _, _, _) ->
    error({no_go, w403_chain_database_unverified}).

assert_fixture_database(<<"attv5_", _/binary>>, _Manifest, _Env, _Path) ->
    ok;
assert_fixture_database(<<"imboy_chain_ci_", _/binary>> = Db, Manifest, Env, Path) ->
    {match, _} = re:run(Db, "^imboy_chain_ci_[0-9]+_[0-9]+_(uid|full)$"),
    absolute = filename:pathtype(Path),
    {ok, #file_info{type = regular, mode = Mode, uid = Owner}} = file:read_link_info(Path),
    {ok, #file_info{type = directory, mode = ParentMode, uid = Owner}} =
        file:read_link_info(filename:dirname(Path)),
    8#600 = Mode band 8#777,
    8#700 = ParentMode band 8#777,
    #{<<"pg_system_identifier">> := SystemId, <<"owner_marker">> := Marker} = Manifest,
    {match, _} = re:run(SystemId, "^[0-9]+$"),
    Marker = <<"imboy-native-eunit:ordinary-base:", Db/binary>>,
    Pg = proplists:get_value(pg_conf, Env),
    {epgsql, connect, [
        #{
            host := "127.0.0.1",
            port := 55432,
            username := "imboy_ci",
            password := ""
        } = Opts
    ]} = maps:get(start_mfa, Pg),
    Db = iolist_to_binary(maps:get(database, Opts)),
    {ok, Started} = application:ensure_all_started(epgsql),
    try
        {ok, Conn} = epgsql:connect(Opts),
        try
            Expected = {Db, <<"imboy_ci">>, SystemId, Marker},
            {ok, _, [Expected]} = epgsql:equery(
                Conn,
                "SELECT current_database(),current_user,system_identifier::text,"
                "shobj_description(d.oid,'pg_database') FROM pg_control_system(),pg_database d"
                " WHERE d.datname=current_database()",
                []
            ),
            ok
        after
            epgsql:close(Conn)
        end
    after
        lists:foreach(fun application:stop/1, lists:reverse(Started))
    end;
assert_fixture_database(_, _, _, _) ->
    error({no_go, w403_fixture_database_unverified}).

assert_database(Expected) ->
    case elib_pg:query(<<"SELECT current_database() AS db">>, []) of
        {ok, [#{<<"db">> := Expected}]} -> ok;
        _ -> erlang:error({no_go, w403_pool_database_mismatch})
    end.

cleanup(#{w403_before := Before} = S) ->
    [
        best_effort(fun() -> imboy_cache:delete(K) end)
     || K <- maps:get(owned_cache_keys, S, [])
    ],
    case maps:get(owned_cache, S, undefined) of
        Cache when is_pid(Cache) ->
            case whereis(imboy_cache) of
                Cache -> best_effort(fun() -> gen_server:stop(Cache, normal, 5000) end);
                _ -> ok
            end;
        _ ->
            ok
    end,
    case maps:get(owned_pool, S, undefined) of
        Pool when is_pid(Pool) ->
            case whereis(pgsql) of
                Pool -> best_effort(fun() -> pooler:rm_pool(pgsql) end);
                _ -> ok
            end;
        _ ->
            ok
    end,
    case maps:get(owned_tsid, S, false) of
        true -> best_effort(fun elib_tsid:reset_for_test/0);
        false -> ok
    end,
    [
        best_effort(fun() -> application:stop(A) end)
     || A <- maps:get(owned_apps, S, [])
    ],
    OldEnv = maps:from_list(maps:get(env, Before)),
    [
        application:unset_env(imboy, K)
     || {K, _} <- application:get_all_env(imboy),
        not maps:is_key(K, OldEnv)
    ],
    [application:set_env(imboy, K, V) || {K, V} <- maps:to_list(OldEnv)],
    ok;
cleanup(_) ->
    ok.
best_effort(Fun) ->
    try
        Fun()
    catch
        _:_ -> ok
    end.
required(Name) ->
    case os:getenv(Name) of
        false -> erlang:error({no_go, required_resource_missing, Name});
        "" -> erlang:error({no_go, required_resource_missing, Name});
        V -> V
    end.
