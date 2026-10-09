-module(micro_community_owned_http_fixture).
-export([
    run/1,
    run_transport/1,
    capture/1,
    report_outcome/2,
    committed/2,
    connect_owned/1,
    assert/2,
    rows/3,
    exec/3,
    request/5,
    set_profile/1,
    business_snapshot/1,
    base_payload/1,
    body_content/0
]).
-include_lib("kernel/include/file.hrl").

-define(CONFIG,
    "/Users/leeyi/.codex/micro-community-runs/mc-20261008T083109Z/private/pg-owned.config"
).
-define(OWNER_COMMENT,
    <<"Owned by MC run mc-20261008T083109Z; synthetic micro-community test data only">>
).
-define(LISTENER, mc_owned_http_listener).
-define(BASE, 9007199254744000).
-define(U, (?BASE + 1)).
-define(MANAGER, (?BASE + 2)).
-define(SUB, (?BASE + 3)).
-define(OUTSIDER, (?BASE + 4)).
-define(ADMIN, (?BASE + 5)).
-define(DENIED_ADMIN, (?BASE + 6)).
-define(G1, (?BASE + 10)).
-define(G2, (?BASE + 11)).
-define(C1, (?BASE + 20)).
-define(C2, (?BASE + 21)).
-define(C3, (?BASE + 22)).
-define(C4, (?BASE + 23)).
-define(BODY, (?BASE + 30)).
-define(TAG_ONLY, (?BASE + 31)).
-define(REVOKED, (?BASE + 32)).
-define(DELETED, (?BASE + 33)).
-define(FOREIGN, (?BASE + 34)).
-define(ADMIN_MSG, (?BASE + 35)).
-define(PKG, <<"mc.run20261008">>).
-define(SIGN_KEY, <<"mc.run20261008_android_1">>).
-define(NEEDLE, <<"mconlybodytoken">>).
-define(CN_NEEDLE, <<"微社群独有正文词"/utf8>>).

%% Explicit opt-in only: no EUnit discovery hook, imboy, WS or migration startup.
%% Every failure crossing run/0 is a fixed code; never return request/config terms.
run(Callback) -> run_mode(Callback, http).

run_transport(Callback) -> run_mode(Callback, websocket).

run_mode(Callback, Mode) when is_function(Callback, 1) ->
    _ = logger:set_primary_config(level, none),
    try
        fresh_vm(),
        Opts = private_config(),
        case connect_owned(Opts) of
            {ok, Conn} ->
                try
                    run_owned(Conn, Opts, Callback, Mode)
                after
                    epgsql:close(Conn)
                end;
            _ ->
                fail(owned_connection_unavailable)
        end
    catch
        throw:{mc_http_failed, Code} -> {error, Code};
        _:_ -> {error, unexpected_failure}
    end.

fresh_vm() ->
    Running = application:which_applications(),
    assert(not lists:keymember(imboy, 1, Running), imboy_started),
    assert(not lists:keymember(pooler, 1, Running), pooler_started),
    assert(whereis(imboy_cache) =:= undefined, cache_already_started),
    assert(whereis(user_server) =:= undefined, user_worker_started),
    assert(whereis(msg_store_sup) =:= undefined, message_worker_started).

private_config() ->
    Path = os:getenv("IMBOY_MICRO_PG_CONFIG"),
    assert(Path =:= ?CONFIG, private_config_path_required),
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode}} ->
            assert(Mode band 8#777 =:= 8#600, private_config_permissions);
        _ ->
            fail(private_config_unavailable)
    end,
    case file:consult(Path) of
        {ok, [#{database := "imboy_test_group", host := Host, port := 4323} = Opts]} when
            Host =:= "localhost"; Host =:= "127.0.0.1"
        ->
            maps:with([host, port, database, username, password, ssl, timeout, codecs], Opts);
        _ ->
            fail(private_target_rejected)
    end.

connect_owned(Opts) ->
    try epgsql:connect(Opts) of
        {ok, Conn} ->
            try
                assert(
                    rows(
                        Conn,
                        <<
                            "SELECT current_database(),shobj_description(oid,'pg_database') "
                            "FROM pg_database WHERE datname=current_database()"
                        >>,
                        []
                    ) =:=
                        [{<<"imboy_test_group">>, ?OWNER_COMMENT}],
                    owned_database_mismatch
                ),
                sql(Conn, <<"SET statement_timeout='15000'; SET lock_timeout='10000'">>),
                {ok, Conn}
            catch
                _:_ ->
                    epgsql:close(Conn),
                    {error, owned_target_rejected}
            end;
        _ ->
            {error, owned_connection_unavailable}
    catch
        _:_ -> {error, owned_connection_unavailable}
    end.

run_owned(Conn, Opts, Callback, Mode) ->
    configure_runtime(),
    start_dependencies(),
    {ok, Cache} = imboy_cache:start_link([{depcache_memory_max, 8}]),
    ok = elib_tsid:init(#{
        dc_id => 7,
        node_id => 126,
        dc_bits => 3,
        names => [group_log, group_member]
    }),
    Pool = #{
        name => pgsql,
        init_count => 2,
        max_count => 3,
        start_mfa => {?MODULE, connect_owned, [Opts]}
    },
    case pooler:new_pool(Pool) of
        {ok, _} ->
            try
                fixture_session(Conn, Callback, Mode)
            after
                _ = pooler:rm_pool(pgsql),
                _ = gen_server:stop(Cache)
            end;
        _ ->
            fail(pool_creation_failed)
    end.

configure_runtime() ->
    Settings = [
        {sql_driver, pgsql},
        {env, test},
        {host, '_'},
        {auto_migrate, false},
        {dsync_enabled, false},
        {payment_gateway_enabled, false},
        {push, [{enabled, false}]},
        {adm_auth_legacy_cookie_enabled, false},
        {adm_ip_allowlist, ["127.0.0.1"]},
        {start_mode, http},
        {api_auth_switch, <<"on">>},
        {product_profile, community},
        {jwt_key, crypto:strong_rand_bytes(32)},
        {adm_cookie_secret, crypto:strong_rand_bytes(32)},
        {postgre_aes_key, crypto:strong_rand_bytes(32)}
    ],
    [application:set_env(imboy, Key, Value) || {Key, Value} <- Settings],
    application:set_env(pooler, pools, []),
    application:set_env(throttle, rates, [{api_per_ip, 1000, per_minute}]),
    ok.

start_dependencies() ->
    Apps = [crypto, asn1, public_key, ssl, jose, jsone, epgsql, pooler, cowboy, throttle, inets],
    [
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            _ -> fail(dependency_start_failed)
        end
     || App <- Apps
    ],
    ok = imboy_app:init_throttle_rates().

fixture_session(Conn, Callback, Mode) ->
    fixtures_absent(Conn),
    ConfigBefore = config_snapshot(Conn),
    committed(Conn, fun setup_fixture/1),
    Outcome = capture(fun() ->
        set_profile(community),
        ok = app_version_ds:set_sign_key(
            <<"android">>, <<"1">>, ?PKG, base64:encode(crypto:strong_rand_bytes(32))
        ),
        assert(
            is_binary(app_version_ds:sign_key(<<"android">>, <<"1">>, ?PKG)),
            signature_config_unavailable
        ),
        with_listener(Conn, Callback, Mode)
    end),
    Cleanup = capture(fun() -> cleanup_all(Conn, ConfigBefore) end),
    report_outcome(Outcome, Cleanup).

config_keys() -> [?SIGN_KEY, <<"product_profile">>, <<"capabilities">>, <<"features">>].

config_snapshot(Conn) ->
    [
        {Key,
            rows(Conn, <<"SELECT row_to_json(c)::text FROM public.config c WHERE key=$1">>, [Key])}
     || Key <- config_keys()
    ].

set_profile(Profile) ->
    ok = config_ds:set(<<"product_profile">>, atom_to_binary(Profile)),
    ok = config_ds:set(<<"capabilities">>, #{}),
    ok = config_ds:set(<<"features">>, #{<<"channel">> => true}),
    [imboy_cache:flush({config5, Key}) || Key <- config_keys()],
    assert(imboy_policy:current_profile() =:= Profile, persisted_profile_mismatch),
    case Profile of
        community -> assert(imboy_feature:enabled(channel), channel_disabled);
        phase1_delivery -> ok
    end.

with_listener(Conn, Callback, Mode) ->
    Wanted = [
        micro_community_handler,
        micro_community_join_handler,
        micro_community_content_handler,
        adm_micro_community_handler
    ],
    Routes = [
        R
     || {_, HostRoutes} <- imboy_router:get_routes(),
        R = {_, Handler, _} <- HostRoutes,
        lists:member(Handler, Wanted)
    ],
    assert(length(Routes) =:= 9, route_whitelist_mismatch),
    Dispatch = cowboy_router:compile([{'_', Routes ++ admin_read_route() ++ transport_routes(Mode)}]),
    start_listener(Dispatch, Conn, Callback).

admin_read_route() ->
    Extra = [
        R
     || {_, Rs} <- imboy_router:get_routes(),
        R = {Path, Handler, State} <- Rs,
        Path =:= "/api/adm/channel/:channel_id/messages",
        Handler =:= adm_channel_handler,
        maps:get(action, State, undefined) =:= messages
    ],
    assert(length(Extra) =:= 1, admin_read_route_whitelist_mismatch),
    Extra.

transport_routes(http) ->
    [];
transport_routes(websocket) ->
    Extra = [
        R
     || {_, Rs} <- imboy_router:get_routes(),
        R = {Path, _, _} <- Rs,
        lists:member(Path, ["/api/v1/ws", "/api/v1/msg/offline"])
    ],
    assert(length(Extra) =:= 2, transport_route_whitelist_mismatch),
    Extra.

start_listener(Dispatch, Conn, Callback) ->
    Middlewares = [
        cowboy_router,
        cors_middleware,
        security_headers_middleware,
        auth_middleware,
        feature_gate_middleware,
        throttle_middleware,
        cowboy_handler
    ],
    {ok, _} = cowboy:start_clear(
        ?LISTENER,
        #{socket_opts => [{ip, {127, 0, 0, 1}}, {port, 0}], num_acceptors => 2},
        #{env => #{dispatch => Dispatch}, middlewares => Middlewares}
    ),
    try
        Port = ranch:get_port(?LISTENER),
        assert(is_integer(Port) andalso Port > 0, listener_port_invalid),
        Counter = make_ref(),
        put(Counter, 0),
        Context = #{conn => Conn, port => Port, credentials => credentials(), counter => Counter},
        try
            Callback(Context)
        after
            io:format("MC_OWNED_HTTP_REQUESTS count=~p~n", [erase(Counter)])
        end
    after
        cowboy:stop_listener(?LISTENER)
    end.

setup_fixture(Conn) ->
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.\"user\" "
                "(id,account,password,reg_ip,reg_cosv,mobile,email) VALUES($1,$2,'synthetic-disabled',"
                "'127.0.0.1','MC HTTP synthetic',NULL,NULL)"
            >>,
            [Uid, account(Uid)]
        )
     || Uid <- [?U, ?MANAGER, ?SUB, ?OUTSIDER]
    ],
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.user_device(id,user_id,device_id,device_type,status) "
                "VALUES($1,$2,$3,'android',1)"
            >>,
            [Uid + 100, Uid, did(Uid)]
        )
     || Uid <- [?U, ?MANAGER, ?SUB, ?OUTSIDER]
    ],
    setup_admin_groups(Conn),
    setup_channels(Conn).

setup_admin_groups(Conn) ->
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.adm_user(id,account,password,role_id,mobile,email) "
                "VALUES($1,$2,'synthetic-disabled',$3,NULL,NULL)"
            >>,
            [Id, account(Id), Roles]
        )
     || {Id, Roles} <- [{?ADMIN, [1]}, {?DENIED_ADMIN, []}]
    ],
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.\"group\" "
                "(id,owner_uid,creator_uid,title,type,join_limit,scope,e2ee_mode) "
                "VALUES($1,$2,$2,'MC HTTP synthetic',2,3,'personal',0)"
            >>,
            [Gid, ?U]
        )
     || Gid <- [?G1, ?G2]
    ],
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.group_member(id,group_id,user_id,role,status,is_join) "
                "VALUES($1,$2,$3,5,1,true)"
            >>,
            [Gid + 200, Gid, ?U]
        )
     || Gid <- [?G1, ?G2]
    ],
    ok.

setup_channels(Conn) ->
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.channel(id,name,creator_uid,scope,visibility,access_type) "
                "VALUES($1,'MC HTTP synthetic',$2,'personal',1,0)"
            >>,
            [Cid, ?U]
        )
     || Cid <- [?C1, ?C2, ?C3, ?C4]
    ],
    exec(
        Conn,
        <<"INSERT INTO public.channel_admin(id,channel_id,user_id,role) VALUES($1,$2,$3,2)">>,
        [?BASE + 50, ?C1, ?MANAGER]
    ),
    exec(
        Conn,
        <<
            "INSERT INTO public.channel_subscription(id,channel_id,user_id,status) "
            "VALUES($1,$2,$3,1)"
        >>,
        [?BASE + 51, ?C1, ?SUB]
    ),
    setup_messages(Conn).

setup_messages(Conn) ->
    Entries = [
        {?BODY, ?C1, body_content(), 1, false},
        {?TAG_ONLY, ?C1, <<"unrelated content">>, 1, false},
        {?REVOKED, ?C1, body_content(), 1, true},
        {?DELETED, ?C1, body_content(), -1, false},
        {?FOREIGN, ?C4, body_content(), 1, false},
        {?ADMIN_MSG, ?C3, <<"admin synthetic">>, 1, false}
    ],
    [
        exec(
            Conn,
            <<
                "INSERT INTO public.channel_message "
                "(id,channel_id,author_id,content,payload,is_pinned,status,revoked) "
                "VALUES($1,$2,$3,$4,$5::jsonb,true,$6,$7)"
            >>,
            [Mid, Cid, ?U, Content, jsone:encode(base_payload(Mid)), Status, Revoked]
        )
     || {Mid, Cid, Content, Status, Revoked} <- Entries
    ],
    ok.

body_content() -> <<?NEEDLE/binary, " ", ?CN_NEEDLE/binary>>.

base_payload(?TAG_ONLY) ->
    #{<<"tags">> => [?NEEDLE, ?CN_NEEDLE], <<"other">> => <<"keep">>};
base_payload(_) ->
    #{
        <<"other">> => <<"keep">>,
        <<"attachment">> => #{<<"id">> => <<"synthetic-no-url">>}
    }.

business_snapshot(Conn) ->
    Specs = [
        {<<"public.channel">>, <<"id BETWEEN $1 AND $2">>},
        {<<"public.\"group\"">>, <<"id BETWEEN $1 AND $2">>},
        {<<"public.group_member">>, <<"group_id BETWEEN $1 AND $2">>},
        {<<"public.group_member_generation">>, <<"group_id BETWEEN $1 AND $2">>},
        {<<"public.channel_message">>, <<"channel_id BETWEEN $1 AND $2">>},
        {<<"public.channel_subscription">>, <<"channel_id BETWEEN $1 AND $2">>},
        {<<"public.channel_admin">>, <<"channel_id BETWEEN $1 AND $2">>},
        {<<"public.group_log">>, <<"group_id BETWEEN $1 AND $2">>},
        {<<"public.user_log">>, <<"uid BETWEEN $1 AND $2">>}
    ],
    [
        rows(
            Conn,
            <<
                "SELECT coalesce(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text),'[]'::jsonb)::text "
                "FROM ",
                Table/binary,
                " t WHERE ",
                Where/binary
            >>,
            [?BASE, ?BASE + 999]
        )
     || {Table, Where} <- Specs
    ] ++
        [
            rows(
                Conn,
                <<
                    "SELECT conv_key,seq FROM public.msg_store_seq WHERE conv_key IN($1,$2) "
                    "ORDER BY conv_key"
                >>,
                [conv_key(?G1), conv_key(?G2)]
            )
        ].

request(
    #{port := Port, credentials := Credentials, counter := Counter}, Identity, Method, Path, Body
) ->
    put(Counter, get(Counter) + 1),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ binary_to_list(Path),
    Headers = maps:get(Identity, Credentials),
    Req =
        case Method of
            get -> {Url, Headers};
            delete -> {Url, Headers};
            _ -> {Url, Headers, "application/json", jsone:encode(Body)}
        end,
    case
        httpc:request(
            Method,
            Req,
            [{timeout, 15000}, {connect_timeout, 5000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, Status, _}, _, Json}} ->
            try
                {Status, jsone:decode(Json, [{object_format, map}])}
            catch
                _:_ -> fail(response_invalid_json)
            end;
        _ ->
            fail(loopback_request_failed)
    end.

%% Issue once before any HTTP case; warm reads and revocation use the same token.
credentials() ->
    Identities = [?U, ?MANAGER, ?SUB, ?OUTSIDER, {admin, ?ADMIN}, {admin, ?DENIED_ADMIN}],
    Headers = maps:from_list([{Id, initial_headers(Id)} || Id <- Identities]),
    OwnerHeaders = maps:get(?U, Headers),
    Headers#{
        none => proplists:delete("authorization", OwnerHeaders),
        bad_signature => lists:keystore(
            "sign",
            1,
            OwnerHeaders,
            {"sign", "invalid-synthetic-signature"}
        )
    }.

initial_headers({admin, Id}) ->
    Uid = bin(Id),
    case adm_session_ds:issue(Uid) of
        {ok, Sig} ->
            [
                {"cookie",
                    binary_to_list(<<"adm_user_id=", Uid/binary, "; adm_user_sig=", Sig/binary>>)}
            ];
        _ ->
            fail(admin_cookie_unavailable)
    end;
initial_headers(Uid) when is_integer(Uid) -> human_headers(Uid, valid).

human_headers(Uid, SignatureMode) ->
    Token = token_ds:encrypt_token(Uid, did(Uid)),
    [
        {"authorization", binary_to_list(<<"Bearer ", Token/binary>>)}
        | signature_headers(Uid, SignatureMode)
    ].

signature_headers(Uid, Mode) ->
    Did = did(Uid),
    Key = app_version_ds:sign_key(<<"android">>, <<"1">>, ?PKG),
    Sign =
        case Mode of
            valid -> elib_hasher:hmac_sha256(<<Did/binary, "|1|android|", ?PKG/binary>>, Key);
            invalid -> <<"invalid-synthetic-signature">>
        end,
    [
        {"did", binary_to_list(Did)},
        {"vsn", "1"},
        {"sk", "1"},
        {"cos", "android"},
        {"pkg", binary_to_list(?PKG)},
        {"method", "sha256"},
        {"sign", binary_to_list(Sign)}
    ].

bin(N) -> integer_to_binary(N).
did(Uid) -> <<"mc-http-", (bin(Uid))/binary>>.
account(Uid) -> <<"mc_http_", (bin(Uid))/binary>>.
conv_key(Gid) -> <<"c2g:", (bin(Gid))/binary>>.

fixtures_absent(Conn) ->
    Tables = [
        <<"public.\"user\"">>,
        <<"public.user_device">>,
        <<"public.adm_user">>,
        <<"public.\"group\"">>,
        <<"public.channel">>,
        <<"public.channel_message">>,
        <<"public.group_member">>,
        <<"public.channel_admin">>,
        <<"public.channel_subscription">>
    ],
    [
        assert(
            rows(
                Conn,
                <<"SELECT count(*) FROM ", T/binary, " WHERE id BETWEEN $1 AND $2">>,
                [?BASE, ?BASE + 999]
            ) =:= [{0}],
            fixture_id_collision_or_residue
        )
     || T <- Tables
    ],
    fixtures_related_absent(Conn).

fixtures_related_absent(Conn) ->
    assert(
        business_snapshot(Conn) =:= empty_business_snapshot(),
        fixture_foreign_key_collision_or_residue
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.user_device WHERE user_id BETWEEN $1 AND $2">>,
            [?BASE, ?BASE + 999]
        ) =:= [{0}],
        device_fixture_collision_or_residue
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.user_auth_epoch WHERE user_id BETWEEN $1 AND $2">>,
            [?BASE, ?BASE + 999]
        ) =:= [{0}],
        user_epoch_fixture_collision_or_residue
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.adm_auth_epoch WHERE admin_id BETWEEN $1 AND $2">>,
            [?BASE, ?BASE + 999]
        ) =:= [{0}],
        admin_epoch_fixture_collision_or_residue
    ).

empty_business_snapshot() -> lists:duplicate(9, [{<<"[]">>}]) ++ [[]].

cleanup_all(Conn, Snapshot) ->
    committed(Conn, fun(Tx) ->
        cleanup_fixture(Tx),
        restore_config(Tx, Snapshot)
    end),
    ok = imboy_cache:flush(),
    fixtures_absent(Conn),
    assert(config_snapshot(Conn) =:= Snapshot, configuration_restore_mismatch),
    io:format("MC_HTTP_CLEANUP_RESTORE_COMPLETED~n"),
    ok.

cleanup_fixture(Conn) ->
    Channels = [?C1, ?C2, ?C3, ?C4],
    Groups = [?G1, ?G2],
    Users = [?U, ?MANAGER, ?SUB, ?OUTSIDER],
    Admins = [?ADMIN, ?DENIED_ADMIN],
    exec(
        Conn, <<"UPDATE public.channel SET discussion_group_id=NULL WHERE id=ANY($1::bigint[])">>, [
            Channels
        ]
    ),
    Deletes = [
        {<<"public.group_log">>, <<"group_id">>, Groups},
        {<<"public.user_log">>, <<"uid">>, Admins},
        {<<"public.group_member_generation">>, <<"group_id">>, Groups},
        {<<"public.group_member">>, <<"group_id">>, Groups},
        {<<"public.channel_admin">>, <<"channel_id">>, Channels},
        {<<"public.channel_subscription">>, <<"channel_id">>, Channels},
        {<<"public.channel_message">>, <<"channel_id">>, Channels},
        {<<"public.channel">>, <<"id">>, Channels},
        {<<"public.\"group\"">>, <<"id">>, Groups},
        {<<"public.user_device">>, <<"user_id">>, Users},
        {<<"public.user_auth_epoch">>, <<"user_id">>, Users},
        {<<"public.adm_auth_epoch">>, <<"admin_id">>, Admins},
        {<<"public.adm_user">>, <<"id">>, Admins},
        {<<"public.\"user\"">>, <<"id">>, Users}
    ],
    [
        exec(
            Conn,
            <<"DELETE FROM ", T/binary, " WHERE ", Field/binary, "=ANY($1::bigint[])">>,
            [Ids]
        )
     || {T, Field, Ids} <- Deletes
    ],
    exec(Conn, <<"DELETE FROM public.msg_store_seq WHERE conv_key IN($1,$2)">>, [
        conv_key(?G1), conv_key(?G2)
    ]),
    ok.

restore_config(Conn, Snapshot) ->
    [
        begin
            exec(Conn, <<"DELETE FROM public.config WHERE key=$1">>, [Key]),
            [
                exec(
                    Conn,
                    <<
                        "INSERT INTO public.config SELECT "
                        "(json_populate_record(NULL::public.config,$1::json)).*"
                    >>,
                    [Raw]
                )
             || {Raw} <- Saved
            ]
        end
     || {Key, Saved} <- Snapshot
    ],
    ok.

capture(Fun) ->
    try Fun() of
        ok -> ok;
        _ -> {error, unexpected_result}
    catch
        throw:{mc_http_failed, Code} -> {error, Code};
        _:_ -> {error, unexpected_failure}
    end.

report_outcome(Outcome, Cleanup) ->
    case Cleanup of
        ok -> ok;
        _ -> io:format("MC_HTTP_CLEANUP_RESTORE_FAILED~n")
    end,
    case {Outcome, Cleanup} of
        {ok, ok} -> ok;
        {{error, Code}, _} -> fail(Code);
        {ok, {error, _}} -> fail(cleanup_restore_failed)
    end.

committed(Conn, Fun) ->
    assert(
        epgsql:with_transaction(Conn, Fun, [{reraise, true}]) =:= ok, fixture_transaction_failed
    ).
rows(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, Result} ->
            Result;
        Other ->
            sql_failure(Sql, Other),
            fail(oracle_query_failed)
    end.
exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, Count} when is_integer(Count) -> ok;
        Other ->
            sql_failure(Sql, Other),
            fail(owned_write_failed)
    end.
sql(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        Results when is_list(Results) ->
            case
                lists:dropwhile(
                    fun
                        ({ok, _, _}) -> true;
                        ({ok, _}) -> true;
                        (_) -> false
                    end,
                    Results
                )
            of
                [] ->
                    ok;
                [Error | _] ->
                    sql_failure(Sql, Error),
                    fail(connection_settings_failed)
            end;
        Other ->
            sql_failure(Sql, Other),
            fail(connection_settings_failed)
    end.

sql_failure(Sql, Result) ->
    Hash = binary:encode_hex(crypto:hash(sha256, iolist_to_binary(Sql))),
    io:format("MC_HTTP_SQL_FAILURE sql_sha256=~s sqlstate=~s~n", [Hash, sql_state(Result)]).

%% epgsql error record shape; omit severity, message, detail and all parameters.
sql_state({error, {error, _, State, _, _, _}}) when is_binary(State), byte_size(State) =:= 5 ->
    case re:run(State, <<"^[0-9A-Z]{5}$">>, [{capture, none}]) of
        match -> State;
        _ -> <<"unknown">>
    end;
sql_state(_) ->
    <<"unknown">>.
assert(true, _) -> ok;
assert(false, Code) -> fail(Code).
fail(Code) -> throw({mc_http_failed, Code}).
