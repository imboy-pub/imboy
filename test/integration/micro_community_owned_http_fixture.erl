-module(micro_community_owned_http_fixture).
-export([
    run/1,
    run_cursor/1,
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

%% No historical target fallback: every run explicitly binds its own resource.
-ifdef(TEST).
-export([
    expected_target/0,
    validate_target/2,
    private_config/0,
    validate_routes/3,
    validate_cursor_routes/1
]).
-endif.

required_env(Name) ->
    Value = os:getenv(Name),
    assert(is_list(Value) andalso Value =/= [], required_run_environment),
    Value.

expected_target() ->
    Run = required_env("MC_RUN_ID"),
    assert(
        re:run(Run, "^[A-Za-z0-9][A-Za-z0-9_-]{0,99}\\z", [{capture, none}]) =:= match,
        invalid_run_id
    ),
    Dir = required_env("MC_RUN_PRIVATE_DIR"),
    assert(filename:pathtype(Dir) =:= absolute, private_directory_required),
    PortText = required_env("MC_RUN_PG_PORT"),
    assert(re:run(PortText, "^[0-9]+\\z", [{capture, none}]) =:= match, invalid_run_port),
    Port = list_to_integer(PortText),
    assert(
        Port > 0 andalso Port =< 65535 andalso Port =/= 4323 andalso Port =/= 5432,
        unsafe_run_port
    ),
    Db = required_env("MC_RUN_PG_DATABASE"),
    assert(
        re:run(Db, "^[a-z][a-z0-9_]{0,62}\\z", [{capture, none}]) =:= match,
        invalid_run_database
    ),
    #{
        port => Port,
        database => Db,
        config => filename:join(Dir, "pg-owned.config"),
        owner => iolist_to_binary([
            "Owned by MC run ",
            Run,
            "; synthetic micro-community test data only"
        ])
    }.

validate_target(Opts, #{port := Port, database := Db}) when is_map(Opts) ->
    Host = maps:get(host, Opts, undefined),
    assert(
        (Host =:= "127.0.0.1" orelse Host =:= "localhost") andalso
            maps:get(port, Opts, undefined) =:= Port andalso
            maps:get(database, Opts, undefined) =:= Db,
        private_target_rejected
    ),
    ok.
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

%% Separate explicit fresh-owned session; original run/1 routes are unchanged.
run_cursor(Callback) -> run_mode(Callback, cursor).

run_transport(Callback) -> run_mode(Callback, websocket).

run_mode(Callback, Mode) when is_function(Callback, 1) ->
    _ =
        case os:getenv("MC_KEEP_LOGGER") of
            "1" -> ok;
            _ -> logger:set_primary_config(level, none)
        end,
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
        throw:{mc_http_failed, Code} ->
            {error, Code};
        Class:Reason:Stack ->
            %% r7d 实证顶层吞异常只剩 unexpected_failure：先暴露形态再定性。
            io:format("MC22_RUN_MODE_EXC ~p:~p~n~p~n", [
                Class, Reason, lists:sublist(Stack, 3)
            ]),
            {error, unexpected_failure}
    end.

fresh_vm() ->
    Running = application:which_applications(),
    assert(not lists:keymember(imboy, 1, Running), imboy_started),
    assert(not lists:keymember(pooler, 1, Running), pooler_started),
    assert(whereis(imboy_cache) =:= undefined, cache_already_started),
    assert(whereis(login_attempt_ds) =:= undefined, limiter_already_started),
    assert(whereis(user_server) =:= undefined, user_worker_started),
    assert(whereis(msg_store_sup) =:= undefined, message_worker_started).

private_config() ->
    Expected = expected_target(),
    Path = os:getenv("IMBOY_MICRO_PG_CONFIG"),
    assert(Path =:= maps:get(config, Expected), private_config_path_required),
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode}} ->
            assert(Mode band 8#777 =:= 8#600, private_config_permissions);
        _ ->
            fail(private_config_unavailable)
    end,
    case file:consult(Path) of
        {ok, [Opts]} when is_map(Opts) ->
            ok = validate_target(Opts, Expected),
            maps:with([host, port, database, username, password, ssl, timeout, codecs], Opts);
        _ ->
            fail(private_target_rejected)
    end.

connect_owned(Opts) ->
    try
        Expected = expected_target(),
        ok = validate_target(Opts, Expected),
        connect_verified(Opts, Expected)
    catch
        throw:{mc_http_failed, _} -> {error, owned_target_rejected}
    end.

connect_verified(Opts, Expected) ->
    try epgsql:connect(Opts) of
        {ok, Conn} ->
            try
                %% Both target and ownership come from the explicit current run.
                Db = ec_cnv:to_binary(maps:get(database, Opts)),
                assert(
                    rows(
                        Conn,
                        <<
                            "SELECT current_database(),shobj_description(oid,'pg_database') "
                            "FROM pg_database WHERE datname=current_database()"
                        >>,
                        []
                    ) =:=
                        [{Db, maps:get(owner, Expected)}],
                    owned_database_mismatch
                ),
                sql(Conn, <<"SET statement_timeout='15000'; SET lock_timeout='10000'">>),
                {ok, Conn}
            catch
                Class:Reason ->
                    %% 内层守卫异常不吞：打印真实形态再归类（r30-era 取证）。
                    io:format("MC22_OWNED_TARGET_REJECT ~p:~p~n", [Class, Reason]),
                    epgsql:close(Conn),
                    {error, owned_target_rejected}
            end;
        Error ->
            %% r7/r7b 稳定复现 owned_connection_unavailable 且 epgsql 手测
            %% 同参 42ms 连通：先暴露真实失败形态再定性。
            io:format("MC22_OWNED_CONNECT_ERR ~p~n", [Error]),
            {error, owned_connection_unavailable}
    catch
        Class:Reason:Stack ->
            io:format("MC22_OWNED_CONNECT_EXC ~p:~p~n~p~n", [Class, Reason, Stack]),
            {error, owned_connection_unavailable}
    end.

run_owned(Conn, Opts, Callback, Mode) ->
    configure_runtime(),
    start_dependencies(),
    {ok, Limiter} = gen_server:start({local, login_attempt_ds}, login_attempt_ds, [], []),
    try
        ok = login_attempt_ds:activate_empty(Limiter),
        true = login_attempt_ds:ready(),
        run_with_limiter(Conn, Opts, Callback, Mode)
    after
        gen_server:stop(Limiter)
    end.

run_with_limiter(Conn, Opts, Callback, Mode) ->
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
        %% Current production owner: no legacy throttle IP bucket or fail-open.
        {limiter_ip_rates, #{
            passport_per_ip => {1000, 60000},
            api_per_ip => {1000, 60000},
            oidc_request => {1000, 3600000}
        }},
        {product_profile, community},
        {jwt_key, crypto:strong_rand_bytes(32)},
        {adm_cookie_secret, crypto:strong_rand_bytes(32)},
        % 必须可打印 ASCII：epgsql 以 text 参数通道传 pgcrypto，任意二进制
        % （strong_rand_bytes）会触发 22023 且 config 加解密两侧不对称 →
        % sign_key 读回 <<>> → Android 全量 902（r14-r16 实证）。
        % base64(24B)=32 字符，与 sys.local.config 的形态一致。
        {postgre_aes_key, base64:encode(crypto:strong_rand_bytes(24))}
    ],
    [application:set_env(imboy, Key, Value) || {Key, Value} <- Settings],
    application:set_env(pooler, pools, []),
    application:set_env(throttle, rates, []),
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
        adm_micro_community_handler,
        channel_discovery_handler,
        app_feature_handler
    ],
    Available = [
        R
     || {_, HostRoutes} <- imboy_router:get_routes(),
        R = {_, Handler, _} <- HostRoutes,
        lists:member(Handler, Wanted)
    ],
    Compiled = lists:member(channel_discover, imboy_feature:compiled_features()),
    Enabled = imboy_feature:enabled(channel_discover),
    Routes = validate_routes(Available, Compiled, Enabled),
    Dispatch = cowboy_router:compile([{'_', Routes ++ admin_read_route() ++ transport_routes(Mode)}]),
    start_listener(Dispatch, Conn, Callback).

validate_routes(Routes, Compiled, Enabled) ->
    Core = [
        {"/api/v1/channel/:channel_id/discussion", micro_community_handler, discussion},
        {"/api/v1/channel/:channel_id/discussion/candidates", micro_community_handler, candidates},
        {"/api/v1/channel/:channel_id/discussion/join", micro_community_join_handler, undefined},
        {"/api/v1/channel/:channel_id/posts", micro_community_content_handler, search},
        {"/api/v1/channel/:channel_id/posts/:message_id/essence", micro_community_content_handler,
            essence},
        {"/api/v1/channel/:channel_id/posts/:message_id/tags", micro_community_content_handler,
            tags},
        {"/api/adm/channel/:channel_id/discussion", adm_micro_community_handler, discussion},
        {"/api/adm/channel/:channel_id/discussion/candidates", adm_micro_community_handler,
            candidates},
        {"/api/adm/channel/:channel_id/message/:message_id/essence", adm_micro_community_handler,
            essence},
        {"/api/v1/app/features", app_feature_handler, features},
        {"/api/v1/app/policy", app_feature_handler, policy},
        {"/api/v1/app/ice_servers", app_feature_handler, ice_servers}
    ],
    Discovery = [
        {"/api/v1/channels/" ++ atom_to_list(Action), channel_discovery_handler, Action}
     || Action <- [search, discover, featured, trending, categories]
    ],
    Expected =
        Core ++
            case Compiled of
                true -> Discovery;
                false -> []
            end,
    Actual = [
        {Path, Handler, maps:get(action, State, undefined)}
     || {Path, Handler, State} <- Routes
    ],
    assert(lists:sort(Actual) =:= lists:sort(Expected), route_whitelist_mismatch),
    case Compiled andalso Enabled of
        true ->
            Routes;
        false ->
            io:format(
                "MC_DISCOVERY_SCOPE_NOT_ACCEPTED compiled=~p runtime_enabled=~p required_gate=BLOCKED_PROFILE~n",
                [Compiled, Enabled]
            ),
            [R || R = {_, Handler, _} <- Routes, Handler =/= channel_discovery_handler]
    end.

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

transport_routes(cursor) ->
    Extra = [
        R
     || {_, Rs} <- imboy_router:get_routes(),
        R = {Path, _, _} <- Rs,
        Path =:= "/api/v1/channel/:channel_id/messages"
    ],
    validate_cursor_routes(Extra);
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

%% Exact production route only; no generic channel_handler registration.
validate_cursor_routes(Routes) ->
    Expected = [
        {"/api/v1/channel/:channel_id/messages", channel_handler, #{
            action => messages, required_feature => channel
        }}
    ],
    assert(Routes =:= Expected, cursor_route_whitelist_mismatch),
    Routes.

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
        throw:{mc_http_failed, Code} ->
            {error, Code};
        Class:Reason:Stack ->
            %% r7d 同款：暴露用例层异常形态再定性，避免只剩 unexpected_failure。
            io:format("MC_HTTP_CASE_EXC ~p:~p~n~p~n", [
                Class, Reason, lists:sublist(Stack, 5)
            ]),
            {error, unexpected_failure}
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
