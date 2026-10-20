-module(enterprise_oa_login_http_tests).
-include_lib("eunit/include/eunit.hrl").

oa_login_http_test_() ->
    {timeout, 600, fun() ->
        State = intbe02_http_support:setup_all(),
        CacheOwner =
            case whereis(imboy_cache_sync) of
                undefined ->
                    {ok, CachePid} = imboy_cache_sync:start_link(),
                    CachePid;
                _ ->
                    undefined
            end,
        try
            machine_signature(State),
            exchange_pending(State),
            register_account(State),
            link_account(State),
            disabled_account(State)
        after
            case CacheOwner of
                undefined -> ok;
                _ -> gen_server:stop(CacheOwner)
            end,
            intbe02_http_support:teardown_all(State),
            inttest_marker_db:release(State)
        end
    end}.

machine_signature(State) ->
    Params = issue_params(<<"http-scope">>, <<"login">>),
    Missing = post(State, issue, Params, #{}),
    ?assertEqual(401, maps:get(status, Missing)),
    ?assert(is_binary(maps:get(<<"code">>, maps:get(<<"error">>, maps:get(json, Missing))))),
    Connection = maps:get(conn, State),
    Application = maps:get(app_sso, State),
    ok = intbe02_http_support:sql_exec(
        Connection,
        <<
            "UPDATE enterprise_application_credential SET organization_id=NULL "
            "WHERE application_id=$1"
        >>,
        [Application]
    ),
    Issued = success(post(State, issue, Params, machine_auth(State))),
    ?assertEqual(60, maps:get(<<"expires_in">>, Issued)),
    ?assertEqual(43, byte_size(maps:get(<<"ticket">>, Issued))).

exchange_pending(State) ->
    Params = ticket(State, <<"http-pending">>, <<"login">>, <<"http-pending-device">>),
    error_reply(
        post(
            State,
            exchange,
            Params#{<<"code_verifier">> => <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>},
            #{}
        ),
        400,
        <<"invalid_request">>
    ),
    error_reply(
        post(
            State,
            exchange,
            Params#{<<"application_key">> => <<"intbe02-oa-a">>},
            #{}
        ),
        400,
        <<"invalid_request">>
    ),
    Pending = success(post(State, exchange, Params, #{})),
    ?assertEqual(true, maps:get(<<"requires_account_choice">>, Pending)),
    ?assertEqual(300, maps:get(<<"expires_in">>, Pending)),
    ?assertEqual(43, byte_size(maps:get(<<"pending_token">>, Pending))),
    ?assertNot(maps:is_key(<<"token">>, Pending)),
    ?assertNot(maps:is_key(<<"refreshtoken">>, Pending)),
    ?assertNotEqual(200, maps:get(status, post(State, exchange, Params, #{}))).

register_account(State) ->
    ok = account_ds:init(),
    Owned = start_allocator(),
    try
        Params = pending(State, <<"http-register">>, <<"login">>, <<"http-register-device">>),
        Before = user_count(State),
        error_reply(
            post(State, register, Params, #{}),
            400,
            <<"explicit_confirmation_required">>
        ),
        error_reply(
            post(State, register, Params#{<<"confirm_create">> => false}, #{}),
            400,
            <<"explicit_confirmation_required">>
        ),
        error_reply(
            post(
                State,
                register,
                Params#{<<"confirm_create">> => true, <<"application_key">> => <<"intbe02-oa-a">>},
                #{}
            ),
            400,
            <<"invalid_request">>
        ),
        ?assertEqual(Before, user_count(State)),
        Session = success(post(State, register, Params#{<<"confirm_create">> => true}, #{})),
        User = maps:get(<<"user_id">>, Session),
        ?assertEqual(Before + 1, user_count(State)),
        assert_session(Session, User, <<"http-register-device">>),
        assert_binding(State, <<"http-register">>, User),
        ?assertNotEqual(
            200,
            maps:get(
                status,
                post(State, register, Params#{<<"confirm_create">> => true}, #{})
            )
        ),
        ?assertEqual(Before + 1, user_count(State))
    after
        case Owned of
            undefined -> ok;
            Pid -> gen_server:stop(Pid)
        end
    end.

link_account(State) ->
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"oa-http-test">>,
        <<"synthetic.oa">>,
        <<"synthetic-oa-device-sign-key">>
    ),
    User = intbe02_http_support:fixture(h_a1, State),
    Other = intbe02_http_support:fixture(h_a2, State),
    Device = <<"http-link-device">>,
    Auth = human_auth(User, Device),
    OtherAuth = human_auth(Other, Device),
    Params = pending(State, <<"http-link">>, <<"link">>, Device),
    Before = user_count(State),
    ?assertEqual(401, maps:get(status, post(State, link, Params, #{}))),
    error_reply(
        post(State, link, Params#{<<"did">> => <<"other-device">>}, Auth),
        400,
        <<"invalid_request">>
    ),
    error_reply(
        post(State, register, Params#{<<"confirm_create">> => true}, #{}),
        400,
        <<"invalid_request">>
    ),
    Session = success(post(State, link, Params, Auth)),
    assert_session(Session, User, Device),
    assert_binding(State, <<"http-link">>, User),
    Conflict = pending(State, <<"http-link">>, <<"link">>, Device),
    error_reply(post(State, link, Conflict, OtherAuth), 409, <<"identity_conflict">>),
    assert_binding(State, <<"http-link">>, User),
    ?assertEqual(Before, user_count(State)),
    success(post(State, link, Conflict, Auth)),
    Login = ticket(State, <<"http-link">>, <<"login">>, Device),
    LoggedIn = success(post(State, exchange, Login, #{})),
    assert_session(LoggedIn, User, Device),
    ?assertNot(maps:is_key(<<"pending_token">>, LoggedIn)),
    ?assertEqual(Before, user_count(State)).

%% ADR 8.3「停用账号拒绝登录」：注册建号后停用（status=0），再走 login
%% exchange 必须拿到明确 403 account_not_active（claim 的 source-person CHECK
%% 23514 → invalid_person），不得落 500 兜底。
disabled_account(State) ->
    ok = account_ds:init(),
    Owned = start_allocator(),
    try
        Params = pending(State, <<"http-disabled">>, <<"login">>, <<"http-disabled-device">>),
        Session = success(post(State, register, Params#{<<"confirm_create">> => true}, #{})),
        User = maps:get(<<"user_id">>, Session),
        ok = intbe02_http_support:sql_exec(
            maps:get(conn, State),
            <<"UPDATE \"user\" SET status=0 WHERE id=$1">>,
            [User]
        ),
        Login = ticket(State, <<"http-disabled">>, <<"login">>, <<"http-disabled-device">>),
        error_reply(post(State, exchange, Login, #{}), 403, <<"account_not_active">>),
        assert_binding(State, <<"http-disabled">>, User)
    after
        case Owned of
            undefined -> ok;
            Pid -> gen_server:stop(Pid)
        end
    end.

human_auth(User, Device) ->
    {ok, Epoch} = elib_pg:with_tx(fun(Connection) ->
        {ok, _} = enterprise_oa_login_repo:device_tx(Connection, User, Device, #{}),
        {ok, #{<<"epoch">> := Current}} = enterprise_oa_login_repo:epoch_tx(Connection, User),
        {ok, Current}
    end),
    Session = token_ds:issue_device_session(User, Device, Epoch),
    intbe02_http_support:auth(maps:get(<<"token">>, Session)).

assert_session(Session, User, Device) ->
    ?assertEqual(User, maps:get(<<"user_id">>, Session)),
    ?assertEqual(false, maps:get(<<"projection_pending">>, Session)),
    ?assert(is_binary(maps:get(<<"refreshtoken">>, Session))),
    {ok, User, _, <<"tk">>, Device, Epoch} =
        token_ds:decrypt_token(maps:get(<<"token">>, Session)),
    ?assert(is_integer(Epoch)).

assert_binding(State, External, User) ->
    #{<<"user_id">> := Bound} = intbe02_http_support:one(
        maps:get(conn, State),
        <<
            "SELECT user_id FROM enterprise_source_person "
            "WHERE application_id=$1 AND external_user_id=$2"
        >>,
        [maps:get(app_sso, State), External]
    ),
    ?assertEqual(User, Bound).

user_count(State) ->
    #{<<"n">> := Count} = intbe02_http_support:one(
        maps:get(conn, State),
        <<"SELECT count(*) AS n FROM \"user\"">>
    ),
    Count.

start_allocator() ->
    case whereis(imboy_account_allocator) of
        undefined ->
            case imboy_account_allocator:start_link() of
                {ok, Pid} -> Pid;
                {error, {already_started, _}} -> undefined
            end;
        _ ->
            undefined
    end.

pending(State, External, Intent, Device) ->
    Params = ticket(State, External, Intent, Device),
    maps:merge(Params, success(post(State, exchange, Params, #{}))).

ticket(State, External, Intent, Device) ->
    Issued = success(post(State, issue, issue_params(External, Intent), machine_auth(State))),
    #{
        <<"ticket">> => maps:get(<<"ticket">>, Issued),
        <<"application_key">> => <<"intbe02-oa-sso">>,
        <<"code_verifier">> => verifier(),
        <<"did">> => Device
    }.

issue_params(External, Intent) ->
    #{
        <<"external_user_id">> => External,
        <<"intent">> => Intent,
        <<"code_challenge_method">> => <<"S256">>,
        <<"code_challenge">> => enterprise_oa_login_logic:pkce(verifier())
    }.

verifier() -> <<"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>.

machine_auth(State) -> intbe02_http_support:auth(maps:get(cred_sso, State)).

post(State, Action, Params, Headers) ->
    Path =
        case Action of
            issue -> <<"/api/internal/v1/oa/login/tickets">>;
            exchange -> <<"/api/v1/oa/login/exchange">>;
            register -> <<"/api/v1/oa/login/register">>;
            link -> <<"/api/v1/oa/login/link">>
        end,
    RequestHeaders =
        case Action of
            link -> maps:merge(link_headers(), Headers);
            _ -> Headers
        end,
    Reply = intbe02_http_support:http(
        maps:get(port, State), <<"POST">>, Path, Params, RequestHeaders
    ),
    Reply#{json => jsone:decode(maps:get(body, Reply))}.

link_headers() ->
    #{
        <<"cos">> => <<"synthetic">>,
        <<"vsn">> => <<"oa-http-test">>,
        <<"pkg">> => <<"synthetic.oa">>,
        <<"did">> => <<"http-link-device">>,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<"http-link-device|oa-http-test|synthetic|synthetic.oa">>,
            <<"synthetic-oa-device-sign-key">>
        )
    }.

success(Reply) ->
    ?assertEqual(200, maps:get(status, Reply)),
    #{<<"code">> := Code, <<"msg">> := Message, <<"payload">> := Payload} = maps:get(json, Reply),
    ?assertEqual(0, Code),
    ?assert(is_binary(Message)),
    Payload.

error_reply(Reply, Status, Message) ->
    ?assertEqual(Status, maps:get(status, Reply)),
    #{<<"code">> := Code, <<"msg">> := Actual, <<"payload">> := Payload} = maps:get(json, Reply),
    ?assertEqual(Status, Code),
    ?assertEqual(Message, Actual),
    ?assertEqual(#{}, Payload).

%% Same original HTTP helpers and real Human device token; no Admin substitution.
oa_login_link_wrong_key_test_() ->
    isolated_pg_fixture:test(fun oa_login_link_wrong_key_fixture/0).

oa_login_link_wrong_key_fixture() ->
    {timeout, 900,
        {setup,
            fun() ->
                isolated_pg_fixture:setup_single_node_cache(
                    fun enterprise_oa_login_three_gap_pg_checks:setup/0,
                    fun enterprise_oa_login_three_gap_pg_checks:teardown/1
                )
            end,
            fun enterprise_oa_login_three_gap_pg_checks:teardown/1, fun(S) ->
                {timeout, 900, fun() -> focused_link_wrong_key(S) end}
            end}}.

focused_link_wrong_key(S) ->
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"oa-http-test">>,
        <<"synthetic.oa">>,
        <<"synthetic-oa-device-sign-key">>
    ),
    User = intbe02_http_support:fixture(h_a1, S),
    Device = <<"http-link-device">>,
    Auth = human_auth(User, Device),
    Params = pending(S, <<"three-gap-http-link">>, <<"link">>, Device),
    #{<<"application_key">> := Wrong} = intbe02_http_support:one(
        maps:get(conn, S),
        <<"SELECT application_key FROM enterprise_application WHERE id=$1 AND status='active'">>,
        [maps:get(app_a, S)]
    ),
    ?assertNotEqual(maps:get(<<"application_key">>, Params), Wrong),
    Before = enterprise_oa_login_three_gap_pg_checks:snapshot(S),
    error_reply(
        post(S, link, Params#{<<"application_key">> => Wrong}, Auth),
        400,
        <<"invalid_request">>
    ),
    ?assertEqual(Before, enterprise_oa_login_three_gap_pg_checks:snapshot(S)),
    Session = success(post(S, link, Params, Auth)),
    assert_session(Session, User, Device),
    assert_binding(S, <<"three-gap-http-link">>, User).
