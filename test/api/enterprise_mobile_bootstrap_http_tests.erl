%%% Real init encryption, password transport and device-bound login on marker PG.
-module(enterprise_mobile_bootstrap_http_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/0, run_fixture/0]).
-define(HTTP, intbe02_http_support).
-define(UID, 995011).
-define(KEY, <<"synthetic-mobile-device-key">>).
-define(PASSWORD, <<"Synthetic-password-42">>).
-define(ACCOUNT, <<"synthetic_enterprise_mobile">>).

bootstrap_test_() -> {timeout, 180, fun run/0}.

%% Test-only native UI fixture; synthetic credentials, disposable marker database.
run_fixture() ->
    H = ?HTTP:setup_all(),
    try
        configure(H),
        Port = maps:get(port, H),
        Origin = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port)]),
        application:set_env(
            imboy,
            ws_url,
            iolist_to_binary(["ws://127.0.0.1:", integer_to_list(Port), "/api/v1/ws"])
        ),
        application:set_env(imboy, upload_url, Origin),
        application:set_env(imboy, base_url, Origin),
        application:set_env(imboy, product_experience, chat),
        application:set_env(imboy, product_profile, enterprise),
        application:set_env(imboy, features, #{e2ee => false}),
        ok = app_version_ds:set_sign_key(
            <<"android">>, <<"1">>, <<"pub.imboy.app.gzcustomer">>, ?KEY
        ),
        assert_init(Port, headers()),
        assert_login(Port, headers()),
        Features = request(Port, <<"GET">>, <<"/api/v1/app/features">>, #{}, headers()),
        ?assertEqual(0, maps:get(<<"code">>, Features)),
        Flags = maps:get(<<"payload">>, Features),
        ?assertEqual(true, maps:get(<<"channel">>, Flags)),
        ?assertEqual(false, maps:get(<<"e2ee">>, Flags)),
        Dir = os:getenv("IMBOY_GATE_RUN_DIR"),
        ok = file:write_file(
            filename:join(Dir, "mobile-fixture.json"),
            jsone:encode(#{
                port => Port,
                account => ?ACCOUNT,
                password => ?PASSWORD,
                sign_key => ?KEY,
                organization_id => <<"995101">>,
                package => <<"pub.imboy.app.gzcustomer">>,
                synthetic_only => true
            })
        ),
        await_device_done(Dir, erlang:monotonic_time(millisecond) + 1800000)
    after
        ?HTTP:teardown_all(H),
        inttest_marker_db:release(H)
    end.

await_device_done(Dir, Deadline) ->
    case filelib:is_file(filename:join(Dir, "mobile.done")) of
        true ->
            ok;
        false ->
            ?assert(erlang:monotonic_time(millisecond) < Deadline),
            receive
            after 100 -> await_device_done(Dir, Deadline)
            end
    end.

run() ->
    H = ?HTTP:setup_all(),
    Old = [
        {K, application:get_env(imboy, K)}
     || K <- [
            api_auth_switch, login_pwd_rsa_encrypt, init_config_legacy_cbc
        ]
    ],
    try
        configure(H),
        Headers = headers(),
        Port = maps:get(port, H),
        assert_init(Port, Headers),
        assert_login(Port, Headers),
        save_proof()
    after
        ?HTTP:teardown_all(H),
        inttest_marker_db:release(H),
        [restore(K, V) || {K, V} <- Old]
    end.

configure(#{conn := C}) ->
    application:set_env(imboy, api_auth_switch, <<"on">>),
    application:set_env(imboy, login_pwd_rsa_encrypt, <<"on">>),
    application:set_env(imboy, init_config_legacy_cbc, <<"off">>),
    ok = app_version_ds:set_sign_key(
        <<"android">>, <<"1">>, <<"synthetic.enterprise.mobile">>, ?KEY
    ),
    Hash = elib_password:generate(?PASSWORD),
    ok = ?HTTP:sql_exec(
        C,
        <<"UPDATE \"user\" SET account=$1,password=$2 WHERE id=$3">>,
        [?ACCOUNT, Hash, ?UID]
    ).

headers() ->
    #{
        <<"cos">> => <<"android">>,
        <<"vsn">> => <<"1">>,
        <<"pkg">> => <<"synthetic.enterprise.mobile">>,
        <<"did">> => <<"synthetic-mobile-device">>,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<"synthetic-mobile-device|1|android|synthetic.enterprise.mobile">>, ?KEY
        )
    }.

assert_init(Port, Headers) ->
    Good = request(Port, <<"GET">>, <<"/api/v1/init">>, #{}, Headers),
    ?assertEqual(0, maps:get(<<"code">>, Good)),
    Payload = maps:get(<<"payload">>, Good),
    ?assertNot(maps:is_key(<<"res">>, Payload)),
    {ok, Plain} = elib_cipher:aes_gcm_decrypt(
        maps:get(<<"res_v2">>, Payload), elib_hasher:md5(?KEY)
    ),
    Config = jsone:decode(Plain),
    ?assertEqual(<<"1">>, maps:get(<<"login_pwd_rsa_encrypt">>, Config)),
    ?assertEqual(config_ds:env(login_rsa_pub_key), maps:get(<<"login_rsa_pub_key">>, Config)),
    Bad = request(
        Port,
        <<"GET">>,
        <<"/api/v1/init">>,
        #{},
        Headers#{<<"sign">> => <<"invalid-synthetic-signature">>}
    ),
    ?assertEqual(902, maps:get(<<"code">>, Bad)).

assert_login(Port, Headers) ->
    Wrong = login(Port, Headers, <<"Wrong-synthetic-password">>),
    ?assertNotEqual(0, maps:get(<<"code">>, Wrong)),
    First = login(Port, Headers, elib_hasher:md5(?PASSWORD)),
    ?assertNotEqual(0, maps:get(<<"code">>, First)),
    ?assertEqual(<<"errorPassword">>, maps:get(<<"msg">>, First)),
    %% A queued notification worker must not delay the device authentication row.
    ok = sys:suspend(user_server),
    try
        assert_ready_login(Port, Headers)
    after
        ok = sys:resume(user_server)
    end.

assert_ready_login(Port, Headers) ->
    Good = login(Port, Headers, ?PASSWORD),
    ?assertEqual(0, maps:get(<<"code">>, Good)),
    Payload = maps:get(<<"payload">>, Good),
    ?assertEqual(?UID, maps:get(<<"uid">>, Payload)),
    Token = maps:get(<<"token">>, Payload),
    ?assertMatch(
        {ok, ?UID, _, <<"tk">>, <<"synthetic-mobile-device">>, _},
        token_ds:decrypt_token(Token)
    ),
    Auth = maps:merge(Headers, ?HTTP:auth(Token)),
    Mine = request(Port, <<"GET">>, <<"/api/v1/organizations/mine">>, #{}, Auth),
    ?assertEqual(0, maps:get(<<"code">>, Mine)),
    Invalid = request(
        Port,
        <<"GET">>,
        <<"/api/v1/organizations/mine">>,
        #{},
        Auth#{<<"sign">> => <<"invalid-synthetic-signature">>}
    ),
    ?assertEqual(902, maps:get(<<"code">>, Invalid)).

login(Port, Headers, Password) ->
    Cipher = elib_cipher:rsa_encrypt(Password),
    ?assert(is_binary(Cipher)),
    request(
        Port,
        <<"POST">>,
        <<"/api/v1/passport/login">>,
        #{
            <<"type">> => <<"account">>,
            <<"account">> => ?ACCOUNT,
            <<"pwd">> => Cipher,
            <<"rsa_encrypt">> => <<"1">>
        },
        Headers
    ).

request(Port, Method, Path, Body, Headers) ->
    R = ?HTTP:http(Port, Method, Path, Body, Headers),
    record_auth_failure(R),
    ?assertEqual(200, maps:get(status, R)),
    jsone:decode(maps:get(body, R)).

record_auth_failure(#{status := 401, body := Body}) ->
    Envelope = jsone:decode(Body),
    Before = user_device_ds:is_active(?UID, <<"synthetic-mobile-device">>),
    timer:sleep(100),
    After = user_device_ds:is_active(?UID, <<"synthetic-mobile-device">>),
    timer:sleep(900),
    Later = user_device_ds:is_active(?UID, <<"synthetic-mobile-device">>),
    Dir = os:getenv("IMBOY_GATE_RUN_DIR", "/tmp"),
    ok = file:write_file(
        filename:join(Dir, "mobile-bootstrap-auth-failure.json"),
        jsone:encode(#{
            status => 401,
            code => maps:get(<<"code">>, Envelope),
            message => maps:get(<<"msg">>, Envelope),
            device_active_immediate => Before,
            device_active_after_100ms => After,
            device_active_after_1s => Later,
            user_server_running => is_pid(whereis(user_server)),
            device_tsid_registered => lists:member(user_device, elib_tsid:registered())
        })
    );
record_auth_failure(_) ->
    ok.

restore(K, {ok, V}) -> application:set_env(imboy, K, V);
restore(K, undefined) -> application:unset_env(imboy, K).

save_proof() ->
    Dir = os:getenv("IMBOY_GATE_RUN_DIR", "/tmp"),
    file:write_file(
        filename:join(Dir, "mobile-bootstrap-contract.json"),
        jsone:encode(#{
            init_gcm => true,
            rsa_retry => true,
            wrong_password_denied => true,
            device_signature_denied => true,
            token_device_bound => true,
            organization_access => true,
            native_device => false
        })
    ).
