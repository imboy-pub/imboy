-module(index_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"GET">>, path => <<"/init">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = index_handler:init(Req, #{action => false}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{}, State)
    end).

init_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 3, fun
                (<<"vsn">>, _Req, _Default) -> <<"1.0.0">>;
                (<<"cos">>, _Req, _Default) -> <<"ios">>;
                (<<"pkg">>, _Req, _Default) -> <<"com.imboy.test">>;
                (<<"sk">>, _Req, Default) -> Default;
                (_Name, _Req, Default) -> Default
            end}
        ]},
        {config_ds, [
            {'get', 1, fun
                (<<"solidified_key">>) -> <<"sol_key">>;
                (<<"solidified_key_iv">>) -> <<"sol_iv">>;
                (<<"ws_url">>) -> <<"wss://example.test/ws">>;
                (<<"upload_url">>) -> <<"https://example.test/upload">>;
                (<<"upload_key">>) -> <<"upload_key">>;
                (<<"upload_scene">>) -> <<"upload_scene">>;
                (<<"login_pwd_rsa_encrypt">>) -> false;
                (<<"login_rsa_pub_key">>) -> <<"rsa_pub">>
            end}
        ]},
        {app_version_ds, [
            {'sign_key', 3, fun(<<"ios">>, <<"1.0.0">>, <<"com.imboy.test">>) ->
                <<>>
            end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(<<"sol_key">>) ->
                <<"md5_key">>
            end}
        ]},
        {jsone, [
            {'encode', 1, fun(_Data) ->
                <<"encoded_json">>
            end}
        ]},
        {elib_cipher, [
            {'aes_encrypt', 4, fun(aes_256_cbc, <<"encoded_json">>, <<"md5_key">>, <<"sol_iv">>) ->
                <<"cipher_bin">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) ->
                {ok, [#{<<"lexeme">> => <<"ok">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = index_handler:init(Req, #{action => init}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"cipher_bin">>, maps:get(res, Payload)),
        ?assertEqual(#{<<"lexeme">> => <<"ok">>}, maps:get(test, Payload)),
        ?assertEqual(#{}, State)
    end).
