-module(e2ee_social_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc e2ee_social_handler 基础行为测试
%%%===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(e2ee_social_handler),
        ?assertMatch({file, _}, code:is_loaded(e2ee_social_handler))
    end).

handle_action_false_returns_original_req_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{uri => <<"/v1/e2ee/social/unknown">>}),
        Result = e2ee_social_handler:handle_action(false, Req0, #{}),
        ?assertEqual(Req0, Result)
    end).

init_with_false_action_removes_action_from_state_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{}),
        {ok, Req1, State1} = e2ee_social_handler:init(Req0, #{action => false, keep => 1}),
        ?assertEqual(Req0, Req1),
        ?assertEqual(#{keep => 1}, State1)
    end).

extract_encrypted_shard_prefers_new_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        Shard = #{
            <<"encrypted_shard">> => <<"cipher-new">>,
            <<"encrypted_data">> => <<"cipher-legacy">>
        },
        ?assertEqual(
            {ok, <<"cipher-new">>},
            e2ee_social_handler:extract_encrypted_shard(Shard)
        )
    end).

extract_encrypted_shard_supports_legacy_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        Shard = #{<<"encrypted_data">> => <<"cipher-legacy">>},
        ?assertEqual(
            {ok, <<"cipher-legacy">>},
            e2ee_social_handler:extract_encrypted_shard(Shard)
        )
    end).

extract_encrypted_shard_missing_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, shard_data_missing},
            e2ee_social_handler:extract_encrypted_shard(#{})
        )
    end).

map_decrypt_shard_error_not_found_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {<<"分片不存在"/utf8>>, ?ERR_NOT_FOUND},
            e2ee_social_handler:map_decrypt_shard_error(not_found)
        )
    end).

map_decrypt_shard_error_key_not_found_test_() ->
    ?TEST_SIMPLE(fun() ->
        {Msg, Code} = e2ee_social_handler:map_decrypt_shard_error(private_key_not_found),
        ?assertEqual(?ERR_E2EE_KEY_NOT_FOUND, Code),
        ?assert(is_binary(Msg))
    end).

map_decrypt_shard_error_decryption_failed_test_() ->
    ?TEST_SIMPLE(fun() ->
        {Msg, Code} = e2ee_social_handler:map_decrypt_shard_error(decryption_failed),
        ?assertEqual(?ERR_E2EE_DECRYPTION_FAILED, Code),
        ?assert(is_binary(Msg))
    end).

%% 零信任 + 一次性语义：服务端只返回加密分片（不解密），
%% 取用走 consume_proxy_shard（成功即置 used）。
decrypt_shard_prefers_encrypted_shard_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"shard_id\":\"shard-1\"}">>, req_after_body}
                end}
            ]},
            {throttle, [
                {'check', 2, fun(e2ee_decrypt_shard, _) -> ok end}
            ]},
            {e2ee_social_ds, [
                {'consume_proxy_shard', 2, fun(<<"shard-1">>, 100) ->
                    {ok, #{
                        <<"uid">> => 9999,
                        <<"encrypted_shard">> => <<"cipher-new">>,
                        <<"encrypted_data">> => <<"cipher-legacy">>
                    }}
                end}
            ]},
            {e2ee_shard_validator, [
                {'log_shard_transmission', 3, fun(shard_decrypted, _, _) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_social_handler:decrypt_shard(Req0, #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertMatch(
                #{<<"encrypted_shard">> := <<"cipher-new">>},
                receive_resp_data()
            ),
            %% 取用必须写审计日志
            ?assertEqual(1, meck:num_calls(e2ee_shard_validator, log_shard_transmission, 3))
        end
    ).

decrypt_shard_supports_legacy_encrypted_data_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"shard_id\":\"shard-legacy\"}">>, req_after_body}
                end}
            ]},
            {throttle, [
                {'check', 2, fun(e2ee_decrypt_shard, _) -> ok end}
            ]},
            {e2ee_social_ds, [
                {'consume_proxy_shard', 2, fun(<<"shard-legacy">>, 100) ->
                    {ok, #{
                        <<"uid">> => 9999,
                        <<"encrypted_data">> => <<"cipher-legacy">>
                    }}
                end}
            ]},
            {e2ee_shard_validator, [
                {'log_shard_transmission', 3, fun(shard_decrypted, _, _) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_social_handler:decrypt_shard(Req0, #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertMatch(
                #{<<"encrypted_shard">> := <<"cipher-legacy">>},
                receive_resp_data()
            )
        end
    ).

decrypt_shard_not_found_maps_to_not_found_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"shard_id\":\"not-exist\"}">>, req_after_body}
                end}
            ]},
            {throttle, [
                {'check', 2, fun(e2ee_decrypt_shard, _) -> ok end}
            ]},
            {e2ee_social_ds, [
                {'consume_proxy_shard', 2, fun(<<"not-exist">>, 100) ->
                    {error, not_found}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_social_handler:decrypt_shard(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_NOT_FOUND, receive_resp_code())
        end
    ).

decrypt_shard_already_used_is_rejected_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"shard_id\":\"shard-used\"}">>, req_after_body}
                end}
            ]},
            {throttle, [
                {'check', 2, fun(e2ee_decrypt_shard, _) -> ok end}
            ]},
            {e2ee_social_ds, [
                {'consume_proxy_shard', 2, fun(<<"shard-used">>, 100) ->
                    {error, shard_not_active}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_social_handler:decrypt_shard(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

receive_resp_data() ->
    receive
        {resp_data, Data} -> Data
    after 1000 ->
        timeout
    end.

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.
