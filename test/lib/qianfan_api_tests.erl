-module(qianfan_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc qianfan_api 模块的 EUnit 测试
%%% 目标：验证百度千帆大模型 API 调用功能
%%% 覆盖：创建对话、访问令牌、签名生成、URI 规范化
%%%===================================================================

%% ===================================================================
%% create_chat/3 测试
%% ===================================================================

create_chat_success_test_() ->
    ?WITH_MECKS([
        {qianfan_api, [
            {'access_token', 0, fun() -> <<"test_access_token">> end},
            {'signature', 3, fun(_Method, _URL, _Headers) ->
                [<<"auth_prefix">>, <<"signed_headers">>, <<"signature">>]
            end}
        ]},
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(Uid) -> ?assertEqual(1, Uid), <<"encoded_uid">> end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, _Data, _Headers) ->
                {ok, #{<<"result">> => <<"AI 回复"/utf8>>}}
            end}
        ]},
        {imboy_error, [
            {'DEBUG_LOG', 2, fun(_Msg, _State) -> ok end}
        ]}
    ], fun() ->
        Result = qianfan_api:create_chat(1, <<"你好"/utf8>>, []),
        ?assertMatch(#{<<"result">> := <<"AI 回复"/utf8>>}, Result)
    end).

%% ===================================================================
%% access_token/0 测试
%% ===================================================================

access_token_from_cache_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'memo', 3, fun(Fun, _Key, _TTL) -> Fun() end},
            {'get', 1, fun(_Key) -> {ok, <<"cached_token">>} end}
        ]},
        {config_ds, [
            {'env', 1, fun(qianfan) ->
                #{api_key => <<"test_api_key">>, secret_key => <<"test_secret_key">>}
            end}
        ]}
    ], fun() ->
        Token = qianfan_api:access_token(),
        ?assertEqual(<<"cached_token">>, Token)
    end).

%% ===================================================================
%% signature/3 测试
%% ===================================================================

signature_success_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(qianfan) ->
                #{auth_access_key => <<"test_access_key">>, auth_secret_key => <<"test_secret_key">>}
            end}
        ]},
        {qianfan_api, [
            {'canonical_uri', 1, fun(_URL) -> {<<"/api/test">>, <<>>} end},
            {'canonical_header', 1, fun(_Headers) -> <<"canonical_header">> end},
            {'generate', 1, fun(_StringToSign) ->
                [<<"auth_prefix">>, <<"signed_headers">>, <<"signature">>]
            end}
        ]}
    ], fun() ->
        Result = qianfan_api:signature("POST", <<"https://aip.baidubce.com/api/test">>, []),
        ?assertEqual(3, length(Result))
    end).

%% ===================================================================
%% canonical_uri/1 测试
%% ===================================================================

canonical_uri_with_full_url_test_() ->
    ?WITH_MECKS([
        {elib_uri, [
            {'parse', 1, fun(_URL) ->
                #{host => <<"aip.baidubce.com">>, path => <<"/api/test">>, query => <<"key=value">>}
            end}
        ]}
    ], fun() ->
        {URI, Query} = qianfan_api:canonical_uri(<<"https://aip.baidubce.com/api/test?key=value">>),
        ?assertEqual(<<"/api/test">>, URI),
        ?assertEqual(<<"key=value">>, Query)
    end).

canonical_uri_without_query_test_() ->
    ?WITH_MECKS([
        {elib_uri, [
            {'parse', 1, fun(_URL) ->
                #{host => <<"aip.baidubce.com">>, path => <<"/api/test">>, query => undefined}
            end}
        ]}
    ], fun() ->
        {URI, Query} = qianfan_api:canonical_uri(<<"https://aip.baidubce.com/api/test">>),
        ?assertEqual(<<"/api/test">>, URI),
        ?assertEqual(<<>>, Query)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_chat_with_empty_content_test_() ->
    ?WITH_MECKS([
        {qianfan_api, [
            {'access_token', 0, fun() -> <<"test_access_token">> end},
            {'signature', 3, fun(_Method, _URL, _Headers) ->
                [<<"auth_prefix">>, <<"signed_headers">>, <<"signature">>]
            end}
        ]},
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(_Uid) -> <<"encoded_uid">> end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, _Data, _Headers) -> {ok, #{<<"result">> => <<>>}} end}
        ]},
        {imboy_error, [
            {'DEBUG_LOG', 2, fun(_Msg, _State) -> ok end}
        ]}
    ], fun() ->
        Result = qianfan_api:create_chat(1, <<>>, []),
        ?assertMatch(#{<<"result">> := <<>>}, Result)
    end).

create_chat_with_utf8_content_test_() ->
    ?WITH_MECKS([
        {qianfan_api, [
            {'access_token', 0, fun() -> <<"test_access_token">> end},
            {'signature', 3, fun(_Method, _URL, _Headers) ->
                [<<"auth_prefix">>, <<"signed_headers">>, <<"signature">>]
            end}
        ]},
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(_Uid) -> <<"encoded_uid">> end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, Data, _Headers) ->
                Content = maps:get(<<"content">>, hd(maps:get(<<"messages">>, Data))),
                ?assertEqual(<<"你好，世界"/utf8>>, Content),
                {ok, #{<<"result">> => <<"你好"/utf8>>}}
            end}
        ]},
        {imboy_error, [
            {'DEBUG_LOG', 2, fun(_Msg, _State) -> ok end}
        ]}
    ], fun() ->
        Result = qianfan_api:create_chat(1, <<"你好，世界"/utf8>>, []),
        ?assertMatch(#{<<"result">> := <<"你好"/utf8>>}, Result)
    end).
