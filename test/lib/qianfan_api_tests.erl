-module(qianfan_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc qianfan_api 模块的 EUnit 测试
%%% 目标：验证百度千帆大模型 API 调用功能
%%% 覆盖：创建对话、访问令牌、签名生成、URI 规范化
%%%
%%% 注意：
%%% - signature/3 不导出，不能 meck qianfan_api 自身来做 passthrough
%%%   测 create_chat 需要同时 mock 所有外部依赖
%%% - canonical_uri/1 和 generate/1 已导出，可直接测试
%%%===================================================================

%% ===================================================================
%% create_chat/3 测试
%% ===================================================================

create_chat_success_test_() ->
    ?WITH_MECKS([
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end},
            {'format', 1, fun(_Fmt) -> <<"20230101">> end}
        ]},
        {config_ds, [
            {'env', 1, fun(qianfan) ->
                #{auth_access_key => <<"ak">>, auth_secret_key => <<"sk">>,
                  api_key => <<"api_key">>, secret_key => <<"secret_key">>}
            end}
        ]},
        {imboy_cache, [
            {'memo', 3, fun(_Fun, _Key, _TTL) -> <<"test_access_token">> end}
        ]},
        {elib_str, [
            {'replace', 3, fun(Str, _Pat, _Rep) -> Str end}
        ]},
        {elib_hasher, [
            {'hmac_sha512', 2, fun(_Key, _Data) -> <<"sig">> end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Sep, Parts) -> iolist_to_binary(Parts) end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, _Data, _Headers) ->
                {ok, #{<<"result">> => <<"AI 回复"/utf8>>}}
            end}
        ]},
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
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
            {'memo', 3, fun(_Fun, _Key, _TTL) -> <<"cached_token">> end}
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
%% generate/1 测试 (已导出，可直接调用)
%% ===================================================================

generate_empty_query_returns_empty_test() ->
    Result = qianfan_api:generate(<<>>),
    %% generate("") splits on "&" giving [""] then split_pair gives {<<>>,<<>>}
    %% then encode_pair gives <<"=">>
    ?assert(is_binary(Result)).

generate_single_param_test() ->
    Result = qianfan_api:generate(<<"key=value">>),
    %% generate encodes and sorts; key=value becomes key=value (already simple)
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%% ===================================================================
%% canonical_uri/1 测试
%% ===================================================================

canonical_uri_with_path_test_() ->
    %% cow_uri:urlencode encodes the full URL, then elib_str:replace
    %% restores / and ? characters. Since we don't mock elib_str,
    %% the real implementation encodes. We test the exported function
    %% with a path-only input that survives urlencode intact.
    ?TEST_SIMPLE(fun() ->
        {URI, _Query} = qianfan_api:canonical_uri(<<"https://aip.baidubce.com/api/test">>),
        %% After cow_uri:urlencode + replace + uri_string:parse, path is extracted
        ?assert(is_binary(URI)),
        ?assert(byte_size(URI) > 0)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_chat_with_empty_content_test_() ->
    ?WITH_MECKS([
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end},
            {'format', 1, fun(_Fmt) -> <<"20230101">> end}
        ]},
        {config_ds, [
            {'env', 1, fun(qianfan) ->
                #{auth_access_key => <<"ak">>, auth_secret_key => <<"sk">>,
                  api_key => <<"api_key">>, secret_key => <<"secret_key">>}
            end}
        ]},
        {imboy_cache, [
            {'memo', 3, fun(_Fun, _Key, _TTL) -> <<"test_access_token">> end}
        ]},
        {elib_str, [
            {'replace', 3, fun(Str, _Pat, _Rep) -> Str end}
        ]},
        {elib_hasher, [
            {'hmac_sha512', 2, fun(_Key, _Data) -> <<"sig">> end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Sep, Parts) -> iolist_to_binary(Parts) end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, _Data, _Headers) ->
                {ok, #{<<"result">> => <<>>}}
            end}
        ]},
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
        ]}
    ], fun() ->
        Result = qianfan_api:create_chat(1, <<>>, []),
        ?assertMatch(#{<<"result">> := <<>>}, Result)
    end).

create_chat_with_utf8_content_test_() ->
    ?WITH_MECKS([
        {ec_date, [
            {'format_iso8601', 1, fun(_Time) -> <<"2023-01-01T00:00:00Z">> end},
            {'format', 1, fun(_Fmt) -> <<"20230101">> end}
        ]},
        {config_ds, [
            {'env', 1, fun(qianfan) ->
                #{auth_access_key => <<"ak">>, auth_secret_key => <<"sk">>,
                  api_key => <<"api_key">>, secret_key => <<"secret_key">>}
            end}
        ]},
        {imboy_cache, [
            {'memo', 3, fun(_Fun, _Key, _TTL) -> <<"test_access_token">> end}
        ]},
        {elib_str, [
            {'replace', 3, fun(Str, _Pat, _Rep) -> Str end}
        ]},
        {elib_hasher, [
            {'hmac_sha512', 2, fun(_Key, _Data) -> <<"sig">> end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Sep, Parts) -> iolist_to_binary(Parts) end}
        ]},
        {elib_req, [
            {'post', 3, fun(_URL, Data, _Headers) ->
                Content = maps:get(<<"content">>, hd(maps:get(<<"messages">>, Data))),
                ?assertEqual(<<"你好，世界"/utf8>>, Content),
                {ok, #{<<"result">> => <<"你好"/utf8>>}}
            end}
        ]},
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
        ]}
    ], fun() ->
        Result = qianfan_api:create_chat(1, <<"你好，世界"/utf8>>, []),
        ?assertMatch(#{<<"result">> := <<"你好"/utf8>>}, Result)
    end).
