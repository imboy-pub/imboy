-module(elib_uri_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_uri 模块的 EUnit 测试
%%%
%%% 目标：验证 URI 工具功能
%%% 覆盖：URL 编码、解码、解析
%%%===================================================================

encode_with_valid_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        String = <<"hello world">>,
        Result = elib_uri:encode(String),
        % 验证URL编码结果
        ?assertEqual(<<"hello%20world">>, Result)
    end).

decode_with_encoded_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Encoded = <<"hello%20world">>,
        Result = elib_uri:decode(Encoded),
        % 验证URL解码结果
        ?assertEqual(<<"hello world">>, Result)
    end).

parse_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/path?param=value">>,
        Result = elib_uri:parse(Url),
        % 验证URL解析结果
        ?assertMatch(#{scheme := <<"https">>, host := <<"example.com">>, path := <<"/path">>, query := #{<<"param">> := <<"value">>}}, Result)
    end).
