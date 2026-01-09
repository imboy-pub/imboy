-module(imboy_req_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_req 模块的 EUnit 测试
%%%
%%% 目标：验证请求处理工具功能
%%% 覆盖：请求解析、参数提取、HTTP请求、IP获取、Cookie处理
%%%===================================================================

%% ===================================================================
%% get_client_ip/1 测试
%% ===================================================================

get_client_ip_with_x_forwarded_for_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, header, fun(<<"x-forwarded-for">>, _Req0, undefined) -> <<"1.2.3.4,5.6.7.8">> end),
            ?assertEqual(<<"1.2.3.4">>, imboy_req:get_client_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

get_client_ip_without_x_forwarded_for_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, header, fun(<<"x-forwarded-for">>, _Req0, undefined) -> undefined end),
            meck:expect(cowboy_req, peer, fun(_Req0) -> {{127, 0, 0, 1}, 1234} end),
            ?assertEqual(<<"127.0.0.1">>, imboy_req:get_client_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

get_client_ip_with_spaces_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, header, fun(<<"x-forwarded-for">>, _Req0, undefined) -> <<"  192.168.1.1  ,  10.0.0.1  ">> end),
            ?assertEqual(<<"192.168.1.1">>, imboy_req:get_client_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

get_client_ip_ipv6_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, header, fun(<<"x-forwarded-for">>, _Req0, undefined) -> undefined end),
            meck:expect(cowboy_req, peer, fun(_Req0) -> {{0,0,0,0,0,0,0,1}, 1234} end),
            ?assertEqual(<<"::1">>, imboy_req:get_client_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% peer_ip/1 测试
%% ===================================================================

peer_ip_ipv4_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, peer, fun(_Req0) -> {{192, 168, 1, 100}, 8080} end),
            ?assertEqual(<<"192.168.1.100">>, imboy_req:peer_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

peer_ip_ipv6_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, peer, fun(_Req0) -> {{8193, 3512, 0, 0, 0, 0, 0, 1}, 443} end),
            ?assertEqual(<<"2001:dc8::1">>, imboy_req:peer_ip(Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% cookie/2 测试
%% ===================================================================

cookie_existing_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, parse_cookies, fun(_Req0) -> [{<<"session_id">>, <<"abc123">>}, {<<"user">>, <<"john">>}] end),
            ?assertEqual(<<"abc123">>, imboy_req:cookie(<<"session_id">>, Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

cookie_nonexistent_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, parse_cookies, fun(_Req0) -> [{<<"session_id">>, <<"abc123">>}] end),
            ?assertEqual(false, imboy_req:cookie(<<"nonexistent">>, Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

cookie_empty_cookies_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, parse_cookies, fun(_Req0) -> [] end),
            ?assertEqual(false, imboy_req:cookie(<<"any">>, Req0)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% post_params/1 测试
%% ===================================================================

post_params_urlencoded_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Body = <<"username=john&password=secret&age=30">>,
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, Body, Req0} end),
            meck:expect(cowboy_req, header, fun(<<"content-type">>, _Req0, <<>>) -> <<"application/x-www-form-urlencoded; charset=utf-8">> end),
            Params = imboy_req:post_params(Req0),
            ?assertEqual(<<"john">>, maps:get(<<"username">>, Params)),
            ?assertEqual(<<"secret">>, maps:get(<<"password">>, Params)),
            ?assertEqual(<<"30">>, maps:get(<<"age">>, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_params_json_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Body = <<"{\"name\":\"Alice\",\"age\":25,\"active\":true}">>,
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, Body, Req0} end),
            meck:expect(cowboy_req, header, fun(<<"content-type">>, _Req0, <<>>) -> <<"application/json">> end),
            Params = imboy_req:post_params(Req0),
            ?assertEqual(<<"Alice">>, maps:get(<<"name">>, Params)),
            ?assertEqual(25, maps:get(<<"age">>, Params)),
            ?assertEqual(true, maps:get(<<"active">>, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_params_empty_body_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<>>, Req0} end),
            Params = imboy_req:post_params(Req0),
            ?assertEqual(#{}, Params),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_params_multipart_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Body = <<"multipart data">>,
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, Body, Req0} end),
            meck:expect(cowboy_req, header, fun(<<"content-type">>, _Req0, <<>>) -> <<"multipart/form-data; boundary=----WebKitFormBoundary">> end),
            Params = imboy_req:post_params(Req0),
            ?assertEqual(#{}, Params),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% parse_urlencoded_body/1 测试
%% ===================================================================

parse_urlencoded_body_simple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"key1=value1&key2=value2">>,
        {ok, Params} = imboy_req:parse_urlencoded_body(Body),
        ?assertEqual(<<"value1">>, maps:get(<<"key1">>, Params)),
        ?assertEqual(<<"value2">>, maps:get(<<"key2">>, Params))
    end).

parse_urlencoded_body_with_spaces_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"name=John%20Doe&city=New%20York">>,
        {ok, Params} = imboy_req:parse_urlencoded_body(Body),
        ?assertEqual(<<"John Doe">>, maps:get(<<"name">>, Params)),
        ?assertEqual(<<"New York">>, maps:get(<<"city">>, Params))
    end).

parse_urlencoded_body_with_special_chars_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"email=test%40example.com&message=Hello%20World%21">>,
        {ok, Params} = imboy_req:parse_urlencoded_body(Body),
        ?assertEqual(<<"test@example.com">>, maps:get(<<"email">>, Params)),
        ?assertEqual(<<"Hello World!">>, maps:get(<<"message">>, Params))
    end).

parse_urlencoded_body_empty_value_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"key1=&key2=value2">>,
        {ok, Params} = imboy_req:parse_urlencoded_body(Body),
        ?assertEqual(<<>>, maps:get(<<"key1">>, Params)),
        ?assertEqual(<<"value2">>, maps:get(<<"key2">>, Params))
    end).

parse_urlencoded_body_empty_body_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<>>,
        {ok, Params} = imboy_req:parse_urlencoded_body(Body),
        ?assertEqual(#{}, Params)
    end).

%% ===================================================================
%% parse_key_value_pairs/1 测试
%% ===================================================================

parse_key_value_pairs_simple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"a=1&b=2">>,
        {ok, Params} = imboy_req:parse_key_value_pairs(Body),
        ?assertEqual(<<"1">>, maps:get(<<"a">>, Params)),
        ?assertEqual(<<"2">>, maps:get(<<"b">>, Params))
    end).

parse_key_value_pairs_duplicate_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"a=1&a=2&a=3">>,
        {ok, Params} = imboy_req:parse_key_value_pairs(Body),
        ?assertEqual([<<"1">>, <<"2">>, <<"3">>], maps:get(<<"a">>, Params))
    end).

parse_key_value_pairs_duplicate_keys_two_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"a=1&b=2&a=3">>,
        {ok, Params} = imboy_req:parse_key_value_pairs(Body),
        ?assertEqual([<<"1">>, <<"3">>], maps:get(<<"a">>, Params)),
        ?assertEqual(<<"2">>, maps:get(<<"b">>, Params))
    end).

parse_key_value_pairs_key_only_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"a=&b=value">>,
        {ok, Params} = imboy_req:parse_key_value_pairs(Body),
        ?assertEqual(<<>>, maps:get(<<"a">>, Params)),
        ?assertEqual(<<"value">>, maps:get(<<"b">>, Params))
    end).

parse_key_value_pairs_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<>>,
        {ok, Params} = imboy_req:parse_key_value_pairs(Body),
        ?assertEqual(#{}, Params)
    end).

%% ===================================================================
%% parse_json_body/1 测试
%% ===================================================================

parse_json_body_object_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"{\"name\":\"Test\",\"value\":123}">>,
        {ok, Result} = imboy_req:parse_json_body(Body),
        ?assertEqual(<<"Test">>, maps:get(<<"name">>, Result)),
        ?assertEqual(123, maps:get(<<"value">>, Result))
    end).

parse_json_body_array_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"[1,2,3]">>,
        {ok, Result} = imboy_req:parse_json_body(Body),
        ?assertEqual([1,2,3], Result)
    end).

parse_json_body_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"\"hello\"">>,
        {ok, Result} = imboy_req:parse_json_body(Body),
        ?assertEqual(<<"hello">>, Result)
    end).

parse_json_body_number_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"42">>,
        {ok, Result} = imboy_req:parse_json_body(Body),
        ?assertEqual(42, Result)
    end).

parse_json_body_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<"{invalid json}">>,
        Result = imboy_req:parse_json_body(Body),
        ?assertMatch({error, _}, Result)
    end).

parse_json_body_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Body = <<>>,
        Result = imboy_req:parse_json_body(Body),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% get/1,2 测试 (HTTP GET 请求)
%% =================================================================%%

get_request_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/test">>,
            ResponseBody = <<"{\"status\":\"ok\",\"data\":123}">>,
            meck:expect(httpc, request, fun(get, _, _, _) ->
                {ok, {{'HTTP/1.1', 200, 'OK'}, [], ResponseBody}}
            end),
            {ok, Result} = imboy_req:get(Url),
            ?assertEqual(<<"ok">>, maps:get(<<"status">>, Result)),
            ?assertEqual(123, maps:get(<<"data">>, Result)),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

get_request_with_custom_headers_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        Url = <<"http://example.com/api/test">>,
        Headers = [{"authorization", "Bearer token123"}],
        ResponseBody = <<"{\"result\":\"success\"}">>,
        meck:expect(httpc, request, fun(get, _, _, _) ->
            {ok, {{'HTTP/1.1', 200, 'OK'}, [], ResponseBody}}
        end),
        {ok, Result} = imboy_req:get(Url, Headers),
        ?assertEqual(<<"success">>, maps:get(<<"result">>, Result)),
        ?assert(meck:validate(httpc)),
        meck:unload(httpc)
    end).

get_request_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/error">>,
            meck:expect(httpc, request, fun(get, _, _, _) ->
                {error, timeout}
            end),
            Result = imboy_req:get(Url),
            ?assertMatch({error, timeout}, Result),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

%% ===================================================================
%% post/2,3 测试 (HTTP POST 请求)
%% ===================================================================

post_request_map_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/create">>,
            Params = #{name => <<"test">>, value => 123},
            ResponseBody = <<"{\"id\":456,\"status\":\"created\"}">>,
            meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{'HTTP/1.1', 200, 'OK'}, [], ResponseBody}}
            end),
            {ok, Result} = imboy_req:post(Url, Params),
            ?assertEqual(456, maps:get(<<"id">>, Result)),
            ?assertEqual(<<"created">>, maps:get(<<"status">>, Result)),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

post_request_list_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/batch">>,
            Params = [1, 2, 3],
            ResponseBody = <<"{\"count\":3}">>,
            meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{'HTTP/1.1', 200, 'OK'}, [], ResponseBody}}
            end),
            {ok, Result} = imboy_req:post(Url, Params),
            ?assertEqual(3, maps:get(<<"count">>, Result)),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

post_request_with_custom_headers_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/update">>,
            Params = #{id => 789, status => <<"active">>},
            Headers = [{"authorization", "Bearer token123"}, {"x-custom", "value"}],
            ResponseBody = <<"{\"updated\":true}">>,
            meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{'HTTP/1.1', 200, 'OK'}, [], ResponseBody}}
            end),
            {ok, Result} = imboy_req:post(Url, Params, Headers),
            ?assertEqual(true, maps:get(<<"updated">>, Result)),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

post_request_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"http://example.com/api/error">>,
            Params = #{test => <<"data">>},
            meck:expect(httpc, request, fun(post, _, _, _) ->
                {error, connection_refused}
            end),
            Result = imboy_req:post(Url, Params),
            ?assertMatch({error, connection_refused}, Result),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).
