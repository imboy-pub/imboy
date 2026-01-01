-module(imboy_req_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_req 模块的 EUnit 测试
%%%
%%% 目标：验证请求处理工具功能
%%% 覆盖：请求解析、参数提取
%%%===================================================================

get_header_from_request_test_() ->
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

get_body_from_request_test_() ->
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

get_query_param_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Body = <<"a=1&b=2">>,
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, Body, Req0} end),
            meck:expect(cowboy_req, header, fun(<<"content-type">>, _Req0, <<>>) -> <<"application/x-www-form-urlencoded">> end),
            Params = imboy_req:post_params(Req0),
            ?assert(lists:member({<<"a">>, <<"1">>}, Params)),
            ?assert(lists:member({<<"b">>, <<"2">>}, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).
