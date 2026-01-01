-module(adm_index_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_index_handler 模块的 EUnit 测试
%%%
%%% 目标：验证管理后台首页处理器功能
%%% 覆盖：首页路由、欢迎页面
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 验证 adm_index_handler 模块可以正常加载
        code:ensure_loaded(adm_index_handler),
        ?assertMatch({file, _}, code:is_loaded(adm_index_handler))
    end).

%% ===================================================================
%% 首页处理器验证
%% ===================================================================

%% @doc 测试首页处理器 - index action
index_handler_responds_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(Req) -> maps:get(method, Req, <<"GET">>) end},
            {'reply', 4, fun(Status, Headers, Body, Req) ->
                Req#{
                    response_status => Status,
                    response_headers => Headers,
                    response_body => Body
                }
            end}
        ]},
        {imboy_syn, [
            {'count_user', 0, fun() -> 100 end},
            {'count', 0, fun() -> 200 end}
        ]},
        {imboy_dtl, [
            {'imadm_param', 1, fun(_State) ->
                [
                    {system_name, "IMBoy Admin System"},
                    {adm_nickname, <<>>}
                ]
            end},
            {'template', 3, fun(_Template, _Data, _App) ->
                {ok, <<"<html><body>Admin Index</body></html>">>}
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        % 调用 handler
        {ok, Req, _State} = adm_index_handler:init(MockReq, #{action => index}),

        % 验证响应
        {StatusCode, Headers, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_EQUAL(<<"text/html; charset=utf-8">>, maps:get(<<"content-type">>, Headers, undefined)),

        % 再验证 welcome action
        MockReq2 = cowboy_req_h:new(#{
            method => <<"GET">>
        }),
        {ok, Req2, _State2} = adm_index_handler:init(MockReq2, #{action => welcome}),
        {StatusCode2, Headers2, _Body2} = cowboy_req_h:response(Req2),
        ?ASSERT_EQUAL(200, StatusCode2),
        ?ASSERT_EQUAL(<<"text/html; charset=utf-8">>, maps:get(<<"content-type">>, Headers2, undefined)),

        % 验证 Mock 调用
        meck_helper:verify_called(imboy_syn, count_user, 0),
        meck_helper:verify_called(imboy_syn, count, 0)
    end).

%% @doc 测试无效的 action
invalid_action_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        % 调用 handler，使用不存在的 action
        ?assertError({case_clause, invalid}, adm_index_handler:init(MockReq, #{action => invalid}))
    end).
