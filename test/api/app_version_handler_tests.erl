-module(app_version_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_handler 模块的 EUnit 测试
%%%
%%% 目标：验证应用版本处理器功能
%%% 覆盖：版本检查、更新验证
%%%===================================================================

%% ===================================================================
%% 版本更新检查测试
%% ===================================================================

%% @doc 测试版本更新检查 - 有更新可用
handle_check_update_test_() ->
    ?WITH_MECKS([
        {app_version_repo, [
            {'find', 2, fun(_Where, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"region_code">> => <<"cn">>,
                    <<"type">> => <<"web">>,
                    <<"package_name">> => <<"com.example.app">>,
                    <<"app_name">> => <<"Test App">>,
                    <<"vsn">> => <<"1.1.0">>,
                    <<"download_url">> => <<"https://example.com/download">>,
                    <<"description">> => <<"Bug fixes">>,
                    <<"force_update">> => false
                }
            end}
        ]},
        {imboy_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]},
        {ec_semver, [
            {'lt', 2, fun(_Vsn1, _Vsn2) ->
                true
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            headers => #{<<"cos">> => <<"web">>},
            qs => <<"vsn=1.0.0&region_code=">>
        }),

        % 调用 handler
        {ok, Req, _State} = app_version_handler:init(MockReq, #{action => check}),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"updatable">> := true}}, Body)
    end).

%% @doc 测试版本更新检查 - 无更新可用
handle_check_no_update_test_() ->
    ?WITH_MECKS([
        {app_version_repo, [
            {'find', 2, fun(_Where, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"region_code">> => <<"cn">>,
                    <<"type">> => <<"web">>,
                    <<"package_name">> => <<"com.example.app">>,
                    <<"app_name">> => <<"Test App">>,
                    <<"vsn">> => <<"1.0.0">>,
                    <<"download_url">> => <<"https://example.com/download">>,
                    <<"description">> => <<"Bug fixes">>,
                    <<"force_update">> => false
                }
            end}
        ]},
        {imboy_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]},
        {ec_semver, [
            {'lt', 2, fun(_Vsn1, _Vsn2) ->
                false
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            headers => #{<<"cos">> => <<"web">>},
            qs => <<"vsn=1.0.0&region_code=">>
        }),

        % 调用 handler
        {ok, Req, _State} = app_version_handler:init(MockReq, #{action => check}),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"updatable">> := false}}, Body)
    end).

%% @doc 测试版本更新检查 - 带区域码
handle_check_with_region_test_() ->
    ?WITH_MECKS([
        {app_version_repo, [
            {'find', 2, fun(Where, _Column) ->
                % 验证 WHERE 条件包含区域码
                case binary:match(Where, <<"region_code='us'">>) of
                    nomatch -> error({unexpected_where, Where});
                    _ -> ok
                end,
                #{
                    <<"id">> => 1,
                    <<"region_code">> => <<"us">>,
                    <<"type">> => <<"web">>,
                    <<"package_name">> => <<"com.example.app">>,
                    <<"app_name">> => <<"Test App">>,
                    <<"vsn">> => <<"1.1.0">>,
                    <<"download_url">> => <<"https://example.com/download">>,
                    <<"description">> => <<"Bug fixes">>,
                    <<"force_update">> => false
                }
            end}
        ]},
        {imboy_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]},
        {ec_semver, [
            {'lt', 2, fun(_Vsn1, _Vsn2) ->
                true
            end}
        ]}
    ], fun() ->
        % 模拟请求（带区域码）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            headers => #{<<"cos">> => <<"web">>},
            qs => <<"vsn=1.0.0&region_code=us">>
        }),

        % 调用 handler
        {ok, Req, _State} = app_version_handler:init(MockReq, #{action => check}),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"updatable">> := true}}, Body)
    end).

%% @doc 测试版本更新检查 - 不同平台
handle_check_different_platform_test_() ->
    ?WITH_MECKS([
        {app_version_repo, [
            {'find', 2, fun(Where, _Column) ->
                % 验证 WHERE 条件包含平台类型
                case binary:match(Where, <<"type='ios'">>) of
                    nomatch -> error({unexpected_where, Where});
                    _ -> ok
                end,
                #{
                    <<"id">> => 1,
                    <<"region_code">> => <<"cn">>,
                    <<"type">> => <<"ios">>,
                    <<"package_name">> => <<"com.example.app">>,
                    <<"app_name">> => <<"Test App">>,
                    <<"vsn">> => <<"1.1.0">>,
                    <<"download_url">> => <<"https://apps.apple.com/app">>,
                    <<"description">> => <<"Bug fixes">>,
                    <<"force_update">> => false
                }
            end}
        ]},
        {imboy_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]},
        {ec_semver, [
            {'lt', 2, fun(_Vsn1, _Vsn2) ->
                true
            end}
        ]}
    ], fun() ->
        % 模拟请求（iOS 平台）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            headers => #{<<"cos">> => <<"ios">>},
            qs => <<"vsn=1.0.0&region_code=">>
        }),

        % 调用 handler
        {ok, Req, _State} = app_version_handler:init(MockReq, #{action => check}),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"updatable">> := true}}, Body),

        % 验证返回的版本信息
        #{data := VersionData} = Body,
        ?ASSERT_EQUAL(<<"1.1.0">>, maps:get(<<"vsn">>, VersionData)),
        ?ASSERT_EQUAL(<<"ios">>, maps:get(<<"type">>, VersionData))
    end).
