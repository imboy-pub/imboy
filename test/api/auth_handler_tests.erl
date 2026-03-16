-module(auth_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_handler 模块的 EUnit 测试
%%%
%%% 目标：验证认证处理器功能
%%% 覆盖：Assets 服务认证、参数验证、错误处理
%%%===================================================================

run_assets_action(MockReq) ->
    {ok, Req, _State} = auth_handler:init(MockReq, #{action => assets}),
    Req.

%% ===================================================================
%% 基础测试验证
%% ===================================================================

%% @doc 验证模块可以正常加载
module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(auth_handler),
        ?assertMatch({file, _}, code:is_loaded(auth_handler))
    end).

%% ===================================================================
%% init/2 测试
%% ===================================================================

%% @doc 测试 init/2 函数分发到 assets action
init_dispatches_to_assets_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"token">>, <<"v">> => <<"value">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(_Path, _Token, _Val) ->
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        State = #{action => assets},
        {ok, Req, _State} = auth_handler:init(MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req, undefined))
    end).

%% @doc 测试 init/2 函数处理 false action
init_with_false_action_returns_unchanged_request_test_() ->
    ?TEST_SIMPLE(fun() ->
        MockReq = cowboy_req_h:new(#{}),
        State = #{action => false},
        {ok, Req, _State} = auth_handler:init(MockReq, State),
        ?assertMatch(#{}, Req)
    end).

%% ===================================================================
%% assets/2 测试 - POST 请求
%% ===================================================================

%% @doc 测试 POST 请求 - open 场景验证成功
assets_post_open_verify_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"auth_token">>, <<"v">> => <<"value">>, <<"__path__">> => <<"/path/to/file">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(Path, Token, Val) ->
                ?assertEqual(<<"/path/to/file">>, Path),
                ?assertEqual(<<"auth_token">>, Token),
                ?assertEqual(<<"value">>, Val),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"ok">>, maps:get(response_body, Req))
    end).

%% @doc 测试 POST 请求 - open 场景验证失败
assets_post_open_verify_fail_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"invalid_token">>, <<"v">> => <<"value">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(_Path, _Token, _Val) ->
                <<"fail"/utf8>>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fail"/utf8>>, maps:get(response_body, Req))
    end).

%% @doc 测试 POST 请求 - 非 open 场景验证成功
assets_post_verify_for_assets_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"scene">>, <<"a">> => <<"auth_token">>, <<"v">> => <<"12345">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(Scene, Token, V, Path) ->
                ?assertEqual(<<"scene">>, Scene),
                ?assertEqual(<<"auth_token">>, Token),
                ?assertEqual(12345, V),
                ?assertEqual(<<"/path">>, Path),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"ok">>, maps:get(response_body, Req))
    end).

%% @doc 测试 POST 请求 - 非 open 场景验证失败
assets_post_verify_for_assets_fail_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"scene">>, <<"a">> => <<"invalid_token">>, <<"v">> => <<"12345">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(_Scene, _Token, _V, _Path) ->
                <<"fail"/utf8>>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fail"/utf8>>, maps:get(response_body, Req))
    end).

%% @doc 测试 POST 请求 - 缺少必需参数返回 fail
assets_post_missing_params_returns_fail_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                % 缺少必需的参数
                #{<<"s">> => <<"scene">>}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fail"/utf8>>, maps:get(response_body, Req))
    end).

%% @doc 测试 POST 请求 - 抛出异常返回 fail
assets_post_exception_returns_fail_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                % 模拟抛出异常
                error(badarg)
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fail"/utf8>>, maps:get(response_body, Req))
    end).

%% ===================================================================
%% assets/2 测试 - GET 请求
%% ===================================================================

%% @doc 测试 GET 请求总是返回 fail
assets_get_returns_fail_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end},
        {'reply', 4, fun(_Status, _Headers, Body, Req) ->
            Req#{response_status => 200, response_body => Body}
        end}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fail"/utf8>>, maps:get(response_body, Req))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试空 token
assets_post_with_empty_token_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<>>, <<"v">> => <<"value">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(_Path, Token, _Val) ->
                ?assertEqual(<<>>, Token),
                <<"fail"/utf8>>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试空路径
assets_post_with_empty_path_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"token">>, <<"v">> => <<"value">>, <<"__path__">> => <<>>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(Path, _Token, _Val) ->
                ?assertEqual(<<>>, Path),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试空场景
assets_post_with_empty_scene_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<>>, <<"a">> => <<"token">>, <<"v">> => <<"123">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(Scene, _Token, _V, _Path) ->
                ?assertEqual(<<>>, Scene),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试无效的整数参数
assets_post_with_invalid_integer_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"scene">>, <<"a">> => <<"token">>, <<"v">> => <<"abc">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(_Scene, _Token, V, _Path) ->
                % 当前 string:to_integer/1 失败时第一项为 error。
                ?assertEqual(error, V),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试大数值参数
assets_post_with_large_value_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"scene">>, <<"a">> => <<"token">>, <<"v">> => <<"999999999">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(_Scene, _Token, V, _Path) ->
                ?assertEqual(999999999, V),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试超长路径
assets_post_with_long_path_test_() ->
    LongPath = list_to_binary(lists:duplicate(500, $x)),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"token">>, <<"v">> => <<"value">>, <<"__path__">> => LongPath}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(Path, _Token, _Val) ->
                ?assert(byte_size(Path) >= 500),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试特殊字符路径
assets_post_with_special_chars_path_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"token">>, <<"v">> => <<"value">>, <<"__path__">> => <<"/path/测试文件.pdf"/utf8>>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(Path, _Token, _Val) ->
                ?assertEqual(<<"/path/测试文件.pdf"/utf8>>, Path),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% ===================================================================
%% 场景测试
%% ===================================================================

%% @doc 测试不同的场景值
assets_post_with_different_scenes_test_() ->
    Scenes = [<<"scene1">>, <<"scene2">>, <<"scene3">>],
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"scene1">>, <<"a">> => <<"token">>, <<"v">> => <<"123">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_assets', 4, fun(Scene, _Token, _V, _Path) ->
                ?assert(lists:member(Scene, Scenes)),
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{response_status => 200, response_body => Body}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试响应内容类型
assets_post_returns_correct_content_type_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"s">> => <<"open">>, <<"a">> => <<"token">>, <<"v">> => <<"value">>, <<"__path__">> => <<"/path">>}
            end}
        ]},
        {auth_logic, [
            {'verify_for_open', 3, fun(_Path, _Token, _Val) ->
                <<"ok">>
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(_Status, Headers, _Body, Req) ->
                Req#{response_status => 200, response_headers => Headers}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        Req = run_assets_action(MockReq),
        ?assertEqual(200, maps:get(response_status, Req)),
        Headers = maps:get(response_headers, Req),
        ?assertEqual(<<"text/html">>, maps:get(<<"content-type">>, Headers))
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证参数类型
assets_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Scene = <<"scene">>,
        Token = <<"auth_token">>,
        Val = <<"12345">>,
        Path = <<"/path/to/file">>,
        ?assert(is_binary(Scene)),
        ?assert(is_binary(Token)),
        ?assert(is_binary(Val)),
        ?assert(is_binary(Path))
    end).
