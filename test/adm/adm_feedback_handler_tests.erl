-module(adm_feedback_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_feedback_handler 模块的 EUnit 测试
%%%
%%% 目标：验证管理员反馈处理器功能
%%% 覆盖：反馈列表查询、反馈回复、错误处理、边界条件
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

%% @doc 验证模块可以正常加载
module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(adm_feedback_handler),
        ?assertMatch({file, _}, code:is_loaded(adm_feedback_handler))
    end).

%% ===================================================================
%% init/2 测试
%% ===================================================================

%% @doc 测试 init/2 函数分发到 index action
init_dispatches_to_index_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, Default) ->
                {ok, Default}
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, _Page, _Size) ->
                {ok, #{data => [], total => 0}}
            end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Data) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        State = #{action => index},
        {ok, Req, _State} = adm_feedback_handler:init(MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req, undefined))
    end).

%% @doc 测试 init/2 函数分发到 reply action
init_dispatches_to_reply_action_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"123">>, <<"body">> => <<"回复内容"/utf8>>}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(_Data) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _Data, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{}),
        State = #{action => reply, adm_user_id => 1},
        {ok, Req, _State} = adm_feedback_handler:init(MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req, undefined))
    end).

%% @doc 测试 init/2 函数处理 false action
init_with_false_action_returns_unchanged_request_test_() ->
    ?TEST_SIMPLE(fun() ->
        MockReq = cowboy_req_h:new(#{}),
        State = #{action => false},
        {ok, Req, _State} = adm_feedback_handler:init(MockReq, State),
        ?assertMatch(#{}, Req)
    end).

%% ===================================================================
%% index/4 测试 - GET HTML 页面
%% ===================================================================

%% @doc 测试 GET 请求返回 HTML 页面（ajax != 1）
index_get_returns_html_page_test_() ->
    ?WITH_MECKS([
        {imboy_dtl, [
            {'template', 3, fun(_Template, Data, _App) ->
                {ok, iolist_to_binary([
                    "<html><body>",
                    proplists:get_value(attach_token, Data),
                    "</body></html>"
                ])}
            end},
            {'imadm_param', 1, fun(_State) ->
                [{system_name, <<"Imboy 管理后台"/utf8>>}]
            end}
        ]},
        {cowboy_req, [
            {'reply', 4, fun(_Status, _Headers, Body, Req) ->
                Req#{
                    response_status => 200,
                    response_body => Body
                }
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 0,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req)),
        Body = maps:get(response_body, Req),
        ?assertNotEqual(nomatch, binary:match(Body, <<"<html>">>))
    end).

%% ===================================================================
%% index/4 测试 - GET JSON 数据
%% ===================================================================

%% @doc 测试 GET 请求返回 JSON 数据（ajax = 1）
index_get_ajax_returns_json_data_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, _Page, _Size) ->
                {ok, #{
                    data => [
                        #{
                            <<"feedback_id">> => 1,
                            <<"user_id">> => 100,
                            <<"type">> => <<"bug"/utf8>>,
                            <<"rating">> => 5,
                            <<"body">> => <<"发现一个bug"/utf8>>,
                            <<"status">> => 1
                        }
                    ],
                    total => 1
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Data) ->
                Req#{
                    response_status => 200,
                    response_data => Data
                }
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 1,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req)),
        Data = maps:get(response_data, Req),
        ?assertEqual(1, maps:get(total, Data))
    end).

%% @doc 测试分页参数
index_get_ajax_with_pagination_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {2, 20} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, Page, Size) ->
                ?assertEqual(2, Page),
                ?assertEqual(20, Size),
                {ok, #{data => [], total => 0}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Data) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 1,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试空结果
index_get_ajax_with_empty_result_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, _Page, _Size) ->
                {ok, #{data => [], total => 0}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Data) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 1,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% ===================================================================
%% reply/3 测试
%% ===================================================================

%% @doc 测试回复反馈成功
reply_post_success_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"id">> => 1, <<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"feedback_id">> => <<"123">>,
                    <<"body">> => <<"感谢您的反馈"/utf8>>,
                    <<"feedback_reply_pid">> => <<"0">>
                }
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                ?assertEqual(123, maps:get(<<"feedback_id">>, Data)),
                ?assertEqual(<<"感谢您的反馈"/utf8>>, maps:get(<<"body">>, Data)),
                ?assertEqual(<<"管理员"/utf8>>, maps:get(<<"replier_name">>, Data)),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试回复带回复ID
reply_post_with_reply_pid_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"feedback_id">> => <<"123">>,
                    <<"body">> => <<"回复"/utf8>>,
                    <<"feedback_reply_pid">> => <<"456">>
                }
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                ?assertEqual(456, maps:get(<<"feedback_reply_pid">>, Data)),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试回复带空body
reply_post_with_empty_body_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"123">>, <<"body">> => <<>>}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                ?assertEqual(<<>>, maps:get(<<"body">>, Data)),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% ===================================================================
%% reply/3 错误处理测试
%% ===================================================================

%% @doc 测试无效的 feedback_id（0）
reply_post_with_zero_feedback_id_returns_error_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"0">>, <<"body">> => <<"回复"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 1, fun(Req) ->
                Req#{response_status => 400}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(400, maps:get(response_status, Req))
    end).

%% @doc 测试无效的 feedback_id（负数）
reply_post_with_negative_feedback_id_returns_error_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"-1">>, <<"body">> => <<"回复"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 1, fun(Req) ->
                Req#{response_status => 400}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(400, maps:get(response_status, Req))
    end).

%% @doc 测试缺少 feedback_id
reply_post_without_feedback_id_returns_error_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"body">> => <<"回复"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 1, fun(Req) ->
                Req#{response_status => 400}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(400, maps:get(response_status, Req))
    end).

%% @doc 测试 feedback_id 解析错误
reply_post_with_invalid_feedback_id_format_returns_error_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"abc">>, <<"body">> => <<"回复"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 1, fun(Req) ->
                Req#{response_status => 400}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(400, maps:get(response_status, Req))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试超长的回复内容
reply_post_with_very_long_body_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                LongBody = list_to_binary(lists:duplicate(5000, $x)),
                #{<<"feedback_id">> => <<"123">>, <<"body">> => LongBody}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                Body = maps:get(<<"body">>, Data),
                ?assert(byte_size(Body) >= 5000),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试带UTF-8的回复内容
reply_post_with_utf8_body_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"123">>, <<"body">> => <<"感谢您的反馈，我们会尽快处理"/utf8>>}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                Body = maps:get(<<"body">>, Data),
                ?assertEqual(<<"感谢您的反馈，我们会尽快处理"/utf8>>, Body),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试带特殊字符的回复内容
reply_post_with_special_characters_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"123">>, <<"body">> => <<"测试\n换行\t制表符\r回车">>}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(_Data) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试大数字 feedback_id
reply_post_with_large_feedback_id_test_() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(_AdmUserId, _Column, _Key) ->
                #{<<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"999999999">>, <<"body">> => <<"回复"/utf8>>}
            end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                ?assertEqual(999999999, maps:get(<<"feedback_id">>, Data)),
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _PostVals, _Msg) ->
                Req#{response_status => 200}
            end}
        ]},
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]}
    ], fun() ->
        Method = <<"POST">>,
        MockReq = cowboy_req_h:new(#{}),
        State = #{adm_user_id => 1},
        Req = adm_feedback_handler:reply(Method, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试分页边界条件
index_get_ajax_with_page_zero_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {0, 10} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, Page, _Size) ->
                ?assertEqual(0, Page),
                {ok, #{data => [], total => 0}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Data) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 1,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

%% @doc 测试最大分页大小
index_get_ajax_with_max_page_size_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10000} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, _Page, Size) ->
                ?assertEqual(10000, Size),
                {ok, #{data => [], total => 0}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Data) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Method = <<"GET">>,
        Ajax = 1,
        MockReq = cowboy_req_h:new(#{}),
        State = #{},
        Req = adm_feedback_handler:index(Method, Ajax, MockReq, State),
        ?assertEqual(200, maps:get(response_status, Req))
    end).
