-module(adm_app_ddl_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_app_ddl_handler 模块的 EUnit 测试
%%%
%%% 目标：验证应用 DDL 管理后台 API 功能
%%% 覆盖：索引查询、DDL 保存、DDL 删除
%%%===================================================================

%% 创建 Cowboy 2.x 模拟请求对象
%% Cowboy 2.x 使用 Map 作为请求对象，而不是 cowboy_req:new()
mock_request() ->
    #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        path => <<"/adm/app_ddl">>,
        qs => <<>>,
        headers => #{},
        peer => {{127,0,0,1}, 12345},
        body_length => 0
    }.

%% ===================================================================
%% init/2 测试
%% ===================================================================

init_with_index_action_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {elib_param, [
            {'int', 3, fun(_ajax, _Req, _Default) -> {ok, 1} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{action => index},
        {ok, NewReq, NewState} = adm_app_ddl_handler:init(Req, State),
        ?assert(is_map(NewState)),
        ?assertNot(maps:is_key(action, NewState))
    end).

init_with_save_action_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"POST">> end}
    ], fun() ->
        Req = mock_request(),
        State = #{action => save},
        {ok, NewReq, NewState} = adm_app_ddl_handler:init(Req, State),
        ?assert(is_map(NewState))
    end).

init_with_delete_action_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"DELETE">> end}
    ], fun() ->
        Req = mock_request(),
        State = #{action => delete},
        {ok, NewReq, NewState} = adm_app_ddl_handler:init(Req, State),
        ?assert(is_map(NewState))
    end).

init_with_false_action_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        State = #{action => false},
        {ok, NewReq, NewState} = adm_app_ddl_handler:init(Req, State),
        ?assert(is_map(NewState))
    end).

%% ===================================================================
%% index/4 测试 - Ajax 查询
%% ===================================================================

index_with_ajax_1_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {app_ddl_repo, [
            {'tablename', 0, fun() -> <<"app_ddl">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Size) ->
                {ok, #{total => 100, rows => []}}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_app_ddl_handler:index(<<"GET">>, 1, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

index_with_ajax_other_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {app_ddl_repo, [
            {'tablename', 0, fun() -> <<"app_ddl">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Size) ->
                {ok, #{total => 50, rows => [#{<<"id">> => 1}]}}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_app_ddl_handler:index(<<"GET">>, 0, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

%% ===================================================================
%% index/4 测试 - HTML 页面
%% ===================================================================

index_html_page_test_() ->
    ?WITH_MECKS([
        {imboy_dtl, [
            {'template', 3, fun(_Tpl, _Data, _App) ->
                {ok, <<"<html>DDL Index</html>">>}
            end},
            {'imadm_param', 1, fun(_State) -> [] end}
        ]},
        {cowboy_req, [
            {'reply', 4, fun(_Code, _Headers, _Body, _Req) ->
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_app_ddl_handler:index(<<"GET">>, 0, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

%% ===================================================================
%% save/3 测试
%% ===================================================================

save_with_valid_data_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"new_vsn">> => <<"1.0.0">>,
                    <<"old_vsn">> => <<"0.9.0">>,
                    <<"status">> => 1,
                    <<"ddl">> => <<"CREATE TABLE test;">>,
                    <<"down_ddl">> => <<"DROP TABLE test;">>
                }
            end}
        ]},
        {app_ddl_ds, [
            {'save', 6, fun(_AdmUserId, _NewVsn, _OldVsn, _Status, _Ddl, _DownDdl) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{adm_user_id => 100},
        Result = adm_app_ddl_handler:save(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

save_with_partial_data_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"ddl">> => <<"CREATE TABLE x;">>}
            end}
        ]},
        {app_ddl_ds, [
            {'save', 6, fun(_AdmUserId, _NewVsn, _OldVsn, _Status, _Ddl, _DownDdl) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{adm_user_id => 100},
        Result = adm_app_ddl_handler:save(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

save_with_non_post_method_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'new', 0, fun() -> #{method => <<"GET">>} end}
    ], fun() ->
        Req = mock_request(),
        State = #{adm_user_id => 100},
        Result = adm_app_ddl_handler:save(<<"GET">>, Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_with_valid_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => <<"test_id">>}
            end}
        ]},
        {app_ddl_ds, [
            {'delete', 1, fun(_Id) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = adm_app_ddl_handler:delete(<<"DELETE">>, Req),
        ?assertMatch(#{response_status := 200}, Result)
    end).

delete_with_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => <<"invalid_id">>}
            end}
        ]},
        {app_ddl_ds, [
            {'delete', 1, fun(_Id) -> {error, not_found} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{response_status => 500} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = adm_app_ddl_handler:delete(<<"DELETE">>, Req),
        ?assertMatch(#{response_status := 500}, Result)
    end).

delete_with_non_delete_method_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'new', 0, fun() -> #{method => <<"GET">>} end}
    ], fun() ->
        Req = mock_request(),
        Result = adm_app_ddl_handler:delete(<<"GET">>, Req),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

index_with_large_page_size_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 1000} end}
        ]},
        {app_ddl_repo, [
            {'tablename', 0, fun() -> <<"app_ddl">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Size) ->
                {ok, #{total => 5000, rows => []}}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_app_ddl_handler:index(<<"GET">>, 1, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

save_with_empty_ddl_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"ddl">> => <<>>, <<"down_ddl">> => <<>>}
            end}
        ]},
        {app_ddl_ds, [
            {'save', 6, fun(_AdmUserId, _NewVsn, _OldVsn, _Status, _Ddl, _DownDdl) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{adm_user_id => 100},
        Result = adm_app_ddl_handler:save(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

delete_with_empty_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => <<>>}
            end}
        ]},
        {app_ddl_ds, [
            {'delete', 1, fun(_Id) -> {ok, 0} end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = adm_app_ddl_handler:delete(<<"DELETE">>, Req),
        ?assertMatch(#{response_status := 200}, Result)
    end).
