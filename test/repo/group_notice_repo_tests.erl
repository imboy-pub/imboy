-module(group_notice_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组公告数据访问层功能
%%% 覆盖：表名获取、demo 查询
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.group_notice">> end}
    ], fun() ->
        Result = group_notice_repo:tablename(),
        ?assertEqual(<<"public.group_notice">>, Result)
    end).

%% ===================================================================
%% demo/3 测试
%% ===================================================================

demo_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>}, {<<"id2">>}]}
        end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<"value1">>,
        Val2 = <<"value2">>,

        Result = group_notice_repo:demo(Uid, Val1, Val2),
        ?assertMatch({ok, _}, Result)
    end).

demo_empty_result_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Uid = 99999,
        Val1 = <<"value1">>,
        Val2 = <<"value2">>,

        Result = group_notice_repo:demo(Uid, Val1, Val2),
        ?assertEqual({ok, []}, Result)
    end).

demo_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, connection_failed}
        end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<"value1">>,
        Val2 = <<"value2">>,

        Result = group_notice_repo:demo(Uid, Val1, Val2),
        ?assertEqual({error, connection_failed}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% group_notice_repo 目前只导出了 tablename/0 和 demo/3 函数
%% demo 函数仅作为示例，实际使用时应根据具体需求修改
%%
%% 当添加新功能时，请在此添加对应的测试：
%% - add/2 - 添加公告
%% - update/2 - 更新公告
%% - delete/1 - 删除公告
%% - find_by_group_id/1 - 查询群组公告
%%===================================================================
