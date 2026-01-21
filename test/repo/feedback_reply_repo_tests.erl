-module(feedback_reply_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_reply_repo 模块的 EUnit 测试
%%%
%%% 目标：验证反馈回复数据访问层功能
%%% 覆盖：表名获取
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.feedback_reply">> end}
    ], fun() ->
        Result = feedback_reply_repo:tablename(),
        ?assertEqual(<<"public.feedback_reply">>, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% feedback_reply_repo 目前只导出了 tablename/0 函数
%% 其他功能函数尚未实现，因此只需测试 tablename
%%
%% 当添加新功能时，请在此添加对应的测试：
%% - add/4 或 save/4 - 添加回复
%% - find_by_feedback_id/1 - 查询反馈的所有回复
%% - delete/1 - 删除回复
%%===================================================================
