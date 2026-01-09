-module(user_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_log_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户日志数据访问层功能
%%% 覆盖：日志查询、创建
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = user_log_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 日志查询测试
%% ===================================================================

find_logs_by_uid_test_() ->
    ?TEST_WITH_APP(fun() ->
        Uid = 1,
        Limit = 20,
        
        Result = user_log_repo:find_logs_by_uid(Uid, Limit),
        % 验证函数调用不会崩溃
        ?assert(is_tuple(Result))
    end).

%% ===================================================================
%% 日志创建测试
%% ===================================================================

create_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        Uid = 1,
        Action = <<"login">>,
        Detail = <<"User logged in from iOS">>,
        
        Result = user_log_repo:create_log(Uid, Action, Detail),
        % 验证函数调用不会崩溃
        ?assert(is_tuple(Result))
    end).
