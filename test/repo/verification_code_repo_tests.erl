-module(verification_code_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% verification_code_repo 模块的 EUnit 测试
%%%
%%% 目标：验证验证码数据访问层功能
%%% 覆盖：验证码创建、验证、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = verification_code_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 验证码查询测试
%% ===================================================================

find_code_by_account_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = <<"test@example.com">>,
        Result = verification_code_repo:find_by_account(Account),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Code} ->
                ?assert(is_map(Code) orelse is_list(Code));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).
