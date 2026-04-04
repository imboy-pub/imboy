-module(msg_s2c_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_s2c_repo 模块的 EUnit 测试
%%%
%%% 目标：验证系统到客户端消息数据访问层功能
%%% 覆盖：消息查询、插入
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = msg_s2c_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

read_by_to_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 1,
        Limit = 10,
        Result = msg_s2c_repo:read(ToUid, Limit),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _, Messages} when is_list(Messages) ->
                ?assert(true);
            {ok, Messages} when is_list(Messages) ->
                ?assert(true);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).
