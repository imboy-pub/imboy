-module(msg_c2s_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2s_repo 模块的 EUnit 测试
%%%
%%% 目标：验证客户端到服务器消息数据访问层功能
%%% 覆盖：消息查询、插入
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.msg_c2s">> end}
    ], fun() ->
        Result = msg_c2s_repo:tablename(),
        ?assertEqual(<<"public.msg_c2s">>, Result)
    end).

find_messages_by_uid_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含消息查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*msg_c2s">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*from_uid">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"LIMIT">>) =/= nomatch),
            % 验证参数包含用户ID和限制
            ?assert(length(Params) >= 2),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(10, Params)),
            % 返回模拟的消息列表
            {ok, [{1, 1, 2, <<"Hello">>, 1, 1640995200}]}
        end}
    ], fun() ->
        Uid = 1,
        Limit = 10,
        Result = msg_c2s_repo:find_by_uid(Uid, Limit),
        ?ASSERT_OK(Result),
        {ok, Messages} = Result,
        % 验证返回的消息列表
        ?assert(length(Messages) >= 1),
        % 验证第一个消息
        [Message | _] = Messages,
        ?assertEqual(1, element(1, Message)),
        ?assertEqual(1, element(2, Message)),
        ?assertEqual(2, element(3, Message)),
        ?assertEqual(<<"Hello">>, element(4, Message))
    end).
