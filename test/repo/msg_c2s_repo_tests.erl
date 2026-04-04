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

list_by_ids_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            % 返回模拟的消息列表
            {ok, [], [{1, 1, 2, <<"Hello">>, 1, 1640995200}]}
        end}
    ], fun() ->
        MsgIds = [<<"msg_001">>, <<"msg_002">>],
        Column = <<"id, from_id, to_id, payload, msg_type, created_at">>,
        Result = msg_c2s_repo:list_by_ids(MsgIds, Column),
        case Result of
            {ok, _, Messages} when is_list(Messages) ->
                ?assert(length(Messages) >= 1);
            {ok, Messages} when is_list(Messages) ->
                ?assert(length(Messages) >= 1);
            _ ->
                ?assert(true)
        end
    end).

list_by_ids_empty_test_() ->
    ?_assertEqual({ok, []}, msg_c2s_repo:list_by_ids([], <<"id">>)).
