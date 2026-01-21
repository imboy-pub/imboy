-module(group_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_log_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组日志数据访问层功能
%%% 覆盖：日志添加、批量添加
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.group_log">> end}
    ], fun() ->
        Result = group_log_repo:tablename(),
        ?assertEqual(<<"public.group_log">>, Result)
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_log_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Conn = mock_conn,
        Data = #{
            <<"type">> => <<"create">>,
            <<"option_uid">> => 12345,
            <<"group_id">> => 67890,
            <<"body">> => <<"创建群组"/utf8>>,
            <<"created_at">> => <<"2021-12-31 16:00:00">>
        },

        Result = group_log_repo:add(Conn, Data),
        ?assertEqual({ok, 1}, Result)
    end).

add_log_different_types_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Conn = mock_conn,
        Data = #{
            <<"type">> => <<"member_join">>,
            <<"option_uid">> => 12345,
            <<"group_id">> => 67890,
            <<"body">> => <<"成员加入群组"/utf8>>,
            <<"created_at">> => <<"2021-12-31 16:00:00">>
        },

        Result = group_log_repo:add(Conn, Data),
        ?assertEqual({ok, 1}, Result)
    end).

add_log_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Conn, _Sql, _Params) -> {error, database_error} end}
    ], fun() ->
        Conn = mock_conn,
        Data = #{
            <<"type">> => <<"create">>,
            <<"option_uid">> => 12345,
            <<"group_id">> => 67890,
            <<"body">> => <<"创建群组"/utf8>>,
            <<"created_at">> => <<"2021-12-31 16:00:00">>
        },

        Result = group_log_repo:add(Conn, Data),
        ?assertEqual({error, database_error}, Result)
    end).

%% ===================================================================
%% batch_add/2 测试
%% ===================================================================

batch_add_empty_list_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Conn, _Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Conn = mock_conn,
        DataList = [],

        Result = group_log_repo:batch_add(Conn, DataList),
        ?assertEqual({ok, 0}, Result)
    end).

batch_add_single_log_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Conn = mock_conn,
        DataList = [
            #{
                <<"type">> => <<"create">>,
                <<"option_uid">> => 12345,
                <<"group_id">> => 67890,
                <<"body">> => <<"创建群组"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:00:00">>
            }
        ],

        Result = group_log_repo:batch_add(Conn, DataList),
        ?assertEqual({ok, 1}, Result)
    end).

batch_add_multiple_logs_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Conn, _Sql, _Params) -> {ok, 3} end}
    ], fun() ->
        Conn = mock_conn,
        DataList = [
            #{
                <<"type">> => <<"create">>,
                <<"option_uid">> => 12345,
                <<"group_id">> => 67890,
                <<"body">> => <<"创建群组"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:00:00">>
            },
            #{
                <<"type">> => <<"member_join">>,
                <<"option_uid">> => 12346,
                <<"group_id">> => 67890,
                <<"body">> => <<"成员加入"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:01:00">>
            },
            #{
                <<"type">> => <<"member_leave">>,
                <<"option_uid">> => 12347,
                <<"group_id">> => 67890,
                <<"body">> => <<"成员离开"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:02:00">>
            }
        ],

        Result = group_log_repo:batch_add(Conn, DataList),
        ?assertEqual({ok, 3}, Result)
    end).

batch_add_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Conn, _Sql, _Params) -> {error, batch_insert_failed} end}
    ], fun() ->
        Conn = mock_conn,
        DataList = [
            #{
                <<"type">> => <<"create">>,
                <<"option_uid">> => 12345,
                <<"group_id">> => 67890,
                <<"body">> => <<"创建群组"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:00:00">>
            }
        ],

        Result = group_log_repo:batch_add(Conn, DataList),
        ?assertEqual({error, batch_insert_failed}, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

add_and_batch_add_logs_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end},
        {'execute', 2, fun(_Conn, _Sql, _Params) -> {ok, 2} end}
    ], fun() ->
        Conn = mock_conn,

        % 1. 单条添加
        SingleData = #{
            <<"type">> => <<"create">>,
            <<"option_uid">> => 12345,
            <<"group_id">> => 67890,
            <<"body">> => <<"创建群组"/utf8>>,
            <<"created_at">> => <<"2021-12-31 16:00:00">>
        },
        ?assertEqual({ok, 1}, group_log_repo:add(Conn, SingleData)),

        % 2. 批量添加
        BatchData = [
            #{
                <<"type">> => <<"member_join">>,
                <<"option_uid">> => 12346,
                <<"group_id">> => 67890,
                <<"body">> => <<"成员加入"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:01:00">>
            },
            #{
                <<"type">> => <<"member_leave">>,
                <<"option_uid">> => 12347,
                <<"group_id">> => 67890,
                <<"body">> => <<"成员离开"/utf8>>,
                <<"created_at">> => <<"2021-12-31 16:02:00">>
            }
        ],
        ?assertEqual({ok, 2}, group_log_repo:batch_add(Conn, BatchData))
    end).
