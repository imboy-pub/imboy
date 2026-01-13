-module(msg_store_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_store_repo 模块的 EUnit 测试
%%%
%%% 目标：验证消息存储备份表仓库操作
%%% 覆盖：表管理、写入操作、删除操作、查询操作
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_name_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
            <<"public.msg_store_staging">>
        end}
    ], fun() ->
        Result = msg_store_repo:tablename(),
        ?assertEqual(<<"public.msg_store_staging">>, Result)
    end).

%% ===================================================================
%% stage/10 测试 - 单聊消息 (integer ToId)
%% ===================================================================

stage_with_integer_toid_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>,
            <<"msg123">>,
            <<"text">>,
            <<"send">>,
            <<>>,
            <<"{\"body\": \"hello\"}">>,
            100,
            200,
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_empty_e2ee_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, Data) ->
                ?assertEqual(null, maps:get(e2ee, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>,
            <<"msg456">>,
            <<"text">>,
            <<"send">>,
            <<>>,
            <<"{}">>,
            100,
            200,
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_e2ee_data_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, Data) ->
                ?assertEqual(<<"e2ee_data">>, maps:get(e2ee, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>,
            <<"msg789">>,
            <<"text">>,
            <<"send">>,
            <<"e2ee_data">>,
            <<"{}">>,
            100,
            200,
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_unique_violation_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) ->
                {error, {error, [[{23505, unique_constraint}]]}}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>,
            <<"msg_duplicate">>,
            <<"text">>,
            <<"send">>,
            <<>>,
            <<"{}">>,
            100,
            200,
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({error, {unique_violation, <<"msg_duplicate">>}}, Result)
    end).

%% ===================================================================
%% stage/10 测试 - 群聊消息 (list ToIdList)
%% ===================================================================

stage_with_list_toidlist_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, Data) ->
                ?assertEqual([100, 200, 300], maps:get(to_id_list, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2g">>,
            <<"msg_group">>,
            <<"text">>,
            <<"send">>,
            <<>>,
            <<"{}">>,
            50,
            [100, 200, 300],
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_list_unique_violation_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) ->
                {error, {error, [[{23505, unique_constraint}]]}}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2g">>,
            <<"msg_group_dup">>,
            <<"text">>,
            <<"send">>,
            <<>>,
            <<"{}">>,
            50,
            [100, 200],
            <<"2024-01-01T00:00:00Z">>,
            <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({error, {unique_violation, <<"msg_group_dup">>}}, Result)
    end).

%% ===================================================================
%% unstage/2 测试
%% ===================================================================

unstage_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:unstage(<<"c2c">>, <<"msg123">>),
        ?assertEqual({ok, 1}, Result)
    end).

unstage_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:unstage(<<"c2c">>, <<"nonexistent">>),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% claim_pending/2 测试
%% ===================================================================

claim_pending_with_messages_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(_TxFun) ->
                {ok, [[
                    #{<<"id">> => 1, <<"msg_id">> => <<"msg1">>, <<"payload">> => <<"{}">>},
                    #{<<"id">> => 2, <<"msg_id">> => <<"msg2">>, <<"payload">> => <<"{}">>}
                ]]}
            end},
            {'query', 3, fun(_Conn, _Sql, [10]) ->
                {ok, [[
                    #{<<"id">> => 1, <<"msg_id">> => <<"msg1">>},
                    #{<<"id">> => 2, <<"msg_id">> => <<"msg2">>}
                ]]}
            end},
            {'execute', 3, fun(_Conn, _Sql, [_Lease, [1, 2]]) -> {ok, 2} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:claim_pending(10, 60),
        ?assertMatch({ok, [_, _]}, Result)
    end).

claim_pending_empty_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(_TxFun) -> {ok, []} end},
            {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:claim_pending(10, 60),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% mark_processed/2 测试
%% ===================================================================

mark_processed_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:mark_processed(<<"c2c">>, <<"msg123">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% mark_failed/4 测试
%% ===================================================================

mark_failed_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, [_Type, _MsgId, _Error, _Delay]) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:mark_failed(
            <<"c2c">>,
            <<"msg123">>,
            <<"connection failed"/utf8>>,
            60
        ),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% get_unstaged/1 测试
%% ===================================================================

get_unstaged_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [_Limit]) ->
                {ok, [[
                    #{<<"msg_id">> => <<"msg1">>, <<"from_id">> => 100},
                    #{<<"msg_id">> => <<"msg2">>, <<"from_id">> => 200}
                ]]}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_unstaged(100),
        ?assertMatch({ok, [_, _]}, Result)
    end).

get_unstaged_empty_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [_Limit]) -> {ok, []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_unstaged(100),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% delete_processed/1 测试
%% ===================================================================

delete_processed_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, [_Seconds]) -> {ok, 100} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:delete_processed(3600),
        ?assertEqual({ok, 100}, Result)
    end).

%% ===================================================================
%% get_staging_stats/0 测试
%% ===================================================================

get_staging_stats_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) ->
                {ok, [[
                    #{<<"pending">> => 10, <<"processed">> => 100, <<"failed">> => 2, <<"total">> => 112}
                ]]}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_staging_stats(),
        ?assertMatch({ok, #{<<"pending">> := 10, <<"processed">> := 100}}, Result)
    end).

%% ===================================================================
%% truncate_processed/0 测试
%% ===================================================================

truncate_processed_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) -> {ok, [], []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:truncate_processed(),
        ?assertEqual({ok, [], []}, Result)
    end).

%% ===================================================================
%% vacuum_table/0 测试
%% ===================================================================

vacuum_table_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) -> {ok, [], []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:vacuum_table(),
        ?assertEqual({ok, [], []}, Result)
    end).

%% ===================================================================
%% ensure_table_exists/0 测试
%% ===================================================================

ensure_table_exists_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, []) -> {ok, [], []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:ensure_table_exists(),
        ?assertEqual(ok, Result)
    end).

ensure_table_exists_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, []) -> {error, table_creation_error} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:ensure_table_exists(),
        ?assertEqual({error, table_creation_error}, Result)
    end).

%% ===================================================================
%% create_indexes/1 测试
%% ===================================================================

create_indexes_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, []) -> {ok, [], []} end}
    ], fun() ->
        Result = msg_store_repo:create_indexes(<<"msg_store_staging">>),
        ?assertEqual(ok, Result)
    end).
