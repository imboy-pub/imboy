-module(msg_read_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_read_repo 模块的 EUnit 测试
%%%
%%% 目标：验证消息已读回执数据仓库功能
%%% 覆盖：保存已读记录、获取已读状态、获取未读消息数
%%%===================================================================

-define(MOCK_ENV,
    {config_ds, [
        {'env', 1, fun
            (sql_driver) -> pgsql;
            (_) -> undefined
        end}
    ]}
).
-define(MOCK_TSID, {elib_tsid, [{'generate', 1, fun(_Table) -> 123456789 end}]}).

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_name_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        TableName = msg_read_repo:tablename(),
        ?assertEqual(<<"public.msg_read">>, TableName)
    end).

%% ===================================================================
%% save_read/5 测试
%% ===================================================================

save_read_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_test_123">>,
            FromUid = 123,
            ToUid = 456,
            ToDid = <<"device_test_abc">>,
            ReadAt = <<"2024-01-01T00:00:00Z">>,
            Result = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
            ?assertEqual(ok, Result)
        end
    ).

%% 【MSG-P2-6】三列去重：SQL 须含 WHERE NOT EXISTS（时间戳不同的重复上报不落新行）
save_read_dedups_by_three_columns_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertMatch({_, _}, binary:match(SqlBin, <<"WHERE NOT EXISTS">>)),
                    ?assertMatch(
                        {_, _},
                        binary:match(SqlBin, <<"msg_id = $1 AND to_uid = $3 AND to_did = $4">>)
                    ),
                    ?assertEqual(5, length(Params)),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_read_repo:save_read(
                    <<"m1">>, 123, 456, <<"did-a">>, <<"2024-01-01T00:00:00Z">>
                )
            )
        end
    ).

%% ===================================================================
%% 幂等性测试
%% ===================================================================

save_read_is_idempotent_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_test_idempotent">>,
            FromUid = 123,
            ToUid = 456,
            ToDid = <<"device_test_idempotent">>,
            ReadAt = <<"2024-01-01T00:00:00Z">>,

            Result1 = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
            ?assertEqual(ok, Result1),

            Result2 = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
            ?assertEqual(ok, Result2)
        end
    ).

%% ===================================================================
%% get_read_status/2 测试
%% ===================================================================

get_read_status_with_existing_record_returns_status_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [
                        #{
                            <<"to_uid">> => 456,
                            <<"to_did">> => <<"device1">>,
                            <<"read_at">> => <<"2024-01-01T00:00:00Z">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_test_get_status">>,
            FromUid = 123,
            Result = msg_read_repo:get_read_status(MsgId, FromUid),
            ?assertMatch({ok, [_]}, Result)
        end
    ).

get_read_status_with_no_records_returns_empty_list_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_test_nonexistent">>,
            FromUid = 999,
            Result = msg_read_repo:get_read_status(MsgId, FromUid),
            ?assertMatch({ok, []}, Result)
        end
    ).

%% ===================================================================
%% delete_read_records/2 测试
%% ===================================================================

delete_read_records_deletes_records_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 2} end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_test_delete">>,
            ToUid = 456,
            Result = msg_read_repo:delete_read_records(MsgId, ToUid),
            ?assertMatch({ok, 2}, Result)
        end
    ).
