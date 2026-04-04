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

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        TableName = msg_read_repo:tablename(),
        ?assertEqual(<<"public.msg_read">>, TableName)
    end).


%% ===================================================================
%% save_read/5 测试
%% ===================================================================

save_read_with_valid_data_succeeds_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        MsgId = <<"msg_test_123">>,
        FromUid = 123,
        ToUid = 456,
        ToDid = <<"device_test_abc">>,
        ReadAt = elib_dt:now(),

        % 清理可能存在的测试数据
        elib_pg:execute(<<"DELETE FROM msg_read WHERE msg_id = $1">>, [MsgId], Conn),

        % 插入已读记录
        Result = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
        ?assertEqual(ok, Result),

        % 验证数据已插入
        {ok, Rows} = elib_pg:query(
            <<"SELECT msg_id, from_uid, to_uid, to_did FROM msg_read WHERE msg_id = $1">>,
            [MsgId], Conn),
        ?assertEqual(1, length(Rows)),
        #{<<"msg_id">> := MsgId, <<"from_uid">> := FromUid, <<"to_uid">> := ToUid} = lists:nth(1, Rows),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_read WHERE msg_id = $1">>, [MsgId], Conn)
    end).


%% ===================================================================
%% 幂等性测试
%% ===================================================================

save_read_is_idempotent_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        MsgId = <<"msg_test_idempotent">>,
        FromUid = 123,
        ToUid = 456,
        ToDid = <<"device_test_idempotent">>,
        ReadAt = elib_dt:now(),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_read WHERE msg_id = $1">>, [MsgId], Conn),

        % 第一次插入
        Result1 = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
        ?assertEqual(ok, Result1),

        % 第二次插入（相同数据，应该幂等）
        Result2 = msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt),
        ?assertEqual(ok, Result2),

        % 验证只有一条记录
        {ok, Rows} = elib_pg:query(
            <<"SELECT COUNT(*) as count FROM msg_read WHERE msg_id = $1">>,
            [MsgId], Conn),
        ?assertEqual(1, length(Rows)),
        #{<<"count">> := 1} = lists:nth(1, Rows),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_read WHERE msg_id = $1">>, [MsgId], Conn)
    end).


%% ===================================================================
%% get_read_status/2 测试
%% ===================================================================

get_read_status_with_existing_record_returns_status_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        MsgId = <<"msg_test_get_status">>,
        FromUid = 123,
        ToUid = 456,
        ToDid = <<"device_test_get_status">>,
        ReadAt = elib_dt:now(),

        % 插入测试数据
        elib_pg:execute(
            <<"INSERT INTO msg_read (msg_id, from_uid, to_uid, to_did, read_at) VALUES ($1, $2, $3, $4, $5)">>,
            [MsgId, FromUid, ToUid, ToDid, ReadAt], Conn),

        % 获取已读状态
        Result = msg_read_repo:get_read_status(MsgId, FromUid),
        ?assertMatch({ok, [_]}, Result),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_read WHERE msg_id = $1">>, [MsgId], Conn)
    end).


get_read_status_with_no_records_returns_empty_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_test_nonexistent">>,
        FromUid = 999,

        % 查询不存在的记录
        Result = msg_read_repo:get_read_status(MsgId, FromUid),
        ?assertMatch({ok, []}, Result)
    end).


%% ===================================================================
%% delete_read_records/2 测试
%% ===================================================================

delete_read_records_deletes_records_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        MsgId = <<"msg_test_delete">>,
        FromUid = 123,
        ToUid = 456,

        % 插入测试数据
        elib_pg:execute(
            <<"INSERT INTO msg_read (msg_id, from_uid, to_uid, to_did, read_at) VALUES ($1, $2, $3, $4, $5)">>,
            [MsgId, FromUid, ToUid, <<"device1">>, elib_dt:now()], Conn),
        elib_pg:execute(
            <<"INSERT INTO msg_read (msg_id, from_uid, to_uid, to_did, read_at) VALUES ($1, $2, $3, $4, $5)">>,
            [MsgId, FromUid, ToUid, <<"device2">>, elib_dt:now()], Conn),

        % 删除记录
        Result = msg_read_repo:delete_read_records(MsgId, ToUid),
        ?assertMatch({ok, 2}, Result),

        % 验证记录已删除
        {ok, Rows} = elib_pg:query(
            <<"SELECT COUNT(*) as count FROM msg_read WHERE msg_id = $1">>,
            [MsgId], Conn),
        ?assertEqual(1, length(Rows)),
        #{<<"count">> := 0} = lists:nth(1, Rows)
    end).
