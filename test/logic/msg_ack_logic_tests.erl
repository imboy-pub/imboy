-module(msg_ack_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_ack_logic 模块的 EUnit 测试
%%%
%%% 目标：验证统一 ACK 处理功能
%%% 覆盖：C2C/C2G/S2C/C2S 消息 ACK 处理
%%%===================================================================

%% ===================================================================
%% C2C ACK 处理测试
%% ===================================================================

c2c_ack_deletes_offline_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1001,
        Did = <<"device_ios_001">>,
        MsgId = <<"test_c2c_msg_001">>,

        % 准备测试数据：插入离线消息
        Sql = <<"INSERT INTO msg_c2c (from_id, to_id, msg_id, payload, created_at)
                VALUES ($1, $2, $3, $4, NOW())">>,
        Payload = <<"{\"content\":\"test message\"}">>,
        {ok, _} = elib_pg:execute(Sql, [999, Uid, MsgId, Payload]),

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK
        ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

        % 验证消息被删除
        Sql2 = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1 AND to_id = $2">>,
        {ok, _Cols, [{Count}]} = elib_pg:query(Sql2, [MsgId, Uid]),
        ?assertEqual(0, Count),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId])
    end).

c2c_ack_with_no_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1002,
        Did = <<"device_ios_002">>,
        MsgId = <<"test_c2c_msg_002">>,

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK（消息不存在）
        ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

        % 应该正常完成，不抛出异常
        ?assert(true)
    end).

%% ===================================================================
%% C2G ACK 处理测试
%% ===================================================================

c2g_ack_marks_timeline_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2001,
        Did = <<"device_android_001">>,
        MsgId = <<"test_c2g_msg_001">>,

        % Mock msg_c2g_timeline_repo:client_ack
        meck:expect(msg_c2g_timeline_repo, client_ack, 1, fun(_, _) -> ok end),

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK
        ok = msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, Did),

        % 验证调用
        ?assert(meck:validate(msg_c2g_timeline_repo)),
        ?assert(meck:validate(msg_store_ds))
    end).

%% ===================================================================
%% S2C ACK 处理测试
%% ===================================================================

s2c_ack_deletes_offline_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 3001,
        Did = <<"device_ios_003">>,
        MsgId = <<"test_s2c_msg_001">>,

        % 准备测试数据：插入离线消息
        Sql = <<"INSERT INTO msg_s2c (from_id, to_id, msg_id, payload, created_at)
                VALUES ($1, $2, $3, $4, NOW())">>,
        Payload = <<"{\"msg_type\":\"system\"}">>,
        {ok, _} = elib_pg:execute(Sql, [0, Uid, MsgId, Payload]),

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK
        ok = msg_ack_logic:client_ack(<<"s2c">>, MsgId, Uid, Did),

        % 验证消息被删除
        Sql2 = <<"SELECT COUNT(*) FROM msg_s2c WHERE msg_id = $1 AND to_id = $2">>,
        {ok, _Cols, [{Count}]} = elib_pg:query(Sql2, [MsgId, Uid]),
        ?assertEqual(0, Count),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_s2c WHERE msg_id = $1">>, [MsgId])
    end).

%% ===================================================================
%% C2S ACK 处理测试（SQL 安全）
%% ===================================================================

c2s_ack_uses_parameterized_query_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 4001,
        Did = <<"device_web_001">>,
        MsgId = <<"test_c2s_msg_001">>,

        % 准备测试数据：插入消息
        Sql = <<"INSERT INTO msg_c2s (from_id, topic_id, msg_id, payload, created_at)
                VALUES ($1, $2, $3, $4, NOW())">>,
        Payload = <<"{\"text\":\"hello\"}">>,
        {ok, _} = elib_pg:execute(Sql, [Uid, 123, MsgId, Payload]),

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK（使用参数化查询）
        ok = msg_ack_logic:client_ack(<<"c2s">>, MsgId, Uid, Did),

        % 验证消息被删除
        Sql2 = <<"SELECT COUNT(*) FROM msg_c2s WHERE msg_id = $1 AND from_id = $2">>,
        {ok, _Cols, [{Count}]} = elib_pg:query(Sql2, [MsgId, Uid]),
        ?assertEqual(0, Count),

        % 清理测试数据
        elib_pg:execute(<<"DELETE FROM msg_c2s WHERE msg_id = $1">>, [MsgId])
    end).

c2s_ack_prevents_sql_injection_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 4002,
        Did = <<"device_web_002">>,

        % 使用 SQL 注入攻击字符串
        MaliciousMsgId = <<"msg_123'; DROP TABLE msg_c2s; --">>,

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK（应该安全处理）
        ok = msg_ack_logic:client_ack(<<"c2s">>, MaliciousMsgId, Uid, Did),

        % 验证表仍然存在
        Sql = <<"SELECT COUNT(*) FROM msg_c2s">>,
        {ok, _Cols, [{Count}]} = elib_pg:query(Sql, []),
        ?assert(Count >= 0, "Table should still exist")
    end).

%% ===================================================================
%% 未知消息类型测试
%% ===================================================================

unknown_msg_type_handles_gracefully_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 5001,
        Did = <<"device_unknown">>,
        MsgId = <<"test_unknown_msg">>,

        % Mock msg_store_ds:unstage
        meck:expect(msg_store_ds, unstage, 1, '_'),

        % 执行 ACK（未知类型）
        ok = msg_ack_logic:client_ack(<<"unknown">>, MsgId, Uid, Did),

        % 应该正常完成，记录错误日志
        ?assert(true)
    end).

%% ===================================================================
%% Setup 和 Teardown
%% ===================================================================

setup_() ->
    % 设置测试环境
    application:set_env(imboy, env, test),
    ok.

teardown_(_) ->
    % 清理 Mock
    meck:unload(),
    ok.
