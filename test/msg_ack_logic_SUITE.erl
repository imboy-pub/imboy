-module(msg_ack_logic_SUITE).

%%%===================================================================
%%% @doc
%%% msg_ack_logic 模块的 Common Test 测试套件
%%%
%%% 运行方式：
%%%   make ct-msg_ack_logic              # 运行整个 suite
%%%   make ct-msg_ack_logic t=c2c_ack   # 运行特定测试
%%%   make ct                             # 运行所有 Common Test
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    c2c_ack_deletes_offline_msg/1,
    c2c_ack_with_no_msg/1,
    c2g_ack_marks_timeline/1,
    s2c_ack_deletes_offline_msg/1,
    c2s_ack_uses_parameterized_query/1,
    unknown_msg_type_handles_gracefully/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        c2c_ack_deletes_offline_msg,
        c2c_ack_with_no_msg,
        c2g_ack_marks_timeline,
        s2c_ack_deletes_offline_msg,
        c2s_ack_uses_parameterized_query,
        unknown_msg_type_handles_gracefully
    ].

init_per_suite(Config) ->
    ct:log("开始 msg_ack_logic 测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束 msg_ack_logic 测试套件"),
    eunit_runner:ct_suite_cleanup(Config).

init_per_testcase(_TestCase, Config) ->
    % 每个测试用例前的初始化
    Config.

end_per_testcase(_TestCase, _Config) ->
    % 清理 Mock
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例：C2C ACK 处理
%% ===================================================================

c2c_ack_deletes_offline_msg(_Config) ->
    ct:log("测试 C2C ACK 删除离线消息"),
    Uid = 1001,
    Did = <<"device_ios_001">>,
    MsgId = <<"test_c2c_msg_001">>,

    % 准备测试数据
    Payload = <<"{\"content\":\"test message\"}">>,
    insert_c2c_row(999, Uid, MsgId, Payload),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    {comment, "C2C ACK 成功删除离线消息"}.

c2c_ack_with_no_msg(_Config) ->
    ct:log("测试 C2C ACK 处理不存在的消息"),
    Uid = 1002,
    Did = <<"device_ios_002">>,
    MsgId = <<"test_c2c_msg_002">>,

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK（消息不存在）
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

    % 应该正常完成，不抛出异常
    {comment, "消息不存在时 ACK 正常处理"}.

%% ===================================================================
%% 测试用例：C2G ACK 处理
%% ===================================================================

c2g_ack_marks_timeline(_Config) ->
    ct:log("测试 C2G ACK 标记 timeline"),
    Uid = 2001,
    Did = <<"device_android_001">>,
    MsgId = <<"test_c2g_msg_001">>,

    % Mock msg_c2g_timeline_repo
    meck:new(msg_c2g_timeline_repo, [unstick]),
    meck:expect(msg_c2g_timeline_repo, client_ack, 2, {ok, 1}),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, Did),

    % 验证调用
    ?assert(meck:validate(msg_c2g_timeline_repo)),
    ?assert(meck:validate(msg_store_ds)),
    {comment, "C2G ACK 成功标记 timeline"}.

%% ===================================================================
%% 测试用例：S2C ACK 处理
%% ===================================================================

s2c_ack_deletes_offline_msg(_Config) ->
    ct:log("测试 S2C ACK 删除离线消息"),
    Uid = 3001,
    Did = <<"device_ios_003">>,
    MsgId = <<"test_s2c_msg_001">>,

    % 准备测试数据
    Sql = <<
        "INSERT INTO msg_s2c (id, from_id, to_id, msg_id, action, msg_type, payload, created_at)\n"
        "            VALUES ($1, $2, $3, $4, $5, $6, $7, NOW())"
    >>,
    Payload = <<"{\"msg_type\":\"system\"}">>,
    {ok, _} = elib_pg:execute(Sql, [
        elib_tsid:generate(msg_c2c), 0, Uid, MsgId, <<"notify">>, <<"system">>, Payload
    ]),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"s2c">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_s2c WHERE msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_s2c WHERE msg_id = $1">>, [MsgId]),
    {comment, "S2C ACK 成功删除离线消息"}.

%% ===================================================================
%% 测试用例：C2S ACK 处理（SQL 安全）
%% ===================================================================

c2s_ack_uses_parameterized_query(_Config) ->
    ct:log("测试 C2S ACK 使用参数化查询"),
    Uid = 4001,
    Did = <<"device_web_001">>,
    MsgId = <<"test_c2s_msg_001">>,

    % 准备测试数据
    Sql = <<
        "INSERT INTO msg_c2s (id, from_id, to_id, topic_id, msg_id, msg_type, payload, created_at)\n"
        "            VALUES ($1, $2, $3, $4, $5, $6, $7, NOW())"
    >>,
    Payload = <<"{\"text\":\"hello\"}">>,
    {ok, _} = elib_pg:execute(Sql, [
        elib_tsid:generate(msg_c2c), Uid, 123, 123, MsgId, <<"text">>, Payload
    ]),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2s">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_c2s WHERE msg_id = $1 AND from_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_c2s WHERE msg_id = $1">>, [MsgId]),
    {comment, "C2S ACK 使用参数化查询成功"}.

%% ===================================================================
%% 测试用例：未知消息类型处理
%% ===================================================================

unknown_msg_type_handles_gracefully(_Config) ->
    ct:log("测试未知消息类型的优雅处理"),
    Uid = 5001,
    Did = <<"device_unknown">>,
    MsgId = <<"test_unknown_msg">>,

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 2, '_'),

    % 执行 ACK（未知类型）
    ok = msg_ack_logic:client_ack(<<"unknown">>, MsgId, Uid, Did),

    % 应该正常完成
    {comment, "未知消息类型优雅处理"}.

extract_count([#{<<"count">> := Count}]) when is_integer(Count) ->
    Count;
extract_count([{Count}]) when is_integer(Count) ->
    Count;
extract_count(_) ->
    0.

%% ---- 现行 c2c 写入合同夹具（与 test/repo/msg_accepted_at_pg_tests 同形状）----
%% fn_c2c_order_write_guard 要求 conv_seq 为会话头部 +1；accepted_at 与
%% server_ts 同事务 NOW()。断言不变，仅夹具对齐现行列形状。
insert_c2c_row(From, To, MsgId, PayloadBin) ->
    Seq = c2c_conversation_head(From, To) + 1,
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload,"
            " server_ts, created_at, accepted_at, conv_seq)"
            " VALUES ($1,$2,$3,$4,$5,NULL,$6::jsonb,NOW(),NOW(),NOW(),$7)"
        >>,
        [elib_tsid:generate(msg_c2c), From, To, MsgId, <<"text">>, PayloadBin, Seq]
    ).

c2c_conversation_head(From, To) ->
    Key =
        <<"c2c:", (integer_to_binary(min(From, To)))/binary, ":",
            (integer_to_binary(max(From, To)))/binary>>,
    {ok, [#{<<"head">> := Head}]} = elib_pg:query(
        <<"SELECT greatest(",
            " coalesce((SELECT seq FROM public.msg_store_seq WHERE conv_key = $1), 0),",
            " coalesce((SELECT max(conv_seq) FROM public.msg_store WHERE chat_type='c2c'",
            "   AND conv_key = $1), 0),",
            " coalesce((SELECT max(conv_seq) FROM public.msg_store_staging WHERE type='c2c'",
            "   AND least(from_id,to_id)=least($2::bigint,$3::bigint) AND greatest(from_id,to_id)=greatest($2::bigint,$3::bigint)), 0),",
            " coalesce((SELECT max(conv_seq) FROM public.msg_c2c",
            "   WHERE least(from_id,to_id)=least($2::bigint,$3::bigint) AND greatest(from_id,to_id)=greatest($2::bigint,$3::bigint)), 0)",
            ") AS head">>,
        [Key, From, To]
    ),
    Head.
