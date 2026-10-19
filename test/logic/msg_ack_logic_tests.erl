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
    ack_db_test(fun(#{uid := Uid, from_uid := FromUid, did := Did, msg_id := MsgId}) ->
        %% 走真实仓储分配服务端顺序，不能用裸 INSERT 绕过接受顺序约束。
        %% Seed through the real allocator instead of bypassing accepted order.
        Now = elib_dt:now(),
        Payload = <<"{\"content\":\"test message\"}">>,
        ok = msg_c2c_repo:write_msg(Now, MsgId, Payload, FromUid, Uid, Now, <<"text">>, null),
        {ok, [#{<<"conv_seq">> := Seq}]} = elib_pg:query(
            <<"SELECT conv_seq FROM msg_c2c WHERE msg_id=$1 AND to_id=$2">>, [MsgId, Uid]
        ),
        ?assert(is_integer(Seq) andalso Seq > 0),
        ?assertEqual({ok, Seq}, personal_message_order_repo:accepted_sequence(FromUid, Uid, MsgId)),
        ?assertEqual({error, conflict_no_insert},
            msg_c2c_repo:write_msg(Now, MsgId, Payload, FromUid, Uid, Now, <<"text">>, null)),
        ?assertEqual({ok, Seq}, personal_message_order_repo:accepted_sequence(FromUid, Uid, MsgId)),
        ?assertEqual(1, message_count(<<"msg_c2c">>, MsgId)),

        ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),
        ?assertEqual(0, message_count(<<"msg_c2c">>, MsgId)),
        ?assertEqual(1, meck:num_calls(elib_metric, increment, [msg_delivered_total]))
    end).

c2c_ack_with_no_msg_test_() ->
    ack_db_test(fun(#{uid := Uid, did := Did, msg_id := MsgId}) ->
        ?assertEqual(0, message_count(<<"msg_c2c">>, MsgId)),
        ?assertEqual(ok, msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did)),
        ?assertEqual(0, message_count(<<"msg_c2c">>, MsgId))
    end).

%% ===================================================================
%% C2G ACK 处理测试
%% ===================================================================

c2g_ack_marks_timeline_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2g_timeline_repo, [{client_ack, 2, fun(_, _) -> {ok, 1} end}]},
            {elib_metric, [{increment, 1, fun(_) -> ok end}]},
            {msg_store_ds, [{unstage, 2, fun(_, _) -> ok end}]}
        ],
        fun() ->
            Uid = 2001,
            MsgId = <<"test_c2g_msg_001">>,
            ok = msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, <<"device_android_001">>),
            ?assertEqual(1, meck:num_calls(msg_c2g_timeline_repo, client_ack, [Uid, MsgId])),
            ?assertEqual(0, meck:num_calls(msg_store_ds, unstage, 2)),
            ?assert(meck:validate(msg_c2g_timeline_repo)),
            ?assert(meck:validate(msg_store_ds))
        end
    ).

%% ===================================================================
%% S2C ACK 处理测试
%% ===================================================================

s2c_ack_deletes_offline_msg_test_() ->
    ack_db_test(fun(#{uid := Uid, did := Did, msg_id := MsgId}) ->
        Sql = <<
            "INSERT INTO msg_s2c (id, from_id, to_id, msg_id, action, msg_type, payload, created_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7, NOW())"
        >>,
        Payload = <<"{\"msg_type\":\"system\"}">>,
        {ok, _} = elib_pg:execute(Sql,
            [elib_tsid:generate(msg_s2c), 0, Uid, MsgId, <<"notify">>, <<"system">>, Payload]),
        ?assertEqual(1, message_count(<<"msg_s2c">>, MsgId)),
        ok = msg_ack_logic:client_ack(<<"s2c">>, MsgId, Uid, Did),
        ?assertEqual(0, message_count(<<"msg_s2c">>, MsgId))
    end).

%% ===================================================================
%% C2S ACK 处理测试（SQL 安全）
%% ===================================================================

c2s_ack_uses_parameterized_query_test_() ->
    ack_db_test(fun(#{uid := Uid, did := Did, msg_id := MsgId}) ->
        Sql = <<
            "INSERT INTO msg_c2s (id, from_id, to_id, topic_id, msg_id, msg_type, payload, created_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7, NOW())"
        >>,
        Payload = <<"{\"text\":\"hello\"}">>,
        %% C2S 使用共享 staging 的 msg_store 发号域；没有 msg_c2s namespace。
        %% C2S uses the shared staging ID allocator, not a msg_c2s namespace.
        {ok, _} = elib_pg:execute(Sql,
            [elib_tsid:generate(msg_store), Uid, 0, 123, MsgId, <<"text">>, Payload]),
        ?assertEqual(1, message_count(<<"msg_c2s">>, MsgId)),
        ok = msg_ack_logic:client_ack(<<"c2s">>, MsgId, Uid, Did),
        ?assertEqual(0, message_count(<<"msg_c2s">>, MsgId))
    end).

c2s_ack_prevents_sql_injection_test_() ->
    ack_db_test(fun(#{uid := Uid, did := Did}) ->
        MaliciousMsgId = <<"msg_123'; DROP TABLE msg_c2s; --">>,
        ok = msg_ack_logic:client_ack(<<"c2s">>, MaliciousMsgId, Uid, Did),
        {ok, [#{<<"count">> := Count}]} = elib_pg:query(<<"SELECT COUNT(*) FROM msg_c2s">>, []),
        ?assert(Count >= 0)
    end).

%% ===================================================================
%% 未知消息类型测试
%% ===================================================================

unknown_msg_type_handles_gracefully_test_() ->
    ack_db_test(fun(#{uid := Uid, did := Did, msg_id := MsgId}) ->
        ?assertEqual(ok, msg_ack_logic:client_ack(<<"unknown">>, MsgId, Uid, Did))
    end).

%% ===================================================================
%% P0-2 回归：ACK 不得提前 unstage（staging 由 worker 落库后清理）
%% 不依赖 DB，纯 meck 验证
%% ===================================================================

client_ack_does_not_unstage_test_() ->
    ?WITH_MECKS(
        [
            {msg_operation_ds, [
                {'ack_c2c_msg', 3, fun(_MsgId, _Uid, _DID) -> ok end}
            ]},
            {elib_metric, [
                {'increment', 1, fun(_) -> ok end}
            ]},
            {msg_store_ds, [
                {'unstage', 2, fun(_Type, _MsgId) -> ok end}
            ]}
        ],
        fun() ->
            ok = msg_ack_logic:client_ack(<<"c2c">>, <<"msg_p0_2">>, 1001, <<"did-1">>),
            %% ACK 只标记送达，不得触碰 staging 生命周期
            ?assertEqual(0, meck:num_calls(msg_store_ds, unstage, 2))
        end
    ).

%% ===================================================================
%% 【MSG-P2-1】重复 ACK 不虚高投递指标（纯 meck，不依赖 DB）
%% ===================================================================

duplicate_ack_does_not_inflate_metric_test_() ->
    ?WITH_MECKS(
        [
            {msg_operation_ds, [
                %% {ok, 0} = 送达标记已存在（重复 ACK）
                {'ack_c2c_msg', 3, fun(_MsgId, _Uid, _DID) -> {ok, 0} end}
            ]},
            {elib_metric, [
                {'increment', 1, fun(_) -> ok end}
            ]}
        ],
        fun() ->
            ok = msg_ack_logic:client_ack(<<"c2c">>, <<"msg_dup">>, 1001, <<"did-1">>),
            ?assertEqual(0, meck:num_calls(elib_metric, increment, 1))
        end
    ).

effective_ack_increments_metric_test_() ->
    ?WITH_MECKS(
        [
            {msg_operation_ds, [
                {'ack_c2c_msg', 3, fun(_MsgId, _Uid, _DID) -> {ok, 1} end}
            ]},
            {elib_metric, [
                {'increment', 1, fun(_) -> ok end}
            ]}
        ],
        fun() ->
            ok = msg_ack_logic:client_ack(<<"c2c">>, <<"msg_new">>, 1001, <<"did-1">>),
            ?assertEqual(1, meck:num_calls(elib_metric, increment, 1))
        end
    ).

%% ===================================================================
%% Setup 和 Teardown
%% ===================================================================

ack_db_test(TestFun) ->
    {setup, fun setup_ack_db/0, fun cleanup_ack_db/1, fun(State) ->
        {timeout, 30, ?_test(begin
            TestFun(State),
            ?assert(meck:validate(elib_metric))
        end)}
    end}.

setup_ack_db() ->
    %% 缺数据库直接失败；mock 归 fixture 所有，断言失败/超时也会清理。
    %% Missing DB fails setup; fixture cleanup also runs after failure or timeout.
    {ok, Conn} = eunit_runner:eunit_setup_with_db(),
    try
        Uid = elib_tsid:generate(user),
        FromUid = elib_tsid:generate(user),
        {ok, elib_metric} = meck_helper:setup_mock(elib_metric, [
            {increment, 1, fun(_) -> ok end},
            {record, 2, fun(_, _) -> ok end}
        ]),
        #{conn => Conn, uid => Uid, from_uid => FromUid,
          did => <<"ack-fixture-device">>,
          msg_id => <<"ack-", (integer_to_binary(Uid))/binary>>,
          conv_key => msg_archive_repo:conv_key(<<"c2c">>, FromUid, Uid)}
    catch
        Class:Reason:Stack ->
            meck_helper:cleanup_mock(elib_metric),
            eunit_runner:eunit_cleanup_db(Conn),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup_ack_db(#{conn := Conn, msg_id := MsgId, conv_key := Key}) ->
    try
        lists:foreach(fun(Table) ->
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM ", Table/binary, " WHERE msg_id=$1">>, [MsgId]
            )
        end, [<<"msg_c2c">>, <<"msg_s2c">>, <<"msg_c2s">>, <<"msg_delivery">>]),
        {ok, _} = elib_pg:execute(<<"DELETE FROM msg_store_seq WHERE conv_key=$1">>, [Key])
    after
        meck_helper:cleanup_mock(elib_metric),
        eunit_runner:eunit_cleanup_db(Conn)
    end.

message_count(Table, MsgId) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        <<"SELECT COUNT(*) FROM ", Table/binary, " WHERE msg_id=$1">>, [MsgId]
    ),
    Count.

%% ===================================================================
%% Q04/F06（AC09）：C2G ACK 落库失败可观察——错误传播到调用方、
%% 投递指标不增长、恢复后补 ACK 只计一次。
%% ===================================================================

c2g_ack_db_failure_is_observable_and_metric_not_incremented_test_() ->
    c2g_metric_test({error, <<"injected_db_failure">>},
        {error, <<"injected_db_failure">>}, 0).

c2g_ack_duplicate_does_not_increment_metric_test_() ->
    c2g_metric_test({ok, 0}, ok, 0).

c2g_ack_first_delivery_increments_metric_once_test_() ->
    c2g_metric_test({ok, 1}, ok, 1).

c2g_metric_test(RepoResult, ExpectedResult, ExpectedIncrements) ->
    %% 返回 descriptor 给 EUnit 执行，不能在测试函数内生成后丢弃。
    %% Return the fixture descriptor so EUnit actually executes its assertions.
    ?WITH_MECKS(
        [
            {msg_c2g_timeline_repo, [{client_ack, 2, fun(_, _) -> RepoResult end}]},
            {elib_metric, [
                {increment, 1, fun(_) -> ok end},
                {record, 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            Uid = 1041,
            MsgId = <<"test_c2g_msg_q04">>,
            ?assertEqual(ExpectedResult,
                msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, <<"device_q04">>)),
            ?assertEqual(1, meck:num_calls(msg_c2g_timeline_repo, client_ack, [Uid, MsgId])),
            ?assertEqual(ExpectedIncrements,
                meck:num_calls(elib_metric, increment, [msg_delivered_total])),
            ?assert(meck:validate(msg_c2g_timeline_repo)),
            ?assert(meck:validate(elib_metric))
        end
    ).
