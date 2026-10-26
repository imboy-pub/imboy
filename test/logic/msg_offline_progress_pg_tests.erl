-module(msg_offline_progress_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R01（IMBOY-TIMEZONE-REPAIR-V1）：离线同步进度脱离客户端创建时间。
%%%
%%% 真实 PG（隔离库）验证：
%%%   R01-1 晚到旧 created_at（11:51 < 已保存游标 12:00）仍可获取：
%%%         带 DID 的按设备拉取不再按 created_at 过滤（pending 集合语义）
%%%   R01-2 时钟 ±10 分钟偏移不遗漏；ACK 后集合收缩（重复拉取有界）
%%%   R01-3 C2G 跨群 v2 cursor：服务端投递进度（per-group conv_seq，B 边界
%%%         钳制）跨群推进，重复拉取无丢失、无无限循环
%%%   R01-4 cursor 版本化：旧 v1 token 继续解码；v2 roundtrip；坏 token 拒绝
%%%   R01-5 补扫恢复位置可断言：批次推进、终点=已消费 seq、B 钳制
%%%===================================================================

-define(R1_GA, 997001).
-define(R1_GB, 997002).
-define(R1_UID, 99701001).
-define(R1_SENDER, 99701002).
-define(C2C_A, 99702001).
-define(C2C_B, 99702002).
-define(C2C_KEY, <<"c2c:99702001:99702002">>).
-define(DID, <<"d1">>).

%% 客户端已保存游标：12:00:00Z（毫秒）
-define(CURSOR_1200_MS, 1789601600000).
-define(CURSOR_1200, <<"2026-09-16T23:33:20Z">>).

cursor_1200_rfc() -> elib_dt:to_rfc3339(?CURSOR_1200_MS, millisecond, "Z").
before_cursor_rfc(MinutesAgo) ->
    elib_dt:to_rfc3339(?CURSOR_1200_MS - MinutesAgo * 60000, millisecond, "Z").

%% ------------------------------------------------------------------
%% fixture
%% ------------------------------------------------------------------

cleanup() ->
    _ = elib_pg:execute(<<"DELETE FROM public.msg_c2c WHERE msg_id LIKE $1">>, [<<"r01c2c_%">>]),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store WHERE msg_id LIKE $1">>, [<<"r01c2c_%">>]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_staging WHERE msg_id LIKE $1">>, [<<"r01c2c_%">>]
    ),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_delivery WHERE uid = $1">>, [?C2C_A]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>, [?C2C_KEY]),
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g_timeline WHERE to_gid = $1">>, [?R1_GA]),
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g_timeline WHERE to_gid = $1">>, [?R1_GB]),
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g WHERE to_id IN ($1, $2)">>, [?R1_GA, ?R1_GB]),
    _ = elib_pg:execute(
        <<"DELETE FROM msg_store WHERE conv_key IN ($1, $2)">>,
        [<<"c2g:997001">>, <<"c2g:997002">>]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM msg_store_staging WHERE type='c2g' AND to_id IN ($1, $2)">>,
        [?R1_GA, ?R1_GB]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM msg_store_seq WHERE conv_key IN ($1, $2)">>,
        [<<"c2g:997001">>, <<"c2g:997002">>]
    ),
    _ = elib_pg:execute(<<"DELETE FROM group_member_generation WHERE group_id IN ($1, $2)">>, [
        ?R1_GA, ?R1_GB
    ]),
    _ = elib_pg:execute(<<"DELETE FROM group_member WHERE group_id IN ($1, $2)">>, [
        ?R1_GA, ?R1_GB
    ]),
    _ = elib_pg:execute(<<"DELETE FROM \"group\" WHERE id IN ($1, $2)">>, [?R1_GA, ?R1_GB]),
    ok.

setup_group(Gid) ->
    {ok, _} =
        elib_pg:execute(
            <<"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status) ",
                "VALUES ($1, $2, $2, 'r01_fixture', 1)">>,
            [Gid, ?R1_SENDER]
        ),
    {ok, _} =
        elib_pg:execute(
            <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status) ",
                "VALUES ($1, $2, $3, 4, true, 1), ($4, $2, $5, 1, true, 1)">>,
            [Gid * 10 + 1, Gid, ?R1_SENDER, Gid * 10 + 2, ?R1_UID]
        ),
    {ok, _} =
        elib_pg:execute(
            <<"INSERT INTO group_member_generation (group_id, user_id, generation_no, start_seq) ",
                "VALUES ($1, $2, 1, 1), ($1, $3, 1, 1)">>,
            [Gid, ?R1_SENDER, ?R1_UID]
        ),
    ok.

%% C2C 直插正式行（已持久接受，未 ACK；conv_seq 显式给出）
insert_c2c(MsgId, FromId, ToId, CreatedAtRfc, Seq) ->
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, "
                "created_at, server_ts, conv_seq) ",
                "VALUES ($1, $2, $3, $4, 'text', '{}', $5, $5, $6)"
            >>,
            [99710000 + Seq, FromId, ToId, MsgId, CreatedAtRfc, Seq]
        ),
    ok.

%% C2G 直插正文 + timeline + 归档行（conv_seq 显式给出）。
%% msg_store 行是 B 边界的正式表 max 来源（worker 归档后的产物）。
insert_c2g(Gid, MsgId, Seq) ->
    {ok, _} =
        elib_pg:execute(
            <<"INSERT INTO msg_c2g (id, from_id, to_id, msg_id, msg_type, payload, created_at) ",
                "VALUES ($1, $2, $3, $4, 'text', '{}', $5)">>,
            [Gid * 100 + Seq, ?R1_SENDER, Gid, MsgId, cursor_1200_rfc()]
        ),
    {ok, _} =
        elib_pg:execute(
            <<"INSERT INTO msg_c2g_timeline (msg_id, to_uid, to_gid, created_at, conv_seq) ",
                "VALUES ($1, $2, $3, $4, $5)">>,
            [MsgId, ?R1_UID, Gid, cursor_1200_rfc(), Seq]
        ),
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO msg_store (id, msg_id, chat_type, conv_key, conv_seq, msg_type, "
                "from_id, to_id, group_id, e2ee, sender_did, payload, created_at, server_ts) ",
                "VALUES ($1, $2, 'c2g', $3, $4, 'text', $5, $6, null, null, null, '{}', $7, $7)"
            >>,
            [
                Gid * 200 + Seq,
                MsgId,
                <<"c2g:", (integer_to_binary(Gid))/binary>>,
                Seq,
                ?R1_SENDER,
                Gid,
                cursor_1200_rfc()
            ]
        ),
    ok.

c2c_offline_ids(CursorMs) ->
    Payload = messaging_logic:offline(?C2C_A, 50, CursorMs, 0, 0, ?DID, undefined),
    lists:sort([
        maps:get(<<"msg_id">>, M)
     || M <- maps:get(<<"list">>, maps:get(<<"c2c">>, Payload))
    ]).

%% ------------------------------------------------------------------
%% R01-1：晚到旧 created_at 仍可获取（11:51 < 游标 12:00）
%% ------------------------------------------------------------------
r01_1_late_old_created_at_reachable_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgLate = <<"r01c2c_late">>,
        %% 服务端在游标之后新接受的 created_at=11:51（客户端时钟慢 9 分钟）
        insert_c2c(MsgLate, ?C2C_B, ?C2C_A, before_cursor_rfc(9), 1),

        %% 旧客户端（无 DID）保持时间游标语义：该消息按 created_at 过滤不取回
        PayloadOld = messaging_logic:offline(?C2C_A, 50, ?CURSOR_1200_MS, 0, 0, <<>>, undefined),
        ?assertEqual(
            [],
            maps:get(<<"list">>, maps:get(<<"c2c">>, PayloadOld))
        ),

        %% 新语义（带 DID，按设备未 ACK 集合）：晚到旧时间消息不漏
        ?assertEqual([MsgLate], c2c_offline_ids(?CURSOR_1200_MS))
    end).

%% ------------------------------------------------------------------
%% R01-2：时钟 ±10 分钟不遗漏；ACK 后集合收缩（有界）
%% ------------------------------------------------------------------
r01_2_clock_skew_no_miss_and_bounded_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"r01c2c_sk1">>,
        MsgB = <<"r01c2c_sk2">>,
        %% 服务端接受顺序 1、2，created_at 分别比游标早 10 分钟 / 8 分钟
        insert_c2c(MsgA, ?C2C_B, ?C2C_A, before_cursor_rfc(10), 1),
        insert_c2c(MsgB, ?C2C_B, ?C2C_A, before_cursor_rfc(8), 2),

        ?assertEqual(
            lists:sort([MsgA, MsgB]), c2c_offline_ids(?CURSOR_1200_MS)
        ),

        %% ACK 后未确认集合收缩：重复拉取有界（不再重取）
        {ok, _} = msg_operation_ds:ack_c2c_batch([MsgA, MsgB], ?C2C_A, ?DID),
        ?assertEqual([], c2c_offline_ids(?CURSOR_1200_MS))
    end).

%% ------------------------------------------------------------------
%% R01-3：C2G 跨群 v2 cursor 推进，无丢失、无无限循环
%% ------------------------------------------------------------------
r01_3_cross_group_v2_cursor_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        ok = setup_group(?R1_GA),
        ok = setup_group(?R1_GB),
        lists:foreach(
            fun(N) -> insert_c2g(?R1_GA, <<"r01ga_", (integer_to_binary(N))/binary>>, N) end,
            lists:seq(1, 2)
        ),
        lists:foreach(
            fun(N) -> insert_c2g(?R1_GB, <<"r01gb_", (integer_to_binary(N))/binary>>, N) end,
            lists:seq(1, 2)
        ),

        Cursor0 = {marks, [{?R1_GA, 0}, {?R1_GB, 0}]},
        Payload1 = messaging_logic:offline(?R1_UID, 10, 0, 0, 0, <<>>, Cursor0),
        C2g1 = maps:get(<<"c2g">>, Payload1),
        Ids1 =
            lists:sort([maps:get(<<"msg_id">>, M) || M <- maps:get(<<"list">>, C2g1)]),
        ?assertEqual(4, length(Ids1)),

        %% next_cursor v2：marks 推进到各群最大 conv_seq（<= safe_head）
        {ok, {marks, Marks1}} =
            msg_offline_cursor:decode(maps:get(<<"next_cursor">>, C2g1)),
        ?assertMatch([{?R1_GA, 2}, {?R1_GB, 2}], lists:sort(Marks1)),

        %% 用新 cursor 再拉：0 条（无重复、无无限循环）
        Payload2 = messaging_logic:offline(?R1_UID, 10, 0, 0, 0, <<>>, {marks, Marks1}),
        C2g2 = maps:get(<<"c2g">>, Payload2),
        ?assertEqual([], maps:get(<<"list">>, C2g2)),
        ?assertEqual(0, maps:get(<<"total">>, C2g2))
    end).

%% ------------------------------------------------------------------
%% R01-5：补扫/推进恢复位置可断言 + B 边界钳制
%% ------------------------------------------------------------------
r01_5_seq_batch_recovery_and_safe_head_clamp_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        ok = setup_group(?R1_GA),
        lists:foreach(
            fun(N) -> insert_c2g(?R1_GA, <<"r01rc_", (integer_to_binary(N))/binary>>, N) end,
            lists:seq(1, 3)
        ),
        %% 故障注入：一条 c2g staging 非终态行 seq=4（暂未归档，可重试）
        {ok, _} =
            elib_pg:execute(
                <<
                    "INSERT INTO msg_store_staging (id, type, msg_id, payload, from_id, to_id, "
                    "to_id_list, created_at, server_ts, conv_seq) ",
                    "VALUES (99799001, 'c2g', 'r01rc_stuck', '{}', $1, $2, '{3}'::bigint[], NOW(), NOW(), 4)"
                >>,
                [?R1_SENDER, ?R1_GA]
            ),

        %% 安全推进边界：正式 max=3，staging 非终态 min=4 → B=3
        {ok, B} = msg_archive_repo:safe_head(<<"c2g:997001">>, <<"c2g">>, ?R1_GA, 0),
        ?assertEqual(3, B),

        %% 批次 limit=2：返回 2 条，marks 恢复位置=2
        Cursor0 = {marks, [{?R1_GA, 0}]},
        P1 = messaging_logic:offline(?R1_UID, 2, 0, 0, 0, <<>>, Cursor0),
        C2g1 = maps:get(<<"c2g">>, P1),
        ?assertEqual(2, length(maps:get(<<"list">>, C2g1))),
        {ok, {marks, Marks1}} =
            msg_offline_cursor:decode(maps:get(<<"next_cursor">>, C2g1)),
        ?assertEqual([{?R1_GA, 2}], Marks1),

        %% 续扫批次：拿到第 3 条，marks=3（不超过 B）
        P2 = messaging_logic:offline(?R1_UID, 2, 0, 0, 0, <<>>, {marks, Marks1}),
        C2g2 = maps:get(<<"c2g">>, P2),
        ?assertEqual(1, length(maps:get(<<"list">>, C2g2))),
        {ok, {marks, Marks2}} =
            msg_offline_cursor:decode(maps:get(<<"next_cursor">>, C2g2)),
        ?assertEqual([{?R1_GA, 3}], Marks2)
    end).

%% ------------------------------------------------------------------
%% R01-4：cursor 版本化兼容（旧 token 可解码、v2 roundtrip、坏 token 拒绝）
%% ------------------------------------------------------------------
r01_4_cursor_version_compat_test_() ->
    {"R01-4 cursor 兼容", [
        ?_assertEqual(
            {ok, {<<"2026-10-07T02:00:00Z">>, <<"m1">>}},
            msg_offline_cursor:decode(
                base64:encode(
                    <<"{\"v\":1,\"at\":\"2026-10-07T02:00:00Z\",\"id\":\"m1\"}">>
                )
            )
        ),
        ?_assertEqual(
            {ok, {marks, [{997001, 3}, {997002, 7}]}},
            msg_offline_cursor:decode(
                msg_offline_cursor:encode({marks, [{997001, 3}, {997002, 7}]})
            )
        ),
        ?_assertEqual(
            <<>>,
            msg_offline_cursor:encode(undefined)
        ),
        ?_assertMatch(
            {error, invalid_offline_cursor},
            msg_offline_cursor:decode(<<"not-a-cursor">>)
        ),
        %% v2 缺 marks / 空 marks / 非法成员一律拒绝（fail-closed）
        ?_assertMatch(
            {error, invalid_offline_cursor},
            msg_offline_cursor:decode(base64:encode(<<"{\"v\":2,\"marks\":[]}">>))
        ),
        ?_assertMatch(
            {error, invalid_offline_cursor},
            msg_offline_cursor:decode(base64:encode(<<"{\"v\":2}">>))
        ),
        %% v1 时间非法值仍按旧逻辑 fail-closed（rfc3339 校验）
        ?_assertMatch(
            {error, invalid_offline_cursor},
            msg_offline_cursor:decode(
                base64:encode(<<"{\"v\":1,\"at\":\"nope\",\"id\":\"m\"}">>)
            )
        )
    ]}.
