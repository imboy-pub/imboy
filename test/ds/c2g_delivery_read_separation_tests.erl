-module(c2g_delivery_read_separation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% Q04/F05（AC07/AC08）：C2G 送达（delivery ACK）与已读（read cursor）分离
%%%
%%% 行为合同不变量（control/designs/Q04-contract.md，冻结版 v1）：
%%%   I1 送达 ACK 永不推进已读游标、永不清/减未读计数
%%%   I2 已读游标单调递增；重复/逆序已读事件不回退游标、不误清新消息未读
%%%   I3 同用户设备 B 离线期间的消息，B 上线后必须完整取回——不得因设备 A
%%%      的送达 ACK 而删除或标记清除
%%%   I4 未读计数 = 已投递给该用户、且未被该用户已读游标覆盖的消息数
%%%   I5 送达与已读事件链路不得互相触发
%%%
%%% 双设备语义（AC07/AC08 oracle）在单测层用同一 uid 的两次不同 DID 的
%%% ACK/read 序列模拟：client_ack 置位代表设备 A 送达确认，list_by_uid_since
%%% 代表设备 B 的离线补拉。
%%%===================================================================

-define(F05_GID, 9876541).
-define(F05_UID, 98765001).
-define(F05_SENDER, 98765002).
-define(F05_CREATED_AT, <<"2026-10-07T02:00:00Z">>).

%% ===================================================================
%% fixture
%% ===================================================================

setup_fixture() ->
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status) ",
            "VALUES ($1, $2, $2, 'f05_fixture', 1)">>,
        [?F05_GID, ?F05_SENDER]
    ),
    %% role/is_join 显式对齐生产写入口 group_member_repo:upsert_active
    %% （owner_uid=sender：sender role=4、receiver role=1，is_join=true）。
    %% 缺列会落 schema 默认 role=0（?ROLE_UNDEFINED）幻影成员——与
    %% ws_e2e_test.py 踩过的 forbidden 同构（EUNIT-FIXTURE-AUDIT A1/HIGH）。
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status) ",
            "VALUES ($1, $2, $3, 4, true, 1), ($4, $2, $5, 1, true, 1)">>,
        [98000001, ?F05_GID, ?F05_SENDER, 98000002, ?F05_UID]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member_generation ", "(group_id, user_id, generation_no, start_seq) ",
            "VALUES ($1, $2, 1, 1), ($1, $3, 1, 1)">>,
        [?F05_GID, ?F05_SENDER, ?F05_UID]
    ),
    ok.

%% 插入一条群消息（正文 + 收件人 timeline 行；发送者默认他人）
-spec insert_msg(binary(), integer(), integer()) -> ok.
insert_msg(MsgId, FromId, Seq) ->
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO msg_c2g (id, from_id, to_id, msg_id, msg_type, payload, created_at) ",
            "VALUES ($1, $2, $3, $4, 'text', '{\"content\":\"f05\"}', $5)">>,
        [98010000 + Seq, FromId, ?F05_GID, MsgId, ?F05_CREATED_AT]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO msg_c2g_timeline (msg_id, to_uid, to_gid, created_at, conv_seq) ",
            "VALUES ($1, $2, $3, $4, $5)">>,
        [MsgId, ?F05_UID, ?F05_GID, ?F05_CREATED_AT, Seq]
    ),
    ok.

-spec read_seq_of(integer(), integer()) -> non_neg_integer().
read_seq_of(Uid, Gid) ->
    Sql =
        <<"SELECT COALESCE(r.read_seq, 0) AS rs FROM group_member_generation gmg ",
            "LEFT JOIN workspace_group_read_cursor r ON r.generation_id = gmg.id ",
            "WHERE gmg.group_id = $1 AND gmg.user_id = $2 AND gmg.end_seq IS NULL">>,
    {ok, [#{<<"rs">> := Rs}]} = elib_pg:query(Sql, [Gid, Uid]),
    Rs.

-spec timeline_client_ack(binary()) -> boolean() | not_found.
timeline_client_ack(MsgId) ->
    Sql = <<"SELECT client_ack FROM msg_c2g_timeline WHERE to_uid = $1 AND msg_id = $2">>,
    case elib_pg:query(Sql, [?F05_UID, MsgId]) of
        {ok, [#{<<"client_ack">> := V}]} -> V;
        {ok, []} -> not_found
    end.

cleanup_fixture() ->
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g_timeline WHERE to_gid = $1">>, [?F05_GID]),
    _ = elib_pg:execute(
        <<"DELETE FROM msg_c2g WHERE to_id = $1 AND msg_id LIKE 'f05m%'">>, [?F05_GID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM workspace_group_read_cursor WHERE generation_id IN ",
            "(SELECT id FROM group_member_generation WHERE group_id = $1)">>,
        [?F05_GID]
    ),
    _ = elib_pg:execute(<<"DELETE FROM group_member_generation WHERE group_id = $1">>, [
        ?F05_GID
    ]),
    _ = elib_pg:execute(<<"DELETE FROM group_member WHERE group_id = $1">>, [?F05_GID]),
    _ = elib_pg:execute(<<"DELETE FROM \"group\" WHERE id = $1">>, [?F05_GID]),
    ok.

-spec with_fixture(fun()) -> ok.
with_fixture(TestFun) ->
    cleanup_fixture(),
    ok = setup_fixture(),
    try
        TestFun()
    after
        cleanup_fixture()
    end.

%% ===================================================================
%% I1：送达 ACK 永不清/减未读（未读只由已读游标驱动）
%% ===================================================================

i1_delivery_ack_keeps_unread_test_() ->
    ?TEST_WITH_DB(fun() ->
        with_fixture(fun() ->
            ok = insert_msg(<<"f05m1">>, ?F05_SENDER, 1),
            ok = insert_msg(<<"f05m2">>, ?F05_SENDER, 2),
            ok = insert_msg(<<"f05m3">>, ?F05_SENDER, 3),
            ?assertEqual(3, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),

            %% 设备 A（DID_A）逐条送达确认——全部消息已"至少一台设备收到"
            {ok, _} = msg_operation_ds:ack_c2g_timeline(<<"f05m1">>, ?F05_UID),
            {ok, _} = msg_operation_ds:ack_c2g_timeline(<<"f05m2">>, ?F05_UID),
            {ok, _} = msg_operation_ds:ack_c2g_timeline(<<"f05m3">>, ?F05_UID),

            %% I1 oracle：送达 ACK 后未读计数不变（用户从未阅读）
            ?assertEqual(3, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),
            %% 已读游标也未被送达 ACK 推进（I1 另一半）
            ?assertEqual(0, read_seq_of(?F05_UID, ?F05_GID))
        end)
    end).

%% ===================================================================
%% I2：已读游标单调；重复/逆序不回退、不误清新消息未读
%% ===================================================================

i2_cursor_monotonic_test_() ->
    ?TEST_WITH_DB(fun() ->
        with_fixture(fun() ->
            ok = insert_msg(<<"f05m1">>, ?F05_SENDER, 1),
            ok = insert_msg(<<"f05m2">>, ?F05_SENDER, 2),
            ok = insert_msg(<<"f05m3">>, ?F05_SENDER, 3),

            %% 设备 A 阅读到 seq2
            {ok, [_]} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m1">>, <<"f05m2">>]),
            ?assertEqual(2, read_seq_of(?F05_UID, ?F05_GID)),
            ?assertEqual(1, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),

            %% 逆序/重复已读事件：游标不回退、未读不变
            {ok, _} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m1">>]),
            ?assertEqual(2, read_seq_of(?F05_UID, ?F05_GID)),
            ?assertEqual(1, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),

            %% 阅读到 seq3（读到最新）
            {ok, [_]} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m3">>]),
            ?assertEqual(3, read_seq_of(?F05_UID, ?F05_GID)),
            ?assertEqual(0, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),

            %% AC08：随后 seq4（新消息 N+1）到达，仍未读；逆序已读不误清它
            ok = insert_msg(<<"f05m4">>, ?F05_SENDER, 4),
            ?assertEqual(1, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),
            {ok, _} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m1">>]),
            ?assertEqual(3, read_seq_of(?F05_UID, ?F05_GID)),
            ?assertEqual(1, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID))
        end)
    end).

%% ===================================================================
%% I3：设备 A 送达 ACK / 离线批量确认后，设备 B 仍可完整取回
%% ===================================================================

i3_offline_retrieval_after_peer_device_ack_test_() ->
    ?TEST_WITH_DB(fun() ->
        with_fixture(fun() ->
            ok = insert_msg(<<"f05m1">>, ?F05_SENDER, 1),
            ok = insert_msg(<<"f05m2">>, ?F05_SENDER, 2),

            %% 设备 A 在线实时收到，发 CLIENT_ACK（per-uid 送达标记置位）
            {ok, _} = msg_operation_ds:ack_c2g_timeline(<<"f05m1">>, ?F05_UID),
            ?assertEqual(true, timeline_client_ack(<<"f05m1">>)),

            %% 设备 B 上线离线补拉：已 ACK 的行必须仍可取回
            {ok, Rows} = msg_c2g_timeline_repo:list_by_uid(?F05_UID, <<"tl.msg_id">>, 10),
            GotIds = [maps:get(<<"msg_id">>, R) || R <- Rows],
            ?assert(lists:member(<<"f05m1">>, GotIds)),
            ?assert(lists:member(<<"f05m2">>, GotIds)),

            %% 设备 A 走 REST offline_ack（批量离线确认）——不得删行
            {ok, _Payload} = messaging_logic:offline_ack(
                ?F05_UID, <<"c2g">>, [<<"f05m1">>, <<"f05m2">>], <<"f05_did_a">>
            ),
            ?assertEqual(true, timeline_client_ack(<<"f05m1">>)),
            ?assertEqual(true, timeline_client_ack(<<"f05m2">>)),

            %% 设备 B 之后离线补拉仍完整取回
            {ok, Rows2} = msg_c2g_timeline_repo:list_by_uid(?F05_UID, <<"tl.msg_id">>, 10),
            GotIds2 = [maps:get(<<"msg_id">>, R) || R <- Rows2],
            ?assert(lists:member(<<"f05m1">>, GotIds2)),
            ?assert(lists:member(<<"f05m2">>, GotIds2))
        end)
    end).

%% ===================================================================
%% I4：未读 = 已投递且未被已读游标覆盖；本人发送不计未读
%% ===================================================================

i4_unread_excludes_own_messages_test_() ->
    ?TEST_WITH_DB(fun() ->
        with_fixture(fun() ->
            ok = insert_msg(<<"f05m1">>, ?F05_SENDER, 1),
            ok = insert_msg(<<"f05m2">>, ?F05_SENDER, 2),
            %% 自己发送的消息（发送者 timeline 行：from_id = 本人）
            ok = insert_msg(<<"f05m3">>, ?F05_UID, 3),

            %% 3 行都已投递，未读只计他人 2 条
            ?assertEqual(2, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID)),

            %% 阅读到 seq2 后未读归零（seq3 是自己发的）
            {ok, [_]} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m2">>]),
            ?assertEqual(0, msg_c2g_ds:count_unread_by_uid(?F05_UID, ?F05_GID))
        end)
    end).

%% ===================================================================
%% I5：送达与已读链路互不触发
%% ===================================================================

i5_no_cross_trigger_test_() ->
    ?TEST_WITH_DB(fun() ->
        with_fixture(fun() ->
            ok = insert_msg(<<"f05m1">>, ?F05_SENDER, 1),
            ok = insert_msg(<<"f05m2">>, ?F05_SENDER, 2),

            %% 送达 ACK 不推进已读游标
            {ok, _} = msg_operation_ds:ack_c2g_timeline(<<"f05m1">>, ?F05_UID),
            ?assertEqual(0, read_seq_of(?F05_UID, ?F05_GID)),

            %% 已读上报不改写送达标记（m1 保持 true，m2 保持 false）
            {ok, [_]} = group_ds:mark_read(?F05_UID, ?F05_GID, [<<"f05m1">>, <<"f05m2">>]),
            ?assertEqual(2, read_seq_of(?F05_UID, ?F05_GID)),
            ?assertEqual(true, timeline_client_ack(<<"f05m1">>)),
            ?assertEqual(false, timeline_client_ack(<<"f05m2">>))
        end)
    end).

%% ===================================================================
%% WS action 编排：c2g/message_read（msg_c2g_logic:c2g_read）
%% ===================================================================

c2g_read_broadcasts_sync_and_replies_ack_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(?F05_UID, ?F05_GID) -> true end},
                {'mark_read', 3, fun(?F05_UID, ?F05_GID, [<<"f05m1">>, <<"f05m2">>]) ->
                    {ok, [#{<<"read_seq">> => 2}]}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun
                    (0, [?F05_UID], <<"message_read_sync">>, <<>>, null, Payload, save) ->
                        self() ! {read_sync, Payload},
                        ok;
                    (_, _, _, _, _, _, _) ->
                        ok
                end}
            ]}
        ],
        fun() ->
            {reply, Msg} = msg_c2g_logic:c2g_read(
                <<"f05rcpt1">>,
                ?F05_UID,
                #{
                    <<"to">> => integer_to_binary(?F05_GID),
                    <<"payload">> => #{<<"msg_ids">> => [<<"f05m2">>, <<"f05m1">>]}
                }
            ),
            ?assertEqual(<<"message_read_ack">>, maps:get(<<"action">>, Msg)),
            ?assertEqual(2, maps:get(<<"read_seq">>, maps:get(<<"payload">>, Msg))),

            %% AC08：跨设备已读同步事件（S2C message_read_sync，c2g 扩展载荷）
            SyncPayload =
                receive
                    {read_sync, P} -> P
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, SyncPayload),
            ?assertEqual(<<"c2g">>, maps:get(<<"conversation_type">>, SyncPayload)),
            ?assertEqual(?F05_GID, maps:get(<<"gid">>, SyncPayload)),
            ?assertEqual(2, maps:get(<<"read_seq">>, SyncPayload)),
            %% msg_ids 排序去重后下发
            ?assertEqual([<<"f05m1">>, <<"f05m2">>], maps:get(<<"msg_ids">>, SyncPayload))
        end
    ).

c2g_read_rejects_non_member_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(?F05_UID, ?F05_GID) -> false end},
                {'mark_read', 3, fun(_, _, _) -> exit(unexpected_mark_read) end}
            ]}
        ],
        fun() ->
            {reply, Msg} = msg_c2g_logic:c2g_read(
                <<"f05rcpt2">>,
                ?F05_UID,
                #{
                    <<"to">> => integer_to_binary(?F05_GID),
                    <<"payload">> => #{<<"msg_ids">> => [<<"f05m1">>]}
                }
            ),
            ?assertEqual(
                <<"invalid_operation">>,
                maps:get(<<"error">>, maps:get(<<"payload">>, Msg))
            ),
            ?assertEqual(0, meck:num_calls(group_ds, mark_read, 3))
        end
    ).

c2g_read_no_op_when_cursor_not_advanced_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(?F05_UID, ?F05_GID) -> true end},
                %% msg_ids 全部未命中本人 timeline（行已被 retention/容量清理）
                {'mark_read', 3, fun(_, _, _) -> {ok, []} end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_, _, _, _, _, _, _) -> exit(unexpected_sync) end}
            ]}
        ],
        fun() ->
            {reply, Msg} = msg_c2g_logic:c2g_read(
                <<"f05rcpt3">>,
                ?F05_UID,
                #{
                    <<"to">> => integer_to_binary(?F05_GID),
                    <<"payload">> => #{<<"msg_ids">> => [<<"f05gone">>]}
                }
            ),
            %% 游标不动 → 不广播 sync；仍幂等回 ack
            ?assertEqual(<<"message_read_ack">>, maps:get(<<"action">>, Msg)),
            ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).

%% ===================================================================
%% WS action 注册表合同
%% ===================================================================

registry_has_c2g_message_read_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, {msg_c2g_logic, c2g_read}},
            imboy_ws_action_registry:builtin_lookup(<<"c2g">>, <<"message_read">>)
        )
    end).
