-module(group_history_boundary_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% E2EE-2026-012 / Task 8 (LT-03)：统一群历史 join boundary。
%%% 当前实现语义：F2（仅入群后）+ R2（重入新世代）+ D3（账号级 ACL）+ M1（祖传 start_seq=1）；用户选择已记录。
%%% 行为矩阵（真库 scratch，逐套件串行）：
%%%   F2 边界：join 后 history(seq=0) 不得返回 start_seq 之前归档；
%%%   R2 重入：leave 关世代 → 旧授权失效；rejoin 新世代 → 只见重入后；
%%%   staging 预分配：c2g 接受事务内固化 conv_seq，archive 搬运不二次分配；
%%%   幂等：重复 msg_id 重试保持原行原 seq；缺边界 fail-closed（deny）；
%%%   M1：直插的 legacy active 成员经 backfill 规则得 start_seq=1。

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

ts() ->
    <<"2026-09-10 08:00:00+08">>.

mk_group(Gid, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, member_max, "
            "member_count, introduction, avatar, title, chat_aes_key, status, "
            "created_at, e2ee_mode, scope) VALUES ($1,$2,$3,500,0,'','','t','k',1,now(),0,'personal')"
        >>,
        [Gid, OwnerUid, OwnerUid]
    ),
    ok.

%% 直插 legacy active 成员（不产生世代——模拟迁移前的存量行）
legacy_member(Gid, Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.group_member (id, group_id, user_id, role, is_join, "
            "join_mode, status, created_at, updated_at) "
            "VALUES ($1,$2,$3,1,true,'invite',1,now(),now())"
        >>,
        [uid(), Gid, Uid]
    ),
    ok.

%% 直插归档行（受控 conv_seq）
archive_row(Gid, Seq, FromUid) ->
    MsgId = iolist_to_binary(["u", integer_to_binary(Seq)]),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.msg_store (id, chat_type, conv_key, conv_seq, msg_id, "
            "msg_type, from_id, group_id, payload, created_at, server_ts) "
            "VALUES ($1,'c2g',$2,$3,$4,'text',$5,$6,'{}',now(),now())"
        >>,
        [
            uid(),
            iolist_to_binary(["c2g:", integer_to_binary(Gid)]),
            Seq,
            MsgId,
            FromUid,
            Gid
        ]
    ),
    ok.

bump_counter(Gid, To) ->
    {ok, _} = elib_pg:query(
        <<"INSERT INTO public.msg_store_seq (conv_key, seq) VALUES ($1, $2) ",
            "ON CONFLICT (conv_key) DO UPDATE SET seq = $2">>,
        [iolist_to_binary(["c2g:", integer_to_binary(Gid)]), To]
    ),
    ok.

gen_rows(Uid, Gid) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT generation_no, start_seq, end_seq FROM public.group_member_generation ",
            "WHERE group_id = $1 AND user_id = $2 ORDER BY generation_no">>,
        [Gid, Uid]
    ),
    [
        begin
            #{<<"generation_no">> := G, <<"start_seq">> := S, <<"end_seq">> := E} = Row,
            {G, S, E}
        end
     || Row <- Rows
    ].

history_seq_rows(Uid, Gid, AfterSeq) ->
    GidEnc = integer_to_binary(Gid),
    case messaging_logic:history(Uid, <<"c2g">>, GidEnc, AfterSeq, 50) of
        {ok, #{<<"messages">> := Msgs}} ->
            [maps:get(<<"conv_seq">>, M) || M <- Msgs];
        {error, _R, _C} ->
            denied
    end.

stage_room_key(Gid, FromUid, SenderDid, SessionId, MsgId) ->
    Payload = jsone:encode(#{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => #{
            <<"msg_type">> => <<"e2ee_room_key">>,
            <<"gid">> => Gid,
            <<"session_id">> => SessionId,
            <<"keys">> => []
        }
    }),
    msg_store_ds:stage(
        <<"c2g">>,
        MsgId,
        <<"e2ee_room_key">>,
        <<"e2ee_room_key">>,
        null,
        Payload,
        FromUid,
        Gid,
        ts(),
        ts(),
        SenderDid,
        1
    ).

stage_megolm(Gid, FromUid, SenderDid, SessionId, MsgId) ->
    E2EE = #{
        <<"meta_version">> => 3,
        <<"protocol_metadata">> => #{
            <<"protocol">> => <<"megolm">>,
            <<"gid">> => Gid,
            <<"session_id">> => SessionId
        }
    },
    Payload = jsone:encode(#{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => <<"ciphertext">>
    }),
    msg_store_ds:stage(
        <<"c2g">>,
        MsgId,
        <<"text">>,
        <<>>,
        E2EE,
        Payload,
        FromUid,
        Gid,
        ts(),
        ts(),
        SenderDid,
        1
    ).

session_range(Gid, SessionId) ->
    {ok, [#{<<"start_seq">> := StartSeq, <<"end_seq">> := EndSeq}]} = elib_pg:query(
        <<"SELECT start_seq, end_seq FROM public.e2ee_group_session_attestation ",
            "WHERE group_id = $1 AND session_id = $2">>,
        [Gid, SessionId]
    ),
    {StartSeq, EndSeq}.

assert_failed_stage_rolled_back(Gid, MsgId, ExpectedSeq) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT ",
            "(SELECT count(*) FROM public.msg_store_staging WHERE msg_id = $1) AS staged, ",
            "(SELECT count(*) FROM public.msg_c2g_request_ledger WHERE msg_id = $1) AS ledger, ",
            "(SELECT count(*) FROM public.e2ee_group_session_attestation ",
            " WHERE room_key_msg_id = $1) AS attested, ",
            "(SELECT seq FROM public.msg_store_seq WHERE conv_key = $2) AS seq">>,
        [MsgId, iolist_to_binary(["c2g:", integer_to_binary(Gid)])]
    ),
    ?assertEqual(
        #{<<"staged">> => 0, <<"ledger">> => 0, <<"attested">> => 0, <<"seq">> => ExpectedSeq},
        Row
    ).

assert_duplicate_stage_stable(Gid, MsgId, ExpectedSeq) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT ",
            "(SELECT count(*) FROM public.msg_store_staging WHERE msg_id = $1) AS staged, ",
            "(SELECT count(*) FROM public.msg_c2g_request_ledger WHERE msg_id = $1) AS ledger, ",
            "(SELECT seq FROM public.msg_store_seq WHERE conv_key = $2) AS seq">>,
        [MsgId, iolist_to_binary(["c2g:", integer_to_binary(Gid)])]
    ),
    ?assertEqual(
        #{<<"staged">> => 1, <<"ledger">> => 1, <<"seq">> => ExpectedSeq},
        Row
    ).

%% ===================================================================

%% F2：join 后 history(seq=0) 只返回 start_seq 及之后的归档
f2_history_clamped_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 500000 + (uid() rem 100000),
        Owner = 7001,
        A = 7002,
        mk_group(Gid, Owner),
        %% 入群前：计数器=1，归档 seq=1（pre-boundary 消息）
        bump_counter(Gid, 1),
        archive_row(Gid, 1, Owner),
        %% A 加入 → start_seq = 锁内计数器当前值 + 1 = 2
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        [{1, StartSeq, null}] = gen_rows(A, Gid),
        ?assertEqual(2, StartSeq),
        %% 入群后归档 seq=2
        bump_counter(Gid, 2),
        archive_row(Gid, 2, A),
        %% F2 断言：游标 0/1 都只见 seq=2；游标=2 已含 → 无增量
        ?assertEqual([2], history_seq_rows(A, Gid, 0)),
        ?assertEqual([2], history_seq_rows(A, Gid, 1)),
        ?assertEqual([], history_seq_rows(A, Gid, 2)),
        %% 非成员 deny
        B = 7003,
        ?assertEqual(denied, history_seq_rows(B, Gid, 0))
    end).

%% R2：leave 关世代 → 授权失效；rejoin 新世代 → 只见重入后
r2_rejoin_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 510000 + (uid() rem 100000),
        A = 7101,
        mk_group(Gid, A),
        bump_counter(Gid, 5),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        [{1, FirstStart, null}] = gen_rows(A, Gid),
        ?assertEqual(6, FirstStart),
        %% leave → 世代关闭：计数器未越过 start_seq-1 → 空区间 end_seq=5
        ok = group_member_logic:leave(A, Gid, A),
        [{1, FirstStart, EndSeq}] = gen_rows(A, Gid),
        ?assertEqual(5, EndSeq),
        ?assertEqual(denied, history_seq_rows(A, Gid, 0)),
        %% 离群期间分配 seq=6（成员看不到）
        bump_counter(Gid, 6),
        archive_row(Gid, 6, A),
        %% rejoin → 新世代 start_seq=计数器+1=7
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        [{1, _, EndSeq2}, {2, SecondStart, null}] = gen_rows(A, Gid),
        ?assertEqual(5, EndSeq2),
        ?assertEqual(7, SecondStart),
        %% 重入后分配 seq=7 → history 只见 7，不见离群期的 6 与更早
        bump_counter(Gid, 7),
        archive_row(Gid, 7, A),
        ?assertEqual([7], history_seq_rows(A, Gid, 0)),
        ?assertEqual([7], history_seq_rows(A, Gid, 6)),
        %% 游标=7 → 已含，无增量
        ?assertEqual([], history_seq_rows(A, Gid, 7))
    end).

%% staging 预分配：c2g 接受事务固化 conv_seq；重复 msg_id 保持原行原 seq
staging_prealloc_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 520000 + (uid() rem 100000),
        A = 7201,
        mk_group(Gid, A),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        MsgId = iolist_to_binary(["m-", integer_to_binary(uid())]),
        %% 生产 c2g payload 契约：group_id 在 <<"to">> 字段（字符串格式），
        %% msg_archive_repo 归档期据此解码（safe_decode_group_id）
        Payload = iolist_to_binary(["{\"to\":\"", integer_to_binary(Gid), "\"}"]),
        {ok, new, ConvSeq1, MemberUids} = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            Payload,
            A,
            Gid,
            ts(),
            ts(),
            <<>>
        ),
        ?assert(lists:member(A, MemberUids)),
        {ok, Rows} = elib_pg:query(
            <<"SELECT conv_seq FROM public.msg_store_staging WHERE msg_id = $1">>,
            [MsgId]
        ),
        [#{<<"conv_seq">> := StoredConvSeq}] = Rows,
        ?assertEqual(ConvSeq1, StoredConvSeq),
        ?assert(is_integer(ConvSeq1)),
        %% 重复投递（同 msg_id）：DS 契约归一化为 {ok, duplicate}（调用方据此
        %% 跳过投递管道）；事务回滚，计数器不空推，单行保持原 seq
        {ok, duplicate} = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            Payload,
            A,
            Gid,
            ts(),
            ts(),
            <<>>
        ),
        {ok, Counters} = elib_pg:query(
            <<"SELECT seq FROM public.msg_store_seq WHERE conv_key = $1">>,
            [iolist_to_binary(["c2g:", integer_to_binary(Gid)])]
        ),
        [#{<<"seq">> := ConvSeq1}] = Counters,
        {ok, Rows2} = elib_pg:query(
            <<"SELECT count(*) AS n, min(conv_seq) AS s FROM public.msg_store_staging WHERE msg_id = $1">>,
            [MsgId]
        ),
        [#{<<"n">> := 1, <<"s">> := ConvSeq1}] = Rows2,
        %% 走生产 worker 的 claim 查询合同，防 SELECT 漏列后被 SELECT * 测试掩盖。
        {ok, ClaimedRows} = msg_store_repo:claim_pending(1000, 30),
        [StRow] = [Row || #{<<"msg_id">> := Id} = Row <- ClaimedRows, Id =:= MsgId],
        ?assertEqual(ConvSeq1, maps:get(<<"conv_seq">>, StRow)),
        %% archive 必须搬运 claim 行里的既定 seq，绝不二次分配。
        ok = msg_archive_repo:archive(StRow),
        {ok, ArchRows} = elib_pg:query(
            <<"SELECT conv_seq FROM public.msg_store WHERE msg_id = $1">>, [MsgId]
        ),
        [#{<<"conv_seq">> := ArchSeq}] = ArchRows,
        ?assertEqual(ConvSeq1, ArchSeq)
    end).

%% fail-closed：成员行在但世代缺失 → deny（不回退 boolean membership）
fail_closed_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 530000 + (uid() rem 100000),
        A = 7301,
        mk_group(Gid, A),
        legacy_member(Gid, A),
        ?assertEqual(denied, history_seq_rows(A, Gid, 0))
    end).

%% 新建群的创建者也必须经统一入群入口建立首个 open generation。
new_group_owner_generation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 535000 + (uid() rem 100000),
        Owner = 7351,
        Gid = elib_pg:with_tx(fun(Conn) ->
            group_ds:create_group(Conn, Gid, Owner, ts(), 2, 1)
        end),
        ?assertEqual([{1, 1, null}], gen_rows(Owner, Gid)),
        ?assertEqual([], history_seq_rows(Owner, Gid, 0))
    end).

%% 解散群必须关闭全部 open generation；残留世代不得继续授权历史。
dissolved_group_closes_generation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 537000 + (uid() rem 100000),
        Owner = 7371,
        mk_group(Gid, Owner),
        ok = group_member_logic:join_group(<<"invite">>, Owner, Gid, #{role => 4}),
        [{1, 1, null}] = gen_rows(Owner, Gid),
        Group = group_ds:find_by_id(Gid, <<"*">>),
        ok = group_ds:dissolve_group(Owner, Gid, Owner, Group),
        [{1, 1, EndSeq}] = gen_rows(Owner, Gid),
        ?assert(is_integer(EndSeq)),
        ?assertEqual(denied, history_seq_rows(Owner, Gid, 0))
    end).

%% M1 backfill 规则：legacy active 成员经 backfill 得 start_seq=1
m1_backfill_rule_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 540000 + (uid() rem 100000),
        A = 7401,
        mk_group(Gid, A),
        legacy_member(Gid, A),
        %% 执行迁移文件中的 M1 回填片段（与 00000101 up 相同语句）
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.group_member_generation "
                "(group_id, user_id, generation_no, start_seq, end_seq, close_reason) "
                "SELECT gm.group_id, gm.user_id, 1, 1, NULL, NULL "
                "FROM public.group_member gm WHERE gm.status = 1 "
                "ON CONFLICT DO NOTHING"
            >>,
            []
        ),
        [{1, 1, null}] = gen_rows(A, Gid)
    end).

%% D3 server attestation：room-key 建立、PFv3 使用、成员集合变化与重入世代
%% 必须在同一 staging 顺序锁边界内闭合。
group_session_attestation_lifecycle_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 545000 + (uid() rem 100000),
        A = 7451,
        B = 7452,
        C = 7453,
        SessionId = iolist_to_binary(["session-", integer_to_binary(uid())]),
        SenderDid = <<"did-a">>,
        mk_group(Gid, A),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        ok = group_member_logic:join_group(<<"invite">>, B, Gid, #{}),

        RoomKeyMsgId = iolist_to_binary(["rk-", integer_to_binary(uid())]),
        {ok, new, RoomKeySeq, _} = stage_room_key(Gid, A, SenderDid, SessionId, RoomKeyMsgId),
        ?assertEqual(
            {ok, #{generation_no => 1, start_seq => RoomKeySeq, end_seq => RoomKeySeq}},
            group_ds:authorize_group_history(B, Gid, SessionId)
        ),

        ContentMsgId = iolist_to_binary(["m-", integer_to_binary(uid())]),
        {ok, new, ContentSeq, _} = stage_megolm(Gid, A, SenderDid, SessionId, ContentMsgId),
        ?assertEqual(
            {ok, #{generation_no => 1, start_seq => RoomKeySeq, end_seq => ContentSeq}},
            group_ds:authorize_group_history(B, Gid, SessionId)
        ),

        ok = group_member_logic:join_group(<<"invite">>, C, Gid, #{}),
        ?assertEqual(
            {error, e2ee_session_conflict},
            stage_megolm(
                Gid,
                A,
                SenderDid,
                SessionId,
                iolist_to_binary(["m-", integer_to_binary(uid())])
            )
        ),
        ?assertEqual({error, denied}, group_ds:authorize_group_history(C, Gid, SessionId)),

        ok = group_member_logic:leave(B, Gid, B),
        ok = group_member_logic:join_group(<<"invite">>, B, Gid, #{}),
        ?assertEqual({error, denied}, group_ds:authorize_group_history(B, Gid, SessionId))
    end).

%% D3 attestation 攻击矩阵：身份/session 冲突、重复与非单调 extend 都必须
%% 回滚 sequence、staging、request ledger 和 attestation 变更。
group_session_attestation_conflict_matrix_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 547000 + (uid() rem 100000),
        A = 7471,
        B = 7472,
        SessionId = iolist_to_binary(["session-", integer_to_binary(uid())]),
        UnknownSessionId = iolist_to_binary(["unknown-", integer_to_binary(uid())]),
        SenderDid = <<"did-a">>,
        mk_group(Gid, A),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        ok = group_member_logic:join_group(<<"invite">>, B, Gid, #{}),

        RoomKeyMsgId = iolist_to_binary(["rk-", integer_to_binary(uid())]),
        {ok, new, RoomKeySeq, _} = stage_room_key(Gid, A, SenderDid, SessionId, RoomKeyMsgId),
        ?assertEqual({RoomKeySeq, RoomKeySeq}, session_range(Gid, SessionId)),

        ConflictingRoomKeyMsgId = iolist_to_binary(["rk-conflict-", integer_to_binary(uid())]),
        ?assertEqual(
            {error, e2ee_session_conflict},
            stage_room_key(Gid, A, SenderDid, SessionId, ConflictingRoomKeyMsgId)
        ),
        assert_failed_stage_rolled_back(Gid, ConflictingRoomKeyMsgId, RoomKeySeq),

        UnknownMsgId = iolist_to_binary(["unknown-", integer_to_binary(uid())]),
        ?assertEqual(
            {error, e2ee_session_unattested},
            stage_megolm(Gid, A, SenderDid, UnknownSessionId, UnknownMsgId)
        ),
        assert_failed_stage_rolled_back(Gid, UnknownMsgId, RoomKeySeq),

        ContentMsgId = iolist_to_binary(["m-", integer_to_binary(uid())]),
        {ok, new, ContentSeq, _} = stage_megolm(Gid, A, SenderDid, SessionId, ContentMsgId),
        ?assertEqual({RoomKeySeq, ContentSeq}, session_range(Gid, SessionId)),

        SenderUidMsgId = iolist_to_binary(["sender-uid-", integer_to_binary(uid())]),
        ?assertEqual(
            {error, e2ee_session_conflict},
            stage_megolm(Gid, B, SenderDid, SessionId, SenderUidMsgId)
        ),
        assert_failed_stage_rolled_back(Gid, SenderUidMsgId, ContentSeq),

        SenderDidMsgId = iolist_to_binary(["sender-did-", integer_to_binary(uid())]),
        ?assertEqual(
            {error, e2ee_session_conflict},
            stage_megolm(Gid, A, <<"did-changed">>, SessionId, SenderDidMsgId)
        ),
        assert_failed_stage_rolled_back(Gid, SenderDidMsgId, ContentSeq),

        ?assertEqual(
            {ok, duplicate},
            stage_megolm(Gid, A, SenderDid, SessionId, ContentMsgId)
        ),
        ?assertEqual(ContentSeq, element(2, session_range(Gid, SessionId))),
        assert_duplicate_stage_stable(Gid, ContentMsgId, ContentSeq),

        bump_counter(Gid, ContentSeq - 1),
        NonMonotonicMsgId = iolist_to_binary(["non-monotonic-", integer_to_binary(uid())]),
        ?assertEqual(
            {error, e2ee_session_stale},
            stage_megolm(Gid, A, SenderDid, SessionId, NonMonotonicMsgId)
        ),
        assert_failed_stage_rolled_back(Gid, NonMonotonicMsgId, ContentSeq - 1),
        ?assertEqual({RoomKeySeq, ContentSeq}, session_range(Gid, SessionId)),
        bump_counter(Gid, ContentSeq)
    end).

%% ===================================================================
%% schema 自持守护（同 00000048 的 e2ee_offline_sender_did_tests 惯例）
%% ===================================================================

%% 全新安装执行迁移时 staging 表尚不存在（由 ensure_table_exists/0 运行时创建）；
%% conv_seq 的 ALTER 与 backlog 回填都必须守护表存在性，否则迁移标记 dirty 阻断首次启动。
migration_101_guards_absent_staging_test() ->
    {ok, Migration} =
        file:read_file("priv/migrations/00000101_group_history_join_boundary.up.sql"),
    ?assert(
        binary:match(Migration, <<"ALTER TABLE IF EXISTS public.msg_store_staging">>) =/= nomatch
    ),
    ?assert(binary:match(Migration, <<"ADD COLUMN IF NOT EXISTS conv_seq">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"to_regclass('public.msg_store_staging')">>) =/= nomatch).

%% 旧 timeline 没有可信接受序号，禁止按 created_at/ACK 猜测或回填世代。
migration_109_keeps_legacy_timeline_fail_closed_test() ->
    {ok, Migration} =
        file:read_file("priv/migrations/00000109_c2g_timeline_generation_boundary.up.sql"),
    ?assert(binary:match(Migration, <<"ADD COLUMN conv_seq bigint">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"conv_seq IS NULL OR conv_seq >= 1">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"client_ack = false AND conv_seq IS NOT NULL">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Migration, <<"UPDATE public.msg_c2g_timeline">>)).

migration_111_repairs_c2g_boundary_idempotently_test() ->
    {ok, Migration} =
        file:read_file("priv/migrations/00000111_c2g_request_recipient_boundary.up.sql"),
    ?assert(binary:match(Migration, <<"ADD COLUMN IF NOT EXISTS conv_seq bigint">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"msg_c2g_request_ledger">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"msg_c2g_recipient_snapshot">>) =/= nomatch).

migration_112_preserves_server_session_attestation_test() ->
    {ok, Up} =
        file:read_file("priv/migrations/00000112_e2ee_group_session_attestation.up.sql"),
    {ok, Down} =
        file:read_file("priv/migrations/00000112_e2ee_group_session_attestation.down.sql"),
    ?assert(binary:match(Up, <<"e2ee_group_session_attestation">>) =/= nomatch),
    ?assert(binary:match(Up, <<"e2ee_group_session_member">>) =/= nomatch),
    ?assert(binary:match(Up, <<"generation_no">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Down, <<"DROP TABLE">>)).

%% 全新安装与存量部署 schema 不得分叉：运行时 DDL 必须包含 conv_seq 列。
ensure_table_ddl_has_conv_seq_test() ->
    meck:new(elib_pg, [passthrough]),
    try
        meck:expect(elib_pg, execute, fun(Sql, _Params) ->
            Bin = iolist_to_binary(Sql),
            case binary:match(Bin, <<"CREATE TABLE IF NOT EXISTS">>) of
                nomatch -> ok;
                _ -> persistent_term:put({?MODULE, ddl_conv_seq}, Bin)
            end,
            {ok, 0}
        end),
        _ = msg_store_repo:ensure_table_exists(),
        Ddl = persistent_term:get({?MODULE, ddl_conv_seq}),
        ?assert(binary:match(Ddl, <<"conv_seq">>) =/= nomatch)
    after
        persistent_term:erase({?MODULE, ddl_conv_seq}),
        _ = (catch meck:unload(elib_pg))
    end.
