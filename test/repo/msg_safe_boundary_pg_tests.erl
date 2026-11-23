-module(msg_safe_boundary_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% R02 / D05（IMBOY-TIMEZONE-REPAIR-V1）：归档增量的安全推进边界。
%%%
%%% 真实 PG（隔离库）故障注入验证：
%%%   R02-1 seq=1 卡 staging（可重试）而 seq=2 已正式 → next_seq 不越过 B；
%%%         恢复 1 后最终两条齐全（断言最终消息集合，不是数字增大）
%%%   R02-2 worker lease 期间（processed_at 仍 NULL）B 不越过；lease 过期
%%%         重试成功后不丢低序号
%%%   R02-3 终态失败（mark_terminal）不永久阻塞：B 上升、gaps 报告
%%%         terminal_failure，客户端合同成立
%%%   R02-4 授权过滤（per_device DID 信封过滤）：被滤行不下发但 next_seq
%%%         按全量行推进且不越过 B；next_seq 只进不退
%%%   R02-5 修复前过冲游标经 history_backfill 有界恢复：跨窗口完整恢复、
%%%         空洞窗口推进、恢复位置可断言
%%%   R02-6 晚到归档后旧 history 游标（过冲）不再取到、必须走 backfill——
%%%         服务端提供显式补取接口，客户端绝不自行推断 gap
%%%===================================================================

-define(UA, 920001).
-define(UB, 920002).
-define(KEY, <<"c2c:920001:920002">>).
-define(PREFIX, <<"tzv1_r02_%">>).

%% ------------------------------------------------------------------
%% fixture
%% ------------------------------------------------------------------

cleanup() ->
    %% 迁移 255/256 起 staging 插入经触发器写 forward source fact 表；
    %% msg_id 固定，中断运行残留的事实行会撞唯一键——先清事实行。
    _ = elib_pg:execute(<<"DELETE FROM c2c_forward_source_fact WHERE msg_id LIKE $1">>, [
        ?PREFIX
    ]),
    _ = elib_pg:execute(<<"DELETE FROM c2g_forward_source_fact WHERE msg_id LIKE $1">>, [
        ?PREFIX
    ]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store WHERE msg_id LIKE $1">>, [?PREFIX]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_c2c WHERE msg_id LIKE $1">>, [?PREFIX]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store_staging WHERE msg_id LIKE $1">>, [?PREFIX]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>, [?KEY]),
    ok.

%% 接受（staging 落库 + 序号分配），非终态（processed_at NULL）。
%% N 仅决定 fixture 内的相对时间序；时间为 RFC3339 binary（timestamptz 编码器要求）。
accept(MsgId, N) ->
    Ts = elib_dt:to_rfc3339(1789600000000 + N * 1000, millisecond, "Z"),
    {ok, _Id, Seq} =
        msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"m\":1}">>,
            ?UA,
            ?UB,
            Ts,
            Ts,
            null,
            Ts,
            null
        ),
    Seq.

%% worker 归档：把 staging 行写入 msg_store（保留接受时分配的 conv_seq）
archive(MsgId) ->
    {ok, [Row]} =
        elib_pg:query(
            <<
                "SELECT type, msg_id, msg_type, from_id, to_id, e2ee, sender_did, payload, "
                "created_at, server_ts, conv_seq, accepted_at, expire_at "
                "FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1"
            >>,
            [MsgId]
        ),
    ok = msg_archive_repo:archive(Row),
    Seq = maps:get(<<"conv_seq">>, Row),
    {ok, _} = msg_store_repo:unstage(<<"c2c">>, MsgId),
    Seq.

%% 对方发出的接受（from=UB → to=UA；conv_key 不变，仍 c2c:min:max）
accept_rev(MsgId, N) ->
    Ts = elib_dt:to_rfc3339(1789600000000 + N * 1000, millisecond, "Z"),
    {ok, _Id, Seq} =
        msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"m\":1}">>,
            ?UB,
            ?UA,
            Ts,
            Ts,
            null,
            Ts,
            null
        ),
    Seq.

%% 真实 PG 故障注入：终态失败（mark_terminal 语义：processed_at 置位+错误保留）
inject_terminal(MsgId) ->
    {ok, _} =
        elib_pg:execute(
            <<
                "UPDATE public.msg_store_staging SET processed_at = NOW(), error_msg = 'fixture_terminal' "
                "WHERE type='c2c' AND msg_id=$1"
            >>,
            [MsgId]
        ),
    ok.

%% 真实 PG 故障注入：worker claim 租约（available_at 推后，processed_at 仍 NULL）
inject_lease(MsgId) ->
    {ok, _} =
        elib_pg:execute(
            <<
                "UPDATE public.msg_store_staging SET available_at = NOW() + INTERVAL '30 seconds' "
                "WHERE type='c2c' AND msg_id=$1"
            >>,
            [MsgId]
        ),
    ok.

history(AfterSeq) ->
    {ok, Payload} = messaging_logic:history(?UA, <<"c2c">>, <<"920002">>, AfterSeq, 50, <<>>),
    Payload.

ids(Payload) ->
    lists:sort([maps:get(<<"msg_id">>, M) || M <- maps:get(<<"messages">>, Payload)]).

%% ------------------------------------------------------------------
%% R02-1：1 失败 → 2 成功 → 同步 → 1 恢复后最终两条齐全
%% ------------------------------------------------------------------
r02_1_stuck_low_seq_never_skipped_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"tzv1_r02_a">>,
        MsgB = <<"tzv1_r02_b">>,
        ?assertEqual(1, accept(MsgA, 1)),
        %% 故障注入：seq=1 卡 staging（可重试，未归档）
        ?assertEqual(2, accept(MsgB, 2)),
        %% 故障继续：seq=2 归档成功，seq=1 仍卡
        ?assertEqual(2, archive(MsgB)),

        %% 阶段1：B 边界 = min(正式 max=2, staging 非终态 min=1 - 1) = 0
        {ok, B1} = msg_archive_repo:safe_head(?KEY, <<"c2c">>, ?UA, ?UB),
        ?assertEqual(0, B1),
        P1 = history(0),
        ?assertEqual(0, maps:get(<<"next_seq">>, P1)),
        ?assertEqual(true, maps:get(<<"blocked">>, P1)),
        %% 行在归档表中可见但游标被钳住——客户端不会把 next_seq 推到 2
        ?assertEqual([MsgB], ids(P1)),

        %% 阶段2：故障恢复，seq=1 晚到归档
        ?assertEqual(1, archive(MsgA)),
        {ok, B2} = msg_archive_repo:safe_head(?KEY, <<"c2c">>, ?UA, ?UB),
        ?assertEqual(2, B2),

        %% 阶段3：从 0 重新同步（游标被钳住未推进的客户端）最终两条齐全
        P3 = history(0),
        ?assertEqual(2, maps:get(<<"next_seq">>, P3)),
        ?assertEqual(false, maps:get(<<"blocked">>, P3)),
        ?assertEqual(lists:sort([MsgA, MsgB]), ids(P3)),

        %% 已在阶段1取到 MsgB 的客户端（修复后实现不会发生，防御性断言）：
        %% 过冲游标 history(2) 返回空、blocked=false，需走 backfill（R02-5 验证）
        P4 = history(2),
        ?assertEqual([], maps:get(<<"messages">>, P4))
    end).

%% ------------------------------------------------------------------
%% R02-2：并发 worker / lease / 重启不丢低序号
%% ------------------------------------------------------------------
r02_2_lease_does_not_release_boundary_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"tzv1_r02_la">>,
        MsgB = <<"tzv1_r02_lb">>,
        ?assertEqual(1, accept(MsgA, 1)),
        %% 故障注入：worker 已 claim seq=1（租约 30s，processed_at 仍 NULL）
        inject_lease(MsgA),
        ?assertEqual(2, accept(MsgB, 2)),
        ?assertEqual(2, archive(MsgB)),

        %% lease 持有期间：非终态（可重试）→ B 不越过
        {ok, B1} = msg_archive_repo:safe_head(?KEY, <<"c2c">>, ?UA, ?UB),
        ?assertEqual(0, B1),
        P1 = history(0),
        ?assertEqual(0, maps:get(<<"next_seq">>, P1)),

        %% 模拟重启后租约过期、重试成功：seq=1 归档
        {ok, _} =
            elib_pg:execute(
                <<
                    "UPDATE public.msg_store_staging SET available_at = NOW() - INTERVAL '1 second' "
                    "WHERE type='c2c' AND msg_id=$1"
                >>,
                [MsgA]
            ),
        ?assertEqual(1, archive(MsgA)),

        P2 = history(0),
        ?assertEqual(2, maps:get(<<"next_seq">>, P2)),
        ?assertEqual(lists:sort([MsgA, MsgB]), ids(P2))
    end).

%% ------------------------------------------------------------------
%% R02-3：终态失败与合法缺号不永久阻塞
%% ------------------------------------------------------------------
r02_3_terminal_failure_reported_not_blocking_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"tzv1_r02_ta">>,
        MsgB = <<"tzv1_r02_tb">>,
        ?assertEqual(1, accept(MsgA, 1)),
        %% 故障注入：seq=1 结构性不可恢复 → mark_terminal（processed_at 置位）
        inject_terminal(MsgA),
        ?assertEqual(2, accept(MsgB, 2)),
        ?assertEqual(2, archive(MsgB)),

        %% 终态失败行排除在阻塞集外：B = 正式表 max = 2
        {ok, B} = msg_archive_repo:safe_head(?KEY, <<"c2c">>, ?UA, ?UB),
        ?assertEqual(2, B),

        P = history(0),
        ?assertEqual(2, maps:get(<<"next_seq">>, P)),
        ?assertEqual(false, maps:get(<<"blocked">>, P)),
        %% 合法空洞显式报告（terminal_failure），客户端不自行推断 gap
        Gaps = maps:get(<<"gaps">>, P),
        ?assertMatch(
            [#{<<"from_seq">> := 1, <<"to_seq">> := 1, <<"reason">> := <<"terminal_failure">>}],
            Gaps
        ),
        %% 最终消息集合只含成功行，且同步合同成立（不卡死、可推进）
        ?assertEqual([MsgB], ids(P))
    end).

%% ------------------------------------------------------------------
%% R02-4：权限（DID 信封）过滤后 next_seq 合同成立
%% ------------------------------------------------------------------
r02_4_filtered_row_advances_cursor_within_safe_head_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"tzv1_r02_fa">>,
        MsgB = <<"tzv1_r02_fb">>,
        ?assertEqual(1, accept(MsgA, 1)),
        ?assertEqual(2, accept_rev(MsgB, 2)),
        ?assertEqual(1, archive(MsgA)),
        ?assertEqual(2, archive(MsgB)),
        %% MsgB 为对方发出的 per_device fan-out，信封无本机 DID（可交付性过滤）
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.msg_store SET e2ee = $2::jsonb WHERE msg_id=$1">>,
                [
                    MsgB,
                    <<"{\"fan_out\":\"per_device\",\"devices\":{\"other_did\":{}}}">>
                ]
            ),

        %% 携带本机 DID 的拉取：MsgB 不下发（不可交付），MsgA 下发
        {ok, P} = messaging_logic:history(?UA, <<"c2c">>, <<"920002">>, 0, 50, <<"d1">>),
        ?assertEqual([MsgA], ids(P)),
        %% next_seq 按全量行推进（滤行位于游标之后不会重取），
        %% 且不越过 B（无 staging 行 → B = 正式表 max = 2）、只进不退
        ?assertEqual(2, maps:get(<<"next_seq">>, P)),
        ?assertEqual(2, maps:get(<<"safe_head">>, P)),

        %% 游标只进不退：after_seq=5（过冲）时 next_seq 保持 5，不回退
        {ok, P2} = messaging_logic:history(?UA, <<"c2c">>, <<"920002">>, 5, 50, <<>>),
        ?assertEqual(5, maps:get(<<"next_seq">>, P2))
    end).

%% ------------------------------------------------------------------
%% R02-5：修复前过冲游标的有界补扫恢复
%% ------------------------------------------------------------------
r02_5_backfill_recovers_overshot_cursor_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgA = <<"tzv1_r02_ba">>,
        MsgB = <<"tzv1_r02_bb">>,
        MsgC = <<"tzv1_r02_bc">>,
        %% 修复前行为模拟：客户端已把 next_seq 推到 2（MsgA seq=1 卡住时
        %% 旧实现取 max 行 seq=2）
        ?assertEqual(1, accept(MsgA, 1)),
        ?assertEqual(2, accept(MsgB, 2)),
        ?assertEqual(2, archive(MsgB)),
        %% 故障恢复后：晚到归档 seq=1，再补一条 seq=3
        ?assertEqual(1, archive(MsgA)),
        ?assertEqual(3, accept(MsgC, 3)),
        ?assertEqual(3, archive(MsgC)),

        %% 过冲客户端 next_seq=2：history 只补到新行（seq=3）
        P1 = history(2),
        ?assertEqual([MsgC], ids(P1)),
        %% 有界补扫：从恢复下界 0 重扫 (0, 3]，晚到的 seq=1/2 全部找回
        {ok, BF1} =
            messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, 0, 50, <<>>),
        ?assertEqual(lists:sort([MsgA, MsgB, MsgC]), ids(BF1)),
        ?assertEqual(3, maps:get(<<"next_seq">>, BF1)),
        ?assertEqual(false, maps:get(<<"has_more">>, BF1)),

        %% 批次推进：limit=1 时恢复位置可断言（next_seq 逐批 1→2→3，终点=safe_head）
        {ok, BF2} =
            messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, 0, 1, <<>>),
        ?assertEqual(1, maps:get(<<"next_seq">>, BF2)),
        ?assertEqual(true, maps:get(<<"has_more">>, BF2)),
        {ok, BF3} =
            messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, 1, 1, <<>>),
        ?assertEqual(2, maps:get(<<"next_seq">>, BF3)),
        {ok, BF4} =
            messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, 2, 1, <<>>),
        ?assertEqual(3, maps:get(<<"next_seq">>, BF4)),
        ?assertEqual(false, maps:get(<<"has_more">>, BF4))
    end).

%% ------------------------------------------------------------------
%% R02-5b：跨窗口完整恢复（真实 PG 合成数据注入 1001 行）
%% ------------------------------------------------------------------
r02_5b_backfill_restores_full_history_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        try
            Sequences = lists:seq(1, 1001),
            Prefix = <<"tzv1_r02_rng_">>,
            seed_backfill(Sequences, Prefix, null),
            ?assertEqual({ok, 1001}, msg_archive_repo:safe_head(?KEY, <<"c2c">>, ?UA, ?UB)),
            Pages = collect_backfill(0, 1001, 25),
            Messages = lists:append([maps:get(<<"messages">>, Page) || Page <- Pages]),
            ?assertEqual(21, length(Pages)),
            ?assertEqual(Sequences, [maps:get(<<"conv_seq">>, Msg) || Msg <- Messages]),
            ?assertEqual(
                [<<Prefix/binary, (integer_to_binary(Seq))/binary>> || Seq <- Sequences],
                [maps:get(<<"msg_id">>, Msg) || Msg <- Messages]
            ),
            ?assertEqual(
                1001, length(lists:usort([maps:get(<<"msg_id">>, Msg) || Msg <- Messages]))
            ),
            ?assertMatch(#{<<"next_seq">> := 1000, <<"has_more">> := true}, lists:nth(20, Pages)),
            ?assertMatch(#{<<"next_seq">> := 1001, <<"has_more">> := false}, lists:last(Pages))
        after
            cleanup()
        end
    end).

r02_5c_backfill_advances_empty_windows_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        try
            seed_backfill([2001, 2500], <<"tzv1_r02_hole_">>, null),
            Pages = collect_backfill(0, 2500, 5),
            ?assertEqual([1000, 2000, 2500], [maps:get(<<"next_seq">>, Page) || Page <- Pages]),
            ?assertEqual([true, true, false], [maps:get(<<"has_more">>, Page) || Page <- Pages]),
            ?assertEqual(
                [[], [], [<<"tzv1_r02_hole_2001">>, <<"tzv1_r02_hole_2500">>]],
                [ids(Page) || Page <- Pages]
            ),
            {ok, Overshot} = messaging_logic:history_backfill(
                ?UA, <<"c2c">>, <<"920002">>, 3000, 50, <<>>
            ),
            ?assertMatch(
                #{<<"messages">> := [], <<"next_seq">> := 3000, <<"has_more">> := false}, Overshot
            )
        after
            cleanup()
        end
    end).

r02_accepted_at_response_rfc3339_pg_test_() ->
    [
        {
            atom_to_list(Action),
            ?TEST_WITH_DB(fun() ->
                cleanup(),
                try
                    AcceptedAt = <<"2026-03-08T05:00:02.345678Z">>,
                    seed_backfill([1], <<"tzv1_r02_wire_">>, AcceptedAt),
                    {ok, Payload} = history_payload(Action),
                    [Message] = maps:get(<<"messages">>, Payload),
                    ?assertEqual(
                        elib_dt:rfc3339_to(AcceptedAt, microsecond),
                        elib_dt:rfc3339_to(maps:get(<<"accepted_at">>, Message), microsecond)
                    ),
                    assert_response_timestamps(Action, 1772946002346)
                after
                    cleanup()
                end
            end)
        }
     || Action <- [history, history_backfill]
    ].

r02_legacy_accepted_at_response_null_pg_test_() ->
    [
        {
            atom_to_list(Action),
            ?TEST_WITH_DB(fun() ->
                cleanup(),
                try
                    seed_backfill([1], <<"tzv1_r02_legacy_">>, null),
                    [Message] = maps:get(<<"messages">>, response_payload(Action)),
                    ?assertEqual(null, maps:get(<<"accepted_at">>, Message))
                after
                    cleanup()
                end
            end)
        }
     || Action <- [history, history_backfill]
    ].

history_payload(history) ->
    messaging_logic:history(?UA, <<"c2c">>, <<"920002">>, 0, 50, <<>>);
history_payload(history_backfill) ->
    messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, 0, 50, <<>>).

seed_backfill(Sequences, Prefix, AcceptedAt) ->
    {ok, Count} = elib_pg:execute(
        <<
            "INSERT INTO public.msg_store "
            "(id, msg_id, chat_type, conv_key, conv_seq, msg_type, from_id, to_id, "
            "payload, created_at, server_ts, accepted_at) "
            "SELECT 941000000000000 + seq, $4::text || seq::text, 'c2c', $1, seq, 'text', $2, $3, "
            "'{}', '2026-03-08T05:00:00.123456Z'::timestamptz, "
            "'2026-03-08T05:00:01.234567Z'::timestamptz, $6::text::timestamptz "
            "FROM unnest($5::bigint[]) AS seq"
        >>,
        [?KEY, ?UA, ?UB, Prefix, Sequences, AcceptedAt]
    ),
    ?assertEqual(length(Sequences), Count).

collect_backfill(AfterSeq, SafeHead, Remaining) when Remaining > 0 ->
    {ok, Page} = messaging_logic:history_backfill(?UA, <<"c2c">>, <<"920002">>, AfterSeq, 50, <<>>),
    NextSeq = maps:get(<<"next_seq">>, Page),
    Messages = maps:get(<<"messages">>, Page),
    ?assertEqual(1000, maps:get(<<"range_limit">>, Page)),
    ?assert(length(Messages) =< 50),
    ?assert(NextSeq > AfterSeq),
    ?assert(NextSeq =< min(SafeHead, AfterSeq + 1000)),
    ?assert(
        lists:all(
            fun(Msg) ->
                Seq = maps:get(<<"conv_seq">>, Msg),
                Seq > AfterSeq andalso Seq =< NextSeq
            end,
            Messages
        )
    ),
    ?assertEqual(NextSeq < SafeHead, maps:get(<<"has_more">>, Page)),
    case maps:get(<<"has_more">>, Page) of
        true -> [Page | collect_backfill(NextSeq, SafeHead, Remaining - 1)];
        false -> [Page]
    end;
collect_backfill(AfterSeq, SafeHead, 0) ->
    erlang:error({backfill_did_not_finish, AfterSeq, SafeHead}).

assert_response_timestamps(Action, AcceptedMs) ->
    [Message] = maps:get(<<"messages">>, response_payload(Action)),
    lists:foreach(
        fun({Key, Expected}) ->
            Value = maps:get(Key, Message),
            ?assertEqual({Key, Expected}, {Key, Value})
        end,
        [
            {<<"accepted_at">>, AcceptedMs},
            {<<"created_at">>, 1772946000123},
            {<<"server_ts">>, 1772946001235}
        ]
    ).

response_payload(Action) ->
    StreamId = make_ref(),
    Req = #{
        pid => self(),
        streamid => StreamId,
        method => <<"GET">>,
        qs => <<"chat_type=c2c&peer_id=920002&after_seq=0&limit=50">>
    },
    {ok, _, _} = msg_handler:init(Req, #{action => Action, current_uid => ?UA}),
    receive
        {{_, StreamId}, {response, 200, _, Body}} ->
            #{<<"code">> := 0, <<"payload">> := Response} = jsone:decode(Body),
            Response
    after 1000 ->
        erlang:error(response_not_emitted)
    end.

%% ===================================================================
%% BE-02（IMBOY_MASTER_V2_2_20261118）补齐断言：成员世代历史边界、
%% 重入新世代、同时间戳多页与重启重同步。真库（隔离 marker 库）。
%% 现有实现（E2EE-2026-012/LT-03 join boundary）语义验证；不修生产码。
%% ===================================================================

-define(C2GGID, 930001).
-define(C2GKEY, <<"c2g:930001">>).

c2g_cleanup() ->
    _ = elib_pg:execute(
        <<"DELETE FROM public.group_member_generation WHERE group_id = $1">>, [?C2GGID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.group_member WHERE group_id = $1">>, [?C2GGID]
    ),
    _ = elib_pg:execute(<<"DELETE FROM public.\"group\" WHERE id = $1">>, [?C2GGID]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store WHERE conv_key = $1">>, [?C2GKEY]),
    _ = elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>, [?C2GKEY]),
    %% BE-05：群动作事实与 staging 清理（触发器捕获行不随成员清理消失）
    _ = elib_pg:execute(
        <<"DELETE FROM public.c2g_revoke_fact WHERE group_id = $1">>, [?C2GGID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.c2g_forward_edit_fact WHERE group_id = $1">>, [?C2GGID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_staging WHERE type = 'c2g' AND to_id = $1">>, [?C2GGID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_staging WHERE type = 'c2g' AND to_id = $1">>, [?C2GGID]
    ),
    %% 共享库 imboy_test_v1：请求台账 UNIQUE(msg_id) 跨 run 残留会使重放
    %% 被判 msg_id_conflict（hash 含 payload），必须随本套件清理。
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_c2g_request_ledger WHERE to_gid = $1">>, [?C2GGID]
    ),
    ok.

c2g_group_fixture() ->
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, title, status, scope) "
                "VALUES ($1, $2, $2, 'be02-r02-c2g', 1, 'personal') "
                "ON CONFLICT (id) DO UPDATE SET status = 1, scope = 'personal'"
            >>,
            [?C2GGID, ?UA]
        ),
    ok = c2g_member_up(?UA, 1),
    %% BE-05 动作卡需要 caller.role >= RequiredRole（撤回 1 / 编辑 3）
    {ok, _} =
        elib_pg:execute(
            <<"UPDATE public.group_member SET role = 3 WHERE group_id=$1 AND user_id=$2">>,
            [?C2GGID, ?UA]
        ),
    ok = c2g_generation_open(?UA, 1, 1).

c2g_member_up(Uid, Status) ->
    %% group_member.id 为 NOT NULL 且无默认（生产经 repo 生成 TSID）；
    %% fixture 用确定性整数 id（谓词查询只依赖 group_id/user_id/status）。
    MemberId = 93000000 + Uid,
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO public.group_member (id, group_id, user_id, status, role) "
                "VALUES ($4, $1, $2, $3, 0) "
                "ON CONFLICT (group_id, user_id) DO UPDATE SET status = $3, id = $4"
            >>,
            [?C2GGID, Uid, Status, MemberId]
        ),
    ok.

%% 关闭该成员当前 open 世代并新开一代（重入语义：R2 每次重入新世代）
c2g_generation_open(Uid, GenNo, StartSeq) ->
    {ok, _} =
        elib_pg:execute(
            <<
                "UPDATE public.group_member_generation SET end_seq = "
                " GREATEST((SELECT COALESCE(MAX(seq), 0) FROM public.msg_store_seq WHERE conv_key = $3), start_seq - 1), "
                " close_reason = 'fixture_rejoin' "
                "WHERE group_id = $1 AND user_id = $2 AND end_seq IS NULL"
            >>,
            [?C2GGID, Uid, ?C2GKEY]
        ),
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO public.group_member_generation "
                "(group_id, user_id, generation_no, start_seq, end_seq) "
                "VALUES ($1, $2, $3, $4, NULL)"
            >>,
            [?C2GGID, Uid, GenNo, StartSeq]
        ),
    ok.

c2g_seed_seq(Seq) ->
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO public.msg_store_seq (conv_key, seq) VALUES ($1, $2) "
                "ON CONFLICT (conv_key) DO UPDATE SET seq = GREATEST(msg_store_seq.seq, $2)"
            >>,
            [?C2GKEY, Seq]
        ),
    ok.

c2g_archive(MsgId, FromId, Seq, N) ->
    Ts = elib_dt:to_rfc3339(1789700000000 + N * 1000, millisecond, "Z"),
    {ok, _} =
        elib_pg:execute(
            <<
                "INSERT INTO public.msg_store "
                "(id, chat_type, conv_key, conv_seq, msg_id, msg_type, from_id, to_id, "
                "group_id, payload, created_at, server_ts) "
                "VALUES ($7, 'c2g', $6, $5, $1, 'text', $2, $3, $3, '{\"m\":1}', "
                " $4, $4) "
            >>,
            [MsgId, FromId, ?C2GGID, Ts, Seq, ?C2GKEY, 93100000 + Seq]
        ),
    ok = c2g_seed_seq(Seq),
    Seq.

c2g_sync(Uid, CursorSeq, Limit) ->
    Cursors = [#{<<"conv_key">> => ?C2GKEY, <<"seq">> => CursorSeq}],
    #{<<"results">> := Results} = msg_c2s_logic:handle_sync(Uid, Cursors, Limit),
    case Results of
        [] ->
            #{messages => [], next_seq => CursorSeq};
        [R] ->
            #{
                messages => lists:sort([
                    maps:get(<<"msg_id">>, M)
                 || M <- maps:get(<<"messages">>, R)
                ]),
                next_seq => maps:get(<<"next_seq">>, R)
            }
    end.

%% R02-7：后来成员不获入群前历史（F2 语义），老成员仍见全量
r02_7_c2g_late_member_no_prejoin_history_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        lists:foreach(
            fun(N) -> c2g_archive(<<"tzv1_r02_g", (integer_to_binary(N))/binary>>, ?UA, N, N) end,
            lists:seq(1, 3)
        ),
        %% B（迟到成员）入群：join boundary = 当前计数器+1 = 4
        ok = c2g_member_up(?UB, 1),
        ok = c2g_generation_open(?UB, 1, 4),
        {ok, #{start_seq := StartSeq}} = group_ds:authorize_group_history(?UB, ?C2GGID),
        ?assertEqual(4, StartSeq),
        %% 老成员全量
        #{messages := OwnerMsgs} = c2g_sync(?UA, 0, 50),
        ?assertEqual(
            lists:sort([<<"tzv1_r02_g1">>, <<"tzv1_r02_g2">>, <<"tzv1_r02_g3">>]),
            OwnerMsgs
        ),
        %% B 游标 0 被 clamp 到 start_seq-1=3：入群前 3 条不可见
        #{messages := LateMsgs, next_seq := NextSeq} = c2g_sync(?UB, 0, 50),
        ?assertEqual([], LateMsgs),
        %% 空窗口会话被过滤：next_seq 不下发，客户端保持自身游标（clamp
        %% 语义已由上方 authorize start_seq=4 断言钉住——查询侧从 seq>3 起）
        ?assertEqual(0, NextSeq),
        %% 入群后新消息 seq4：B 可见；仍不可见 seq1..3
        _ = c2g_archive(<<"tzv1_r02_g4">>, ?UA, 4, 4),
        #{messages := LateMsgs2, next_seq := NextSeq2} = c2g_sync(?UB, 0, 50),
        ?assertEqual([<<"tzv1_r02_g4">>], LateMsgs2),
        ?assertEqual(4, NextSeq2)
    end).

%% R02-8：退出后 fail-closed；重入新世代只获新窗口（无旧权限继承/游标串用）
r02_8_c2g_rejoin_new_generation_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        lists:foreach(
            fun(N) -> c2g_archive(<<"tzv1_r02_j", (integer_to_binary(N))/binary>>, ?UA, N, N) end,
            lists:seq(1, 6)
        ),
        ok = c2g_member_up(?UB, 1),
        ok = c2g_generation_open(?UB, 1, 4),
        %% 第一代内可见 g4..g6
        #{messages := Gen1} = c2g_sync(?UB, 0, 50),
        ?assertEqual(
            lists:sort([<<"tzv1_r02_j4">>, <<"tzv1_r02_j5">>, <<"tzv1_r02_j6">>]),
            Gen1
        ),
        %% 退出：成员行 status=0 + 世代关闭 → fail-closed 无结果
        ok = c2g_member_up(?UB, 0),
        {ok, _} =
            elib_pg:execute(
                <<
                    "UPDATE public.group_member_generation SET end_seq = 6, close_reason = 'fixture_leave' "
                    "WHERE group_id = $1 AND user_id = $2 AND end_seq IS NULL"
                >>,
                [?C2GGID, ?UB]
            ),
        #{messages := AfterLeave} = c2g_sync(?UB, 0, 50),
        ?assertEqual([], AfterLeave),
        %% 期间新消息 seq7,8
        lists:foreach(
            fun(N) -> c2g_archive(<<"tzv1_r02_j", (integer_to_binary(N))/binary>>, ?UA, N, N) end,
            lists:seq(7, 8)
        ),
        %% 重入：新世代 gen2 start_seq=7（R2 每次重入新世代）→ 不获 gen1 窗口
        ok = c2g_member_up(?UB, 1),
        ok = c2g_generation_open(?UB, 2, 7),
        {ok, #{generation_no := GenNo, start_seq := StartSeq2}} =
            group_ds:authorize_group_history(?UB, ?C2GGID),
        ?assertEqual(2, GenNo),
        ?assertEqual(7, StartSeq2),
        #{messages := Gen2, next_seq := NextSeq2} = c2g_sync(?UB, 0, 50),
        ?assertEqual(
            lists:sort([<<"tzv1_r02_j7">>, <<"tzv1_r02_j8">>]),
            Gen2
        ),
        ?assertEqual(8, NextSeq2),
        %% B 携带第一代旧游标（如 6）也不越权：clamp 后仍从 7 起
        #{messages := WithStaleCursor} = c2g_sync(?UB, 6, 50),
        ?assertEqual(
            lists:sort([<<"tzv1_r02_j7">>, <<"tzv1_r02_j8">>]),
            WithStaleCursor
        )
    end).

%% R02-9：同时间戳多页完整、重启重同步一致、空页不推进不循环
r02_9_same_timestamp_pagination_restart_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup(),
        MsgIds = [<<"tzv1_r02_s", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 4)],
        %% 四条同 server_ts（仅 msg_id/seq 不同）
        SameTs = elib_dt:to_rfc3339(1789600000000, millisecond, "Z"),
        lists:foldl(
            fun(MsgId, Idx) ->
                {ok, _Id, Seq} =
                    msg_store_repo:stage_with_sequence(
                        <<"c2c">>,
                        MsgId,
                        <<"text">>,
                        <<>>,
                        null,
                        <<"{\"m\":1}">>,
                        ?UA,
                        ?UB,
                        SameTs,
                        SameTs,
                        null,
                        SameTs,
                        null
                    ),
                ?assertEqual(Idx, Seq),
                ?assertEqual(Idx, archive(MsgId)),
                Idx + 1
            end,
            1,
            MsgIds
        ),
        %% 同时间戳整段一次取全（next_seq=4）
        P1 = history(0),
        ?assertEqual(lists:sort(MsgIds), ids(P1)),
        ?assertEqual(4, maps:get(<<"next_seq">>, P1)),
        %% 模拟重启：全新进程/连接重放同一游标 → 集合一致、游标不回退
        P2 = history(0),
        ?assertEqual(ids(P1), ids(P2)),
        %% 逐条翻页（limit=1，同时间戳不跳不重）：handle_sync 语义
        Walk = lists:foldl(
            fun(Idx, {Seen, Cursor}) ->
                Cursors = [#{<<"conv_key">> => ?KEY, <<"seq">> => Cursor}],
                #{<<"results">> := [R]} = msg_c2s_logic:handle_sync(?UA, Cursors, 1),
                [M] = maps:get(<<"messages">>, R),
                MsgId = maps:get(<<"msg_id">>, M),
                ?assert(maps:get(<<"msg_id">>, M) =/= <<>>),
                Next = maps:get(<<"next_seq">>, R),
                ?assertEqual(Idx, Next),
                ?assertNot(lists:member(MsgId, Seen)),
                {Seen ++ [MsgId], Next}
            end,
            {[], 0},
            lists:seq(1, 4)
        ),
        {SeenAll, _} = Walk,
        ?assertEqual(lists:sort(MsgIds), lists:sort(SeenAll)),
        %% 空页：cursor=4（头部）→ 结果过滤，无伪推进、无循环入口
        CursorsEnd = [#{<<"conv_key">> => ?KEY, <<"seq">> => 4}],
        #{<<"results">> := EndResults} = msg_c2s_logic:handle_sync(?UA, CursorsEnd, 1),
        ?assertEqual([], EndResults)
    end).

%% ===================================================================
%% BE-05（IMBOY_MASTER_V2_2_20261118）：群控制事件与成员世代专项重验。
%% 真库（隔离 marker 库）；生产入口（stage_with_sequence/stage_action/
%% safe_head/messaging_logic:history/authorize_group_history）全链验证。
%% ===================================================================

%% c2g staging 归档（镜像 archive/1，type='c2g'）
c2g_archive_row(MsgId) ->
    {ok, [Row]} =
        elib_pg:query(
            <<
                "SELECT type, msg_id, msg_type, from_id, to_id, e2ee, sender_did, payload, "
                "created_at, server_ts, conv_seq, accepted_at, expire_at "
                "FROM public.msg_store_staging WHERE type='c2g' AND msg_id=$1"
            >>,
            [MsgId]
        ),
    ok = msg_archive_repo:archive(Row),
    Seq = maps:get(<<"conv_seq">>, Row),
    {ok, _} = msg_store_repo:unstage(<<"c2g">>, MsgId),
    Seq.

%% c2g 普通发送的生产接受入口（seq 由 msg_store_seq 分配器权威分配；
%% stage_c2g 同事务固化 recipient snapshot——动作卡的 target 谓词依赖它）。
%% payload 携带 to：msg_archive_repo c2g 归档解码 group_id 的合同字段。
c2g_stage(MsgId) ->
    Ts = elib_dt:to_rfc3339(
        1789800000000 + erlang:unique_integer([positive]) rem 1000000,
        millisecond,
        "Z"
    ),
    Payload = <<"{\"m\":1,\"to\":", (integer_to_binary(?C2GGID))/binary, "}">>,
    Res =
        msg_store_repo:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            Payload,
            ?UA,
            ?C2GGID,
            Ts,
            Ts,
            <<>>,
            3,
            null
        ),
    case Res of
        {ok, _Id, Seq, _MemberUids} when is_integer(Seq) ->
            Seq;
        _ ->
            erlang:error({be05_stage_unexpected, MsgId, Res})
    end.

%% 共享库 imboy_test_v1 跨 run 残留（msg_c2g_request_ledger UNIQUE(msg_id)+
%% request_hash 含 payload）：本卡全部消息 id 用 run 内唯一后缀，杜绝
%% 跨 run 的 msg_id_conflict / unique_violation 假失败。
be05_uq() ->
    integer_to_binary(erlang:unique_integer([positive])).

%% BE-05:LOCAL:01 同会话并发发送、回滚及乱序：seq 分配器权威位置稳定；
%% 接受后未归档（in-flight）形成的安全边界使读取进度不越过未完成范围。
be05_01_c2g_seq_authority_hole_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        %% 生产分配器：连续接受 seq 严格递增（并发/乱序到达不改权威位置）
        Uq = be05_uq(),
        S1 = c2g_stage(<<"be05_seq1_", Uq/binary>>),
        S2 = c2g_stage(<<"be05_seq2_", Uq/binary>>),
        S3 = c2g_stage(<<"be05_seq3_", Uq/binary>>),
        ?assertEqual([1, 2, 3], [S1, S2, S3]),
        %% o1/o2 归档；o3 卡 staging（回滚/重试中）；o4 已分配且归档
        ?assertEqual(1, c2g_archive_row(<<"be05_seq1_", Uq/binary>>)),
        ?assertEqual(2, c2g_archive_row(<<"be05_seq2_", Uq/binary>>)),
        S4 = c2g_stage(<<"be05_seq4_", Uq/binary>>),
        ?assertEqual(4, S4),
        ?assertEqual(4, c2g_archive_row(<<"be05_seq4_", Uq/binary>>)),
        %% 安全边界 = min(正式 max=4, staging 非终态 min=3 - 1) = 2
        {ok, B} = msg_archive_repo:safe_head(?C2GKEY, <<"c2g">>, ?C2GGID, 0),
        ?assertEqual(2, B),
        %% 一致快照历史：安全边界 B=2；行查询含 >B 的已归档行（1,2,4），
        %% 但消费方 next_seq 被钳在 B——读取进度不越过未完成范围
        {ok, Rows, SafeHead, Gaps} =
            msg_archive_repo:safe_history(?C2GKEY, 0, 50, <<"c2g">>, ?C2GGID, 0),
        ?assertEqual(2, SafeHead),
        ?assertEqual([], Gaps),
        ?assertEqual(
            [
                <<"be05_seq1_", Uq/binary>>,
                <<"be05_seq2_", Uq/binary>>,
                <<"be05_seq4_", Uq/binary>>
            ],
            [maps:get(<<"msg_id">>, R) || R <- Rows]
        ),
        %% 消费面（messaging_logic:history 同源谓词）：next_seq 钳在 2
        {ok, HistP} = messaging_logic:history(?UA, <<"c2g">>, <<"930001">>, 0, 50, <<>>),
        ?assertEqual(2, maps:get(<<"next_seq">>, HistP)),
        ?assertEqual(true, maps:get(<<"blocked">>, HistP)),
        %% 恢复：o3 归档后边界推进、全量齐、blocked 解除
        ?assertEqual(3, c2g_archive_row(<<"be05_seq3_", Uq/binary>>)),
        {ok, Rows2, SafeHead2, _} =
            msg_archive_repo:safe_history(?C2GKEY, 0, 50, <<"c2g">>, ?C2GGID, 0),
        ?assertEqual(4, SafeHead2),
        ?assertEqual(
            [
                <<"be05_seq1_", Uq/binary>>,
                <<"be05_seq2_", Uq/binary>>,
                <<"be05_seq3_", Uq/binary>>,
                <<"be05_seq4_", Uq/binary>>
            ],
            [maps:get(<<"msg_id">>, R) || R <- Rows2]
        ),
        {ok, HistP2} = messaging_logic:history(?UA, <<"c2g">>, <<"930001">>, 0, 50, <<>>),
        ?assertEqual(4, maps:get(<<"next_seq">>, HistP2)),
        ?assertEqual(false, maps:get(<<"blocked">>, HistP2))
    end).

%% BE-05:LOCAL:02 入群/退出/踢人/转让/解散：读写权限一致，
%% 后来成员不获未授权历史；路径不影响其他成员的授权状态。
be05_02_membership_transitions_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        UC = 920003,
        lists:foreach(
            fun(N) -> c2g_archive(<<"be05_mt", (integer_to_binary(N))/binary>>, ?UA, N, N) end,
            lists:seq(1, 3)
        ),
        %% B、C 先后入群（join boundary = 4）
        ok = c2g_member_up(?UB, 1),
        ok = c2g_generation_open(?UB, 1, 4),
        ok = c2g_member_up(UC, 1),
        ok = c2g_generation_open(UC, 1, 4),
        ?assertMatch({ok, #{start_seq := 4}}, group_ds:authorize_group_history(?UB, ?C2GGID)),
        %% 转让：owner A→B、角色互换——世代边界与历史授权不变（后来成员
        %% 不因角色变化获得更早历史，原 owner 不丢失全量）
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.\"group\" SET owner_uid = $1 WHERE id = $2">>, [?UB, ?C2GGID]
            ),
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.group_member SET role = 2 WHERE group_id=$1 AND user_id=$2">>,
                [?C2GGID, ?UB]
            ),
        ?assertMatch({ok, #{start_seq := 4}}, group_ds:authorize_group_history(?UB, ?C2GGID)),
        ?assertMatch({ok, #{start_seq := 1}}, group_ds:authorize_group_history(?UA, ?C2GGID)),
        %% B 退出：status→0 + 世代关闭 → 授权 fail-closed denied
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.group_member SET status = 0 WHERE group_id=$1 AND user_id=$2">>,
                [?C2GGID, ?UB]
            ),
        {ok, _} =
            elib_pg:execute(
                <<
                    "UPDATE public.group_member_generation SET end_seq = 999, "
                    "close_reason = 'be05_quit' WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL"
                >>,
                [?C2GGID, ?UB]
            ),
        ?assertEqual({error, denied}, group_ds:authorize_group_history(?UB, ?C2GGID)),
        %% C 被踢：status→-1 + 世代关闭 → denied；A 不受影响（不误删他人授权）
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.group_member SET status = -1 WHERE group_id=$1 AND user_id=$2">>,
                [?C2GGID, UC]
            ),
        {ok, _} =
            elib_pg:execute(
                <<
                    "UPDATE public.group_member_generation SET end_seq = 999, "
                    "close_reason = 'be05_kick' WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL"
                >>,
                [?C2GGID, UC]
            ),
        ?assertEqual({error, denied}, group_ds:authorize_group_history(UC, ?C2GGID)),
        {ok, #{start_seq := 1}} = group_ds:authorize_group_history(?UA, ?C2GGID),
        %% 解散：group.status→0 → 全体 denied（fail-closed）
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.\"group\" SET status = 0 WHERE id = $1">>, [?C2GGID]
            ),
        ?assertEqual({error, denied}, group_ds:authorize_group_history(?UA, ?C2GGID)),
        %% 重入语义复核：复活群 + C 重入新世代 → 只见入群后内容
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.\"group\" SET status = 1 WHERE id = $1">>, [?C2GGID]
            ),
        {ok, _} =
            elib_pg:execute(
                <<"UPDATE public.group_member SET status = 1 WHERE group_id=$1 AND user_id=$2">>,
                [?C2GGID, UC]
            ),
        ok = c2g_generation_open(UC, 2, 4),
        {ok, #{start_seq := 4}} = group_ds:authorize_group_history(UC, ?C2GGID),
        #{messages := CMs} = c2g_sync(UC, 0, 50),
        ?assertEqual([], CMs)
    end).

%% 群编辑/撤回动作的 staging 生产入口（触发器在同 INSERT 内捕获事实）
c2g_stage_action(ActionMsgId, Action, PayloadMap, RequiredRole, OriginalMsgId) ->
    Ts = elib_dt:to_rfc3339(
        1789900000000 + erlang:unique_integer([positive]) rem 1000000,
        millisecond,
        "Z"
    ),
    msg_store_repo:stage_action(
        <<"c2g">>,
        ActionMsgId,
        <<"custom">>,
        Action,
        null,
        PayloadMap,
        ?UA,
        ?C2GGID,
        Ts,
        Ts,
        <<>>,
        RequiredRole,
        OriginalMsgId
    ).

scalar_first(Sql, Params, Key) ->
    {ok, [Row]} = elib_pg:query(Sql, Params),
    maps:get(Key, Row).

%% BE-05:LOCAL:03 两次编辑反序/并发/旧设备重放（群域）：受理序 = 事实序
%% 权威；同 action 重放 = 唯一冲突显式拒绝；编辑事实不重写正文。
be05_03_c2g_edit_facts_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        Uq3 = be05_uq(),
        Orig = <<"be05_ef_orig_", Uq3/binary>>,
        %% original 走生产链（stage_c2g 同事务固化 recipient snapshot——动作 target 谓词依赖）
        _SeqOrig = c2g_stage(Orig),
        _ = c2g_archive_row(Orig),
        %% 编辑 E1（先受理）：staging INSERT 同步触发事实捕获
        P1 = #{
            <<"action">> => <<"message_edit_ack">>,
            <<"content">> => <<"v2">>,
            <<"original_msg_id">> => Orig
        },
        {ok, _, Seq1, _} = c2g_stage_action(
            <<"be05_ef_e1_", Uq3/binary>>, <<"message_edit_ack">>, P1, 3, Orig
        ),
        ?assert(is_integer(Seq1)),
        ?assertEqual(
            1,
            scalar_first(
                <<
                    "SELECT count(*)::bigint AS v FROM public.c2g_forward_edit_fact "
                    "WHERE group_id=$1 AND original_msg_id=$2 AND action_msg_id = $3"
                >>,
                [?C2GGID, Orig, <<"be05_ef_e1_", Uq3/binary>>],
                <<"v">>
            )
        ),
        %% 编辑事实不重写正文（00000258 设计）：正式行 payload 保持原文
        %% 编辑事实不重写正文：原行不含任何编辑痕迹（content/original_msg_id 均未出现）
        NoEditPayload = scalar_first(
            <<"SELECT payload::text AS v FROM public.msg_store WHERE msg_id=$1">>,
            [Orig],
            <<"v">>
        ),
        ?assertEqual(nomatch, binary:match(NoEditPayload, <<"v2">>)),
        ?assertEqual(nomatch, binary:match(NoEditPayload, <<"v3">>)),
        ?assertEqual(nomatch, binary:match(NoEditPayload, <<"original_msg_id">>)),
        %% 旧设备重放：同 action_msg_id 再投 → UNIQUE(action_msg_id) 冲突显式拒绝
        ?assertMatch(
            {error, {unique_violation, <<"be05_ef_e1_", Uq3/binary>>}},
            c2g_stage_action(<<"be05_ef_e1_", Uq3/binary>>, <<"message_edit_ack">>, P1, 3, Orig)
        ),
        %% 后受理 E2（反序到达）：事实行 accepted_seq 严格大于 E1——受理序可恢复
        P2 = #{
            <<"action">> => <<"message_edit_ack">>,
            <<"content">> => <<"v3">>,
            <<"original_msg_id">> => Orig
        },
        {ok, _, Seq2, _} = c2g_stage_action(
            <<"be05_ef_e2_", Uq3/binary>>, <<"message_edit_ack">>, P2, 3, Orig
        ),
        ?assert(Seq2 > Seq1),
        {ok, Rows} = elib_pg:query(
            <<
                "SELECT action_msg_id, accepted_seq FROM public.c2g_forward_edit_fact "
                "WHERE group_id=$1 AND original_msg_id=$2 ORDER BY accepted_seq"
            >>,
            [?C2GGID, Orig]
        ),
        ?assertEqual([<<"be05_ef_e1_", Uq3/binary>>, <<"be05_ef_e2_", Uq3/binary>>], [
            maps:get(<<"action_msg_id">>, R)
         || R <- Rows
        ]),
        %% 正文仍未被任何编辑重写（事实权威，非 payload 权威）
        NoEditPayload2 = scalar_first(
            <<"SELECT payload::text AS v FROM public.msg_store WHERE msg_id=$1">>,
            [Orig],
            <<"v">>
        ),
        ?assertEqual(nomatch, binary:match(NoEditPayload2, <<"v2">>)),
        ?assertEqual(nomatch, binary:match(NoEditPayload2, <<"v3">>)),
        ?assertEqual(nomatch, binary:match(NoEditPayload2, <<"original_msg_id">>))
    end).

%% BE-05:LOCAL:04/05 撤回先到、编辑后到：正文不复活，控制事实可恢复；
%% 撤回后历史重拉只见 tombstone（本轮 msg_store_repo tombstone 修复）。
be05_04_c2g_revoke_first_test_() ->
    ?TEST_WITH_DB(fun() ->
        c2g_cleanup(),
        c2g_group_fixture(),
        Uq4 = be05_uq(),
        Orig = <<"be05_rf_orig_", Uq4/binary>>,
        %% original 走生产链（同 be05_03）
        _SeqOrig = c2g_stage(Orig),
        _ = c2g_archive_row(Orig),
        %% 撤回先到：事实 + 正式行 tombstone 同事务落地
        PR = #{
            <<"action">> => <<"message_revoke_ack">>,
            <<"content">> => <<"">>,
            <<"original_msg_id">> => Orig
        },
        {ok, _, _, _} = c2g_stage_action(
            <<"be05_rf_r1_", Uq4/binary>>, <<"message_revoke_ack">>, PR, 1, Orig
        ),
        ?assertEqual(
            1,
            scalar_first(
                <<
                    "SELECT count(*)::bigint AS v FROM public.c2g_revoke_fact "
                    "WHERE group_id=$1 AND original_msg_id=$2"
                >>,
                [?C2GGID, Orig],
                <<"v">>
            )
        ),
        Tomb = scalar_first(
            <<"SELECT payload::text AS v FROM public.msg_store WHERE msg_id=$1">>,
            [Orig],
            <<"v">>
        ),
        ?assertEqual(nomatch, binary:match(Tomb, <<"\"m\":1">>)),
        ?assert(nomatch =/= binary:match(Tomb, <<"message_revoke_ack">>)),
        %% 编辑后到：事实照常捕获，但正文保持 tombstone——不复活
        PE = #{
            <<"action">> => <<"message_edit_ack">>,
            <<"content">> => <<"late">>,
            <<"original_msg_id">> => Orig
        },
        {ok, _, _, _} = c2g_stage_action(
            <<"be05_rf_e1_", Uq4/binary>>, <<"message_edit_ack">>, PE, 3, Orig
        ),
        Tomb2 = scalar_first(
            <<"SELECT payload::text AS v FROM public.msg_store WHERE msg_id=$1">>,
            [Orig],
            <<"v">>
        ),
        ?assertEqual(nomatch, binary:match(Tomb2, <<"late">>)),
        %% 转发可见性权威：撤回事实生效 → 源消息不可再转发
        ?assertEqual(
            {error, forbidden},
            elib_pg:with_tx(fun(C) ->
                forward_attachment_source_repo:c2g_tx(C, Orig, ?C2GGID, ?UA)
            end)
        ),
        %% 控制事实可恢复：两条事实行均在库可查（撤回 + 迟到编辑）
        {ok, RevRow} = elib_pg:query(
            <<
                "SELECT accepted_seq, action_msg_id FROM public.c2g_revoke_fact "
                "WHERE group_id=$1 AND original_msg_id=$2"
            >>,
            [?C2GGID, Orig]
        ),
        ?assertEqual(1, length(RevRow)),
        {ok, EditRow} = elib_pg:query(
            <<
                "SELECT action_msg_id FROM public.c2g_forward_edit_fact "
                "WHERE group_id=$1 AND original_msg_id=$2"
            >>,
            [?C2GGID, Orig]
        ),
        ?assertEqual(1, length(EditRow)),
        %% 历史重拉：行仍存在（控制事实可见）但正文= tombstone，原内容不重现
        {ok, Payload} = messaging_logic:history(?UA, <<"c2g">>, <<"930001">>, 0, 50, <<>>),
        HistBin = jsone:encode(Payload, [native_utf8]),
        ?assertEqual(nomatch, binary:match(HistBin, <<"\"m\":1">>))
    end).
