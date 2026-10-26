-module(msg_expire_at_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R05-A（IMBOY-TIMEZONE-REPAIR-V1）：自毁绝对期限（expire_at）持久化。
%%%
%%% 真实 PG（本轮隔离库）验证：
%%%   R05-1 接受事务内决定期限 → 暂停 worker（不启动/enqueue）→ staging
%%%         行携带期限（接受后崩溃不丢）→ 恢复 worker（process_row）→
%%%         正式表/归档搬运；归档开关 true/false 两条路径。
%%%   R05-2 worker 先转正 vs reply 直写先落，两种先后顺序同一期限；
%%%         重复接受不重赋；set_expire_at 对已有期限不覆盖（不延长）。
%%%   R05-6 旧数据 NULL expire_at 不被伪造（staging 无期限 → 转正/归档
%%%         均为 NULL）。
%%%
%%% worker 暂停控制：本套件直接调用 repo/worker 的行级函数
%%% （process_row），不经 enqueue kick；eunit 测试 VM 的周期 tick 已由
%%% eunit_runner 关闭（msg_store_worker_tick_ms=0），staging 行保持
%%% processed_at IS NULL 直到手动脉冲。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(CLEAN_PREFIX, <<"tzv1x_%">>).

%% 固定接受时钟（与 msg_accepted_at_pg_tests 一致锚点）
-define(FIXED_MS, 1789600000000).
-define(EXPIRE_SECS, 60).
-define(EXPECT_EXPIRE_MS, (?FIXED_MS + ?EXPIRE_SECS * 1000)).

accepted_rfc() ->
    elib_dt:to_rfc3339(?FIXED_MS, millisecond, "Z").

expire_rfc() ->
    elib_dt:to_rfc3339(?EXPECT_EXPIRE_MS, millisecond, "Z").

client_created_rfc() ->
    %% 客户端慢 10 分钟——R05 语义下期限与它无关
    elib_dt:to_rfc3339(?FIXED_MS - 600000, millisecond, "Z").

cleanup_test_rows() ->
    lists:foreach(
        fun(Sql) -> _ = elib_pg:execute(Sql, [?CLEAN_PREFIX]) end,
        [
            <<"DELETE FROM public.msg_store WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_c2c WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_c2g WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_store_staging WHERE msg_id LIKE $1">>
        ]
    ),
    %% 本套件会话计数器行一并清理
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key IN ",
            "('c2c:910001:910002','c2g:990001')">>,
        []
    ),
    ok.

fetch_staging(MsgId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT * FROM public.msg_store_staging WHERE type=$1 AND msg_id=$2">>,
        [<<"c2c">>, MsgId]
    ),
    Row.

fetch_staging_expire(MsgId) ->
    {ok, [#{<<"expire_at">> := E}]} = elib_pg:query(
        <<"SELECT expire_at FROM public.msg_store_staging WHERE type=$1 AND msg_id=$2">>,
        [<<"c2c">>, MsgId]
    ),
    E.

fetch_c2c_expire(MsgId) ->
    {ok, [#{<<"expire_at">> := E}]} = elib_pg:query(
        <<"SELECT expire_at FROM public.msg_c2c WHERE msg_id=$1">>,
        [MsgId]
    ),
    E.

%%% worker 转正后的 staging 收尾走 msg_store_ds:unstage/2 —— gen_server:cast
%%% 异步调 msg_store_repo:mark_processed/2（UPDATE processed_at = NOW()，行
%%% 保留为审计备份；既有 A1 设计，非 R05-A 变更）。断言前轮询等 cast 落库，
%%% 超时仍未标记即失败（不吞掉收尾故障）。
wait_staging_processed(MsgId) ->
    wait_staging_processed(MsgId, 50, 100).

wait_staging_processed(_MsgId, _IntervalMs, 0) ->
    error(staging_not_processed);
wait_staging_processed(MsgId, IntervalMs, N) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) as count FROM public.msg_store_staging ",
            "WHERE type='c2c' AND msg_id=$1 AND processed_at IS NOT NULL">>,
        [MsgId]
    ),
    case Count of
        1 ->
            ok;
        _ ->
            timer:sleep(IntervalMs),
            wait_staging_processed(MsgId, IntervalMs, N - 1)
    end.

fetch_archive_expire(MsgId) ->
    {ok, [#{<<"expire_at">> := E}]} = elib_pg:query(
        <<"SELECT expire_at FROM public.msg_store WHERE msg_id=$1">>,
        [MsgId]
    ),
    E.

to_ms(null) -> null;
to_ms(Bin) -> elib_dt:rfc3339_to(Bin, millisecond).

with_archive_enabled(Value, Fun) ->
    Prev = application:get_env(imboy, msg_archive_enabled, false),
    application:set_env(imboy, msg_archive_enabled, Value),
    try
        Fun()
    after
        case Prev of
            undefined -> application:unset_env(imboy, msg_archive_enabled);
            _ -> application:set_env(imboy, msg_archive_enabled, Prev)
        end
    end.

%%%-------------------------------------------------------------------
%%% R05-1：期限随接受事务落 staging；暂停 worker（接受后"崩溃"）不丢；
%%% 恢复 worker 后正式表/归档拿到同一期限（归档开关两条路径）。
%%%-------------------------------------------------------------------
r05_1_deadline_survives_worker_pause_and_crash_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        ExpireAt = expire_rfc(),

        %% -- 阶段 1：接受事务落库（worker 未运行：repo 直调，无 enqueue kick） --
        MsgId = <<"tzv1x_r1_pause">>,
        {ok, _Id, _Seq} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"burn\"}">>,
            910001,
            910002,
            client_created_rfc(),
            accepted_rfc(),
            null,
            accepted_rfc(),
            ExpireAt
        ),

        %% staging 行带期限（接受的同一事务落库——deadline 已持久，不等 worker）
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_staging_expire(MsgId))),

        %% -- 阶段 2：模拟接受后进程崩溃（不启动 worker）——staging 行仍在，期限不丢 --
        Row1 = fetch_staging(MsgId),
        ?assertEqual(null, maps:get(<<"processed_at">>, Row1)),

        %% -- 阶段 3：归档关闭路径——恢复 worker（行级脉冲），正式表拿到期限 --
        with_archive_enabled(false, fun() ->
            ok = msg_store_worker:process_row(Row1)
        end),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgId))),
        %% staging 已标记处理（unstage → mark_processed 异步，轮询等待）
        ok = wait_staging_processed(MsgId),

        %% -- 阶段 4：归档开启路径——staging→正式表→归档三处同一期限 --
        MsgId2 = <<"tzv1x_r1_archive">>,
        {ok, _Id2, _Seq2} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId2,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"burn\"}">>,
            910001,
            910002,
            client_created_rfc(),
            accepted_rfc(),
            null,
            accepted_rfc(),
            ExpireAt
        ),
        Row2 = fetch_staging(MsgId2),
        with_archive_enabled(true, fun() ->
            ok = msg_store_worker:process_row(Row2)
        end),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgId2))),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_archive_expire(MsgId2))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R05-2：两种写入先后一致 + 重复接受不重赋 + 已有期限不覆盖。
%%%-------------------------------------------------------------------
r05_2_write_ordering_and_no_extension_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        ExpireAt = expire_rfc(),
        AcceptedAt = accepted_rfc(),
        ClientCreated = client_created_rfc(),

        %% -- A. worker 先转正，reply 直写后到（冲突 DO NOTHING，期限不变） --
        MsgA = <<"tzv1x_r2_worker_first">>,
        {ok, _IA, _SA} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgA,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"a\"}">>,
            910001,
            910002,
            ClientCreated,
            AcceptedAt,
            null,
            AcceptedAt,
            ExpireAt
        ),
        RowA = fetch_staging(MsgA),
        with_archive_enabled(false, fun() ->
            ok = msg_store_worker:process_row(RowA)
        end),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgA))),
        %% reply 直写（同 accepted/expire 值）撞唯一键 → 补洞只回填 reply 列
        ok = msg_c2c_ds:write_msg_with_reply(
            ClientCreated,
            MsgA,
            <<"{\"t\":\"a\"}">>,
            910001,
            910002,
            AcceptedAt,
            <<"text">>,
            null,
            <<"replied_1">>,
            910001,
            <<"snippet">>,
            AcceptedAt,
            ExpireAt
        ),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgA))),

        %% -- B. reply 直写先落，worker 后到（冲突 no_insert，期限保持首写值） --
        MsgB = <<"tzv1x_r2_reply_first">>,
        {ok, _IB, _SB} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgB,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"b\"}">>,
            910001,
            910002,
            ClientCreated,
            AcceptedAt,
            null,
            AcceptedAt,
            ExpireAt
        ),
        ok = msg_c2c_ds:write_msg_with_reply(
            ClientCreated,
            MsgB,
            <<"{\"t\":\"b\"}">>,
            910001,
            910002,
            AcceptedAt,
            <<"text">>,
            null,
            <<"replied_2">>,
            910001,
            <<"snippet">>,
            AcceptedAt,
            ExpireAt
        ),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgB))),
        RowB = fetch_staging(MsgB),
        %% 撞唯一键的同步层精确语义：repo ON CONFLICT DO NOTHING 0 行 →
        %% {error, conflict_no_insert}（不重写、期限保持首写值）
        ?assertMatch(
            {error, conflict_no_insert},
            msg_c2c_ds:write_msg(
                ClientCreated,
                MsgB,
                <<"{\"t\":\"b\"}">>,
                910001,
                910002,
                AcceptedAt,
                <<"text">>,
                null,
                null,
                AcceptedAt,
                ExpireAt
            )
        ),
        with_archive_enabled(false, fun() ->
            %% worker 后到：write 层冲突幂等成功（返回值视 unstage 通道而定，
            %% ok 或 {error, conflict_no_insert} 皆可），staging 异步清理
            RowResult = msg_store_worker:process_row(RowB),
            ?assert(
                lists:member(RowResult, [ok, {error, conflict_no_insert}])
            )
        end),
        ok = wait_staging_processed(MsgB),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgB))),

        %% -- C. 重复接受（同 msg_id 二次 staging）——首期限不重赋 --
        MsgC = <<"tzv1x_r2_dup">>,
        {ok, _IC1, _SC1} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgC,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"c\"}">>,
            910001,
            910002,
            ClientCreated,
            AcceptedAt,
            null,
            AcceptedAt,
            ExpireAt
        ),
        %% 重试带**更晚**的期限（accepted 慢 5 分钟 + 同 secs）——不得延长
        LaterAccepted = elib_dt:to_rfc3339(?FIXED_MS + 300000, millisecond, "Z"),
        LaterExpire = elib_dt:to_rfc3339(
            ?FIXED_MS + 300000 + ?EXPIRE_SECS * 1000, millisecond, "Z"
        ),
        {ok, duplicate, _SC2} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgC,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"c\"}">>,
            910001,
            910002,
            ClientCreated,
            LaterAccepted,
            null,
            LaterAccepted,
            LaterExpire
        ),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_staging_expire(MsgC))),

        %% -- D. set_expire_at：已有期限不覆盖（不延长期限），0 行是明确结果 --
        RowC = fetch_staging(MsgC),
        with_archive_enabled(false, fun() ->
            ok = msg_store_worker:process_row(RowC)
        end),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgC))),
        {ok, 0} = msg_c2c_repo:set_expire_at(MsgC, LaterExpire),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgC))),
        %% 旧行 NULL 时 set_expire_at 命中 1 行（兜底补齐语义）
        MsgD = <<"tzv1x_r2_null_fill">>,
        {ok, _ID1, _SD1} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgD,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"d\"}">>,
            910001,
            910002,
            ClientCreated,
            AcceptedAt,
            null,
            AcceptedAt,
            null
        ),
        RowD = fetch_staging(MsgD),
        with_archive_enabled(false, fun() ->
            ok = msg_store_worker:process_row(RowD)
        end),
        ?assertEqual(null, fetch_c2c_expire(MsgD)),
        {ok, 1} = msg_c2c_repo:set_expire_at(MsgD, ExpireAt),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_c2c_expire(MsgD))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R05-6：旧数据（无期限）不被伪造——staging NULL → 转正/归档均 NULL。
%%%-------------------------------------------------------------------
r05_6_legacy_null_not_fabricated_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1x_r6_legacy">>,

        %% 旧入口（/12，无期限）staging——列保持 NULL
        {ok, _Id, _Seq} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"legacy\"}">>,
            910001,
            910002,
            client_created_rfc(),
            accepted_rfc(),
            null,
            accepted_rfc()
        ),
        ?assertEqual(null, fetch_staging_expire(MsgId)),

        Row = fetch_staging(MsgId),
        with_archive_enabled(true, fun() ->
            ok = msg_store_worker:process_row(Row)
        end),
        %% 正式表与归档均不伪造期限
        ?assertEqual(null, fetch_c2c_expire(MsgId)),
        ?assertEqual(null, fetch_archive_expire(MsgId)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R05-A（c2g 侧）：repo 归档搬运期限——c2g staging 形状的行归档到
%%% msg_store 后期限等值（staging→archive 传递链；c2g staging 事务级
%%% 全链需 group fixture， wiring 由 msg_c2g_logic_tests capture 覆盖）。
%%%-------------------------------------------------------------------
r05_c2g_archive_carries_expire_at_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1x_c2g_archive">>,
        AcceptedAt = accepted_rfc(),
        ExpireAt = expire_rfc(),
        %% c2g staging 行形状（claim_pending SELECT 输出形状的子集）
        Row = #{
            <<"type">> => <<"c2g">>,
            <<"msg_id">> => MsgId,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => null,
            <<"sender_did">> => null,
            <<"payload">> => <<"{\"to\":\"990001\",\"t\":\"burn\"}">>,
            <<"from_id">> => 910001,
            <<"created_at">> => client_created_rfc(),
            <<"server_ts">> => AcceptedAt,
            <<"accepted_at">> => AcceptedAt,
            <<"expire_at">> => ExpireAt,
            <<"conv_seq">> => 1,
            <<"to_id_list">> => [910001, 910002]
        },
        ?assertEqual(ok, msg_archive_repo:archive(Row)),
        ?assertEqual(?EXPECT_EXPIRE_MS, to_ms(fetch_archive_expire(MsgId))),
        %% accepted_at 同搬运（R03 回归锚点）
        {ok, [#{<<"accepted_at">> := A}]} = elib_pg:query(
            <<"SELECT accepted_at FROM public.msg_store WHERE msg_id=$1">>, [MsgId]
        ),
        ?assertEqual(?FIXED_MS, to_ms(A)),
        cleanup_test_rows()
    end).
