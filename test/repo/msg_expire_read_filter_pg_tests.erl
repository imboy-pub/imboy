-module(msg_expire_read_filter_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R05-B-2 / R05-7（IMBOY-TIMEZONE-DECISIONS-V1，D01 冻结合同）：
%%% 自毁消息读侧过滤——history / history_backfill / 离线三类实际读取
%%% 路径不返回 `expire_at IS NOT NULL AND expire_at <= NOW()` 的消息正文。
%%%
%%% 真实 PG（本轮隔离库）断言，全部走接口函数（ds/repo 读取入口，与
%%% msg_handler/messaging_logic 同一调用面），不以 SQL 字符串拼接代替：
%%%   R05B-2a C2C 离线三读（read_msg/3 无游标 + DID + 会话列表）排除已到期；
%%%   R05B-2b history（safe_history）排除已到期行，safe_head 不受影响；
%%%   R05B-2c history_backfill（backfill）排除已到期行，NextSeq 推进不空转；
%%%   R05B-2d C2G 离线（时间线→list_by_ids 正文装载）排除已到期正文；
%%%   R05-7  旧 NULL expire_at 行不被过滤、不被伪造（三路径全部返回）。
%%%
%%% 期限值用 PG `NOW() ± INTERVAL` 与过滤条件锚同一数据库时钟，消除
%%% 应用/DB 钟差竞态。DB 不可用按 TEST_WITH_DB 惯例 skip（不算通过）。
%%%===================================================================

-define(UID_A, 920001).
-define(UID_B, 920002).
-define(GID, 992001).
-define(PREFIX, <<"tzd01x_%">>).
-define(ST_UID_A, 920011).
-define(ST_UID_B, 920012).
-define(CONV_KEY, <<"c2c:920011:920012">>).

cleanup_test_rows() ->
    lists:foreach(
        fun(Sql) -> _ = elib_pg:execute(Sql, [?PREFIX]) end,
        [
            <<"DELETE FROM public.msg_store WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_c2c WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_c2g WHERE msg_id LIKE $1">>,
            <<"DELETE FROM public.msg_c2g_timeline WHERE msg_id LIKE $1">>
        ]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key = 'c2c:920011:920012'">>,
        []
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM group_member_generation WHERE group_id = $1">>,
        [?GID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM group_member WHERE group_id = $1">>,
        [?GID]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM \"group\" WHERE id = $1">>,
        [?GID]
    ),
    ok.

%%% 三类期限：已到期（60s 前）/ 未到期（+1h）/ 旧 NULL
insert_c2c_row(Id, Seq, MsgId, ExpireSqlFragment) ->
    %% conv_seq 必填：fn_c2c_order_write_guard 要求 C2C 直插带正服务端序号
    %%（独立 uid 对 + 单调 seq，与 allocator 同锁序断言不冲突）。
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.msg_c2c "
            "(id, from_id, to_id, msg_id, msg_type, payload, created_at, server_ts, "
            "accepted_at, conv_seq, expire_at) "
            "VALUES ($1, $2, $3, $4, 'text', $5, NOW() - INTERVAL '5 minutes', "
            "NOW() - INTERVAL '5 minutes', NOW() - INTERVAL '5 minutes', $6, ",
            ExpireSqlFragment/binary,
            ")"
        >>,
        [Id, ?UID_A, ?UID_B, MsgId, <<"{\"t\":\"", MsgId/binary, "\"}">>, Seq]
    ),
    ok.

insert_store_row(Id, Seq, MsgId, ExpireSqlFragment) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.msg_store "
            "(id, chat_type, conv_key, conv_seq, msg_id, msg_type, from_id, to_id, "
            "payload, created_at, server_ts, accepted_at, expire_at) "
            "VALUES ($1, 'c2c', $2, $3, $4, 'text', $5, $6, $7, "
            "NOW() - INTERVAL '10 minutes', NOW() - INTERVAL '10 minutes', "
            "NOW() - INTERVAL '10 minutes', ",
            ExpireSqlFragment/binary,
            ")"
        >>,
        [Id, ?CONV_KEY, Seq, MsgId, ?ST_UID_A, ?ST_UID_B, <<"{\"t\":\"", MsgId/binary, "\"}">>]
    ),
    ok.

insert_c2g_fixture(Id, Seq, MsgId, ExpireSqlFragment) ->
    %% 离线读取链要求 timeline.conv_seq 非空且 >= 成员代际 start_seq；
    %% 群/成员/代际 fixture 复用 msg_offline_progress_pg_tests:setup_group 范式。
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.msg_c2g "
            "(id, from_id, to_id, msg_id, msg_type, payload, created_at, expire_at) "
            "VALUES ($1, $2, $3, $4, 'text', $5::jsonb, "
            "NOW() - INTERVAL '5 minutes', ",
            ExpireSqlFragment/binary,
            ")"
        >>,
        [Id, ?UID_A, ?GID, MsgId, <<"{\"t\":\"", MsgId/binary, "\"}">>]
    ),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.msg_c2g_timeline "
            "(msg_id, to_uid, to_gid, created_at, conv_seq) "
            "VALUES ($1, $2, $3, NOW() - INTERVAL '5 minutes', $4)"
        >>,
        [MsgId, ?UID_B, ?GID, Seq]
    ),
    ok.

setup_group() ->
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status) ",
            "VALUES ($1, $2, $3, 'tzd01x_fixture', 1)">>,
        [?GID, ?UID_A, ?UID_A]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status) ",
            "VALUES ($1, $2, $3, 4, true, 1), ($4, $2, $5, 1, true, 1)">>,
        [?GID * 10 + 1, ?GID, ?UID_A, ?GID * 10 + 2, ?UID_B]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member_generation (group_id, user_id, generation_no, start_seq) ",
            "VALUES ($1, $2, 1, 1), ($1, $3, 1, 1)">>,
        [?GID, ?UID_A, ?UID_B]
    ),
    ok.

seed_all_tables() ->
    cleanup_test_rows(),
    %% C2C 离线表：已到期 / 未到期 / 旧 NULL
    ok = insert_c2c_row(920101, 1, <<"tzd01x_c2c_expired">>, <<"NOW() - INTERVAL '60 seconds'">>),
    ok = insert_c2c_row(920102, 2, <<"tzd01x_c2c_future">>, <<"NOW() + INTERVAL '3600 seconds'">>),
    ok = insert_c2c_row(920103, 3, <<"tzd01x_c2c_null">>, <<"NULL">>),
    %% 归档表（history / backfill 数据面）：seq 3/5 已到期，1/2/4 存活。
    %% seq 5 刻意放尾部：SafeHead=5 与 NextSeq=5 才能证伪"safe_head/推进
    %% 被正文过滤吞掉"与"只推进到末条存活行 seq"两类变异（若过期行在
    %% 中间，两断言与"过滤后 max seq"无法区分）。
    ok = insert_store_row(920201, 1, <<"tzd01x_st_alive1">>, <<"NULL">>),
    ok = insert_store_row(920202, 2, <<"tzd01x_st_alive2">>, <<"NOW() + INTERVAL '3600 seconds'">>),
    ok = insert_store_row(920203, 3, <<"tzd01x_st_expired">>, <<"NOW() - INTERVAL '60 seconds'">>),
    ok = insert_store_row(920204, 4, <<"tzd01x_st_null">>, <<"NULL">>),
    ok = insert_store_row(
        920205, 5, <<"tzd01x_st_expired_tail">>, <<"NOW() - INTERVAL '60 seconds'">>
    ),
    %% C2G 正文 + 时间线（含群/成员/代际 fixture）
    ok = setup_group(),
    ok = insert_c2g_fixture(
        920301, 1, <<"tzd01x_c2g_expired">>, <<"NOW() - INTERVAL '60 seconds'">>
    ),
    ok = insert_c2g_fixture(
        920302, 2, <<"tzd01x_c2g_future">>, <<"NOW() + INTERVAL '3600 seconds'">>
    ),
    ok = insert_c2g_fixture(920303, 3, <<"tzd01x_c2g_null">>, <<"NULL">>),
    ok.

msg_ids(Rows) -> [maps:get(<<"msg_id">>, R) || R <- Rows].

%%%-------------------------------------------------------------------
%%% R05B-2a：C2C 离线三读（无游标 / DID 按设备 / 会话列表）
%%%-------------------------------------------------------------------
r05b_2a_c2c_offline_reads_filter_expired_test_() ->
    with_fixture(fun() ->
        seed_all_tables(),
        Expected = [<<"tzd01x_c2c_future">>, <<"tzd01x_c2c_null">>],

        %% 离线拉取（offline v1 无游标路径）
        C2C = msg_c2c_ds:read_msg(?UID_B, 10),
        ?assertEqual(Expected, msg_ids(C2C)),

        %% 按设备离线拉取（offline v2 DID 路径；无 msg_delivery ACK 行=全部待投）
        Dev = msg_c2c_ds:read_msg_for_device(?UID_B, <<"tzd01x_dev1">>, 10, undefined),
        ?assertEqual(Expected, msg_ids(Dev)),

        %% 会话列表读取（双向聚合）
        Conv = msg_c2c_ds:read_msg_for_conversation(?UID_B, 10, undefined),
        ?assertEqual(Expected, msg_ids(Conv))
    end).

%%%-------------------------------------------------------------------
%%% R05B-2b：history（safe_history）过滤 + safe_head 不受影响
%%%-------------------------------------------------------------------
r05b_2b_history_filters_expired_test_() ->
    with_fixture(fun() ->
        seed_all_tables(),
        {ok, Rows, SafeHead, _Gaps} =
            msg_archive_repo:safe_history(?CONV_KEY, 0, 100, <<"c2c">>, ?ST_UID_A, ?ST_UID_B),
        ?assertEqual(
            [1, 2, 4],
            [maps:get(<<"conv_seq">>, R) || R <- Rows]
        ),
        ?assert(
            lists:any(fun(R) -> maps:get(<<"msg_id">>, R) =:= <<"tzd01x_st_expired">> end, Rows) =:=
                false
        ),
        %% safe_head 是序号边界查询，不受正文过滤影响（过期行仍占 seq 空间）。
        %% 尾部过期行使本断言可证伪：若 safe_head 吞掉过期行会得 4。
        ?assertEqual(5, SafeHead)
    end).

%%%-------------------------------------------------------------------
%%% R05B-2c：history_backfill 过滤 + NextSeq 推进不空转
%%%-------------------------------------------------------------------
r05b_2c_backfill_filters_expired_test_() ->
    with_fixture(fun() ->
        seed_all_tables(),
        {ok, Rows, NextSeq, HasMore} =
            msg_archive_repo:backfill(?CONV_KEY, 0, 100, <<"c2c">>, ?ST_UID_A, ?ST_UID_B),
        ?assertEqual([1, 2, 4], [maps:get(<<"conv_seq">>, R) || R <- Rows]),
        %% 全批存活行 ≤ Limit → NextSeq 推进到 EndSeq（=SafeHead=5，含尾部
        %% 过期行占位），不空转；若实现退化成"推进到末条存活行 seq"会得 4。
        ?assertEqual(5, NextSeq),
        ?assertEqual(false, HasMore),

        %% AfterSeq=2 起：仅 seq 4 存活（3/5 已过滤），NextSeq 仍推进到 5
        {ok, Rows2, NextSeq2, _} =
            msg_archive_repo:backfill(?CONV_KEY, 2, 100, <<"c2c">>, ?ST_UID_A, ?ST_UID_B),
        ?assertEqual([4], [maps:get(<<"conv_seq">>, R) || R <- Rows2]),
        ?assertEqual(5, NextSeq2)
    end).

%%%-------------------------------------------------------------------
%%% R05B-2d：C2G 离线（时间线→正文装载）过滤已到期正文
%%%-------------------------------------------------------------------
r05b_2d_c2g_offline_filters_expired_test_() ->
    with_fixture(fun() ->
        seed_all_tables(),
        Rows = msg_c2g_ds:read_msg(?UID_B, 10, undefined),
        Ids = lists:sort(msg_ids(Rows)),
        ?assertEqual([<<"tzd01x_c2g_future">>, <<"tzd01x_c2g_null">>], Ids)
    end).

%%%-------------------------------------------------------------------
%%% R05-7：旧 NULL 行三路径全部返回（不被过滤/伪造）
%%%-------------------------------------------------------------------
r05_7_null_rows_never_filtered_test_() ->
    with_fixture(fun() ->
        seed_all_tables(),
        ?assert(lists:member(<<"tzd01x_c2c_null">>, msg_ids(msg_c2c_ds:read_msg(?UID_B, 10)))),
        {ok, StoreRows, _, _} =
            msg_archive_repo:safe_history(?CONV_KEY, 0, 100, <<"c2c">>, ?ST_UID_A, ?ST_UID_B),
        ?assert(
            lists:member(<<"tzd01x_st_null">>, [maps:get(<<"msg_id">>, R) || R <- StoreRows])
        ),
        ?assert(
            lists:member(<<"tzd01x_c2g_null">>, msg_ids(msg_c2g_ds:read_msg(?UID_B, 10, undefined)))
        ),
        %% NULL 行不进物理扫描集（既有合同）：直接核对 msg_c2c 中 NULL 行留存
        {ok, [#{<<"count">> := NullCount}]} = elib_pg:query(
            <<"SELECT count(*) as count FROM public.msg_c2c ",
                "WHERE msg_id LIKE $1 AND expire_at IS NULL">>,
            [?PREFIX]
        ),
        ?assertEqual(1, NullCount)
    end).

%% Release owned rows even when an assertion fails; later roots share this DB.
%% 断言失败也清理自有数据，避免污染共库的后续测试。
with_fixture(Test) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        try Test() after cleanup_test_rows() end
    end).
