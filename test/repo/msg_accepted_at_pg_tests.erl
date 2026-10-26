-module(msg_accepted_at_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R03 / D04（IMBOY-TIMEZONE-REPAIR-V1）：不可变服务端首次接受时间。
%%%
%%% 真实 PG（本轮隔离库）验证：
%%%   R03-1 客户端 created_at 偏移 ±10 分钟时，接受时间 = 服务端固定时钟
%%%   R03-2 重复请求（同 msg_id 二次 staging）接受值不变（duplicate）
%%%   R03-3 引用双写（worker 转正 vs reply 直写）两种先后顺序同结果
%%%   R03-4 编辑（update_payload_by_msg_id）不重置 accepted_at
%%%   R03-5 staging → 正式表 → 归档同 msg_id 的接受时间一致
%%%   C2G 止污：新行 server_ts 对齐 accepted_at，不再写入客户端时间
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(CLEAN_PREFIX, <<"tzv1_%">>).

cleanup_test_rows() ->
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_c2c WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_c2g WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_staging WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_c2g_timeline WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_c2g_recipient_snapshot WHERE msg_id LIKE $1">>, [?CLEAN_PREFIX]
    ),
    %% 本套件测试会话的计数器行（c2c:910001:910002 / c2g:990001）一并清理，
    %% 避免跨用例 conv_seq 头部累积触发 c2c_order_conflict 写守卫
    _ = elib_pg:execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key IN ",
            "('c2c:910001:910002','c2g:990001')">>,
        []
    ),
    ok.

%% 服务端固定测试时钟（D04 采样：elib_dt:millisecond()，UTC）
-define(FIXED_MS, 1789600000000).
-define(FIXED_RFC, <<"2026-09-16T23:06:40Z">>).

fixed_accepted_rfc() ->
    elib_dt:to_rfc3339(?FIXED_MS, millisecond, "Z").

%% 客户端慢 10 分钟（created_at < 接受时刻）
client_created_rfc(slow) ->
    elib_dt:to_rfc3339(?FIXED_MS - 600000, millisecond, "Z");
%% 客户端快 10 分钟（created_at > 接受时刻）
client_created_rfc(fast) ->
    elib_dt:to_rfc3339(?FIXED_MS + 600000, millisecond, "Z").

fetch_staging(MsgId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT created_at, server_ts, accepted_at FROM public.msg_store_staging ",
            "WHERE type='c2c' AND msg_id = $1">>,
        [MsgId]
    ),
    Row.

fetch_c2c(MsgId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT created_at, server_ts, accepted_at FROM public.msg_c2c ", "WHERE msg_id = $1">>,
        [MsgId]
    ),
    Row.

fetch_c2g(MsgId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT created_at, server_ts, accepted_at FROM public.msg_c2g ", "WHERE msg_id = $1">>,
        [MsgId]
    ),
    Row.

fetch_archive(MsgId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT created_at, server_ts, accepted_at FROM public.msg_store ", "WHERE msg_id = $1">>,
        [MsgId]
    ),
    Row.

to_ms(null) -> null;
to_ms(Bin) -> elib_dt:rfc3339_to(Bin, millisecond).

%%%-------------------------------------------------------------------
%%% R03-1：客户端 ±10 分钟时钟偏移下，接受时间 = 服务端固定时钟
%%%-------------------------------------------------------------------
r03_1_accept_time_is_server_clock_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        AcceptedAt = fixed_accepted_rfc(),
        lists:foreach(
            fun(Clock) ->
                MsgId = <<"tzv1_r1_", (atom_to_binary(Clock))/binary>>,
                ClientCreated = client_created_rfc(Clock),
                {ok, _Id, _Seq} = msg_store_repo:stage_with_sequence(
                    <<"c2c">>,
                    MsgId,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    910001,
                    910002,
                    ClientCreated,
                    AcceptedAt,
                    null,
                    AcceptedAt
                ),
                Row = fetch_staging(MsgId),
                %% 接受时间 = 固定服务端时钟，与客户端偏移无关
                ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Row))),
                %% created_at 保留客户端原始时间（D04：不修正历史值）
                ?assertEqual(
                    elib_dt:rfc3339_to(ClientCreated, millisecond),
                    to_ms(maps:get(<<"created_at">>, Row))
                ),
                %% 客户端快 10 分钟时 created_at ≠ accepted_at（污染源可区分）
                ?assert(to_ms(maps:get(<<"created_at">>, Row)) =/= ?FIXED_MS)
            end,
            [slow, fast]
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R03-2：重复请求/重放——accepted_at 不重新赋值
%%%-------------------------------------------------------------------
r03_2_duplicate_keeps_first_accept_time_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1_r2_dup">>,
        AcceptedAt = fixed_accepted_rfc(),
        BaseArgs = [
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{}">>,
            910001,
            910002,
            client_created_rfc(slow),
            AcceptedAt
        ],
        {ok, _Id1, _Seq1} = erlang:apply(
            msg_store_repo, stage_with_sequence, BaseArgs ++ [null, AcceptedAt]
        ),
        %% 客户端重发：同一 msg_id，服务端时钟已前进（更晚的接受值）
        LaterAccepted = elib_dt:to_rfc3339(?FIXED_MS + 30000, millisecond, "Z"),
        {ok, duplicate, _Seq2} = erlang:apply(
            msg_store_repo, stage_with_sequence, BaseArgs ++ [null, LaterAccepted]
        ),
        Row = fetch_staging(MsgId),
        %% 首次接受值保持不变，未被重试覆盖
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Row))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R03-3：引用双写——worker 转正与 reply 直写两种先后顺序同结果
%%%-------------------------------------------------------------------
r03_3_reply_double_write_orders_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        AcceptedAt = fixed_accepted_rfc(),
        CreatedAt = client_created_rfc(slow),
        lists:foreach(
            fun({Order, MsgId}) ->
                case Order of
                    worker_first ->
                        %% 顺序 A：worker 转正先插（带 accepted_at），
                        %% reply 直写后到 → ON CONFLICT 只补 reply 三列
                        ok = msg_c2c_repo:write_msg_with_sender(
                            CreatedAt,
                            MsgId,
                            <<"{\"t\":\"a\"}">>,
                            910001,
                            910002,
                            AcceptedAt,
                            <<"text">>,
                            null,
                            null,
                            AcceptedAt
                        ),
                        ok = msg_c2c_repo:write_msg_with_reply(
                            CreatedAt,
                            MsgId,
                            <<"{\"t\":\"a\"}">>,
                            910001,
                            910002,
                            AcceptedAt,
                            <<"text">>,
                            null,
                            <<"tzv1_r3_ref">>,
                            910001,
                            <<"snip">>,
                            AcceptedAt
                        );
                    reply_first ->
                        %% 顺序 B：reply 直写先插（同一 accepted_at），
                        %% worker 后到 → ON CONFLICT DO NOTHING
                        ok = msg_c2c_repo:write_msg_with_reply(
                            CreatedAt,
                            MsgId,
                            <<"{\"t\":\"a\"}">>,
                            910001,
                            910002,
                            AcceptedAt,
                            <<"text">>,
                            null,
                            <<"tzv1_r3_ref">>,
                            910001,
                            <<"snip">>,
                            AcceptedAt
                        ),
                        %% worker 后到：冲突收敛（不重插、不重赋接受时间）
                        {error, conflict_no_insert} = msg_c2c_repo:write_msg_with_sender(
                            CreatedAt,
                            MsgId,
                            <<"{\"t\":\"a\"}">>,
                            910001,
                            910002,
                            AcceptedAt,
                            <<"text">>,
                            null,
                            null,
                            AcceptedAt
                        )
                end,
                Row = fetch_c2c(MsgId),
                %% 两种先后顺序：同一首次接受值 + 引用元数据齐全
                ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Row))),
                {ok, [Full]} = elib_pg:query(
                    <<"SELECT reply_to_msg_id, reply_snippet FROM public.msg_c2c ",
                        "WHERE msg_id = $1">>,
                    [MsgId]
                ),
                ?assertEqual(<<"tzv1_r3_ref">>, maps:get(<<"reply_to_msg_id">>, Full)),
                ?assertEqual(<<"snip">>, maps:get(<<"reply_snippet">>, Full))
            end,
            [
                {worker_first, <<"tzv1_r3_a">>},
                {reply_first, <<"tzv1_r3_b">>}
            ]
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R03-4：编辑不重置 accepted_at（c2c/c2g 的 payload 更新路径）
%%%-------------------------------------------------------------------
r03_4_edit_keeps_accept_time_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        AcceptedAt = fixed_accepted_rfc(),
        CreatedAt = client_created_rfc(slow),
        MsgId = <<"tzv1_r4_edit">>,
        ok = msg_c2c_repo:write_msg_with_sender(
            CreatedAt,
            MsgId,
            <<"{\"t\":\"before\"}">>,
            910001,
            910002,
            AcceptedAt,
            <<"text">>,
            null,
            null,
            AcceptedAt
        ),
        %% 编辑：独立更新机制只改 payload（与 server_ts=CURRENT_TIMESTAMP），
        %% 不得触碰 accepted_at
        {ok, _} = msg_c2c_repo:update_payload_by_msg_id(MsgId, <<"{\"t\":\"edited\"}">>),
        Row = fetch_c2c(MsgId),
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Row))),
        ?assertEqual(<<"{\"t\":\"edited\"}">>, fetch_c2c_payload(MsgId)),
        cleanup_test_rows()
    end).

fetch_c2c_payload(MsgId) ->
    {ok, [#{<<"payload">> := P}]} = elib_pg:query(
        <<"SELECT payload FROM public.msg_c2c WHERE msg_id = $1">>, [MsgId]
    ),
    P.

%%%-------------------------------------------------------------------
%%% R03-5：staging → 正式表 → 归档同 msg_id 接受时间一致（c2c 全链）
%%%-------------------------------------------------------------------
r03_5_chain_consistency_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1_r5_chain">>,
        AcceptedAt = fixed_accepted_rfc(),
        CreatedAt = client_created_rfc(fast),
        {ok, _Id, Seq} = msg_store_repo:stage_with_sequence(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"t\":\"chain\"}">>,
            910001,
            910002,
            CreatedAt,
            AcceptedAt,
            null,
            AcceptedAt
        ),
        Staging = fetch_staging(MsgId),
        %% worker 转正：从 staging 行读 accepted_at 搬进正式表（与
        %% msg_store_worker:do_write(c2c, Row) 同形状的 Row 构造）
        WorkerRow = #{
            <<"type">> => <<"c2c">>,
            <<"msg_id">> => MsgId,
            <<"payload">> => <<"{\"t\":\"chain\"}">>,
            <<"from_id">> => 910001,
            <<"to_id">> => 910002,
            <<"created_at">> => maps:get(<<"created_at">>, Staging),
            <<"server_ts">> => maps:get(<<"server_ts">>, Staging),
            <<"accepted_at">> => maps:get(<<"accepted_at">>, Staging),
            <<"msg_type">> => <<"text">>,
            <<"e2ee">> => null,
            <<"sender_did">> => null,
            <<"conv_seq">> => Seq
        },
        ok = msg_c2c_ds:write_msg(
            maps:get(<<"created_at">>, Staging),
            MsgId,
            <<"{\"t\":\"chain\"}">>,
            910001,
            910002,
            maps:get(<<"server_ts">>, Staging),
            <<"text">>,
            null,
            null,
            maps:get(<<"accepted_at">>, Staging)
        ),
        C2C = fetch_c2c(MsgId),
        %% 归档（worker maybe_archive 同款：staging 行 → msg_store）
        ok = msg_archive_repo:archive(WorkerRow),
        Archive = fetch_archive(MsgId),
        ?assertEqual(
            to_ms(maps:get(<<"accepted_at">>, Staging)),
            to_ms(maps:get(<<"accepted_at">>, C2C))
        ),
        ?assertEqual(
            to_ms(maps:get(<<"accepted_at">>, Staging)),
            to_ms(maps:get(<<"accepted_at">>, Archive))
        ),
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Archive))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% C2G 止污：write_accepted_msg 新行 server_ts = accepted_at（非客户端时间）
%%%-------------------------------------------------------------------
c2g_server_ts_no_longer_polluted_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1_c2g_clean">>,
        AcceptedAt = fixed_accepted_rfc(),
        ClientCreated = client_created_rfc(slow),
        %% accepted 模式要求先有提交的收件人快照（worker 转正前置）
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.msg_c2g_recipient_snapshot ",
                "(msg_id, from_id, to_gid, conv_seq, recipient_uids, created_at) ",
                "VALUES ($1, $2, $3, $4, $5::bigint[], $6)">>,
            [MsgId, 910001, 990001, 1, [910001, 910002], ClientCreated]
        ),
        ok = msg_c2g_repo:write_accepted_msg(
            ClientCreated,
            MsgId,
            <<"{\"t\":\"c2g\"}">>,
            910001,
            [910001, 910002],
            990001,
            <<"text">>,
            null,
            null,
            1,
            null,
            AcceptedAt
        ),
        Row = fetch_c2g(MsgId),
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, Row))),
        %% 新行 server_ts 对齐接受时间——不再等于客户端 created_at
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"server_ts">>, Row))),
        ?assertEqual(
            elib_dt:rfc3339_to(ClientCreated, millisecond),
            to_ms(maps:get(<<"created_at">>, Row))
        ),
        ?assert(
            to_ms(maps:get(<<"server_ts">>, Row)) =/= to_ms(maps:get(<<"created_at">>, Row))
        ),
        %% R03-4（c2g 侧）：编辑只改 payload（server_ts=CURRENT_TIMESTAMP 独立
        %% 更新机制），不得重置 accepted_at
        {ok, _} = msg_c2g_repo:update_payload_by_msg_id(MsgId, <<"{\"t\":\"edited\"}">>),
        RowAfterEdit = fetch_c2g(MsgId),
        ?assertEqual(?FIXED_MS, to_ms(maps:get(<<"accepted_at">>, RowAfterEdit))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% 读侧兼容：resolve_accepted_ms 的 trusted / low_trust / 无来源
%%%-------------------------------------------------------------------
resolve_accepted_ms_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, 1789600000000, trusted},
            msg_store_repo:resolve_accepted_ms(#{
                <<"accepted_at">> => ?FIXED_RFC,
                <<"server_ts">> => <<"2026-09-16T20:06:40Z">>,
                <<"created_at">> => <<"2026-09-16T18:06:40Z">>
            })
        ),
        %% 旧行 accepted_at NULL → 回退 server_ts（低可信，不用 now 回填）
        ?assertEqual(
            {ok, 1789589200000, low_trust},
            msg_store_repo:resolve_accepted_ms(#{
                <<"accepted_at">> => null,
                <<"server_ts">> => <<"2026-09-16T20:06:40Z">>,
                <<"created_at">> => <<"2026-09-16T18:06:40Z">>
            })
        ),
        %% server_ts 也缺失 → created_at 兜底（仍低可信）
        ?assertEqual(
            {ok, 1789582000000, low_trust},
            msg_store_repo:resolve_accepted_ms(#{
                <<"accepted_at">> => null,
                <<"server_ts">> => null,
                <<"created_at">> => <<"2026-09-16T18:06:40Z">>
            })
        ),
        ?assertEqual(
            {error, no_time_source},
            msg_store_repo:resolve_accepted_ms(#{
                <<"accepted_at">> => null, <<"server_ts">> => null, <<"created_at">> => null
            })
        )
    end).

%%%-------------------------------------------------------------------
%%% OA 直留链路（enterprise_message_repo:insert_direct_tx 同形状 SQL）：
%%% 正式表 accepted_at 随行读回并经 archive_tx 归档搬运。
%%% （oa_direct_cloud_retention_pg_tests 属专属专项套件——IMBOYENV=test +
%%% marker server 端口合同，非通用 EUNIT_MODS 入口；此处用同形状 SQL 覆盖
%%% accepted_at 贯通。）
%%%-------------------------------------------------------------------
oa_direct_archive_carries_accepted_at_test_() ->
    ?TEST_WITH_DB(fun() ->
        cleanup_test_rows(),
        MsgId = <<"tzv1_oa_direct">>,
        %% 与 insert_direct_tx 相同的列形状：accepted_at 与 server_ts 同事务 NOW()。
        %% conv_seq 取 c2c_order 写守卫认可的当前头部 +1（生产由
        %% execute_formal_tx 在锁内完成，此处测试直接复算）
        Head = c2c_conversation_head(910001, 910002),
        Seq = Head + 1,
        {ok, _} = elib_pg:execute(
            <<
                "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload,"
                " server_ts, created_at, accepted_at, conv_seq)"
                " VALUES ($1,$2,$3,$4,$5,NULL,$6::jsonb,NOW(),NOW(),NOW(),$7)"
            >>,
            [
                elib_tsid:generate(msg_c2c),
                910001,
                910002,
                MsgId,
                <<"text">>,
                <<"{\"t\":\"oa\"}">>,
                Seq
            ]
        ),
        {ok, [Row]} = elib_pg:query(
            <<"SELECT *, payload::text AS payload FROM msg_c2c WHERE msg_id = $1">>, [MsgId]
        ),
        ?assert(is_binary(maps:get(<<"accepted_at">>, Row, null))),
        %% OA 直留归档：同事务 archive_tx（与 archive_direct_tx 同调用形状）
        ok = elib_pg:with_tx(fun(Conn) ->
            msg_archive_repo:archive_tx(Conn, Row#{<<"type">> => <<"c2c">>})
        end),
        Archive = fetch_archive(MsgId),
        ?assertEqual(
            to_ms(maps:get(<<"accepted_at">>, Row)),
            to_ms(maps:get(<<"accepted_at">>, Archive))
        ),
        cleanup_test_rows()
    end).

%% 写守卫口径的会话头部（与 00000194 fn_c2c_order_write_guard 的 current_head
%% 同源：counter 与三表 max 的最大值；测试专用，不加锁）
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
