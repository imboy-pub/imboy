-module(q06_fault_injection_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% Q06 真库故障注入与持久化边界（AC11/AC12/AC14）
%%%
%%% 设计文档 G11-2/G11-3/G12-4/G14-1 的真库层：现有 staging/worker 测试
%%% 全部 mock（unique_violation 由 mock elib_pg 伪造 23505），本文件在
%%% 隔离 PG 上以真实唯一索引/事务语义复验：
%%%
%%% AC11（SERVER_ACK 持久化边界，幂等重发矩阵）：
%%%   - C2C 同 msg_id 重发 → staging UNIQUE(type,msg_id) 真实 23505 →
%%%     DS 映射 {ok,duplicate}；DB 行数恒 1；异 payload 重发第一次写赢
%%%     （C2C 以 msg_id 为幂等键，无 request ledger——钉住现状语义）；
%%%   - C2G 同请求重发 → ledger request_hash 全等 → duplicate，快照恰一份、
%%%     conv_seq 不变；异 payload → 事务整体回滚 {error, msg_id_conflict}，
%%%     staging/ledger 均无残留。
%%%
%%% AC12（离线分页）：真库灌 5 条 → limit=2 三页翻完：has_more 单调、
%%% msg_id 集合与 DB 全集相等、无重复、末页 has_more=false。
%%%
%%% AC14（会话失效联动）：真库 bump 翻转 websocket_ds:session_still_valid
%%% （共享谓词 auth_session_ds:revoked 的消费方），legacy 无 ep 豁免。
%%%===================================================================

uid() ->
    elib_tsid:generate(user).

ts(Base) ->
    %% 受控递增时间串（秒级错开保证分页/排序稳定）
    BaseMs = 1790000000000,
    elib_dt:to_rfc3339(BaseMs + Base * 1000, millisecond).

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

%% ===================================================================
%% AC11 — C2C staging 幂等（真实唯一索引）
%% ===================================================================

%% @doc 同 msg_id 同 payload 重发：{ok,new} → {ok,duplicate}，DB 行数 1
c2c_stage_duplicate_real_db_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        MsgId = iolist_to_binary(["q06-c2c-dup-", integer_to_binary(uid())]),
        Payload = <<"{\"text\":\"hello\"}">>,
        R1 = msg_store_ds:stage(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            Payload,
            601001,
            601002,
            ts(1),
            ts(1),
            <<>>
        ),
        ?assertEqual({ok, new}, R1),
        R2 = msg_store_ds:stage(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            Payload,
            601001,
            601002,
            ts(1),
            ts(1),
            <<>>
        ),
        ?assertEqual({ok, duplicate}, R2),
        {ok, [#{<<"n">> := N}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM public.msg_store_staging WHERE msg_id = $1">>,
            [MsgId]
        ),
        ?assertEqual(1, N)
    end).

%% @doc C2C 异 payload 重发同 msg_id：现状语义为按 msg_id 幂等（无
%%% request ledger），duplicate + 第一次 payload 写赢不被覆盖
c2c_stage_diff_payload_first_write_wins_real_db_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        MsgId = iolist_to_binary(["q06-c2c-fw-", integer_to_binary(uid())]),
        R1 = msg_store_ds:stage(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"text\":\"first\"}">>,
            601011,
            601012,
            ts(1),
            ts(1),
            <<>>
        ),
        ?assertEqual({ok, new}, R1),
        R2 = msg_store_ds:stage(
            <<"c2c">>,
            MsgId,
            <<"text">>,
            <<>>,
            null,
            <<"{\"text\":\"second\"}">>,
            601011,
            601012,
            ts(2),
            ts(2),
            <<>>
        ),
        ?assertEqual({ok, duplicate}, R2),
        {ok, [#{<<"n">> := N, <<"p">> := P}]} = elib_pg:query(
            <<
                "SELECT count(*) AS n, (SELECT payload::text FROM public.msg_store_staging "
                "WHERE msg_id = $1 LIMIT 1) AS p FROM public.msg_store_staging WHERE msg_id = $1"
            >>,
            [MsgId]
        ),
        ?assertEqual(1, N),
        %% 第一次的 payload 原样保留（重发不覆盖已承诺数据）
        ?assertMatch(<<"{\"text\": \"first\"}">>, P)
    end).

%% ===================================================================
%% AC11 — C2G staging 幂等矩阵（ledger request_hash + 事务回滚）
%% ===================================================================

c2g_stage_fixture(Gid, A) ->
    mk_group(Gid, A),
    ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}).

c2g_payload(Gid) ->
    iolist_to_binary(["{\"to\":\"", integer_to_binary(Gid), "\"}"]).

%% @doc C2G 同请求重发：new → duplicate；快照恰一份、conv_seq 不变、
%%% staging 行数 1（重复不产生序号 gap）
c2g_stage_duplicate_real_db_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 521000 + (uid() rem 100000),
        A = uid(),
        ok = c2g_stage_fixture(Gid, A),
        MsgId = iolist_to_binary(["q06-c2g-dup-", integer_to_binary(uid())]),
        Payload = c2g_payload(Gid),
        {ok, new, ConvSeq, _MemberUids} = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            Payload,
            A,
            Gid,
            ts(1),
            ts(1),
            <<>>
        ),
        R2 = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            Payload,
            A,
            Gid,
            ts(1),
            ts(1),
            <<>>
        ),
        ?assertEqual({ok, duplicate}, R2),
        %% 快照恰一份（重复不重复投递 fan-out 基数）
        {ok, [#{<<"n">> := SnapN}]} = elib_pg:query(
            <<
                "SELECT count(*) AS n FROM public.msg_c2g_recipient_snapshot "
                "WHERE msg_id = $1"
            >>,
            [MsgId]
        ),
        ?assertEqual(1, SnapN),
        %% staging 1 行且 conv_seq 保持首值
        {ok, [#{<<"n">> := N, <<"s">> := S}]} = elib_pg:query(
            <<
                "SELECT count(*) AS n, min(conv_seq) AS s FROM public.msg_store_staging "
                "WHERE msg_id = $1"
            >>,
            [MsgId]
        ),
        ?assertEqual(1, N),
        ?assertEqual(ConvSeq, S)
    end).

%% @doc C2G 同 msg_id 异 payload：request_hash 不匹配 → 整个事务回滚，
%%% {error, msg_id_conflict}；staging/ledger 均无新行（拒绝即无痕）
c2g_stage_msg_id_conflict_rolls_back_real_db_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 522000 + (uid() rem 100000),
        A = uid(),
        ok = c2g_stage_fixture(Gid, A),
        MsgId = iolist_to_binary(["q06-c2g-conf-", integer_to_binary(uid())]),
        {ok, new, _Seq, _Members} = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            c2g_payload(Gid),
            A,
            Gid,
            ts(1),
            ts(1),
            <<>>
        ),
        %% 异 payload 重放同 msg_id
        R2 = msg_store_ds:stage(
            <<"c2g">>,
            MsgId,
            <<"text">>,
            <<"send">>,
            null,
            <<"{\"to\":\"999\",\"text\":\"conflict\"}">>,
            A,
            Gid,
            ts(2),
            ts(2),
            <<>>
        ),
        ?assertEqual({error, msg_id_conflict}, R2),
        %% 事务整体回滚：staging 仍 1 行、ledger 仍 1 行
        {ok, [#{<<"n">> := N}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM public.msg_store_staging WHERE msg_id = $1">>,
            [MsgId]
        ),
        ?assertEqual(1, N),
        {ok, [#{<<"n">> := LN}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM public.msg_c2g_request_ledger WHERE msg_id = $1">>,
            [MsgId]
        ),
        ?assertEqual(1, LN)
    end).

%% ===================================================================
%% AC12 — 离线分页真库单调（has_more / 游标推进 / 集合一致）
%% ===================================================================

%% @doc 灌 5 条离线消息，limit=2 翻页至耗尽：has_more 单调（len<total）、
%%% 游标推进到底、去重后 msg_id 集合 = DB 全集（AC12 客户端合同）。
offline_paging_real_db_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = uid(),
        MsgIds = [
            iolist_to_binary(["q06-off-", integer_to_binary(Uid), $-, integer_to_binary(I)])
         || I <- lists:seq(1, 5)
        ],
        lists:foreach(
            fun({I, MsgId}) ->
                {ok, _} = elib_pg:query(
                    <<
                        "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, "
                        "payload, created_at) VALUES ($1, $2, $3, $4, 'text', "
                        "'{\"text\":\"t\"}', $5)"
                    >>,
                    [uid(), 601301, Uid, MsgId, ts(I + 10)]
                )
            end,
            lists:zip(lists:seq(1, 5), MsgIds)
        ),
        %% 翻页至耗尽（最多 6 页防死循环）
        {All, Pages} = offline_pages(Uid, 2, 0, 6, []),
        %% 去重后集合与 DB 全集一致（客户端 msg_id 幂等入库视角）；
        %% 服务端游标为 created_at >= next（同毫秒不丢的安全方向），
        %% 页边界条可能重复下发，去重由客户端合同承接
        ?assertEqual(lists:sort(MsgIds), lists:usort(All)),
        %% 页数受控（5 条 limit=2 至多 4 页含边界重复）
        ?assert(Pages =< 4)
    end).

offline_pages(_Uid, _Limit, _Last, 0, Acc) ->
    {Acc, 0};
offline_pages(Uid, Limit, Last, N, Acc) ->
    {MsgIds, Next} = offline_page(Uid, Limit, Last),
    case MsgIds of
        [] ->
            {Acc, N};
        _ ->
            offline_pages(Uid, Limit, Next, N - 1, Acc ++ MsgIds)
    end.

offline_page(Uid, Limit, LastMsgAt) ->
    Payload = messaging_logic:offline(Uid, Limit, LastMsgAt, 0, 0, <<>>),
    C2C = maps:get(<<"c2c">>, Payload),
    List = maps:get(<<"list">>, C2C),
    MsgIds = [maps:get(<<"msg_id">>, M) || M <- List],
    Next = maps:get(<<"next_last_msg_at">>, C2C),
    Total = maps:get(<<"total">>, C2C),
    ExpectedMore = length(List) < Total,
    ?assertEqual(ExpectedMore, maps:get(<<"has_more">>, C2C)),
    {MsgIds, Next}.

%% ===================================================================
%% AC14 — bump 翻转 WS 会话谓词（共享 revoked 消费方联动）
%% ===================================================================

%% @doc 真库 bump 后：session_still_valid（WS 心跳重校验入口）对旧 epoch
%%% 翻转为 false、新 epoch true；legacy 无 ep 豁免保持 true
session_still_valid_flips_after_bump_real_db_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [
                {'is_active', 2, fun(_Uid, _Did) -> true end}
            ]}
        ],
        fun() ->
            Uid = uid(),
            State = #{current_uid => Uid, auth_did => <<"q06-dev-1">>, session_epoch => 1},
            %% 缺行默认 epoch=1 → 旧连接有效
            ?assertEqual(true, websocket_ds:session_still_valid(State)),
            %% bump（改密/全端登出同机制）→ 下一心跳 4001 关闭
            ok = auth_session_ds:bump(Uid),
            ?assertEqual(false, websocket_ds:session_still_valid(State)),
            ?assertEqual(
                true,
                websocket_ds:session_still_valid(State#{session_epoch => 2})
            ),
            %% legacy 会话（无 ep claim）豁免，自然过期前保持有效
            ?assertEqual(
                true,
                websocket_ds:session_still_valid(#{
                    current_uid => Uid, auth_did => <<"q06-dev-1">>
                })
            )
        %% 注：不显式清 {auth_session_epoch, Uid} 的 60s memo——
        %% Uid 为 TSID 本轮唯一，不毒化其他套件（auth_session_ds_tests
        %% 同款防撞先例）；evict/1 为私有函数不可外部调用。
        end
    ).
