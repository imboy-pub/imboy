-module(channel_legacy_mark_read_watermark_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W08（channel-group-ux-v2.2 §6.8/§10.4）：旧 mark_read 的水位适配
%%% （真实 PostgreSQL 向量）：
%%% - 旧 mark_read 经同一新水位适配器显式清到**当次服务端发布边界**
%%%   （单语句 GREATEST(view_seq, publish_seq)：单调、不从聚合 unread
%%%   推断、迁移前状态不洗零）；
%%% - 边界后新发布不被旧读清掉（update_seq > 边界仍计未查看）；
%%% - 新端 mark_viewed 旧边界不倒退（GREATEST 混合路径）；
%%% - 逻辑层旧端点端到端：clear_unread（旧合同）+ 水位适配同发；
%%% - 未订阅：适配返回 not_subscribed，旧端点静默保持 ok。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(AUTHOR, 882091).
-define(SUB, 882092).
-define(CH, 882095).
-define(CH2, 882096).
-define(ALL_CHS, [?CH, ?CH2]).
-define(ALL_UIDS, [?AUTHOR, ?SUB]).

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM channel_message WHERE channel_id = ANY($1)">>, [
                ?ALL_CHS
            ]},
            {<<"DELETE FROM channel_subscription WHERE channel_id = ANY($1)">>, [
                ?ALL_CHS
            ]},
            {<<"DELETE FROM channel WHERE id = ANY($1)">>, [?ALL_CHS]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?ALL_UIDS]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<"INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES "/utf8,
                    "($1,'w08l-author','w08l-author','w08l-synthetic','127.0.0.1','w08l-synthetic'),"/utf8,
                    "($2,'w08l-sub','w08l-sub','w08l-synthetic','127.0.0.1','w08l-synthetic')"/utf8>>,
                [?AUTHOR, ?SUB]
            },
            {<<"INSERT INTO channel (id, name, creator_uid) VALUES ($1, '旧读水位频道', $2), ($3, '未订阅频道', $4)"/utf8>>,
                [?CH, ?AUTHOR, ?CH2, ?AUTHOR]},
            {
                <<"INSERT INTO channel_subscription (id, channel_id, user_id, status) VALUES "/utf8,
                    "(882097, $1, $2, 1)">>,
                [?CH, ?SUB]
            }
        ]
    ),
    ok.

%% 直播发布 N 条（update_seq 从当前 publish_seq+1 起连续分配）并把频道
%% 发布上界推到新值；消息 id 用 unique_integer 保证同轮内唯一。
publish_n(ChannelId, N) ->
    {ok, #{<<"publish_seq">> := Prev}} =
        elib_pg:one(
            <<"SELECT publish_seq FROM channel WHERE id = $1">>, [ChannelId]
        ),
    lists:foreach(
        fun(Seq) ->
            MsgId = 882500 + erlang:unique_integer([positive]),
            {ok, _} = elib_pg:execute(
                <<
                    "INSERT INTO channel_message "
                    "(id, channel_id, author_id, author_name, content, msg_type, payload, status, revoked, update_seq) "
                    "VALUES ($1, $2, $3, 'w08l', 'u', 'text', '{}', 1, false, $4)"
                >>,
                [MsgId, ChannelId, ?AUTHOR, Seq]
            )
        end,
        lists:seq(Prev + 1, N)
    ),
    {ok, _} = elib_pg:execute(
        <<"UPDATE channel SET publish_seq = $1 WHERE id = $2">>, [N, ChannelId]
    ),
    ok.

view_seq(ChannelId, Uid) ->
    case
        elib_pg:one(
            <<
                "SELECT view_seq FROM channel_subscription "
                "WHERE channel_id = $1 AND user_id = $2"
            >>,
            [ChannelId, Uid]
        )
    of
        {ok, #{<<"view_seq">> := V}} -> V;
        Other -> error({view_seq_failed, Other})
    end.

unseen(ChannelId) ->
    {ok, #{<<"count">> := N}} =
        elib_pg:one(
            <<
                "SELECT count(*) AS count FROM channel_message "
                "WHERE channel_id = $1 AND status = 1 AND revoked = false "
                "AND update_seq > ("
                "  SELECT view_seq FROM channel_subscription "
                "  WHERE channel_id = $1 AND user_id = $2)"
            >>,
            [ChannelId, ?SUB]
        ),
    N.

channel_legacy_mark_read_watermark_vectors_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(90, fun vectors/0).

vectors() ->
    cleanup_test_rows(),
    seed_test_rows(),
    try
        %% ── V1：3 条发布 → 旧读适配推 view_seq 到当次边界 3，未查看 0。
        ok = publish_n(?CH, 3),
        {ok, 3} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
        3 = view_seq(?CH, ?SUB),
        0 = unseen(?CH),

        %% ── V2：边界后新发布不被旧读清掉——第 4 条发布后未查看 1；
        %% 再次旧读 → 边界追到 4、未查看 0。
        ok = publish_n(?CH, 4),
        {ok, _} = elib_pg:execute(
            <<"UPDATE channel SET publish_seq = 4 WHERE id = $1">>, [?CH]
        ),
        1 = unseen(?CH),
        {ok, 4} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
        0 = unseen(?CH),

        %% ── V3（旧/新混合·单调）：新端 mark_viewed(2)（旧边界）不倒退；
        %% 发布到 5 后旧读推到当次边界 5。
        {ok, 4} = channel_subscription_repo:advance_view(?CH, ?SUB, 2),
        4 = view_seq(?CH, ?SUB),
        ok = publish_n(?CH, 5),
        {ok, _} = elib_pg:execute(
            <<"UPDATE channel SET publish_seq = 5 WHERE id = $1">>, [?CH]
        ),
        {ok, 5} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
        0 = unseen(?CH),

        %% ── V4（逻辑层端到端）：mark_as_read 同发旧清零 + 水位适配。
        ok = publish_n(?CH, 6),
        {ok, _} = elib_pg:execute(
            <<"UPDATE channel SET publish_seq = 6 WHERE id = $1">>, [?CH]
        ),
        1 = unseen(?CH),
        meck:new(channel_logic_common, [no_link]),
        meck:expect(channel_logic_common, resolve_channel_id, 1, fun(_Bin) -> ?CH end),
        meck:expect(
            channel_logic_common,
            ensure_channel_content_access,
            2,
            fun(_Uid, _Ch) -> ok end
        ),
        meck:new(channel_logic_notify, [no_link]),
        meck:expect(
            channel_logic_notify,
            notify_channel_unread_count,
            3,
            fun(_Ch, _Uid, _N) -> ok end
        ),
        ok = channel_logic_message:mark_as_read(?SUB, <<"legacy">>, <<"m">>),
        meck:unload(channel_logic_common),
        meck:unload(channel_logic_notify),
        6 = view_seq(?CH, ?SUB),
        0 = unseen(?CH),
        {ok, #{<<"unread_count">> := 0}} =
            elib_pg:one(
                <<
                    "SELECT unread_count FROM channel_subscription "
                    "WHERE channel_id = $1 AND user_id = $2"
                >>,
                [?CH, ?SUB]
            ),

        %% ── V5：未订阅频道适配返回 not_subscribed；旧端点静默 ok 不炸。
        {error, not_subscribed} =
            channel_subscription_repo:advance_view_to_current(?CH2, ?SUB),
        meck:new(channel_logic_common, [no_link]),
        meck:expect(channel_logic_common, resolve_channel_id, 1, fun(_Bin) -> ?CH2 end),
        meck:expect(
            channel_logic_common,
            ensure_channel_content_access,
            2,
            fun(_Uid, _Ch) -> ok end
        ),
        meck:new(channel_logic_notify, [no_link]),
        meck:expect(
            channel_logic_notify,
            notify_channel_unread_count,
            3,
            fun(_Ch, _Uid, _N) -> ok end
        ),
        ok = channel_logic_message:mark_as_read(?SUB, <<"nosub">>, <<"m">>),
        meck:unload(channel_logic_common),
        meck:unload(channel_logic_notify),
        ok
    after
        cleanup_test_rows()
    end.
