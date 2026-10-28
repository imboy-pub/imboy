-module(channel_view_watermark_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W08（channel-group-ux-v2.2 §6.8 C4/D3 查看更新边界）：真实 PostgreSQL
%%% 上验证单一更新边界水位语义：
%%% - 发布单调序列：update_seq 同事务分配、逐帖递增；幂等重放不换序号；
%%% - 作者自看：作者新发布同事务推进其 view_seq（不计自己未查看）；
%%% - GREATEST：晚到旧边界不倒退；超上界夹到 publish_seq；
%%% - 重订阅：activation_version 递增且 view_seq 重置（J13.e）；
%%% - 旧 clear_unread 只清 unread_count、不触碰 view_seq（非精确水位）；
%%% - count_unseen 排除撤销/删除项。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(AUTHOR, 880901).
-define(SUBSCRIBER, 880902).
-define(STRANGER, 880903).
-define(CH, 880910).
-define(CH2, 880911).
-define(ALL_UIDS, [?AUTHOR, ?SUBSCRIBER, ?STRANGER]).
-define(ALL_CHS, [?CH, ?CH2]).

%%%===================================================================
%%% 夹具
%%%===================================================================

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
                    "($1,'w08-author','w08-author','w08-synthetic','127.0.0.1','w08-synthetic'),"/utf8,
                    "($2,'w08-sub','w08-sub','w08-synthetic','127.0.0.1','w08-synthetic'),"/utf8,
                    "($3,'w08-stranger','w08-stranger','w08-synthetic','127.0.0.1','w08-synthetic')"/utf8>>,
                [?AUTHOR, ?SUBSCRIBER, ?STRANGER]
            },
            {<<"INSERT INTO channel (id, name, creator_uid) VALUES ($1, '水位频道一', $2), ($3, '水位频道二', $4)"/utf8>>,
                [
                    ?CH, ?AUTHOR, ?CH2, ?AUTHOR
                ]},
            {
                <<"INSERT INTO channel_subscription (id, channel_id, user_id, status) VALUES "/utf8,
                    "(881910, $1, $2, 1), (881911, $1, $3, 1)">>,
                [?CH, ?AUTHOR, ?SUBSCRIBER]
            }
        ]
    ),
    ok.

sub_row(ChannelId, Uid) ->
    case
        elib_pg:one(
            <<
                "SELECT view_seq, activation_version, unread_count FROM channel_subscription "
                "WHERE channel_id = $1 AND user_id = $2"
            >>,
            [ChannelId, Uid]
        )
    of
        {ok, Row} -> Row;
        Other -> error({sub_row_failed, Other})
    end.

msg_seq(MessageId) ->
    case
        elib_pg:one(
            <<"SELECT update_seq FROM channel_message WHERE id = $1">>, [MessageId]
        )
    of
        {ok, #{<<"update_seq">> := Seq}} -> Seq;
        Other -> error({msg_seq_failed, Other})
    end.

%%%===================================================================
%%% 用例
%%%===================================================================

channel_view_watermark_vectors_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(90, fun watermark_vectors/0).

watermark_vectors() ->
    cleanup_test_rows(),
    try
        seed_test_rows(),

        %% ── V1：发布单调序列 + 同事务作者自看 + 订阅者未读累加。
        {ok, M1} =
            channel_ds:publish_message(?CH, ?AUTHOR, <<"第一条"/utf8>>, <<"text">>, #{}),
        {ok, M2} =
            channel_ds:publish_message(?CH, ?AUTHOR, <<"第二条"/utf8>>, <<"text">>, #{}),
        ?assertMatch(1, msg_seq(M1)),
        ?assertMatch(2, msg_seq(M2)),
        AuthorSub = sub_row(?CH, ?AUTHOR),
        %% 作者自己的新发布不计其未查看：view_seq 已推进到最新序号。
        ?assertMatch(#{<<"view_seq">> := 2}, AuthorSub),
        ?assertMatch(#{<<"unread_count">> := 0}, AuthorSub),
        SubscriberSub0 = sub_row(?CH, ?SUBSCRIBER),
        ?assertMatch(#{<<"view_seq">> := 0}, SubscriberSub0),
        ?assertMatch(#{<<"unread_count">> := 2}, SubscriberSub0),
        ?assertMatch(#{<<"activation_version">> := 0}, SubscriberSub0),

        %% ── V2：advance_view GREATEST——推进到 1、晚到旧边界(0)不倒退、
        %% 超上界(999)夹到 publish_seq=2。
        {ok, 1} =
            channel_subscription_repo:advance_view(?CH, ?SUBSCRIBER, 1),
        ?assertMatch(
            #{<<"view_seq">> := 1}, sub_row(?CH, ?SUBSCRIBER)
        ),
        {ok, 1} =
            channel_subscription_repo:advance_view(?CH, ?SUBSCRIBER, 0),
        {ok, 2} =
            channel_subscription_repo:advance_view(?CH, ?SUBSCRIBER, 999),
        ?assertMatch(
            #{<<"view_seq">> := 2}, sub_row(?CH, ?SUBSCRIBER)
        ),
        {ok, 0} = channel_subscription_repo:count_unseen(?CH, 2),

        %% ── V3：旧 clear_unread 只清 unread_count，不触碰 view_seq
        %%（§10：不把原 clear_unread 当新版精确水位）。
        {ok, M3} =
            channel_ds:publish_message(?CH, ?AUTHOR, <<"第三条"/utf8>>, <<"text">>, #{}),
        ?assertMatch(3, msg_seq(M3)),
        ?assertMatch(
            %% view 推进不清遗留 unread_count（两套口径独立；unread 保留至
            %% 新版权威重算完成）：M1..M3 累计 3。
            #{<<"unread_count">> := 3, <<"view_seq">> := 2},
            sub_row(?CH, ?SUBSCRIBER)
        ),
        SavedFlag = application:get_env(imboy, channel_legacy_mark_read_watermark),
        try
            ok = application:set_env(imboy, channel_legacy_mark_read_watermark, false),
            ok = channel_logic:mark_as_read(
                ?SUBSCRIBER, erlang:integer_to_binary(?CH), <<>>
            )
        after
            case SavedFlag of
                undefined -> application:unset_env(imboy, channel_legacy_mark_read_watermark);
                {ok, Flag} -> application:set_env(imboy, channel_legacy_mark_read_watermark, Flag)
            end
        end,
        ?assertMatch(
            #{<<"unread_count">> := 0, <<"view_seq">> := 2},
            sub_row(?CH, ?SUBSCRIBER)
        ),

        %% ── V4：count_unseen 排除撤销与删除项。
        {ok, M4} =
            channel_ds:publish_message(?CH, ?AUTHOR, <<"第四条"/utf8>>, <<"text">>, #{}),
        ?assertMatch(4, msg_seq(M4)),
        {ok, 1} = channel_subscription_repo:count_unseen(?CH, 3),
        {ok, _} = elib_pg:execute(
            <<"UPDATE channel_message SET revoked = true WHERE id = $1">>, [M4]
        ),
        {ok, 0} = channel_subscription_repo:count_unseen(?CH, 3),

        %% ── V5：重订阅——activation_version 递增且 view_seq 重置
        %%（复用订阅行 id 不复用旧查看资格，J13.e）。
        _ = elib_pg:execute(
            <<
                "UPDATE channel_subscription SET status = 0 "
                "WHERE channel_id = $1 AND user_id = $2"
            >>,
            [?CH, ?SUBSCRIBER]
        ),
        {ok, true} =
            elib_pg:with_tx(fun(Conn) ->
                channel_subscription_repo:upsert_active(Conn, ?CH, ?SUBSCRIBER)
            end),
        Resub = sub_row(?CH, ?SUBSCRIBER),
        ?assertMatch(#{<<"activation_version">> := 1}, Resub),
        ?assertMatch(#{<<"view_seq">> := 0}, Resub),

        %% ── V6：未订阅者推进被拒（not_subscribed）。
        ?assertMatch(
            {error, not_subscribed},
            channel_subscription_repo:advance_view(?CH, ?STRANGER, 1)
        ),

        %% ── V7：mark_viewed logic 层——边界夹取（999→4）+ 无效边界拒绝。
        {ok, #{view_seq := VS, unseen_count := Unseen}} =
            channel_logic:mark_viewed(
                ?SUBSCRIBER, erlang:integer_to_binary(?CH), <<"999">>
            ),
        ?assertEqual(4, VS),
        ?assertEqual(0, Unseen),
        ?assertMatch(
            {error, _},
            channel_logic:mark_viewed(
                ?SUBSCRIBER, erlang:integer_to_binary(?CH), <<"abc">>
            )
        ),

        %% ── V8：幂等重放不换序号（duplicate 返回原消息）。
        ReqId = <<"w08-idem-req-1">>,
        {ok, Mid, inserted} =
            channel_ds:publish_message(
                ?CH2, ?AUTHOR, <<"幂等一条"/utf8>>, <<"text">>, #{}, ReqId
            ),
        SeqBefore = msg_seq(Mid),
        {ok, Mid, duplicate} =
            channel_ds:publish_message(
                ?CH2, ?AUTHOR, <<"幂等一条"/utf8>>, <<"text">>, #{}, ReqId
            ),
        ?assertEqual(SeqBefore, msg_seq(Mid)),

        %% ── V9：多频道序列独立（CH2 序号从 1 起，不与 CH 共享计数器）。
        ?assertMatch(1, msg_seq(Mid)),

        ok
    after
        cleanup_test_rows()
    end.
