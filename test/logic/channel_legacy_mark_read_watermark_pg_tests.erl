-module(channel_legacy_mark_read_watermark_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W08（channel-group-ux-v2.2 §6.8/§10.4）：旧 mark_read 的水位适配
%%% （真实 PostgreSQL 向量）。
%%%
%%% 适配默认关闭（§10.4：新能力未达全节点部署前保持关闭，开关
%%% imboy.channel_legacy_mark_read_watermark）。mark_as_read 在默认关
%%% 下保持旧合同精确语义（只清聚合 unread_count、不触碰 view_seq，
%%% channel_view_watermark_pg_tests V3 为该合同的回归锚）；开启后由
%%% mark_as_read 经真实 config_ds 配置链承担适配。本套件保存并恢复
%%% 进程级开关，必须与其他改应用环境/meck 的套件串行运行：
%%% - 适配显式清到**当次服务端发布边界**（单语句 GREATEST：单调、
%%%   不从聚合 unread 推断、迁移前状态不洗零）；
%%% - 边界后新发布不被旧读清掉（update_seq > 边界仍计未查看）；
%%% - 新端 mark_viewed 旧边界不倒退（GREATEST 混合路径）；
%%% - 逻辑层 mark_as_read（默认关）清零 unread 且不推水位；
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
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
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
    SavedFlag = application:get_env(imboy, channel_legacy_mark_read_watermark),
    cleanup_test_rows(),
    try
        seed_test_rows(),
        ok = application:set_env(imboy, channel_legacy_mark_read_watermark, false),
        meck:new(channel_logic_notify, [no_link]),
        meck:expect(
            channel_logic_notify,
            notify_channel_unread_count,
            fun(_ChannelId, _Uid, 0) -> ok end
        ),
        repository_vectors(),
        entry_vectors(),
        unsubscribed_vector(),
        ?assert(meck:validate(channel_logic_notify)),
        ok
    after
        case lists:member(channel_logic_notify, meck:mocked()) of
            true -> meck:unload(channel_logic_notify);
            false -> ok
        end,
        case SavedFlag of
            undefined -> application:unset_env(imboy, channel_legacy_mark_read_watermark);
            {ok, Flag} -> application:set_env(imboy, channel_legacy_mark_read_watermark, Flag)
        end,
        cleanup_test_rows()
    end.

repository_vectors() ->
    ok = publish_n(?CH, 3),
    {ok, 3} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
    3 = view_seq(?CH, ?SUB),
    0 = unseen(?CH),
    ok = publish_n(?CH, 4),
    1 = unseen(?CH),
    {ok, 4} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
    0 = unseen(?CH),
    {ok, 4} = channel_subscription_repo:advance_view(?CH, ?SUB, 2),
    4 = view_seq(?CH, ?SUB),
    ok = publish_n(?CH, 5),
    {ok, 5} = channel_subscription_repo:advance_view_to_current(?CH, ?SUB),
    0 = unseen(?CH).

entry_vectors() ->
    ok = publish_n(?CH, 6),
    {ok, 1} = elib_pg:execute_count(
        <<
            "UPDATE channel_subscription SET unread_count = 9 "
            "WHERE channel_id = $1 AND user_id = $2"
        >>,
        [?CH, ?SUB]
    ),
    1 = unseen(?CH),
    ok = mark_read(?CH),
    5 = view_seq(?CH, ?SUB),
    ?assertEqual(
        {ok, #{<<"unread_count">> => 0}},
        elib_pg:one(
            <<
                "SELECT unread_count FROM channel_subscription "
                "WHERE channel_id = $1 AND user_id = $2"
            >>,
            [?CH, ?SUB]
        )
    ),
    1 = unseen(?CH),
    ok = application:set_env(imboy, channel_legacy_mark_read_watermark, true),
    ok = mark_read(?CH),
    6 = view_seq(?CH, ?SUB),
    0 = unseen(?CH),
    ok = publish_n(?CH, 7),
    1 = unseen(?CH),
    6 = view_seq(?CH, ?SUB),
    ok = mark_read(?CH),
    ok = mark_read(?CH),
    7 = view_seq(?CH, ?SUB),
    0 = unseen(?CH).

unsubscribed_vector() ->
    {error, not_subscribed} =
        channel_subscription_repo:advance_view_to_current(?CH2, ?SUB),
    ok = mark_read(?CH2),
    ?assertEqual(
        {ok, []},
        elib_pg:query(
            <<"SELECT id FROM channel_subscription WHERE channel_id = $1 AND user_id = $2">>,
            [?CH2, ?SUB]
        )
    ).

mark_read(ChannelId) ->
    channel_logic:mark_as_read(?SUB, integer_to_binary(ChannelId), <<"m">>).
