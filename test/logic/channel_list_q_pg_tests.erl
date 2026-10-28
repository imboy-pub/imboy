-module(channel_list_q_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W07.a（channel-group-ux-v2.2 §6.5 C2 已订阅/我管理频道）：真实
%%% PostgreSQL 上验证 subscribed/managed 列表的可选服务端 q：
%%% - q 只在各自授权交集（订阅 / 管理角色）内过滤，绝不放宽；
%%% - q 为空（arity 旧调用）与 <<>> 行为一致，全量形状/排序零变化；
%%% - LIKE 通配注入被 escape_like 中和；
%%% - 管理集合不是订阅集合（managed 命中不产生订阅副作用）。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880501).
-define(OTHER_UID, 880502).
%% 产品频道 creator=UID，UID 为管理员
-define(C1, 880601).
%% 研发频道 creator=OTHER，UID 已订阅
-define(C2, 880602).
%% 产品外部频道 creator=OTHER，UID 无任何关系
-define(C3, 880603).
-define(ALL_CIDS, [?C1, ?C2, ?C3]).
-define(ALL_UIDS, [?UID, ?OTHER_UID]).
-define(SUB_ID, 880611).
-define(ADMIN_ID, 880612).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM channel_subscription WHERE channel_id = ANY($1)">>, [?ALL_CIDS]},
            {<<"DELETE FROM channel_admin WHERE channel_id = ANY($1)">>, [?ALL_CIDS]},
            {<<"DELETE FROM channel WHERE id = ANY($1)">>, [?ALL_CIDS]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?ALL_UIDS]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<
                    "INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES ",
                    "($1,'w07a-ch-uid','w07a-ch-uid','w07a-synthetic','127.0.0.1','w07a-synthetic'),",
                    "($2,'w07a-ch-other','w07a-ch-other','w07a-synthetic','127.0.0.1','w07a-synthetic')"
                >>,
                [?UID, ?OTHER_UID]
            },
            {
                %% ⚠️ 含 CJK 的分段各自 /utf8（低字节截断 → 22021，W04.c 教训）。
                <<
                    "INSERT INTO channel (id, name, creator_uid) VALUES ",
                    "($1, '产品频道', $4),"/utf8,
                    "($2, '研发频道', $5),"/utf8,
                    "($3, '产品外部频道', $5)"/utf8
                >>,
                [?C1, ?C2, ?C3, ?UID, ?OTHER_UID]
            },
            {
                <<"INSERT INTO channel_subscription (id, channel_id, user_id, status) ",
                    "VALUES ($1, $2, $3, 1)">>,
                [?SUB_ID, ?C2, ?UID]
            },
            {
                <<"INSERT INTO channel_admin (id, channel_id, user_id, role) ",
                    "VALUES ($1, $2, $3, 2)">>,
                [?ADMIN_ID, ?C1, ?UID]
            }
        ]
    ),
    ok.

ids({ok, Rows}) when is_list(Rows) ->
    [maps:get(<<"id">>, R) || R <- Rows, is_map(R)];
ids(Other) ->
    error({unexpected_list_shape, Other}).

%%%===================================================================
%%% 用例
%%%===================================================================

channel_list_q_scope_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),

            %% 1) subscribed：旧调用（arity-2）与 <<>> 一致；仅已订阅 C2。
            ?assertEqual(
                ids(channel_repo:list_subscribed(?UID, <<"id">>)),
                ids(channel_repo:list_subscribed(?UID, <<"id">>, <<>>))
            ),
            ?assertEqual([?C2], ids(channel_repo:list_subscribed(?UID, <<"id">>))),

            %% 2) subscribed：q 授权交集内过滤——C1 是管理面命中、C3 是外部
            %% 命中，都不得因 q='产品' 出现在订阅列表。
            ?assertEqual(
                [],
                ids(channel_repo:list_subscribed(?UID, <<"id">>, <<"产品"/utf8>>))
            ),
            ?assertEqual(
                [?C2],
                ids(channel_repo:list_subscribed(?UID, <<"id">>, <<"研发"/utf8>>))
            ),

            %% 3) managed：旧调用与 <<>> 一致；仅管理 C1。
            ?assertEqual(
                ids(channel_repo:list_managed(?UID)),
                ids(channel_repo:list_managed(?UID, <<>>))
            ),
            ?assertEqual([?C1], ids(channel_repo:list_managed(?UID))),

            %% 4) managed：q='研发' → 空（C2 是订阅面命中，不属于管理集合
            %% ——两集合互不越界）；q='产品' → C1；C3 外部不出现。
            ?assertEqual(
                [],
                ids(channel_repo:list_managed(?UID, <<"研发"/utf8>>))
            ),
            ?assertEqual(
                [?C1],
                ids(channel_repo:list_managed(?UID, <<"产品"/utf8>>))
            ),
            ?assertEqual(
                [],
                ids(channel_repo:list_managed(?UID, <<"外部"/utf8>>))
            ),

            %% 5) LIKE 通配注入被中和。
            ?assertEqual(
                [],
                ids(channel_repo:list_subscribed(?UID, <<"id">>, <<"%"/utf8>>))
            ),

            %% 6) logic 层（handler 实际入口）：q 命中返回传输形状，
            %% name 保留；q 未命中返回空列表。
            {ok, Hit} = channel_logic:get_subscribed_channels(?UID, <<"研发"/utf8>>),
            ?assertMatch(
                [#{<<"name">> := <<"研发频道"/utf8>>}],
                Hit
            ),
            ?assertMatch(
                {ok, []},
                channel_logic:get_managed_channels(?UID, <<"研发"/utf8>>)
            ),

            %% 7) 管理命中不产生订阅副作用（管理集合不是订阅集合）。
            {ok, _} = channel_repo:list_managed(?UID, <<"产品"/utf8>>),
            {ok, Subs} = channel_repo:list_subscribed(?UID, <<"id">>),
            ?assertEqual([?C2], ids({ok, Subs}))
        after
            cleanup_test_rows()
        end
    end).
