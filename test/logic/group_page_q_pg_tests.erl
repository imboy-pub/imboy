-module(group_page_q_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W07.a（channel-group-ux-v2.2 §6.5 C2 个人群）：真实 PostgreSQL 上验证
%%% group/page 三类 attr（owner/join/manager）的服务端 q 与稳定排序：
%%% - q 只在授权交集内缩小结果（尾页私有对象可达；绝不放宽授权）；
%%% - 稳定 tie-break created_at DESC, id DESC（同刻创建的群翻页不漏/重）；
%%% - LIKE 通配符注入被 escape_like 中和；
%%% - q 缺省（arity-3 旧调用）行为与 <<>> 一致（旧客户端零变化）。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880301).
-define(OTHER_UID, 880302).
%% 产品群   owner=UID  created_at = T0
-define(G1, 880401).
%% 研发群   owner=UID  created_at = T0（与 G1 同刻）
-define(G2, 880402).
%% 产品外部 owner=OTHER（UID 非成员）
-define(G3, 880403).
%% 外部群   owner=OTHER，UID 为普通成员
-define(G4, 880404).
-define(ALL_GIDS, [?G1, ?G2, ?G3, ?G4]).
-define(ALL_UIDS, [?UID, ?OTHER_UID]).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM group_member WHERE group_id = ANY($1)">>, [?ALL_GIDS]},
            {<<"DELETE FROM \"group\" WHERE id = ANY($1)">>, [?ALL_GIDS]},
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
                    "($1,'w07a-pg-uid','w07a-pg-uid','w07a-synthetic','127.0.0.1','w07a-synthetic'),",
                    "($2,'w07a-pg-other','w07a-pg-other','w07a-synthetic','127.0.0.1','w07a-synthetic')"
                >>,
                [?UID, ?OTHER_UID]
            },
            {
                %% ⚠️ 含 CJK 的分段必须各自 /utf8（低字节截断 → 22021，
                %% 见 W04.c 教训）。
                <<
                    "INSERT INTO \"group\" (id, title, owner_uid, creator_uid, type, created_at) VALUES ",
                    "($1, '产品群', $4, $4, 2, '2026-01-01 00:00:00+00'),"/utf8,
                    "($2, '研发群', $4, $4, 2, '2026-01-01 00:00:00+00'),"/utf8,
                    "($3, '产品外部群', $5, $5, 2, '2026-01-02 00:00:00+00'),"/utf8,
                    "($6, '外部群', $5, $5, 2, '2026-01-03 00:00:00+00')"/utf8
                >>,
                [?G1, ?G2, ?G3, ?UID, ?OTHER_UID, ?G4]
            },
            {
                %% group_member.id 无默认（TSID 由应用侧生成）→ 显式合成值。
                <<"INSERT INTO group_member (id, group_id, user_id, role, status) ",
                    "VALUES (880411, $1, $2, 1, 1)">>,
                [?G4, ?UID]
            }
        ]
    ),
    ok.

ids(#{list := Rows}) when is_list(Rows) ->
    [row_id(R) || R <- Rows];
ids(Other) ->
    error({unexpected_page_shape, Other}).

%% repo 分页行可能是 proplist 或 map（page_transfer 前的原始形状）。
row_id(Row) when is_list(Row) -> proplists:get_value(<<"id">>, Row);
row_id(Row) when is_map(Row) -> maps:get(<<"id">>, Row).

%%%===================================================================
%%% 用例
%%%===================================================================

group_page_q_scope_and_stability_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),

            %% 1) owner：q 缺省（arity-3 旧调用）与 <<>> 行为一致。
            {ok, DefPage} = group_ds:page_by_owner(?UID, 1, 10),
            {ok, BlankPage} = group_ds:page_by_owner(?UID, 1, 10, <<>>),
            ?assertEqual(ids(BlankPage), ids(DefPage)),
            %% 稳定 tie-break：G1/G2 同刻创建 → created_at DESC, id DESC。
            ?assertEqual([?G2, ?G1], ids(BlankPage)),

            %% 2) owner：q='产品' 授权交集内过滤 → 仅 G1（G2 不含词；G3 非
            %% 本人所有，即使标题命中也不得出现）。
            {ok, Owned} = group_ds:page_by_owner(?UID, 1, 10, <<"产品"/utf8>>),
            ?assertEqual([?G1], ids(Owned)),

            %% 3) join：q 不放宽授权——G3 标题命中 '产品' 但 UID 非成员 →
            %% 不出现；'外部' 命中成员群 G4。
            {ok, JoinedBlank} = group_ds:page_joined(?UID, 1, 10, <<>>),
            ?assertEqual([?G4], ids(JoinedBlank)),
            ?assertMatch(
                {ok, #{list := []}},
                group_ds:page_joined(?UID, 1, 10, <<"产品"/utf8>>)
            ),
            {ok, JoinedHit} = group_ds:page_joined(?UID, 1, 10, <<"外部"/utf8>>),
            ?assertEqual([?G4], ids(JoinedHit)),

            %% 4) manager：UID 非任何群管理角色 → 空；G4 仅是普通成员
            %% （role=1 < 3）不得因 q 命中出现。
            ?assertMatch(
                {ok, #{list := []}},
                group_ds:page_managed(?UID, 1, 10, <<"外部"/utf8>>)
            ),

            %% 5) LIKE 通配注入被中和：'%' 只当字面字符，不放大结果集。
            ?assertMatch(
                {ok, #{list := []}},
                group_ds:page_by_owner(?UID, 1, 10, <<"%"/utf8>>)
            ),

            %% 6) 前后空白 trim：' 产品 ' 等价 '产品'。
            {ok, Trimmed} = group_ds:page_by_owner(?UID, 1, 10, <<" 产品 "/utf8>>),
            ?assertEqual([?G1], ids(Trimmed)),

            %% 7) repo 直查稳定排序形状不变（同 W07.a 目标序）。
            {ok, RepoShape} = group_repo:page(
                1,
                10,
                #{status => 1, owner_uid => ?UID},
                <<"created_at DESC, id DESC">>
            ),
            ?assertEqual(
                [?G2, ?G1],
                [row_id(R) || R <- maps:get(list, RepoShape)]
            )
        after
            cleanup_test_rows()
        end
    end).
