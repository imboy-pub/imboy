-module(group_creation_recovery_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W04.b（channel-group-ux-v2.2）：真实 PostgreSQL 上验证工作区建群
%%% `group_logic:workspace_add_tx/4` 在初始成员入群失败时整体回滚。
%%%
%%% mock 套件（group_logic_tests / group_scope_tests）已覆盖
%%% join_group 返回 {error,_} → join_initial_member throw({abort_tx,_}) 的
%%% 映射；本套件在真实 PG 上证明同一事务语义：DS 层
%%% ensure_workspace_membership 对非 active workspace_member 抛
%%% {abort_tx, workspace_membership_required} 后，已插入的群行随事务回滚，
%%% 不留半成品群；对照组（成员资格存在）正常落库。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(ORG_ID, 880001).
-define(CREATOR_UID, 880011).
-define(MEMBER_UID, 880012).
-define(OUTSIDER_UID, 880013).
-define(WS_ID, 880021).
-define(FIXTURE_UIDS, [?CREATOR_UID, ?MEMBER_UID, ?OUTSIDER_UID]).
-define(MEMBER_UID_BIN, <<"880012">>).
-define(OUTSIDER_UID_BIN, <<"880013">>).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    WsParams = [?WS_ID],
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM \"group\" WHERE workspace_id = $1">>, WsParams},
            {<<"DELETE FROM workspace_member WHERE workspace_id = $1">>, WsParams},
            {<<"DELETE FROM workspace WHERE id = $1">>, WsParams},
            {<<"DELETE FROM organization WHERE id = $1">>, [?ORG_ID]},
            {<<"DELETE FROM group_member WHERE user_id = ANY($1)">>, [?FIXTURE_UIDS]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?FIXTURE_UIDS]}
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
                    "($1,'w04b-pg-creator','w04b-pg-creator','w04b-synthetic','127.0.0.1','w04b-synthetic'),",
                    "($2,'w04b-pg-member','w04b-pg-member','w04b-synthetic','127.0.0.1','w04b-synthetic'),",
                    "($3,'w04b-pg-outsider','w04b-pg-outsider','w04b-synthetic','127.0.0.1','w04b-synthetic')"
                >>,
                [?CREATOR_UID, ?MEMBER_UID, ?OUTSIDER_UID]
            },
            {<<"INSERT INTO organization (id, name, owner_id) VALUES ($1,'w04b-pg-org',$2)">>, [
                ?ORG_ID, ?CREATOR_UID
            ]},
            {
                <<"INSERT INTO workspace (id, name, owner_id, organization_id, status, branding) ",
                    "VALUES ($1,'w04b-pg-ws',$2,$3,'active','{}'::jsonb)">>,
                [?WS_ID, ?CREATOR_UID, ?ORG_ID]
            },
            %% 组织成员行：资源谓词要求 workspace 归属 org 时用户为 active
            %% organization_member。owner（CREATOR_UID）由
            %% trg_organization_owner_member_sync 自动同步，这里只补 MEMBER_UID；
            %% OUTSIDER_UID 仍然故意缺席两级成员表。
            {<<"INSERT INTO organization_member (organization_id, user_id, status) VALUES ($1,$2,'active')">>,
                [?ORG_ID, ?MEMBER_UID]},
            %% 初始成员只有 MEMBER_UID 有 active 资格；OUTSIDER_UID 故意缺席。
            {<<"INSERT INTO workspace_member (workspace_id, user_id, status) VALUES ($1,$2,'active'),($1,$3,'active')">>,
                [?WS_ID, ?CREATOR_UID, ?MEMBER_UID]}
        ]
    ).

scoped_group_count() ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM \"group\" WHERE workspace_id = $1">>,
            [?WS_ID]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({scoped_group_count_failed, Other})
    end.

%%%===================================================================
%%% 用例
%%%===================================================================

workspace_member_failure_rolls_back_creation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            %% 对照组：资格齐全的初始成员 → 建群成功并落库。
            {ok, Gid} = group_logic:add(
                0, ?CREATOR_UID, 2, [?MEMBER_UID_BIN], {<<"workspace">>, ?WS_ID}
            ),
            ?assertMatch(
                {ok, [#{<<"id">> := Gid, <<"scope">> := <<"workspace">>}]},
                elib_pg:query(
                    <<"SELECT id, scope FROM \"group\" WHERE id = $1">>, [Gid]
                )
            ),
            ?assertMatch(
                {ok, [_, _]},
                elib_pg:query(
                    <<"SELECT user_id FROM group_member WHERE group_id = $1 ORDER BY user_id">>,
                    [Gid]
                )
            ),
            BaseCount = scoped_group_count(),
            ?assertEqual(1, BaseCount),

            %% 失败组：OUTSIDER_UID 无 workspace_member 资格 → DS 层
            %% throw({abort_tx, workspace_membership_required})，整个建群事务回滚；
            %% add/5 公开合同把中止原因包装为 {409, 二进制文案}。
            ?assertMatch(
                {error, {409, _}},
                group_logic:add(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?OUTSIDER_UID_BIN],
                    {<<"workspace">>, ?WS_ID}
                )
            ),
            %% 前一成功群仍在，失败群未落库（群行数不变，无半成品）。
            ?assertEqual(BaseCount, scoped_group_count()),
            ?assertMatch(
                {ok, []},
                elib_pg:query(
                    <<"SELECT id FROM \"group\" WHERE workspace_id = $1 AND id <> $2">>,
                    [?WS_ID, Gid]
                )
            ),

            %% 混合成员集：资格成员 + 无资格成员 → 首个失败即回滚，
            %% 资格成员也不落任何新群的群成员行。
            ?assertMatch(
                {error, {409, _}},
                group_logic:add(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN, ?OUTSIDER_UID_BIN],
                    {<<"workspace">>, ?WS_ID}
                )
            ),
            ?assertEqual(BaseCount, scoped_group_count()),
            %% 回滚后资格成员与无资格成员都不落任何新群的群成员行。
            {ok, []} = elib_pg:query(
                <<"SELECT user_id FROM group_member WHERE user_id IN ($1, $2) ",
                    "AND group_id <> $3">>,
                [?MEMBER_UID, ?OUTSIDER_UID, Gid]
            )
        after
            cleanup_test_rows()
        end
    end).
