-module(group_creation_recovery_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W04.b（channel-group-ux-v2.2）：真实 PostgreSQL 上验证工作区建群
%%% `group_logic:workspace_add_tx/4` 在初始成员入群失败时整体回滚。
%%%
%%% mock 套件（group_logic_tests / group_scope_tests）已覆盖
%%% 业务中止及异常返回引发 badmatch 时，事务包装均回滚并返回错误；
%%% 本套件在真实 PG 上证明同一事务语义：DS 层
%%% ensure_workspace_membership 对非 active workspace_member 抛
%%% {abort_tx, workspace_membership_required} 后，已插入的群行随事务回滚，
%%% 不留半成品群；对照组（成员资格存在）正常落库。
%%%
%%% W04.b（§6.4 C1）扩展：resource_create_receipt 受理合同——首受理同事务
%%% （初始名+成员+receipt+持久通知入队）、重放不重效（J23.b/c）、
%%% recover_only 只查零副作用（§6.4 恢复状态表）、成员失败全回滚（J23.a）、
%%% 解散删除标识与 7d 服务端过期窗口（J24.b）、规范等价/冲突（J24.c）。
%%% 规范摘要纯函数性质单测不依赖 DB。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(ORG_ID, 880001).
-define(CREATOR_UID, 880011).
-define(MEMBER_UID, 880012).
-define(OUTSIDER_UID, 880013).
-define(MEMBER2_UID, 880014).
-define(WS_ID, 880021).
-define(FIXTURE_UIDS, [?CREATOR_UID, ?MEMBER_UID, ?OUTSIDER_UID, ?MEMBER2_UID]).
-define(MEMBER_UID_BIN, <<"880012">>).
-define(OUTSIDER_UID_BIN, <<"880013">>).
-define(MEMBER2_UID_BIN, <<"880014">>).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    WsParams = [?WS_ID],
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM resource_create_receipt WHERE uid = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM msg_store_staging WHERE from_id = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM msg_s2c WHERE from_id = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM group_log WHERE option_uid = $1">>, [?CREATOR_UID]},
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
                    "($3,'w04b-pg-outsider','w04b-pg-outsider','w04b-synthetic','127.0.0.1','w04b-synthetic'),",
                    "($4,'w04b-pg-member2','w04b-pg-member2','w04b-synthetic','127.0.0.1','w04b-synthetic')"
                >>,
                [?CREATOR_UID, ?MEMBER_UID, ?OUTSIDER_UID, ?MEMBER2_UID]
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
            {<<"INSERT INTO organization_member (organization_id, user_id, status) VALUES ($1,$2,'active')">>,
                [?ORG_ID, ?MEMBER2_UID]},
            %% 初始成员只有 MEMBER_UID/MEMBER2_UID 有 active 资格；OUTSIDER_UID 故意缺席。
            {<<"INSERT INTO workspace_member (workspace_id, user_id, status) VALUES ($1,$2,'active'),($1,$3,'active'),($1,$4,'active')">>,
                [
                    ?WS_ID, ?CREATOR_UID, ?MEMBER_UID, ?MEMBER2_UID
                ]}
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

%%%===================================================================
%%% W04.b（channel-group-ux v2.2 §6.4 C1）：受理回执合同
%%%===================================================================

%% 规范载荷摘要纯函数：确定性、64hex、字段敏感性（顺序/去重语义由
%% normalize 端到端用例覆盖）。
canonical_digest_properties_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = #{
            title => <<"  规范名  ">>,
            scope => <<"workspace">>,
            workspace_id => 42,
            members => [2, 3, 5]
        },
        D1 = resource_create_receipt_repo:canonical_group_digest(Base),
        D2 = resource_create_receipt_repo:canonical_group_digest(Base),
        ?assertEqual(D1, D2),
        ?assertMatch({match, _}, re:run(D1, "^[0-9a-f]{64}$")),
        %% 敏感性：标题/scope/workspace 任一变化即不同摘要
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_group_digest(Base#{title => <<"其他名">>})
        ),
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_group_digest(Base#{scope => <<"personal">>})
        ),
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_group_digest(Base#{workspace_id => 43})
        ),
        %% personal 的 workspace_id 段为空：0 与任意值同摘要（不在规范形式内）
        ?assertEqual(
            resource_create_receipt_repo:canonical_group_digest(Base#{
                scope => <<"personal">>, workspace_id => 0
            }),
            resource_create_receipt_repo:canonical_group_digest(Base#{
                scope => <<"personal">>, workspace_id => 999
            })
        ),
        %% sha256_hex 确定性与小写
        ?assertEqual(
            resource_create_receipt_repo:sha256_hex(<<>>),
            <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>
        )
    end).

%% J04.b / J23.b：首受理（同事务初始名+成员+receipt+通知）→ 重放同 ID 同
%% 载荷不重效（资源/成员/回执行/通知计数全不变）；顺序不同+重复成员不构成
%% 新意图；真实载荷变化才 request_id_conflict。
receipt_first_accept_replay_conflict_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            Members = [?MEMBER_UID_BIN, ?MEMBER2_UID_BIN],
            {ok, Gid, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                Members,
                {<<"workspace">>, ?WS_ID},
                <<"req-acc-1">>,
                <<"W04b初始名"/utf8>>,
                false
            ),
            %% J04.b：初始名与成员同事务落库
            ?assertMatch(
                {ok, [#{<<"title">> := <<"W04b初始名"/utf8>>, <<"scope">> := <<"workspace">>}]},
                elib_pg:query(
                    <<"SELECT title, scope FROM \"group\" WHERE id = $1">>, [Gid]
                )
            ),
            {ok, MemberRows} = elib_pg:query(
                <<"SELECT user_id FROM group_member WHERE group_id = $1 ORDER BY user_id">>,
                [Gid]
            ),
            ?assertEqual(3, length(MemberRows)),
            %% receipt 已受理且未删除、未过期
            ?assertMatch(
                {ok, [#{<<"resource_id">> := Gid, <<"resource_deleted">> := false}]},
                receipt_row(<<"req-acc-1">>)
            ),
            %% 初始成员各一份持久通知（效果身份计数=成员数）
            N1 = msg_effect_count(Gid),
            ?assertEqual(length(Members), N1),

            %% J23.b：重放（响应丢失场景）——同 ID 同载荷
            ?assertEqual(
                {ok, Gid, replayed},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Members,
                    {<<"workspace">>, ?WS_ID},
                    <<"req-acc-1">>,
                    <<"W04b初始名"/utf8>>,
                    false
                )
            ),
            ?assertEqual(1, scoped_group_count()),
            ?assertEqual(3, group_member_count(Gid)),
            ?assertEqual(1, receipt_count()),
            %% 不重复通知（J23.c 业务邀请仅一份；staging/promoted 联合计数）
            ?assertEqual(N1, msg_effect_count(Gid)),

            %% J24.c：顺序不同 + 重复成员 → 规范等价，仍是 replayed
            ?assertEqual(
                {ok, Gid, replayed},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER2_UID_BIN, ?MEMBER_UID_BIN, ?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-acc-1">>,
                    <<"W04b初始名"/utf8>>,
                    false
                )
            ),
            ?assertEqual(N1, msg_effect_count(Gid)),

            %% J24.c：同键真实载荷变化（成员集不同）→ conflict，不剥键不换键
            ?assertEqual(
                {error, request_id_conflict},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?OUTSIDER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-acc-1">>,
                    <<"W04b初始名"/utf8>>,
                    false
                )
            ),
            ?assertEqual(1, scoped_group_count()),
            ?assertEqual(N1, msg_effect_count(Gid))
        after
            cleanup_test_rows()
        end
    end).

%% §6.4 恢复状态表：recover_only 只查——未知键 create_not_accepted 且零
%% 副作用（不新增占位/资源/通知）；同键随后仍可首次受理（迟到请求仍可
%% 首次受理一次）；已知键只查返回同 ID replayed；只查载荷变化同样 conflict。
receipt_recover_only_semantics_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            %% 未知键只查：create_not_accepted + 零副作用
            ?assertEqual(
                {error, create_not_accepted},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-ro-1">>,
                    <<"只查名"/utf8>>,
                    true
                )
            ),
            ?assertEqual(0, receipt_count()),
            ?assertEqual(0, scoped_group_count()),

            %% 同键正常提交：迟到场景仍可首次受理一次
            {ok, Gid, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-ro-1">>,
                <<"只查名"/utf8>>,
                false
            ),
            ?assertEqual(1, receipt_count()),

            %% 已知键只查：同 ID replayed（无新资源/通知）
            N1 = msg_effect_count(Gid),
            ?assertEqual(
                {ok, Gid, replayed},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-ro-1">>,
                    <<"只查名"/utf8>>,
                    true
                )
            ),
            ?assertEqual(1, scoped_group_count()),
            ?assertEqual(N1, msg_effect_count(Gid)),

            %% 只查路径同样拒绝载荷漂移
            ?assertEqual(
                {error, request_id_conflict},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-ro-1">>,
                    <<"别的名"/utf8>>,
                    true
                )
            )
        after
            cleanup_test_rows()
        end
    end).

%% J23.a：提交前失败（成员资格不足）→ 资源/成员/receipt/通知全部回滚，
%% 同键之后仍可首次受理。
receipt_member_failure_rolls_back_receipt_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            ?assertMatch(
                {error, {409, _}},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?OUTSIDER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-fail-1">>,
                    <<"失败名"/utf8>>,
                    false
                )
            ),
            %% 全回滚：无资源、无 receipt、无通知副作用
            ?assertEqual(0, scoped_group_count()),
            ?assertEqual(0, receipt_count()),
            %% 同键可再次首次受理
            {ok, Gid, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-fail-1">>,
                <<"失败名"/utf8>>,
                false
            ),
            ?assertMatch(
                {ok, [#{<<"resource_id">> := Gid}]}, receipt_row(<<"req-fail-1">>)
            ),
            ?assertEqual(1, msg_effect_count(Gid))
        after
            cleanup_test_rows()
        end
    end).

%% J24.b：删除例外与 7d 服务端时钟——解散同事务置位 resource_deleted，
%% 重放/只查不复活对象、不泄露内容；服务端受理年龄独立决定完整结果，
%% 过 7d 只给核对元信息（create_recovery_expired），receipt 仍阻止重建。
receipt_delete_and_expiry_window_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            {ok, Gid, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-del-1">>,
                <<"待删名"/utf8>>,
                false
            ),
            %% 群主解散（同事务置位删除标识）
            ok = group_logic:dissolve(?CREATOR_UID, Gid),
            ?assertMatch(
                {ok, [#{<<"resource_deleted">> := true}]}, receipt_row(<<"req-del-1">>)
            ),
            %% 删除例外优先：重放/只查都不复活、不泄露（错误≠首次创建）
            ?assertEqual(
                {error, {create_resource_unavailable, Gid}},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-del-1">>,
                    <<"待删名"/utf8>>,
                    false
                )
            ),
            ?assertEqual(
                {error, {create_resource_unavailable, Gid}},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    [?MEMBER_UID_BIN],
                    {<<"workspace">>, ?WS_ID},
                    <<"req-del-1">>,
                    <<"待删名"/utf8>>,
                    true
                )
            ),

            %% 7d 窗口：服务端受理年龄（fixture 注入 8d 前）独立判定过期
            {ok, Gid2, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-exp-1">>,
                <<"过期名"/utf8>>,
                false
            ),
            {ok, _} = elib_pg:execute(
                <<"UPDATE resource_create_receipt SET server_accepted_at = ",
                    "CURRENT_TIMESTAMP - INTERVAL '8 days' ",
                    "WHERE uid = $1 AND request_id = $2">>,
                [?CREATOR_UID, <<"req-exp-1">>]
            ),
            {error, {create_recovery_expired, Gid2}} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-exp-1">>,
                <<"过期名"/utf8>>,
                false
            ),
            %% 过窗只查：仍禁止重建，只给核对 ID
            {error, {create_recovery_expired, Gid2}} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                [?MEMBER_UID_BIN],
                {<<"workspace">>, ?WS_ID},
                <<"req-exp-1">>,
                <<"过期名"/utf8>>,
                true
            ),
            %% receipt 事实仍在：同键不产生第二个资源
            ?assertEqual(2, receipt_count()),
            ?assertEqual(1, scoped_group_count())
        after
            cleanup_test_rows()
        end
    end).

%%%===================================================================
%%% W04.b 辅助
%%%===================================================================

receipt_row(RequestId) ->
    elib_pg:query(
        <<"SELECT resource_id, resource_deleted FROM resource_create_receipt ",
            "WHERE uid = $1 AND request_id = $2">>,
        [?CREATOR_UID, RequestId]
    ).

receipt_count() ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM resource_create_receipt WHERE uid = $1">>,
            [?CREATOR_UID]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({receipt_count_failed, Other})
    end.

group_member_count(Gid) ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM group_member WHERE group_id = $1">>, [Gid]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({group_member_count_failed, Other})
    end.

%% 固定效果身份计数（J23.c）：staging（msg_store_staging）与已晋升
%% （msg_s2c）两侧按 msg_id 去重联合计数——worker 晋升后 staging 行保留
%%（processed_at 标记、延迟清理），两侧并存的同一效果身份只计一次；
%% (action, payload->>gid, from_id) 定位本用例创建的 group_member_join。
msg_effect_count(Gid) ->
    GidBin = integer_to_binary(Gid),
    case
        elib_pg:query(
            <<"SELECT COUNT(DISTINCT msg_id) AS cnt FROM (",
                "SELECT msg_id FROM msg_store_staging WHERE from_id = $1 ",
                "AND action = 'group_member_join' AND payload->>'gid' = $2 ", "UNION ALL ",
                "SELECT msg_id FROM msg_s2c WHERE from_id = $1 ",
                "AND action = 'group_member_join' AND payload->>'gid' = $2", ") effects">>,
            [?CREATOR_UID, GidBin]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({msg_effect_count_failed, Other})
    end.
