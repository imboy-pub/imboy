-module(resource_receipt_revocation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% J25.a（channel-group-ux v2.2 §9）：已创建后撤权/删除资源分别——同键
%%% 重放或只查不返回无权内容、不复活、不重建、最小受理事实保留。
%%%
%%% 覆盖缺口（静态盘点 2026-11-01）：
%%%   - 删除向量（dissolve/delete_channel 同事务置位 resource_deleted →
%%%     unavailable）已由 group_creation_recovery_pg_tests 的
%%%     receipt_delete_and_expiry_window_test_ 与
%%%     channel_creation_recovery_tests 的 channel_receipt_delete_and_
%%%     expiry_test_ 覆盖（J24.b），本文件不重复；
%%%   - 撤权向量（plan §9.1 明确归属 J25.a 的缺口）此前无任何 case：
%%%     群侧 replay_decision/4 存活判定 status=1 且创建者仍是成员
%%%     （group_logic.erl group_receipt_resource_alive/2）；频道侧
%%%     replay_channel_decision/4 判定 status=1 且 creator_uid 仍为本
%%%     人（channel_logic_message.erl）。本文件以真实业务动作触发撤权：
%%%     群=转让后新群主移除原创建者（transfer/3 + group_member_logic:
%%%     leave/3 群角色权限矩阵）；频道=创建者归档（archive_channel/2，
%%%     status 1→0，不置 resource_deleted）。
%%%
%%% 断言合同（§9 J25.a 行）：
%%%   1. 不返回无权内容：重放/只查结果为
%%%      {error, {create_resource_unavailable, ResourceId}}——元组仅含
%%%      核对用 ID，不含标题/成员/频道 DTO；
%%%   2. 不复活：成员行不回、对象不重建、status 不变；
%%%   3. 不重建：资源行数与 receipt 行数不变，错误 ≠ 首次创建；
%%%   4. 最小受理事实保留：resource_id/resource_deleted/request_digest
%%%      原值不动，撤权后同键不同载荷仍 request_id_conflict（事实绑定
%%%      优先于可用性判定）；
%%%   5. 撤权态是实时授权派生：真实恢复成员/归档恢复后重放回到
%%%      replayed——证明 receipt 从不"发放"授权，只回放既有事实。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(ORG_ID, 880201).
-define(WS_ID, 880202).
-define(CREATOR_UID, 880211).
-define(MEMBER_UID, 880212).
-define(MEMBER2_UID, 880213).
-define(MEMBER_UID_BIN, <<"880212">>).
-define(MEMBER2_UID_BIN, <<"880213">>).
-define(FIXTURE_UIDS, [?CREATOR_UID, ?MEMBER_UID, ?MEMBER2_UID]).

-define(G_REQ, <<"req-j25a-group">>).
-define(G_TITLE, <<"J25a撤权名"/utf8>>).
-define(G_TITLE_OTHER, <<"J25a另一名"/utf8>>).

-define(C_REQ, <<"req-j25a-channel">>).
-define(C_NAME, <<"J25a撤权频道"/utf8>>).
-define(C_NAME_OTHER, <<"J25a另一频道"/utf8>>).

%%%===================================================================
%%% 夹具（与 group_creation_recovery_pg_tests / channel_creation_recovery_tests
%%% 同构；UID 段独立避免串扰）
%%%===================================================================

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            %% 频道侧
            {
                <<"DELETE FROM channel_admin WHERE channel_id IN ",
                    "(SELECT id FROM channel WHERE creator_uid = $1)">>,
                [?CREATOR_UID]
            },
            {
                <<"DELETE FROM channel_subscription WHERE channel_id IN ",
                    "(SELECT id FROM channel WHERE creator_uid = $1)">>,
                [?CREATOR_UID]
            },
            {<<"DELETE FROM channel WHERE creator_uid = $1">>, [?CREATOR_UID]},
            %% 群侧
            {
                <<"DELETE FROM group_member WHERE group_id IN ",
                    "(SELECT id FROM \"group\" WHERE workspace_id = $1)">>,
                [?WS_ID]
            },
            {
                <<"DELETE FROM group_log WHERE group_id IN ",
                    "(SELECT id FROM \"group\" WHERE workspace_id = $1)">>,
                [?WS_ID]
            },
            {<<"DELETE FROM \"group\" WHERE workspace_id = $1">>, [?WS_ID]},
            {<<"DELETE FROM msg_store_staging WHERE from_id = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM msg_s2c WHERE from_id = $1">>, [?CREATOR_UID]},
            %% 公共
            {<<"DELETE FROM resource_create_receipt WHERE uid = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM workspace_member WHERE workspace_id = $1">>, [?WS_ID]},
            {<<"DELETE FROM workspace WHERE id = $1">>, [?WS_ID]},
            {<<"DELETE FROM organization_member WHERE organization_id = $1">>, [?ORG_ID]},
            {<<"DELETE FROM organization WHERE id = $1">>, [?ORG_ID]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?FIXTURE_UIDS]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<"INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES ",
                    "($1,'j25a-creator','j25a-creator','j25a-synthetic','127.0.0.1','j25a-synthetic'),",
                    "($2,'j25a-member','j25a-member','j25a-synthetic','127.0.0.1','j25a-synthetic'),",
                    "($3,'j25a-member2','j25a-member2','j25a-synthetic','127.0.0.1','j25a-synthetic')">>,
                [?CREATOR_UID, ?MEMBER_UID, ?MEMBER2_UID]
            },
            {<<"INSERT INTO organization (id, name, owner_id) VALUES ($1,'j25a-org',$2)">>, [
                ?ORG_ID, ?CREATOR_UID
            ]},
            %% MEMBER/MEMBER2 需 active organization_member（workspace 资源谓词）；
            %% owner 行由 trg_organization_owner_member_sync 自动同步。
            {<<"INSERT INTO organization_member (organization_id, user_id, status) VALUES ($1,$2,'active'),($1,$3,'active')">>,
                [?ORG_ID, ?MEMBER_UID, ?MEMBER2_UID]},
            {
                <<"INSERT INTO workspace (id, name, owner_id, organization_id, status, branding) ",
                    "VALUES ($1,'j25a-ws',$2,$3,'active','{}'::jsonb)">>,
                [?WS_ID, ?CREATOR_UID, ?ORG_ID]
            },
            {
                <<"INSERT INTO workspace_member (workspace_id, user_id, status) ",
                    "VALUES ($1,$2,'active'),($1,$3,'active'),($1,$4,'active')">>,
                [?WS_ID, ?CREATOR_UID, ?MEMBER_UID, ?MEMBER2_UID]
            }
        ]
    ).

receipt_row(Uid, Operation, RequestId) ->
    elib_pg:query(
        <<"SELECT resource_id, resource_deleted, request_digest, server_accepted_at ",
            "FROM resource_create_receipt ",
            "WHERE uid = $1 AND operation = $2 AND request_id = $3">>,
        [Uid, Operation, RequestId]
    ).

receipt_count(Uid, Operation) ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM resource_create_receipt ",
                "WHERE uid = $1 AND operation = $2">>,
            [Uid, Operation]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({receipt_count_failed, Other})
    end.

creator_is_member(Gid, Uid) ->
    case
        elib_pg:query(
            <<"SELECT 1 AS one FROM group_member WHERE group_id = $1 AND user_id = $2">>,
            [Gid, Uid]
        )
    of
        {ok, [_ | _]} -> true;
        {ok, []} -> false;
        Other -> error({member_probe_failed, Other})
    end.

%%%===================================================================
%%% BG：群撤权向量
%%%===================================================================

%% BG（J25.a）：创建受理 → 转让 → 新群主移除原创建者 → 同键重放/只查
%% 均 unavailable；不复活、不重建、最小事实保留、冲突绑定仍在。
group_receipt_revocation_replay_and_recover_only_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            Payload = [?MEMBER_UID_BIN, ?MEMBER2_UID_BIN],
            {ok, Gid, accepted} = group_logic:add_with_receipt(
                0,
                ?CREATOR_UID,
                2,
                Payload,
                {<<"workspace">>, ?WS_ID},
                ?G_REQ,
                ?G_TITLE,
                false
            ),
            %% 基线：撤权前同键重放可得完整结果
            ?assertEqual(
                {ok, Gid, replayed},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Payload,
                    {<<"workspace">>, ?WS_ID},
                    ?G_REQ,
                    ?G_TITLE,
                    false
                )
            ),
            {ok, [#{<<"request_digest">> := Digest0}]} = receipt_row(
                ?CREATOR_UID, <<"create_group">>, ?G_REQ
            ),

            %% 撤权动作（真实业务路径）：转让给初始成员 MEMBER2 →
            %% 创建者降为普通成员 → 新群主按角色矩阵移除创建者。
            ok = group_logic:transfer(?CREATOR_UID, Gid, ?MEMBER2_UID),
            ok = group_member_logic:leave(?CREATOR_UID, Gid, ?MEMBER2_UID),
            ?assertEqual(false, creator_is_member(Gid, ?CREATOR_UID)),

            %% 同键重放：撤权优先——不返回无权内容（仅核对 ID）、不复活
            %%（成员不回）、不重建（错误 ≠ 首次创建）。
            ?assertEqual(
                {error, {create_resource_unavailable, Gid}},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Payload,
                    {<<"workspace">>, ?WS_ID},
                    ?G_REQ,
                    ?G_TITLE,
                    false
                )
            ),
            %% 只查同款：不新增占位/资源。
            ?assertEqual(
                {error, {create_resource_unavailable, Gid}},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Payload,
                    {<<"workspace">>, ?WS_ID},
                    ?G_REQ,
                    ?G_TITLE,
                    true
                )
            ),
            ?assertEqual(false, creator_is_member(Gid, ?CREATOR_UID)),
            %% 不重建：资源与 receipt 各仍一行；对象存活（撤权 ≠ 删除）。
            ?assertEqual(1, receipt_count(?CREATOR_UID, <<"create_group">>)),
            ?assertMatch(
                {ok, [#{<<"status">> := 1}]},
                elib_pg:query(<<"SELECT status FROM \"group\" WHERE id = $1">>, [Gid])
            ),
            %% 最小受理事实保留：resource_id/resource_deleted/digest 原值不动。
            ?assertMatch(
                {ok, [#{<<"resource_id">> := Gid, <<"resource_deleted">> := false}]},
                receipt_row(?CREATOR_UID, <<"create_group">>, ?G_REQ)
            ),
            {ok, [#{<<"request_digest">> := Digest1}]} = receipt_row(
                ?CREATOR_UID, <<"create_group">>, ?G_REQ
            ),
            ?assertEqual(Digest0, Digest1),
            %% 撤权后事实绑定优先：同键不同规范载荷仍 conflict（不因不可用
            %% 降级为首次创建或换键）。
            ?assertEqual(
                {error, request_id_conflict},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Payload,
                    {<<"workspace">>, ?WS_ID},
                    ?G_REQ,
                    ?G_TITLE_OTHER,
                    false
                )
            ),

            %% 撤权态实时派生：真实业务重加成员（add_member 内部走 join_group
            %% 事务）后，重放回到 replayed——receipt 从不发放授权，只回放
            %% 实时授权下的既有事实（"不复活"指回执不制造授权，非冻结对象）。
            ok = group_member_ds:add_member(Gid, ?CREATOR_UID),
            ?assertEqual(
                {ok, Gid, replayed},
                group_logic:add_with_receipt(
                    0,
                    ?CREATOR_UID,
                    2,
                    Payload,
                    {<<"workspace">>, ?WS_ID},
                    ?G_REQ,
                    ?G_TITLE,
                    false
                )
            ),
            ?assertEqual(1, receipt_count(?CREATOR_UID, <<"create_group">>))
        after
            cleanup_test_rows()
        end
    end).

%%%===================================================================
%%% BS：频道撤权向量
%%%===================================================================

%% BS（J25.a）：创建受理 → 创建者归档频道（status 1→0，不置
%% resource_deleted）→ 同键重放/只查均 unavailable；归档与删除语义分开
%%（归档保留受理事实、删除置位删除标识），恢复归档后重放回 replayed。
channel_receipt_revocation_replay_and_recover_only_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            Opts = #{},
            {ok, Channel, accepted} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID, ?C_NAME, Opts, 20, {<<"personal">>, 0}, ?C_REQ, false
            ),
            Cid = maps:get(<<"id">>, Channel),
            %% 基线：撤权前重放可得完整 DTO
            ?assertMatch(
                {ok, #{<<"id">> := Cid}, replayed},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID, ?C_NAME, Opts, 20, {<<"personal">>, 0}, ?C_REQ, false
                )
            ),
            {ok, [#{<<"request_digest">> := Digest0}]} = receipt_row(
                ?CREATOR_UID, <<"create_channel">>, ?C_REQ
            ),

            %% 撤权动作：创建者归档频道（频道无创建者转让——status 变更即
            %% 创建者回执不可用态；archive 与 delete 是两条不同代码路径，
            %% 本断言同时钉住两者不混淆）。
            {ok, _} = channel_logic:archive_channel(?CREATOR_UID, integer_to_binary(Cid)),
            ?assertMatch(
                {ok, [#{<<"status">> := 0}]},
                elib_pg:query(<<"SELECT status FROM channel WHERE id = $1">>, [Cid])
            ),
            ?assertMatch(
                {ok, [#{<<"resource_deleted">> := false}]},
                receipt_row(?CREATOR_UID, <<"create_channel">>, ?C_REQ)
            ),

            %% 同键重放/只查：不返回无权内容、不复活、不重建。
            ?assertEqual(
                {error, {create_resource_unavailable, Cid}},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID, ?C_NAME, Opts, 20, {<<"personal">>, 0}, ?C_REQ, false
                )
            ),
            ?assertEqual(
                {error, {create_resource_unavailable, Cid}},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID, ?C_NAME, Opts, 20, {<<"personal">>, 0}, ?C_REQ, true
                )
            ),
            ?assertEqual(1, receipt_count(?CREATOR_UID, <<"create_channel">>)),
            %% 最小受理事实保留：resource_id/resource_deleted/digest 原值。
            ?assertMatch(
                {ok, [#{<<"resource_id">> := Cid, <<"resource_deleted">> := false}]},
                receipt_row(?CREATOR_UID, <<"create_channel">>, ?C_REQ)
            ),
            {ok, [#{<<"request_digest">> := Digest1}]} = receipt_row(
                ?CREATOR_UID, <<"create_channel">>, ?C_REQ
            ),
            ?assertEqual(Digest0, Digest1),
            %% 撤权后冲突绑定仍在。
            ?assertEqual(
                {error, request_id_conflict},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID, ?C_NAME_OTHER, Opts, 20, {<<"personal">>, 0}, ?C_REQ, false
                )
            ),

            %% 撤权态实时派生：业务恢复归档（restore_channel）后重放回
            %% replayed，同 ID 零新资源。
            {ok, _} = channel_logic:restore_channel(?CREATOR_UID, integer_to_binary(Cid)),
            {ok, Channel2, replayed} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID, ?C_NAME, Opts, 20, {<<"personal">>, 0}, ?C_REQ, false
            ),
            ?assertEqual(Cid, maps:get(<<"id">>, Channel2)),
            ?assertEqual(1, receipt_count(?CREATOR_UID, <<"create_channel">>))
        after
            cleanup_test_rows()
        end
    end).
