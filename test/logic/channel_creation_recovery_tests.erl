-module(channel_creation_recovery_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W04.c（channel-group-ux v2.2 §6.4 C1）：真实 PostgreSQL 上验证频道创建
%%% 受理回执合同（resource_create_receipt，operation=create_channel）。
%%%
%%% 覆盖（§6.4/J23/J24）：
%%%   - 首受理同事务：频道+创建者订阅/管理员+receipt 回填（J23.b 前半）；
%%%   - 重放同键同载荷：同 ID replayed、零新资源（J23.b）；
%%%   - 同键载荷变化：request_id_conflict；规范等价（tags 顺序/重复、
%%%     名称 trim）不构成冲突（J24.c）；
%%%   - recover_only 只查：未知键 create_not_accepted 且零副作用；同键
%%%     迟到仍可首次受理一次（§6.4 恢复状态表）；
%%%   - 删除例外：delete_channel 同事务置位 resource_deleted，重放/只查
%%%     不复活、不泄露（J24.b）；7d 服务端年龄独立判定 expired；
%%%   - workspace scope 资格路径（ensure_can_create_resource）。
%%%
%%% 注意：中文字符串字面量必须带 /utf8 —— 无标注的 binary 字面量会按
%%% 低字节截断（如 <<"频道">> 落成非法 UTF-8），DB 报 22021
%%% character_not_in_repertoire（首轮实测）。
%%%
%%% 规范摘要纯函数性质不依赖 DB。DB 不可用时按 TEST_WITH_DB 惯例 skip。
%%%===================================================================

-define(ORG_ID, 880091).
-define(WS_ID, 880092).
-define(CREATOR_UID, 880101).
-define(MAX_CHANNELS, 20).

-define(CH_NAME_TRIM, <<"  W04c频道  "/utf8>>).
-define(CH_NAME, <<"W04c频道"/utf8>>).
-define(CH_NAME_RO, <<"只查频道"/utf8>>).
-define(CH_NAME_OTHER, <<"别的频道"/utf8>>).
-define(CH_NAME_DEL, <<"待删频道"/utf8>>).
-define(CH_NAME_EXP, <<"过期频道"/utf8>>).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
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
            {<<"DELETE FROM resource_create_receipt WHERE uid = $1">>, [?CREATOR_UID]},
            {<<"DELETE FROM workspace_member WHERE workspace_id = $1">>, [?WS_ID]},
            {<<"DELETE FROM workspace WHERE id = $1">>, [?WS_ID]},
            {<<"DELETE FROM organization_member WHERE organization_id = $1">>, [?ORG_ID]},
            {<<"DELETE FROM organization WHERE id = $1">>, [?ORG_ID]},
            {<<"DELETE FROM \"user\" WHERE id = $1">>, [?CREATOR_UID]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<"INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) ",
                    "VALUES ($1,'w04c-pg-creator','w04c-pg-creator','w04c-synthetic','127.0.0.1','w04c-synthetic')">>,
                [
                    ?CREATOR_UID
                ]
            },
            %% owner 组织成员行由 trg_organization_owner_member_sync 自动同步。
            {<<"INSERT INTO organization (id, name, owner_id) VALUES ($1,'w04c-pg-org',$2)">>, [
                ?ORG_ID, ?CREATOR_UID
            ]},
            {
                <<"INSERT INTO workspace (id, name, owner_id, organization_id, status, branding) ",
                    "VALUES ($1,'w04c-pg-ws',$2,$3,'active','{}'::jsonb)">>,
                [
                    ?WS_ID, ?CREATOR_UID, ?ORG_ID
                ]
            },
            {<<"INSERT INTO workspace_member (workspace_id, user_id, status) VALUES ($1,$2,'active')">>,
                [
                    ?WS_ID, ?CREATOR_UID
                ]}
        ]
    ).

channel_count() ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM channel WHERE creator_uid = $1">>, [?CREATOR_UID]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({channel_count_failed, Other})
    end.

receipt_count() ->
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS cnt FROM resource_create_receipt ",
                "WHERE uid = $1 AND operation = 'create_channel'">>,
            [?CREATOR_UID]
        )
    of
        {ok, [#{<<"cnt">> := Cnt}]} -> Cnt;
        Other -> error({receipt_count_failed, Other})
    end.

receipt_row(RequestId) ->
    elib_pg:query(
        <<"SELECT resource_id, resource_deleted FROM resource_create_receipt ",
            "WHERE uid = $1 AND operation = 'create_channel' AND request_id = $2">>,
        [?CREATOR_UID, RequestId]
    ).

%%%===================================================================
%%% 用例
%%%===================================================================

%% 规范载荷摘要纯函数：确定性、64hex、字段敏感性、tags 顺序不敏感、
%% personal 的 workspace 段不在规范形式内。
canonical_channel_digest_properties_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = #{
            name => <<"  规范频道  "/utf8>>,
            scope => <<"personal">>,
            workspace_id => 0,
            visibility => 0,
            access_type => 0,
            join_policy => 0,
            description => <<"desc">>,
            avatar => <<>>,
            custom_id => <<>>,
            tags => [<<"b">>, <<"a">>]
        },
        D1 = resource_create_receipt_repo:canonical_channel_digest(Base),
        ?assertEqual(D1, resource_create_receipt_repo:canonical_channel_digest(Base)),
        ?assertMatch({match, _}, re:run(D1, "^[0-9a-f]{64}$")),
        %% tags 顺序不同/重复 → 同一规范形式
        ?assertEqual(
            D1,
            resource_create_receipt_repo:canonical_channel_digest(Base#{
                tags => [<<"a">>, <<"b">>, <<"a">>]
            })
        ),
        %% 敏感性：名称/可见性/scope/workspace 任一变化即不同摘要
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_channel_digest(Base#{
                name => <<"别的"/utf8>>
            })
        ),
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_channel_digest(Base#{visibility => 1})
        ),
        ?assertNotEqual(
            D1,
            resource_create_receipt_repo:canonical_channel_digest(Base#{
                scope => <<"workspace">>, workspace_id => 7
            })
        ),
        ?assertNotEqual(
            resource_create_receipt_repo:canonical_channel_digest(Base#{
                scope => <<"workspace">>, workspace_id => 7
            }),
            resource_create_receipt_repo:canonical_channel_digest(Base#{
                scope => <<"workspace">>, workspace_id => 8
            })
        )
    end).

%% J23.b / J24.c：首受理（personal）→ 重放同 ID 同载荷零新资源；名称
%% trim 规范等价仍是 replayed；真实载荷变化（可见性）→ conflict。
channel_receipt_first_accept_replay_conflict_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            Opts = #{visibility => 0, access_type => 0, join_policy => 0},
            {ok, Channel, accepted} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_TRIM,
                Opts,
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-1">>,
                false
            ),
            Cid = maps:get(<<"id">>, Channel),
            %% 初始名以服务端规范化（trim）形态落库
            ?assertMatch(
                {ok, [#{<<"name">> := ?CH_NAME}]},
                elib_pg:query(<<"SELECT name FROM channel WHERE id = $1">>, [Cid])
            ),
            ?assertMatch(
                {ok, [#{<<"resource_id">> := Cid, <<"resource_deleted">> := false}]},
                receipt_row(<<"req-ch-1">>)
            ),
            ?assertEqual(1, channel_count()),
            ?assertEqual(1, receipt_count()),
            %% 创建者订阅/管理员同事务落库
            ?assertMatch(
                {ok, [_]},
                elib_pg:query(
                    <<"SELECT user_id FROM channel_subscription WHERE channel_id = $1">>,
                    [Cid]
                )
            ),

            %% J23.b：重放（响应丢失）——同 ID、零新资源
            {ok, Channel2, replayed} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME,
                Opts,
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-1">>,
                false
            ),
            ?assertEqual(Cid, maps:get(<<"id">>, Channel2)),
            ?assertEqual(1, channel_count()),
            ?assertEqual(1, receipt_count()),

            %% J24.c：同键真实载荷变化（可见性）→ conflict
            ?assertEqual(
                {error, request_id_conflict},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID,
                    ?CH_NAME,
                    Opts#{visibility => 1},
                    ?MAX_CHANNELS,
                    {<<"personal">>, 0},
                    <<"req-ch-1">>,
                    false
                )
            ),
            ?assertEqual(1, channel_count())
        after
            cleanup_test_rows()
        end
    end).

%% §6.4 恢复状态表：recover_only 未知键 create_not_accepted 零副作用；
%% 同键随后可首次受理；已知键只查 replayed；只查载荷漂移同样 conflict。
channel_receipt_recover_only_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            Opts = #{},
            ?assertEqual(
                {error, create_not_accepted},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID,
                    ?CH_NAME_RO,
                    Opts,
                    ?MAX_CHANNELS,
                    {<<"personal">>, 0},
                    <<"req-ch-ro">>,
                    true
                )
            ),
            ?assertEqual(0, receipt_count()),
            ?assertEqual(0, channel_count()),

            {ok, _Channel, accepted} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_RO,
                Opts,
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-ro">>,
                false
            ),
            ?assertEqual(1, receipt_count()),

            {ok, _Channel2, replayed} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_RO,
                Opts,
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-ro">>,
                true
            ),
            ?assertEqual(1, channel_count()),

            ?assertEqual(
                {error, request_id_conflict},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID,
                    ?CH_NAME_OTHER,
                    Opts,
                    ?MAX_CHANNELS,
                    {<<"personal">>, 0},
                    <<"req-ch-ro">>,
                    true
                )
            )
        after
            cleanup_test_rows()
        end
    end).

%% J24.b：workspace scope 资格路径 + 删除例外（delete_channel 同事务置位）
%% + 7d 服务端年龄过期窗口（fixture 注入 8d 前）。
channel_receipt_delete_and_expiry_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        try
            seed_test_rows(),
            %% workspace scope：创建者为 Owner（资格成立）
            {ok, _Channel, accepted} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_DEL,
                #{},
                ?MAX_CHANNELS,
                {<<"workspace">>, ?WS_ID},
                <<"req-ch-del">>,
                false
            ),
            ?assertMatch(
                {ok, [#{<<"resource_deleted">> := false}]}, receipt_row(<<"req-ch-del">>)
            ),
            {ok, [#{<<"resource_id">> := Cid}]} = receipt_row(<<"req-ch-del">>),

            %% 创建者删除频道（同事务置位删除标识）
            ok = channel_logic:delete_channel(?CREATOR_UID, integer_to_binary(Cid)),
            ?assertMatch(
                {ok, [#{<<"resource_deleted">> := true}]}, receipt_row(<<"req-ch-del">>)
            ),
            %% 删除例外优先：重放/只查都不复活、不泄露（错误≠首次创建）
            ?assertEqual(
                {error, {create_resource_unavailable, Cid}},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID,
                    ?CH_NAME_DEL,
                    #{},
                    ?MAX_CHANNELS,
                    {<<"workspace">>, ?WS_ID},
                    <<"req-ch-del">>,
                    false
                )
            ),
            ?assertEqual(
                {error, {create_resource_unavailable, Cid}},
                channel_logic:create_channel_with_receipt(
                    ?CREATOR_UID,
                    ?CH_NAME_DEL,
                    #{},
                    ?MAX_CHANNELS,
                    {<<"workspace">>, ?WS_ID},
                    <<"req-ch-del">>,
                    true
                )
            ),

            %% 7d 窗口：服务端受理年龄独立判定
            {ok, _Channel2, accepted} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_EXP,
                #{},
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-exp">>,
                false
            ),
            {ok, _} = elib_pg:execute(
                <<"UPDATE resource_create_receipt SET server_accepted_at = ",
                    "CURRENT_TIMESTAMP - INTERVAL '8 days' ",
                    "WHERE uid = $1 AND request_id = $2">>,
                [?CREATOR_UID, <<"req-ch-exp">>]
            ),
            {ok, [#{<<"resource_id">> := Cid2}]} = receipt_row(<<"req-ch-exp">>),
            {error, {create_recovery_expired, Cid2}} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_EXP,
                #{},
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-exp">>,
                false
            ),
            {error, {create_recovery_expired, Cid2}} = channel_logic:create_channel_with_receipt(
                ?CREATOR_UID,
                ?CH_NAME_EXP,
                #{},
                ?MAX_CHANNELS,
                {<<"personal">>, 0},
                <<"req-ch-exp">>,
                true
            ),
            %% receipt 事实仍在：同键不产生第二个资源
            ?assertEqual(2, receipt_count())
        after
            cleanup_test_rows()
        end
    end).
