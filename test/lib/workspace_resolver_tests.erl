-module(workspace_resolver_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP3/T5 — workspace_resolver 单元测试
%%% SEC-03 扩充：fail-closed（DB 异常 503 拒绝）+ 附件归属最小闭环 + 未知类型拒绝。
%%%
%%% 覆盖矩阵：
%%%   resolve_workspace：资源类型（workspace/group/notice/channel 系/attachment
%%%   六 scope）× 情形（正常命中 / 资源不存在 / DB 异常（返回错误值/崩溃））；
%%%   边界门（ensure_*/guard_*）：角色（成员/非成员/personal）× 情形（命中 403 /
%%%   不存在放行 404 流程 / DB 异常 503 fail-closed / 非法数据 503）。
%%%
%%% elib_pg:one/2 语义（忠实模拟）：命中 {ok, Row}；无行 {ok, #{}}（默认值）；
%%% 故障 {error, Reason} 或崩溃。one_row/2 据此三态归一。

-define(WS_ID, 800001).
-define(GID, 777001).
-define(CID, 666001).
-define(UID, 900001).
-define(OUTSIDER, 900004).

ws_group_row() ->
    #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => ?WS_ID}.
personal_group_row() ->
    #{<<"scope">> => <<"personal">>, <<"workspace_id">> => null}.

%% ===================================================================
%% resolve_workspace/1：正常命中矩阵
%% ===================================================================

resolve_workspace_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, Params) -> resolve_one(Sql, Params) end}
            ]}
        ],
        fun() ->
            [
                {"workspace resolves to itself", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID}, workspace_resolver:resolve_workspace({workspace, ?WS_ID})
                    )
                end},
                {"workspace group resolves to ws id", fun() ->
                    ?assertEqual({ok, ?WS_ID}, workspace_resolver:resolve_workspace({group, ?GID}))
                end},
                {"personal group resolves to personal", fun() ->
                    ?assertEqual(personal, workspace_resolver:resolve_workspace({group, 777099}))
                end},
                {"missing group resolves to not_found", fun() ->
                    ?assertEqual(
                        {error, not_found}, workspace_resolver:resolve_workspace({group, 1})
                    )
                end},
                {"group notice resolves via group_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID}, workspace_resolver:resolve_workspace({group_notice, 555001})
                    )
                end},
                {"channel subscription resolves via channel scope", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_subscription, ?CID})
                    )
                end},
                {"channel admin resolves via channel scope", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID}, workspace_resolver:resolve_workspace({channel_admin, ?CID})
                    )
                end},
                {"channel message resolves via channel_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_message, 444001})
                    )
                end},
                {"channel comment resolves via channel_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_comment, 444002})
                    )
                end},
                {"channel reaction resolves via channel_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_reaction, 444003})
                    )
                end},
                {"channel webhook resolves via channel_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_webhook, 444004})
                    )
                end},
                {"channel invitation resolves via channel_id", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID},
                        workspace_resolver:resolve_workspace({channel_invitation, 444005})
                    )
                end},
                {"personal channel message is personal", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({channel_message, 444099})
                    )
                end},
                {"group attachment resolves via scope_ref group", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID}, workspace_resolver:resolve_workspace({attachment, 333001})
                    )
                end},
                {"channel attachment resolves via scope_ref channel", fun() ->
                    ?assertEqual(
                        {ok, ?WS_ID}, workspace_resolver:resolve_workspace({attachment, 333002})
                    )
                end},
                {"c2c attachment resolves to personal (provable chain)", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({attachment, 333003})
                    )
                end},
                {"moment attachment resolves to personal (provable chain)", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({attachment, 333004})
                    )
                end},
                {"private attachment resolves to personal (provable chain)", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({attachment, 333005})
                    )
                end},
                {"public attachment resolves to personal (not a workspace resource)", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({attachment, 333006})
                    )
                end},
                {"attachment with scope out of enum is rejected, no default ownership", fun() ->
                    ?assertEqual(
                        {error, {unsupported_scope, <<"galaxy">>}},
                        workspace_resolver:resolve_workspace({attachment, 333007})
                    )
                end},
                {"group attachment with null scope_ref is rejected", fun() ->
                    ?assertMatch(
                        {error, {unsupported_scope, _}},
                        workspace_resolver:resolve_workspace({attachment, 333008})
                    )
                end},
                {"missing attachment row is not_found (was silently personal)", fun() ->
                    ?assertEqual(
                        {error, not_found},
                        workspace_resolver:resolve_workspace({attachment, 333009})
                    )
                end},
                {"unknown resource type is rejected, never personal", fun() ->
                    ?assertEqual(
                        {error, {unsupported_resource, {moment, 1}}},
                        workspace_resolver:resolve_workspace({moment, 1})
                    )
                end}
            ]
        end
    ).

resolve_one(<<"SELECT id FROM workspace", _/binary>>, [?WS_ID]) ->
    {ok, #{<<"id">> => ?WS_ID}};
resolve_one(<<"SELECT group_id FROM group_notice", _/binary>>, [555001]) ->
    {ok, #{<<"group_id">> => ?GID}};
resolve_one(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [?GID]) ->
    {ok, ws_group_row()};
resolve_one(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [777099]) ->
    {ok, personal_group_row()};
resolve_one(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [1]) ->
    %% elib_pg:one/2 无行返回默认 #{}（不是 {error, no_rows}）
    {ok, #{}};
resolve_one(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [777777]) ->
    %% notice 回溯 personal 群
    {ok, personal_group_row()};
resolve_one(<<"SELECT workspace_id FROM channel", _/binary>>, [?CID]) ->
    {ok, #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => ?WS_ID}};
resolve_one(<<"SELECT workspace_id FROM channel", _/binary>>, [_]) ->
    {ok, #{<<"scope">> => <<"personal">>, <<"workspace_id">> => null}};
resolve_one(<<"SELECT channel_id FROM channel_message", _/binary>>, [444001]) ->
    {ok, #{<<"channel_id">> => ?CID}};
resolve_one(<<"SELECT channel_id FROM channel_message", _/binary>>, [444099]) ->
    {ok, #{<<"channel_id">> => 666099}};
resolve_one(<<"SELECT channel_id FROM channel_comment", _/binary>>, [444002]) ->
    {ok, #{<<"channel_id">> => ?CID}};
resolve_one(<<"SELECT channel_id FROM channel_reaction", _/binary>>, [444003]) ->
    {ok, #{<<"channel_id">> => ?CID}};
resolve_one(<<"SELECT channel_id FROM channel_webhook", _/binary>>, [444004]) ->
    {ok, #{<<"channel_id">> => ?CID}};
resolve_one(<<"SELECT channel_id FROM channel_invitation", _/binary>>, [444005]) ->
    {ok, #{<<"channel_id">> => ?CID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333001]) ->
    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => ?GID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333002]) ->
    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => ?CID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333003]) ->
    {ok, #{<<"scope">> => <<"c2c">>, <<"scope_ref">> => <<"c2c:900001:900004">>}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333004]) ->
    {ok, #{<<"scope">> => <<"moment">>, <<"scope_ref">> => <<"99001">>}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333005]) ->
    {ok, #{<<"scope">> => <<"private">>, <<"scope_ref">> => null}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333006]) ->
    {ok, #{<<"scope">> => <<"public">>, <<"scope_ref">> => null}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333007]) ->
    {ok, #{<<"scope">> => <<"galaxy">>, <<"scope_ref">> => null}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333008]) ->
    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => null}};
resolve_one(_, _) ->
    {ok, #{}}.

%% ===================================================================
%% resolve_workspace/1：DB 异常矩阵（fail-closed 前置：异常必须可区分）
%% ===================================================================

db_error_matrix_test_() ->
    OneReturned = fun(_, _) -> {error, pool_exhausted} end,
    [
        {"returned db error propagates for every resource chain", fun() ->
            ?WITH_MECKS([{elib_pg, [{'one', 2, OneReturned}]}], fun() ->
                Targets = [
                    {workspace, ?WS_ID},
                    {project, 123},
                    {project_task, 456},
                    {group, ?GID},
                    {group_notice, 555001},
                    {channel, ?CID},
                    {channel_message, 444001},
                    {channel_comment, 444002},
                    {channel_reaction, 444003},
                    {channel_webhook, 444004},
                    {channel_invitation, 444005},
                    {channel_subscription, ?CID},
                    {channel_admin, ?CID},
                    {attachment, 333001}
                ],
                lists:foreach(
                    fun(Target) ->
                        ?assertEqual(
                            {error, {db_error, pool_exhausted}},
                            workspace_resolver:resolve_workspace(Target),
                            {target, Target}
                        )
                    end,
                    Targets
                )
            end)
        end},
        {"crashing one (exit) maps to db_error, never not_found", fun() ->
            ?WITH_MECKS(
                [{elib_pg, [{'one', 2, fun(_, _) -> exit(pool_down) end}]}],
                fun() ->
                    ?assertMatch(
                        {error, {db_error, _}}, workspace_resolver:resolve_workspace({group, ?GID})
                    ),
                    ?assertMatch(
                        {error, {db_error, _}},
                        workspace_resolver:resolve_workspace({channel, ?CID})
                    )
                end
            )
        end},
        {"crashing one (throw) maps to db_error", fun() ->
            ?WITH_MECKS(
                [{elib_pg, [{'one', 2, fun(_, _) -> throw(driver_lost) end}]}],
                fun() ->
                    ?assertEqual(
                        {error, {db_error, driver_lost}},
                        workspace_resolver:resolve_workspace({group, ?GID})
                    )
                end
            )
        end},
        {"db error must not be confused with not_found", fun() ->
            ?WITH_MECKS([{elib_pg, [{'one', 2, OneReturned}]}], fun() ->
                ?assertNotEqual(
                    {error, not_found}, workspace_resolver:resolve_workspace({group, ?GID})
                )
            end)
        end}
    ].

%% ===================================================================
%% Workspace 成员边界（T5 直访越权红线 + SEC-03 fail-closed）
%% ===================================================================

boundary_guards_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, Params) -> resolve_one(Sql, Params) end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun
                    (?WS_ID, ?UID, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?OUTSIDER, _) ->
                        #{<<"role">> => <<"guest">>, <<"status">> => <<"removed">>};
                    (_, _, _) ->
                        #{}
                end}
            ]}
        ],
        fun() ->
            [
                {"member passes workspace group boundary", fun() ->
                    ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, ?GID))
                end},
                {"non member blocked on workspace group boundary (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_resolver:ensure_group_member_access(?OUTSIDER, ?GID)
                    )
                end},
                {"member passes workspace channel boundary", fun() ->
                    ?assertEqual(ok, workspace_resolver:ensure_channel_member_access(?UID, ?CID))
                end},
                {"non member blocked on workspace channel boundary (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_resolver:ensure_channel_member_access(?OUTSIDER, ?CID)
                    )
                end},
                {"personal group always passes (zero regression)", fun() ->
                    ?assertEqual(
                        ok, workspace_resolver:ensure_group_member_access(?OUTSIDER, 777099)
                    )
                end},
                {"personal channel always passes (zero regression)", fun() ->
                    ?assertEqual(
                        ok, workspace_resolver:ensure_channel_member_access(?OUTSIDER, 666099)
                    )
                end},
                {"non-existent group passes through to legacy 404 flow", fun() ->
                    ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, 1))
                end},
                {"guard_group_gid with zero/invalid gid passes", fun() ->
                    ?assertEqual(ok, workspace_resolver:guard_group_gid(?OUTSIDER, <<"abc">>)),
                    ?assertEqual(ok, workspace_resolver:guard_group_gid(?OUTSIDER, 0))
                end},
                {"guard_group_notice_id blocks workspace group notice for outsider", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_resolver:guard_group_notice_id(?OUTSIDER, 555001)
                    )
                end},
                {"guard_group_notice_id passes for member", fun() ->
                    ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, 555001))
                end},
                {"guard_group_notice_id passes for invalid notice", fun() ->
                    ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, <<"x">>))
                end}
            ]
        end
    ).

%% ===================================================================
%% SEC-03 fail-closed：DB 异常 → 503，与角色无关（成员/非成员同样被拒）
%% ===================================================================

fail_closed_gates_test_() ->
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    [
        {"db error denies workspace group gate (503, member)", fun() ->
            ?WITH_MECKS([OneFailed], fun() ->
                ?assertMatch(
                    {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                    workspace_resolver:ensure_group_member_access(?UID, ?GID)
                )
            end)
        end},
        {"db error denies workspace group gate (503, outsider)", fun() ->
            ?WITH_MECKS([OneFailed], fun() ->
                ?assertMatch(
                    {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                    workspace_resolver:ensure_group_member_access(?OUTSIDER, ?GID)
                )
            end)
        end},
        {"db error denies workspace channel gate (503)", fun() ->
            ?WITH_MECKS([OneFailed], fun() ->
                ?assertMatch(
                    {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                    workspace_resolver:ensure_channel_member_access(?UID, ?CID)
                )
            end)
        end},
        {"db error denies notice gate (503)", fun() ->
            ?WITH_MECKS([OneFailed], fun() ->
                ?assertMatch(
                    {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                    workspace_resolver:guard_group_notice_id(?UID, 555001)
                )
            end)
        end},
        {"db error denies guard_group_gid (503)", fun() ->
            ?WITH_MECKS([OneFailed], fun() ->
                ?assertMatch(
                    {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                    workspace_resolver:guard_group_gid(?UID, ?GID)
                )
            end)
        end},
        {"db error denies guard_channel_binding when binding present (503)", fun() ->
            ?WITH_MECKS(
                [
                    OneFailed,
                    {cowboy_req, [
                        {'binding', 2, fun(channel_id, _Req) -> <<"666001">> end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                        workspace_resolver:guard_channel_binding(#{qs => []}, ?UID)
                    )
                end
            )
        end},
        {"crashing one denies gate (503)", fun() ->
            ?WITH_MECKS(
                [{elib_pg, [{'one', 2, fun(_, _) -> exit(pool_down) end}]}],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                        workspace_resolver:ensure_group_member_access(?UID, ?GID)
                    )
                end
            )
        end},
        {"attachment dirty scope yields explicit unsupported_scope error", fun() ->
            ?WITH_MECKS(
                [
                    {elib_pg, [
                        {'one', 2, fun
                            (<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333007]) ->
                                {ok, #{<<"scope">> => <<"galaxy">>, <<"scope_ref">> => null}};
                            (_, _) ->
                                {ok, #{}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, {unsupported_scope, <<"galaxy">>}},
                        workspace_resolver:resolve_workspace({attachment, 333007})
                    )
                end
            )
        end}
    ].

%% ===================================================================
%% guard_channel_custom_id：custom_id 直访门（SEC-03 fail-closed）
%% ===================================================================

guard_channel_custom_id_test_() ->
    MemberRepo =
        {workspace_member_repo, [
            {'find', 3, fun
                (?WS_ID, ?UID, _) ->
                    #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
                (_, _, _) ->
                    #{}
            end}
        ]},
    ChannelRow =
        {elib_pg, [
            {'one', 2, fun
                (<<"SELECT workspace_id FROM channel", _/binary>>, [?CID]) ->
                    {ok, #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => ?WS_ID}};
                (_, _) ->
                    {ok, #{}}
            end}
        ]},
    [
        {"workspace channel by custom id blocks outsider (403)", fun() ->
            ?WITH_MECKS(
                [
                    {channel_ds, [
                        {'find_by_custom_id', 1, fun(_) ->
                            #{<<"id">> => ?CID, <<"custom_id">> => <<"ws-chan">>}
                        end}
                    ]},
                    ChannelRow,
                    MemberRepo
                ],
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_resolver:guard_channel_custom_id(?OUTSIDER, <<"ws-chan">>)
                    )
                end
            )
        end},
        {"member passes workspace channel by custom id", fun() ->
            ?WITH_MECKS(
                [
                    {channel_ds, [
                        {'find_by_custom_id', 1, fun(_) ->
                            #{<<"id">> => ?CID, <<"custom_id">> => <<"ws-chan">>}
                        end}
                    ]},
                    ChannelRow,
                    MemberRepo
                ],
                fun() ->
                    ?assertEqual(
                        ok, workspace_resolver:guard_channel_custom_id(?UID, <<"ws-chan">>)
                    )
                end
            )
        end},
        {"custom id miss (empty map) passes to legacy 404 flow", fun() ->
            ?WITH_MECKS(
                [{channel_ds, [{'find_by_custom_id', 1, fun(_) -> #{} end}]}],
                fun() ->
                    ?assertEqual(
                        ok, workspace_resolver:guard_channel_custom_id(?OUTSIDER, <<"ghost">>)
                    )
                end
            )
        end},
        {"find_by_custom_id db error denied (503, fail-closed)", fun() ->
            ?WITH_MECKS(
                [
                    {channel_ds, [
                        {'find_by_custom_id', 1, fun(_) -> {error, no_connection} end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                        workspace_resolver:guard_channel_custom_id(?UID, <<"ws-chan">>)
                    )
                end
            )
        end},
        {"find_by_custom_id crash denied (503, fail-closed)", fun() ->
            ?WITH_MECKS(
                [
                    {channel_ds, [
                        {'find_by_custom_id', 1, fun(_) -> exit(pool_down) end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                        workspace_resolver:guard_channel_custom_id(?UID, <<"ws-chan">>)
                    )
                end
            )
        end},
        {"invalid custom id (empty/not binary) passes", fun() ->
            ?assertEqual(ok, workspace_resolver:guard_channel_custom_id(?UID, <<>>)),
            ?assertEqual(ok, workspace_resolver:guard_channel_custom_id(?UID, 123))
        end}
    ].

%% ===================================================================
%% guard_channel_binding：无 binding（测试 Req 非 cowboy req）放行
%% ===================================================================

guard_channel_binding_no_binding_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_, _) -> {ok, #{}} end}
            ]}
        ],
        fun() ->
            {"plain map req without channel binding passes", fun() ->
                ?assertEqual(ok, workspace_resolver:guard_channel_binding(#{qs => []}, ?UID))
            end}
        end
    ).

%% ===================================================================
%% 防回归（T14 Demo B 抓到的真缺陷）：ensure_member 返回 {ok, Role}，
%% handler 便捷门契约必须归一为原子 ok——否则合法成员访问 workspace
%% 资源会在 handler case 上 case_clause 崩成 HTTP 500。
%% ===================================================================

access_gate_normalizes_role_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, Params) -> resolve_one(Sql, Params) end}
            ]},
            {workspace_logic, [
                {'ensure_member', 2, fun(_WsId, _Uid) -> {ok, <<"owner">>} end}
            ]}
        ],
        fun() ->
            [
                {"channel access gate returns plain ok for legal member", fun() ->
                    ?assertEqual(
                        ok, workspace_resolver:ensure_channel_member_access(?UID, ?CID)
                    )
                end},
                {"group access gate returns plain ok for legal member", fun() ->
                    ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, ?GID))
                end},
                {"notice gate returns plain ok for legal member", fun() ->
                    ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, 555001))
                end}
            ]
        end
    ).
