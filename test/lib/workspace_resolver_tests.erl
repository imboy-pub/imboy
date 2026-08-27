-module(workspace_resolver_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — workspace_resolver 单元测试
%%% 覆盖：统一资源归属解析（group/notice/channel/message/comment/reaction/
%%% webhook/attachment/workspace）、Workspace 成员边界（403）、
%%% personal 恒放行（回归红线）、资源不存在放行走既有 404 流程。

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
%% resolve_workspace/1
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
                {"c2c attachment stays personal (T7 TODO path)", fun() ->
                    ?assertEqual(
                        personal, workspace_resolver:resolve_workspace({attachment, 333003})
                    )
                end},
                {"unknown resource type is personal", fun() ->
                    ?assertEqual(personal, workspace_resolver:resolve_workspace({moment, 1}))
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
    {error, no_rows};
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
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333001]) ->
    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => ?GID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333002]) ->
    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => ?CID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333003]) ->
    {ok, #{<<"scope">> => <<"c2c">>, <<"scope_ref">> => ?UID}};
resolve_one(_, _) ->
    {error, no_rows}.

%% ===================================================================
%% Workspace 成员边界（T5 直访越权红线）
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
%% guard_channel_binding：无 binding（测试 Req 非 cowboy req）放行
%% ===================================================================

guard_channel_binding_no_binding_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_, _) -> {error, no_rows} end}
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
