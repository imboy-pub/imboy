-module(workspace_boundary_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — Workspace 边界直访越权 + personal 零回归 + handler 契约
%%% 覆盖：workspace 群详情/消息直访非 wm 403、workspace 成员放行、
%%% group_notice 边界、channel 子 handler 守卫、webhook incoming 不守卫、
%%% workspace_handler 列表端点成员边界、路由片段清单契约（T7 消费）。

-define(UID, 900001).
-define(GID, 777001).
-define(CID, 666001).
-define(WS_ID, 800001).
-define(OUTSIDER, 900004).
-define(FORBIDDEN, {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}}).

%% ===================================================================
%% group detail 直访越权（§1.4.2 规则 2：已有资源 ID 不能绕过 Workspace 403）
%% ===================================================================

group_detail_boundary_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"777001">>}] end}
            ]},
            {workspace_resolver, [
                {'guard_group_gid', 2, fun
                    (?OUTSIDER, ?GID) -> ?FORBIDDEN;
                    (?UID, ?GID) -> ok
                end}
            ]},
            {imboy_error, [
                {'validate_id', 2, fun(_, Gid) -> {ok, elib_cnv:safe_to_integer(Gid)} end}
            ]},
            {group_logic, [
                {'find_by_id', 2, fun(Gid, _) ->
                    Self ! {detail_allowed, Gid},
                    #{<<"id">> => Gid, <<"title">> => <<"General">>}
                end},
                {'group_transfer', 1, fun(G) -> G end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end},
                {'success', 3, fun(_Req, Payload, _) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            [
                {"outsider blocked from workspace group detail (403)", fun() ->
                    Req = group_handler:handle_action(detail, req0, #{current_uid => ?OUTSIDER}),
                    ?assertEqual(403, maps:get(response_status, Req)),
                    receive
                        {detail_allowed, _} -> ?assert(false, "must not leak group data")
                    after 0 -> ok
                    end
                end},
                {"workspace member reads group detail", fun() ->
                    Req = group_handler:handle_action(detail, req0, #{current_uid => ?UID}),
                    ?assertEqual(200, maps:get(response_status, Req)),
                    receive
                        {detail_allowed, ?GID} -> ok
                    after 500 -> ?assert(false)
                    end
                end}
            ]
        end
    ).

%% ===================================================================
%% group msg_page 直访越权
%% ===================================================================

group_msg_page_boundary_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"777001">>}] end}
            ]},
            {workspace_resolver, [
                {'guard_group_gid', 2, fun
                    (?OUTSIDER, ?GID) -> ?FORBIDDEN;
                    (?UID, ?GID) -> ok
                end}
            ]},
            {elib_param, [
                {'int', 3, fun(_, _, _) -> {ok, 0} end},
                {'page', 1, fun(_) -> {1, 20} end}
            ]},
            {group_logic, [
                {'is_member', 2, fun(Gid, Uid) ->
                    Self ! {membership_checked, Gid, Uid},
                    case Uid of
                        ?UID -> #{<<"id">> => 1};
                        ?OUTSIDER -> #{}
                    end
                end},
                {'msg_page', 3, fun(_, _, _) -> {ok, #{total => 0, list => []}} end},
                {'group_transfer', 1, fun(M) -> M end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) -> #{response_status => 403} end},
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end},
                {'success', 2, fun(_Req, Payload) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            [
                {"outsider blocked from workspace group messages (403)", fun() ->
                    Req = group_handler:handle_action(msg_page, req0, #{current_uid => ?OUTSIDER}),
                    ?assertEqual(403, maps:get(response_status, Req)),
                    receive
                        {membership_checked, _, _} ->
                            ?assert(false, "must not reach group membership check")
                    after 0 -> ok
                    end
                end},
                {"workspace member reaches legacy membership check", fun() ->
                    Req = group_handler:handle_action(msg_page, req0, #{current_uid => ?UID}),
                    ?assertEqual(200, maps:get(response_status, Req)),
                    receive
                        {membership_checked, ?GID, ?UID} -> ok
                    after 500 -> ?assert(false)
                    end
                end}
            ]
        end
    ).

%% ===================================================================
%% group_notice 边界（workspace 群公告非 wm 403；personal 群放行）
%% ===================================================================

group_notice_boundary_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(req0) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_) ->
                    #{<<"gid">> => <<"777001">>, <<"title">> => <<"t">>, <<"body">> => <<"b">>}
                end}
            ]},
            {workspace_resolver, [
                {'guard_group_gid', 2, fun
                    (?OUTSIDER, ?GID) -> ?FORBIDDEN;
                    (?UID, ?GID) -> ok
                end}
            ]},
            {elib_dt, [
                {'rfc3339_to', 2, fun(_, _) -> 1 end},
                {'now', 0, fun() -> <<"2026-03-13T00:00:00Z">> end}
            ]},
            {throttle, [
                {'check', 2, fun(_, _) -> ok end}
            ]},
            {group_notice_logic, [
                {'insert', 2, fun(_, _) ->
                    Self ! notice_insert_reached,
                    {ok, 1}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
            ]}
        ],
        fun() ->
            [
                {"outsider cannot write notice to workspace group (403)", fun() ->
                    {ok, Req, _} = group_notice_handler:init(
                        req0, #{action => add, current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req)),
                    receive
                        notice_insert_reached -> ?assert(false, "must not write notice")
                    after 0 -> ok
                    end
                end},
                {"member passes boundary (reaches legacy validation)", fun() ->
                    %% gid=777002 未被 guard meck 覆盖 → guard ok；随后走既有
                    %% status 校验（缺 status 字段默认 0 合法）→ insert 被拦截
                    meck(elib_param, [
                        {'post', 1, fun(_) ->
                            #{
                                <<"gid">> => <<"777002">>,
                                <<"title">> => <<"t">>,
                                <<"body">> => <<"b">>
                            }
                        end}
                    ]),
                    meck(workspace_resolver, [
                        {'guard_group_gid', 2, fun(_, _) -> ok end}
                    ]),
                    meck(group_notice_logic, [
                        {'insert', 2, fun(_, _) ->
                            Self ! notice_insert_reached,
                            {ok, 9}
                        end}
                    ]),
                    meck(elib_response, [
                        {'success', 2, fun(_Req, _P) -> #{response_status => 200} end}
                    ]),
                    {ok, Req2, _} = group_notice_handler:init(
                        req0, #{action => add, current_uid => ?UID}
                    ),
                    ?assertEqual(200, maps:get(response_status, Req2)),
                    receive
                        notice_insert_reached -> ok
                    after 500 -> ?assert(false)
                    end
                end}
            ]
        end
    ).

%% ===================================================================
%% channel 子 handler 守卫（message/comment/webhook 管理端点）
%% ===================================================================

channel_subhandler_guard_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun
                    (channel_id, req0) -> <<"666001">>;
                    (_, _) -> undefined
                end},
                {'read_body', 2, fun(req0, _) -> {ok, <<"{}">>, req0} end},
                {'reply', 4, fun(_, _, _, req0) -> req0 end}
            ]},
            {workspace_resolver, [
                {'guard_channel_binding', 2, fun(req0, ?OUTSIDER) ->
                    Self ! guard_invoked,
                    ?FORBIDDEN
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
            ]},
            {channel_webhook_logic, [
                {'incoming', 3, fun(_, _, _) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            [
                {"message handler blocked (403)", fun() ->
                    {ok, Req, _} = channel_handler_message:init(
                        req0, #{action => pin_message, current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req))
                end},
                {"comment handler blocked (403)", fun() ->
                    {ok, Req, _} = channel_handler_comment:init(
                        req0, #{action => list_comments, current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req))
                end},
                {"webhook admin endpoint blocked (403)", fun() ->
                    {ok, Req, _} = channel_webhook_handler:init(
                        req0, #{action => list, current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req))
                end},
                {"webhook incoming (no JWT) is never guarded (T7 scope)", fun() ->
                    {ok, _, _} = channel_webhook_handler:init(req0, #{action => incoming}),
                    receive
                        guard_invoked -> ?assert(false, "incoming must not hit member guard")
                    after 0 -> ok
                    end
                end}
            ]
        end
    ).

%% ===================================================================
%% workspace_handler：列表端点成员边界
%% ===================================================================

workspace_handler_endpoints_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(workspace_id, req0) -> <<"800001">> end}
            ]},
            {elib_param, [
                {'page', 1, fun(_) -> {1, 10} end},
                {'int', 3, fun(limit, _, _) -> {ok, 50} end},
                {'post', 1, fun(_) -> #{} end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(#{current_uid := Uid}) -> Uid end}
            ]},
            {workspace_logic, [
                {'ensure_member', 2, fun
                    (?WS_ID, ?UID) -> {ok, <<"member">>};
                    (?WS_ID, ?OUTSIDER) -> ?FORBIDDEN
                end}
            ]},
            {channel_logic, [
                {'list_workspace_channels', 2, fun(WsId, _) ->
                    Self ! {channels_listed, WsId},
                    {ok, []}
                end}
            ]},
            {group_logic, [
                {'list_workspace_groups', 2, fun(WsId, _) ->
                    Self ! {groups_listed, WsId},
                    {ok, []}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end},
                {'success', 2, fun(_Req, Payload) ->
                    #{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            [
                {"member lists workspace channels (scope partitioned)", fun() ->
                    Req = workspace_handler:handle_action(
                        channel_list, req0, #{current_uid => ?UID}
                    ),
                    ?assertEqual(200, maps:get(response_status, Req)),
                    receive
                        {channels_listed, ?WS_ID} -> ok
                    after 500 -> ?assert(false)
                    end
                end},
                {"outsider blocked from workspace channels (403)", fun() ->
                    Req = workspace_handler:handle_action(
                        channel_list, req0, #{current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req))
                end},
                {"member lists workspace groups", fun() ->
                    Req = workspace_handler:handle_action(
                        group_list, req0, #{current_uid => ?UID}
                    ),
                    ?assertEqual(200, maps:get(response_status, Req)),
                    receive
                        {groups_listed, ?WS_ID} -> ok
                    after 500 -> ?assert(false)
                    end
                end},
                {"outsider blocked from workspace groups (403)", fun() ->
                    Req = workspace_handler:handle_action(
                        group_list, req0, #{current_uid => ?OUTSIDER}
                    ),
                    ?assertEqual(403, maps:get(response_status, Req))
                end}
            ]
        end
    ).

%% ===================================================================
%% 路由片段清单契约（T7 统一注册的输入，防漂移）
%% ===================================================================

route_fragment_manifest_contract_test() ->
    {ok, Bin} = file:read_file("src/api/workspace_handler.erl"),
    Routes = [
        <<"POST   /api/v1/workspaces">>,
        <<"GET    /api/v1/workspaces/:workspace_id">>,
        <<"GET    /api/v1/workspaces/mine">>,
        <<"POST   /api/v1/workspaces/:workspace_id/update">>,
        <<"GET    /api/v1/workspaces/:workspace_id/branding">>,
        <<"POST   /api/v1/workspaces/:workspace_id/branding">>,
        <<"GET    /api/v1/workspaces/:workspace_id/overview">>,
        <<"GET    /api/v1/workspaces/:workspace_id/channels">>,
        <<"GET    /api/v1/workspaces/:workspace_id/groups">>,
        <<"GET    /api/v1/workspaces/:workspace_id/members">>,
        <<"POST   /api/v1/workspaces/:workspace_id/members/invite">>,
        <<"POST   /api/v1/workspaces/:workspace_id/members/remove">>,
        <<"POST   /api/v1/workspaces/:workspace_id/members/role">>,
        <<"POST   /api/v1/workspaces/:workspace_id/members/transfer_owner">>
    ],
    lists:foreach(
        fun(Route) ->
            ?assert(binary:match(Bin, Route) =/= nomatch, Route)
        end,
        Routes
    ).

%% group_handler 更新白名单不含 scope/workspace_id（防线契约）
group_edit_whitelist_contract_test() ->
    {ok, Bin} = file:read_file("src/api/group_handler.erl"),
    ?assert(
        binary:match(Bin, <<"Fields = [<<\"title\">>, <<\"avatar\">>, <<\"introduction\">>]">>) =/=
            nomatch
    ).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
