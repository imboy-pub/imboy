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
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(req0) -> [{<<"gid">>, <<"777001">>}] end}
            ]},
            {workspace_resolver, [
                {'guard_group_gid', 2, fun
                    %% detail 动作把 QS 原始 binary gid 直传 guard（msg_page
                    %% 才 safe_to_integer）——mock 按 handler 真实形态匹配
                    (?OUTSIDER, <<"777001">>) -> ?FORBIDDEN;
                    (?UID, <<"777001">>) -> ok
                end}
            ]},
            {imboy_error, [
                {'validate_id', 2, fun(_, Gid) -> {ok, elib_cnv:safe_to_integer(Gid)} end}
            ]},
            {group_logic, [
                {'find_by_id', 2, fun(Gid, _) ->
                    put(t_detail_allowed, Gid),
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
        fun() -> group_detail_boundary_body() end
    ).

group_detail_boundary_body() ->
    begin
        %% outsider blocked from workspace group detail (403)
        Req1 = group_handler:handle_action(detail, req0, #{current_uid => ?OUTSIDER}),
        ?assertEqual(403, maps:get(response_status, Req1)),
        ?assert(undefined =:= get(t_detail_allowed), "must not leak group data"),

        %% workspace member reads group detail
        Req2 = group_handler:handle_action(detail, req0, #{current_uid => ?UID}),
        ?assertEqual(200, maps:get(response_status, Req2)),
        ?assertEqual(?GID, erase(t_detail_allowed)),
        ok
    end.

%% ===================================================================
%% group msg_page 直访越权
%% ===================================================================

group_msg_page_boundary_test_() ->
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
                    put(t_membership_checked, {Gid, Uid}),
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
        fun() -> group_msg_page_boundary_body() end
    ).

group_msg_page_boundary_body() ->
    begin
        %% outsider blocked from workspace group messages (403)
        Req1 = group_handler:handle_action(msg_page, req0, #{current_uid => ?OUTSIDER}),
        ?assertEqual(403, maps:get(response_status, Req1)),
        ?assert(
            undefined =:= get(t_membership_checked),
            "must not reach group membership check"
        ),

        %% workspace member reaches legacy membership check
        Req2 = group_handler:handle_action(msg_page, req0, #{current_uid => ?UID}),
        ?assertEqual(200, maps:get(response_status, Req2)),
        ?assertEqual({?GID, ?UID}, erase(t_membership_checked)),
        ok
    end.

%% ===================================================================
%% group_notice 边界（workspace 群公告非 wm 403；personal 群放行）
%% ===================================================================

group_notice_boundary_test_() ->
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
                    put(t_notice_insert, true),
                    {ok, 1}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
            ]}
        ],
        fun() -> group_notice_boundary_body() end
    ).

group_notice_boundary_body() ->
    begin
        %% outsider cannot write notice to workspace group (403)
        {ok, Req, _} = group_notice_handler:init(
            req0, #{action => add, current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req)),
        ?assert(undefined =:= get(t_notice_insert), "must not write notice"),

        %% member passes boundary (reaches legacy validation)
        %% gid=777002 未被 guard meck 覆盖 → guard ok；随后走既有
        %% status 校验（缺 status 字段默认 0 合法）→ insert 被拦截
        meck(elib_param, [
            {'post', 1, fun(_) ->
                #{<<"gid">> => <<"777002">>, <<"title">> => <<"t">>, <<"body">> => <<"b">>}
            end}
        ]),
        meck(workspace_resolver, [
            {'guard_group_gid', 2, fun(_, _) -> ok end}
        ]),
        meck(group_notice_logic, [
            {'insert', 2, fun(_, _) ->
                put(t_notice_insert, true),
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
        ?assertEqual(true, erase(t_notice_insert)),
        ok
    end.

%% ===================================================================
%% channel 子 handler 守卫（message/comment/webhook 管理端点）
%% ===================================================================

channel_subhandler_guard_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun
                    (channel_id, req0) -> <<"666001">>;
                    (_, _) -> undefined
                end},
                {'read_body', 2, fun(req0, _) -> {ok, <<"{}">>, req0} end},
                {'reply', 4, fun(_, _, _, req0) -> req0 end},
                %% webhook incoming 的 client_ip 依赖（空头回落 peer）
                {'headers', 1, fun(req0) -> #{} end},
                {'peer', 1, fun(req0) -> {{127, 0, 0, 1}, 4711} end}
            ]},
            {workspace_resolver, [
                {'guard_channel_binding', 2, fun(req0, ?OUTSIDER) ->
                    put(t_guard_invoked, true),
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
        fun() -> channel_subhandler_guard_body() end
    ).

channel_subhandler_guard_body() ->
    begin
        %% message handler blocked (403)
        {ok, Req1, _} = channel_handler_message:init(
            req0, #{action => pin_message, current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req1)),
        erase(t_guard_invoked),

        %% comment handler blocked (403)
        {ok, Req2, _} = channel_handler_comment:init(
            req0, #{action => list_comments, current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req2)),
        erase(t_guard_invoked),

        %% webhook admin endpoint blocked (403)
        {ok, Req3, _} = channel_webhook_handler:init(
            req0, #{action => list, current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req3)),
        erase(t_guard_invoked),

        %% webhook incoming (no JWT) is never guarded (T7 scope)
        {ok, _, _} = channel_webhook_handler:init(req0, #{action => incoming}),
        ?assert(undefined =:= get(t_guard_invoked), "incoming must not hit member guard"),
        ok
    end.

%% ===================================================================
%% workspace_handler：列表端点成员边界
%% ===================================================================

workspace_handler_endpoints_test_() ->
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
                    put(t_channels_listed, WsId),
                    {ok, []}
                end}
            ]},
            {group_logic, [
                {'list_workspace_groups', 2, fun(WsId, _) ->
                    put(t_groups_listed, WsId),
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
        fun() -> workspace_handler_endpoints_body() end
    ).

workspace_handler_endpoints_body() ->
    begin
        %% member lists workspace channels (scope partitioned)
        Req1 = workspace_handler:handle_action(channel_list, req0, #{current_uid => ?UID}),
        ?assertEqual(200, maps:get(response_status, Req1)),
        ?assertEqual(?WS_ID, erase(t_channels_listed)),

        %% outsider blocked from workspace channels (403)
        Req2 = workspace_handler:handle_action(channel_list, req0, #{current_uid => ?OUTSIDER}),
        ?assertEqual(403, maps:get(response_status, Req2)),

        %% member lists workspace groups
        Req3 = workspace_handler:handle_action(group_list, req0, #{current_uid => ?UID}),
        ?assertEqual(200, maps:get(response_status, Req3)),
        ?assertEqual(?WS_ID, erase(t_groups_listed)),

        %% outsider blocked from workspace groups (403)
        Req4 = workspace_handler:handle_action(group_list, req0, #{current_uid => ?OUTSIDER}),
        ?assertEqual(403, maps:get(response_status, Req4)),
        ok
    end.

%% ===================================================================
%% 团队码端点（T2.4）：invite_code / join handler 契约
%% ===================================================================

workspace_invite_code_endpoints_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(workspace_id, req0) -> <<"800001">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_) -> #{<<"code">> => <<" abcd2345 ">>} end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(#{current_uid := Uid}) -> Uid end}
            ]},
            {throttle, [
                {'check', 2, fun(_, _) -> ok end}
            ]},
            {workspace_logic, [
                {'generate_invite_code', 2, fun
                    (?UID, ?WS_ID) ->
                        put(t_generate_called, {?UID, ?WS_ID}),
                        {ok, #{code => <<"ABCD2345">>, expires_at => <<"2099-01-01T00:00:00Z">>}};
                    (?OUTSIDER, ?WS_ID) ->
                        {error, {403, <<"仅工作区 Owner 可执行该操作"/utf8>>}}
                end},
                {'join_by_code', 2, fun
                    (?UID, <<"ABCD2345">>) ->
                        put(t_join_normalized, true),
                        {ok, unchanged, ws_map()};
                    (?OUTSIDER, <<"ABCD2345">>) ->
                        put(t_join_reached, true),
                        {ok, joined, ws_map()};
                    (_, <<"ZZZZ9999">>) ->
                        {error, {981, <<"团队码无效或已失效"/utf8>>}}
                end},
                {'revoke_invite_code', 2, fun
                    (?UID, ?WS_ID) ->
                        put(t_revoke_called, {?UID, ?WS_ID}),
                        {ok, #{revoked => 1}};
                    (?OUTSIDER, ?WS_ID) ->
                        {error, {403, <<"仅工作区 Owner 可执行该操作"/utf8>>}}
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
            %% ?WITH_MECKS 的 TestFun 须单表达式：多断言移入下方本地辅助
            %% 函数 invite_code_endpoints_body/0（begin 在此文件被宏参数
            %% 扫描拒绝；辅助函数彻底绕开宏参数 token 限制；哨兵经进程
            %% 字典传递——mock 与用例体同进程）。
            invite_code_endpoints_body()
        end
    ).

invite_code_endpoints_body() ->
    begin
        %% owner generates invite code via POST invite_code
        Req1 = workspace_handler:handle_action(
            invite_code, req0, #{current_uid => ?UID}
        ),
        ?assertEqual(200, maps:get(response_status, Req1)),
        ?assertMatch(
            #{code := <<"ABCD2345">>, expires_at := <<"2099-01-01T00:00:00Z">>},
            maps:get(payload, Req1)
        ),
        ?assertEqual({?UID, ?WS_ID}, erase(t_generate_called)),

        %% non owner gets 403 envelope on invite_code
        Req2 = workspace_handler:handle_action(
            invite_code, req0, #{current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req2)),

        %% join success returns status=joined + workspace map
        Req3 = workspace_handler:handle_action(
            join, req0, #{current_uid => ?OUTSIDER}
        ),
        ?assertEqual(200, maps:get(response_status, Req3)),
        ?assertMatch(
            #{status := joined, workspace := #{<<"id">> := ?WS_ID}},
            maps:get(payload, Req3)
        ),
        ?assertEqual(true, erase(t_join_reached)),

        %% join idempotent repeat returns status=unchanged
        Req4 = workspace_handler:handle_action(
            join, req0, #{current_uid => ?UID}
        ),
        ?assertEqual(200, maps:get(response_status, Req4)),
        ?assertMatch(
            #{status := unchanged, workspace := #{<<"id">> := ?WS_ID}},
            maps:get(payload, Req4)
        ),

        %% join trims + uppercases code before logic
        %% body 里是 <<" abcd2345 ">>（小写带空白）→ logic 收到 <<"ABCD2345">>
        _ = workspace_handler:handle_action(join, req0, #{current_uid => ?UID}),
        ?assertEqual(true, erase(t_join_normalized)),

        %% join invalid code surfaces 981 envelope
        meck(elib_param, [
            {'post', 1, fun(_) -> #{<<"code">> => <<"ZZZZ9999">>} end}
        ]),
        Req5 = workspace_handler:handle_action(
            join, req0, #{current_uid => ?UID}
        ),
        ?assertEqual(981, maps:get(response_status, Req5)),

        %% owner revokes invite code via POST invite_code/revoke
        Req6 = workspace_handler:handle_action(
            invite_code_revoke, req0, #{current_uid => ?UID}
        ),
        ?assertEqual(200, maps:get(response_status, Req6)),
        ?assertMatch(#{revoked := 1}, maps:get(payload, Req6)),
        ?assertEqual({?UID, ?WS_ID}, erase(t_revoke_called)),

        %% non owner gets 403 envelope on invite_code/revoke
        Req7 = workspace_handler:handle_action(
            invite_code_revoke, req0, #{current_uid => ?OUTSIDER}
        ),
        ?assertEqual(403, maps:get(response_status, Req7)),
        ok
    end.

ws_map() ->
    #{
        <<"id">> => ?WS_ID,
        <<"name">> => <<"Team WS">>,
        <<"logo">> => <<>>,
        <<"owner_id">> => ?UID,
        <<"status">> => <<"active">>
    }.

%% ===================================================================
%% 路由片段清单契约（T7 统一注册的输入，防漂移）
%% ===================================================================

route_fragment_manifest_contract_test() ->
    {ok, Bin} = file:read_file("src/api/workspace_handler.erl"),
    Routes = [
        <<"POST   /api/v1/workspaces">>,
        <<"GET    /api/v1/workspaces/:workspace_id">>,
        <<"GET    /api/v1/workspaces/mine">>,
        <<"POST   /api/v1/workspaces/join">>,
        <<"POST   /api/v1/workspaces/:workspace_id/invite_code">>,
        <<"POST   /api/v1/workspaces/:workspace_id/invite_code/revoke">>,
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
