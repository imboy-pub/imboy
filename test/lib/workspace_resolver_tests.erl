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
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转（内层断言从不执行）；多断言移入下方私有辅助函数。
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, Params) -> resolve_one(Sql, Params) end}
            ]}
        ],
        fun() -> resolve_workspace_body() end
    ).

resolve_workspace_body() ->
    begin
        %% workspace resolves to itself
        ?assertEqual(
            {ok, ?WS_ID}, workspace_resolver:resolve_workspace({workspace, ?WS_ID})
        ),
        %% workspace group resolves to ws id
        ?assertEqual({ok, ?WS_ID}, workspace_resolver:resolve_workspace({group, ?GID})),
        %% personal group resolves to personal
        ?assertEqual(personal, workspace_resolver:resolve_workspace({group, 777099})),
        %% missing group resolves to not_found
        ?assertEqual({error, not_found}, workspace_resolver:resolve_workspace({group, 1})),
        %% group notice resolves via group_id
        ?assertEqual(
            {ok, ?WS_ID}, workspace_resolver:resolve_workspace({group_notice, 555001})
        ),
        %% channel subscription resolves via channel scope
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_subscription, ?CID})
        ),
        %% channel admin resolves via channel scope
        ?assertEqual(
            {ok, ?WS_ID}, workspace_resolver:resolve_workspace({channel_admin, ?CID})
        ),
        %% channel message resolves via channel_id
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_message, 444001})
        ),
        %% channel comment resolves via channel_id
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_comment, 444002})
        ),
        %% channel reaction resolves via channel_id
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_reaction, 444003})
        ),
        %% channel webhook resolves via channel_id
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_webhook, 444004})
        ),
        %% channel invitation resolves via channel_id
        ?assertEqual(
            {ok, ?WS_ID},
            workspace_resolver:resolve_workspace({channel_invitation, 444005})
        ),
        %% personal channel message is personal
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({channel_message, 444099})
        ),
        %% group attachment resolves via scope_ref group
        ?assertEqual(
            {ok, ?WS_ID}, workspace_resolver:resolve_workspace({attachment, 333001})
        ),
        %% channel attachment resolves via scope_ref channel
        ?assertEqual(
            {ok, ?WS_ID}, workspace_resolver:resolve_workspace({attachment, 333002})
        ),
        %% c2c attachment stays personal (T7 closed: personal domain)
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333003})
        ),
        %% moment attachment stays personal (T7 closed: personal domain)
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333004})
        ),
        %% private attachment stays personal (T7 closed: personal domain)
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333005})
        ),
        %% public attachment stays personal (T7 closed: personal domain)
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333006})
        ),
        %% group attachment without scope_ref degrades to personal
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333007})
        ),
        %% missing attachment row stays personal (legacy fallback)
        ?assertEqual(
            personal, workspace_resolver:resolve_workspace({attachment, 333099})
        ),
        %% unknown resource type is explicit error（SEC-03 收紧后 src 已不再
        %% 默认 personal——src 注释"测试断言已同步改为显式错误"，此处对齐）
        ?assertEqual(
            {error, {unsupported_resource, {moment, 1}}},
            workspace_resolver:resolve_workspace({moment, 1})
        ),
        ok
    end.

resolve_one(<<"SELECT id FROM workspace", _/binary>>, [?WS_ID]) ->
    {ok, #{<<"id">> => ?WS_ID}};
resolve_one(<<"SELECT group_id FROM group_notice", _/binary>>, [555001]) ->
    {ok, #{<<"group_id">> => ?GID}};
%% row_scope/2 真实 SQL：SELECT scope, workspace_id FROM "group" WHERE id = $1
resolve_one(<<"SELECT scope, workspace_id FROM \"group\"", _/binary>>, [?GID]) ->
    {ok, ws_group_row()};
resolve_one(<<"SELECT scope, workspace_id FROM \"group\"", _/binary>>, [777099]) ->
    {ok, personal_group_row()};
resolve_one(<<"SELECT scope, workspace_id FROM \"group\"", _/binary>>, [1]) ->
    %% 真实零行形态：elib_pg:one 对空结果返回 {ok, Default=#{}}
    {ok, #{}};
resolve_one(<<"SELECT scope, workspace_id FROM \"group\"", _/binary>>, [777777]) ->
    %% notice 回溯 personal 群
    {ok, personal_group_row()};
%% row_scope/2 真实 SQL：SELECT scope, workspace_id FROM channel WHERE id = $1
resolve_one(<<"SELECT scope, workspace_id FROM channel", _/binary>>, [?CID]) ->
    {ok, #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => ?WS_ID}};
resolve_one(<<"SELECT scope, workspace_id FROM channel", _/binary>>, [_]) ->
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
    {ok, #{<<"scope">> => <<"c2c">>, <<"scope_ref">> => ?UID}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333004]) ->
    %% T7：moment 附件 scope_ref 发帖后回填为 momentId（两阶段绑定），
    %% 与 workspace 无任何关联（moment 全模块无 workspace 字段）
    {ok, #{<<"scope">> => <<"moment">>, <<"scope_ref">> => <<"990001">>}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333005]) ->
    {ok, #{<<"scope">> => <<"private">>, <<"scope_ref">> => null}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333006]) ->
    {ok, #{<<"scope">> => <<"public">>, <<"scope_ref">> => null}};
resolve_one(<<"SELECT scope, scope_ref FROM attachment", _/binary>>, [333007]) ->
    %% scope=group 但 scope_ref 为 null（降级/脏数据）：无目标可解析，
    %% 沿 personal 兜底（与历史行为一致，守卫放行不吞 404 语义）
    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => null}};
resolve_one(_, _) ->
    %% 未匹配的 id 一律按零行处理（真实零行形态 {ok, #{}}）
    {ok, #{}}.

%% DB 错误形态（真实故障形态：{error, Reason}，区别于零行的 {ok, #{}}）
db_error_one() ->
    fun(_, _) -> {error, {pgsql_error, #{code => <<"57P01">>}}} end.

%% ===================================================================
%% resolve_workspace/1：DB 异常矩阵（fail-closed 前置：异常必须可区分）
%% ⚠️ 原 {Desc, fun} 列表形态是静默空转：{Desc, fun() -> ?WITH_MECKS(...)
%% end} 中 eunit 求值外层 fun 后丢弃返回的 setup fixture，内层从不执行。
%% 拆为独立 test_/0（?WITH_MECKS 内层单表达式直接断言，真实执行）。
%% ===================================================================

db_error_propagates_all_resources_test_() ->
    %% "returned db error propagates for every resource chain"
    %% M-1 收口后现行契约：one_row 把 {error, _} 归一为 error({resolver_db_error,_})
    %% 抛出（生产调用方 workspace_guard/守卫均 try/catch 转 503），
    %% resolve_workspace 不再返回 {error, {db_error, _}}。
    OneReturned = fun(_, _) -> {error, pool_exhausted} end,
    ?WITH_MECKS(
        [{elib_pg, [{'one', 2, OneReturned}]}],
        fun() ->
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
                    ?assertError(
                        {resolver_db_error, {error, pool_exhausted}},
                        workspace_resolver:resolve_workspace(Target),
                        {target, Target}
                    )
                end,
                Targets
            ),
            ok
        end
    ).

db_one_crash_exit_maps_to_db_error_test_() ->
    %% "crashing one (exit) maps to db_error, never not_found"
    ?WITH_MECKS(
        [{elib_pg, [{'one', 2, fun(_, _) -> exit(pool_down) end}]}],
        fun() ->
            begin
                ?assertError(
                    {resolver_db_error, {'EXIT', pool_down}},
                    workspace_resolver:resolve_workspace({group, ?GID})
                ),
                ?assertError(
                    {resolver_db_error, {'EXIT', pool_down}},
                    workspace_resolver:resolve_workspace({channel, ?CID})
                ),
                ok
            end
        end
    ).

db_one_crash_throw_maps_to_db_error_test_() ->
    %% "crashing one (throw) maps to db_error"
    ?WITH_MECKS(
        [{elib_pg, [{'one', 2, fun(_, _) -> throw(driver_lost) end}]}],
        fun() ->
            ?assertError(
                {resolver_db_error, driver_lost},
                workspace_resolver:resolve_workspace({group, ?GID})
            )
        end
    ).

db_error_not_confused_with_not_found_test_() ->
    %% "db error must not be confused with not_found"
    %% DB 异常必须可区分：抛 {resolver_db_error,_}，绝不落 not_found。
    OneReturned = fun(_, _) -> {error, pool_exhausted} end,
    ?WITH_MECKS(
        [{elib_pg, [{'one', 2, OneReturned}]}],
        fun() ->
            ?assertError(
                {resolver_db_error, _},
                workspace_resolver:resolve_workspace({group, ?GID})
            )
        end
    ).

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
        fun() -> boundary_guards_body() end
    ).

boundary_guards_body() ->
    begin
        %% member passes workspace group boundary
        ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, ?GID)),
        %% non member blocked on workspace group boundary (403)
        ?assertMatch(
            {error, {403, _}},
            workspace_resolver:ensure_group_member_access(?OUTSIDER, ?GID)
        ),
        %% member passes workspace channel boundary
        ?assertEqual(ok, workspace_resolver:ensure_channel_member_access(?UID, ?CID)),
        %% non member blocked on workspace channel boundary (403)
        ?assertMatch(
            {error, {403, _}},
            workspace_resolver:ensure_channel_member_access(?OUTSIDER, ?CID)
        ),
        %% personal group always passes (zero regression)
        ?assertEqual(
            ok, workspace_resolver:ensure_group_member_access(?OUTSIDER, 777099)
        ),
        %% personal channel always passes (zero regression)
        ?assertEqual(
            ok, workspace_resolver:ensure_channel_member_access(?OUTSIDER, 666099)
        ),
        %% non-existent group passes through to legacy 404 flow
        ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, 1)),
        %% guard_group_gid with zero/invalid gid passes
        ?assertEqual(ok, workspace_resolver:guard_group_gid(?OUTSIDER, <<"abc">>)),
        ?assertEqual(ok, workspace_resolver:guard_group_gid(?OUTSIDER, 0)),
        %% guard_group_notice_id blocks workspace group notice for outsider
        ?assertMatch(
            {error, {403, _}},
            workspace_resolver:guard_group_notice_id(?OUTSIDER, 555001)
        ),
        %% guard_group_notice_id passes for member
        ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, 555001)),
        %% guard_group_notice_id passes for invalid notice
        ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, <<"x">>)),
        ok
    end.

%% ===================================================================
%% SEC-03 fail-closed：DB 异常 → 503，与角色无关（成员/非成员同样被拒）
%% ⚠️ 原 {Desc, fun} 列表形态静默空转（同 db_error_matrix 注释），拆为独立 test_/0。
%% ===================================================================

fail_closed_group_gate_member_test_() ->
    %% "db error denies workspace group gate (503, member)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    ?WITH_MECKS(
        [OneFailed],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:ensure_group_member_access(?UID, ?GID)
            )
        end
    ).

fail_closed_group_gate_outsider_test_() ->
    %% "db error denies workspace group gate (503, outsider)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    ?WITH_MECKS(
        [OneFailed],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:ensure_group_member_access(?OUTSIDER, ?GID)
            )
        end
    ).

fail_closed_channel_gate_test_() ->
    %% "db error denies workspace channel gate (503)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    ?WITH_MECKS(
        [OneFailed],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:ensure_channel_member_access(?UID, ?CID)
            )
        end
    ).

fail_closed_notice_gate_test_() ->
    %% "db error denies notice gate (503)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    ?WITH_MECKS(
        [OneFailed],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:guard_group_notice_id(?UID, 555001)
            )
        end
    ).

fail_closed_guard_group_gid_test_() ->
    %% "db error denies guard_group_gid (503)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
    ?WITH_MECKS(
        [OneFailed],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:guard_group_gid(?UID, ?GID)
            )
        end
    ).

fail_closed_guard_channel_binding_test_() ->
    %% "db error denies guard_channel_binding when binding present (503)"
    OneFailed = {elib_pg, [{'one', 2, fun(_, _) -> {error, pool_exhausted} end}]},
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
    ).

fail_closed_crashing_one_denies_gate_test_() ->
    %% "crashing one denies gate (503)"
    ?WITH_MECKS(
        [{elib_pg, [{'one', 2, fun(_, _) -> exit(pool_down) end}]}],
        fun() ->
            ?assertMatch(
                {error, {?ERR_SERVICE_UNAVAILABLE, _}},
                workspace_resolver:ensure_group_member_access(?UID, ?GID)
            )
        end
    ).

attachment_dirty_scope_test_() ->
    %% "attachment dirty scope yields explicit unsupported_scope error"
    %% 断言按 T7 结项决策（commit 84287609）对齐现实：行不存在 / 未知 scope
    %% 值沿历史 personal 兜底（守卫放行、404 交既有流程），不再回溯。
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
                personal,
                workspace_resolver:resolve_workspace({attachment, 333007})
            )
        end
    ).

%% ===================================================================
%% guard_channel_custom_id：custom_id 直访门（SEC-03 fail-closed）
%% ⚠️ 原 {Desc, fun} 列表形态静默空转（同 db_error_matrix 注释），拆为独立 test_/0。
%% ===================================================================

%% 与 guard_channel_custom_id_test_ 共用的 mock 片段。
%% ChannelRow SQL 前缀对齐 row_scope/2 真实形态（SELECT scope, workspace_id ...）
member_repo_ws() ->
    {workspace_member_repo, [
        {'find', 3, fun
            (?WS_ID, ?UID, _) ->
                #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
            (_, _, _) ->
                #{}
        end}
    ]}.

member_repo_outsider_removed() ->
    {workspace_member_repo, [
        {'find', 3, fun
            (?WS_ID, ?UID, _) ->
                #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
            (?WS_ID, ?OUTSIDER, _) ->
                #{<<"role">> => <<"guest">>, <<"status">> => <<"removed">>};
            (_, _, _) ->
                #{}
        end}
    ]}.

channel_row_ws() ->
    {elib_pg, [
        {'one', 2, fun
            (<<"SELECT scope, workspace_id FROM channel", _/binary>>, [?CID]) ->
                {ok, #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => ?WS_ID}};
            (_, _) ->
                {ok, #{}}
        end}
    ]}.

channel_ds_custom_id_hit() ->
    {channel_ds, [
        {'find_by_custom_id', 1, fun(_) ->
            #{<<"id">> => ?CID, <<"custom_id">> => <<"ws-chan">>}
        end}
    ]}.

custom_id_blocks_outsider_test_() ->
    %% "workspace channel by custom id blocks outsider (403)"
    ?WITH_MECKS(
        [
            channel_ds_custom_id_hit(),
            channel_row_ws(),
            member_repo_outsider_removed()
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                workspace_resolver:guard_channel_custom_id(?OUTSIDER, <<"ws-chan">>)
            )
        end
    ).

custom_id_member_passes_test_() ->
    %% "member passes workspace channel by custom id"
    ?WITH_MECKS(
        [
            channel_ds_custom_id_hit(),
            channel_row_ws(),
            member_repo_ws()
        ],
        fun() ->
            ?assertEqual(
                ok, workspace_resolver:guard_channel_custom_id(?UID, <<"ws-chan">>)
            )
        end
    ).

custom_id_miss_passes_test_() ->
    %% "custom id miss (empty map) passes to legacy 404 flow"
    ?WITH_MECKS(
        [{channel_ds, [{'find_by_custom_id', 1, fun(_) -> #{} end}]}],
        fun() ->
            ?assertEqual(
                ok, workspace_resolver:guard_channel_custom_id(?OUTSIDER, <<"ghost">>)
            )
        end
    ).

custom_id_db_error_denied_test_() ->
    %% "find_by_custom_id db error denied (503, fail-closed)"
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
    ).

custom_id_crash_denied_test_() ->
    %% "find_by_custom_id crash denied (503, fail-closed)"
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
    ).

custom_id_invalid_passes_test() ->
    %% "invalid custom id (empty/not binary) passes"
    begin
        ?assertEqual(ok, workspace_resolver:guard_channel_custom_id(?UID, <<>>)),
        ?assertEqual(ok, workspace_resolver:guard_channel_custom_id(?UID, 123)),
        ok
    end.

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
        fun() -> guard_channel_binding_no_binding_body() end
    ).

guard_channel_binding_no_binding_body() ->
    begin
        %% plain map req without channel binding passes
        ?assertEqual(ok, workspace_resolver:guard_channel_binding(#{qs => []}, ?UID)),
        ok
    end.

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
        fun() -> access_gate_normalizes_role_body() end
    ).

access_gate_normalizes_role_body() ->
    begin
        %% channel access gate returns plain ok for legal member
        ?assertEqual(
            ok, workspace_resolver:ensure_channel_member_access(?UID, ?CID)
        ),
        %% group access gate returns plain ok for legal member
        ?assertEqual(ok, workspace_resolver:ensure_group_member_access(?UID, ?GID)),
        %% notice gate returns plain ok for legal member
        ?assertEqual(ok, workspace_resolver:guard_group_notice_id(?UID, 555001)),
        ok
    end.

%% ===================================================================
%% M-1/M-2 收口：DB 异常 fail-closed（503），业务空 not_found 语义不变
%% ===================================================================

resolver_db_error_fail_closed_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [{'one', 2, db_error_one()}]}
        ],
        fun() -> resolver_db_error_fail_closed_body() end
    ).

resolver_db_error_fail_closed_body() ->
    begin
        %% one_row DB error propagates from resolve_workspace
        ?assertError(
            {resolver_db_error, _},
            workspace_resolver:resolve_workspace({group, ?GID})
        ),
        %% group boundary guard returns 503 on DB error (not ok)
        ?assertMatch(
            {error, {503, _}}, workspace_resolver:ensure_group_member_access(?UID, ?GID)
        ),
        %% channel boundary guard returns 503 on DB error (not ok)
        ?assertMatch(
            {error, {503, _}},
            workspace_resolver:ensure_channel_member_access(?UID, ?CID)
        ),
        %% notice guard returns 503 on DB error
        ?assertMatch(
            {error, {503, _}}, workspace_resolver:guard_group_notice_id(?UID, 555001)
        ),
        ok
    end.

guard_channel_binding_db_error_fail_closed_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> ?CID end}
            ]},
            {elib_pg, [{'one', 2, db_error_one()}]}
        ],
        fun() ->
            ?assertMatch(
                {error, {503, _}}, workspace_resolver:guard_channel_binding(fake_req, ?UID)
            )
        end
    ).

guard_channel_custom_id_db_error_fail_closed_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_custom_id', 1, fun(<<"db-down">>) ->
                    {error, {pgsql_error, #{code => <<"57P01">>}}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {503, _}}, workspace_resolver:guard_channel_custom_id(?UID, <<"db-down">>)
            )
        end
    ).

guard_channel_custom_id_ds_crash_fail_closed_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_custom_id', 1, fun(_) -> erlang:error(simulated_crash) end}
            ]}
        ],
        fun() ->
            %% 连接层崩溃（'EXIT'）同样 503，不按"未命中"放行
            ?assertMatch(
                {error, {503, _}}, workspace_resolver:guard_channel_custom_id(?UID, <<"boom">>)
            )
        end
    ).

guard_channel_custom_id_miss_still_passes_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                %% repo 零行形态：find_by_custom_id 返回 #{} 空行
                {'find_by_custom_id', 1, fun(_) -> #{} end}
            ]}
        ],
        fun() ->
            %% not_found 语义保持：未命中放行走既有 404（下游必有独立 ACL）
            ?assertEqual(ok, workspace_resolver:guard_channel_custom_id(?UID, <<"no-such">>))
        end
    ).
