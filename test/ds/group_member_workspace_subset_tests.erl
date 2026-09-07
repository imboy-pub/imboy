-module(group_member_workspace_subset_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — Group Member ⊆ Workspace Member 应用层子集校验
%%% 覆盖：workspace 群入群的同事务校验（非 wm → abort_tx 回滚；
%%% active wm → 放行）、personal 群零行为变化（不查 workspace_member）、
%%% handler 层 409 workspace_membership_required 稳定错误码。

-define(UID, 900001).
-define(GID, 777001).
-define(WS_ID, 800001).

%% ===================================================================
%% DS 层：join_group/5 同事务校验
%% ===================================================================

join_group_subset_test_() ->
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) -> subset_query(Sql, Params) end},
                {'execute', 3, fun(_Conn, _, _) -> {ok, 1} end},
                {'update', 5, fun(_Conn, _Tb, _Data, _Where, _Params) -> {ok, 1} end}
            ]},
            {group_member_repo, [
                {'upsert_active', 5, fun(_Conn, _Gid, Uid, _Role, _JoinMode) ->
                    put(t_gm_added, Uid),
                    {ok, true}
                end}
            ]},
            {group_ds, [
                {'join', 2, fun(Uid, Gid) ->
                    put(t_gm_cache_join, {Uid, Gid}),
                    ok
                end}
            ]}
        ],
        fun() -> join_group_subset_body() end
    ).

join_group_subset_body() ->
    begin
        %% active workspace member joins workspace group
        ?assertMatch(
            {ok, _},
            group_member_ds:join_group(fake_conn, <<"invite">>, ?UID, ?GID, #{})
        ),
        ?assertEqual(?UID, erase(t_gm_added), "member row not written"),

        %% non workspace member join aborts tx (no member row)
        ?assertThrow(
            {abort_tx, workspace_membership_required},
            group_member_ds:join_group(fake_conn, <<"invite">>, 900009, ?GID, #{})
        ),
        ?assert(undefined =:= get(t_gm_added), "must not write member row"),

        %% removed workspace member join also aborts
        %% (workspace_member 行存在但 status=removed → SELECT active 不命中)
        ?assertThrow(
            {abort_tx, workspace_membership_required},
            group_member_ds:join_group(fake_conn, <<"invite">>, 910010, ?GID, #{})
        ),

        %% personal group skips workspace check entirely
        ?assertMatch(
            {ok, _},
            group_member_ds:join_group(fake_conn, <<"invite">>, 900009, 777099, #{})
        ),
        ?assertEqual(900009, erase(t_gm_added)),

        %% non-existent group falls through to legacy flow
        ?assertMatch(
            {ok, _},
            group_member_ds:join_group(fake_conn, <<"invite">>, ?UID, 1, #{})
        ),
        ok
    end.

%% elib_pg:query/3 mock：按 SQL 前缀返回受控行
subset_query(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [?GID]) ->
    %% workspace 群
    {ok, [#{<<"workspace_id">> => ?WS_ID}]};
subset_query(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [777099]) ->
    %% personal 群：真实 SQL 带 AND scope = 'workspace'，personal 行不返回
    %% （原 mock 返回 null 行与真实查询形态漂移，ensure_workspace_membership
    %%  的 case 无此分支，真实执行即 case_clause）
    {ok, []};
subset_query(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [1]) ->
    %% 群不存在
    {ok, []};
subset_query(<<"SELECT role FROM workspace_member", _/binary>>, [?WS_ID, ?UID]) ->
    %% active 工作区成员
    {ok, [#{<<"role">> => <<"member">>}]};
subset_query(<<"SELECT role FROM workspace_member", _/binary>>, [_, 910010]) ->
    %% removed 工作区成员（status=removed → active 查询不命中 → 空行）
    {ok, []};
subset_query(<<"SELECT role FROM workspace_member", _/binary>>, [_, _]) ->
    {ok, []};
subset_query(<<"SELECT COUNT(*)", _/binary>>, [_]) ->
    %% update_statistics 的聚合查询（P0 后 COUNT-only）
    {ok, [#{<<"member_count">> => 2}]};
subset_query(_, _) ->
    {ok, []}.

%% ===================================================================
%% with_tx 包装语义：abort_tx → {error, workspace_membership_required}
%% ===================================================================

with_tx_maps_abort_test_() ->
    %% 真实 elib_pg:with_tx 需要连接池（WITH_MECKS 不启动 app，真池必 noproc），
    %% 故按 elib_pg 真实契约（throw:{abort_tx, Reason} → ROLLBACK + {error, Reason}）
    %% 使用仓内既定 faithful mock（与 handler_join_409 / group_scope 同型）。
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) -> subset_query(Sql, Params) end},
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(_, _, _) -> #{} end}
            ]}
        ],
        fun() -> with_tx_maps_abort_body() end
    ).

with_tx_maps_abort_body() ->
    begin
        %% abort_tx surfaces as {error, workspace_membership_required}
        ?assertEqual(
            {error, workspace_membership_required},
            elib_pg:with_tx(fun(_Conn) ->
                group_member_ds:join_group(fake_conn, <<"invite">>, 900009, ?GID, #{})
            end)
        ),
        ok
    end.

%% ===================================================================
%% Handler 层：join_with_capacity 返回稳定 409
%% ===================================================================

handler_join_409_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_) -> ?UID end}
            ]},
            {elib_param, [
                {'post', 1, fun(_) ->
                    #{
                        <<"gid">> => ?GID,
                        <<"member_uids">> => [<<"900009">>],
                        <<"join_mode">> => <<"invite">>
                    }
                end}
            ]},
            {throttle, [
                {'check', 2, fun(_, _) -> ok end}
            ]},
            {group_member_logic, [
                {'find_by_gid_and_uid', 3, fun(_, _, _) -> #{<<"id">> => 1} end},
                {'get_group_capacity', 1, fun(_) ->
                    #{<<"member_max">> => 100, <<"member_count">> => 1}
                end},
                {'list_member', 2, fun(_, _) -> {ok, []} end},
                {'join_group', 5, fun(_Conn, _Mode, Uid, Gid, _) ->
                    put(t_gm_ds_join, {Uid, Gid}),
                    throw({abort_tx, workspace_membership_required})
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
            ]}
        ],
        fun() -> handler_join_409_body() end
    ).

handler_join_409_body() ->
    begin
        %% workspace group join returns 409 stable code
        %% （handler 真实入口是 init/2，State 携带 action——原用例假设的
        %% handle_action/3 不存在，属空转掩盖的 API 漂移）
        {ok, Req, _} = group_member_handler:init(
            req0, #{action => join, current_uid => ?UID}
        ),
        ?assertEqual(409, maps:get(response_status, Req)),
        ?assertMatch({900009, ?GID}, erase(t_gm_ds_join), "ds join not attempted"),
        ok
    end.
