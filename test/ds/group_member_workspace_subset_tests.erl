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
    Self = self(),
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) -> subset_query(Sql, Params) end},
                {'execute', 3, fun(_Conn, _, _) -> {ok, 1} end},
                {'update', 5, fun(_Conn, _Tb, _Data, _Where, _Params) -> {ok, 1} end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(_, _, _) -> #{} end},
                {'add', 2, fun(_Conn, Data) ->
                    Self ! {member_added, maps:get(user_id, Data)},
                    {ok, 1}
                end}
            ]},
            {group_ds, [
                {'join', 2, fun(Uid, Gid) ->
                    Self ! {cache_join, Uid, Gid},
                    ok
                end}
            ]}
        ],
        fun() ->
            [
                {"active workspace member joins workspace group", fun() ->
                    ?assertMatch(
                        {ok, _},
                        group_member_ds:join_group(fake_conn, <<"invite">>, ?UID, ?GID, #{})
                    ),
                    receive
                        {member_added, ?UID} -> ok
                    after 500 -> ?assert(false, "member row not written")
                    end
                end},
                {"non workspace member join aborts tx (no member row)", fun() ->
                    ?assertThrow(
                        {abort_tx, workspace_membership_required},
                        group_member_ds:join_group(fake_conn, <<"invite">>, 900009, ?GID, #{})
                    ),
                    receive
                        {member_added, 900009} -> ?assert(false, "must not write member row")
                    after 0 -> ok
                    end
                end},
                {"removed workspace member join also aborts", fun() ->
                    %% workspace_member 行存在但 status=removed → SELECT active 不命中
                    ?assertThrow(
                        {abort_tx, workspace_membership_required},
                        group_member_ds:join_group(fake_conn, <<"invite">>, 910010, ?GID, #{})
                    )
                end},
                {"personal group skips workspace check entirely", fun() ->
                    ?assertMatch(
                        {ok, _},
                        group_member_ds:join_group(fake_conn, <<"invite">>, 900009, 777099, #{})
                    ),
                    receive
                        {member_added, 900009} -> ok
                    after 500 -> ?assert(false)
                    end
                end},
                {"non-existent group falls through to legacy flow", fun() ->
                    ?assertMatch(
                        {ok, _},
                        group_member_ds:join_group(fake_conn, <<"invite">>, ?UID, 1, #{})
                    )
                end}
            ]
        end
    ).

%% elib_pg:query/3 mock：按 SQL 前缀返回受控行
subset_query(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [?GID]) ->
    %% workspace 群
    {ok, [#{<<"workspace_id">> => ?WS_ID}]};
subset_query(<<"SELECT workspace_id FROM \"group\"", _/binary>>, [777099]) ->
    %% personal 群（scope=personal 时 workspace_id 为 null）
    {ok, [#{<<"workspace_id">> => null}]};
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
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) -> subset_query(Sql, Params) end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(_, _, _) -> #{} end}
            ]}
        ],
        fun() ->
            {"abort_tx surfaces as {error, workspace_membership_required}", fun() ->
                ?assertEqual(
                    {error, workspace_membership_required},
                    elib_pg:with_tx(fun(_Conn) ->
                        group_member_ds:join_group(fake_conn, <<"invite">>, 900009, ?GID, #{})
                    end)
                )
            end}
        end
    ).

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
                    self() ! {ds_join, Uid, Gid},
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
        fun() ->
            {"workspace group join returns 409 stable code", fun() ->
                Req = group_member_handler:handle_action(
                    join, req0, #{current_uid => ?UID}
                ),
                ?assertEqual(409, maps:get(response_status, Req)),
                receive
                    {ds_join, 900009, ?GID} -> ok
                after 0 -> ?assert(false, "ds join not attempted")
                end
            end}
        end
    ).
