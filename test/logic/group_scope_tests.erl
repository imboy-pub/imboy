-module(group_scope_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — group scope 感知单元测试
%%% 覆盖：group_logic:add/5 scope 校验（Guest/非成员 403、非法 scope/ws_id 400、
%%% personal 分流零变化、workspace 创建走 create_scoped_group）、
%%% edit_checked scope 不可变、list_workspace_groups scope 严格分区。

-define(UID, 900001).
-define(WS_ID, 800001).
-define(GID, 777001).

add_scope_validation_test_() ->
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    put(t_gs_ensure_create, {WsId, Uid}),
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {group_ds, [
                {'create_scoped_group', 7, fun(_Conn, Gid, Uid, _Now, _Type, Scope, WsId) ->
                    put(t_gs_scoped_created, {Gid, Uid, Scope, WsId}),
                    Gid
                end},
                {'find_by_creator_and_sum', 2, fun(_, _) -> 0 end},
                {'create_group', 6, fun(_Conn, _Gid, Uid, _Now, _Type, _Jl) ->
                    put(t_gs_legacy_created, Uid),
                    ?GID
                end}
            ]},
            {user_ds, [
                {'title', 1, fun(_) -> <<"t">> end}
            ]},
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, Uid, Gid, _) ->
                    put(t_gs_join, {Uid, Gid}),
                    {ok, Uid}
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(_, _, _) -> #{} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end},
                %% workspace_guard:ensure_writable_tx 依赖（原 mock 缺失会
                %% passthrough 打真库/假连接崩溃）：
                %% resolve_workspace({workspace, WsId}) → SELECT id FROM workspace
                {'one', 2, fun
                    (<<"SELECT id FROM workspace", _/binary>>, [?WS_ID]) ->
                        {ok, #{<<"id">> => ?WS_ID}};
                    (_, _) ->
                        {ok, #{}}
                end},
                %% FOR UPDATE status 行锁查询
                {'query', 3, fun(_Conn, <<"SELECT status FROM workspace", _/binary>>, [?WS_ID]) ->
                    {ok, [#{<<"status">> => <<"active">>}]}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(group_info) -> ?GID end}
            ]}
        ],
        fun() -> add_scope_validation_body() end
    ).

add_scope_validation_body() ->
    begin
        %% guest cannot create workspace group (403)
        ?assertMatch(
            {error, {403, _}},
            group_logic:add(0, 900002, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        %% non workspace member cannot create workspace group (403)
        ?assertMatch(
            {error, {403, _}},
            group_logic:add(0, 900003, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        %% workspace scope without workspace_id is 400
        ?assertMatch(
            {error, {400, _}},
            group_logic:add(0, ?UID, 2, [], {<<"workspace">>, 0})
        ),
        %% invalid scope value is 400
        ?assertMatch(
            {error, {400, _}},
            group_logic:add(0, ?UID, 2, [], {<<"team">>, ?WS_ID})
        ),
        %% owner creates workspace group with scope fields
        ?assertMatch(
            {ok, ?GID},
            group_logic:add(0, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        ?assertEqual(
            {?GID, ?UID, <<"workspace">>, ?WS_ID},
            erase(t_gs_scoped_created),
            "scoped group not created"
        ),
        %% workspace group create blocked by membership subset rolls back
        meck(group_member_ds, [
            {'join_group', 5, fun(_, _, _, _, _) ->
                throw({abort_tx, workspace_membership_required})
            end}
        ]),
        ?assertMatch(
            {error, {409, _}},
            group_logic:add(0, ?UID, 2, [<<"900009">>], {<<"workspace">>, ?WS_ID})
        ),
        %% personal scope delegates to legacy add/4 (zero change)
        %% 段内哨兵先清零（先前段遗留 put 会使反向断言误报）
        erase(t_gs_ensure_create),
        erase(t_gs_scoped_created),
        ?assertMatch(
            {ok, ?GID},
            group_logic:add(0, ?UID, 2, [], {<<"personal">>, 0})
        ),
        ?assertEqual(?UID, erase(t_gs_legacy_created)),
        ?assert(undefined =:= get(t_gs_ensure_create), "personal must not check role"),
        ?assert(
            undefined =:= get(t_gs_scoped_created),
            "personal must not create scoped group"
        ),
        %% creation limit still enforced on workspace path
        ?assertMatch(
            {error, <<"每人最多创建100个群"/utf8>>},
            group_logic:add(101, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        ok
    end.

edit_checked_rejects_scope_mutation_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                %% edit/3 真实实现会查群成员；提交 scope 时必须早退不触达
                {'find_by_gid_and_uid', 3, fun(_, _, _) -> throw(must_not_reach) end}
            ]}
        ],
        fun() -> edit_checked_rejects_scope_mutation_body() end
    ).

edit_checked_rejects_scope_mutation_body() ->
    begin
        %% scope immutable on group edit (400)
        ?assertMatch(
            {error, {400, _}},
            group_logic:edit_checked(?UID, ?GID, #{<<"scope">> => <<"workspace">>})
        ),
        %% workspace_id immutable on group edit (400)
        ?assertMatch(
            {error, {400, _}},
            group_logic:edit_checked(?UID, ?GID, #{<<"workspace_id">> => 1})
        ),
        %% normal fields pass through
        meck(group_member_ds, [
            {'find_by_gid_and_uid', 3, fun(_, _, _) ->
                #{<<"id">> => 1, <<"role">> => 4}
            end}
        ]),
        meck(group_ds, [
            {'exists', 1, fun(_) -> true end},
            {'update_by_id', 2, fun(_, _) -> {ok, 1} end},
            {'member_uids', 1, fun(_) -> [] end}
        ]),
        meck(msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
        ]),
        ?assertMatch(
            ok, group_logic:edit_checked(?UID, ?GID, #{<<"title">> => <<"new">>})
        ),
        %% 中段 re-mock 不在 WITH_MECKS 清理清单内，必须显式卸载，
        %% 否则泄漏到后续套件（实测污染 closure 套件的 group_ds 守卫）
        meck_helper:cleanup_mock(group_ds),
        meck_helper:cleanup_mock(msg_s2c_ds),
        meck_helper:cleanup_mock(group_member_ds),
        ok
    end.

list_workspace_groups_partitions_by_scope_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    put(t_gs_list_sql, {Sql, Limit}),
                    {ok, [#{<<"id">> => ?GID, <<"title">> => <<"General">>}]}
                end}
            ]},
            {group_logic, []}
        ],
        fun() -> list_workspace_groups_partitions_by_scope_body() end
    ).

list_workspace_groups_partitions_by_scope_body() ->
    begin
        %% workspace group list filters scope strictly
        ?assertMatch(
            {ok, [#{<<"title">> := <<"General">>}]},
            group_logic:list_workspace_groups(?WS_ID, 50)
        ),
        {Sql, 50} = erase(t_gs_list_sql),
        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch),
        ok
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
