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
    Self = self(),
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    Self ! {ensure_create, WsId, Uid},
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {group_ds, [
                {'create_scoped_group', 7, fun(_Conn, Gid, Uid, _Now, _Type, Scope, WsId) ->
                    Self ! {scoped_group_created, Gid, Uid, Scope, WsId},
                    Gid
                end},
                {'find_by_creator_and_sum', 2, fun(_, _) -> 0 end},
                {'create_group', 6, fun(_Conn, _Gid, Uid, _Now, _Type, _Jl) ->
                    Self ! {legacy_group_created, Uid},
                    ?GID
                end}
            ]},
            {user_ds, [
                {'title', 1, fun(_) -> <<"t">> end}
            ]},
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, Uid, Gid, _) ->
                    Self ! {join, Uid, Gid},
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
                {'insert', 3, fun(_Conn, _Tb, _Data, _) -> {ok, 1} end},
                {'insert', 4, fun(_Conn, _Tb, _Data, _R, _) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(group_info) -> ?GID end}
            ]}
        ],
        fun() ->
            [
                {"guest cannot create workspace group (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        group_logic:add(0, 900002, 2, [], {<<"workspace">>, ?WS_ID})
                    )
                end},
                {"non workspace member cannot create workspace group (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        group_logic:add(0, 900003, 2, [], {<<"workspace">>, ?WS_ID})
                    )
                end},
                {"workspace scope without workspace_id is 400", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        group_logic:add(0, ?UID, 2, [], {<<"workspace">>, 0})
                    )
                end},
                {"invalid scope value is 400", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        group_logic:add(0, ?UID, 2, [], {<<"team">>, ?WS_ID})
                    )
                end},
                {"owner creates workspace group with scope fields", fun() ->
                    ?assertMatch(
                        {ok, ?GID},
                        group_logic:add(0, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
                    ),
                    receive
                        {scoped_group_created, ?GID, ?UID, <<"workspace">>, ?WS_ID} -> ok
                    after 500 -> ?assert(false, "scoped group not created")
                    end
                end},
                {"workspace group create blocked by membership subset rolls back", fun() ->
                    meck(group_member_ds, [
                        {'join_group', 5, fun(_, _, _, _, _) ->
                            throw({abort_tx, workspace_membership_required})
                        end}
                    ]),
                    ?assertMatch(
                        {error, {409, _}},
                        group_logic:add(0, ?UID, 2, [<<"900009">>], {<<"workspace">>, ?WS_ID})
                    )
                end},
                {"personal scope delegates to legacy add/4 (zero change)", fun() ->
                    ?assertMatch(
                        {ok, ?GID},
                        group_logic:add(0, ?UID, 2, [], {<<"personal">>, 0})
                    ),
                    receive
                        {legacy_group_created, ?UID} -> ok
                    after 500 -> ?assert(false)
                    end,
                    receive
                        {ensure_create, _, _} -> ?assert(false, "personal must not check role")
                    after 0 -> ok
                    end,
                    receive
                        {scoped_group_created, _, _, _, _} ->
                            ?assert(false, "personal must not create scoped group")
                    after 0 -> ok
                    end
                end},
                {"creation limit still enforced on workspace path", fun() ->
                    ?assertMatch(
                        {error, <<"每人最多创建100个群"/utf8>>},
                        group_logic:add(101, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
                    )
                end}
            ]
        end
    ).

edit_checked_rejects_scope_mutation_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                %% edit/3 真实实现会查群成员；提交 scope 时必须早退不触达
                {'find_by_gid_and_uid', 3, fun(_, _, _) -> throw(must_not_reach) end}
            ]}
        ],
        fun() ->
            [
                {"scope immutable on group edit (400)", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        group_logic:edit_checked(?UID, ?GID, #{<<"scope">> => <<"workspace">>})
                    )
                end},
                {"workspace_id immutable on group edit (400)", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        group_logic:edit_checked(?UID, ?GID, #{<<"workspace_id">> => 1})
                    )
                end},
                {"normal fields pass through", fun() ->
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
                    )
                end}
            ]
        end
    ).

list_workspace_groups_partitions_by_scope_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    Self ! {list_sql, Sql, ?WS_ID, Limit},
                    {ok, [#{<<"id">> => ?GID, <<"title">> => <<"General">>}]}
                end}
            ]},
            {group_logic, []}
        ],
        fun() ->
            {"workspace group list filters scope strictly", fun() ->
                ?assertMatch(
                    {ok, [#{<<"title">> := <<"General">>}]},
                    group_logic:list_workspace_groups(?WS_ID, 50)
                ),
                receive
                    {list_sql, Sql, ?WS_ID, 50} ->
                        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
                        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch)
                after 500 -> ?assert(false)
                end
            end}
        end
    ).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
