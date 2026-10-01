-module(organization_invite_code_app_tests).

%% GZAPP-01 Invite Code 命令（application 层）测试：
%% 治理门（owner/admin）、archived 409、跨 Org 码 981（不泄露存在性）、
%% 过期 982、幂等、code 冲突重试与 join 编排接线。
%% SQL 真实行为由一次性 PG 上的 organization_invite_code_behavior_harness.escript 覆盖。

-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(ORG_ID, 301).
-define(ORG_OTHER, 399).
-define(OWNER, 401).
-define(MEMBER, 403).
-define(TARGET, 402).

%% elib_pg:with_tx 同语义直通：在测试进程内以 fake_conn 执行事务体。
run_tx(TxFun) ->
    try TxFun(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        throw:{abort_tx, Reason} ->
            {error, Reason};
        throw:{rollback, Reason} ->
            {rollback, Reason};
        Class:Reason ->
            {error, {db_exception, Class, Reason}}
    end.

%%--------------------------------------------------------------------
%% mock 装配
%%--------------------------------------------------------------------

code_row() ->
    #{
        <<"id">> => 820000001,
        <<"organization_id">> => ?ORG_ID,
        <<"code">> => <<"ABCD2345">>,
        <<"created_by">> => ?OWNER,
        <<"role">> => <<"member">>,
        <<"expires_at">> => os:system_time(second) + 604800,
        <<"expired">> => false,
        <<"status">> => <<"active">>,
        <<"created_at">> => os:system_time(second),
        <<"updated_at">> => os:system_time(second)
    }.

default_mocks() ->
    [
        {elib_pg, [
            {'with_tx', 1, fun run_tx/1}
        ]},
        {organization_invite_code_pg, [
            {'generate_code', 0, fun() -> <<"ZZZZ9999">> end},
            {'revoke_active_by_org_tx', 2, fun(fake_conn, _OrgId) -> {ok, 0} end},
            {'add_tx', 6, fun(fake_conn, _OrgId, Code, _By, _Exp, _Role) ->
                {ok, (code_row())#{<<"code">> => Code}}
            end},
            {'find_active_by_org_tx', 2, fun(fake_conn, _OrgId) ->
                {ok, code_row()}
            end},
            {'find_active_by_code_tx', 3, fun(fake_conn, _OrgId, _Code) ->
                {ok, code_row()}
            end},
            {'find_active_by_code_for_share_tx', 3, fun(Conn, OrgId, Code) ->
                organization_invite_code_pg:find_active_by_code_tx(Conn, OrgId, Code)
            end},
            {'find_active_by_code_global_tx', 2, fun(fake_conn, _Code) ->
                {ok, code_row()}
            end}
        ]},
        %% 治理门依赖（镜像 organization_member_logic:write_tx 锁序）
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(fake_conn, _OrgId, _Cols) ->
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"name">> => <<"演示组织"/utf8>>,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_active_for_share_tx', 4, fun(fake_conn, _OrgId, Uid, _Cols) ->
                case get({t_member, Uid}) of
                    undefined -> {error, not_found};
                    Role -> {ok, #{<<"role">> => Role}}
                end
            end}
        ]},
        %% join 编排挂点（编排行为由 organization_join_orchestrator_tests 冻结）
        {organization_join_orchestrator, [
            {'join_tx', 5, fun(fake_conn, OrgId, Uid, _InvitedBy, _Role) ->
                self() ! {orchestrator_join, fake_conn, OrgId, Uid},
                {ok, joined, #{organization_id => OrgId, workspace_id => none}}
            end}
        ]}
    ].

with_mocks(Extra, TestFun) ->
    Mocks = merge_mocks(default_mocks(), Extra),
    Mods = [M || {M, _} <- Mocks],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    lists:foreach(
        fun({M, Expects}) ->
            lists:foreach(
                fun({Name, Arity, Fun}) -> meck:expect(M, Name, Arity, Fun) end,
                Expects
            )
        end,
        Mocks
    ),
    try
        TestFun()
    after
        lists:foreach(fun(M) -> meck:unload(M) end, Mods),
        lists:foreach(
            fun(K) -> erlang:erase(K) end,
            [{t_member, ?OWNER}, {t_member, ?MEMBER}]
        )
    end.

%% Base/Extra 均为 [{Module, [{Name, Arity, Fun}]}]：
%% 同 Module 同 Name 的替换，同 Module 新增的追加。
merge_mocks(Base, Extra) ->
    AllMods = lists:usort([M || {M, _} <- Base] ++ [M || {M, _} <- Extra]),
    [
        begin
            BaseExpects = proplists:get_value(M, Base, []),
            ExtraExpects = proplists:get_value(M, Extra, []),
            Overridden = [Name || {Name, _A, _F} <- ExtraExpects],
            Kept = [E || E = {Name, _A, _F} <- BaseExpects, not lists:member(Name, Overridden)],
            {M, Kept ++ ExtraExpects}
        end
     || M <- AllMods
    ].

set_role(Uid, Role) ->
    put({t_member, Uid}, Role).

%%--------------------------------------------------------------------
%% create
%%--------------------------------------------------------------------

create_success_test_() ->
    {"create 成功：owner 建码（同事务先撤旧）；视图含 code/expires_at", fun() ->
        with_mocks([], fun() ->
            set_role(?OWNER, <<"owner">>),
            {ok, View} = organization_invite_code_app:create(?OWNER, ?ORG_ID, #{}),
            ?assertEqual(<<"ZZZZ9999">>, maps:get(code, View)),
            ?assertEqual(<<"active">>, maps:get(status, View)),
            ?assert(is_integer(maps:get(expires_at, View))),
            %% 撤旧先于插入（重新生成=旧码失效）
            ?assert(meck:called(organization_invite_code_pg, revoke_active_by_org_tx, 2)),
            ?assert(meck:called(organization_invite_code_pg, add_tx, 6))
        end)
    end}.

create_role_test_() ->
    [
        {"create role=admin：码上透传 admin（发码方决定初始角色）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'add_tx', 6, fun(fake_conn, _OrgId, Code, _By, _Exp, Role) ->
                            put(t_code_role, Role),
                            {ok, (code_row())#{<<"code">> => Code, <<"role">> => Role}}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    try
                        {ok, View} =
                            organization_invite_code_app:create(?OWNER, ?ORG_ID, #{
                                role => <<"admin">>
                            }),
                        ?assertEqual(<<"admin">>, maps:get(role, View)),
                        ?assertEqual(<<"admin">>, erlang:erase(t_code_role))
                    after
                        erlang:erase(t_code_role)
                    end
                end
            )
        end},
        {"create 非法 role → 400（owner 不入枚举/垃圾值拒绝）", fun() ->
            with_mocks([], fun() ->
                set_role(?OWNER, <<"owner">>),
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:create(?OWNER, ?ORG_ID, #{role => <<"owner">>})
                ),
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:create(?OWNER, ?ORG_ID, #{role => <<"hacker">>})
                )
            end)
        end},
        {"create role 大小写/空白归一（' ADMIN ' → admin）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'add_tx', 6, fun(fake_conn, _OrgId, Code, _By, _Exp, Role) ->
                            put(t_code_role, Role),
                            {ok, (code_row())#{<<"code">> => Code, <<"role">> => Role}}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    try
                        {ok, _} =
                            organization_invite_code_app:create(?OWNER, ?ORG_ID, #{
                                role => <<" ADMIN ">>
                            }),
                        ?assertEqual(<<"admin">>, erlang:erase(t_code_role))
                    after
                        erlang:erase(t_code_role)
                    end
                end
            )
        end}
    ].

create_guards_test_() ->
    [
        {"archived 组织 409", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(fake_conn, _O, _C) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invite_code_app:create(?OWNER, ?ORG_ID, #{})
                    )
                end
            )
        end},
        {"组织不存在 404", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(fake_conn, _O, _C) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {404, _}},
                        organization_invite_code_app:create(?OWNER, ?ORG_ID, #{})
                    )
                end
            )
        end},
        {"普通 member 建码 403", fun() ->
            with_mocks([], fun() ->
                set_role(?MEMBER, <<"member">>),
                ?assertMatch(
                    {error, {403, _}},
                    organization_invite_code_app:create(?MEMBER, ?ORG_ID, #{})
                )
            end)
        end},
        {"非成员建码 403（不泄露组织存在性之外的差异）", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    organization_invite_code_app:create(?TARGET, ?ORG_ID, #{})
                )
            end)
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:create(?OWNER, 0, #{})
                )
            end)
        end}
    ].

create_code_conflict_retry_test_() ->
    [
        {"code 冲突：换码重试后成功（重试循环真实到达第二次）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'add_tx', 6, fun(fake_conn, _O, _Code, _B, _E, _R) ->
                            case get(t_conflict_once) of
                                undefined ->
                                    put(t_conflict_once, true),
                                    {error, code_conflict};
                                _ ->
                                    {ok, code_row()}
                            end
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    try
                        {ok, _View} =
                            organization_invite_code_app:create(?OWNER, ?ORG_ID, #{})
                    after
                        erlang:erase(t_conflict_once)
                    end
                end
            )
        end},
        {"code 冲突重试耗尽 → 500", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'add_tx', 6, fun(_C, _O, _Code, _B, _E, _R) ->
                            {error, code_conflict}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    ?assertMatch(
                        {error, {500, _}},
                        organization_invite_code_app:create(?OWNER, ?ORG_ID, #{})
                    )
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% revoke / get
%%--------------------------------------------------------------------

revoke_test_() ->
    [
        {"owner 撤销幂等（revoked N）", fun() ->
            with_mocks([], fun() ->
                set_role(?OWNER, <<"owner">>),
                {ok, #{revoked := 0}} =
                    organization_invite_code_app:revoke(?OWNER, ?ORG_ID)
            end)
        end},
        {"archived 组织允许撤销（收紧操作放行）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(fake_conn, _O, _C) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    ?assertMatch(
                        {ok, #{revoked := _}},
                        organization_invite_code_app:revoke(?OWNER, ?ORG_ID)
                    )
                end
            )
        end},
        {"非治理成员撤销 403", fun() ->
            with_mocks([], fun() ->
                set_role(?MEMBER, <<"member">>),
                ?assertMatch(
                    {error, {403, _}},
                    organization_invite_code_app:revoke(?MEMBER, ?ORG_ID)
                )
            end)
        end}
    ].

get_test_() ->
    [
        {"有 active 码返回视图", fun() ->
            with_mocks([], fun() ->
                set_role(?OWNER, <<"admin">>),
                {ok, View} = organization_invite_code_app:get(?OWNER, ?ORG_ID),
                ?assertEqual(<<"ABCD2345">>, maps:get(code, View))
            end)
        end},
        {"无 active 码 → not_found（handler 映射 code=null 成功信封）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_org_tx', 2, fun(fake_conn, _O) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    ?assertEqual(
                        {error, not_found},
                        organization_invite_code_app:get(?OWNER, ?ORG_ID)
                    )
                end
            )
        end},
        {"archived 组织 get 门 409（禁新读面裁决同 create）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(fake_conn, _O, _C) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]}
                ],
                fun() ->
                    set_role(?OWNER, <<"owner">>),
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invite_code_app:get(?OWNER, ?ORG_ID)
                    )
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% join_by_code（负例矩阵：981/982/980/409/幂等）
%%--------------------------------------------------------------------

join_by_code_test_() ->
    [
        {"有效码：同事务转 orchestrator（统一编排入口）", fun() ->
            with_mocks([], fun() ->
                {ok, joined, Summary} =
                    organization_invite_code_app:join_by_code(
                        ?TARGET, ?ORG_ID, <<"abcd2345">>
                    ),
                %% 输码大小写不敏感（normalize → uppercase）
                ?assert(
                    meck:called(
                        organization_invite_code_pg,
                        find_active_by_code_tx,
                        [fake_conn, ?ORG_ID, <<"ABCD2345">>]
                    )
                ),
                ?assertEqual(?ORG_ID, maps:get(organization_id, Summary)),
                OrchestratorCalled =
                    receive
                        {orchestrator_join, fake_conn, ?ORG_ID, ?TARGET} -> true
                    after 0 -> false
                    end,
                ?assert(OrchestratorCalled)
            end)
        end},
        {"码不存在 981", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_tx', 3, fun(fake_conn, _O, _C) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                        organization_invite_code_app:join_by_code(
                            ?TARGET, ?ORG_ID, <<"NOPE9999">>
                        )
                    )
                end
            )
        end},
        {"跨 Org 输码与码不存在同错误 981（不泄露存在性）", fun() ->
            %% pg 层 find 按同语句 org 作用域：跨 Org 命中不了行 → not_found。
            %% mock 该口径，断言两路径错误完全一致（code+文案）。
            CrossMocks = [
                {organization_invite_code_pg, [
                    {'find_active_by_code_tx', 3, fun(fake_conn, OrgId, _Code) when
                        OrgId =:= ?ORG_OTHER
                    ->
                        {error, not_found}
                    end}
                ]}
            ],
            with_mocks(CrossMocks, fun() ->
                SameOrg = organization_invite_code_app:join_by_code(
                    ?TARGET, ?ORG_OTHER, <<"ABCD2345">>
                ),
                NotFound = organization_invite_code_app:join_by_code(
                    ?TARGET, ?ORG_OTHER, <<"NOPE9999">>
                ),
                ?assertEqual(SameOrg, NotFound),
                ?assertMatch({error, {?ERR_WORKSPACE_INVITE_INVALID, _}}, SameOrg)
            end)
        end},
        {"已撤销码 981（status=revoked 命中不了 active 查找）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_tx', 3, fun(fake_conn, _O, _C) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                        organization_invite_code_app:join_by_code(
                            ?TARGET, ?ORG_ID, <<"ABCD2345">>
                        )
                    )
                end
            )
        end},
        {"过期码 982", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_tx', 3, fun(fake_conn, _O, _C) ->
                            {ok, (code_row())#{<<"expired">> => true}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_EXPIRED, _}},
                        organization_invite_code_app:join_by_code(
                            ?TARGET, ?ORG_ID, <<"ABCD2345">>
                        )
                    )
                end
            )
        end},
        {"非 binary / 空码统一 981", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:join_by_code(?TARGET, ?ORG_ID, <<>>)
                ),
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:join_by_code(?TARGET, ?ORG_ID, 12345)
                )
            end)
        end},
        {"编排拒绝 980（默认 WS archived）透传", fun() ->
            with_mocks(
                [
                    {organization_join_orchestrator, [
                        {'join_tx', 5, fun(_C, _O, _U, _B, _R) ->
                            throw({abort_tx, {?ERR_WORKSPACE_ARCHIVED, <<"工作区已归档"/utf8>>}})
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_ARCHIVED, _}},
                        organization_invite_code_app:join_by_code(
                            ?TARGET, ?ORG_ID, <<"ABCD2345">>
                        )
                    )
                end
            )
        end},
        {"重复加入幂等 unchanged 透传", fun() ->
            with_mocks(
                [
                    {organization_join_orchestrator, [
                        {'join_tx', 5, fun(_C, OrgId, _U, _B, _R) ->
                            {ok, unchanged, #{organization_id => OrgId}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {ok, unchanged, _},
                        organization_invite_code_app:join_by_code(
                            ?TARGET, ?ORG_ID, <<"ABCD2345">>
                        )
                    )
                end
            )
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:join_by_code(?TARGET, 0, <<"ABCD2345">>)
                )
            end)
        end}
    ].

%%--------------------------------------------------------------------
%% preview_by_code / join_by_code_only（GZAPP-J11 code-only 面：
%% 码全局唯一即凭据，无需 orgId——扫码 / 单码手输加入路径）
%%--------------------------------------------------------------------

preview_by_code_test_() ->
    [
        {"有效码：返回 organization_id + name（uppercase 归一后查全局）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, Cols) ->
                            ?assert(
                                binary:match(Cols, <<"name">>) =/= nomatch
                            ),
                            {ok, #{
                                <<"id">> => ?ORG_ID,
                                <<"name">> => <<"广州演示企业"/utf8>>,
                                <<"status">> => <<"active">>
                            }}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, View} = organization_invite_code_app:preview_by_code(
                        ?TARGET, <<"abcd2345">>
                    ),
                    ?assertEqual(?ORG_ID, maps:get(organization_id, View)),
                    ?assertEqual(<<"广州演示企业"/utf8>>, maps:get(name, View)),
                    ?assert(
                        meck:called(
                            organization_invite_code_pg,
                            find_active_by_code_global_tx,
                            [fake_conn, <<"ABCD2345">>]
                        )
                    )
                end
            )
        end},
        {"码不存在 981", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                        organization_invite_code_app:preview_by_code(?TARGET, <<"NOPE9999">>)
                    )
                end
            )
        end},
        {"已撤销码 981（active 查找命中不了 revoked 行）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                        organization_invite_code_app:preview_by_code(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"过期码 982", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {ok, (code_row())#{<<"expired">> => true}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_EXPIRED, _}},
                        organization_invite_code_app:preview_by_code(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"目标 org 已归档 409", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, _Cols) ->
                            {ok, #{
                                <<"id">> => ?ORG_ID,
                                <<"name">> => <<"x">>,
                                <<"status">> => <<"archived">>
                            }}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invite_code_app:preview_by_code(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"目标 org 不存在 981（不泄露组织存在性差异）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, _Cols) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    NotFound = organization_invite_code_app:preview_by_code(
                        ?TARGET, <<"ABCD2345">>
                    ),
                    ?assertMatch({error, {?ERR_WORKSPACE_INVITE_INVALID, _}}, NotFound)
                end
            )
        end},
        {"有效码 preview 返回码上 role（发码方决定初始角色）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {ok, (code_row())#{<<"role">> => <<"admin">>}}
                        end},
                        {'find_active_by_code_for_share_tx', 3, fun(_C, _O, _Code) ->
                            {ok, (code_row())#{<<"role">> => <<"admin">>}}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, View} = organization_invite_code_app:preview_by_code(
                        ?TARGET, <<"ABCD2345">>
                    ),
                    ?assertEqual(<<"admin">>, maps:get(role, View))
                end
            )
        end},
        {"目标 org pending / rejected 409（注册审核门，00000155）", fun() ->
            lists:foreach(
                fun(Status) ->
                    with_mocks(
                        [
                            {organization_member_repo, [
                                {'find_organization_for_share_tx', 3, fun(_C, _O, _Cols) ->
                                    {ok, #{
                                        <<"id">> => ?ORG_ID,
                                        <<"name">> => <<"X">>,
                                        <<"status">> => Status
                                    }}
                                end}
                            ]}
                        ],
                        fun() ->
                            ?assertMatch(
                                {error, {409, _}},
                                organization_invite_code_app:preview_by_code(
                                    ?TARGET, <<"ABCD2345">>
                                )
                            )
                        end
                    )
                end,
                [<<"pending">>, <<"rejected">>]
            )
        end},
        {"空码 / 非 binary 统一 981", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:preview_by_code(?TARGET, <<>>)
                ),
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:preview_by_code(?TARGET, 12345)
                )
            end)
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:preview_by_code(0, <<"ABCD2345">>)
                )
            end)
        end}
    ].

join_by_code_only_test_() ->
    [
        {"有效码：从码行取 organization_id 转统一编排", fun() ->
            with_mocks([], fun() ->
                {ok, joined, Summary} =
                    organization_invite_code_app:join_by_code_only(?TARGET, <<"abcd2345">>),
                ?assert(
                    meck:called(
                        organization_invite_code_pg,
                        find_active_by_code_global_tx,
                        [fake_conn, <<"ABCD2345">>]
                    )
                ),
                ?assertEqual(?ORG_ID, maps:get(organization_id, Summary)),
                OrchestratorCalled =
                    receive
                        {orchestrator_join, fake_conn, ?ORG_ID, ?TARGET} -> true
                    after 0 -> false
                    end,
                ?assert(OrchestratorCalled)
            end)
        end},
        {"码不存在 981（不与 preview 语义分歧）", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                        organization_invite_code_app:join_by_code_only(?TARGET, <<"NOPE9999">>)
                    )
                end
            )
        end},
        {"过期码 982", fun() ->
            with_mocks(
                [
                    {organization_invite_code_pg, [
                        {'find_active_by_code_global_tx', 2, fun(_C, _Code) ->
                            {ok, (code_row())#{<<"expired">> => true}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_INVITE_EXPIRED, _}},
                        organization_invite_code_app:join_by_code_only(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"空码 / 非 binary 统一 981", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:join_by_code_only(?TARGET, <<>>)
                ),
                ?assertMatch(
                    {error, {?ERR_WORKSPACE_INVITE_INVALID, _}},
                    organization_invite_code_app:join_by_code_only(?TARGET, [])
                )
            end)
        end},
        {"编排拒绝 980（默认 WS archived）透传", fun() ->
            with_mocks(
                [
                    {organization_join_orchestrator, [
                        {'join_tx', 5, fun(_C, _O, _U, _B, _R) ->
                            throw({abort_tx, {?ERR_WORKSPACE_ARCHIVED, <<"工作区已归档"/utf8>>}})
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_ARCHIVED, _}},
                        organization_invite_code_app:join_by_code_only(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"重复加入幂等 unchanged 透传", fun() ->
            with_mocks(
                [
                    {organization_join_orchestrator, [
                        {'join_tx', 5, fun(_C, OrgId, _U, _B, _R) ->
                            {ok, unchanged, #{organization_id => OrgId}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {ok, unchanged, _},
                        organization_invite_code_app:join_by_code_only(?TARGET, <<"ABCD2345">>)
                    )
                end
            )
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invite_code_app:join_by_code_only(0, <<"ABCD2345">>)
                )
            end)
        end}
    ].
