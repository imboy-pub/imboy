%%% @doc 离职交接**读取面**（offboarding read）的路由契约套件（纯静态 + 纯函数，无 DB）。
%%%
%%% 依据：closure 计划 §8（offboarding 需「创建/查询交接 case」）、EB-09-A01/A02/A04
%%% 的同口径契约、`docs/architecture/feature-slice-rules.md` 铁律 2/3。
%%%
%%% 为什么单独立一个模块：`eb_route_contract_tests.erl` 与 `eb_tenant_handler_tests.erl`
%%% 正被并行卡（W1-F6，密钥合同翻转）持有文件租约；本卡新增的读路由契约条目放在
%%% 这里，避免同文件写冲突。**合并时**需把本模块 `tenant_read_routes/0` /
%%% `platform_read_routes/0` 的 4 条条目追加进 `eb_route_contract_tests` 的冻结清单
%%% （该文件的双向审计：动作表 ⊆ 冻结清单、注册路由 ⊆ 冻结清单，新增动作不加清单必红
%%% ——这是该套件的**预期**红灯，已登记 residual-risks，不是本模块能消除的）。
%%%
%%% 覆盖：
%%%   * **A01**：4 条读路由在 `imboy_router:get_routes/0` 里**恰好注册一次**，
%%%     method/path/action/auth_context/feature/surface 与冻结动作表逐键一致；
%%%     `eb_auth_principal:principal_for_route/1` 真调用接受每条 metadata；
%%%   * **A04**：平台面读路由每条都显式带 `:org_id`（不存在「不带 Org 的全局列举」）；
%%%   * **只读不变量**：读动作的 facade 用例只有 `list_offboarding` / `offboarding_detail`，
%%%     不带 delivery_only / proxy_content 语义；
%%%   * **投影红线（§8）**：动作表读用例的参数表里没有任何 cipher / key / plaintext 键
%%%     ——读取面的入参白名单从表上就杜绝密钥材料。
-module(eb_offboarding_route_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).

%% ===================================================================
%% 本卡冻结的读路由清单（与 router 的字面登记互为审计面）
%% ===================================================================

tenant_read_routes() ->
    P = <<"/api/v1/enterprise/organizations/:org_id">>,
    [
        {<<P/binary, "/offboarding/cases">>, offboarding_list, [<<"GET">>]},
        {<<P/binary, "/offboarding/cases/:id">>, offboarding_detail, [<<"GET">>]}
    ].

platform_read_routes() ->
    P = <<"/api/adm/enterprise-business/organizations/:org_id">>,
    [
        {<<P/binary, "/offboarding/cases">>, p_offboarding_list, [<<"GET">>]},
        {<<P/binary, "/offboarding/cases/:id">>, p_offboarding_detail, [<<"GET">>]}
    ].

%% ===================================================================
%% A01：真路由表 ↔ 冻结读清单 ↔ 动作表 三方一致
%% ===================================================================

a01_read_routes_registered_exactly_once_test() ->
    lists:foreach(
        fun(Surface) ->
            Routes = ?S:enterprise_routes(Surface),
            lists:foreach(
                fun({Path, Action, _Methods}) ->
                    Matches = [
                        {P, Opts}
                     || {P, _H, Opts} <- Routes,
                        P =:= Path,
                        maps:get(action, Opts, undefined) =:= Action
                    ],
                    ?assertMatch([_], Matches)
                end,
                frozen(Surface)
            )
        end,
        [tenant, platform]
    ).

a01_read_route_metadata_matches_action_table_test() ->
    lists:foreach(
        fun(Surface) ->
            Routes = ?S:enterprise_routes(Surface),
            lists:foreach(
                fun({Path, Action, Methods}) ->
                    {Handler, Opts} = single(Routes, Path, Action),
                    ?assertEqual(handler_for(Surface), Handler, {Path, handler}),
                    {ok, Entry} = entry(Surface, Action),
                    Auth = maps:get(auth, Entry),
                    %% 路径逐字一致已由 single/4 的查找保证；这里核对方法与 metadata 键
                    ?assertEqual(Methods, entry_methods(Entry)),
                    ?assertEqual(
                        maps:get(auth_context, Auth),
                        maps:get(auth_context, Opts, undefined)
                    ),
                    [
                        ?assertEqual(V, maps:get(K, Opts, undefined), {Path, K})
                     || {K, V} <- maps:to_list(Auth),
                        K =/= auth_context
                    ],
                    ?assertEqual(
                        eb_enterprise_actions:feature(),
                        maps:get(feature, Opts, undefined)
                    ),
                    ?assertEqual(Surface, maps:get(surface, Opts, undefined)),
                    %% 面级装配键一个都不能少（缺一即生产 500 fail-closed）
                    lists:foreach(
                        fun(K) -> ?assert(maps:is_key(K, Opts), {Path, K}) end,
                        eb_enterprise_actions:surface_required()
                    )
                end,
                frozen(Surface)
            )
        end,
        [tenant, platform]
    ).

%% @doc A01 非真空：把一条读路由的 auth_context 改错，同口径审计必须报出违规。
a01_read_route_audit_is_not_vacuous_test() ->
    Routes = ?S:enterprise_routes(tenant),
    Mutated = lists:map(
        fun({Path, H, Opts}) ->
            case maps:get(action, Opts, undefined) of
                offboarding_list -> {Path, H, Opts#{auth_context => enterprise_member}};
                _ -> {Path, H, Opts}
            end
        end,
        Routes
    ),
    ?assertNotEqual([], read_auth_violations(Mutated)).

read_auth_violations(Routes) ->
    lists:append([
        [{auth_context_mismatch, Path}]
     || {Path, _H, Opts} <- Routes,
        maps:get(action, Opts, undefined) =:= offboarding_list,
        maps:get(auth_context, Opts, undefined) =/= enterprise_owner_admin
    ]).

%% @doc 每条读路由 metadata 都被 `eb_auth_principal` 接受且 principal 与登记一致
%% （真调用；租户面 → enterprise_owner_admin，平台面 → platform_admin）。
a01_principal_for_read_routes_test() ->
    lists:foreach(
        fun(Surface) ->
            ExpectedPrincipal =
                case Surface of
                    tenant -> enterprise_owner_admin;
                    platform -> platform_admin
                end,
            lists:foreach(
                fun({Path, Action, _Methods}) ->
                    Routes = ?S:enterprise_routes(Surface),
                    {_Handler, Opts} = single(Routes, Path, Action),
                    Metadata = maps:put(path, Path, Opts),
                    {ok, Requirement} = eb_auth_principal:principal_for_route(Metadata),
                    ?assertEqual(
                        ExpectedPrincipal, maps:get(principal, Requirement), {Path, Action}
                    )
                end,
                frozen(Surface)
            )
        end,
        [tenant, platform]
    ).

%% ===================================================================
%% A04：平台读路由显式租户条件
%% ===================================================================

a04_platform_read_routes_require_org_test() ->
    lists:foreach(
        fun({Path, _Action, _Methods}) ->
            ?assertNotEqual(nomatch, binary:match(Path, <<":org_id">>), Path)
        end,
        platform_read_routes()
    ),
    %% 反向：不存在「不带 Org」的 offboarding 读路径
    AllPaths = [P || {P, _H, _O} <- ?S:enterprise_routes(platform)],
    OffPaths = [P || P <- AllPaths, binary:match(P, <<"offboarding">>) =/= nomatch],
    lists:foreach(
        fun(P) -> ?assertNotEqual(nomatch, binary:match(P, <<":org_id">>), P) end,
        OffPaths
    ).

%% ===================================================================
%% 只读不变量 + 参数红线
%% ===================================================================

read_only_invariants_hold_test() ->
    lists:foreach(
        fun(Surface) ->
            lists:foreach(
                fun({_Path, Action, _Methods}) ->
                    {ok, Entry} = entry(Surface, Action),
                    ?assertNot(maps:get(delivery_only, Entry, false), Action),
                    ?assertNot(maps:get(proxy_content, Entry, false), Action),
                    %% 读用例的 facade 函数只能是本卡登记的两个只读用例
                    lists:foreach(
                        fun(Case) ->
                            ?assert(
                                lists:member(
                                    maps:get(facade, Case),
                                    [list_offboarding, offboarding_detail]
                                ),
                                Action
                            )
                        end,
                        maps:get(cases, Entry)
                    ),
                    %% 参数红线：入参白名单里不得出现密钥材料/密文/明文键（§8 投影红线）
                    lists:foreach(
                        fun(Case) ->
                            lists:foreach(
                                fun({K, _T, _R}) ->
                                    ?assertNot(
                                        lists:member(
                                            K,
                                            [
                                                profile_cipher,
                                                profile_key_version,
                                                body_cipher,
                                                body_key_version,
                                                key_material,
                                                profile_plaintext,
                                                body_plaintext
                                            ]
                                        ),
                                        {Action, K}
                                    )
                                end,
                                maps:get(params, Case)
                            )
                        end,
                        maps:get(cases, Entry)
                    )
                end,
                frozen(Surface)
            )
        end,
        [tenant, platform]
    ).

%% @doc 读动作的分页参数与既有先例同形（`after_id` 是 TSID、`limit` 是 int、均 optional），
%% 详情的失败项过滤参数 `items_status` 是 optional binary —— 契约化，防止后续漂移。
pagination_and_filter_contract_test() ->
    {ok, ListEntry} = eb_enterprise_actions:tenant(offboarding_list),
    [ListCase] = maps:get(cases, ListEntry),
    ?assertEqual(
        [{after_id, tsid, optional}, {limit, int, optional}, {status, binary, optional}],
        lists:sort(fun({A, _, _}, {B, _, _}) -> A < B end, maps:get(params, ListCase))
    ),
    {ok, DetailEntry} = eb_enterprise_actions:tenant(offboarding_detail),
    [DetailCase] = maps:get(cases, DetailEntry),
    ?assertEqual([{items_status, binary, optional}], maps:get(params, DetailCase)),
    ?assertEqual([{id, case_id}], maps:get(path_params, DetailCase)).

%% ===================================================================
%% 内部辅助
%% ===================================================================

frozen(tenant) -> tenant_read_routes();
frozen(platform) -> platform_read_routes().

entry(tenant, Action) -> eb_enterprise_actions:tenant(Action);
entry(platform, Action) -> eb_enterprise_actions:platform(Action).

handler_for(tenant) -> eb_tenant_handler;
handler_for(platform) -> eb_platform_handler.

entry_methods(Entry) ->
    [maps:get(method, Case) || Case <- maps:get(cases, Entry)].

single(Routes, Path, Action) ->
    Matches = [
        {H, Opts}
     || {P, H, Opts} <- Routes,
        P =:= Path,
        maps:get(action, Opts, undefined) =:= Action
    ],
    case Matches of
        [One] -> One;
        [] -> erlang:error({read_route_not_registered, Path, Action});
        _ -> erlang:error({read_route_duplicated, Path, Action})
    end.
