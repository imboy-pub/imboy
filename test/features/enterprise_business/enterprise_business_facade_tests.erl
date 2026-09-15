%%% @doc `enterprise_business_facade` 的 §5 入口形状与委派判定（EB-05 重开）。
%%%
%%% 本套件只覆盖**本卡新增的四个 §5 入口**（GET /contacts、GET /contacts/{id}、
%%% PATCH /contacts/{id}、POST /contacts/{id}/assignment）与既有的
%%% GET /business-identities 入口：
%%%   * 形状收敛：资源级必填键、类型、OrgId 形状不满足 ⇒ `{error,{invalid_argument,…}}`
%%%     且**不触库**；
%%%   * 委派：形状成立时原样透传 application 用例的结论（facade 不自行判定归属、
%%%     不做业务规则、不读库）。
%%%
%%% 隔离与命名同其它 EB-05 套件（随机 TSID 合成租户、`eb05-` 前缀）。
-module(enterprise_business_facade_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(FACADE, enterprise_business_facade).

facade_contact_entrypoints_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun entrypoints_exist_with_convergence_shape/0},
        {timeout, 60, fun contact_entrypoints_delegate_on_valid_shape/0},
        {timeout, 60, fun contact_entrypoints_reject_bad_shape_without_touching_db/0},
        {timeout, 60, fun identity_list_entrypoint_delegates/0},
        {timeout, 60, fun contact_entrypoints_are_tenant_scoped_through_delegation/0}
    ];
cases({error, Reason}) ->
    erlang:error({eb05_facade_suite_db_unavailable, Reason}).

%% 四个新增入口都必须存在，且与既有入口同形状：(OrgId, Params) / 2。
entrypoints_exist_with_convergence_shape() ->
    Exports = [
        {Name, Arity}
     || {Name, Arity} <- ?FACADE:module_info(exports), Name =/= module_info
    ],
    lists:foreach(
        fun(Fun) ->
            ?assert(lists:member({Fun, 2}, Exports))
        end,
        [get_contact, list_contacts, update_contact, assign_contact, list_identities]
    ).

contact_entrypoints_delegate_on_valid_shape() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        %% GET /contacts/{id}
        {ok, Row} = ?FACADE:get_contact(Org, #{workspace_id => Ws, contact_id => Contact}),
        ?assertEqual(Contact, maps:get(id, Row)),
        ?assertEqual(Org, maps:get(organization_id, Row)),
        %% GET /contacts
        {ok, Listed} = ?FACADE:list_contacts(Org, #{workspace_id => Ws}),
        ?assertEqual([Contact], [maps:get(id, R) || R <- Listed]),
        %% PATCH /contacts/{id}
        {ok, Updated} = ?FACADE:update_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            display_name => <<"eb05-facade-renamed">>
        }),
        ?assertEqual(<<"eb05-facade-renamed">>, maps:get(display_name, Updated)),
        %% POST /contacts/{id}/assignment
        {ok, Assigned} = ?FACADE:assign_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            business_identity_id => Sales,
            role => <<"primary">>
        }),
        ?assertEqual(active, maps:get(status, Assigned)),
        ?assertEqual(Contact, maps:get(contact_id, Assigned))
    after
        ?FIX:cleanup(Scope)
    end.

%% 形状不成立 ⇒ 在 facade 内即拒绝，application 不被调用（无副作用可言）。
contact_entrypoints_reject_bad_shape_without_touching_db() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        ?assertEqual(
            {error, {invalid_argument, get_contact}},
            ?FACADE:get_contact(Org, #{workspace_id => Ws})
        ),
        ?assertEqual(
            {error, {invalid_argument, update_contact}},
            ?FACADE:update_contact(Org, #{workspace_id => Ws})
        ),
        ?assertEqual(
            {error, {invalid_argument, assign_contact}},
            ?FACADE:assign_contact(Org, #{workspace_id => Ws, contact_id => 1})
        ),
        %% OrgId 形状不满足 ⇒ 三个入口一致拒绝
        lists:foreach(
            fun(Call) ->
                ?assertMatch({error, {invalid_argument, {organization_id, _}}}, Call)
            end,
            [
                ?FACADE:get_contact(nope, #{contact_id => 1}),
                ?FACADE:list_contacts(nope, #{}),
                ?FACADE:update_contact(nope, #{contact_id => 1}),
                ?FACADE:assign_contact(nope, #{contact_id => 1, business_identity_id => 2})
            ]
        ),
        %% Params 不是 map ⇒ 同样拒绝
        lists:foreach(
            fun(Call) -> ?assertMatch({error, {invalid_argument, _}}, Call) end,
            [
                ?FACADE:get_contact(Org, not_a_map),
                ?FACADE:list_contacts(Org, not_a_map),
                ?FACADE:update_contact(Org, not_a_map),
                ?FACADE:assign_contact(Org, not_a_map)
            ]
        ),
        %% 形状成立但业务非法 ⇒ **委派**后由 application 判定（证明形状门不是业务门）
        ?assertMatch(
            {error, {invalid_role, _}},
            ?FACADE:assign_contact(Org, #{
                workspace_id => Ws,
                contact_id => 1,
                business_identity_id => 2,
                role => <<"owner">>
            })
        ),
        ?assertEqual(
            {error, empty_patch},
            ?FACADE:update_contact(Org, #{workspace_id => Ws, contact_id => 1})
        )
    after
        ?FIX:cleanup(Scope)
    end.

identity_list_entrypoint_delegates() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        {ok, Rows} = ?FACADE:list_identities(Org, #{workspace_id => Ws}),
        ?assertEqual(
            lists:sort([Sales, Service]),
            lists:sort([maps:get(id, R) || R <- Rows])
        ),
        %% 键集参数原样透传（facade 不解释分页）
        {ok, Page} = ?FACADE:list_identities(Org, #{workspace_id => Ws, limit => 1}),
        ?assertEqual(1, length(Page))
    after
        ?FIX:cleanup(Scope)
    end.

contact_entrypoints_are_tenant_scoped_through_delegation() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        %% 跨 Org 经 facade 不改变结论：仍是 contact_not_found / 空列表
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            ?FACADE:get_contact(OtherOrg, #{workspace_id => OtherWs, contact_id => Contact})
        ),
        ?assertEqual({ok, []}, ?FACADE:list_contacts(OtherOrg, #{workspace_id => OtherWs})),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            ?FACADE:update_contact(OtherOrg, #{
                workspace_id => OtherWs, contact_id => Contact, display_name => <<"x">>
            })
        ),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            ?FACADE:assign_contact(OtherOrg, #{
                workspace_id => OtherWs, contact_id => Contact, business_identity_id => Sales
            })
        ),
        %% 本 Org 客户未被他 Org 调用的副作用影响
        {ok, Row} = ?FACADE:get_contact(Org, #{workspace_id => Ws, contact_id => Contact}),
        ?assertEqual(Contact, maps:get(id, Row))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).
