%%% @doc C5（closure run 冻结合同）：identity 列表的 active_assignment 投影与
%%% 键集分页 SQL 下推的 application 套件（真库）。
%%%
%%% 覆盖：
%%%   * **投影**：每个 identity 附 `active_assignment`（对象或 null），对象键集
%%%     **恰好**等于既有 assignment 白名单七键（勿扩），且值与库中 active 行一致；
%%%   * **null 分支**：无 active 经办的 identity 投影为 null（不是缺键、不是
%%%     undefined、不是空对象）；
%%%   * **rebind 切换**：end + 重绑后，投影跟随库里的 active 行切换；
%%%   * **分页边界**：空页 / 单页 / 翻页（next_after_id 游标续读）/ limit 越界、
%%%     after_id 非法（结构化错误 → HTTP 面 422）；
%%%   * **契约四方核对**：`list_identities_page/3` 在 behaviour / contracts() /
%%%     实现导出 / application 调用点四处同在；触碰条目（business_identities /
%%%     p_identities）的 actions 表显式登记 `workspace_id required`（F-LAY-10）。
%%%
%%% 隔离与失败语义同 `eb_identity_app_tests`：随机 TSID 合成租户，不 TRUNCATE；
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）。
-module(eb_identity_projection_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

%% C5 冻结的 active_assignment 白名单（= 既有 assignment_fields() 去掉
%% organization_id / ended_at；勿扩）。
-define(ASSIGNMENT_KEYS, [
    assignment_id,
    business_identity_id,
    user_id,
    function_key,
    status,
    assigned_at,
    version
]).

identity_projection_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun c5_active_assignment_projection_exact_keys/0},
        {timeout, 60, fun c5_null_branch_without_active_assignment/0},
        {timeout, 60, fun c5_rebind_switches_projection/0},
        {timeout, 60, fun c5_pagination_empty_single_and_next_page/0},
        {timeout, 60, fun c5_invalid_limit_and_after_id_are_structural_errors/0},
        {timeout, 30, fun c5_http_maps_pagination_errors_to_422/0},
        {timeout, 30, fun c5_port_contract_is_in_sync_on_four_sides/0},
        {timeout, 30, fun c5_actions_table_registers_workspace_id_required/0}
    ];
cases({error, Reason}) ->
    erlang:error({c5_identity_projection_suite_db_unavailable, Reason}).

%% ===================================================================
%% 投影：键集精确 + 值正确
%% ===================================================================

c5_active_assignment_projection_exact_keys() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Assignment = maps:get(assignment_id, Scope),
        {ok, #{business_identities := Rows}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
        SalesRow = maps:get(Sales, ById),
        AA = maps:get(active_assignment, SalesRow),
        %% ① 键集**精确**相等：七键一个不多、一个不少（勿扩的机械判据）
        ?assertMatch(AA when is_map(AA), AA),
        ?assertEqual(lists:sort(?ASSIGNMENT_KEYS), lists:sort(maps:keys(AA))),
        %% ② identity 行上不得残留扁平的 JOIN 列（aa_* / 裸 assignment 键）
        Leaked = lists:filter(
            fun(K) ->
                is_atom(K) andalso
                    (K =:= assignment_id orelse K =:= assigned_at orelse
                        K =:= ended_at orelse lists:prefix("aa_", atom_to_list(K)))
            end,
            maps:keys(SalesRow)
        ),
        ?assertEqual([], Leaked),
        %% ③ 值与库中 active 行一致
        ?assertEqual(Assignment, maps:get(assignment_id, AA)),
        ?assertEqual(Sales, maps:get(business_identity_id, AA)),
        ?assertEqual(Actor, maps:get(user_id, AA)),
        ?assertEqual(<<"sales">>, maps:get(function_key, AA)),
        ?assertEqual(active, maps:get(status, AA)),
        ?assert(is_integer(maps:get(assigned_at, AA))),
        ?assert(maps:get(assigned_at, AA) > 0),
        ?assertEqual(1, maps:get(version, AA))
    after
        ?FIX:cleanup(Scope)
    end.

%% null 分支：fixture 里 service identity 从未绑定 ⇒ active_assignment = null
%%（不是 undefined——出口处必须投影成 JSON null 可编码的原子）。
c5_null_branch_without_active_assignment() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Service = maps:get(service_identity_id, Scope),
        {ok, #{business_identities := Rows}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
        ServiceRow = maps:get(Service, ById),
        ?assertEqual(null, maps:get(active_assignment, ServiceRow, missing)),
        %% 反向非真空：同一页里带 active 经办的行**不是** null（判据是活的）
        Sales = maps:get(sales_identity_id, Scope),
        SalesRow = maps:get(Sales, ById),
        ?assert(maps:get(active_assignment, SalesRow) =/= null)
    after
        ?FIX:cleanup(Scope)
    end.

%% rebind 后投影切换：end 掉 Sales 的 active、把 Actor 绑到 Service ——
%% 列表投影必须同步：Sales → null，Service → 新 active 行。
c5_rebind_switches_projection() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        {ok, _} = eb_identity_app:end_assignment(Org, #{
            workspace_id => Ws,
            identity_id => Sales,
            user_id => Actor,
            end_reason => <<"c5-projection-rebind">>
        }),
        {ok, _} = eb_identity_app:bind_assignment(Org, #{
            workspace_id => Ws, identity_id => Service, user_id => Actor
        }),
        {ok, #{business_identities := Rows}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
        ?assertEqual(null, maps:get(active_assignment, maps:get(Sales, ById))),
        Switched = maps:get(active_assignment, maps:get(Service, ById)),
        ?assertMatch(Switched when is_map(Switched), Switched),
        ?assertEqual(Service, maps:get(business_identity_id, Switched)),
        ?assertEqual(<<"customer_service">>, maps:get(function_key, Switched)),
        ?assertEqual(active, maps:get(status, Switched)),
        ?assertEqual(1, maps:get(version, Switched)),
        lists:foreach(
            fun(Row) ->
                ?assertEqual(
                    lists:sort(?ASSIGNMENT_KEYS),
                    lists:sort(maps:keys(maps:get(active_assignment, Row)))
                )
            end,
            [maps:get(Service, ById)]
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 分页边界：空 / 单页 / 翻页 / 非法参数
%% ===================================================================

c5_pagination_empty_single_and_next_page() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        %% 空页：他 Org（配自己的 Workspace）⇒ 空页 + null 游标
        ?assertEqual(
            {ok, #{business_identities => [], next_after_id => null}},
            eb_identity_app:list_identities(maps:get(other_org_id, Scope), #{
                workspace_id => maps:get(other_workspace_id, Scope)
            })
        ),
        %% 造到 4 行：fixture 2 + 追加 2，limit=2 ⇒ 真两页满页
        lists:foreach(
            fun(N) ->
                {ok, _} = eb_identity_app:create_identity(Org, #{
                    workspace_id => Ws,
                    function_key => <<"customer_service">>,
                    display_name => <<"c5-page-", (integer_to_binary(N))/binary>>
                })
            end,
            lists:seq(1, 2)
        ),
        {ok, #{business_identities := All, next_after_id := FullNext}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        ?assertEqual(4, length(All)),
        ?assertEqual(null, FullNext),
        %% 第 1 页（满页）⇒ next_after_id = 本页最后一行 id；用它续读恰好取到余下行
        {ok, #{business_identities := Page1, next_after_id := Cursor}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, limit => 2}),
        Page1Ids = [maps:get(id, R) || R <- Page1],
        ?assertEqual(2, length(Page1Ids)),
        ?assertEqual(lists:last(Page1Ids), Cursor),
        {ok, #{business_identities := Page2, next_after_id := Cursor2}} =
            eb_identity_app:list_identities(Org, #{
                workspace_id => Ws, after_id => Cursor, limit => 2
            }),
        Page2Ids = [maps:get(id, R) || R <- Page2],
        Expected = [Id || Id <- [maps:get(id, R) || R <- All], Id < Cursor],
        ?assertEqual(Expected, Page2Ids),
        %% 第 2 页也是满页 ⇒ 游标 = 本页最后一行 id
        ?assertEqual(lists:last(Page2Ids), Cursor2),
        %% 尾页之后：空页 + null 游标
        {ok, #{business_identities := Tail, next_after_id := TailNext}} =
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, after_id => Cursor2}),
        ?assertEqual([], Tail),
        ?assertEqual(null, TailNext),
        %% 两页拼接 = 全量（无重无漏）
        ?assertEqual([maps:get(id, R) || R <- All], Page1Ids ++ Page2Ids)
    after
        ?FIX:cleanup(Scope)
    end.

%% limit 越界（0 / 201）/ after_id 非 TSID（0 / 负数 / 非整数）⇒ 结构化错误，
%% 不静默钳制、不 500。
c5_invalid_limit_and_after_id_are_structural_errors() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        ?assertMatch(
            {error, {invalid_limit, 0}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, limit => 0})
        ),
        ?assertMatch(
            {error, {invalid_limit, 201}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, limit => 201})
        ),
        ?assertMatch(
            {error, {invalid_limit, <<"50">>}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, limit => <<"50">>})
        ),
        ?assertMatch(
            {error, {invalid_after_id, 0}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, after_id => 0})
        ),
        ?assertMatch(
            {error, {invalid_after_id, -5}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, after_id => -5})
        ),
        ?assertMatch(
            {error, {invalid_after_id, <<"abc">>}},
            eb_identity_app:list_identities(Org, #{workspace_id => Ws, after_id => <<"abc">>})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% HTTP 面显式映射（无兜底）：两个新错误原子都必须是 422。
c5_http_maps_pagination_errors_to_422() ->
    ?assertEqual(422, eb_enterprise_http:status({invalid_limit, 0})),
    ?assertEqual(422, eb_enterprise_http:status({invalid_limit, 201})),
    ?assertEqual(422, eb_enterprise_http:status({invalid_after_id, <<"abc">>})),
    %% 稳定标签只含原子路径，不含取值
    ?assertEqual(<<"invalid_limit">>, eb_enterprise_http:tag({invalid_limit, 999})),
    ?assertEqual(<<"invalid_after_id">>, eb_enterprise_http:tag({invalid_after_id, 0})).

%% ===================================================================
%% 契约核对（四方 + actions 表登记）
%% ===================================================================

%% `list_identities_page/3` 四方同 PR：behaviour 声明 ↔ registry contracts ↔
%% store 实现导出 ↔ application 调用点。缺任一方即红。
c5_port_contract_is_in_sync_on_four_sides() ->
    %% (1) behaviour 声明
    ?assert(
        lists:member({list_identities_page, 3}, eb_store_port:behaviour_info(callbacks))
    ),
    %% (2) registry contracts
    ?assert(
        lists:member(
            {list_identities_page, 3}, maps:get(eb_store_port, eb_ports:contracts())
        )
    ),
    %% (3) 实现导出（装配实现 eb_pg_store）
    ?assert(
        lists:member(
            {list_identities_page, 3},
            [E || {N, _} = E <- eb_pg_store:module_info(exports), N =:= list_identities_page]
        )
    ),
    %% (4) application 调用点（源码级，避免「声明了没人调」的假绿）
    {ok, Src} = file:read_file(
        "src/features/enterprise_business/application/identity/eb_identity_app.erl"
    ),
    ?assertNotEqual(
        nomatch, binary:match(Src, <<"list_identities_page(OrgId, WorkspaceId, Query)">>)
    ),
    %% 负向对照：形状不在契约里必须判假
    ?assertNot(
        lists:member({list_identities_page, 2}, eb_store_port:behaviour_info(callbacks))
    ).

%% F-LAY-10 消缺：触碰条目（tenant business_identities / platform p_identities）
%% 的**每个 case**都显式登记 `{workspace_id, tsid, required}`。
c5_actions_table_registers_workspace_id_required() ->
    lists:foreach(
        fun({Owner, Action}) ->
            {ok, Entry} = eb_enterprise_actions:Owner(Action),
            lists:foreach(
                fun(Case) ->
                    ?assert(
                        lists:member(
                            {workspace_id, tsid, required}, maps:get(params, Case)
                        )
                    )
                end,
                maps:get(cases, Entry)
            )
        end,
        [{tenant, business_identities}, {platform, p_identities}]
    ),
    %% 非真空：未触碰的条目（contacts）GET case 不必登记（判据不是恒真）
    {ok, Contacts} = eb_enterprise_actions:tenant(contacts),
    {ok, ContactsGet} = eb_enterprise_actions:case_for(Contacts, <<"GET">>),
    ?assertNot(
        lists:member({workspace_id, tsid, required}, maps:get(params, ContactsGet))
    ).

%% ===================================================================
%% 内部辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).
