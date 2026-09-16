%%% @doc EB-03 业务身份 / 经办关系的 PostgreSQL store 套件。
%%%
%%% 覆盖：
%%%   EB-03-A01 资源级 store 的前两个业务参数是 OrgId/WorkspaceId，且**每条 SQL
%%%             同语句带两者**（用跨 Org / 跨 Workspace 负例 + 全部语句静态断言证明，
%%%             不是「分两次查询再拼」）；
%%%   EB-03-A02 assignment CAS 的真并发只有一个成功（多进程抢同一行，不是顺序调用），
%%%             且 domain 层判定的非法迁移一律不落库。
-module(eb_identity_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% 夹具
%% ===================================================================

identity_store_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            ok = eb_pg_test_fixture:ensure_purge_role(),
            {ok, Conn};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_insert_and_fetch_identity_within_scope/0},
        {timeout, 60, fun a01_insert_identity_rejects_workspace_outside_org/0},
        {timeout, 60, fun a01_fetch_identity_is_scoped_by_both_tenants/0},
        {timeout, 60, fun a01_every_callback_fails_closed_without_both_tenants/0},
        {timeout, 60, fun a01_every_statement_carries_both_tenants/0},
        {timeout, 60, fun a01_list_assignments_fails_closed_outside_scope/0},
        {timeout, 60, fun a02_cas_active_to_ended_succeeds_once/0},
        {timeout, 60, fun a02_cas_second_attempt_conflicts/0},
        {timeout, 60, fun a02_illegal_transitions_are_rejected_without_writing/0},
        {timeout, 120, fun a02_concurrent_cas_has_exactly_one_winner/0},
        {timeout, 60, fun c5_page_joins_active_assignment/0},
        {timeout, 60, fun c5_page_projection_follows_rebind/0},
        {timeout, 60, fun c5_page_desc_keyset_cursor/0},
        {timeout, 60, fun c5_page_rejects_bad_limit_and_after_id/0}
    ];
cases(_Skipped) ->
    {skip, "identity store suite requires the scratch database connection"}.

%% ===================================================================
%% EB-03-A01：租户贯穿
%% ===================================================================

a01_insert_and_fetch_identity_within_scope() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        NewId = eb_pg_test_fixture:id(),
        Identity = #{
            id => NewId,
            function_key => <<"sales">>,
            display_name => <<"eb03-store-sales-", (integer_to_binary(NewId))/binary>>,
            created_by_user_id => Owner
        },
        {ok, Inserted} = eb_pg_store:insert_identity(Org, Ws, Identity),
        ?assertEqual(Org, maps:get(organization_id, Inserted)),
        ?assertEqual(Ws, maps:get(workspace_id, Inserted)),
        ?assertEqual(<<"sales">>, maps:get(function_key, Inserted)),
        ?assertEqual(active, maps:get(status, Inserted)),
        ?assertEqual(1, maps:get(version, Inserted)),
        %% 同一条 INSERT 在已存在时返回 conflict（不静默改写既有行）
        ?assertEqual({error, conflict}, eb_pg_store:insert_identity(Org, Ws, Identity)),
        ?assertEqual({ok, Inserted}, eb_pg_store:fetch_identity(Org, Ws, NewId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_insert_identity_rejects_workspace_outside_org() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Identity = fun() ->
            #{
                id => eb_pg_test_fixture:id(),
                function_key => <<"sales">>,
                display_name => <<"eb03-x-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>
            }
        end,
        %% Workspace 属于另一个 Org：同一语句里的 workspace 归属校验必须拒绝写入
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_pg_store:insert_identity(Org, OtherWs, Identity())
        ),
        %% Org 与 Workspace 互换同样拒绝
        ?assertEqual(
            {error, {workspace_not_in_org, Ws}},
            eb_pg_store:insert_identity(OtherOrg, Ws, Identity())
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_fetch_identity_is_scoped_by_both_tenants() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        %% 正确租户 → 命中
        ?assertMatch({ok, _}, eb_pg_store:fetch_identity(Org, Ws, Sales)),
        %% 只对一半租户：跨 Org → not_found；跨 Workspace → not_found
        ?assertEqual({error, not_found}, eb_pg_store:fetch_identity(OtherOrg, Ws, Sales)),
        ?assertEqual({error, not_found}, eb_pg_store:fetch_identity(Org, OtherWs, Sales)),
        ?assertEqual({error, not_found}, eb_pg_store:fetch_identity(OtherOrg, OtherWs, Sales))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_every_callback_fails_closed_without_both_tenants() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Cases = [
            {fetch_identity, [undefined, Ws, Sales]},
            {fetch_identity, [Org, undefined, Sales]},
            {fetch_identity, ["1", Ws, Sales]},
            {insert_identity, [
                undefined,
                Ws,
                #{
                    id => eb_pg_test_fixture:id(),
                    function_key => <<"sales">>,
                    display_name => <<"nope">>
                }
            ]},
            {insert_identity, [
                Org,
                undefined,
                #{
                    id => eb_pg_test_fixture:id(),
                    function_key => <<"sales">>,
                    display_name => <<"nope">>
                }
            ]},
            {insert_conversation, [undefined, Ws, #{}]},
            {insert_conversation, [Org, undefined, #{}]},
            {append_message, [undefined, Ws, #{}]},
            {append_message, [Org, undefined, #{}]},
            {fetch_conversation, [undefined, Ws, Conv]},
            {fetch_conversation, [Org, undefined, Conv]},
            {advance_assignment, [undefined, Ws, Sales, active, ended]},
            {advance_assignment, [Org, undefined, Sales, active, ended]},
            {list_assignments, [undefined, Ws]},
            {list_assignments, [Org, undefined]}
        ],
        lists:foreach(
            fun({Function, Args}) ->
                Result = apply(eb_pg_store, Function, Args),
                ?assertMatch({error, {invalid_tenant, _}}, Result)
            end,
            Cases
        ),
        %% 只带 Org 的调用不得触库改行：assignment 仍是 active
        {ok, Assignments} = eb_pg_store:list_assignments(Org, Ws),
        ?assertEqual([active], [maps:get(status, A) || A <- Assignments])
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_every_statement_carries_both_tenants() ->
    Statements = eb_pg_store:sql_statements(),
    ?assert(length(Statements) >= 12),
    lists:foreach(
        fun(Sql) ->
            ?assert(binary:match(Sql, <<"organization_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"workspace_id">>) =/= nomatch)
        end,
        Statements
    ),
    %% 每条语句都必须显式带占位参数（$1=OrgId, $2=WorkspaceId），不允许「先查后拼」
    lists:foreach(
        fun(Sql) ->
            ?assert(binary:match(Sql, <<"$1">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"$2">>) =/= nomatch)
        end,
        Statements
    ).

a01_list_assignments_fails_closed_outside_scope() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        {ok, Assignments} = eb_pg_store:list_assignments(Org, Ws),
        ?assert(length(Assignments) >= 1),
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_pg_store:list_assignments(Org, OtherWs)
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A02：CAS
%% ===================================================================

a02_cas_active_to_ended_succeeds_once() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        ?assertEqual(ok, eb_pg_store:advance_assignment(Org, Ws, Sales, active, ended)),
        {ok, [Assignment]} = eb_pg_store:list_assignments(Org, Ws),
        ?assertEqual(ended, maps:get(status, Assignment)),
        ?assert(is_integer(maps:get(ended_at, Assignment))),
        ?assertEqual(2, maps:get(version, Assignment)),
        %% 与 domain 的同一套不变量判据对齐（domain 是不变量语义的唯一真源）
        ?assertEqual(ok, eb_identity:assignment_invariants([Assignment]))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a02_cas_second_attempt_conflicts() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        ?assertEqual(ok, eb_pg_store:advance_assignment(Org, Ws, Sales, active, ended)),
        ?assertEqual(
            {error, conflict},
            eb_pg_store:advance_assignment(Org, Ws, Sales, active, ended)
        ),
        %% 不存在的 identity 也是 conflict（CAS 未命中，绝不当成功）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:advance_assignment(Org, Ws, eb_pg_test_fixture:id(), active, ended)
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a02_illegal_transitions_are_rejected_without_writing() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        %% domain 判定：active->active 是重复占用；已 ended 不得自由复活
        ?assertEqual(
            {error, duplicate_occupation},
            eb_pg_store:advance_assignment(Org, Ws, Sales, active, active)
        ),
        ?assertEqual(
            {error, reopen_required},
            eb_pg_store:advance_assignment(Org, Ws, Sales, ended, active)
        ),
        ?assertMatch(
            {error, _},
            eb_pg_store:advance_assignment(Org, Ws, Sales, retired, ended)
        ),
        %% 全部被拒后行必须逐字不变
        {ok, [Assignment]} = eb_pg_store:list_assignments(Org, Ws),
        ?assertEqual(active, maps:get(status, Assignment)),
        ?assertEqual(1, maps:get(version, Assignment)),
        ?assertEqual(undefined, maps:get(ended_at, Assignment))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 真并发：8 个进程用 8 条连接抢同一行的 active->ended。
%% 断言：恰好 1 个 ok、7 个 {error, conflict}，且库里恰好 1 条 ended（version 只推进一次）。
a02_concurrent_cas_has_exactly_one_winner() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Self = self(),
        Workers = 8,
        [
            spawn_link(fun() ->
                Self ! {cas_result, eb_pg_store:advance_assignment(Org, Ws, Sales, active, ended)}
            end)
         || _ <- lists:seq(1, Workers)
        ],
        Results = [
            receive
                {cas_result, R} -> R
            after 30000 -> timeout
            end
         || _ <- lists:seq(1, Workers)
        ],
        ?assertEqual(1, length([ok || ok <- Results])),
        ?assertEqual(Workers - 1, length([conflict || {error, conflict} <- Results])),
        {ok, [Assignment]} = eb_pg_store:list_assignments(Org, Ws),
        ?assertEqual(ended, maps:get(status, Assignment)),
        ?assertEqual(2, maps:get(version, Assignment)),
        ?assertEqual(
            1,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM organization_business_identity_assignment"
                    " WHERE organization_id=$1 AND business_identity_id=$2 AND status='ended'"
                >>,
                [Org, Sales]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% C5：键集分页下推 + active assignment JOIN 投影（store/ext 级）
%% ===================================================================

%% JOIN 正确性：/3 每行附 active_assignment，且与库中 active assignment 行逐键
%% 一致；无 active 的行投影为 undefined（store 的 NULL 约定，HTTP 面再转 null）。
c5_page_joins_active_assignment() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Assignment = maps:get(assignment_id, Scope),
        {ok, Rows} = eb_pg_store:list_identities_page(Org, Ws, #{}),
        ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
        ?assertEqual(lists:sort([Sales, Service]), lists:sort(maps:keys(ById))),
        SalesRow = maps:get(Sales, ById),
        AA = maps:get(active_assignment, SalesRow),
        ?assertMatch(AA when is_map(AA), AA),
        ?assertEqual(Assignment, maps:get(assignment_id, AA)),
        ?assertEqual(Sales, maps:get(business_identity_id, AA)),
        ?assertEqual(Actor, maps:get(user_id, AA)),
        ?assertEqual(<<"sales">>, maps:get(function_key, AA)),
        ?assertEqual(active, maps:get(status, AA)),
        ?assert(is_integer(maps:get(assigned_at, AA))),
        ?assertEqual(1, maps:get(version, AA)),
        %% identity 自身字段不被 JOIN 列覆盖（别名隔离的判据）
        ?assertEqual(<<"sales">>, maps:get(function_key, SalesRow)),
        ?assertEqual(active, maps:get(status, SalesRow)),
        ?assertEqual(1, maps:get(version, SalesRow)),
        %% 无 active 经办的行 ⇒ undefined（不残留扁平 aa_* 键）
        ServiceRow = maps:get(Service, ById),
        ?assertEqual(undefined, maps:get(active_assignment, ServiceRow, present)),
        ?assertEqual(
            [],
            lists:filter(
                fun(K) -> is_atom(K) andalso lists:prefix("aa_", atom_to_list(K)) end,
                maps:keys(ServiceRow)
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% rebind 后投影切换（pg 级）：CAS active→ended 后旧行消失；
%% insert_assignment 新建行后投影跟随切换。
c5_page_projection_follows_rebind() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        %% end 掉 fixture 的 active（CAS），Sales 的投影应消失
        ok = eb_pg_store:advance_assignment(Org, Ws, Sales, active, ended),
        {ok, Rows1} = eb_pg_store:list_identities_page(Org, Ws, #{}),
        ById1 = maps:from_list([{maps:get(id, R), R} || R <- Rows1]),
        ?assertEqual(undefined, maps:get(active_assignment, maps:get(Sales, ById1))),
        %% 新建 active（首次绑定 Service）⇒ 投影切到 Service
        NewAssignment = eb_pg_test_fixture:id(),
        {ok, _} = eb_pg_store:insert_assignment(Org, Ws, #{
            id => NewAssignment,
            business_identity_id => Service,
            function_key => <<"customer_service">>,
            user_id => Owner,
            assigned_by => Owner
        }),
        {ok, Rows2} = eb_pg_store:list_identities_page(Org, Ws, #{}),
        ById2 = maps:from_list([{maps:get(id, R), R} || R <- Rows2]),
        AA = maps:get(active_assignment, maps:get(Service, ById2)),
        ?assertMatch(AA when is_map(AA), AA),
        ?assertEqual(NewAssignment, maps:get(assignment_id, AA)),
        ?assertEqual(Owner, maps:get(user_id, AA))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 倒序键集游标：默认首页 = 最大 id 起、DESC；`after_id` 严格 `id < 游标`；
%% LIMIT 是绑定参数（满页截断生效），游标续页无重无漏。
c5_page_desc_keyset_cursor() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% 追加 3 行 ⇒ 共 5 行
        lists:foreach(
            fun(N) ->
                {ok, _} = eb_pg_store:insert_identity(Org, Ws, #{
                    id => eb_pg_test_fixture:id(),
                    function_key => <<"customer_service">>,
                    display_name => <<"eb03-c5-page-", (integer_to_binary(N))/binary>>
                })
            end,
            lists:seq(1, 3)
        ),
        {ok, All} = eb_pg_store:list_identities_page(Org, Ws, #{}),
        AllIds = [maps:get(id, R) || R <- All],
        ?assertEqual(5, length(AllIds)),
        ?assertEqual(lists:reverse(lists:sort(AllIds)), AllIds),
        {ok, Page1} = eb_pg_store:list_identities_page(Org, Ws, #{limit => 2}),
        Page1Ids = [maps:get(id, R) || R <- Page1],
        ?assertEqual(lists:sublist(AllIds, 2), Page1Ids),
        {ok, Page2} =
            eb_pg_store:list_identities_page(Org, Ws, #{
                limit => 2, after_id => lists:last(Page1Ids)
            }),
        Page2Ids = [maps:get(id, R) || R <- Page2],
        ?assertEqual(lists:sublist(AllIds, 3, 2), Page2Ids),
        {ok, Page3} = eb_pg_store:list_identities_page(Org, Ws, #{
            limit => 2, after_id => lists:last(Page2Ids)
        }),
        ?assertEqual([lists:last(AllIds)], [maps:get(id, R) || R <- Page3]),
        %% 越过游标末端 ⇒ 空页
        {ok, []} = eb_pg_store:list_identities_page(Org, Ws, #{
            limit => 2, after_id => lists:last(AllIds)
        })
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 参数门（store 层防御）：limit 越界 / after_id 非法一律结构化错误，不触库。
c5_page_rejects_bad_limit_and_after_id() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?assertEqual(
            {error, {invalid_limit, 0}},
            eb_pg_store:list_identities_page(Org, Ws, #{limit => 0})
        ),
        ?assertEqual(
            {error, {invalid_limit, 201}},
            eb_pg_store:list_identities_page(Org, Ws, #{limit => 201})
        ),
        ?assertEqual(
            {error, {invalid_after_id, 0}},
            eb_pg_store:list_identities_page(Org, Ws, #{after_id => 0})
        ),
        ?assertEqual(
            {error, {invalid_after_id, -1}},
            eb_pg_store:list_identities_page(Org, Ws, #{after_id => -1})
        ),
        %% 非 map Query 同样拒绝（不猜测）
        ?assertEqual({error, invalid_query}, eb_pg_store:list_identities_page(Org, Ws, nope))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.
