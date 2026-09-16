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
        {timeout, 120, fun a02_concurrent_cas_has_exactly_one_winner/0}
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
