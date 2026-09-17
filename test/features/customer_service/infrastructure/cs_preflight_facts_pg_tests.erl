-module(cs_preflight_facts_pg_tests).

%% ORG-08：User deletion preflight 的 EB/CS 域 provider 测试（C17 / 计划 §1.6）。
%%
%% 覆盖：
%%   * eb_preflight_facts_pg:facts_enterprise_business/1 —— active assignment
%%     → BUSINESS_IDENTITY_ASSIGNMENT_ACTIVE（opaque 四字段冻结形状）；
%%     assignment 结束（handover）→ blocker 消除；
%%   * cs_preflight_facts_pg:facts_customer_service/1 —— active assignment +
%%     enabled customer_service Seat → CUSTOMER_SERVICE_OPERATION_ACTIVE；
%%     assignment 结束（Seat 保留 enabled）→ blocker 消除（§6：User delete 前
%%     Assignment 必须无 active，Seat 不随 User 删除）；Seat 停用 → 无 blocker；
%%   * 编排器集成：真实 EB/CS provider + ORG-02 org/workspace provider +
%%     agent stub 全注册 → blockers 聚合；缺 EB/CS/agent 注册 → fail-closed 拒。
%%
%% 运行：make eunit-local t=cs_preflight_facts_pg_tests
%% PG：一次性容器 imboy-org08-pg18 @127.0.0.1:4393（ORG08_PGPORT 可覆盖）；
%% 环境不可用 ⇒ erlang:error/1（不是 skip）。

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_preflight_facts_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    ensure_test_pg_conf(),
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            {ok, Conn};
        {error, Reason} ->
            erlang:error({cs_preflight_facts_db_unavailable, Reason})
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun eb_provider_reports_active_assignment/0},
        {timeout, 60, fun cs_provider_reports_operating_seat/0},
        {timeout, 60, fun orchestrator_aggregates_with_real_eb_cs_providers/0},
        {timeout, 0, fun orchestrator_fail_closed_without_registration/0}
    ];
cases({error, Reason}) ->
    erlang:error({cs_preflight_facts_db_unavailable, Reason}).

%% ===================================================================
%% EB 域 provider
%% ===================================================================

eb_provider_reports_active_assignment() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Assignment = maps:get(assignment_id, Scope),
    try
        {ok, Fact} = eb_preflight_facts_pg:facts_enterprise_business(Actor),
        %% §1.6 冻结形状
        ?assertEqual(Actor, maps:get(subject_user_id, Fact)),
        ?assertEqual(enterprise_business, maps:get(domain, Fact)),
        ?assert(is_integer(maps:get(observed_at, Fact))),
        ?assertEqual(1, maps:get(fact_version, Fact)),
        [Blocker] = maps:get(blockers, Fact),
        ?assertEqual(<<"BUSINESS_IDENTITY_ASSIGNMENT_ACTIVE">>, maps:get(code, Blocker)),
        ?assertEqual(
            <<"organization_business_identity_assignment">>,
            maps:get(resource_type, Blocker)
        ),
        ?assertEqual(integer_to_binary(Assignment), maps:get(resource_id, Blocker)),
        ?assertEqual(Org, maps:get(organization_id, Blocker)),
        %% 无 assignment 的用户 → 空 blocker（本次实时读取无 blocker）
        {ok, CleanFact} = eb_preflight_facts_pg:facts_enterprise_business(
            maps:get(peer_user_id, Scope)
        ),
        ?assertEqual([], maps:get(blockers, CleanFact)),
        %% handover（assignment 结束）→ blocker 消除
        ok = ?FIX:exec(
            <<
                "UPDATE organization_business_identity_assignment"
                " SET status='ended', ended_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
            >>,
            [Org, Actor]
        ),
        {ok, FactAfter} = eb_preflight_facts_pg:facts_enterprise_business(Actor),
        ?assertEqual([], maps:get(blockers, FactAfter))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS 域 provider
%% ===================================================================

cs_provider_reports_operating_seat() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        %% fixture：actor 有 sales active assignment + service identity 有 enabled seat。
        %% actor 没有 customer_service 职能 assignment → 无 CS 在岗 blocker。
        {ok, Fact0} = cs_preflight_facts_pg:facts_customer_service(Actor),
        ?assertEqual(customer_service, maps:get(domain, Fact0)),
        ?assertEqual([], maps:get(blockers, Fact0)),
        %% 接上 customer_service 职能的 active assignment（+ enabled seat 已就位）
        AssignmentId = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
                " VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [AssignmentId, Org, Service, Actor, maps:get(owner_user_id, Scope)]
        ),
        {ok, Fact1} = cs_preflight_facts_pg:facts_customer_service(Actor),
        [Blocker] = maps:get(blockers, Fact1),
        ?assertEqual(<<"CUSTOMER_SERVICE_OPERATION_ACTIVE">>, maps:get(code, Blocker)),
        ?assertEqual(<<"customer_service_seat">>, maps:get(resource_type, Blocker)),
        ?assertEqual(integer_to_binary(Service), maps:get(resource_id, Blocker)),
        ?assertEqual(Org, maps:get(organization_id, Blocker)),
        %% Seat 停用 → 坐席门关闭 → 无在岗 blocker（与 cs_auth A04 门同口径）
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_seat SET enabled=false WHERE organization_id=$1"
                " AND business_identity_id=$2"
            >>,
            [Org, Service]
        ),
        {ok, Fact2} = cs_preflight_facts_pg:facts_customer_service(Actor),
        ?assertEqual([], maps:get(blockers, Fact2)),
        %% Seat 恢复、assignment handover 结束（Seat 保留 enabled）→ blocker 消除
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_seat SET enabled=true WHERE organization_id=$1"
                " AND business_identity_id=$2"
            >>,
            [Org, Service]
        ),
        ok = ?FIX:exec(
            <<
                "UPDATE organization_business_identity_assignment"
                " SET status='ended', ended_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP"
                " WHERE id=$1"
            >>,
            [AssignmentId]
        ),
        {ok, Fact3} = cs_preflight_facts_pg:facts_customer_service(Actor),
        ?assertEqual([], maps:get(blockers, Fact3)),
        %% Seat 仍在（§6：Seat 不随 User 删除/离岗）
        ?assertEqual(1, ?FIX:count(Org, seats))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 编排器集成（真实 EB/CS provider 注册）
%% ===================================================================

orchestrator_aggregates_with_real_eb_cs_providers() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    try
        %% actor：active member（fixture）+ customer_service active assignment
        AssignmentId = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
                " VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [AssignmentId, Org, Service, Actor, Owner]
        ),
        Registry =
            [
                {organization, organization_preflight_facts_pg, facts_organization},
                {workspace, organization_preflight_facts_pg, facts_workspace},
                {enterprise_business, eb_preflight_facts_pg, facts_enterprise_business},
                {customer_service, cs_preflight_facts_pg, facts_customer_service},
                {agent, organization_preflight_stub_providers, facts_agent}
            ],
        ok = application:set_env(imboy, deletion_preflight_providers, Registry),
        ok = application:set_env(imboy, preflight_stub_behavior, ok_all),
        try
            {ok, #{blockers := Blockers}} = organization_deletion_preflight:run(Actor),
            Codes = [maps:get(code, B) || B <- Blockers],
            %% org 域（ORG-02）：active member blocker
            ?assert(lists:member(<<"ORG_MEMBERSHIP_ACTIVE">>, Codes)),
            %% EB 域（本任务）：sales active assignment blocker
            ?assert(lists:member(<<"BUSINESS_IDENTITY_ASSIGNMENT_ACTIVE">>, Codes)),
            %% CS 域（本任务）：customer_service 在岗 blocker
            ?assert(lists:member(<<"CUSTOMER_SERVICE_OPERATION_ACTIVE">>, Codes))
        after
            application:unset_env(imboy, deletion_preflight_providers),
            application:unset_env(imboy, preflight_stub_behavior)
        end,
        %% workspace 域 provider（ORG-02 只读代查）对 owner 名下 fixture workspace
        %% 报 WORKSPACE_OWNER_ACTIVE
        ok = application:set_env(imboy, deletion_preflight_providers, [
            {workspace, organization_preflight_facts_pg, facts_workspace}
        ]),
        try
            {error, #{code := <<"DEPENDENCY_FACTS_UNAVAILABLE">>, reason := provider_unregistered}} =
                organization_deletion_preflight:run(Owner)
        after
            application:unset_env(imboy, deletion_preflight_providers)
        end
    after
        ?FIX:cleanup(Scope)
    end.

%% 未注册域 = facts 不可得 = 拒（合同明文）。
%% 期望演进（ORG-12 集成门基线重建时修正）：A0 合并（ORG-08 BLOCKED_INT-2）
%% 已把 EB/CS provider 登记进默认注册表（4/5 域 staged 语义），缺域只剩 agent
%% ——见 control/ruling-agent-provider-defer.md（第 5 域推迟登记至 Agent track）。
%% 冻结语义不变：缺域照拒，本断言验证的正是「缺什么拒什么」的如实上报。
orchestrator_fail_closed_without_registration() ->
    application:unset_env(imboy, deletion_preflight_providers),
    {error, #{
        code := <<"DEPENDENCY_FACTS_UNAVAILABLE">>,
        reason := provider_unregistered,
        detail := Missing
    }} = organization_deletion_preflight:run(424242),
    ?assertEqual([agent], lists:sort(Missing)).

%% ===================================================================
%% 内部辅助（与 cs_org_compat_tests 同一容器口径）
%% ===================================================================

ensure_test_pg_conf() ->
    _ = application:load(imboy),
    Port = list_to_integer(os:getenv("ORG08_PGPORT", "4393")),
    PgConf = #{
        name => pgsql,
        max_count => 40,
        init_count => 5,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => "127.0.0.1",
                    username => "imboy_user",
                    password => "abc54321",
                    database => "imboy_v1",
                    port => Port,
                    ssl => false,
                    timeout => 4000,
                    codecs => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]}
    },
    application:set_env(imboy, pg_conf, PgConf).
