-module(organization_preflight_stub_providers).

%% DeletionPreflightFacts provider 测试替身（仅 EUnit 使用）。
%%
%% 行为由 app env `imboy.preflight_stub_behavior` 驱动：
%%   ok_all                  —— 全部五域返回空 blocker（放行口径）
%%   blockers                —— organization 域返回一个 ORG_OWNER_ACTIVE blocker
%%   unavailable             —— organization 域返回 {error, unavailable}
%%   inconsistent            —— organization 域返回 {error, inconsistent}
%%   malformed               —— organization 域返回缺字段/坏形状的 fact
%%   crash                   —— organization 域进程内抛异常（→ inconsistent 归口）
%%   hang                    —— organization 域挂起（配合小 timeout 测超时）
%%   agent_inserts_org_owner —— facts_agent 在返回前用独立连接插入
%%                              「用户成为 org owner」并发变更（TOCTOU/DB
%%                              RESTRICT 兜底专用；org id 取
%%                              `imboy.preflight_stub_org_id`）
%%
%% 注册表（五域全 stub）见 full_registry/0，测试直接写入
%% `imboy.deletion_preflight_providers`。

-export([
    full_registry/0,
    facts_organization/1,
    facts_workspace/1,
    facts_enterprise_business/1,
    facts_customer_service/1,
    facts_agent/1
]).

-define(BEHAVIOR_KEY, preflight_stub_behavior).
-define(ORG_ID_KEY, preflight_stub_org_id).

full_registry() ->
    [
        {organization, ?MODULE, facts_organization},
        {workspace, ?MODULE, facts_workspace},
        {enterprise_business, ?MODULE, facts_enterprise_business},
        {customer_service, ?MODULE, facts_customer_service},
        {agent, ?MODULE, facts_agent}
    ].

facts_organization(UserId) ->
    act(behavior(), organization, UserId).

facts_workspace(UserId) ->
    act(behavior(), workspace, UserId).

facts_enterprise_business(UserId) ->
    act(behavior(), enterprise_business, UserId).

facts_customer_service(UserId) ->
    act(behavior(), customer_service, UserId).

facts_agent(UserId) ->
    case behavior() of
        agent_inserts_org_owner ->
            {ok, _} = insert_org_owner(UserId),
            fact(organization, UserId, []);
        _ ->
            act(behavior(), agent, UserId)
    end.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

act(ok_all, Domain, UserId) ->
    {ok, fact(Domain, UserId, [])};
act(blockers, organization, UserId) ->
    {
        ok,
        fact(organization, UserId, [
            #{
                code => <<"ORG_OWNER_ACTIVE">>,
                resource_type => <<"organization">>,
                resource_id => <<"111222333">>,
                organization_id => 111222333
            }
        ])
    };
act(blockers, Domain, UserId) ->
    {ok, fact(Domain, UserId, [])};
act(unavailable, organization, _UserId) ->
    {error, unavailable};
act(inconsistent, organization, _UserId) ->
    {error, inconsistent};
act(malformed, organization, _UserId) ->
    %% 缺 observed_at/fact_version + blocker 缺 code：按 inconsistent 归口
    {ok, #{subject_user_id => 1, domain => organization, blockers => [#{resource_type => <<"x">>}]}};
act(crash, organization, _UserId) ->
    erlang:error(stub_provider_crash);
act(hang, organization, UserId) ->
    timer:sleep(60000),
    {ok, fact(organization, UserId, [])};
act(_Other, Domain, UserId) ->
    {ok, fact(Domain, UserId, [])}.

behavior() ->
    case application:get_env(imboy, ?BEHAVIOR_KEY) of
        {ok, B} -> B;
        undefined -> ok_all
    end.

fact(Domain, UserId, Blockers) ->
    #{
        subject_user_id => UserId,
        domain => Domain,
        observed_at => erlang:system_time(millisecond),
        fact_version => 1,
        blockers => Blockers
    }.

insert_org_owner(UserId) ->
    OrgId =
        case application:get_env(imboy, ?ORG_ID_KEY) of
            {ok, Id} when is_integer(Id) -> Id;
            _ -> 999000111
        end,
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization (id, name, owner_id)"
            " VALUES ($1, 'toctou-probe', $2) ON CONFLICT (id) DO NOTHING"
        >>,
        [OrgId, UserId]
    ),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization_member"
            " (organization_id, user_id, role, joined_at, status)"
            " VALUES ($1, $2, 'owner', CURRENT_TIMESTAMP, 'active')"
            " ON CONFLICT (organization_id, user_id) DO NOTHING"
        >>,
        [OrgId, UserId]
    ),
    {ok, OrgId}.
