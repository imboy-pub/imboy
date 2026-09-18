-module(agent_preflight_facts).

%% Agent 域 DeletionPreflightFacts provider（infrastructure，只读）。
%%
%% Core Contract C17 + 计划 §1.6：providers 只读、不做 handover/offboarding，
%% 返回不含 PII/credential/内部 SQL，resource_id 为 opaque string。
%% 本模块隶属 Agent 轨道（ORG-06 交付物），2026-09-18 用户拍板登记进
%% organization_deletion_preflight 默认注册表（原「推迟登记至 Agent track」
%% 裁决条款解除，见 control/ruling-agent-provider-defer.md 顶部注记）。
%%
%% 冻结 blocker（§1.6 稳定 code 清单）：
%%   * `AGENT_OWNER_ACTIVE` —— 用户名下存在任一「活跃」agent 资源：
%%       bot      status = 1（1=active；0=disabled、-1=deleted 均不构成 blocker）
%%       ai_agent status = 1（1=启用；0=停用不构成 blocker）
%%     资源处置（bot/ai_agent 停用或移交）由 Agent 轨道显式 command 先完成，
%%     本 provider 只如实上报实时事实，不做任何写操作。
%%     bot/ai_agent 均不挂 organization → blocker 的 organization_id 恒为 null。
%%
%% fact_version：COALESCE(EXTRACT(EPOCH FROM updated_at)*1000000)::bigint 的
%% 单调投影（与 organization_agent_facts_pg 的 ?FACT_VERSION_EXPR 同款；
%% updated_at 单调不回退 → 资源变更令 fact_version 单调递增），空集取下限 1
%% （编排器校验 fact_version >= 1）。
%%
%% 返回形状（§1.6 逐字段冻结）：
%%   {ok, #{subject_user_id => UserId, domain => agent, observed_at => Ms,
%%          fact_version => V,
%%          blockers => [#{code => B, resource_type => T,
%%                         resource_id => OpaqueId, organization_id => null}]}}
%%   | {error, unavailable}

-export([facts_agent/1]).

%% 活跃资源行 → opaque blocker 四字段；fact_version 取行级单调投影。
-define(FACT_VERSION_EXPR,
    <<"COALESCE((EXTRACT(EPOCH FROM updated_at) * 1000000)::bigint, 0) AS fact_version">>
).

-define(SQL_BOT_ACTIVE, <<
    "SELECT user_id, ",
    (?FACT_VERSION_EXPR)/binary,
    "  FROM ",
    (bot_table())/binary,
    " WHERE owner_uid = $1 AND status = 1",
    " ORDER BY user_id"
>>).

-define(SQL_AI_AGENT_ACTIVE, <<
    "SELECT user_id, ",
    (?FACT_VERSION_EXPR)/binary,
    "  FROM ",
    (ai_agent_table())/binary,
    " WHERE owner_uid = $1 AND status = 1",
    " ORDER BY user_id"
>>).

-define(CODE_AGENT_OWNER_ACTIVE, <<"AGENT_OWNER_ACTIVE">>).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc Agent 域实时 facts（§1.6 冻结形状）。查询失败一律 `{error,
%% unavailable}`——编排器归口 `DEPENDENCY_FACTS_UNAVAILABLE` 整体拒绝，
%% 本模块不做本地兜底。
-spec facts_agent(integer()) -> {ok, map()} | {error, unavailable}.
facts_agent(UserId) when is_integer(UserId), UserId > 0 ->
    case elib_pg:query(?SQL_BOT_ACTIVE, [UserId]) of
        {ok, BotRows} ->
            case elib_pg:query(?SQL_AI_AGENT_ACTIVE, [UserId]) of
                {ok, AgentRows} ->
                    Blockers = bot_blockers(BotRows) ++ ai_agent_blockers(AgentRows),
                    {ok, fact(UserId, Blockers, fact_version(BotRows, AgentRows))};
                {error, _Reason} ->
                    {error, unavailable}
            end;
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_agent(_) ->
    {error, unavailable}.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

bot_blockers(Rows) ->
    [blocker(<<"bot">>, Row) || Row <- Rows].

ai_agent_blockers(Rows) ->
    [blocker(<<"ai_agent">>, Row) || Row <- Rows].

blocker(ResourceType, #{<<"user_id">> := UserId}) ->
    #{
        code => ?CODE_AGENT_OWNER_ACTIVE,
        resource_type => ResourceType,
        resource_id => integer_to_binary(UserId),
        organization_id => null
    }.

%% 行级单调投影取最大值；空集回退下限 1（编排器校验 fact_version >= 1）。
fact_version(BotRows, AgentRows) ->
    Versions = [
        maps:get(<<"fact_version">>, Row)
     || Row <- BotRows ++ AgentRows
    ],
    lists:max([1 | Versions]).

fact(UserId, Blockers, Version) ->
    #{
        subject_user_id => UserId,
        domain => agent,
        observed_at => erlang:system_time(millisecond),
        fact_version => Version,
        blockers => Blockers
    }.

bot_table() ->
    elib_pg_sql:public_tablename(<<"bot">>).

ai_agent_table() ->
    elib_pg_sql:public_tablename(<<"ai_agent">>).
