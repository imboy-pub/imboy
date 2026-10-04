-module(agent_preflight_facts_pg).

-moduledoc "Agent 域 DeletionPreflightFacts provider（D6 认领 / ORG-A0 裁决），PG 实现。".
%% Agent 域 DeletionPreflightFacts provider（D6 认领 / ORG-A0 裁决
%% ruling-agent-provider-defer.md / Core Contract C17 + 计划 §1.6）。
%%
%% 只读：本模块是 User deletion preflight 的 agent 域实时事实源，不做
%% handover/offboarding、不扩展任何 Agent Runtime 能力（D6 裁决边界：
%% 无 UI、无公开 API、无自动 offboarding）。
%%
%% 冻结 blocker：
%%   * `AGENT_OWNER_ACTIVE` —— 用户名下拥有 status=1（active）的 bot：
%%     bot.owner_uid = subject 且 bot.status = 1。bot.owner_uid 无外键
%%     保护（仅 bot.user_id 有 FK ON DELETE CASCADE），删除所有者会孤儿化
%%     其名下 active bot——本 blocker 即该缺口的 preflight 解释性证据。
%%     消除路径：bot 停用（status=0）或删除（status=-1）后 blocker 消失。
%%
%% 不产生 blocker 的情形（有意，零重解释）：
%%   * 名下 bot 已停用/已删除（status 0 / -1）；
%%   * subject 自身是 bot 账号（bot.user_id = subject）——该 bot 行由
%%     FK ON DELETE CASCADE 随账号删除处理，非「所有者归属」事实；
%%   * agent_grant/run 等授权与运行事实——授权可撤销、非所有权，其
%%     清理语义归各自 command 域（D6 裁决禁令：不扩展其他能力）。
%%
%% 返回形状（§1.6 逐字段冻结；不含 PII/credential/内部 SQL；resource_id
%% opaque；bot 不挂组织 → organization_id = null，合同允许 null）：
%%   {ok, #{subject_user_id => UserId, domain => agent,
%%          observed_at => Ms, fact_version => 1,
%%          blockers => [#{code => <<"AGENT_OWNER_ACTIVE">>,
%%                         resource_type => <<"bot">>,
%%                         resource_id => OpaqueBotUserId,
%%                         organization_id => null}]}}
%%   | {error, unavailable}
%%
%% 登记（ORG-A0 合并权限执行，见 D6 移交文件）：
%%   {agent, agent_preflight_facts_pg, facts_agent}。

-export([facts_agent/1]).
-export([fact_version/0]).

-define(FACT_VERSION, 1).

-define(SQL_AGENT_OWNER_ACTIVE, <<
    "SELECT user_id AS bot_user_id"
    "  FROM bot"
    " WHERE owner_uid = $1 AND status = 1"
    " ORDER BY user_id"
>>).

%% @doc agent 域实时 facts（§1.6 冻结形状）。查询失败一律 `{error,
%% unavailable}`——编排器（organization_deletion_preflight）把 unavailable
%% 归口为 `DEPENDENCY_FACTS_UNAVAILABLE` 整体拒绝，本模块不做本地兜底。
-spec facts_agent(integer()) -> {ok, map()} | {error, unavailable}.
facts_agent(UserId) when is_integer(UserId), UserId > 0 ->
    case elib_pg:query(?SQL_AGENT_OWNER_ACTIVE, [UserId]) of
        {ok, Rows} ->
            Blockers = [
                #{
                    code => <<"AGENT_OWNER_ACTIVE">>,
                    resource_type => <<"bot">>,
                    resource_id => opaque(maps:get(<<"bot_user_id">>, Row)),
                    organization_id => null
                }
             || Row <- Rows
            ],
            {ok, fact(UserId, Blockers)};
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_agent(_) ->
    {error, unavailable}.

-spec fact_version() -> pos_integer().
fact_version() ->
    ?FACT_VERSION.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

fact(UserId, Blockers) ->
    #{
        subject_user_id => UserId,
        domain => agent,
        observed_at => erlang:system_time(millisecond),
        fact_version => ?FACT_VERSION,
        blockers => Blockers
    }.

opaque(Id) when is_integer(Id) ->
    integer_to_binary(Id);
opaque(Id) when is_binary(Id) ->
    Id.
