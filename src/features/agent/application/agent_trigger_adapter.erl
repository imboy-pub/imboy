%%% @doc Message/Schedule/Webhook Trigger adapter（AG31-08；架构 §8 触发面首切片）。
%%%
%%% 职责：三类 Trigger 只创建**经验证的 AgentRun**（内部 start_run/2），
%%% 不执行自有 loop、不调用 Hirð/Tool/LLM（NON_GOALS 冻结）。
%%%
%%% 验证序（先验证后创建，任一失败零 Run 行）：
%%%   1. 幂等预检：同 source key（agent+org+trigger_type+trigger_id+
%%%      idempotency_key）重投 → find_run_by_trigger 命中 → 返回既有 Run
%%%      （A08：distinct_run_count=1，不新建）；
%%%   2. Organization state 必须 active（经 AG31-01 membership port 实时
%%%      重读）——archived/suspended/removed → {error, org_not_active}
%%%      （A15：零 Run）；webhook 域默认 workspace 同样不豁免此门；
%%%   3. Agent 身份：user 行存在 ∧ account_type=1 ∧ status=1
%%%      （R1 同权威事实）；
%%%   4. Grant 有效：agent_grant_domain:effective_status=active（实时重读）；
%%%   5. Workspace 候选：显式 workspace_id 直接采用；未显式且配置了
%%%      default workspace 模块（env `agent_default_workspace_module`，
%%%      集成树消费 ORG-05 organization_default_workspace 关系 API）→
%%%      解析失败/漂移 → {error, default_workspace_missing}（**禁止 min-ID
%%%      推导**）；模块未配置 → 不启用默认语义（workspace_id=undefined，
%%%      04B schema 允许 null）；
%%%   6. webhook 域硬门：请求必须携带 verified=true（服务端验签事实注入，
%%%      验签实现归 webhook owner）——否则 {error, webhook_unverified}
%%%      （禁止退化到 account_type 2/3 枚举）。
%%%
%%% 全过 → agent_run_command:create_run/2（04B 冻结链：created+事件同事务）。
-module(agent_trigger_adapter).

-moduledoc "Message/Schedule/Webhook Trigger adapter（AG31-08，触发面首切片）。".
-export([start_run/2]).

membership_module() ->
    application:get_env(imboy, agent_membership_module, agent_org_membership_adapter).

default_workspace_module() ->
    application:get_env(imboy, agent_default_workspace_module, none).

%% @doc 触发请求 → 验证 → 建 Run（或幂等返回既有）：
%% ```
%% start_run(Conn, TriggerRequest)
%%   -> {ok, created | existing, RunMap}
%%    | {error, ReasonAtom}
%% '''
%% TriggerRequest 必备键：trigger_type（message|schedule|webhook）、
%% trigger_id（非空 binary）、idempotency_key（非空 binary）、agent_id、
%% organization_id、grant_id、runtime_type、context_digest、now。
%% 可选：workspace_id（显式候选）、verified（webhook 必须 true）。
start_run(Conn, Req) when is_map(Req) ->
    case validate_request(Req) of
        ok -> idempotent_probe(Conn, Req);
        {error, _} = Err -> Err
    end;
start_run(_Conn, _BadReq) ->
    {error, invalid_trigger_request}.

validate_request(Req) ->
    NonEmptyB = fun(V) -> is_binary(V) andalso V =/= <<>> end,
    TypeOk = lists:member(maps:get(trigger_type, Req, undefined), [message, schedule, webhook]),
    IdsOk =
        is_pos_int(maps:get(agent_id, Req, undefined)) andalso
            is_pos_int(maps:get(organization_id, Req, undefined)) andalso
            is_pos_int(maps:get(grant_id, Req, undefined)),
    StrOk =
        NonEmptyB(maps:get(trigger_id, Req, undefined)) andalso
            NonEmptyB(maps:get(idempotency_key, Req, undefined)) andalso
            NonEmptyB(maps:get(context_digest, Req, undefined)),
    NowOk = is_tuple(maps:get(now, Req, undefined)),
    WebhookOk =
        maps:get(trigger_type, Req, message) =/= webhook orelse
            maps:get(verified, Req, false) =:= true,
    case {TypeOk, IdsOk, StrOk, NowOk, WebhookOk} of
        {true, true, true, true, true} -> ok;
        {_, _, _, _, false} -> {error, webhook_unverified};
        _ -> {error, invalid_trigger_request}
    end.

%% 步骤 1：同 source key 重投 → 既有 Run（不新建，A08）。
idempotent_probe(Conn, Req) ->
    case
        agent_run_pg:find_run_by_trigger(
            Conn,
            maps:get(agent_id, Req),
            maps:get(organization_id, Req),
            maps:get(trigger_type, Req),
            maps:get(trigger_id, Req),
            maps:get(idempotency_key, Req)
        )
    of
        {ok, Run} ->
            {ok, existing, Run};
        {error, not_found} ->
            verify_gates(Conn, Req);
        {error, _} ->
            {error, run_probe_failed}
    end.

%% 步骤 2-4：Org active → Agent 身份 → Grant 有效（先验证后创建）。
verify_gates(Conn, Req) ->
    OrgId = maps:get(organization_id, Req),
    AgentId = maps:get(agent_id, Req),
    case org_state(OrgId) of
        {ok, #{status := active}} ->
            case agent_identity(Conn, AgentId) of
                ok ->
                    case grant_effective(Conn, Req) of
                        ok ->
                            resolve_workspace_then_create(Conn, Req);
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {ok, _NonActiveShape} ->
            {error, org_not_active};
        {error, _} ->
            {error, org_not_active}
    end.

org_state(OrgId) ->
    try (membership_module()):resolve_organization_state(OrgId) of
        {ok, _} = Ok -> Ok;
        {error, _} = Err -> Err;
        _ -> {error, unavailable}
    catch
        _Class:_Reason -> {error, unavailable}
    end.

agent_identity(Conn, AgentId) ->
    try agent_run_pg:get_agent_identity(Conn, AgentId) of
        {ok, #{account_type := 1, status := 1}} ->
            ok;
        {ok, #{account_type := 1}} ->
            {error, agent_disabled};
        {ok, _} ->
            {error, agent_not_agent};
        {error, not_found} ->
            {error, agent_not_found};
        {error, _} ->
            {error, agent_read_failed}
    catch
        _Class:_Reason -> {error, agent_read_failed}
    end.

grant_effective(Conn, Req) ->
    try agent_run_pg:get_grant(Conn, maps:get(grant_id, Req)) of
        {ok, Grant} ->
            case agent_grant_domain:effective_status(Grant, maps:get(now, Req)) of
                active -> ok;
                _NonActive -> {error, grant_not_active}
            end;
        {error, not_found} ->
            {error, grant_not_found};
        {error, _} ->
            {error, grant_read_failed}
    catch
        _Class:_Reason -> {error, grant_read_failed}
    end.

%% 步骤 5：workspace 候选（显式优先；默认语义经 seam 消费 ORG-05 关系 API，
%% 禁 min-ID 推导；配置了模块而解析失败=漂移 → 拒绝）。
resolve_workspace_then_create(Conn, Req) ->
    case maps:get(workspace_id, Req, undefined) of
        WsId when is_integer(WsId), WsId > 0 ->
            do_create(Conn, Req, WsId);
        _ ->
            case default_workspace_module() of
                none ->
                    do_create(Conn, Req, undefined);
                Module ->
                    case default_ws_lookup(Module, maps:get(organization_id, Req)) of
                        {ok, DefaultWsId} when is_integer(DefaultWsId), DefaultWsId > 0 ->
                            do_create(Conn, Req, DefaultWsId);
                        _ ->
                            {error, default_workspace_missing}
                    end
            end
    end.

default_ws_lookup(Module, OrgId) ->
    try Module:resolve_default_workspace(OrgId) of
        {ok, _} = Ok -> Ok;
        {error, _} = Err -> Err;
        _ -> {error, unavailable}
    catch
        _Class:_Reason -> {error, unavailable}
    end.

do_create(Conn, Req, WorkspaceId) ->
    CreateCtx = #{
        id => agent_run_pg:next_id(agent_run),
        agent_id => maps:get(agent_id, Req),
        organization_id => maps:get(organization_id, Req),
        workspace_id => WorkspaceId,
        grant_id => maps:get(grant_id, Req),
        grant_version_at_start => maps:get(grant_version_at_start, Req, 1),
        delegating_principal_id => maps:get(delegating_principal_id, Req, undefined),
        trigger_type => maps:get(trigger_type, Req),
        trigger_id => maps:get(trigger_id, Req),
        runtime_type => maps:get(runtime_type, Req, mock),
        context_digest => maps:get(context_digest, Req),
        idempotency_key => maps:get(idempotency_key, Req),
        now => maps:get(now, Req)
    },
    case agent_run_command:create_run(Conn, CreateCtx) of
        {ok, Run} -> {ok, created, Run};
        {error, Reason} -> {error, {create_failed, Reason}}
    end.

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.
