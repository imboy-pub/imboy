%%% @doc PoC（IMBOY_HIRD_AUTH_REPLAY_POC_V1）membership port 实现——**非生产模块**。
%%%
%%% 背景：生产绑定 `agent_org_membership_adapter` → `organization_agent_facts_app`
%%% → `organization_agent_facts_pg`（**elib_pg 全局连接池**，需 imboy app 运行）。
%%% 隔离测试 VM 不起 app，故经 `imboy` app env `agent_membership_module`
%%% （AG31-01 冻结 port seam）注入本实现。
%%%
%%% **真实性边界**：本模块不是 mock——三个 resolve 均以显式 epgsql conn
%%% 直查 marker 库真实行（SQL 谓词镜像 organization_agent_facts_pg），
%%% 结果由真实数据决定；裁决语义（active/member 门、binary→atom 归一化、
%%% fail-closed）镜像 agent_org_membership_adapter。跳过的只是
%%% facts_app→elib_pg 池这一实现层耦合。
-module(imboy_hird_poc_membership).

-behaviour(agent_membership_port).

-export([install/1, teardown/0]).
%% agent_membership_port callbacks
-export([
    resolve_organization_membership/2,
    resolve_workspace_membership/3,
    resolve_organization_state/1
]).

-define(CONN_KEY, {imboy_hird_poc_membership, conn}).

install(Conn) ->
    persistent_term:put(?CONN_KEY, Conn),
    ok.

teardown() ->
    persistent_term:erase(?CONN_KEY),
    ok.

conn() ->
    persistent_term:get(?CONN_KEY, undefined).

%% ===================================================================
%% agent_membership_port callbacks（裁决镜像 agent_org_membership_adapter）
%% ===================================================================

resolve_organization_state(OrgId) when is_integer(OrgId), OrgId > 0 ->
    case conn() of
        undefined ->
            {error, unavailable};
        C ->
            case
                epgsql:equery(
                    C,
                    "SELECT status, COALESCE((EXTRACT(EPOCH FROM updated_at) * 1000000)::bigint, 0) "
                    "FROM organization WHERE id = $1",
                    [OrgId]
                )
            of
                {ok, _, [{StatusB, Vsn}]} ->
                    case norm(StatusB) of
                        active -> {ok, #{status => active, version => Vsn}};
                        archived -> {error, archived};
                        _ -> {error, unavailable}
                    end;
                {ok, _, []} ->
                    {error, not_found};
                {error, _} ->
                    {error, unavailable}
            end
    end;
resolve_organization_state(_) ->
    {error, not_found}.

resolve_organization_membership(OrgId, AgentId) when
    is_integer(OrgId), OrgId > 0, is_integer(AgentId), AgentId > 0
->
    case resolve_organization_state(OrgId) of
        {ok, #{status := active}} ->
            member_row(OrgId, AgentId);
        {ok, _} ->
            {error, inactive};
        {error, archived} ->
            {error, inactive};
        {error, not_found} ->
            {error, not_found};
        {error, _} ->
            {error, unavailable}
    end;
resolve_organization_membership(_, _) ->
    {error, not_found}.

member_row(OrgId, AgentId) ->
    C = conn(),
    case
        epgsql:equery(
            C,
            "SELECT role, status, COALESCE((EXTRACT(EPOCH FROM updated_at) * 1000000)::bigint, 0) "
            "FROM organization_member WHERE organization_id = $1 AND user_id = $2",
            [OrgId, AgentId]
        )
    of
        {ok, _, [{RoleB, StatusB, Vsn}]} ->
            case {norm(StatusB), norm(RoleB)} of
                {active, member} -> {ok, #{status => active, role => member, version => Vsn}};
                _ -> {error, inactive}
            end;
        {ok, _, []} ->
            {error, not_found};
        {error, _} ->
            {error, unavailable}
    end.

resolve_workspace_membership(OrgId, WsId, AgentId) when
    is_integer(OrgId), OrgId > 0, is_integer(WsId), WsId > 0, is_integer(AgentId), AgentId > 0
->
    case conn() of
        undefined ->
            {error, unavailable};
        C ->
            case
                epgsql:equery(
                    C,
                    "SELECT organization_id, status FROM workspace WHERE id = $1",
                    [WsId]
                )
            of
                {ok, _, [{OrgId, StatusB}]} ->
                    ws_member_gate(C, WsId, AgentId, norm(StatusB));
                {ok, _, [{_OtherOrg, _}]} ->
                    {error, cross_organization};
                {ok, _, []} ->
                    {error, not_found};
                {error, _} ->
                    {error, unavailable}
            end
    end;
resolve_workspace_membership(_, _, _) ->
    {error, not_found}.

ws_member_gate(C, WsId, AgentId, WsStatus) ->
    case WsStatus of
        active -> ws_member_row(C, WsId, AgentId);
        _ -> {error, inactive}
    end.

ws_member_row(C, WsId, AgentId) ->
    case
        epgsql:equery(
            C,
            "SELECT role, status FROM workspace_member WHERE workspace_id = $1 AND user_id = $2",
            [WsId, AgentId]
        )
    of
        {ok, _, [{RoleB, StatusB}]} ->
            case norm(StatusB) of
                active when RoleB =/= undefined ->
                    {ok, #{status => active, role => norm(RoleB), version => 0}};
                _ ->
                    {error, inactive}
            end;
        {ok, _, []} ->
            {error, not_found};
        {error, _} ->
            {error, unavailable}
    end.

norm(<<"active">>) -> active;
norm(<<"removed">>) -> removed;
norm(<<"archived">>) -> archived;
norm(<<"owner">>) -> owner;
norm(<<"admin">>) -> admin;
norm(<<"member">>) -> member;
norm(V) -> V.
