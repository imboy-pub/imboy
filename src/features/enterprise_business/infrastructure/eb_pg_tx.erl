%%% @doc EB-03R T1/T2：`eb_tx_port` 的实现（用例级事务，薄信封）。
%%%
%%% 依据：用户裁决 §三——「application 不得直连 `eb_pg_canonical_tx`/`eb_pg_purge`；
%%% 为原子消息、会话审计和 bounded purge 提供用例级 Port」。
%%%
%%% ## 为什么是「薄」
%%%
%%% 事务编排（consent gate / policy snapshot / 加密封装 / 审计 detail）**已经**在
%%% `eb_pg_canonical_tx` 里做对了（R0：语义对、边界错）。本模块**不重写**它，
%%% 只把调用面收敛成 Port 的信封 `(OrgId, WorkspaceId, Params)`，使
%%% application 层**只能**通过具名用例触达事务，而拿不到「开事务 + 发任意 SQL」的能力。
%%%
%%% 因此本模块的导出面**只有**两个具名用例：
%%%   * `accept_message/3` → 委托 `eb_pg_canonical_tx:accept_message/3`；
%%%   * `append_conversation_audit/3` → 在同一事务内追加 `conversation.open` 审计。
%%% 白名单外**没有任何** `exec/1`、`query/2`、`transaction/1`（A04 的导出面门）。
-module(eb_pg_tx).

-moduledoc "eb_tx_port 实现（EB-03R T1/T2）—— 用例级事务薄信封。".
-behaviour(eb_tx_port).

-export([accept_message/3, append_conversation_audit/3]).

-define(DEFAULT_CONVERSATION_ACTION, <<"conversation.open">>).

%% @doc 原子接受企业消息（canonical message + policy snapshot + audit 同事务）。
%%
%% 语义与 `eb_pg_canonical_tx:accept_message/3` **逐字相同**（Params 键、返回结构、
%% 失败多保留的回滚行为）。本函数只做 Port 信封校验（租户键必须是整数）。
-spec accept_message(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
accept_message(OrgId, WorkspaceId, Params) when is_map(Params) ->
    case eb_pg_exec:tenant_error(OrgId, WorkspaceId) of
        ok -> eb_pg_canonical_tx:accept_message(OrgId, WorkspaceId, Params);
        {error, _} = Err -> Err
    end;
accept_message(OrgId, WorkspaceId, _Params) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

%% @doc 会话生命周期审计**与调用方上下文同事务提交**。
%%
%% `Params`：`action`（默认 `conversation.open`）、`detail`（jsonb 友好 map，
%% 不得含密文/明文/token）、`resource_type`（默认 `enterprise_conversation`）、
%% `resource_id`、`business_identity_id`、`actor_user_id`、`actor_role`。
%%
%% 失败一律 `{error, _}`（审计丢失不得静默——静默会让「会话已建立」失去证据）。
-spec append_conversation_audit(integer(), integer(), map()) -> {ok, integer()} | {error, term()}.
append_conversation_audit(OrgId, WorkspaceId, Params) when is_map(Params) ->
    case eb_pg_exec:tenant_error(OrgId, WorkspaceId) of
        ok ->
            Event = audit_event(WorkspaceId, Params),
            run_in_tx(OrgId, Event);
        {error, _} = Err ->
            Err
    end;
append_conversation_audit(OrgId, WorkspaceId, _Params) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

run_in_tx(OrgId, Event) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> eb_pg_audit:append_in(Conn, OrgId, Event) end,
            [{reraise, false}]
        )
    of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

audit_event(WorkspaceId, Params) ->
    #{
        id => eb_tsid:new_id(enterprise_audit),
        resource_type => maps:get(resource_type, Params, <<"enterprise_conversation">>),
        resource_id => maps:get(resource_id, Params, undefined),
        action => maps:get(action, Params, ?DEFAULT_CONVERSATION_ACTION),
        business_identity_id => maps:get(business_identity_id, Params, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined),
        actor_role => maps:get(actor_role, Params, undefined),
        detail => conversation_detail(WorkspaceId, Params)
    }.

conversation_detail(WorkspaceId, Params) ->
    Detail = maps:get(detail, Params, #{}),
    case is_map(Detail) of
        true -> Detail#{<<"workspace_id">> => WorkspaceId};
        false -> #{<<"workspace_id">> => WorkspaceId}
    end.
