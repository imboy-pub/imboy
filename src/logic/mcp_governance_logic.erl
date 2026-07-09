-module(mcp_governance_logic).

%%%
% MCP 治理业务逻辑 / MCP governance business logic
%
% 承载：客户端惰性登记、授权判定（enforce gate）、审批状态机、审计。
% 分层：logic → repo（mcp_client/mcp_client_grant/mcp_audit）+ 管理员动作双写
% adm_operation_log_ds。client 身份 = owner_uid（MVP：一个用户一 client）。
%
% enforce 开关：application:get_env(imboy, mcp_governance_enforce, false)。
%   false（默认）：只惰性登记 + 记 tool_call 审计，永远放行 —— 不破坏 MCP MVP。
%   true：tools/call 前校验 client 已 approved 且该 tool grant enabled，否则拒绝。
%%%

-export([enforce/0]).
-export([ensure_client/1, ensure_client/2]).
-export([authorize/2]).
-export([list_clients/4]).
-export([approve/3, reject/4, revoke/4]).
-export([set_grant/3]).
-export([grants/1]).
-export([audit_page/4]).

-include("log.hrl").

%% @doc enforce 开关（默认关闭，opt-in 强制）
-spec enforce() -> boolean().
enforce() ->
    application:get_env(imboy, mcp_governance_enforce, false) =:= true.

%% @doc 惰性登记客户端（owner_uid 无记录则插 pending），返回 client_id
-spec ensure_client(integer()) -> {ok, integer()} | {error, term()}.
ensure_client(OwnerUid) ->
    ensure_client(OwnerUid, <<>>).

-spec ensure_client(integer(), binary()) -> {ok, integer()} | {error, term()}.
ensure_client(OwnerUid, Name) when is_integer(OwnerUid), OwnerUid > 0 ->
    %% ponytail: 每次 find_by_owner 查 DB；真成热点再给 (owner_uid→client) 加 depcache。
    mcp_client_repo:ensure(OwnerUid, Name);
ensure_client(_OwnerUid, _Name) ->
    {error, invalid_owner}.

%% @doc 授权判定（MCP tools/call 前置闸门调用）。
%% 返回 allow | {deny, ReasonBin}。总是尽力登记 + 记 tool_call 审计（best-effort）。
-spec authorize(integer(), binary()) -> allow | {deny, binary()}.
authorize(OwnerUid, ToolName) when is_integer(OwnerUid), OwnerUid > 0 ->
    case ensure_client(OwnerUid) of
        {ok, ClientId} ->
            audit_tool_call(ClientId, OwnerUid, ToolName),
            case enforce() of
                false ->
                    allow;
                true ->
                    authorize_enforced(ClientId, ToolName)
            end;
        {error, _} ->
            %% 登记失败：enforce 时保守拒绝，否则放行（不因治理故障阻断 MVP）
            case enforce() of
                true -> {deny, <<"治理服务不可用"/utf8>>};
                false -> allow
            end
    end;
authorize(_OwnerUid, _ToolName) ->
    %% uid=0 未认证：JWT 层本应已拦；enforce 时再兜底拒绝
    case enforce() of
        true -> {deny, <<"未认证"/utf8>>};
        false -> allow
    end.

authorize_enforced(ClientId, ToolName) ->
    case client_status(ClientId) of
        <<"approved">> ->
            case mcp_client_grant_repo:is_enabled(ClientId, ToolName) of
                true -> allow;
                false -> {deny, <<"该 tool 未授权"/utf8>>}
            end;
        <<"revoked">> ->
            {deny, <<"客户端授权已撤销"/utf8>>};
        _ ->
            {deny, <<"客户端待审批"/utf8>>}
    end.

%% @doc 分页列出客户端（status/keyword 可空）
-spec list_clients(pos_integer(), pos_integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
list_clients(Page, Size, Status, Keyword) ->
    mcp_client_repo:page(Page, Size, Status, Keyword).

%% @doc 审批通过：改 approved + 授予当前全部已注册 tool + 双写审计
-spec approve(integer(), integer(), binary()) -> {ok, map()} | {error, binary()}.
approve(ClientId, AdmUid, Ip) ->
    transition(ClientId, <<"approved">>, <<>>, <<"approve">>, AdmUid, Ip, fun(Cid) ->
        grant_all_tools(Cid)
    end).

%% @doc 拒绝：改 revoked + 记 reason
-spec reject(integer(), binary(), integer(), binary()) -> {ok, map()} | {error, binary()}.
reject(ClientId, Reason, AdmUid, Ip) ->
    transition(ClientId, <<"revoked">>, Reason, <<"reject">>, AdmUid, Ip, fun(_Cid) -> ok end).

%% @doc 撤销：改 revoked + 记 reason
-spec revoke(integer(), binary(), integer(), binary()) -> {ok, map()} | {error, binary()}.
revoke(ClientId, Reason, AdmUid, Ip) ->
    transition(ClientId, <<"revoked">>, Reason, <<"revoke">>, AdmUid, Ip, fun(_Cid) -> ok end).

%% 状态迁移共用：校验存在 → set_status → 附加动作 → 双写审计
transition(ClientId, NewStatus, Reason, Action, AdmUid, Ip, ExtraFun) ->
    case mcp_client_repo:find(ClientId) of
        {ok, #{<<"owner_uid">> := OwnerUid}} ->
            case mcp_client_repo:set_status(ClientId, NewStatus, Reason, AdmUid) of
                {ok, _} ->
                    _ = ExtraFun(ClientId),
                    audit_admin(ClientId, OwnerUid, Action, AdmUid, Ip, Reason),
                    {ok, #{<<"client_id">> => ClientId, <<"status">> => NewStatus}};
                {error, _} ->
                    {error, <<"状态更新失败"/utf8>>}
            end;
        {error, notfound} ->
            {error, <<"客户端不存在"/utf8>>};
        {error, _} ->
            {error, <<"操作失败"/utf8>>}
    end.

%% @doc 单独开关某 tool 授权（治理详情用）
-spec set_grant(integer(), binary(), boolean()) -> {ok, map()} | {error, binary()}.
set_grant(ClientId, ToolName, Enabled) ->
    case mcp_client_grant_repo:set_enabled(ClientId, ToolName, Enabled) of
        {ok, _} ->
            {ok, #{
                <<"client_id">> => ClientId, <<"tool">> => ToolName, <<"enabled">> => Enabled
            }};
        {error, _} ->
            {error, <<"授权更新失败"/utf8>>}
    end.

%% @doc 读某 client 的授权（tools 列表 + scopes 占位）
-spec grants(integer()) -> {ok, map()} | {error, term()}.
grants(ClientId) ->
    case mcp_client_grant_repo:list_by_client(ClientId) of
        {ok, Rows} ->
            Tools = [
                #{
                    <<"name">> => maps:get(<<"tool_name">>, R),
                    <<"enabled">> => maps:get(<<"enabled">>, R)
                }
             || R <- Rows
            ],
            {ok, #{<<"client_id">> => ClientId, <<"tools">> => Tools, <<"scopes">> => []}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 分页审计
-spec audit_page(pos_integer(), pos_integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
audit_page(Page, Size, ClientId, Action) ->
    mcp_audit_repo:page(Page, Size, ClientId, Action).

%% ===================================================================
%% Internal
%% ===================================================================

client_status(ClientId) ->
    case mcp_client_repo:find(ClientId) of
        {ok, #{<<"status">> := Status}} -> Status;
        _ -> <<"unknown">>
    end.

%% approve 时授予当前注册的全部 tool（管理员可在详情里逐个关闭）
grant_all_tools(ClientId) ->
    Tools = barrel_mcp_registry:all(tool),
    lists:foreach(
        fun({Name, _Handler}) -> mcp_client_grant_repo:upsert(ClientId, Name, true) end,
        Tools
    ),
    ok.

%% tool_call 审计：best-effort，失败不阻断放行
audit_tool_call(ClientId, OwnerUid, ToolName) ->
    try
        mcp_audit_repo:insert(ClientId, OwnerUid, <<"tool_call">>, ToolName, OwnerUid, <<"{}">>)
    catch
        _:_ -> ok
    end,
    ok.

%% 管理员动作审计：双写 mcp_audit_log + adm_operation_log_ds
audit_admin(ClientId, OwnerUid, Action, AdmUid, Ip, Reason) ->
    Detail = iolist_to_binary(json:encode(#{<<"reason">> => Reason})),
    try
        mcp_audit_repo:insert(ClientId, OwnerUid, Action, <<>>, AdmUid, Detail),
        adm_operation_log_ds:insert(
            AdmUid,
            <<"mcp_client_", Action/binary>>,
            ClientId,
            <<"mcp_client">>,
            Detail,
            Ip
        )
    catch
        _:_ -> ok
    end,
    ok.
