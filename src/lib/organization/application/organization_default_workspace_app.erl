%%% @doc Organization Default Workspace 应用层命令（Core Contract C05 / 计划 ORG-05）。
%%%
%%% 显式默认关系取代最小 active Workspace ID 推导（C05 TRANSITION）。
%%% API（应用层）：get/set/clear；set 同值幂等、clear 空值幂等；
%%% 变更锁 organization 行（与 owner transfer 同锁序：组织行先）；
%%% 默认是**指路事实而非权限事实**——本模块不提供任何授权原语，
%%% 读取不回落 min-ID 推导。
%%%
%%% Workspace 生命周期同事务钩子（由 workspace 侧精确调用，非公开命令）：
%%%   * `ensure_first_workspace_tx/3`：首个 Org Workspace 创建时同事务设默认；
%%%   * `replace_or_clear_on_archive_tx/3`：归档默认时同事务交接——
%%%     有剩余 active 时 replace_with_min_active；无剩余 active 时拒绝归档
%%%     （GZAPP-02/G3 强交接：须先显式指定替代默认，不再 clear）。
-module(organization_default_workspace_app).

-export([get/1, set/3, clear/2]).
-export([ensure_first_workspace_tx/3, replace_or_clear_on_archive_tx/3]).

-include("log.hrl").

%% ===================================================================
%% 读
%% ===================================================================

%% @doc 读取 Org 当前默认 Workspace（可空语义：{error, not_set} = 未设默认）。
%% **不回落 min-ID 推导**——显式关系是唯一读取真源（负例见 EUnit）。
-spec get(integer()) -> {ok, integer()} | {error, not_set | term()}.
get(OrgId) ->
    case organization_default_workspace:valid_id(OrgId) of
        {error, {invalid_id, _}} ->
            {error, not_set};
        ok ->
            case organization_default_workspace_pg:find(OrgId) of
                {ok, WsId} -> {ok, WsId};
                {error, not_found} -> {error, not_set};
                {error, Reason} -> {error, Reason}
            end
    end.

%% ===================================================================
%% 写命令
%% ===================================================================

%% @doc 设置/改设默认 Workspace。
%% 幂等：同值 set 返回 {ok, unchanged}（不刷新审计列）。
%% 锁序：先锁 organization 行，再做目标预检与写入。
%% 稳定错误码：404 目标/Org 不存在 / 409 目标非 active 或跨 Org。
%% （跨 Org / 非 active 的最终裁决在 DB：组合 FK 23503 + 守卫触发器 23514；
%%   500 为 DB 异常兜底，不吞真实故障。）
-spec set(integer(), integer(), integer()) ->
    {ok, changed | unchanged} | {error, {400 | 404 | 409 | 500, binary()}}.
set(OperatorUid, OrgId, WsId) ->
    case precheck_ids(OrgId, WsId) of
        {error, _} = Error ->
            Error;
        ok ->
            case elib_pg:with_tx(fun(Conn) -> set_tx(Conn, OrgId, WsId) end) of
                {ok, Status} ->
                    _ = ?INFO_LOG([
                        organization_default_workspace_set, OrgId, WsId, OperatorUid, Status
                    ]),
                    {ok, Status};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_default_workspace_set_failed, OrgId, WsId, Reason]),
                    set_error(Reason)
            end
    end.

%% @doc 清空默认（幂等：未设默认返回 {ok, already_empty}）。
-spec clear(integer(), integer()) ->
    {ok, cleared | already_empty} | {error, {400 | 404 | 500, binary()}}.
clear(OperatorUid, OrgId) ->
    case organization_default_workspace:valid_id(OrgId) of
        {error, {invalid_id, _}} ->
            {error, {400, <<"organization_id 必须是正整数"/utf8>>}};
        ok ->
            case
                elib_pg:with_tx(fun(Conn) ->
                    ok = lock_organization_tx(Conn, OrgId),
                    organization_default_workspace_pg:delete_tx(Conn, OrgId)
                end)
            of
                {ok, Status} ->
                    _ = ?INFO_LOG([
                        organization_default_workspace_cleared, OrgId, OperatorUid, Status
                    ]),
                    {ok, Status};
                {error, not_found} ->
                    %% organization 行不存在（锁失败）——与 set 同口径 404
                    {error, {404, <<"Organization 不存在"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_default_workspace_clear_failed, OrgId, Reason]),
                    {error, {500, <<"清空默认 Workspace 失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% Workspace 生命周期同事务钩子
%% ===================================================================

%% @doc 首个 Org Workspace 创建时同事务设默认。
%% `Conn` 必须是 create_template 的同一事务连接；OrgId=undefined（个人域）直接 ok。
%% 非首个 / 已有默认 → 不动作（ok）；失败抛 abort_tx 由 create_template 回滚。
-spec ensure_first_workspace_tx(any(), integer() | undefined, integer()) -> ok.
ensure_first_workspace_tx(_Conn, undefined, _WsId) ->
    ok;
ensure_first_workspace_tx(Conn, OrgId, WsId) ->
    case organization_default_workspace_pg:ensure_first_workspace_tx(Conn, OrgId, WsId) of
        ok ->
            ok;
        {error, Reason} ->
            throw({abort_tx, {organization_default_workspace_set_failed, Reason}})
    end.

%% @doc 归档工作区时的默认同事务交接。
%% 仅当被归档者是该 Org 当前默认时动作：有剩余 active → replace-with-min-active；
%% 无剩余 active → **拒绝归档**（GZAPP-02/G3 强交接，计划 §4.2：归档默认
%% Workspace 前必须先指定替代项），抛专用 abort 标记由 workspace_logic
%% 映射稳定错误码 409；其余失败抛 abort_tx 由 archive 事务回滚
%% （默认永不指向 archived Workspace）。
-spec replace_or_clear_on_archive_tx(any(), integer() | undefined, integer()) -> ok.
replace_or_clear_on_archive_tx(_Conn, undefined, _WsId) ->
    ok;
replace_or_clear_on_archive_tx(Conn, OrgId, WsId) ->
    case organization_default_workspace_pg:replace_or_clear_on_archive_tx(Conn, OrgId, WsId) of
        ok ->
            ok;
        {error, no_active_replacement} ->
            %% G3 强交接拒绝：稳定标记上抛（非 500 类故障），
            %% workspace_logic 的 archive/admin_archive 映射 409。
            throw({abort_tx, {default_workspace_handover_required, no_active_replacement}});
        {error, Reason} ->
            throw({abort_tx, {organization_default_workspace_handover_failed, Reason}})
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec precheck_ids(integer(), integer()) -> ok | {error, {400, binary()}}.
precheck_ids(OrgId, WsId) ->
    case organization_default_workspace:valid_id(OrgId) of
        {error, {invalid_id, _}} ->
            {error, {400, <<"organization_id 必须是正整数"/utf8>>}};
        ok ->
            case organization_default_workspace:valid_id(WsId) of
                {error, {invalid_id, _}} ->
                    {error, {400, <<"workspace_id 必须是正整数"/utf8>>}};
                ok ->
                    ok
            end
    end.

-spec set_tx(any(), integer(), integer()) ->
    {ok, changed | unchanged}
    | {error, not_found | not_active | cross_org | org_not_found | term()}.
set_tx(Conn, OrgId, WsId) ->
    case lock_organization_tx(Conn, OrgId) of
        ok ->
            case organization_default_workspace_pg:target_row_tx(Conn, WsId) of
                {error, not_found} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, Reason};
                {ok, #{<<"organization_id">> := RowOrg, <<"status">> := Status}} ->
                    %% 同 Org / active 裁决集中在 domain（单一决策真源）：
                    %% cross_org 409 / not_active 409 / not_found 404
                    case
                        organization_default_workspace:ensure_settable_target(OrgId, RowOrg, Status)
                    of
                        ok ->
                            organization_default_workspace_pg:upsert_tx(Conn, OrgId, WsId);
                        {error, Reason2} ->
                            {error, Reason2}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% organization 行锁（与 owner transfer 同一 store：组织行先、成员/资源行后）。
%% 行不存在 → {error, org_not_found}。
-spec lock_organization_tx(any(), integer()) -> ok | {error, org_not_found | term()}.
lock_organization_tx(Conn, OrgId) ->
    case organization_owner_store:lock_organization_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, not_found} -> {error, org_not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec set_error(term()) -> {error, {400 | 404 | 409 | 500, binary()}}.
set_error(org_not_found) ->
    {error, {404, <<"Organization 不存在"/utf8>>}};
set_error(not_found) ->
    {error, {404, <<"目标 Workspace 不存在或不属于该 Organization"/utf8>>}};
set_error(not_active) ->
    {error, {409, <<"目标 Workspace 已归档，不能设为默认"/utf8>>}};
set_error(cross_org) ->
    {error, {409, <<"默认 Workspace 必须属于同一 Organization"/utf8>>}};
set_error(_Reason) ->
    {error, {500, <<"设置默认 Workspace 失败，请稍后重试"/utf8>>}}.
