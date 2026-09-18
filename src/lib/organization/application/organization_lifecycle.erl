-module(organization_lifecycle).

%% Organization 生命周期 command（application 层）：archive / restore。
%%
%% Core Contract C16：
%%   * Organization 支持 create、active、archive、restore；普通 V1 API
%%     不提供物理 delete。
%%   * SOURCE OF TRUTH：organization.status（枚举不变：active | archived）。
%%   * archive/restore 是幂等 command + 审计；重复命令返回稳定当前状态，
%%     不产生重复审计事件。
%%   * MUST NOT：archive 自动恢复/撤销 member、Workspace、Assignment、
%%     Seat、Grant 的独立状态（只改 organization 行本身）。
%%   * archived 禁新写、授权只读与 restore 放行——写入口的拒绝由各入口
%%     自行裁决（organization_logic:update、organization_member_logic:write_tx、
%%     organization_owner_transfer 等已按 status='active' 门禁）。
%%
%% 鉴权口径与既有治理写一致：仅 active owner/admin 可 archive/restore。

-export([archive/2, restore/2]).

-include("log.hrl").

%% @doc 归档 Organization（幂等：已归档返回当前状态，不重复审计）。
-spec archive(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
archive(ActorUid, OrgId) when is_integer(ActorUid), ActorUid > 0, is_integer(OrgId), OrgId > 0 ->
    transition(ActorUid, OrgId, <<"archived">>);
archive(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% @doc 恢复 Organization（幂等：已 active 返回当前状态，不重复审计）。
-spec restore(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
restore(ActorUid, OrgId) when is_integer(ActorUid), ActorUid > 0, is_integer(OrgId), OrgId > 0 ->
    transition(ActorUid, OrgId, <<"active">>);
restore(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

transition(ActorUid, OrgId, TargetStatus) ->
    AuditTag =
        case TargetStatus of
            <<"archived">> -> organization_archived;
            <<"active">> -> organization_restored
        end,
    Tx = fun(Conn) -> transition_tx(Conn, ActorUid, OrgId, TargetStatus) end,
    case elib_pg:with_tx(Tx) of
        {ok, {Org, true}} ->
            %% 状态实际发生变化：审计一次（幂等重放不进此分支）
            ok = ?INFO_LOG([AuditTag, OrgId, ActorUid, TargetStatus]),
            {ok, Org};
        {ok, {Org, false}} ->
            %% 幂等重放：状态未变，返回稳定当前状态，不重复审计
            {ok, Org};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_lifecycle_failed, AuditTag, OrgId, ActorUid, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            ?ERROR_LOG([organization_lifecycle_failed, AuditTag, OrgId, ActorUid, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

transition_tx(Conn, ActorUid, OrgId, TargetStatus) ->
    %% 1) 锁组织行 + 存在性
    Org =
        case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
            {ok, Row} ->
                Row;
            {error, not_found} ->
                abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} ->
                throw({abort_tx, {internal, Reason1}})
        end,
    %% 2) actor 必须是 active owner/admin（成员行 SHARE 锁，组织行已先锁）
    case organization_lifecycle_pg:lock_member_role_tx(Conn, OrgId, ActorUid) of
        {ok, Role} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
            ok;
        {ok, _} ->
            abort(403, <<"仅 Organization Owner 或 Admin 可执行此操作"/utf8>>);
        {error, not_found} ->
            abort(403, <<"仅 Organization Owner 或 Admin 可执行此操作"/utf8>>);
        {error, Reason2} ->
            throw({abort_tx, {internal, Reason2}})
    end,
    %% 3) 幂等推进：目标状态 == 当前状态 → 直接返回当前行（不写、不审计）
    case maps:get(<<"status">>, Org) of
        TargetStatus ->
            {ok, {Org, false}};
        _Current ->
            case organization_lifecycle_pg:set_status_tx(Conn, OrgId, TargetStatus) of
                {ok, Updated} ->
                    {ok, {Updated, true}};
                {error, Reason3} ->
                    throw({abort_tx, {internal, Reason3}})
            end
    end.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
