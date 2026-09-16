-module(organization_owner_transfer).

%% Owner 转移 command（application 层）。
%%
%% Core Contract C04：转移在单事务和 Organization 行锁内完成；
%% `organization.owner_id` 是兼容投影，active Human owner membership 是真源。
%%
%% 冻结实现（计划 ORG-01 + 任务卡）：
%%   1. `SELECT ... FOR UPDATE` 锁 Organization 行（与既有治理写同一锁序：
%%      组织行先、成员行后）；
%%   2. 验证 target 是 active Human member（account_type = 0）；
%%   3. 同一事务内：先降旧 owner（owner -> admin）→ 再升新 owner → 最后更新
%%      `owner_id` 投影（同步触发器 trg_organization_owner_member_sync 幂等 upsert）。
%%   语句间瞬态（0 个 owner）由 partial unique index
%%   uq_organization_member_single_active_owner 的时序容忍（先降后升），
%%   提交时由两侧 DEFERRABLE INITIALLY DEFERRED invariant 触发器做最终校验
%%   （恰好一个 active Human owner 且 = owner_id）；deferred 化的
%%   trg_organization_primary_owner_member_guard 在提交时以 owner_id 最终值
%%   裁决旧 owner 降级的合法性。

-export([transfer/3]).

-include("log.hrl").

%% @doc 转移 Owner。
%%
%% 返回（与既有 API 响应兼容）：
%%   {ok, #{organization_id, owner_id, previous_owner_id, previous_owner_role}}
%%   | {error, {HttpCode, Message}}
-spec transfer(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
transfer(ActorUid, OrgId, TargetUid) when
    is_integer(ActorUid),
    ActorUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_integer(TargetUid),
    TargetUid > 0
->
    case organization_owner_invariant:validate_self_transfer(ActorUid, TargetUid) of
        {error, _} = Rejected ->
            Rejected;
        ok ->
            Tx = fun(Conn) -> transfer_tx(Conn, ActorUid, OrgId, TargetUid) end,
            case elib_pg:with_tx(Tx) of
                {ok, #{owner_id := TargetUid} = Result} when is_map(Result) ->
                    {ok, Result};
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    %% invariant/guard 的提交时拒绝或基础设施错误：统一以 500 归口，
                    %% 细节留日志（与既有 transfer 路径的日志行为一致）。
                    ?ERROR_LOG([
                        organization_owner_transfer_failed, OrgId, ActorUid, TargetUid, Reason
                    ]),
                    {error, {500, owner_transfer_internal_error(Reason)}}
            end
    end;
transfer(_, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

transfer_tx(Conn, ActorUid, OrgId, TargetUid) ->
    %% 1) 锁组织行 + 生命周期/身份裁决
    Org =
        case organization_owner_store:lock_organization_tx(Conn, OrgId) of
            {ok, #{<<"status">> := <<"active">>} = OrgRow} ->
                OrgRow;
            {ok, _Archived} ->
                abort(409, <<"Organization 已归档，不能转移 Owner"/utf8>>);
            {error, not_found} ->
                abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} ->
                throw({abort_tx, {internal, Reason1}})
        end,
    %% 2) 仅当前投影 owner 可发起
    case organization_owner_invariant:validate_current_owner(Org, ActorUid) of
        ok -> ok;
        {error, {Code, Msg}} -> abort(Code, Msg)
    end,
    ActorMember =
        case organization_owner_store:lock_member_with_account_tx(Conn, OrgId, ActorUid) of
            {ok, Row} -> Row;
            {error, not_found} -> abort(403, <<"当前主 Owner 成员状态无效"/utf8>>);
            {error, Reason2} -> throw({abort_tx, {internal, Reason2}})
        end,
    case organization_owner_invariant:validate_actor_membership(ActorMember) of
        ok -> ok;
        {error, {Code3, Msg3}} -> abort(Code3, Msg3)
    end,
    %% 3) target 锁内裁决：active + 非 owner + Human
    TargetMember =
        case organization_owner_store:lock_member_with_account_tx(Conn, OrgId, TargetUid) of
            {ok, Row2} -> Row2;
            {error, not_found} -> abort(409, <<"该用户不是组织成员或已被移除"/utf8>>);
            {error, Reason4} -> throw({abort_tx, {internal, Reason4}})
        end,
    case organization_owner_invariant:validate_target_membership(TargetMember) of
        ok -> ok;
        {error, {Code5, Msg5}} -> abort(Code5, Msg5)
    end,
    %% 4) 降旧 owner（提交时 deferred guard 以 owner_id 最终值放行）
    case organization_owner_store:demote_previous_owner_tx(Conn, OrgId, ActorUid) of
        ok -> ok;
        {error, Reason6} -> throw({abort_tx, {internal, Reason6}})
    end,
    %% 5) 升新 owner（此刻唯一索引下 0 -> 1 条 active owner）
    case organization_owner_store:promote_target_tx(Conn, OrgId, TargetUid) of
        ok -> ok;
        {error, Reason7} -> throw({abort_tx, {internal, Reason7}})
    end,
    %% 6) owner_id 兼容投影（sync 触发器幂等 upsert；提交时双侧 invariant 终检）
    case organization_owner_store:update_owner_projection_tx(Conn, OrgId, TargetUid) of
        {ok, _} ->
            {ok, #{
                organization_id => OrgId,
                owner_id => TargetUid,
                previous_owner_id => ActorUid,
                previous_owner_role => <<"admin">>
            }};
        {error, Reason8} ->
            throw({abort_tx, {internal, Reason8}})
    end.

-spec owner_transfer_internal_error(term()) -> binary().
owner_transfer_internal_error(_Reason) ->
    <<"Owner 转移失败，请稍后重试"/utf8>>.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
