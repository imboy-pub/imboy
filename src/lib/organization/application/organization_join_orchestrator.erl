-module(organization_join_orchestrator).

%%% @doc Organization 加入编排（GZAPP-01 核心）。
%%%
%%% 统一入口 `join_tx/4`：同一事务内完成「org 成员 → 默认 Workspace 成员 →
%%% 全员群(General) → 公告频道(Announcements)」四级落地，被两个上游共享：
%%%
%%%   1. `organization_invitation_app:accept/4` 的 membership_hook
%%%      （organization_api_handler 注入，invitation 定向邀请接受链）；
%%%   2. `organization_invite_code_app:join_by_code/3`（org 邀请码加入链）。
%%%
%%% 编排语义（与广州企业 APP 闭环 V1 冻结）：
%%%   * 同事务 upsert organization_member(role=member)——幂等：active 同角色
%%%     unchanged；active 异角色 role_conflict 409（不降级既有角色）；
%%%   * `organization_default_workspace_pg:find_tx/2` 读显式默认 WS
%%%     （C05：不回落 min-ID 推导）；**默认 WS 缺失时只写 org member
%%%     （不阻塞，workspace_id => none）**；
%%%   * 默认 WS 存在：workspace_guard 归档守卫（FOR UPDATE 行锁，archived →
%%%     稳定 980 整体回滚）→ upsert workspace_member(role=member) →
%%%     自动加入该 WS 全员群（General，scope=workspace 且 status=1 的最老群，
%%%     与 workspace_ds:create_template 建群特征一致）→ 订阅公告频道
%%%     （Announcements，scope=workspace 且 status=1 的最老频道，channel_subscription
%%%     upsert）。群/频道行缺失（历史数据异常）跳过该级，不阻塞整体。
%%%   * 幂等可重放：所有写均为 upsert 语义，重复调用收敛 unchanged，
%%%     不产生第二次副作用（群历史世代等由 group_member_ds:join_group
%%%     的 ON CONFLICT 语义兜底）。
%%%   * 锁序：组织行（FOR SHARE）→ workspace 行（FOR UPDATE）→ 成员行，
%%%     与既有治理写「组织行先、成员/资源行后」一致；群加入的
%%%     Group Member ⊆ Workspace Member 校验依赖先写 workspace_member
%%%     （同事务可见）。

-export([join_tx/4, membership_hook/2]).

-include("log.hrl").

%% ===================================================================
%% 统一加入编排（事务体；由上游 with_tx / hook 传入同一 Conn）
%% ===================================================================

%% @doc 同事务加入编排。Outcome = joined | unchanged（org member 维度）。
%% Summary = #{organization_id, workspace_id, group_id, channel_id}，
%% 默认 WS 缺失时三项均为 none。失败 throw({abort_tx, Reason}) 由
%% elib_pg:with_tx 归一回滚（上游归一为 {error, Reason}）。
-spec join_tx(any(), integer(), integer(), integer() | null) ->
    {ok, joined | unchanged, map()}.
join_tx(Conn, OrgId, Uid, InvitedBy) ->
    %% 1) 锁组织行 + 生命周期裁决（C16：archived 拒绝加入，稳定 409）
    ok = ensure_org_active_tx(Conn, OrgId),
    %% 2) org member upsert（幂等；role=member，不触碰治理角色）
    {ok, Outcome} = ensure_org_member_tx(Conn, OrgId, Uid, InvitedBy),
    %% 3) 默认 WS（显式关系唯一真源；缺失只写 org member 不阻塞）
    case organization_default_workspace_pg:find_tx(Conn, OrgId) of
        {error, not_found} ->
            {ok, Outcome, #{
                organization_id => OrgId,
                workspace_id => none,
                group_id => none,
                channel_id => none
            }};
        {error, Reason} ->
            throw({abort_tx, {internal, {default_workspace_lookup, Reason}}});
        {ok, WsId} ->
            join_workspace_tx(Conn, OrgId, Uid, InvitedBy, WsId, Outcome)
    end.

%% ===================================================================
%% invitation accept 挂点（C11 membership_hook 的编排化实现）
%% ===================================================================

%% @doc organization_invitation_app:accept 的 membership_hook 适配器：
%% 从消费后的邀请行取作用域三要素，转入统一编排。失败 {error, Reason}
%% → accept 事务整体回滚（消费 + 成员变更原子）。
-spec membership_hook(any(), map()) -> ok | {error, term()}.
membership_hook(Conn, Row) ->
    OrgId = positive(maps:get(<<"organization_id">>, Row, undefined)),
    TargetUid = positive(maps:get(<<"target_user_id">>, Row, undefined)),
    InvitedBy = nullable(maps:get(<<"invited_by">>, Row, undefined)),
    try join_tx(Conn, OrgId, TargetUid, InvitedBy) of
        {ok, _Outcome, _Summary} -> ok
    catch
        %% workspace_logic/invitation_app 同口径：业务 abort 直接透传错误值
        throw:{abort_tx, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 组织行 FOR SHARE：锁序起点；archived 409 / 不存在 404。
-spec ensure_org_active_tx(any(), integer()) -> ok.
ensure_org_active_tx(Conn, OrgId) ->
    case
        organization_member_repo:find_organization_for_share_tx(
            Conn, OrgId, <<"id,owner_id,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>}} ->
            ok;
        {ok, _Archived} ->
            abort(409, <<"Organization 已归档，不能加入"/utf8>>);
        {error, not_found} ->
            abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, {organization_lookup, Reason}}})
    end.

%% org member upsert：active 同角色 unchanged / 异角色 409 / 其余激活或新建。
-spec ensure_org_member_tx(any(), integer(), integer(), integer() | null) ->
    {ok, joined | unchanged}.
ensure_org_member_tx(Conn, OrgId, Uid, InvitedBy) ->
    case organization_member_repo:upsert_active_tx(Conn, OrgId, Uid, <<"member">>, InvitedBy) of
        {ok, changed, _} ->
            _ = ?INFO_LOG([organization_join_orchestrator_member, OrgId, Uid, changed]),
            {ok, joined};
        {ok, unchanged, _} ->
            {ok, unchanged};
        {ok, role_conflict, _} ->
            abort(409, <<"该用户已是组织成员，角色不同"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, {organization_member_upsert, Reason}}})
    end.

%% 默认 WS 存在时的三级落地：ws member → 全员群 → 公告频道。
-spec join_workspace_tx(
    any(),
    integer(),
    integer(),
    integer() | null,
    integer(),
    joined | unchanged
) ->
    {ok, joined | unchanged, map()}.
join_workspace_tx(Conn, OrgId, Uid, InvitedBy, WsId, Outcome) ->
    %% 归档守卫（FOR UPDATE 行锁；archived → 稳定 980，整体回滚——
    %% org member 也不落，保持「要么全加入、要么都没加」原子性）
    ok = workspace_guard:abort_on_error(
        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
    ),
    %% workspace member upsert（幂等；必须在 join_group 之前——
    %% Group Member ⊆ Workspace Member 同事务校验依赖此行可见）
    case workspace_member_repo:upsert_active_tx(Conn, WsId, Uid, <<"member">>, InvitedBy) of
        {ok, changed, _} -> ok;
        {ok, unchanged, _} -> ok;
        {ok, role_conflict, _} -> abort(409, <<"该用户已是工作区成员，角色不同"/utf8>>);
        {error, Reason} -> throw({abort_tx, {internal, {workspace_member_upsert, Reason}}})
    end,
    %% 全员群（General）：scope=workspace 且 status=1 的最老群
    Gid = default_workspace_group_tx(Conn, WsId),
    ok = join_general_tx(Conn, Uid, Gid),
    %% 公告频道（Announcements）：scope=workspace 且 status=1 的最老频道
    CId = default_workspace_channel_tx(Conn, WsId),
    ok = subscribe_announcements_tx(Conn, Uid, CId),
    _ = ?INFO_LOG([organization_join_orchestrator_done, OrgId, WsId, Uid, Gid, CId]),
    {ok, Outcome, #{
        organization_id => OrgId,
        workspace_id => WsId,
        group_id => Gid,
        channel_id => CId
    }}.

%% 全员群加入：行缺失（历史数据异常）跳过；join_group 自身幂等
%% （upsert_active false → {ok, 0}，不重开群历史世代）。
-spec join_general_tx(any(), integer(), integer() | none) -> ok.
join_general_tx(_Conn, _Uid, none) ->
    ok;
join_general_tx(Conn, Uid, Gid) ->
    case group_member_ds:join_group(Conn, <<"org_join">>, Uid, Gid, #{role => 1}) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({abort_tx, {internal, {general_group_join, Gid, Reason}}})
    end.

%% 公告频道订阅：行缺失跳过；upsert 幂等（已订阅 → {ok, false}）。
-spec subscribe_announcements_tx(any(), integer(), integer() | none) -> ok.
subscribe_announcements_tx(_Conn, _Uid, none) ->
    ok;
subscribe_announcements_tx(Conn, Uid, CId) ->
    case channel_subscription_repo:upsert_active(Conn, CId, Uid) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({abort_tx, {internal, {announcements_subscribe, CId, Reason}}})
    end.

%% 默认全员群查找（与 workspace_ds:create_template 建群特征一致：
%% scope='workspace' AND status=1，最老者；无行 → none）。
-spec default_workspace_group_tx(any(), integer()) -> integer() | none.
default_workspace_group_tx(Conn, WsId) ->
    Sql =
        <<"SELECT id FROM \"group\" WHERE workspace_id = $1 AND scope = 'workspace'",
            " AND status = 1 ORDER BY created_at ASC, id ASC LIMIT 1">>,
    one_id_tx(Conn, Sql, [WsId]).

%% 默认公告频道查找（同上口径，channel 表）。
-spec default_workspace_channel_tx(any(), integer()) -> integer() | none.
default_workspace_channel_tx(Conn, WsId) ->
    Sql =
        <<"SELECT id FROM channel WHERE workspace_id = $1 AND scope = 'workspace'",
            " AND status = 1 ORDER BY created_at ASC, id ASC LIMIT 1">>,
    one_id_tx(Conn, Sql, [WsId]).

-spec one_id_tx(any(), binary(), list()) -> integer() | none.
one_id_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"id">> := Id} | _]} -> Id;
        _ -> none
    end.

-spec positive(term()) -> integer().
positive(Value) ->
    case elib_cnv:safe_to_integer(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> 0
    end.

-spec nullable(term()) -> integer() | null.
nullable(Value) when is_integer(Value), Value > 0 ->
    Value;
nullable(_) ->
    null.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
