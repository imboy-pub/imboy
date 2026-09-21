-module(organization_invite_code_app).

%%% @doc Organization Invite Code 应用层命令（GZAPP-01）。
%%%
%%% 镜像 workspace 团队码（workspace_logic.erl T2.3 / 迁移 00000082）的
%%% org 侧版本，差异点：
%%%   * 治理门复用 organization_member_logic:write_tx 同款语义
%%%     （组织行 FOR SHARE 先锁、成员行 FOR SHARE 后锁；仅 owner/admin），
%%%     不重复实现授权原语；
%%%   * join 不直接写 member，而是统一走 organization_join_orchestrator
%%%     （org member → 默认 WS → 全员群 → 公告频道 同事务编排）；
%%%   * 错误口径：981 码无效/已失效（含跨 Org 输码——同语句 org 作用域
%%%     命中不了行，不泄露组织存在性）、982 已过期、980 默认 WS 已归档、
%%%     403 非治理建码、409 org 已归档、404 org 不存在。

-export([
    create/3,
    revoke/2,
    get/2,
    join_by_code/3
]).

-include("error_code.hrl").
-include("log.hrl").

-define(CODE_TTL_SECONDS, 7 * 24 * 60 * 60).
-define(CODE_RETRY_LIMIT, 3).

%% ===================================================================
%% create —— 生成组织邀请码（owner/admin；重新生成=旧码失效）
%% ===================================================================

%% @doc 生成组织邀请码（仅组织 Owner/Admin；org archived 409；7 天有效）。
%% 同事务先撤销本组织既有 active 码（一组织至多一个 active 码；重新
%% 生成即旧码失效）；code 全局唯一冲突（23505→code_conflict）时换码
%% 重开事务重试 ≤3 次（镜像 workspace_logic:insert_invite_code/4）。
%% 返回 {ok, #{code, expires_at}}（expires_at epoch 秒）。
-spec create(integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
create(ActorUid, OrgId, _Opts) when
    is_integer(ActorUid), ActorUid > 0, is_integer(OrgId), OrgId > 0
->
    ExpiresAt = os:system_time(second) + ?CODE_TTL_SECONDS,
    case insert_code_tx(ActorUid, OrgId, ExpiresAt, ?CODE_RETRY_LIMIT) of
        {ok, Row} ->
            _ = ?INFO_LOG([organization_invite_code_created, OrgId, ActorUid]),
            {ok, view(Row)};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_invite_code_create_failed, OrgId, ActorUid, Reason]),
            {error, {500, <<"生成组织邀请码失败，请稍后重试"/utf8>>}}
    end;
create(_, _, _) ->
    {error, {400, <<"organization_id 与 user_id 必须是正整数"/utf8>>}}.

%% 治理门 + 撤旧 + 插入（单事务）；code_conflict 由外层换码重试。
-spec insert_code_tx(integer(), integer(), integer(), non_neg_integer()) ->
    {ok, map()} | {error, term()}.
insert_code_tx(_ActorUid, _OrgId, _ExpiresAt, 0) ->
    {error, code_retry_exhausted};
insert_code_tx(ActorUid, OrgId, ExpiresAt, Left) ->
    Code = organization_invite_code_pg:generate_code(),
    Tx = fun(Conn) ->
        %% 治理门（组织行先锁 → actor owner/admin；archived 409）
        ok = ensure_governance_tx(Conn, OrgId, ActorUid, reject_archived),
        %% 撤旧是「重新生成=旧码失效」的语义前提，失败即中止整个事务
        case organization_invite_code_pg:revoke_active_by_org_tx(Conn, OrgId) of
            {ok, _} ->
                organization_invite_code_pg:add_tx(Conn, OrgId, Code, ActorUid, ExpiresAt);
            {error, Reason} ->
                throw({abort_tx, {internal, {revoke_old_code, Reason}}})
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Row} ->
            {ok, Row};
        {error, code_conflict} ->
            insert_code_tx(ActorUid, OrgId, ExpiresAt, Left - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% revoke —— 撤销（owner/admin；幂等；归档 org 允许撤销）
%% ===================================================================

%% @doc 撤销本组织全部有效邀请码（幂等：无 active 码 → revoked 0）。
%% 撤销后输码即 981；归档组织允许撤销（收紧操作，不设 409 门——
%% 镜像 workspace_logic:revoke_invite_code/2 对归档工作区的口径）。
-spec revoke(integer(), integer()) ->
    {ok, #{revoked := non_neg_integer()}} | {error, {integer(), binary()}}.
revoke(ActorUid, OrgId) when
    is_integer(ActorUid), ActorUid > 0, is_integer(OrgId), OrgId > 0
->
    Tx = fun(Conn) ->
        ok = ensure_governance_tx(Conn, OrgId, ActorUid, allow_archived),
        organization_invite_code_pg:revoke_active_by_org_tx(Conn, OrgId)
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Count} ->
            _ = ?INFO_LOG([organization_invite_code_revoked, OrgId, ActorUid, Count]),
            {ok, #{revoked => Count}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_invite_code_revoke_failed, OrgId, ActorUid, Reason]),
            {error, {500, <<"撤销组织邀请码失败，请稍后重试"/utf8>>}}
    end;
revoke(_, _) ->
    {error, {400, <<"organization_id 与 user_id 必须是正整数"/utf8>>}}.

%% ===================================================================
%% get —— 治理面读当前 active 码（owner/admin）
%% ===================================================================

%% @doc 读取本组织当前 active 邀请码；无 active 码 → {error, not_found}
%% （handler 映射为 code => null 的成功信封）。
-spec get(integer(), integer()) ->
    {ok, map()} | {error, not_found | {integer(), binary()}}.
get(ActorUid, OrgId) when
    is_integer(ActorUid), ActorUid > 0, is_integer(OrgId), OrgId > 0
->
    Tx = fun(Conn) ->
        ok = ensure_governance_tx(Conn, OrgId, ActorUid, reject_archived),
        organization_invite_code_pg:find_active_by_org_tx(Conn, OrgId)
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Row} ->
            {ok, view(Row)};
        {error, not_found} ->
            {error, not_found};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_invite_code_get_failed, OrgId, ActorUid, Reason]),
            {error, {500, <<"读取组织邀请码失败，请稍后重试"/utf8>>}}
    end;
get(_, _) ->
    {error, {400, <<"organization_id 与 user_id 必须是正整数"/utf8>>}}.

%% ===================================================================
%% join_by_code —— 凭码加入（任意登录用户；统一编排入口）
%% ===================================================================

%% @doc 凭码加入组织（任意登录用户）：码校验 981/982 → 统一加入编排
%% （organization_join_orchestrator:join_tx/4：org member → 默认 WS →
%% 全员群 → 公告频道，同事务、幂等可重放）。
%% 非字符串/空/跨 Org 码统一 981（不泄露组织存在性）。
-spec join_by_code(integer(), integer(), binary()) ->
    {ok, joined | unchanged, map()} | {error, {integer(), binary()}}.
join_by_code(Uid, OrgId, Code0) when
    is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0
->
    %% 非 binary（JSON number/array 等）或 trim 后为空统一按无效码 981
    %% （显式短路，省一次 DB roundtrip；真实 SQL 同口径查不到行）
    Code =
        case is_binary(Code0) of
            true -> string:uppercase(string:trim(Code0));
            false -> <<>>
        end,
    Tx = fun(Conn) ->
        case Code of
            <<>> ->
                abort(?ERR_WORKSPACE_INVITE_INVALID, <<"组织邀请码无效或已失效"/utf8>>);
            _ ->
                lookup_and_join_tx(Conn, OrgId, Uid, Code)
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Outcome, Summary} ->
            {ok, Outcome, Summary};
        {error, {Code2, Msg}} when is_integer(Code2), is_binary(Msg) ->
            {error, {Code2, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_invite_code_join_failed, OrgId, Uid, Reason]),
            {error, {500, <<"加入组织失败，请稍后重试"/utf8>>}}
    end;
join_by_code(_, _, _) ->
    {error, {400, <<"organization_id、code 与用户标识必须是有效值"/utf8>>}}.

%% 码查找 → 过期裁决 → 统一编排（with_tx 事务体内部）。
-spec lookup_and_join_tx(any(), integer(), integer(), binary()) ->
    {ok, joined | unchanged, map()}.
lookup_and_join_tx(Conn, OrgId, Uid, Code) ->
    case organization_invite_code_pg:find_active_by_code_tx(Conn, OrgId, Code) of
        {error, not_found} ->
            abort(?ERR_WORKSPACE_INVITE_INVALID, <<"组织邀请码无效或已失效"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, {code_lookup, Reason}}});
        {ok, #{<<"expired">> := true}} ->
            abort(?ERR_WORKSPACE_INVITE_EXPIRED, <<"组织邀请码已过期"/utf8>>);
        {ok, Row} ->
            CreatedBy =
                case maps:get(<<"created_by">>, Row, null) of
                    By when is_integer(By) -> By;
                    _ -> null
                end,
            organization_join_orchestrator:join_tx(Conn, OrgId, Uid, CreatedBy)
    end.

%% ===================================================================
%% 内部
%% ===================================================================

%% 治理门（镜像 organization_member_logic:write_tx 的锁序与裁决：
%% 组织行 FOR SHARE 先、成员行 FOR SHARE 后；owner/admin 放行）。
%% ArchivedPolicy = reject_archived（create/get：C16 archived 禁新写）
%%              | allow_archived（revoke：收紧操作放行）。
-spec ensure_governance_tx(any(), integer(), integer(), reject_archived | allow_archived) ->
    ok.
ensure_governance_tx(Conn, OrgId, ActorUid, ArchivedPolicy) ->
    case
        organization_member_repo:find_organization_for_share_tx(
            Conn, OrgId, <<"id,owner_id,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>}} ->
            ok;
        {ok, _Archived} when ArchivedPolicy =:= allow_archived ->
            ok;
        {ok, _Archived} ->
            abort(409, <<"Organization 已归档，不能生成邀请码"/utf8>>);
        {error, not_found} ->
            abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, {organization_lookup, Reason}}})
    end,
    case organization_member_repo:find_active_for_share_tx(Conn, OrgId, ActorUid, <<"role">>) of
        {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
            ok;
        {ok, _} ->
            abort(403, <<"仅组织 Owner 或 Admin 可管理邀请码"/utf8>>);
        {error, not_found} ->
            abort(403, <<"仅组织 Owner 或 Admin 可管理邀请码"/utf8>>);
        {error, Reason2} ->
            throw({abort_tx, {internal, {actor_member_lookup, Reason2}}})
    end.

%% 响应投影：白名单键（含 expired 状态由读取时刻决定，仅治理面可见）。
-spec view(map()) -> map().
view(Row) ->
    #{
        organization_id => maps:get(<<"organization_id">>, Row, undefined),
        code => maps:get(<<"code">>, Row, undefined),
        status => maps:get(<<"status">>, Row, undefined),
        expires_at => maps:get(<<"expires_at">>, Row, undefined),
        created_at => maps:get(<<"created_at">>, Row, undefined)
    }.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
