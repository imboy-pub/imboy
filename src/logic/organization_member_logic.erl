-module(organization_member_logic).

%% Organization 治理成员。该关系不派生 Workspace 或 Group 成员资格。
%%
%% `suspend/3` 与 `remove/3` 的**依赖资源守卫**（EB-08 租约内的精确 removal/suspend
%% 段）：两者都是 `organization_member` 这一张 Core 表的**通用**状态迁移——
%%   * `suspend/3` 把 active 成员置为 suspended（可恢复的撤权第一步，不删个人账号）；
%%   * `remove/3` 在数据库守卫（同语句 BEFORE 触发器）拒绝「仍被依赖资源引用」的
%%     移除时，把该拒绝**翻译**成 409（`dependent_resources_conflict/1`）。
%% 本模块不引用任何纵切单元模块（`*_feature` / `customer_service_*` 一律不出现）：
%% 反向依赖由 `make arch-check` 的铁律 5 与 `organization_member_logic_tests` 的
%% A06 静态判定共同看守。

-export([
    list/4,
    workspaces/3,
    invite/4,
    change_role/4,
    remove/3,
    transfer_owner/3,
    suspend/3,
    restore/3,
    dependent_resources_conflict/1
]).

-include("log.hrl").

-spec list(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
list(Uid, OrgId, Page0, Size0) when
    is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0
->
    case organization_member_repo:find_active(OrgId, Uid, <<"role">>) of
        {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
            Page = max(1, Page0),
            Size = max(1, min(100, Size0)),
            case
                organization_member_repo:page_by_organization(
                    OrgId,
                    Page,
                    Size,
                    <<"om.organization_id,om.user_id,om.role,om.invited_by,",
                        "om.joined_at,om.status,u.nickname,u.avatar,u.account">>
                )
            of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    ?ERROR_LOG([organization_member_page_failed, OrgId, Reason]),
                    internal_error(<<"查询失败，请稍后重试"/utf8>>)
            end;
        {ok, _} ->
            forbidden();
        {error, not_found} ->
            forbidden();
        {error, Reason} ->
            ?ERROR_LOG([organization_member_acl_failed, OrgId, Uid, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end;
list(_, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% @doc 某成员在本 Organization 内的**有权 Workspace**（计划 §5.2：通讯录的
%% 成员详情必须含有权 Workspace 信息，且**所有企业成员可见**）。
%%
%% 权限面刻意不同于 `list/4`（治理门 owner/admin）：本函数只要求调用者是
%% **本 Org 的 active 成员**（任意角色）——成员详情是全员可见能力，不能
%% 挂在治理端点上（否则普通成员看到的永远是 403）。
%% 目标也必须是本 Org 的 active 成员：非成员一律 404（不泄露外部用户是否
%% 在本企业内有工作区）。
%% 跨 Org 的 Workspace 授权不算数（回答的是"在本企业内有权的工作区"）。
-spec workspaces(integer(), integer(), integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
workspaces(CallerUid, OrgId, TargetUid) when
    is_integer(CallerUid),
    CallerUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_integer(TargetUid),
    TargetUid > 0
->
    case organization_member_repo:find_active(OrgId, CallerUid, <<"role">>) of
        {ok, _Caller} ->
            case organization_member_repo:find_active(OrgId, TargetUid, <<"user_id">>) of
                {ok, _Target} ->
                    member_workspaces(OrgId, TargetUid);
                {error, not_found} ->
                    {error, {404, <<"该用户不是本 Organization 成员"/utf8>>}};
                {error, Reason} ->
                    ?ERROR_LOG([organization_member_workspaces_acl_failed, OrgId, Reason]),
                    internal_error(<<"查询失败，请稍后重试"/utf8>>)
            end;
        {error, not_found} ->
            %% 非本 Org 成员与"Org 不存在"同口径，不泄露组织存在性
            {error, {403, <<"仅本 Organization 成员可查看成员详情"/utf8>>}};
        {error, Reason} ->
            ?ERROR_LOG([organization_member_workspaces_acl_failed, OrgId, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end;
workspaces(_, _, _) ->
    {error, {400, <<"organization_id 与 user_id 必须是正整数"/utf8>>}}.

-spec member_workspaces(integer(), integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
member_workspaces(OrgId, TargetUid) ->
    case organization_member_repo:member_workspaces(OrgId, [TargetUid]) of
        {ok, Grouped} ->
            {ok, maps:get(TargetUid, Grouped, [])};
        {error, Reason} ->
            ?ERROR_LOG([organization_member_workspaces_failed, OrgId, Reason]),
            internal_error(<<"查询失败，请稍后重试"/utf8>>)
    end.

-spec invite(integer(), integer(), integer(), binary()) ->
    {ok, changed | unchanged, map()} | {error, {integer(), binary()}}.
invite(Uid, OrgId, TargetUid, Role) ->
    case valid_managed_role(Role) of
        false ->
            {error, {400, <<"角色仅支持 admin/member"/utf8>>}};
        true when not is_integer(TargetUid); TargetUid =< 0 ->
            {error, {400, <<"user_id 必须是正整数"/utf8>>}};
        true ->
            invite_registered_user(Uid, OrgId, TargetUid, Role)
    end.

-spec change_role(integer(), integer(), integer(), binary()) ->
    {ok, changed | unchanged, map()} | {error, {integer(), binary()}}.
change_role(Uid, OrgId, TargetUid, Role) ->
    case valid_managed_role(Role) of
        false ->
            {error, {400, <<"角色仅支持 admin/member"/utf8>>}};
        true when not is_integer(TargetUid); TargetUid =< 0 ->
            {error, {400, <<"user_id 必须是正整数"/utf8>>}};
        true ->
            Result = write_tx(
                Uid,
                OrgId,
                fun(Conn, Org, _ActorRole) ->
                    ensure_primary_owner(Uid, Org),
                    change_role_tx(Conn, OrgId, TargetUid, Role)
                end,
                <<"更新失败，请稍后重试"/utf8>>
            ),
            case Result of
                {ok, changed, _} ->
                    ?INFO_LOG([organization_member_role_changed, OrgId, Uid, TargetUid, Role]);
                _ ->
                    ok
            end,
            Result
    end.

-spec remove(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
remove(Uid, OrgId, TargetUid) when is_integer(TargetUid), TargetUid > 0 ->
    Result = write_tx(
        Uid,
        OrgId,
        fun(Conn, Org, _ActorRole) -> remove_tx(Conn, Uid, Org, OrgId, TargetUid) end,
        <<"移除失败，请稍后重试"/utf8>>
    ),
    case Result of
        {ok, _} -> ?INFO_LOG([organization_member_removed, OrgId, Uid, TargetUid]);
        _ -> ok
    end,
    Result;
remove(_, _, _) ->
    {error, {400, <<"user_id 必须是正整数"/utf8>>}}.

-spec transfer_owner(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
transfer_owner(Uid, OrgId, TargetUid) when
    is_integer(Uid),
    Uid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_integer(TargetUid),
    TargetUid > 0
->
    case TargetUid =:= Uid of
        true ->
            {error, {400, <<"新主 Owner 不能是当前主 Owner"/utf8>>}};
        false ->
            transfer_owner_validated(Uid, OrgId, TargetUid)
    end;
transfer_owner(_, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

%% Owner Source-of-Truth 迁移（Enterprise Organization V1 / ORG-01）：
%% transfer command 下沉到 src/lib/organization/application/organization_owner_transfer
%% （单事务、组织行锁、先降旧 → 再升新 → 最后改 owner_id 投影，提交时双侧
%% deferred invariant 终检）。本函数仅保留 legacy 入口兼容：日志与错误归口不变。
transfer_owner_validated(Uid, OrgId, TargetUid) ->
    case organization_owner_transfer:transfer(Uid, OrgId, TargetUid) of
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            case Code of
                500 ->
                    ?ERROR_LOG([organization_owner_transfer_failed, OrgId, Uid, TargetUid]),
                    {error, {Code, Msg}};
                _ ->
                    {error, {Code, Msg}}
            end;
        {ok, Result} when is_map(Result) ->
            ?INFO_LOG([organization_owner_transferred, OrgId, Uid, TargetUid]),
            {ok, Result}
    end.

invite_registered_user(Uid, OrgId, TargetUid, Role) ->
    case user_repo:find_by_id(TargetUid, <<"id">>) of
        User when is_map(User), map_size(User) > 0 ->
            case user_denylist_logic:blocked_between(Uid, TargetUid) of
                true ->
                    {error, {403, <<"存在拉黑关系，无法邀请该用户"/utf8>>}};
                false ->
                    invite_tx(Uid, OrgId, TargetUid, Role)
            end;
        _ ->
            {error, {404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>}}
    end.

invite_tx(Uid, OrgId, TargetUid, Role) ->
    Result = write_tx(
        Uid,
        OrgId,
        fun(Conn, Org, _ActorRole) ->
            ensure_role_grant_allowed(Uid, Org, Role),
            case
                organization_member_repo:upsert_active_tx(
                    Conn, OrgId, TargetUid, Role, Uid
                )
            of
                {ok, Status, _} when Status =:= changed; Status =:= unchanged ->
                    {ok, Member} = organization_member_repo:find_active_tx(
                        Conn,
                        OrgId,
                        TargetUid,
                        <<"organization_id,user_id,role,invited_by,joined_at,status">>
                    ),
                    {ok, Status, Member};
                {ok, role_conflict, _} ->
                    abort(409, <<"该用户已是组织成员，角色不同；请使用角色调整接口"/utf8>>);
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
        end,
        <<"邀请失败，请稍后重试"/utf8>>
    ),
    case Result of
        {ok, Status, _} ->
            ?INFO_LOG([organization_member_invited, OrgId, Uid, TargetUid, Role, Status]);
        _ ->
            ok
    end,
    Result.

change_role_tx(Conn, OrgId, TargetUid, Role) ->
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := <<"owner">>}} ->
            abort(409, <<"主 Owner 不能通过成员角色接口修改，请使用 Owner 转移流程"/utf8>>);
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} ->
            {ok, unchanged, member_result(OrgId, TargetUid, Role, <<"active">>)};
        {ok, #{<<"status">> := <<"active">>}} ->
            case organization_member_repo:update_role_tx(Conn, OrgId, TargetUid, Role) of
                ok ->
                    {ok, changed, member_result(OrgId, TargetUid, Role, <<"active">>)};
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end;
        {ok, _} ->
            member_not_active();
        {error, not_found} ->
            member_not_active();
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

%% 可被移除的在册状态：active（直接移除）与 suspended（EB-08 的两步离场：
%% 先撤权再移除）。两者都必须先过数据库守卫（active 经办关系存在即 23514）。
%% removed / 非成员不在其列 —— 仍然 409，fail-closed 不变。
-define(REMOVABLE_SOURCE(Status), (Status =:= <<"active">> orelse Status =:= <<"suspended">>)).

remove_tx(Conn, Uid, Org, OrgId, TargetUid) ->
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := Status} = Member} when ?REMOVABLE_SOURCE(Status) ->
            remove_by_role_tx(Conn, Uid, Org, OrgId, TargetUid, Status, Member);
        {ok, _} ->
            member_not_active();
        {error, not_found} ->
            member_not_active();
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

%% 角色规则逐条保持原样（主 Owner 一律 409；Admin 需主 Owner 执行；其余直接移除）。
remove_by_role_tx(Conn, Uid, Org, OrgId, TargetUid, Status, Member) ->
    case maps:get(<<"role">>, Member, undefined) of
        <<"owner">> ->
            abort(409, <<"主 Owner 不能被移除，请先转移 Owner"/utf8>>);
        <<"admin">> ->
            ensure_primary_owner(Uid, Org),
            remove_active_tx(Conn, OrgId, TargetUid, Status);
        _MemberLike ->
            remove_active_tx(Conn, OrgId, TargetUid, Status)
    end.

remove_active_tx(Conn, OrgId, TargetUid, <<"active">>) ->
    remove_row_tx(
        Conn, OrgId, TargetUid, organization_member_repo:remove_tx(Conn, OrgId, TargetUid)
    );
remove_active_tx(Conn, OrgId, TargetUid, _Suspended) ->
    remove_row_tx(Conn, OrgId, TargetUid, remove_member_row_tx(Conn, OrgId, TargetUid)).

remove_row_tx(_Conn, OrgId, TargetUid, Result) ->
    case Result of
        ok ->
            {ok, member_result(OrgId, TargetUid, undefined, <<"removed">>)};
        {error, Reason} ->
            case dependent_resources_conflict(Reason) of
                {conflict, Message} -> abort(409, Message);
                none -> throw({abort_tx, {internal, Reason}})
            end
    end.

%% 从**非 active**（即 suspended）出发的精确移除：唯一一条写语句，org 作用域显式
%% 贯穿，且只接受在册状态（active|suspended）——`removed` / 非成员的行不匹配 ⇒
%% 影响行数不为 1 即显式失败（并发重复移除不静默成功）。
%% 为什么需要它：`organization_member_repo:remove_tx/3` 的 WHERE 只匹配
%% `status = 'active'`，而 EB-08 的离场是两步（S1 suspend -> S3 removed）。
remove_member_row_tx(Conn, OrgId, Uid) ->
    Sql =
        <<
            "UPDATE organization_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status IN ('active','suspended')"
        >>,
    case elib_pg:execute(Conn, Sql, [OrgId, Uid]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, member_not_active};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 暂停（suspend）Org 成员的通用能力：立即撤权，但**不**删个人账号。
%%
%% 判定顺序（与 remove/3 同规，锁顺序也是「组织行先、成员行后」）：
%%   1. 参数形状；2. 组织存在且 active；3. 操作人必须是 Owner/Admin；
%%   4. 目标必须是 active 成员，且主 Owner 不可被暂停；5. 精确单列状态迁移。
%%
%% 语义要点：
%%   * suspended 是**可恢复**的撤权第一步（EB-D07）：不动 `workspace_member`、
%%     不动经办关系、不动个人账号；「企业业务授权立即失败」由授权路径逐请求读事实
%%     实现（本函数只负责把事实改成 suspended）。
%%   * `remove/3` 会因「仍被依赖资源引用」被数据库拒绝并映射为 409；suspend 不会
%%     （暂停不破坏任何引用）——两者语义由 `dependent_resources_conflict/1` 的
%%     窄映射保持一致：**只有真被依赖挡住时才说 409**。
-spec suspend(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
suspend(Uid, OrgId, TargetUid) when
    is_integer(OrgId), OrgId > 0, is_integer(TargetUid), TargetUid > 0
->
    Result = write_tx(
        Uid,
        OrgId,
        fun(Conn, _Org, _ActorRole) -> suspend_tx(Conn, OrgId, TargetUid) end,
        <<"暂停失败，请稍后重试"/utf8>>
    ),
    case Result of
        {ok, _} -> ?INFO_LOG([organization_member_suspended, OrgId, Uid, TargetUid]);
        _ -> ok
    end,
    Result;
suspend(_, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

%% @doc 恢复（restore）suspended 的 Org 成员：EB-D07「suspended 是可恢复的撤权
%% 第一步」的复位端。与 suspend/3 同规（锁顺序「组织行先、成员行后」）：
%%   1. 参数形状；2. 组织存在且 active；3. 操作人必须是 Owner/Admin；
%%   4. 目标必须是 suspended 成员；5. 精确单列状态迁移（suspended → active）。
%%
%% 语义要点：
%%   * 只接受 suspended 来源：active（无需恢复）与 removed（终态，恢复走重新
%%     邀请）都 409 明确拒绝——与 suspend 的「不静默成功」同一纪律；
%%   * 主 Owner 的成员行不可能处于 suspended（DB 守卫
%%     trg_organization_primary_owner_member_guard 23514），故无需 owner 特判。
-spec restore(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
restore(Uid, OrgId, TargetUid) when
    is_integer(OrgId), OrgId > 0, is_integer(TargetUid), TargetUid > 0
->
    Result = write_tx(
        Uid,
        OrgId,
        fun(Conn, _Org, _ActorRole) -> restore_tx(Conn, OrgId, TargetUid) end,
        <<"恢复失败，请稍后重试"/utf8>>
    ),
    case Result of
        {ok, _} -> ?INFO_LOG([organization_member_restored, OrgId, Uid, TargetUid]);
        _ -> ok
    end,
    Result;
restore(_, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

restore_tx(Conn, OrgId, TargetUid) ->
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := <<"suspended">>, <<"role">> := Role}} ->
            case
                set_member_status_tx(
                    Conn, OrgId, TargetUid, <<"suspended">>, <<"active">>
                )
            of
                ok ->
                    {ok, member_result(OrgId, TargetUid, Role, <<"active">>)};
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end;
        {ok, _NotSuspended} ->
            abort(409, <<"该成员不在暂停状态，无法恢复"/utf8>>);
        {error, not_found} ->
            member_not_active();
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

suspend_tx(Conn, OrgId, TargetUid) ->
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := <<"owner">>}} ->
            abort(409, <<"主 Owner 不能被暂停，请先转移 Owner"/utf8>>);
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} ->
            case set_member_status_tx(Conn, OrgId, TargetUid, <<"active">>, <<"suspended">>) of
                ok ->
                    {ok, member_result(OrgId, TargetUid, Role, <<"suspended">>)};
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end;
        {ok, _NotActive} ->
            member_not_active();
        {error, not_found} ->
            member_not_active();
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

%% 主 Owner 的成员行受数据库守卫保护（00000113 的 trg_organization_primary_owner_member_guard，
%% 任何 status <> 'active' 的变更都是 23514）；`suspend_tx/4` 的第一条子句先判
%% `role = owner` ⇒ 409，避免把 DB 的 23514 当成普通内部错误上报。
%% 精确的单列状态迁移（suspend: active→suspended；restore: suspended→active）：
%% org 作用域显式贯穿，且只接受仍处于来源态的行（并发重复迁移由该条件裁决 ——
%% 影响行数不为 1 即显式失败，不静默成功）。
set_member_status_tx(Conn, OrgId, Uid, FromStatus, Status) ->
    Sql =
        <<
            "UPDATE organization_member SET status = $4, updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status = $3"
        >>,
    case elib_pg:execute(Conn, Sql, [OrgId, Uid, FromStatus, Status]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, member_not_active};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 把「仍被依赖资源引用」的数据库拒绝映射成 409（**通用**、窄口径）。
%%
%% 依赖关系由数据库在**同一语句内**裁决（BEFORE 触发器），本函数只做错误翻译：
%%   * 只有 SQLSTATE `23514`（check violation）**且**约束名在 Core 的依赖守卫表里
%%     才映射；其它约束、其它错误码、非数据库错误一律 `none`（不把失败都说成 409）；
%%   * 表里只有**约束名**（数据库对象名），没有任何纵切单元模块名——依赖方向保持不变。
%%
%% 返回 `{conflict, Message}` 供调用方 `abort(409, Message)`；`none` 表示按原错误上报。
-spec dependent_resources_conflict(term()) -> {conflict, binary()} | none.
dependent_resources_conflict(Reason) ->
    case {sqlstate(Reason), constraint_name(Reason)} of
        {<<"23514">>, Constraint} when is_binary(Constraint) ->
            case dependent_resource_guard(Constraint) of
                {ok, Resource} -> {conflict, dependent_resources_message(Resource)};
                error -> none
            end;
        _NotADependencyRejection ->
            none
    end.

%% Core 侧的「被依赖资源挡住」守卫表：约束名 → 依赖资源类别。
%% 新守卫只在这里加一行（判定因此是「表驱动」而不是给某一个用例写的特例）。
dependent_resource_guard(<<"trg_organization_member_offboarding_guard">>) ->
    {ok, <<"active 经办关系"/utf8>>};
dependent_resource_guard(_Other) ->
    error.

dependent_resources_message(Resource) ->
    iolist_to_binary([
        <<"该成员仍被依赖资源引用（"/utf8>>,
        Resource,
        <<"），直接移除被拒绝：请先完成交接"/utf8>>
    ]).

%% epgsql 的错误项形态：#error{code, extra}。为避免在 Core 引入驱动头文件依赖，
%% 这里按记录形状宽松提取（`{error, Severity, Code, Codename, Message, Extra}`），
%% 非该形状一律 `undefined`（调用方据此走原有错误分支）。
sqlstate({error, _Severity, Code, _Codename, _Message, _Extra}) when is_binary(Code) ->
    Code;
sqlstate(_Other) ->
    undefined.

constraint_name({error, _Severity, _Code, _Codename, _Message, Extra}) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} when is_binary(Name) -> Name;
        _ -> undefined
    end;
constraint_name(_Other) ->
    undefined.

%% 组织行先锁、成员行后锁，所有治理写保持同一锁顺序。
write_tx(Uid, OrgId, Fun, ErrorMsg) when is_integer(Uid), Uid > 0, is_integer(OrgId), OrgId > 0 ->
    Tx = fun(Conn) ->
        Org =
            case
                organization_member_repo:find_organization_for_share_tx(
                    Conn, OrgId, <<"id,owner_id,status">>
                )
            of
                {ok, #{<<"status">> := <<"active">>} = Row} -> Row;
                {ok, _Archived} -> abort(409, <<"组织已归档，成员管理操作被拒绝"/utf8>>);
                {error, not_found} -> abort(404, <<"组织不存在"/utf8>>);
                {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
            end,
        ActorRole =
            case
                organization_member_repo:find_active_for_share_tx(
                    Conn, OrgId, Uid, <<"role">>
                )
            of
                {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
                    Role;
                {ok, _} ->
                    abort(403, <<"仅 Organization Owner 或 Admin 可执行此操作"/utf8>>);
                {error, not_found} ->
                    abort(403, <<"仅 Organization Owner 或 Admin 可执行此操作"/utf8>>);
                {error, Reason2} ->
                    throw({abort_tx, {internal, Reason2}})
            end,
        Fun(Conn, Org, ActorRole)
    end,
    case elib_pg:with_tx(Tx) of
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, {internal, Reason}} ->
            ?ERROR_LOG([organization_member_write_failed, OrgId, Uid, Reason]),
            internal_error(ErrorMsg);
        {error, Reason} ->
            ?ERROR_LOG([organization_member_write_failed, OrgId, Uid, Reason]),
            internal_error(ErrorMsg);
        Result ->
            Result
    end;
write_tx(_, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

ensure_role_grant_allowed(Uid, Org, <<"admin">>) ->
    ensure_primary_owner(Uid, Org);
ensure_role_grant_allowed(_, _, <<"member">>) ->
    ok.

ensure_primary_owner(Uid, #{<<"owner_id">> := Uid}) ->
    ok;
ensure_primary_owner(_, _) ->
    abort(403, <<"仅主 Owner 可管理 Admin 角色"/utf8>>).

valid_managed_role(<<"admin">>) -> true;
valid_managed_role(<<"member">>) -> true;
valid_managed_role(_) -> false.

member_result(OrgId, Uid, undefined, Status) ->
    #{organization_id => OrgId, user_id => Uid, status => Status};
member_result(OrgId, Uid, Role, Status) ->
    #{organization_id => OrgId, user_id => Uid, role => Role, status => Status}.

forbidden() ->
    {error, {403, <<"仅 Organization Owner 或 Admin 可查看成员列表"/utf8>>}}.

member_not_active() ->
    abort(409, <<"该用户不是组织成员或已被移除"/utf8>>).

abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).

internal_error(Msg) ->
    {error, {500, Msg}}.
