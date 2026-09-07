-module(workspace_logic).
-compile([nowarn_deprecated_catch]).
%%%
% workspace_logic 工作区业务逻辑（双体验 v2.5.2 WP3/T4）
%
% 权限模型（计划 §1.4.2 三角色最小权限表，唯一授权真相）：
%   Owner  : 全部治理（成员增删/改角色/主 Owner 转移/Branding/改名改 logo）+ 读
%   Member : 读 + 创建 Workspace Channel/Group
%   Guest  : 只读（W0 控制面；不改变其在已加入 Group/Channel 中的既有能力）
%   非工作区成员：一切 Workspace 资源入口 403（稳定错误码）
%
% 错误约定：{error, {Code, Msg}}，Code 取 error_code.hrl 语义码（403/404/409/400），
% handler 层直接映射 envelope code。
%%%

-export([create/3]).
-export([detail/2]).
-export([mine/3]).
-export([update_profile/4]).
-export([read_branding/2]).
-export([update_branding/3]).
-export([overview/2]).
-export([invite/4]).
-export([generate_invite_code/2]).
-export([revoke_invite_code/2]).
-export([join_by_code/2]).
-export([remove_member/3]).
-export([change_role/4]).
-export([transfer_owner/3]).
-export([member_list/4]).
-export([archive/2]).
-export([restore/2]).
-export([my_role/2]).
-export([ensure_member/2]).
-export([ensure_can_create_resource/2]).
-export([valid_role/1]).
%% Admin 运营管理入口（双体验 v2.5.2 WP7/T11b；鉴权在 adm_workspace_handler 层
%% 走 adm_acl，此处不做 Owner 校验——运营归档/恢复是平台侧动作）
-export([admin_page/4]).
-export([admin_detail/1]).
-export([admin_member_page/3]).
-export([admin_archive/2]).
-export([admin_restore/2]).

-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建工作区（Template 原子初始化，I13）
-spec create(integer(), binary(), binary() | undefined) ->
    {ok, map(), created | existing} | {error, {400 | 409, binary()}}.
create(Uid, Name, RequestId) ->
    case valid_name(Name) of
        false ->
            {error, {400, <<"工作区名称不能为空且不超过 200 字符"/utf8>>}};
        true ->
            case workspace_ds:create_template(Uid, Name, RequestId) of
                {ok, Result, Status} ->
                    _ = ?INFO_LOG([
                        workspace_created,
                        Uid,
                        maps:get(workspace_id, Result),
                        Status
                    ]),
                    {ok, Result, Status};
                {error, owner_workspace_limit} ->
                    {error, {409, <<"已达工作区创建上限"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([workspace_create_failed, Uid, Reason]),
                    {error, {500, <<"工作区创建失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 工作区详情（active 工作区成员可读；Owner/Member/Guest 同权读）
-spec detail(integer(), integer()) -> {ok, map()} | {error, {403 | 404, binary()}}.
detail(Uid, WsId) ->
    case load_workspace(WsId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, WS} ->
            case ensure_member(WsId, Uid) of
                {ok, _Role} -> {ok, WS};
                {error, Forbidden} -> {error, Forbidden}
            end
    end.

%% @doc 我的工作区列表（分页，稳定排序，limit 钳制见 DS 层）
-spec mine(integer(), integer(), integer()) -> {ok, map()}.
mine(Uid, Page, Size) ->
    case workspace_ds:page_by_member(Uid, Page, Size) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_page_failed, Uid, Reason]),
            {ok, #{list => [], page => Page, size => Size, total => 0, total_page => 0}}
    end.

%% @doc 改名/改 logo（仅 Owner）
-spec update_profile(integer(), integer(), binary() | undefined, binary() | undefined) ->
    {ok, map()} | {error, {integer(), binary()}}.
update_profile(Uid, WsId, Name, Logo) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, WS} ->
            Name2 =
                case Name of
                    N when is_binary(N), N =/= <<>> -> N;
                    _ -> maps:get(<<"name">>, WS, <<>>)
                end,
            Logo2 =
                case Logo of
                    L when is_binary(L) -> L;
                    _ -> maps:get(<<"logo">>, WS, <<>>)
                end,
            case valid_name(Name2) of
                false ->
                    {error, {400, <<"工作区名称不能为空且不超过 200 字符"/utf8>>}};
                true ->
                    case workspace_ds:update_profile(WsId, Name2, Logo2) of
                        {ok, Updated} ->
                            {ok, Updated};
                        {error, Reason2} ->
                            _ = ?ERROR_LOG([workspace_update_failed, WsId, Uid, Reason2]),
                            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc branding 读（active 成员可读）
-spec read_branding(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
read_branding(Uid, WsId) ->
    case ensure_member(WsId, Uid) of
        {error, Reason} -> {error, Reason};
        {ok, _Role} -> workspace_ds:read_branding(WsId)
    end.

%% @doc branding 写（仅 Owner；白名单键 name/logo/primaryColor）
-spec update_branding(integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
update_branding(Uid, WsId, Fields) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            %% 原始 branding（含 _request_id 内部键）由 DS 层在合并写时保留
            case workspace_ds:update_branding(WsId, Fields, read_raw_branding(WsId)) of
                {ok, View} ->
                    {ok, View};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([workspace_branding_failed, WsId, Uid, Reason2]),
                    {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc Overview（active 成员可读）：资源摘要 + 工作区成员预览
-spec overview(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
overview(Uid, WsId) ->
    case ensure_member(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Role} ->
            case workspace_ds:overview(WsId, 20) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    _ = ?ERROR_LOG([workspace_overview_failed, WsId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 邀请工作区成员（仅 Owner；仅现有注册用户；幂等）
%% 邀请只写 workspace_member，不自动加入 General 群、不自动订阅 Announcements
%% 频道（I14：三种关系分别写入，加入群/订阅由客户端显式调用并分别返回结果）。
-spec invite(integer(), integer(), integer(), binary()) ->
    {ok, changed | unchanged, map()} | {error, {integer(), binary()}}.
invite(Uid, WsId, TargetUid, Role) ->
    case valid_role(Role) of
        false ->
            {error, {400, <<"角色仅支持 owner/member/guest"/utf8>>}};
        true ->
            case ensure_owner(WsId, Uid) of
                {error, Reason} ->
                    {error, Reason};
                {ok, _WS} ->
                    case ensure_not_archived(WsId) of
                        {error, Reason2} -> {error, Reason2};
                        ok -> invite_checked(Uid, WsId, TargetUid, Role)
                    end
            end
    end.

invite_checked(Uid, WsId, TargetUid, Role) ->
    case TargetUid > 0 of
        false ->
            {error, {400, <<"用户ID格式有误"/utf8>>}};
        true ->
            case user_repo:find_by_id(TargetUid, <<"id">>) of
                #{} = U when map_size(U) =:= 0 ->
                    {error, {404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>}};
                _ ->
                    %% B-01：邀请是点对点直接接触（与好友申请同级），任一方向拉黑即拒绝撮合
                    case user_denylist_logic:blocked_between(Uid, TargetUid) of
                        true ->
                            {error, {403, <<"存在拉黑关系，无法邀请该用户"/utf8>>}};
                        false ->
                            invite_member_tx(Uid, WsId, TargetUid, Role)
                    end
            end
    end.

invite_member_tx(Uid, WsId, TargetUid, Role) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            workspace_member_repo:upsert_active_tx(Conn, WsId, TargetUid, Role, Uid)
        end)
    of
        {ok, changed, _} ->
            _ = ?INFO_LOG([
                workspace_member_invited, WsId, Uid, TargetUid, Role
            ]),
            Member = workspace_member_repo:find(
                WsId,
                TargetUid,
                <<"workspace_id,user_id,role,joined_at,status">>
            ),
            {ok, changed, Member};
        {ok, unchanged, _} ->
            {ok, unchanged,
                workspace_member_repo:find(
                    WsId,
                    TargetUid,
                    <<"workspace_id,user_id,role,joined_at,status">>
                )};
        {ok, role_conflict, _} ->
            {error, {409, <<"该用户已是工作区成员，角色不同；请先改角色或移除后重新邀请"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_invite_failed, WsId, TargetUid, Reason]),
            {error, {500, <<"邀请失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 团队码（工作区可复用加入凭证 T2.3，迁移 00000082）
%% 8 位 A-Z2-9、7 天有效、一码多人复用、Owner 可撤销；
%% 已加入者重复输码幂等 unchanged。
%% ===================================================================

%% @doc 生成工作区团队码（仅 Owner；归档 409 与 invite 同口径；7 天有效）
%% code 全局唯一冲突（23505→code_conflict）时重新生成，重试 ≤3 次。
-spec generate_invite_code(integer(), integer()) ->
    {ok, #{code := binary(), expires_at := binary()}} | {error, {integer(), binary()}}.
generate_invite_code(Uid, WsId) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            case ensure_not_archived(WsId) of
                {error, Reason2} ->
                    {error, Reason2};
                ok ->
                    ExpiresAt = invite_expire_at(),
                    case insert_invite_code(WsId, Uid, ExpiresAt, 3) of
                        {ok, Code} ->
                            _ = ?INFO_LOG([workspace_invite_code_created, WsId, Uid]),
                            {ok, #{code => Code, expires_at => ExpiresAt}};
                        {error, Reason3} ->
                            _ = ?ERROR_LOG([workspace_invite_code_failed, WsId, Uid, Reason3]),
                            {error, {500, <<"生成团队码失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% 团队码有效期：now + 7 天（RFC3339 binary，与库内 timestamptz 编解码同格式）
-spec invite_expire_at() -> binary().
invite_expire_at() ->
    SevenDaysUs = 7 * 24 * 60 * 60 * 1000000,
    elib_dt:to_rfc3339(erlang:system_time(microsecond) + SevenDaysUs).

%% 码唯一冲突重试：code_conflict → 换码重插，其余错误直接冒泡。
%% 同事务先撤销该工作区既有 active 码（一工作区至多一个 active 码；
%% 重新生成即旧码失效）。
-spec insert_invite_code(integer(), integer(), binary(), non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
insert_invite_code(_WsId, _Uid, _ExpiresAt, 0) ->
    {error, invite_code_retry_exhausted};
insert_invite_code(WsId, Uid, ExpiresAt, Left) ->
    Code = workspace_invite_repo:generate_invite_code(),
    Insert = fun(Conn) ->
        %% 撤旧是"重新生成=旧码失效"的语义前提，失败即中止整个事务
        %% （连接级故障下后续 INSERT 大概率同样失败，提前失败更明确）
        {ok, _} = workspace_invite_repo:revoke_active_by_ws_tx(Conn, WsId),
        workspace_invite_repo:add_tx(Conn, WsId, Code, Uid, ExpiresAt)
    end,
    case elib_pg:with_tx(Insert) of
        {ok, _Row} ->
            {ok, Code};
        {error, code_conflict} ->
            insert_invite_code(WsId, Uid, ExpiresAt, Left - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 撤销工作区团队码（仅 Owner；幂等：无 active 码 → revoked 0）。
%% 撤销后该码输码即 981；归档工作区允许撤销（收紧操作，不设 409 门）。
-spec revoke_invite_code(integer(), integer()) ->
    {ok, #{revoked := non_neg_integer()}} | {error, {integer(), binary()}}.
revoke_invite_code(Uid, WsId) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            Revoke = fun(Conn) -> workspace_invite_repo:revoke_active_by_ws_tx(Conn, WsId) end,
            case elib_pg:with_tx(Revoke) of
                {ok, Count} ->
                    _ = ?INFO_LOG([workspace_invite_code_revoked, WsId, Uid, Count]),
                    {ok, #{revoked => Count}};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([workspace_invite_code_revoke_failed, WsId, Uid, Reason2]),
                    {error, {500, <<"撤销团队码失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 团队码加入工作区（任意登录用户）：码校验 981/982 → 工作区 404 →
%% 幂等 unchanged → 否则 write_tx（归档稳定码 980）事务内 upsert member。
-spec join_by_code(integer(), binary()) ->
    {ok, joined | unchanged, map()} | {error, {integer(), binary()}}.
join_by_code(Uid, Code) ->
    Lookup = fun(Conn) -> workspace_invite_repo:find_active_by_code_tx(Conn, Code) end,
    case elib_pg:with_tx(Lookup) of
        not_found ->
            {error, {?ERR_WORKSPACE_INVITE_INVALID, <<"团队码无效或已失效"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_invite_lookup_failed, Uid, Reason]),
            {error, {500, <<"加入失败，请稍后重试"/utf8>>}};
        {ok, #{<<"expired">> := true}} ->
            {error, {?ERR_WORKSPACE_INVITE_EXPIRED, <<"团队码已过期"/utf8>>}};
        {ok, Invite} ->
            join_valid_code(
                Uid,
                maps:get(<<"workspace_id">>, Invite, 0),
                %% DB NULL 经 epgsql 解码为 null atom（DEFAULT_NULLS）；
                %% nil atom 无法编码回 NULL，默认值勿用 nil
                maps:get(
                    <<"created_by">>, Invite, null
                )
            )
    end.

%% 码有效：工作区存在性（detail 同口径 404）→ 幂等检查 → 入会写事务
-spec join_valid_code(integer(), integer(), integer() | null) ->
    {ok, joined | unchanged, map()} | {error, {integer(), binary()}}.
join_valid_code(Uid, WsId, CreatedBy) ->
    case load_workspace(WsId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, WS} ->
            case ensure_member(WsId, Uid) of
                {ok, _Role} ->
                    {ok, unchanged, WS};
                {error, _} ->
                    join_as_member(Uid, WsId, CreatedBy, WS)
            end
    end.

%% 入会写入：workspace_guard:write_tx 归档守卫（980）+ upsert member（幂等）
-spec join_as_member(integer(), integer(), integer() | null, map()) ->
    {ok, joined | unchanged, map()} | {error, {integer(), binary()}}.
join_as_member(Uid, WsId, CreatedBy, WS) ->
    Upsert = fun(Conn) ->
        workspace_member_repo:upsert_active_tx(Conn, WsId, Uid, <<"member">>, CreatedBy)
    end,
    case workspace_guard:write_tx({workspace, WsId}, Upsert) of
        {ok, changed, _} ->
            _ = ?INFO_LOG([workspace_joined_by_code, WsId, Uid]),
            {ok, joined, WS};
        {ok, unchanged, _} ->
            {ok, unchanged, WS};
        {ok, role_conflict, _} ->
            {error, {409, <<"该用户已是工作区成员，角色不同"/utf8>>}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_join_failed, WsId, Uid, Reason]),
            {error, {500, <<"加入失败，请稍后重试"/utf8>>}}
    end.

%% @doc 移除工作区成员（仅 Owner；§1.4.2 规则 8）
%% 1) 冲突检查：主 Owner 身份 / 拥有 Project / 未完成 Task → 409 membership_conflict 全回滚；
%% 2) 无冲突：同事务禁用下属 workspace 群成员关系（status=0）再 removed 父关系，
%%    返回受影响资源清单；重新加入不自动恢复下级关系（不写回 group_member）。
%% DB 触发器 trg_workspace_member_remove_guard 是第二道兜底（T3-②④）。
-spec remove_member(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
remove_member(Uid, WsId, TargetUid) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, WS} ->
            case ensure_not_archived(WsId) of
                {error, Reason2} ->
                    {error, Reason2};
                ok ->
                    PrimaryOwner = maps:get(<<"owner_id">>, WS, 0),
                    case TargetUid =:= PrimaryOwner of
                        true ->
                            {error, {409, <<"主 Owner 不能被移除，请先转移主 Owner"/utf8>>}};
                        false ->
                            case TargetUid =:= Uid of
                                true -> {error, {400, <<"不能移除自己，请先转移主 Owner"/utf8>>}};
                                false -> remove_member_checked(WsId, TargetUid)
                            end
                    end
            end
    end.

remove_member_checked(WsId, TargetUid) ->
    case elib_pg:with_tx(fun(Conn) -> remove_member_tx(Conn, WsId, TargetUid) end) of
        Result when is_map(Result) ->
            AffectedGroups = maps:get(affected_groups, Result, []),
            lists:foreach(
                fun(#{group_id := Gid}) -> group_ds:leave(TargetUid, Gid) end,
                AffectedGroups
            ),
            imboy_domain_event:publish([
                {member_removed, Gid, TargetUid}
             || #{group_id := Gid} <- AffectedGroups
            ]),
            _ = ?INFO_LOG([
                workspace_member_removed,
                WsId,
                TargetUid,
                {affected_groups, length(AffectedGroups)}
            ]),
            {ok, Result};
        {error, {membership_conflict, Conflicts}} ->
            {error, {409, membership_conflict_msg(Conflicts)}};
        {error, member_not_active} ->
            {error, {409, <<"该用户不是工作区成员或已被移除"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_remove_failed, WsId, TargetUid, Reason]),
            {error, {500, <<"移除失败，请稍后重试"/utf8>>}}
    end.

remove_member_tx(Conn, WsId, TargetUid) ->
    %% 冲突检查（同事务，防检查-移除间竞态；fail-closed）
    case workspace_member_repo:owned_projects_of_user(Conn, WsId, TargetUid) of
        {ok, [_ | _] = Projects} ->
            throw({abort_tx, {membership_conflict, #{owned_projects => Projects}}});
        {ok, []} ->
            ok;
        {error, Reason} ->
            throw({abort_tx, Reason})
    end,
    case workspace_member_repo:unfinished_tasks_of_user(Conn, WsId, TargetUid) of
        {ok, [_ | _] = Tasks} ->
            throw({abort_tx, {membership_conflict, #{unfinished_tasks => Tasks}}});
        {ok, []} ->
            ok;
        {error, Reason2} ->
            throw({abort_tx, Reason2})
    end,
    %% 无冲突：先禁用下属 workspace 群成员（同事务，满足移除保护触发器）
    {ok, Affected} = workspace_member_repo:list_active_workspace_groups_of_user(
        Conn, WsId, TargetUid
    ),
    DisableSql = <<"UPDATE group_member SET status = 0, updated_at = $1 WHERE id = $2">>,
    lists:foreach(
        fun(#{<<"gm_id">> := GmId}) ->
            case elib_pg:execute(Conn, DisableSql, [elib_dt:now(), GmId]) of
                {ok, 1} -> ok;
                {error, DisableReason} -> throw({abort_tx, DisableReason})
            end
        end,
        Affected
    ),
    %% 再 removed 父关系（触发器在 COMMIT 校验无残留 active 下级）
    ok = workspace_member_repo:remove_tx(Conn, WsId, TargetUid),
    AffectedOut = [#{group_id => G} || #{<<"group_id">> := G} <- Affected],
    %% 审计：受影响资源清单入服务端日志（T7 归档审计列之外的成员治理审计）
    _ = ?INFO_LOG([
        workspace_member_cascade,
        WsId,
        TargetUid,
        {disabled_group_members, AffectedOut}
    ]),
    #{
        workspace_id => WsId,
        user_id => TargetUid,
        status => <<"removed">>,
        affected_groups => AffectedOut
    }.

%% @doc 改角色（仅 Owner；最后 Owner 保护；Guest↔member/owner 均可由 Owner 调整）
-spec change_role(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
change_role(Uid, WsId, TargetUid, Role) ->
    case valid_role(Role) of
        false ->
            {error, {400, <<"角色仅支持 owner/member/guest"/utf8>>}};
        true ->
            case ensure_owner(WsId, Uid) of
                {error, Reason} ->
                    {error, Reason};
                {ok, _WS} ->
                    case ensure_not_archived(WsId) of
                        {error, Reason2} -> {error, Reason2};
                        ok -> change_role_checked(Uid, WsId, TargetUid, Role)
                    end
            end
    end.

change_role_checked(Uid, WsId, TargetUid, Role) ->
    case elib_pg:with_tx(fun(Conn) -> change_role_tx(Conn, Uid, WsId, TargetUid, Role) end) of
        ok ->
            _ = ?INFO_LOG([workspace_member_role_changed, WsId, TargetUid, Role]),
            {ok, #{workspace_id => WsId, user_id => TargetUid, role => Role}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, member_not_active} ->
            {error, {409, <<"该用户不是工作区成员"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_role_failed, WsId, TargetUid, Reason]),
            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
    end.

change_role_tx(Conn, Uid, WsId, TargetUid, Role) ->
    ok = workspace_guard:abort_on_error(
        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
    ),
    Actor = workspace_member_repo:find_tx(Conn, WsId, Uid, <<"role,status">>),
    case Actor of
        #{<<"role">> := <<"owner">>, <<"status">> := <<"active">>} -> ok;
        _ -> throw({abort_tx, {403, <<"仅工作区 Owner 可执行此操作"/utf8>>}})
    end,
    Member = workspace_member_repo:find_tx(Conn, WsId, TargetUid, <<"role,status">>),
    case maps:get(<<"status">>, Member, <<>>) of
        <<"active">> ->
            CurrentRole = maps:get(<<"role">>, Member, <<>>),
            LastOwnerProtected =
                CurrentRole =:= <<"owner">> andalso Role =/= <<"owner">> andalso
                    workspace_member_repo:count_by_role_tx(Conn, WsId, <<"owner">>) =< 1,
            case LastOwnerProtected of
                true ->
                    throw({abort_tx, {409, <<"工作区至少保留一名 Owner"/utf8>>}});
                false ->
                    workspace_member_repo:update_role_tx(Conn, WsId, TargetUid, Role)
            end;
        _ ->
            throw({abort_tx, member_not_active})
    end.

%% @doc 主 Owner 转移（仅 Owner；目标须为非 Guest 的 active 工作区成员；至少保留一名 Owner）
%% 同事务：workspace.owner_id = 目标 + 目标角色→owner + 原主 Owner 角色→member。
-spec transfer_owner(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
transfer_owner(Uid, WsId, TargetUid) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            case ensure_not_archived(WsId) of
                {error, Reason2} ->
                    {error, Reason2};
                ok ->
                    case TargetUid =:= Uid of
                        true -> {error, {400, <<"新主 Owner 不能是当前主 Owner"/utf8>>}};
                        false -> transfer_owner_checked(Uid, WsId, TargetUid)
                    end
            end
    end.

transfer_owner_checked(Uid, WsId, TargetUid) ->
    Target = workspace_member_repo:find(WsId, TargetUid, <<"role,status">>),
    case maps:get(<<"status">>, Target, <<>>) of
        <<"active">> ->
            case maps:get(<<"role">>, Target, <<>>) of
                <<"guest">> ->
                    {error, {409, <<"主 Owner 转移目标不能是 Guest"/utf8>>}};
                <<"owner">> ->
                    {error, {409, <<"目标用户已是 Owner"/utf8>>}};
                _ ->
                    case
                        elib_pg:with_tx(fun(Conn) ->
                            ok = workspace_member_repo:update_role_tx(
                                Conn, WsId, TargetUid, <<"owner">>
                            ),
                            ok = workspace_member_repo:update_role_tx(
                                Conn, WsId, Uid, <<"member">>
                            ),
                            workspace_ds:ws_transfer_tx(Conn, WsId, TargetUid)
                        end)
                    of
                        ok ->
                            _ = ?INFO_LOG([workspace_owner_transferred, WsId, Uid, TargetUid]),
                            {ok, #{
                                workspace_id => WsId,
                                owner_id => TargetUid,
                                previous_owner_id => Uid
                            }};
                        {error, Reason} ->
                            _ = ?ERROR_LOG([workspace_transfer_failed, WsId, TargetUid, Reason]),
                            {error, {500, <<"转移失败，请稍后重试"/utf8>>}}
                    end
            end;
        _ ->
            {error, {409, <<"目标用户不是该工作区的 active 成员"/utf8>>}}
    end.

%% @doc 工作区成员列表（active 成员可读；分页）
-spec member_list(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
member_list(Uid, WsId, Page0, Size0) ->
    case ensure_member(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Role} ->
            Size = max(1, min(Size0, 100)),
            Page = max(Page0, 1),
            case
                workspace_member_repo:page_by_workspace(
                    WsId,
                    Page,
                    Size,
                    <<"wm.workspace_id,wm.user_id,wm.role,wm.invited_by,wm.joined_at,wm.status,",
                        "u.nickname,u.avatar,u.account">>
                )
            of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    _ = ?ERROR_LOG([workspace_member_page_failed, WsId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% 生命周期：归档/恢复（T7；Owner only；写 archived_at/archived_by 审计列）
%% ===================================================================

%% @doc 归档工作区（仅 Owner；审计列 archived_at/archived_by；服务端日志审计）
%% 归档后所有 workspace 资源写操作被 workspace_guard 拒绝（稳定错误码 980）；
%% 读取/历史浏览不受影响；personal 资源永不受 guard 影响。
-spec archive(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
archive(Uid, WsId) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            case elib_pg:with_tx(fun(Conn) -> archive_tx(Conn, WsId, Uid) end) of
                {ok, Result} when is_map(Result) ->
                    _ = ?INFO_LOG([workspace_archived, WsId, Uid]),
                    {ok, Result};
                {error, already_archived} ->
                    {error, {409, <<"工作区已处于归档状态"/utf8>>}};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([workspace_archive_failed, WsId, Uid, Reason2]),
                    {error, {500, <<"归档失败，请稍后重试"/utf8>>}}
            end
    end.

archive_tx(Conn, WsId, Uid) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE workspace SET status = 'archived', archived_at = $1,",
            " archived_by = $2, updated_at = $1", " WHERE id = $3 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, Uid, WsId]) of
        {ok, 1} ->
            {ok, #{
                workspace_id => WsId,
                status => <<"archived">>,
                archived_by => Uid,
                archived_at => Now
            }};
        {ok, 0} ->
            throw({abort_tx, already_archived});
        {error, Reason} ->
            throw({abort_tx, Reason})
    end.

%% @doc 恢复工作区（仅 Owner；清空归档审计列；恢复后写操作放行）
-spec restore(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
restore(Uid, WsId) ->
    case ensure_owner(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _WS} ->
            case elib_pg:with_tx(fun(Conn) -> restore_tx(Conn, WsId) end) of
                {ok, Result} when is_map(Result) ->
                    _ = ?INFO_LOG([workspace_restored, WsId, Uid]),
                    {ok, Result};
                {error, not_archived} ->
                    {error, {409, <<"工作区不处于归档状态"/utf8>>}};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([workspace_restore_failed, WsId, Uid, Reason2]),
                    {error, {500, <<"恢复失败，请稍后重试"/utf8>>}}
            end
    end.

restore_tx(Conn, WsId) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE workspace SET status = 'active', archived_at = NULL,",
            " archived_by = NULL, updated_at = $1", " WHERE id = $2 AND status = 'archived'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId]) of
        {ok, 1} ->
            {ok, #{workspace_id => WsId, status => <<"active">>}};
        {ok, 0} ->
            throw({abort_tx, not_archived});
        {error, Reason} ->
            throw({abort_tx, Reason})
    end.

%% ===================================================================
%% 权限辅助（T5 各入口复用）
%% ===================================================================

%% @doc 当前用户在工作区的角色（无/非 active → {error, 403}）
-spec my_role(integer(), integer()) -> {ok, binary()} | {error, {403, binary()}}.
my_role(WsId, Uid) ->
    Row = workspace_member_repo:find(WsId, Uid, <<"role,status">>),
    case maps:get(<<"status">>, Row, <<>>) of
        <<"active">> -> {ok, maps:get(<<"role">>, Row, <<"member">>)};
        _ -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}}
    end.

%% @doc active 工作区成员校验（Owner/Member/Guest 均通过）
-spec ensure_member(integer(), integer()) -> {ok, binary()} | {error, {403, binary()}}.
ensure_member(WsId, Uid) ->
    my_role(WsId, Uid).

%% @doc 可创建 Workspace Channel/Group 的校验（Owner/Member ✅；Guest ❌）
%% （§1.4.2 三角色矩阵：创建 Workspace Channel/Group 行）
-spec ensure_can_create_resource(integer(), integer()) -> ok | {error, {403, binary()}}.
ensure_can_create_resource(WsId, Uid) ->
    case my_role(WsId, Uid) of
        {ok, <<"guest">>} ->
            {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
        {ok, _Role} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec load_workspace(integer()) -> {ok, map()} | {error, {404, binary()}}.
load_workspace(WsId) ->
    case workspace_ds:find_by_id(WsId) of
        WS when is_map(WS), map_size(WS) > 0 ->
            {ok, WS};
        _ ->
            {error, {404, <<"工作区不存在"/utf8>>}}
    end.

%% 仅 Owner 可治理（增删工作区成员、改角色、Branding、转移）
-spec ensure_owner(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
ensure_owner(WsId, Uid) ->
    case load_workspace(WsId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, WS} ->
            case my_role(WsId, Uid) of
                {ok, <<"owner">>} -> {ok, WS};
                {ok, _Other} -> {error, {403, <<"仅工作区 Owner 可执行该操作"/utf8>>}};
                {error, Reason} -> {error, Reason}
            end
    end.

%% 归档工作区拒绝成员管理写操作（简单前置检查；完整归档写守卫是 T7 的活）
-spec ensure_not_archived(integer()) -> ok | {error, {409, binary()}}.
ensure_not_archived(WsId) ->
    case workspace_ds:find_by_id(WsId, <<"status">>) of
        #{<<"status">> := <<"archived">>} ->
            {error, {409, <<"工作区已归档，成员管理操作被拒绝"/utf8>>}};
        _ ->
            ok
    end.

-spec read_raw_branding(integer()) -> map().
read_raw_branding(WsId) ->
    case workspace_ds:find_by_id(WsId, <<"branding">>) of
        #{<<"branding">> := Branding} when is_binary(Branding) ->
            case catch jsone:decode(Branding, [{object_format, map}]) of
                Map when is_map(Map) -> Map;
                _ -> #{}
            end;
        _ ->
            #{}
    end.

-spec valid_name(term()) -> boolean().
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    %% 200 字符列宽（varchar(200)），按字符数校验防截断报错
    string:length(Name) =< 200;
valid_name(_) ->
    false.

-spec valid_role(term()) -> boolean().
valid_role(<<"owner">>) -> true;
valid_role(<<"member">>) -> true;
valid_role(<<"guest">>) -> true;
valid_role(_) -> false.

-spec membership_conflict_msg(map()) -> binary().
membership_conflict_msg(#{owned_projects := Projects}) ->
    Names = elib_cnv:implode(<<"、"/utf8>>, [maps:get(<<"name">>, P, <<"">>) || P <- Projects]),
    <<"membership_conflict：该用户仍是项目 Owner（"/utf8, Names/binary, "），须先转移项目 Owner 再移除"/utf8>>;
membership_conflict_msg(#{unfinished_tasks := Tasks}) ->
    Titles = elib_cnv:implode(<<"、"/utf8>>, [maps:get(<<"title">>, T, <<"">>) || T <- Tasks]),
    <<"membership_conflict：该用户有未完成任务（"/utf8, Titles/binary, "），须先改派或完成后再移除"/utf8>>;
membership_conflict_msg(_) ->
    <<"membership_conflict：该用户存在未完成的成员关系冲突"/utf8>>.

%% ===================================================================
%% Admin 运营管理（双体验 v2.5.2 WP7/T11b）
%% 与 Owner 侧 archive/restore 并存：本段不做 Owner 校验（平台运营动作），
%% 归档同样写审计列并触发 T7 写守卫（980）。
%% ===================================================================

%% @doc Admin 工作区分页列表（搜索/状态筛选；批量资源计数防 N+1）
-spec admin_page(integer(), integer(), binary() | all, binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_page(Page, Size, Status, Keyword) ->
    case workspace_ds:admin_page(Page, Size, normalize_admin_status(Status), Keyword) of
        {ok, #{list := []} = Result} ->
            %% 空页跳过资源计数查询
            {ok, Result};
        {ok, #{list := Rows} = Result} ->
            Counts = workspace_ds:admin_batch_resource_counts([
                maps:get(<<"id">>, Row, 0)
             || Row <- Rows
            ]),
            List2 = [attach_admin_counts(Row, Counts) || Row <- Rows],
            {ok, Result#{list => List2}};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_admin_page_failed, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc Admin 工作区详情：基本信息 + branding 公共视图 + 工作区成员（分页）+ 资源清单
-spec admin_detail(integer()) -> {ok, map()} | {error, {404, binary()}}.
admin_detail(WsId) ->
    case workspace_ds:find_by_id(WsId) of
        WS when is_map(WS), map_size(WS) > 0 ->
            Owner = user_ds:find_by_id(
                maps:get(<<"owner_id">>, WS, 0), <<"id,nickname,avatar,account">>
            ),
            {ok, Members} = workspace_member_repo:page_by_workspace(
                WsId,
                1,
                20,
                <<"wm.workspace_id,wm.user_id,wm.role,wm.joined_at,wm.status,",
                    "u.nickname,u.avatar,u.account">>
            ),
            {ok, Projects} = workspace_ds:admin_resource_list(project, WsId, 20),
            {ok, Groups} = workspace_ds:admin_resource_list(group, WsId, 20),
            {ok, Channels} = workspace_ds:admin_resource_list(channel, WsId, 20),
            {ok, WS#{
                owner => Owner,
                members => Members,
                projects => Projects,
                groups => Groups,
                channels => Channels
            }};
        _ ->
            {error, {404, <<"工作区不存在"/utf8>>}}
    end.

%% @doc Admin 工作区成员分页（详情页"工作区成员"列表，role 徽标数据源）
-spec admin_member_page(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_member_page(WsId, Page0, Size0) ->
    Size = max(1, min(Size0, 100)),
    Page = max(Page0, 1),
    case
        workspace_member_repo:page_by_workspace(
            WsId,
            Page,
            Size,
            <<"wm.workspace_id,wm.user_id,wm.role,wm.invited_by,wm.joined_at,wm.status,",
                "u.nickname,u.avatar,u.account">>
        )
    of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            _ = ?ERROR_LOG([workspace_admin_member_page_failed, WsId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc 运营归档（平台侧；不做 Owner 校验；同样写审计列 archived_by=操作管理员）
%% 归档后 T7 写守卫（稳定错误码 980）对全部 workspace 业务写生效。
-spec admin_archive(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_archive(AdmUserId, WsId) ->
    case workspace_ds:find_by_id(WsId, <<"id">>) of
        #{<<"id">> := _} ->
            case elib_pg:with_tx(fun(Conn) -> admin_archive_tx(Conn, WsId, AdmUserId) end) of
                {ok, Result} when is_map(Result) ->
                    _ = ?INFO_LOG([workspace_admin_archived, WsId, AdmUserId]),
                    {ok, Result};
                {error, already_archived} ->
                    {error, {409, <<"工作区已处于归档状态"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([workspace_admin_archive_failed, WsId, AdmUserId, Reason]),
                    {error, {500, <<"归档失败，请稍后重试"/utf8>>}}
            end;
        _ ->
            {error, {404, <<"工作区不存在"/utf8>>}}
    end.

admin_archive_tx(Conn, WsId, _AdmUserId) ->
    Now = elib_dt:now(),
    %% archived_by 列带 FK → "user"(id)（迁移 00000076），而运营操作者是
    %% adm_user.id，写入库必 23503 回滚成 500——admin 路径固定写 NULL，
    %% 操作者审计由 handler 层 audit_workspace_governance（admin_operation_logs）
    %% 承担；user 侧 Owner 归档（archive/2）不受影响，仍写 Uid。
    Sql =
        <<"UPDATE workspace SET status = 'archived', archived_at = $1,",
            " archived_by = NULL, updated_at = $1", " WHERE id = $2 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId]) of
        {ok, 1} ->
            {ok, #{
                workspace_id => WsId,
                status => <<"archived">>,
                archived_by => null,
                archived_at => Now
            }};
        {ok, 0} ->
            throw({abort_tx, already_archived});
        {error, Reason} ->
            throw({abort_tx, Reason})
    end.

%% @doc 运营恢复（平台侧；清空归档审计列；恢复后写守卫放行）
-spec admin_restore(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_restore(AdmUserId, WsId) ->
    case workspace_ds:find_by_id(WsId, <<"id">>) of
        #{<<"id">> := _} ->
            case elib_pg:with_tx(fun(Conn) -> admin_restore_tx(Conn, WsId) end) of
                {ok, Result} when is_map(Result) ->
                    _ = ?INFO_LOG([workspace_admin_restored, WsId, AdmUserId]),
                    {ok, Result};
                {error, not_archived} ->
                    {error, {409, <<"工作区不处于归档状态"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([workspace_admin_restore_failed, WsId, AdmUserId, Reason]),
                    {error, {500, <<"恢复失败，请稍后重试"/utf8>>}}
            end;
        _ ->
            {error, {404, <<"工作区不存在"/utf8>>}}
    end.

admin_restore_tx(Conn, WsId) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE workspace SET status = 'active', archived_at = NULL,",
            " archived_by = NULL, updated_at = $1", " WHERE id = $2 AND status = 'archived'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId]) of
        {ok, 1} ->
            {ok, #{workspace_id => WsId, status => <<"active">>}};
        {ok, 0} ->
            throw({abort_tx, not_archived});
        {error, Reason} ->
            throw({abort_tx, Reason})
    end.

%% Admin 状态筛选归一：仅认 active/archived，其余 all
-spec normalize_admin_status(binary() | all) -> binary() | all.
normalize_admin_status(<<"active">>) -> <<"active">>;
normalize_admin_status(<<"archived">>) -> <<"archived">>;
normalize_admin_status(_) -> all.

%% 列表行附加批量资源计数（缺省 0）
-spec attach_admin_counts(map(), map()) -> map().
attach_admin_counts(Row, Counts) ->
    WsId = maps:get(<<"id">>, Row, 0),
    Row#{
        <<"project_count">> => count_of(Counts, <<"project_count">>, WsId),
        <<"group_count">> => count_of(Counts, <<"group_count">>, WsId),
        <<"channel_count">> => count_of(Counts, <<"channel_count">>, WsId),
        <<"member_count">> => count_of(Counts, <<"member_count">>, WsId)
    }.

-spec count_of(map(), binary(), integer()) -> integer().
count_of(Counts, Key, WsId) ->
    case maps:get(Key, Counts, #{}) of
        Map when is_map(Map) -> maps:get(WsId, Map, 0);
        _ -> 0
    end.
