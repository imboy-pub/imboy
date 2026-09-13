-module(organization_member_logic).

%% Organization 治理成员。该关系不派生 Workspace 或 Group 成员资格。

-export([list/4, invite/4, change_role/4, remove/3, transfer_owner/3]).

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

transfer_owner_validated(Uid, OrgId, TargetUid) ->
    Tx = fun(Conn) -> transfer_owner_tx(Conn, Uid, OrgId, TargetUid) end,
    case elib_pg:with_tx(Tx) of
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_owner_transfer_failed, OrgId, Uid, TargetUid, Reason]),
            internal_error(<<"Owner 转移失败，请稍后重试"/utf8>>);
        Result when is_map(Result) ->
            ?INFO_LOG([organization_owner_transferred, OrgId, Uid, TargetUid]),
            {ok, Result}
    end.

transfer_owner_tx(Conn, Uid, OrgId, TargetUid) ->
    Org =
        case organization_repo:find_for_update_tx(Conn, OrgId) of
            {ok, #{<<"status">> := <<"active">>} = Row} -> Row;
            {ok, _} -> abort(409, <<"Organization 已归档，不能转移 Owner"/utf8>>);
            {error, not_found} -> abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
        end,
    case maps:get(<<"owner_id">>, Org, 0) of
        Uid -> ok;
        _ -> abort(403, <<"仅当前主 Owner 可转移 Owner"/utf8>>)
    end,
    case organization_member_repo:find_for_update_tx(Conn, OrgId, Uid, <<"role,status">>) of
        {ok, #{<<"role">> := <<"owner">>, <<"status">> := <<"active">>}} -> ok;
        {ok, _} -> abort(403, <<"当前主 Owner 成员状态无效"/utf8>>);
        {error, not_found} -> abort(403, <<"当前主 Owner 成员状态无效"/utf8>>);
        {error, Reason2} -> throw({abort_tx, {internal, Reason2}})
    end,
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} when
            Role =:= <<"admin">>; Role =:= <<"member">>
        ->
            ok;
        {ok, #{<<"status">> := <<"active">>, <<"role">> := <<"owner">>}} ->
            abort(409, <<"目标用户已是 Owner"/utf8>>);
        {ok, _} ->
            member_not_active();
        {error, not_found} ->
            member_not_active();
        {error, Reason3} ->
            throw({abort_tx, {internal, Reason3}})
    end,
    case organization_repo:update_owner_tx(Conn, OrgId, TargetUid) of
        {ok, _} -> ok;
        {error, Reason4} -> throw({abort_tx, {internal, Reason4}})
    end,
    case organization_member_repo:update_role_tx(Conn, OrgId, Uid, <<"admin">>) of
        ok -> ok;
        {error, Reason5} -> throw({abort_tx, {internal, Reason5}})
    end,
    case organization_member_repo:find_active_tx(Conn, OrgId, TargetUid, <<"role">>) of
        {ok, #{<<"role">> := <<"owner">>}} ->
            #{
                organization_id => OrgId,
                owner_id => TargetUid,
                previous_owner_id => Uid,
                previous_owner_role => <<"admin">>
            };
        {ok, _} ->
            throw({abort_tx, owner_membership_not_synchronized});
        {error, Reason6} ->
            throw({abort_tx, {owner_membership_not_synchronized, Reason6}})
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

remove_tx(Conn, Uid, Org, OrgId, TargetUid) ->
    case
        organization_member_repo:find_for_update_tx(
            Conn, OrgId, TargetUid, <<"role,status">>
        )
    of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := <<"owner">>}} ->
            abort(409, <<"主 Owner 不能被移除，请先转移 Owner"/utf8>>);
        {ok, #{<<"status">> := <<"active">>, <<"role">> := <<"admin">>}} ->
            ensure_primary_owner(Uid, Org),
            remove_active_tx(Conn, OrgId, TargetUid);
        {ok, #{<<"status">> := <<"active">>}} ->
            remove_active_tx(Conn, OrgId, TargetUid);
        {ok, _} ->
            member_not_active();
        {error, not_found} ->
            member_not_active();
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

remove_active_tx(Conn, OrgId, TargetUid) ->
    case organization_member_repo:remove_tx(Conn, OrgId, TargetUid) of
        ok ->
            {ok, member_result(OrgId, TargetUid, undefined, <<"removed">>)};
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

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
