-module(adm_organization_handler).
-compile([nowarn_deprecated_catch]).

%%%
% adm_organization 控制器（TASK_ID=ORG-ADM-ORG-API）
% Platform Admin Organization 治理 API——IMBoyAdmin 从「调租户 /api/v1 接口」
% 切换为专用平台治理端点：
%   * 读（organizations:read）：组织分页搜索 / 详情 / 成员 / 邀请 / 部门 /
%     Workspace 只读关系事实
%   * 写（organizations:write）：archive/restore、owner-transfer、成员
%     suspend/restore/remove、邀请 create/cancel、部门
%     create/rename/move/archive
%
% 硬边界：
%   * Platform Admin 不映射 org owner/admin；不签发/代理/冒充租户 session；
%   * 鉴权镜像 adm_workspace_handler：每个 action 显式走
%     adm_acl:ensure_permission（fail-closed：无权限/无 adm_user_id 恒 403）；
%   * handler 只做平台鉴权、参数转换（TSID string→int）、审计
%     （adm_operation_log_ds）与稳定错误分类；业务一律经
%     organization_admin_logic（平台通道，复用 src/lib/organization 的
%     domain 校验与 infrastructure 原语）；
%   * mutation 成功（含幂等 unchanged）即写平台审计：操作者 adm uid、
%     目标 org、动作、请求摘要；审计失败不阻断已完成的业务操作。
%
% TSID 传输规则：JSON 里 64-bit ID 一律 string 下发/接收（防 JS 精度丢失）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(ACL_READ, <<"organizations:read">>).
-define(ACL_WRITE, <<"organizations:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_organization_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(list, Method, Req0, State) ->
    list_action(Method, Req0, State);
dispatch(show, Method, Req0, State) ->
    show_action(Method, Req0, State);
dispatch(members, Method, Req0, State) ->
    members_action(Method, Req0, State);
dispatch(invitations, Method, Req0, State) ->
    invitations_action(Method, Req0, State);
dispatch(departments, Method, Req0, State) ->
    departments_action(Method, Req0, State);
dispatch(workspaces, Method, Req0, State) ->
    workspaces_action(Method, Req0, State);
dispatch(org_archive, Method, Req0, State) ->
    org_archive_action(Method, Req0, State);
dispatch(org_restore, Method, Req0, State) ->
    org_restore_action(Method, Req0, State);
dispatch(owner_transfer, Method, Req0, State) ->
    owner_transfer_action(Method, Req0, State);
dispatch(member_suspend, Method, Req0, State) ->
    member_action(Method, Req0, State, suspend);
dispatch(member_restore, Method, Req0, State) ->
    member_action(Method, Req0, State, restore);
dispatch(member_remove, Method, Req0, State) ->
    member_action(Method, Req0, State, remove);
dispatch(invitation_cancel, Method, Req0, State) ->
    invitation_cancel_action(Method, Req0, State);
dispatch(department_rename, Method, Req0, State) ->
    department_rename_action(Method, Req0, State);
dispatch(department_move, Method, Req0, State) ->
    department_move_action(Method, Req0, State);
dispatch(department_archive, Method, Req0, State) ->
    department_archive_action(Method, Req0, State);
dispatch(_, _Method, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 读：组织分页 + 搜索
%% ------------------------------------------------------------------

-spec list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:binary(status, Req0, <<"all">>),
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
            case organization_admin_logic:admin_page(Page, Size, Status, Keyword) of
                {ok, P} ->
                    elib_response:success(Req0, normalize_org_page(P));
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end;
list_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 读：组织详情
%% ------------------------------------------------------------------

-spec show_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
show_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case organization_admin_logic:admin_detail(OrgId) of
                        {ok, Detail} ->
                            elib_response:success(Req0, normalize_org_detail(Detail));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
show_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 读：成员分页
%% ------------------------------------------------------------------

-spec members_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    {Page, Size} = elib_param:page(Req0),
                    case organization_admin_logic:admin_member_page(OrgId, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, normalize_member_page(P));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
members_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 邀请：GET=列表（read）；POST=create（write）——同路径分 method
%% ------------------------------------------------------------------

-spec invitations_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
invitations_action(<<"POST">>, Req0, State) ->
    invitation_create(Req0, State);
invitations_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    {ok, Status} = elib_param:binary(status, Req0, <<"all">>),
                    {ok, LimitBin} = elib_param:binary(limit, Req0, <<"20">>),
                    Limit = elib_cnv:safe_to_integer(LimitBin),
                    case
                        organization_admin_logic:admin_invitation_list(
                            OrgId, normalize_status(Status), normalize_limit(Limit)
                        )
                    of
                        {ok, Rows} ->
                            elib_response:success(Req0, [normalize_invitation_row(R) || R <- Rows]);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
invitations_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 部门：GET=列表（read）；POST=create（write）——同路径分 method
%% ------------------------------------------------------------------

-spec departments_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
departments_action(<<"POST">>, Req0, State) ->
    department_create(Req0, State);
departments_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    {ok, Status} = elib_param:binary(status, Req0, <<"all">>),
                    case organization_admin_logic:admin_department_list(OrgId, Status) of
                        {ok, Rows} ->
                            elib_response:success(Req0, [normalize_department_row(R) || R <- Rows]);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
departments_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 读：组织下 Workspace 只读关系事实
%% ------------------------------------------------------------------

-spec workspaces_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
workspaces_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    {Page, Size} = elib_param:page(Req0),
                    case organization_admin_logic:admin_workspace_page(OrgId, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, normalize_workspace_page(P));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
workspaces_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 写：archive / restore
%% ------------------------------------------------------------------

-spec org_archive_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
org_archive_action(<<"POST">>, Req0, State) ->
    lifecycle_write(Req0, State, <<"archive">>, fun organization_admin_logic:admin_archive/2);
org_archive_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec org_restore_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
org_restore_action(<<"POST">>, Req0, State) ->
    lifecycle_write(Req0, State, <<"restore">>, fun organization_admin_logic:admin_restore/2);
org_restore_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec lifecycle_write(cowboy_req:req(), map(), binary(), fun((integer(), integer()) -> term())) ->
    cowboy_req:req().
lifecycle_write(Req0, State, ActionBin, Fun) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case Fun(AdmUserId, OrgId) of
                        {ok, Result} ->
                            audit(AdmUserId, OrgId, ActionBin, #{}, Req0),
                            elib_response:success(
                                Req0,
                                normalize_result(Result),
                                <<"Organization 已", ActionBin/binary>>
                            );
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% ------------------------------------------------------------------
%% 写：Owner 转移
%% ------------------------------------------------------------------

-spec owner_transfer_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
owner_transfer_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            case parse_tsid_map(maps:get(<<"target_user_id">>, Data, <<>>)) of
                                {ok, TargetUid} ->
                                    transfer_write(Req0, State, AdmUserId, OrgId, TargetUid);
                                error ->
                                    elib_response:error(
                                        Req0,
                                        <<"target_user_id 必须是正整数"/utf8>>,
                                        ?ERR_BAD_REQUEST
                                    )
                            end
                    end
            end
    end;
owner_transfer_action(_, Req0, _State) ->
    method_not_allowed(Req0).

transfer_write(Req0, State, AdmUserId, OrgId, TargetUid) ->
    case organization_admin_logic:admin_transfer_owner(AdmUserId, OrgId, TargetUid) of
        {ok, Result} ->
            audit(
                AdmUserId,
                OrgId,
                <<"owner_transfer">>,
                #{<<"target_user_id">> => TargetUid},
                Req0
            ),
            _ = State,
            elib_response:success(Req0, normalize_result(Result), <<"Owner 已转移"/utf8>>);
        {error, {Code, Msg}} ->
            elib_response:error(Req0, Msg, Code)
    end.

%% ------------------------------------------------------------------
%% 写：成员 suspend / restore / remove
%% ------------------------------------------------------------------

-spec member_action(binary(), cowboy_req:req(), map(), suspend | restore | remove) ->
    cowboy_req:req().
member_action(<<"POST">>, Req0, State, Action) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case {parse_org_id(Req0), parse_user_id(Req0)} of
                {{error, Msg}, _} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {_, {error, Msg}} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {{ok, OrgId}, {ok, TargetUid}} ->
                    Fun =
                        case Action of
                            suspend -> fun organization_admin_logic:admin_member_suspend/3;
                            restore -> fun organization_admin_logic:admin_member_restore/3;
                            remove -> fun organization_admin_logic:admin_member_remove/3
                        end,
                    case Fun(AdmUserId, OrgId, TargetUid) of
                        {ok, Result} ->
                            audit(
                                AdmUserId,
                                OrgId,
                                <<"member_", (atom_to_binary(Action))/binary>>,
                                #{<<"target_user_id">> => TargetUid},
                                Req0
                            ),
                            elib_response:success(Req0, normalize_result(Result));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
member_action(_, Req0, _State, _Action) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 写：邀请 create / cancel
%% ------------------------------------------------------------------

-spec invitation_create(cowboy_req:req(), map()) -> cowboy_req:req().
invitation_create(Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            invitation_create_write(Req0, AdmUserId, OrgId, Data)
                    end
            end
    end.

invitation_create_write(Req0, AdmUserId, OrgId, Data) ->
    case parse_tsid_map(maps:get(<<"target_user_id">>, Data, <<>>)) of
        {ok, TargetUid} ->
            ExpiresAt = normalize_expires_at(maps:get(<<"expires_at">>, Data, undefined)),
            case
                organization_admin_logic:admin_invitation_create(
                    AdmUserId, OrgId, TargetUid, ExpiresAt
                )
            of
                {ok, View} ->
                    audit(
                        AdmUserId,
                        OrgId,
                        <<"invitation_create">>,
                        #{<<"target_user_id">> => TargetUid},
                        Req0
                    ),
                    elib_response:success(Req0, normalize_invitation_row(View));
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        error ->
            elib_response:error(Req0, <<"target_user_id 必须是正整数"/utf8>>, ?ERR_BAD_REQUEST)
    end.

normalize_expires_at(Value) ->
    case elib_cnv:safe_to_integer(Value) of
        N when is_integer(N), N > 0 -> N;
        _ -> undefined
    end.

-spec invitation_cancel_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
invitation_cancel_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case {parse_org_id(Req0), parse_invitation_id(Req0)} of
                {{error, Msg}, _} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {_, {error, Msg}} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {{ok, OrgId}, {ok, InvitationId}} ->
                    case
                        organization_admin_logic:admin_invitation_cancel(
                            AdmUserId, OrgId, InvitationId
                        )
                    of
                        {ok, View} ->
                            audit(
                                AdmUserId,
                                OrgId,
                                <<"invitation_cancel">>,
                                #{<<"invitation_id">> => InvitationId},
                                Req0
                            ),
                            elib_response:success(Req0, normalize_invitation_row(View));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
invitation_cancel_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ------------------------------------------------------------------
%% 写：部门 create / rename / move / archive
%% ------------------------------------------------------------------

-spec department_create(cowboy_req:req(), map()) -> cowboy_req:req().
department_create(Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_org_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, OrgId} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            case
                                organization_admin_logic:admin_department_create(
                                    AdmUserId, OrgId, Data
                                )
                            of
                                {ok, Row} ->
                                    audit(
                                        AdmUserId,
                                        OrgId,
                                        <<"department_create">>,
                                        #{
                                            <<"name">> => maps:get(<<"name">>, Data, <<>>),
                                            <<"parent_id">> =>
                                                to_json_value(maps:get(<<"parent_id">>, Data, null))
                                        },
                                        Req0
                                    ),
                                    elib_response:success(Req0, normalize_department_row(Row));
                                {error, {Code, Msg}} ->
                                    elib_response:error(Req0, Msg, Code)
                            end
                    end
            end
    end.

-spec department_rename_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
department_rename_action(<<"POST">>, Req0, State) ->
    department_write_with_body(
        Req0, State, rename, fun organization_admin_logic:admin_department_rename/4
    );
department_rename_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec department_move_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
department_move_action(<<"POST">>, Req0, State) ->
    department_write_with_body(
        Req0, State, move, fun organization_admin_logic:admin_department_move/4
    );
department_move_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec department_archive_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
department_archive_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case {parse_org_id(Req0), parse_department_id(Req0)} of
                {{error, Msg}, _} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {_, {error, Msg}} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {{ok, OrgId}, {ok, DeptId}} ->
                    case
                        organization_admin_logic:admin_department_archive(
                            AdmUserId, OrgId, DeptId
                        )
                    of
                        {ok, Row} ->
                            audit(
                                AdmUserId,
                                OrgId,
                                <<"department_archive">>,
                                #{<<"department_id">> => DeptId},
                                Req0
                            ),
                            elib_response:success(Req0, normalize_department_row(Row));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
department_archive_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec department_write_with_body(cowboy_req:req(), map(), rename | move, fun()) ->
    cowboy_req:req().
department_write_with_body(Req0, State, Kind, Fun) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case {parse_org_id(Req0), parse_department_id(Req0)} of
                {{error, Msg}, _} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {_, {error, Msg}} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {{ok, OrgId}, {ok, DeptId}} ->
                    case read_body_map(Req0) of
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                        {ok, Data} ->
                            department_write_apply(Req0, AdmUserId, OrgId, DeptId, Kind, Fun, Data)
                    end
            end
    end.

department_write_apply(Req0, AdmUserId, OrgId, DeptId, Kind, Fun, Data) ->
    case Fun(AdmUserId, OrgId, DeptId, Data) of
        {ok, Row} ->
            audit(
                AdmUserId,
                OrgId,
                <<"department_", (atom_to_binary(Kind))/binary>>,
                #{<<"department_id">> => DeptId, <<"request">> => to_json_value(Data)},
                Req0
            ),
            elib_response:success(Req0, normalize_department_row(Row));
        {error, {Code, Msg}} ->
            elib_response:error(Req0, Msg, Code)
    end.

%% ===================================================================
%% 参数解析（TSID string→int；路径绑定与 body 通用）
%% ===================================================================

-spec parse_org_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_org_id(Req0) ->
    parse_binding(organization_id, <<"Organization ID 不能为空"/utf8>>, Req0).

-spec parse_user_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_user_id(Req0) ->
    parse_binding(user_id, <<"用户 ID 不能为空"/utf8>>, Req0).

-spec parse_invitation_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_invitation_id(Req0) ->
    parse_binding(invitation_id, <<"邀请 ID 不能为空"/utf8>>, Req0).

-spec parse_department_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_department_id(Req0) ->
    parse_binding(department_id, <<"部门 ID 不能为空"/utf8>>, Req0).

-spec parse_binding(atom(), binary(), cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_binding(Key, EmptyMsg, Req0) ->
    case cowboy_req:binding(Key, Req0, <<>>) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 ->
            parse_tsid_map(Bin, EmptyMsg);
        _ ->
            {error, EmptyMsg}
    end.

%% @doc TSID（string 或 int）→ 正整数；非法形状返回 error。
-spec parse_tsid_map(term()) -> {ok, integer()} | error.
parse_tsid_map(Value) ->
    case parse_tsid_map(Value, <<>>) of
        {ok, Id} -> {ok, Id};
        _ -> error
    end.

-spec parse_tsid_map(term(), binary()) -> {ok, integer()} | {error, binary()}.
parse_tsid_map(Value, _EmptyMsg) when is_integer(Value), Value > 0 ->
    {ok, Value};
parse_tsid_map(Value, _EmptyMsg) when is_binary(Value) ->
    case catch binary_to_integer(string:trim(Value)) of
        Id when is_integer(Id), Id > 0 ->
            {ok, Id};
        _ ->
            {error, <<"ID 格式错误"/utf8>>}
    end;
parse_tsid_map(_, EmptyMsg) ->
    {error, EmptyMsg}.

%% @doc 读 JSON body（空 body = #{}）；非 JSON body → 400。
-spec read_body_map(cowboy_req:req()) -> {ok, map()} | {error, binary()}.
read_body_map(Req0) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    case byte_size(Body) of
        0 ->
            {ok, #{}};
        _ ->
            try jsone:decode(Body, [{object_format, map}]) of
                Data when is_map(Data) ->
                    {ok, Data};
                _ ->
                    {error, <<"请求体必须是 JSON 对象"/utf8>>}
            catch
                _:_ ->
                    {error, <<"请求体必须是合法 JSON"/utf8>>}
            end
    end.

-spec normalize_status(binary()) -> binary() | all.
normalize_status(<<"all">>) -> all;
normalize_status(Bin) when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
normalize_status(_) -> all.

-spec normalize_limit(integer() | binary()) -> integer().
normalize_limit(V) when is_integer(V), V > 0 -> V;
normalize_limit(V) when is_binary(V) ->
    case elib_cnv:safe_to_integer(V) of
        N when is_integer(N), N > 0 -> N;
        _ -> 20
    end;
normalize_limit(_) ->
    20.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 出站归一化（TSID 一律 string 下发，防 JS 精度丢失）
%% ===================================================================

-define(ORG_ID_KEYS, [<<"id">>, <<"owner_id">>]).
-define(ORG_ROW_ID_KEYS, [<<"id">>, <<"owner_id">>, <<"organization_id">>]).
-define(MEMBER_ID_KEYS, [<<"organization_id">>, <<"user_id">>, <<"invited_by">>]).
-define(WORKSPACE_ID_KEYS, [<<"id">>, <<"owner_id">>, <<"organization_id">>]).
%% department_pg 行键为原子；invitation_admin_logic 视图键为原子
-define(DEPT_ID_KEYS, [id, organization_id, parent_id, created_by_user_id, updated_by_user_id]).
-define(INVITE_ID_KEYS, [
    invitation_id, organization_id, target_user_id, invited_by
]).

-spec normalize_org_page(map()) -> map().
normalize_org_page(P) ->
    normalize_page(P, fun normalize_org_row/1).

-spec normalize_member_page(map()) -> map().
normalize_member_page(P) ->
    normalize_page(P, fun normalize_member_row/1).

-spec normalize_workspace_page(map()) -> map().
normalize_workspace_page(P) ->
    normalize_page(P, fun normalize_workspace_row/1).

-spec normalize_page(map(), fun((map()) -> map())) -> map().
normalize_page(#{list := List} = P, Fun) ->
    P#{list => [Fun(Item) || Item <- List]};
normalize_page(P, _Fun) ->
    P.

-spec normalize_org_row(map()) -> map().
normalize_org_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, ?ORG_ROW_ID_KEYS).

-spec normalize_org_detail(map()) -> map().
normalize_org_detail(Detail) ->
    elib_id:tsid_keys_to_bin(Detail, ?ORG_ID_KEYS ++ [<<"organization_id">>]).

-spec normalize_member_row(map()) -> map().
normalize_member_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, ?MEMBER_ID_KEYS).

-spec normalize_workspace_row(map()) -> map().
normalize_workspace_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, ?WORKSPACE_ID_KEYS).

-spec normalize_invitation_row(map()) -> map().
normalize_invitation_row(Row) ->
    Bin = elib_id:tsid_keys_to_bin(Row, ?INVITE_ID_KEYS),
    atom_keys_to_bin(Bin).

%% mutation 返回值（atom 键 map）出站：键转 binary + TSID 转 string
-define(RESULT_ID_KEYS, [
    <<"id">>,
    <<"organization_id">>,
    <<"user_id">>,
    <<"subject_user_id">>,
    <<"target_user_id">>,
    <<"owner_id">>,
    <<"previous_owner_id">>,
    <<"invitation_id">>,
    <<"department_id">>
]).

-spec normalize_result(map()) -> map().
normalize_result(Result) ->
    Bin = atom_keys_to_bin(Result),
    elib_id:tsid_keys_to_bin(Bin, ?RESULT_ID_KEYS).

-spec normalize_department_row(map()) -> map().
normalize_department_row(Row) ->
    Bin = elib_id:tsid_keys_to_bin(Row, ?DEPT_ID_KEYS),
    atom_keys_to_bin(Bin).

%% atom 键出站统一转 binary（混合键 map 的 JSON 友好形态）
-spec atom_keys_to_bin(map()) -> map().
atom_keys_to_bin(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{to_binary_key(K) => to_json_value(V)}
        end,
        #{},
        Map
    ).

-spec to_binary_key(atom() | binary()) -> binary().
to_binary_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
to_binary_key(K) when is_binary(K) -> K.

%% 递归值出站净化：整数 TSID 不在白名单键内的场景保持原样；
%% atom/null 等 JSON 不友好值收敛为 JSON 可表达形态。
-spec to_json_value(term()) -> term().
to_json_value(undefined) ->
    null;
to_json_value(null) ->
    null;
to_json_value(true) ->
    true;
to_json_value(false) ->
    false;
to_json_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_json_value(V) when is_map(V) ->
    maps:fold(
        fun(K, V2, Acc) -> Acc#{to_binary_key(K) => to_json_value(V2)} end,
        #{},
        V
    );
to_json_value(V) when is_list(V) ->
    [to_json_value(Item) || Item <- V];
to_json_value(V) ->
    V.

%% ===================================================================
%% 审计（镜像 adm_workspace_handler:audit_workspace_governance 模式：
%% 审计失败不阻断已完成的业务操作）
%% ===================================================================

-spec audit(integer(), integer(), binary(), map(), cowboy_req:req()) -> ok.
audit(AdmUserId, OrgId, Action, Extra, Req0) ->
    Detail = maps:merge(
        #{
            <<"organization_id">> => OrgId,
            <<"action">> => Action
        },
        Extra
    ),
    try
        _ = adm_operation_log_ds:insert(
            AdmUserId,
            <<"organization_", Action/binary>>,
            OrgId,
            <<"organization">>,
            Detail,
            elib_req:peer_ip(Req0)
        ),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("organization governance audit failed: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.
