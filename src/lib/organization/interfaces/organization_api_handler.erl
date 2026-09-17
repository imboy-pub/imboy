-module(organization_api_handler).

%% ORG-10（API Contract and Router Integration）：Organization V1 v2 面
%% 的**集成 handler**。只做协议面接线：method 分派、binding/body/query
%% 参数提取、调用已交付的应用层 command（ORG-03 invitation / ORG-04
%% department / ORG-05 default workspace）并把结果映射为 HTTP 信封。
%%
%% 【禁止】在此实现任何业务裁决：资格/状态/幂等/审计语义全部由
%% src/lib/organization/application/* 冻结。错误映射表（map_dept_error/1）
%% 只做 department 应用层错误形状 → HTTP 状态码的**机械翻译**，
%% 不新增、不吞并、不改写任何业务码。
%%
%% 术语冻结（Core Contract C18）：invite / accept / reject / revoke /
%% member / department / archive / restore 严格区分；既有
%% POST /organizations/:id/members 是 legacy direct-add adapter（C11
%% TRANSITION），本 handler 不重复暴露 direct-add 语义。

-behavior(cowboy_rest).

-export([init/2, handle_action/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    {ok, handle_action(Action, Req0, State), State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
%% —— invitation（ORG-03）：目标用户自己的待决邀请列表 ——
handle_action(invitation_mine, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> invitation_mine(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET">>)
    end;
%% —— invitation：Org 治理面（GET 列表 / POST 邀请 同路径双语义）——
handle_action(invitation_collection, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> invitation_list(Req0, State);
        <<"POST">> -> invitation_create(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST">>)
    end;
handle_action(invitation_accept, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invitation_accept(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(invitation_reject, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invitation_reject(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(invitation_revoke, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invitation_revoke(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
%% —— department（ORG-04）：目录 CRUD / move / archive ——
handle_action(department_collection, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> department_list(Req0, State);
        <<"POST">> -> department_create(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST">>)
    end;
handle_action(department_item, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> department_detail(Req0, State);
        <<"PATCH">> -> department_update(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, PATCH">>)
    end;
handle_action(department_move, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> department_move(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(department_archive, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> department_archive(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
%% —— department：成员 / 局部目录 admin ——
handle_action(department_member_collection, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> department_member_list(Req0, State);
        <<"POST">> -> department_member_add(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST">>)
    end;
handle_action(department_member_item, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"DELETE">> -> department_member_remove(Req0, State);
        _ -> method_not_allowed(Req0, <<"DELETE">>)
    end;
handle_action(department_member_admin, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"PUT">> -> department_member_admin(Req0, State);
        _ -> method_not_allowed(Req0, <<"PUT">>)
    end;
%% —— default workspace（ORG-05）：GET 读 / POST set / DELETE clear ——
handle_action(default_workspace, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> default_workspace_get(Req0, State);
        <<"POST">> -> default_workspace_set(Req0, State);
        <<"DELETE">> -> default_workspace_clear(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST, DELETE">>)
    end.

%% ===================================================================
%% invitation（organization_invitation_app；{error, {HTTPCode, Msg}} 直映射）
%% ===================================================================

invitation_mine(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Opts = #{
        status => query_atom(Req0, <<"status">>),
        limit => query_limit(Req0)
    },
    respond(Req0, organization_invitation_app:list_for_target(Uid, Opts)).

invitation_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        Opts = #{
            status => query_atom(Req0, <<"status">>),
            limit => query_limit(Req0)
        },
        respond(Req0, organization_invitation_app:list_for_org(Uid, OrgId, Opts))
    end).

invitation_create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        TargetUid = positive_id(maps:get(<<"user_id">>, Params, undefined)),
        Opts =
            case maps:get(<<"expires_at">>, Params, undefined) of
                undefined ->
                    #{};
                ExpiresAt ->
                    #{expires_at => elib_cnv:safe_to_integer(ExpiresAt)}
            end,
        respond(Req0, organization_invitation_app:create(Uid, OrgId, TargetUid, Opts))
    end).

invitation_accept(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        Token = maps:get(<<"token">>, Params, undefined),
        respond(Req0, organization_invitation_app:accept(Uid, OrgId, Token, #{}))
    end).

invitation_reject(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_invitation_id(Req0, fun(OrgId, InvitationId) ->
        respond(Req0, organization_invitation_app:reject(Uid, OrgId, InvitationId))
    end).

invitation_revoke(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_invitation_id(Req0, fun(OrgId, InvitationId) ->
        respond(Req0, organization_invitation_app:revoke(Uid, OrgId, InvitationId))
    end).

%% ===================================================================
%% department（organization_department_app；错误形状为 {ReasonAtom, ...}，
%% 经 map_dept_error/1 机械映射为 HTTP 状态）
%% ===================================================================

department_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        Params = #{
            actor_user_id => Uid,
            status => query_atom(Req0, <<"status">>)
        },
        respond_dept(Req0, organization_department_app:list_departments(OrgId, Params))
    end).

department_create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        Params = Body#{actor_user_id => Uid},
        respond_dept(Req0, organization_department_app:create_department(OrgId, Params))
    end).

department_detail(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = #{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:get_department(OrgId, Params))
    end).

department_update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = Body#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:update_department(OrgId, Params))
    end).

department_move(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = Body#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:move_department(OrgId, Params))
    end).

department_archive(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = Body#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:archive_department(OrgId, Params))
    end).

department_member_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = #{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:list_members(OrgId, Params))
    end).

department_member_add(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = Body#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:add_member(OrgId, Params))
    end).

department_member_remove(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_department_member_id(Req0, fun(OrgId, DeptId, UserId) ->
        Params = #{actor_user_id => Uid, department_id => DeptId, user_id => UserId},
        respond_dept(Req0, organization_department_app:remove_member(OrgId, Params))
    end).

department_member_admin(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_member_id(Req0, fun(OrgId, DeptId, UserId) ->
        Params =
            Body#{
                actor_user_id => Uid,
                department_id => DeptId,
                user_id => UserId,
                admin => body_bool(maps:get(<<"admin">>, Body, undefined))
            },
        respond_dept(Req0, organization_department_app:set_admin(OrgId, Params))
    end).

%% 局部目录 admin 开关：JSON 原生布尔透传；字符串形态（"true"/"false"）
%% 容错归一；其余交应用层 is_boolean 门裁决（返回 invalid_admin_flag）。
body_bool(true) ->
    true;
body_bool(false) ->
    false;
body_bool(<<"true">>) ->
    true;
body_bool(<<"false">>) ->
    false;
body_bool(V) ->
    V.

%% ===================================================================
%% default workspace（organization_default_workspace_app）
%% get/1 的 {error, not_set} 是**可空语义**（应用层冻结注释），映射为
%% default_workspace_id => null 的成功信封；其余错误 500 兜底。
%% ===================================================================

default_workspace_get(Req0, State) ->
    _Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        case organization_default_workspace_app:get(OrgId) of
            {ok, WsId} ->
                elib_response:success(Req0, #{
                    organization_id => OrgId, default_workspace_id => WsId
                });
            {error, not_set} ->
                elib_response:success(Req0, #{
                    organization_id => OrgId, default_workspace_id => null
                });
            {error, _Reason} ->
                elib_response:error(Req0, <<"读取默认 Workspace 失败，请稍后重试"/utf8>>, 500)
        end
    end).

default_workspace_set(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        WsId = positive_id(maps:get(<<"workspace_id">>, Params, undefined)),
        respond(Req0, organization_default_workspace_app:set(Uid, OrgId, WsId))
    end).

default_workspace_clear(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        respond(Req0, organization_default_workspace_app:clear(Uid, OrgId))
    end).

%% ===================================================================
%% 协议面辅助（与 organization_handler 同口径）
%% ===================================================================

respond(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond(Req0, {error, {Code, Msg}}) when is_integer(Code), is_binary(Msg) ->
    elib_response:error(Req0, Msg, Code);
respond(Req0, {error, _Reason}) ->
    elib_response:error(Req0, <<"请求处理失败，请稍后重试"/utf8>>, 500).

respond_dept(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond_dept(Req0, {error, Reason}) ->
    {Code, Msg} = map_dept_error(Reason),
    elib_response:error(Req0, Msg, Code).

%% department 应用层错误形状 → HTTP 状态机械翻译。
%% 400 = 参数/形状/约束非法；403 = 调用者资格不足；404 = 目标不存在；
%% 409 = 目标状态冲突；其余 500 兜底（不吞真实故障）。
map_dept_error(not_found) ->
    {404, <<"部门不存在"/utf8>>};
map_dept_error({parent_not_found, _}) ->
    {404, <<"父部门不存在"/utf8>>};
map_dept_error({new_parent_not_found, _}) ->
    {404, <<"目标父部门不存在"/utf8>>};
map_dept_error({actor_not_member, _}) ->
    {403, <<"仅 Organization 成员可执行该操作"/utf8>>};
map_dept_error({actor_not_active, _, _}) ->
    {403, <<"Organization 成员资格非 active"/utf8>>};
map_dept_error({actor_not_permitted, _}) ->
    {403, <<"无权执行该部门操作"/utf8>>};
map_dept_error({parent_archived, _}) ->
    {409, <<"父部门已归档"/utf8>>};
map_dept_error({new_parent_archived, _}) ->
    {409, <<"目标父部门已归档"/utf8>>};
map_dept_error({cycle, _, _}) ->
    {409, <<"部门层级禁止成环"/utf8>>};
map_dept_error({invalid_transition, _, _}) ->
    {409, <<"部门状态迁移非法"/utf8>>};
%% 并发冲突（expected-version CAS 不符）：organization_department_pg 的乐观锁
%% 拒绝（UPDATE 影响行数 0）经应用层透传为 {error, conflict}。必须稳定映射 409
%% （else 兜底 400 会把「并发写碰撞」伪装成客户端参数错误）。
map_dept_error(conflict) ->
    {409, <<"部门已被并发修改，请刷新版本后重试"/utf8>>};
map_dept_error(_) ->
    {400, <<"请求参数非法"/utf8>>}.

with_organization_id(Req0, Fun) ->
    case positive_binding(organization_id, Req0) of
        {ok, OrgId} -> Fun(OrgId);
        error -> elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400)
    end.

with_invitation_id(Req0, Fun) ->
    case {positive_binding(organization_id, Req0), positive_binding(invitation_id, Req0)} of
        {{ok, OrgId}, {ok, InvitationId}} ->
            Fun(OrgId, InvitationId);
        {error, _} ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400);
        _ ->
            elib_response:error(Req0, <<"invitation_id 必须是正整数"/utf8>>, 400)
    end.

with_department_id(Req0, Fun) ->
    case {positive_binding(organization_id, Req0), positive_binding(department_id, Req0)} of
        {{ok, OrgId}, {ok, DeptId}} ->
            Fun(OrgId, DeptId);
        {error, _} ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400);
        _ ->
            elib_response:error(Req0, <<"department_id 必须是正整数"/utf8>>, 400)
    end.

with_department_member_id(Req0, Fun) ->
    case
        {
            positive_binding(organization_id, Req0),
            positive_binding(department_id, Req0),
            positive_binding(user_id, Req0)
        }
    of
        {{ok, OrgId}, {ok, DeptId}, {ok, UserId}} ->
            Fun(OrgId, DeptId, UserId);
        {{error, _}, _, _} ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400);
        {{ok, _}, {error, _}, _} ->
            elib_response:error(Req0, <<"department_id 必须是正整数"/utf8>>, 400);
        _ ->
            elib_response:error(Req0, <<"user_id 必须是正整数"/utf8>>, 400)
    end.

positive_binding(Name, Req0) ->
    case positive_id(cowboy_req:binding(Name, Req0)) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        _ -> error
    end.

positive_id(Value) ->
    case elib_cnv:safe_to_integer(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> 0
    end.

%% query 参数：status 可选（未带 = undefined，交应用层默认）；
%% limit 可选（1..100 截断，默认 20 与 list_for_target 默认一致）。
query_atom(Req0, Key) ->
    case elib_param:get(Key, Req0, <<>>) of
        <<>> -> undefined;
        Bin -> binary_to_atom(Bin, utf8)
    end.

query_limit(Req0) ->
    case elib_cnv:safe_to_integer(elib_param:get(<<"limit">>, Req0, <<>>)) of
        N when is_integer(N), N >= 1, N =< 100 -> N;
        _ -> 20
    end.

method_not_allowed(Req0, Allow) ->
    cowboy_req:reply(
        405,
        #{<<"allow">> => Allow, <<"content-type">> => <<"text/plain; charset=utf-8">>},
        <<"Method Not Allowed">>,
        Req0
    ).
