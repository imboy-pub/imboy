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
    end;
%% —— invite code（GZAPP-01）：org 可复用加入凭证，同路径按 method 分派 ——
%% GET=读当前 active 码 / POST=生成（重新生成即旧码失效）/ DELETE=撤销
handle_action(invite_code, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> invite_code_get(Req0, State);
        <<"POST">> -> invite_code_create(Req0, State);
        <<"DELETE">> -> invite_code_revoke(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST, DELETE">>)
    end;
%% —— invite code join（GZAPP-01）：任意登录用户凭码加入 org
%% （统一走 organization_join_orchestrator 编排）——
handle_action(invite_code_join, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invite_code_join(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
%% —— invite code preview / code-only join（GZAPP-J11）：码全局唯一即凭据，
%% 无需 path orgId（扫码 / 单码手输加入路径）；业务语义与 invite_code_join 同构
handle_action(invite_code_preview, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invite_code_preview(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(invite_code_join_by_code, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> invite_code_join_by_code(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
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
    respond_list(Req0, organization_invitation_app:list_for_target(Uid, Opts)).

invitation_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        Opts = #{
            status => query_atom(Req0, <<"status">>),
            limit => query_limit(Req0)
        },
        respond_list(Req0, organization_invitation_app:list_for_org(Uid, OrgId, Opts))
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
        Opts = #{membership_hook => fun organization_join_orchestrator:membership_hook/2},
        case maps:get(<<"token">>, Params, undefined) of
            Token when is_binary(Token), Token =/= <<>> ->
                respond(Req0, organization_invitation_app:accept(Uid, OrgId, Token, Opts));
            _ ->
                %% 免口令（P0 定向邀请直达）：JWT 身份即 target 凭据。
                respond(Req0, organization_invitation_app:accept_targeted(Uid, OrgId, Opts))
        end
    end).

%% C11 收口（GZAPP-01 编排化）：邀请首次消费成功后**同事务**完成统一加入
%% 编排——org member(role=member) → 默认 Workspace member → 全员群
%% (General) → 公告频道 (Announcements) 订阅，全部幂等可重放；默认 WS
%% 缺失只写 org member（不阻塞）。hook 失败 → 整个 accept 事务回滚
%% （消费 + 成员变更原子，见 organization_invitation_app 冻结注释；
%% 编排实现冻结于 organization_join_orchestrator）。

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
        %% 应用层约定：status 键**缺省**才落 all 默认；显式 undefined 会被判
        %% invalid_status。查询串未带 status 时保持缺键。
        Params0 = #{actor_user_id => Uid},
        Params =
            case query_atom(Req0, <<"status">>) of
                undefined -> Params0;
                Status -> Params0#{status => Status}
            end,
        respond_list_dept(Req0, organization_department_app:list_departments(OrgId, Params))
    end).

department_create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        Params = (dept_body_params(Body))#{actor_user_id => Uid},
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
        Params = (dept_body_params(Body))#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:update_department(OrgId, Params))
    end).

department_move(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = (dept_body_params(Body))#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:move_department(OrgId, Params))
    end).

department_archive(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = (dept_body_params(Body))#{actor_user_id => Uid, department_id => DeptId},
        respond_dept(Req0, organization_department_app:archive_department(OrgId, Params))
    end).

department_member_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = #{actor_user_id => Uid, department_id => DeptId},
        respond_list_dept(Req0, organization_department_app:list_members(OrgId, Params))
    end).

department_member_add(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = elib_param:post(Req0),
    with_department_id(Req0, fun(OrgId, DeptId) ->
        Params = (dept_body_params(Body))#{actor_user_id => Uid, department_id => DeptId},
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

%% ORG-BACKEND-GAP2：elib_param:post 产物是二进制键 map，部门 app 层读原子键；
%% 这里做 handler→app 的唯一一次形状归一。未知键丢弃；垃圾值原样透传，
%% 交应用层 invalid_* 裁决（经 map_dept_error 后仍是 400，但可诊断）。
dept_body_params(Body) when is_map(Body) ->
    maps:fold(
        fun
            (<<"name">>, V, Acc) -> Acc#{name => V};
            (<<"parent_id">>, V, Acc) -> Acc#{parent_id => body_parent_id(V)};
            (<<"expected_version">>, V, Acc) -> Acc#{expected_version => body_version(V)};
            (<<"user_id">>, V, Acc) -> Acc#{user_id => positive_id(V)};
            (_, _, Acc) -> Acc
        end,
        #{},
        Body
    ).

%% 建部门/移动：JSON null、缺省与空串都归一为 null（根/提根语义）。
body_parent_id(null) ->
    null;
body_parent_id(undefined) ->
    null;
body_parent_id(<<>>) ->
    null;
body_parent_id(V) ->
    case elib_cnv:safe_to_integer(V) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> V
    end.

body_version(undefined) ->
    undefined;
body_version(V) ->
    case elib_cnv:safe_to_integer(V) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> V
    end.

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
%% invite code（organization_invite_code_app；{error, {HTTPCode, Msg}}
%% 直映射；业务码 981/982 沿用 workspace 团队码 envelope 口径）
%% ===================================================================

%% 治理面读当前 active 码：无 active 码是可空语义（code => null 成功信封），
%% 与 default_workspace_get 的 not_set 同口径。
invite_code_get(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        case organization_invite_code_app:get(Uid, OrgId) of
            {ok, View} ->
                elib_response:success(Req0, View);
            {error, not_found} ->
                elib_response:success(Req0, #{
                    organization_id => OrgId, code => null, status => null
                });
            {error, {Code, Msg}} ->
                elib_response:error(Req0, Msg, Code)
        end
    end).

invite_code_create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        respond(Req0, organization_invite_code_app:create(Uid, OrgId, #{}))
    end).

invite_code_revoke(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_organization_id(Req0, fun(OrgId) ->
        respond(Req0, organization_invite_code_app:revoke(Uid, OrgId))
    end).

%% 凭码加入：code 非 binary / trim 空统一 981（app 层裁决），handler 只透传。
%% payload 镜像 workspace join：{status => joined|unchanged, join => 编排摘要}。
invite_code_join(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        Code = maps:get(<<"code">>, Params, undefined),
        case organization_invite_code_app:join_by_code(Uid, OrgId, Code) of
            {ok, joined, Summary} ->
                elib_response:success(Req0, #{status => joined, join => Summary});
            {ok, unchanged, Summary} ->
                elib_response:success(Req0, #{status => unchanged, join => Summary});
            {error, {Code2, Msg}} ->
                elib_response:error(Req0, Msg, Code2)
        end
    end).

%% 凭码预览目标组织（GZAPP-J11）：{organization_id, name}——扫码/单码
%% 加入的「确认加入 XX 企业」步骤；码校验 981/982 与 join 同口径。
invite_code_preview(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    Code = maps:get(<<"code">>, Params, undefined),
    respond(Req0, organization_invite_code_app:preview_by_code(Uid, Code)).

%% 凭码加入（无需 orgId，GZAPP-J11）：payload 与 invite_code_join 完全同构。
invite_code_join_by_code(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    Code = maps:get(<<"code">>, Params, undefined),
    case organization_invite_code_app:join_by_code_only(Uid, Code) of
        {ok, joined, Summary} ->
            elib_response:success(Req0, #{status => joined, join => Summary});
        {ok, unchanged, Summary} ->
            elib_response:success(Req0, #{status => unchanged, join => Summary});
        {error, {Code2, Msg}} ->
            elib_response:error(Req0, Msg, Code2)
    end.

%% ===================================================================
%% 协议面辅助（与 organization_handler 同口径）
%% ===================================================================

respond(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond(Req0, {error, {Code, Msg}}) when is_integer(Code), is_binary(Msg) ->
    elib_response:error(Req0, Msg, Code);
respond(Req0, {error, _Reason}) ->
    elib_response:error(Req0, <<"请求处理失败，请稍后重试"/utf8>>, 500).

%% v2 列表端点信封约定：裸列表包装为 #{list => Rows}。
%% App 端 IMBoyHttpResponse.payloadList/2 只认 list 键（与
%% /organizations/mine 的 map 形状一致）；此前裸数组 payload 会被
%% App 解析成恒空列表（REAL BUG 2026-09-23：邀请页/部门页恒空）。
respond_list(Req0, {ok, Rows}) when is_list(Rows) ->
    respond(Req0, {ok, #{list => Rows}});
respond_list(Req0, Other) ->
    respond(Req0, Other).

respond_dept(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond_dept(Req0, {error, Reason}) ->
    {Code, Msg} = map_dept_error(Reason),
    elib_response:error(Req0, Msg, Code).

%% department 列表端点专用：错误走 map_dept_error 机械翻译，
%% 成功裸列表包装 #{list => Rows}（信封约定见 respond_list/2 注释）。
respond_list_dept(Req0, {ok, Rows}) when is_list(Rows) ->
    respond_dept(Req0, {ok, #{list => Rows}});
respond_list_dept(Req0, Other) ->
    respond_dept(Req0, Other).

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
%% 形状/参数类：给具体文案（否则全部落进 else 兜底，客户端无从分辨）。
map_dept_error({invalid_name, _}) ->
    {400, <<"部门名称非法"/utf8>>};
map_dept_error({invalid_status, _}) ->
    {400, <<"status 参数非法"/utf8>>};
map_dept_error({invalid_parent_id, _}) ->
    {400, <<"parent_id 非法"/utf8>>};
map_dept_error({invalid_expected_version, _}) ->
    {400, <<"expected_version 非法"/utf8>>};
map_dept_error({invalid_user_id, _}) ->
    {400, <<"user_id 非法"/utf8>>};
map_dept_error({invalid_admin_flag, _}) ->
    {400, <<"admin 标志非法"/utf8>>};
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
        {error, _, _} ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400);
        {{ok, _}, error, _} ->
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
