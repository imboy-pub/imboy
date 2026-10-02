%%% @doc Organization Department 用例层（Core Contract C10/C15；ORG-04）。
%%%
%%% 职责：部门 create/update/move/archive/list、部门成员 add/remove、
%%% 部门管理员 set/remove。目录读取要求同 Org active membership；
%%% 部门结构 create/update/move/archive 仅允许本 Org active owner/admin。
%%% 成员级操作额外放行**该部门**的
%%% 局部目录管理员（department_admin，department_member.is_admin 标记）。
%%%
%%% C15 铁律：department_admin 是**局部目录角色**，不产生任何 Workspace/CS/
%%% Agent/资源权限。本模块没有也不得有「授权」原语；archive 只改目录状态，
%%% 绝不级联撤销任何 Org/Workspace 权限（C10）。
%%%
%%% 幂等（ORG-04 卡）：重复 archive / member add / remove / admin set 均返回
%%% 当前状态，不产生第二次写入。
%%%
%%% 边界：本模块不注册路由（router 归 ORG-10 集成）；不实现 Employee 表；
%%% 不映射 class_staff/learner/Seat。
-module(organization_department_app).

-export([
    create_department/2,
    update_department/2,
    move_department/2,
    archive_department/2,
    list_departments/2,
    get_department/2,
    add_member/2,
    remove_member/2,
    set_admin/2,
    list_members/2
]).

-define(PG, organization_department_pg).
-define(DOMAIN, organization_department).

%% department 行出站键（读面白名单）
-define(DEPT_KEYS, [
    id,
    organization_id,
    parent_id,
    name,
    status,
    version,
    created_at,
    updated_at
]).

%% member 行出站键
-define(MEMBER_KEYS, [
    organization_id,
    department_id,
    user_id,
    is_admin,
    created_at,
    updated_at
]).

%% ===================================================================
%% create
%% ===================================================================

%% @doc 建部门。Params：name（必填）、parent_id（可空=根）、actor_user_id（必填）。
%% actor 必须是本 Org 的 active owner/admin；父部门必须同 Org 且 active。
-spec create_department(integer(), map()) -> {ok, map()} | {error, term()}.
create_department(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_structure_manager(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            Name = maps:get(name, Params, undefined),
            case ?DOMAIN:valid_name(Name) of
                {error, _} = Err ->
                    Err;
                ok ->
                    ParentId = maps:get(parent_id, Params, null),
                    create_gate(OrgId, ParentId, Name, ActorId, Params)
            end
    end;
create_department(_OrgId, _Params) ->
    {error, {invalid_argument, create_department}}.

create_gate(OrgId, null, Name, ActorId, _Params) ->
    insert(OrgId, null, Name, ActorId);
create_gate(OrgId, ParentId, Name, ActorId, _Params) when is_integer(ParentId) ->
    case ?PG:fetch_department(OrgId, ParentId, none) of
        {error, not_found} ->
            {error, {parent_not_found, ParentId}};
        {ok, #{status := archived}} ->
            {error, {parent_archived, ParentId}};
        {ok, _Parent} ->
            insert(OrgId, ParentId, Name, ActorId);
        {error, _} = Err ->
            Err
    end;
create_gate(_OrgId, ParentId, _Name, _ActorId, _Params) ->
    {error, {invalid_parent_id, ParentId}}.

insert(OrgId, ParentId, Name, ActorId) ->
    case ?PG:insert_department(OrgId, ParentId, trimmed(Name), ActorId, fun id/0) of
        {ok, Row} -> {ok, project_dept(Row)};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% update（改名）
%% ===================================================================

%% @doc 改名。Params：department_id、name、actor_user_id、expected_version（CAS）。
%% archived 部门禁改（archived 禁新写）。
-spec update_department(integer(), map()) -> {ok, map()} | {error, term()}.
update_department(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_structure_manager(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            DeptId = maps:get(department_id, Params, undefined),
            Name = maps:get(name, Params, undefined),
            Expected = maps:get(expected_version, Params, undefined),
            case {is_pos_int(DeptId), ?DOMAIN:valid_name(Name), is_pos_int(Expected)} of
                {false, _, _} ->
                    {error, {invalid_department_id, DeptId}};
                {_, {error, _} = Err, _} ->
                    Err;
                {_, _, false} ->
                    {error, {invalid_expected_version, Expected}};
                {true, ok, true} ->
                    update_gate(OrgId, DeptId, trimmed(Name), ActorId, Expected)
            end
    end;
update_department(_OrgId, _Params) ->
    {error, {invalid_argument, update_department}}.

update_gate(OrgId, DeptId, Name, ActorId, Expected) ->
    case ?PG:fetch_department(OrgId, DeptId, none) of
        {error, not_found} ->
            {error, not_found};
        {ok, #{status := archived}} ->
            {error, department_archived};
        {ok, _Dept} ->
            case ?PG:update_name(OrgId, DeptId, Name, ActorId, Expected) of
                ok -> fresh_dept(OrgId, DeptId);
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%% ===================================================================
%% move
%% ===================================================================

%% @doc 移动部门（换父 / 提为根）。Params：department_id、parent_id（null=根）、
%% actor_user_id、expected_version（CAS）。
%% 锁口径：单事务内按 id 全序锁定该 Org 全部部门节点后再裁决与落库
%% （锁相关树节点；同 Org 结构性写串行化，跨事务零死锁）。
%% self-parent / 祖先环 / 跨 Org 新父 / archived 新父一律拒绝。
-spec move_department(integer(), map()) -> {ok, map()} | {error, term()}.
move_department(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_structure_manager(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            DeptId = maps:get(department_id, Params, undefined),
            NewParentId = maps:get(parent_id, Params, undefined),
            Expected = maps:get(expected_version, Params, undefined),
            case {is_pos_int(DeptId), valid_parent_arg(NewParentId), is_pos_int(Expected)} of
                {false, _, _} ->
                    {error, {invalid_department_id, DeptId}};
                {_, false, _} ->
                    {error, {invalid_parent_id, NewParentId}};
                {_, _, false} ->
                    {error, {invalid_expected_version, Expected}};
                {true, true, true} ->
                    case ?PG:move_tx(OrgId, DeptId, NewParentId, ActorId, Expected) of
                        {ok, Row} -> {ok, project_dept(move_result(OrgId, DeptId, Row))};
                        {error, _} = Err -> Err
                    end
            end
    end;
move_department(_OrgId, _Params) ->
    {error, {invalid_argument, move_department}}.

valid_parent_arg(null) -> true;
valid_parent_arg(P) when is_integer(P), P > 0 -> true;
valid_parent_arg(_) -> false.

%% move 返回行缺 name/status 等列，回读一次补全投影（该读不加锁）
move_result(OrgId, DeptId, _Row) ->
    case ?PG:fetch_department(OrgId, DeptId, none) of
        {ok, Fresh} -> Fresh;
        {error, _} -> #{id => DeptId}
    end.

%% ===================================================================
%% archive
%% ===================================================================

%% @doc 归档部门：原子归档自身与全部 active 后代（纯目录状态；**不级联撤销**
%% 任何 Org/Workspace/CS/Agent 权限，不动 organization_member / workspace_member）。
%% 已归档 ⇒ 幂等成功（零写入）。Params：department_id、actor_user_id。
-spec archive_department(integer(), map()) -> {ok, map()} | {error, term()}.
archive_department(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_structure_manager(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            DeptId = maps:get(department_id, Params, undefined),
            case is_pos_int(DeptId) of
                false ->
                    {error, {invalid_department_id, DeptId}};
                true ->
                    case ?PG:archive_subtree_tx(OrgId, DeptId, ActorId, none) of
                        {ok, Dept} -> {ok, project_dept(archive_result(OrgId, DeptId, Dept))};
                        {error, _} = Err -> Err
                    end
            end
    end;
archive_department(_OrgId, _Params) ->
    {error, {invalid_argument, archive_department}}.

archive_result(OrgId, DeptId, Row) ->
    case ?PG:fetch_department(OrgId, DeptId, none) of
        {ok, Fresh} -> Fresh#{archive_idempotent => maps:get(archive_idempotent, Row, false)};
        {error, _} -> Row
    end.

%% ===================================================================
%% list / get
%% ===================================================================

%% @doc 列部门。Params：status（可选 all 缺省|active|archived）。
%% 目录读取要求同 Org active member（门先于一切读，含 not_found）。
-spec list_departments(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_departments(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_actor(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, _ActorId} ->
            do_list_departments(OrgId, Params)
    end;
list_departments(_OrgId, _Params) ->
    {error, {invalid_argument, list_departments}}.

do_list_departments(OrgId, Params) ->
    Status =
        case maps:get(status, Params, all) of
            all -> all;
            active -> active;
            archived -> archived;
            Other -> {error, {invalid_status, Other}}
        end,
    case Status of
        {error, _} = Err ->
            Err;
        S ->
            case ?PG:list_departments(OrgId, S, none) of
                {ok, Rows} -> {ok, [project_dept(R) || R <- Rows]};
                {error, _} = Err -> Err
            end
    end.

%% @doc 部门详情（含成员列表）。授权基线同 list_departments；
%% 门先于存在性判定（非成员探测部门 id 一律 403，不做租户枚举）。
-spec get_department(integer(), map()) -> {ok, map()} | {error, term()}.
get_department(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_actor(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, _ActorId} ->
            do_get_department(OrgId, Params)
    end;
get_department(_OrgId, _Params) ->
    {error, {invalid_argument, get_department}}.

do_get_department(OrgId, Params) ->
    DeptId = maps:get(department_id, Params, undefined),
    case is_pos_int(DeptId) of
        false ->
            {error, {invalid_department_id, DeptId}};
        true ->
            case ?PG:fetch_department(OrgId, DeptId, none) of
                {ok, Dept} ->
                    case ?PG:list_members(DeptId, none) of
                        {ok, Members} ->
                            {ok, (project_dept(Dept))#{
                                members => [project_member(M) || M <- Members]
                            }};
                        {error, _} = Err ->
                            Err
                    end;
                {error, not_found} ->
                    {error, not_found};
                {error, _} = Err ->
                    Err
            end
    end.

%% ===================================================================
%% member add / remove
%% ===================================================================

%% @doc 部门加成员（兼职=一人可属多部门）。Params：department_id、user_id、
%% actor_user_id。授权：actor 是本 Org active member 或**该部门**管理员。
%% 用户必须是本 Org 的 active organization_member（DB 组合 FK + 触发器权威）。
%% 已在部门 ⇒ 幂等返回当前行。archived 部门拒绝。
-spec add_member(integer(), map()) -> {ok, map()} | {error, term()}.
add_member(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    DeptId = maps:get(department_id, Params, undefined),
    UserId = maps:get(user_id, Params, undefined),
    case {is_pos_int(DeptId), is_pos_int(UserId)} of
        {false, _} ->
            {error, {invalid_department_id, DeptId}};
        {_, false} ->
            {error, {invalid_user_id, UserId}};
        {true, true} ->
            case member_op_gate(OrgId, DeptId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, ActorId} ->
                    case ?PG:insert_member(OrgId, DeptId, UserId, ActorId, none) of
                        {ok, Row, AddedFlag} ->
                            {ok, project_member(Row#{idempotent => AddedFlag =:= already_member})};
                        {error, _} = Err ->
                            Err
                    end
            end
    end;
add_member(_OrgId, _Params) ->
    {error, {invalid_argument, add_member}}.

%% @doc 部门移除成员。授权同 add_member。本就不在 ⇒ 幂等成功。
%% archived 部门允许移除（离开失效目录属于清理事，不新增任何东西）。
-spec remove_member(integer(), map()) -> {ok, map()} | {error, term()}.
remove_member(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    DeptId = maps:get(department_id, Params, undefined),
    UserId = maps:get(user_id, Params, undefined),
    case {is_pos_int(DeptId), is_pos_int(UserId)} of
        {false, _} ->
            {error, {invalid_department_id, DeptId}};
        {_, false} ->
            {error, {invalid_user_id, UserId}};
        {true, true} ->
            case member_op_gate(OrgId, DeptId, Params, #{allow_archived => true}) of
                {error, _} = Err ->
                    Err;
                {ok, _ActorId} ->
                    case ?PG:delete_member(DeptId, UserId, none) of
                        {ok, Result} ->
                            {ok, #{
                                department_id => DeptId,
                                user_id => UserId,
                                result => Result,
                                idempotent => Result =:= not_present
                            }};
                        {error, _} = Err ->
                            Err
                    end
            end
    end;
remove_member(_OrgId, _Params) ->
    {error, {invalid_argument, remove_member}}.

%% 成员操作的公共闸门：部门存在（org 内）→（可选）非 archived →
%% actor 是 org active member 或该部门管理员。
member_op_gate(OrgId, DeptId, Params) ->
    member_op_gate(OrgId, DeptId, Params, #{}).

member_op_gate(OrgId, DeptId, Params, Opts) ->
    case require_actor(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            case ?PG:fetch_department(OrgId, DeptId, none) of
                {error, not_found} ->
                    {error, not_found};
                {ok, #{status := archived}} ->
                    case maps:get(allow_archived, Opts, false) of
                        true -> {ok, ActorId};
                        false -> {error, department_archived}
                    end;
                {ok, _Dept} ->
                    case is_dept_privileged(OrgId, DeptId, ActorId) of
                        {ok, true} -> {ok, ActorId};
                        {ok, false} -> {error, {actor_not_permitted, ActorId}};
                        {error, _} = Err -> Err
                    end
            end
    end.

%% ===================================================================
%% department admin set/remove（局部目录角色，非权限）
%% ===================================================================

%% @doc 设置/取消部门管理员（department_member.is_admin 标记）。
%% Params：department_id、user_id、admin（boolean）、actor_user_id。
%% 授权：actor 是 org owner/admin 或该部门现任管理员。
%% 目标用户必须已是该部门成员；org role 与任何权限表**零变化**（C15）。
-spec set_admin(integer(), map()) -> {ok, map()} | {error, term()}.
set_admin(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    DeptId = maps:get(department_id, Params, undefined),
    UserId = maps:get(user_id, Params, undefined),
    Flag = maps:get(admin, Params, undefined),
    case {is_pos_int(DeptId), is_pos_int(UserId), is_boolean(Flag)} of
        {false, _, _} ->
            {error, {invalid_department_id, DeptId}};
        {_, false, _} ->
            {error, {invalid_user_id, UserId}};
        {_, _, false} ->
            {error, {invalid_admin_flag, Flag}};
        {true, true, true} ->
            case admin_op_gate(OrgId, DeptId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, _ActorId} ->
                    set_admin_gate(OrgId, DeptId, UserId, Flag)
            end
    end;
set_admin(_OrgId, _Params) ->
    {error, {invalid_argument, set_admin}}.

set_admin_gate(_OrgId, DeptId, UserId, Flag) when is_boolean(Flag) ->
    case ?PG:fetch_member(DeptId, UserId, none) of
        {error, not_found} ->
            {error, not_department_member};
        {ok, #{is_admin := Flag}} ->
            %% 幂等：同值零写入
            {ok, #{
                department_id => DeptId,
                user_id => UserId,
                is_admin => Flag,
                idempotent => true
            }};
        {ok, _Row} ->
            case ?PG:set_admin(DeptId, UserId, Flag, none, none) of
                ok ->
                    {ok, #{department_id => DeptId, user_id => UserId, is_admin => Flag}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% admin 操作闸门：部门存在且 active；actor 是 org owner|admin 或该部门管理员。
admin_op_gate(OrgId, DeptId, Params) ->
    case require_actor(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ActorId} ->
            case ?PG:fetch_department(OrgId, DeptId, none) of
                {error, not_found} ->
                    {error, not_found};
                {ok, #{status := archived}} ->
                    {error, department_archived};
                {ok, _Dept} ->
                    case org_role_of(OrgId, ActorId) of
                        {ok, #{role := Role, status := active}} when
                            Role =:= owner; Role =:= admin
                        ->
                            {ok, ActorId};
                        {ok, _OtherMember} ->
                            case ?PG:is_department_admin(DeptId, ActorId, none) of
                                {ok, true} -> {ok, ActorId};
                                {ok, false} -> {error, {actor_not_permitted, ActorId}};
                                {error, _} = Err -> Err
                            end;
                        {error, not_found} ->
                            {error, {actor_not_member, ActorId}};
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

%% ===================================================================
%% list members
%% ===================================================================

%% @doc 列部门成员。授权基线同 list_departments；部门须存在于本 Org
%% （archived 也可读：目录事实可审计）。
-spec list_members(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_members(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case require_actor(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, _ActorId} ->
            do_list_members(OrgId, Params)
    end;
list_members(_OrgId, _Params) ->
    {error, {invalid_argument, list_members}}.

do_list_members(OrgId, Params) ->
    DeptId = maps:get(department_id, Params, undefined),
    case is_pos_int(DeptId) of
        false ->
            {error, {invalid_department_id, DeptId}};
        true ->
            case ?PG:fetch_department(OrgId, DeptId, none) of
                {error, not_found} ->
                    {error, not_found};
                {ok, _Dept} ->
                    case ?PG:list_members(DeptId, none) of
                        {ok, Rows} -> {ok, [project_member(M) || M <- Rows]};
                        {error, _} = Err -> Err
                    end
            end
    end.

%% ===================================================================
%% 内部：授权基线 / 事实查询 / 投影
%% ===================================================================

%% actor 必须是本 Org 的 active member（ORG-04 冻结基线）。
require_actor(OrgId, Params) ->
    require_actor(OrgId, Params, false).

require_structure_manager(OrgId, Params) ->
    require_actor(OrgId, Params, true).

require_actor(OrgId, Params, ManagerOnly) ->
    ActorId = maps:get(actor_user_id, Params, undefined),
    case is_pos_int(ActorId) of
        false ->
            {error, {invalid_actor_user_id, ActorId}};
        true ->
            case org_role_of(OrgId, ActorId) of
                {ok, #{status := active}} when ManagerOnly =:= false -> {ok, ActorId};
                {ok, #{role := Role, status := active}} when Role =:= owner; Role =:= admin ->
                    {ok, ActorId};
                {ok, #{status := active}} ->
                    {error, {actor_not_permitted, ActorId}};
                {ok, #{status := Other}} ->
                    {error, {actor_not_active, ActorId, Other}};
                {error, not_found} ->
                    {error, {actor_not_member, ActorId}};
                {error, _} = Err ->
                    Err
            end
    end.

%% 成员操作授权：org owner/admin，或**该部门**现任管理员（局部目录角色委托）。
is_dept_privileged(OrgId, DeptId, ActorId) ->
    case org_role_of(OrgId, ActorId) of
        {ok, #{role := Role, status := active}} when Role =:= owner; Role =:= admin ->
            {ok, true};
        {ok, _PlainMember} ->
            ?PG:is_department_admin(DeptId, ActorId, none);
        {error, not_found} ->
            {ok, false};
        {error, _} = Err ->
            Err
    end.

org_role_of(OrgId, UserId) ->
    ?PG:org_role_of(OrgId, UserId).

id() ->
    %% default 生成器：命名生成器需显式 register，Core 侧不引入该耦合
    elib_tsid:generate().

trimmed(Name) when is_binary(Name) ->
    string:trim(Name);
trimmed(Name) ->
    Name.

fresh_dept(OrgId, DeptId) ->
    case ?PG:fetch_department(OrgId, DeptId, none) of
        {ok, Dept} -> {ok, project_dept(Dept)};
        {error, _} = Err -> Err
    end.

project_dept(Row) ->
    (?DOMAIN:project(?DEPT_KEYS, maps:without([archive_idempotent], Row)))#{
        archive_idempotent => maps:get(archive_idempotent, Row, false)
    }.

project_member(Row) ->
    Idempotent = maps:get(idempotent, Row, false),
    (?DOMAIN:project(?MEMBER_KEYS, maps:without([idempotent], Row)))#{
        idempotent => Idempotent
    }.

is_pos_int(V) ->
    is_integer(V) andalso V > 0.
