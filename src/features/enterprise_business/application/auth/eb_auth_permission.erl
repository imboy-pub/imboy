%%% @doc `function_key` / `permission` / `governance role` 的**概念分离**与判定。
%%%
%%% 依据：plan v4.1 EB-D11、EB-D02、§5.4。
%%%
%%% 三类概念**不得合并**：
%%%
%%%   * `function_key`——业务身份**类型**，只回答「该稳定身份承担什么职能」。
%%%     V1 值域仅 `sales | customer_service`，**直接复用 domain 语义**
%%%     （`eb_identity:function_keys/0`），本模块不自建第二套值域。
%%%   * `permission`——对**企业资源的动作能力**（`contact.read`、`conversation.write`
%%%     等）。不得通过新增 function 名称替代权限：`permission_satisfied/2` 对
%%%     传入的 `function_key` 字面量**显式拒绝**，不会因为权限集合里恰好有同名
%%%     字符串就放行。
%%%   * `governance role`——Organization 的组织治理权（`owner | admin`），与既有
%%%     `moya_acl:resolve_org_manager/2` 读取的同一事实源（`organization_member.role`）。
%%%     持有 sales/customer_service identity **不会**自动获得治理权；反之治理角色
%%%     也不会让 owner/admin 自动获得成员面的业务身份与权限。
%%%
%%% 本模块是**纯函数**：无 I/O、无进程、无隐式时间/随机源；权限与角色集合一律由
%%% 调用方（经 `eb_auth_port` 逐请求加载）注入。
%%%
%%% 本卡不新建任何 RBAC：角色/权限的权威来源仍是既有机制（Admin 面 `adm_acl` 的
%%% role → permission，Org 面 `organization_member.role`），本模块只做「三概念分离」
%%% 的判定，不持有、不持久化任何角色或权限数据。
-module(eb_auth_permission).

-export([
    function_keys/0,
    governance_roles/0,
    known_permissions/0,
    function_satisfied/2,
    permission_satisfied/2,
    governance_satisfied/2
]).

-type function_key() :: binary().
-type permission() :: binary().
-type governance_role() :: binary().

-export_type([function_key/0, permission/0, governance_role/0]).

%% ===================================================================
%% 值域
%% ===================================================================

%% @doc 业务身份类型值域。**复用 domain**：不得在本模块另立一份。
-spec function_keys() -> [function_key()].
function_keys() ->
    eb_identity:function_keys().

%% @doc 组织治理角色值域（与既有 `moya_acl:resolve_org_manager/2` 同源同值）。
-spec governance_roles() -> [governance_role()].
governance_roles() ->
    [<<"owner">>, <<"admin">>].

%% @doc V1 已知的企业资源动作权限集合（§5.4 示例的最小冻结集）。
%%
%% 与 `function_keys/0`、`governance_roles/0` **两两不相交**——交集非空即意味着
%% 有人打算用职能名或治理角色名替代权限。
-spec known_permissions() -> [permission()].
known_permissions() ->
    [
        <<"contact.read">>,
        <<"contact.write">>,
        <<"note.write">>,
        <<"conversation.read">>,
        <<"conversation.write">>,
        <<"message.write">>,
        <<"asset.read">>,
        <<"asset.write">>,
        <<"retention.manage">>,
        <<"offboarding.manage">>,
        <<"member.suspend">>,
        <<"enterprise_business:read">>,
        <<"enterprise_business:write">>,
        <<"customer_service:read">>,
        <<"customer_service:write">>
    ].

%% ===================================================================
%% 判定
%% ===================================================================

%% @doc 业务身份类型判定。
%%
%% `Required = undefined` 表示该路由不要求特定职能；否则必须命中集合之一。
%% 失败项携带**实际**职能集合，使「identity 正确但职能不符」与「identity 缺失」
%% 在本模块层面就可区分。
-spec function_satisfied(function_key() | undefined, [function_key()]) ->
    ok | {error, {function_mismatch, function_key(), [function_key()]}}.
function_satisfied(undefined, _ActualFunctions) ->
    ok;
function_satisfied(Required, ActualFunctions) when is_binary(Required), is_list(ActualFunctions) ->
    case lists:member(Required, ActualFunctions) of
        true ->
            ok;
        false ->
            {error, {function_mismatch, Required, ActualFunctions}}
    end;
function_satisfied(Required, ActualFunctions) ->
    {error, {function_mismatch, Required, ActualFunctions}}.

%% @doc 权限判定（**独立**于职能与治理角色）。
%%
%% 传入的 `Required` 若本身是 `function_key` 字面量 → 一律拒绝
%% （`function_cannot_substitute_permission`）：新增 function 名不得替代权限。
-spec permission_satisfied(permission() | undefined, [permission()]) ->
    ok
    | {error, {permission_missing, permission()}}
    | {error, {function_cannot_substitute_permission, function_key()}}
    | {error, {invalid_required_permission, term()}}.
permission_satisfied(undefined, _Granted) ->
    ok;
permission_satisfied(Required, Granted) when is_binary(Required), is_list(Granted) ->
    case lists:member(Required, function_keys()) of
        true ->
            {error, {function_cannot_substitute_permission, Required}};
        false ->
            case lists:member(Required, Granted) of
                true -> ok;
                false -> {error, {permission_missing, Required}}
            end
    end;
permission_satisfied(Required, _Granted) ->
    {error, {invalid_required_permission, Required}}.

%% @doc 治理角色判定（**独立**于职能与权限）。
%%
%% `RequiredRoles = []` 表示该路由不要求治理权。持有业务 identity 的成员其治理
%% 角色集合通常为空 → `governance_insufficient`，这正是「function_key 对但
%% governance 不够」的可区分负例。
-spec governance_satisfied([governance_role()], [governance_role()]) ->
    ok
    | {error, {governance_insufficient, [governance_role()]}}
    | {error, {invalid_required_governance, term()}}.
governance_satisfied([], _ActualRoles) ->
    ok;
governance_satisfied(RequiredRoles, ActualRoles) when
    is_list(RequiredRoles), is_list(ActualRoles)
->
    case [Role || Role <- RequiredRoles, lists:member(Role, ActualRoles)] of
        [] -> {error, {governance_insufficient, RequiredRoles}};
        _Matched -> ok
    end;
governance_satisfied(RequiredRoles, _ActualRoles) ->
    {error, {invalid_required_governance, RequiredRoles}}.
