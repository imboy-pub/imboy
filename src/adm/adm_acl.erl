-module(adm_acl).
-dialyzer({nowarn_function, [role_permissions/1]}).
-compile([nowarn_deprecated_catch]).

%%%
% adm 后台 RBAC 权限校验共享模块
% Shared RBAC permission helper for admin handlers
%
% 统一收敛原先散落在 11 个 adm_*_handler 中逐字重复的权限校验代码。
% 权限的权威来源是 adm_index_handler:role_acl/1（角色 -> 权限列表）。
%
% 缓存键统一为 {adm_user_permission, AdmUserId}（原各 handler 各用一个键，
% 但 Fun 与 TTL 完全相同、缓存值一致，故合并为单键，并由 flush_cache/1 统一失效）。
%%%

-export([
    ensure_permission/3,
    ensure_any_permission/3,
    permissions/1,
    flush_cache/1,
    normalize_role_ids/1
]).

-include("error_code.hrl").

%% ===================================================================
%% 入口守卫（含 cowboy_req 响应构造）
%% ===================================================================

%% @doc 要求当前管理员必须拥有指定单个权限
-spec ensure_permission(map(), binary(), cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_permission(State, Permission, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_permission(AdmUserId, Permission) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

%% @doc 要求当前管理员拥有给定权限列表中的任意一个
-spec ensure_any_permission(map(), [binary()], cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_any_permission(State, Permissions, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_any_permission(AdmUserId, Permissions) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

%% ===================================================================
%% 权限判定（纯逻辑）
%% ===================================================================

-spec has_permission(term(), binary()) -> boolean().
has_permission(AdmUserId, Permission) when
    is_integer(AdmUserId), AdmUserId > 0, is_binary(Permission)
->
    lists:member(Permission, permissions(AdmUserId));
has_permission(_, _) ->
    false.

-spec has_any_permission(term(), [binary()]) -> boolean().
has_any_permission(AdmUserId, PermissionsToCheck) when
    is_integer(AdmUserId), AdmUserId > 0, is_list(PermissionsToCheck)
->
    UserPermissions = permissions(AdmUserId),
    lists:any(fun(Permission) -> lists:member(Permission, UserPermissions) end, PermissionsToCheck);
has_any_permission(_, _) ->
    false.

%% ===================================================================
%% 权限解析（含 DB 访问，经 adm_user_logic 缓存层）
%% ===================================================================

%% @doc 解析管理员的全部权限（合并其所有角色的 ACL 权限并去重）
-spec permissions(integer()) -> [binary()].
permissions(AdmUserId) ->
    Key = {adm_user_permission, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,role_id">>, Key) of
        AdmUser when is_map(AdmUser) ->
            RoleIds = normalize_role_ids(maps:get(<<"role_id">>, AdmUser, 0)),
            lists:usort(lists:append([role_permissions(RoleId) || RoleId <- RoleIds]));
        _ ->
            []
    end.

-spec role_permissions(integer()) -> [binary()].
role_permissions(RoleId) ->
    try adm_index_handler:role_acl(RoleId) of
        {_RoleName, Permissions, _MenuPaths} when is_list(Permissions) ->
            Permissions;
        _ ->
            []
    catch
        _:_ ->
            []
    end.

%% @doc 失效指定管理员的权限缓存
-spec flush_cache(integer()) -> ok.
flush_cache(AdmUserId) ->
    imboy_cache:flush({adm_user_permission, AdmUserId}),
    ok.

%% ===================================================================
%% role_id 归一化
%% ===================================================================

-spec normalize_role_ids(term()) -> [integer()].
normalize_role_ids(RoleId) when is_integer(RoleId), RoleId > 0 ->
    [RoleId];
normalize_role_ids(RoleIds) when is_list(RoleIds) ->
    lists:usort([Id || Value <- RoleIds, Id <- [normalize_role_id(Value)], Id > 0]);
normalize_role_ids(RoleValue) ->
    case normalize_role_id(RoleValue) of
        Id when Id > 0 ->
            [Id];
        _ ->
            []
    end.

-spec normalize_role_id(term()) -> integer().
normalize_role_id(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_role_id(Value) when is_binary(Value); is_list(Value) ->
    try ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    catch
        _:_ ->
            0
    end;
normalize_role_id(_) ->
    0.
