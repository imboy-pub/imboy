-module(adm_index_handler).
-compile([nowarn_deprecated_catch]).
%%%
% adm_index 控制器模块
% adm_index controller module
%%%
-behavior(cowboy_rest).

-export([init/2, role_acl/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

-define(ROLE_PERMISSIONS_CONFIG_KEY, <<"adm_role_permissions">>).
-define(ROLE_NAMES_CONFIG_KEY, <<"adm_role_names">>).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化管理后台首页处理器
%% 根据请求中的 action 参数分发到不同的处理函数
%% @param Req0 Cowboy 请求对象
%% @param State0 状态映射，包含 action 等信息
%% @return {ok, Req, State} 更新后的请求和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            index ->
                index(Method, Req0, State);
            current ->
                current(Method, Req0, State);
            rbac ->
                rbac(Method, Req0, State);
            welcome ->
                welcome(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理管理后台首页请求（JSON）
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, _State) ->
    elib_response:success(Req0, overview_payload());
index(_Method, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 处理欢迎页面请求（JSON）
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec welcome(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
welcome(<<"GET">>, Req0, _State) ->
    elib_response:success(Req0, overview_payload());
welcome(_Method, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 获取当前登录管理员信息（JSON）
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射（由 adm_auth_middleware 注入 adm_user_id）
%% @return cowboy_req:req() 更新后的请求对象
-spec current(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
current(<<"GET">>, Req0, State) ->
    case maps:get(adm_user_id, State, undefined) of
        Uid when is_integer(Uid), Uid > 0 ->
            Key = {adm_user_current, Uid},
            AdmUser = adm_user_logic:find(
                Uid,
                <<"id,account,nickname,avatar,role_id,login_count,last_login_ip,last_login_at,status,created_at">>,
                Key
            ),
            AdmUser2 = AdmUser,
            elib_response:success(Req0, AdmUser2);
        _ ->
            elib_response:error(Req0, <<"Need to log in again"/utf8>>, 706)
    end;
current(_Method, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 获取当前管理员精简 RBAC（JSON）
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射（由 adm_auth_middleware 注入 adm_user_id）
%% @return cowboy_req:req() 更新后的请求对象
-spec rbac(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
rbac(<<"GET">>, Req0, State) ->
    case maps:get(adm_user_id, State, undefined) of
        Uid when is_integer(Uid), Uid > 0 ->
            Key = {adm_user_rbac, Uid},
            AdmUser = adm_user_logic:find(Uid, <<"id,role_id">>, Key),
            RoleValue = maps:get(<<"role_id">>, AdmUser, 0),
            elib_response:success(Req0, rbac_payload(RoleValue));
        _ ->
            elib_response:error(Req0, <<"Need to log in again"/utf8>>, 706)
    end;
rbac(_Method, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec overview_payload() -> map().
overview_payload() ->
    #{
        <<"coversation_online_user">> => imboy_syn:count_user(),
        <<"coversation_online_device">> => imboy_syn:count()
    }.

-spec rbac_payload(term()) -> map().
rbac_payload(RoleValue) ->
    RoleIds = normalize_role_ids(RoleValue),
    case RoleIds of
        [] ->
            {RoleName, Permissions, MenuPaths} = role_acl(0),
            #{
                <<"role_id">> => 0,
                <<"role_ids">> => [],
                <<"role_name">> => RoleName,
                <<"role_names">> => [RoleName],
                <<"permissions">> => Permissions,
                <<"menu_paths">> => MenuPaths
            };
        _ ->
            Acls = [role_acl(RoleId) || RoleId <- RoleIds],
            RoleNames = lists:usort([RoleName || {RoleName, _Permissions, _MenuPaths} <- Acls]),
            Permissions = lists:usort(
                lists:append([Perms || {_RoleName, Perms, _MenuPaths} <- Acls])
            ),
            MenuPaths = lists:usort(lists:append([Paths || {_RoleName, _Perms, Paths} <- Acls])),
            #{
                <<"role_id">> => hd(RoleIds),
                <<"role_ids">> => RoleIds,
                <<"role_name">> => merge_role_name(RoleNames),
                <<"role_names">> => RoleNames,
                <<"permissions">> => Permissions,
                <<"menu_paths">> => MenuPaths
            }
    end.

-spec merge_role_name(list(binary())) -> binary().
merge_role_name([RoleName]) ->
    RoleName;
merge_role_name([]) ->
    <<"readonly">>;
merge_role_name(_RoleNames) ->
    <<"multi_role">>.

-spec normalize_role_ids(term()) -> list(integer()).
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

-spec role_acl(integer()) -> {binary(), list(binary()), list(binary())}.
role_acl(1) ->
    RoleName = <<"super_admin">>,
    Permissions = [
        <<"dashboard:view">>,
        <<"users:read">>,
        <<"users:create">>,
        <<"users:update">>,
        <<"users:delete">>,
        <<"groups:read">>,
        <<"groups:create">>,
        <<"groups:update">>,
        <<"groups:delete">>,
        <<"groups:vote:read">>,
        <<"groups:vote:close">>,
        <<"groups:notice:read">>,
        <<"groups:notice:delete">>,
        <<"groups:category:read">>,
        <<"groups:category:delete">>,
        <<"groups:tag:read">>,
        <<"groups:tag:delete">>,
        <<"groups:file:read">>,
        <<"groups:file:delete">>,
        <<"groups:album:read">>,
        <<"groups:album:delete">>,
        <<"groups:schedule:read">>,
        <<"groups:schedule:restore">>,
        <<"groups:schedule:cancel">>,
        <<"groups:task:read">>,
        <<"groups:task:restore">>,
        <<"groups:task:review">>,
        <<"groups:task:close">>,
        <<"groups:task:delete">>,
        <<"channels:read">>,
        <<"channels:create">>,
        <<"channels:update">>,
        <<"channels:delete">>,
        <<"channel_order_refund">>,
        %% finance 运营财务（钱包/充值/支付/SaaS 计费）
        %% TODO: finance 细粒度权限当前仅 read/write 两级，后续按需细化
        %% （如 finance:wallet:read / finance:billing:write）由 RBAC 补充。
        <<"finance:read">>,
        <<"finance:write">>,
        <<"moments:read">>,
        <<"moments:delete">>,
        <<"moments:report:read">>,
        <<"moments:report:handle">>,
        <<"reports:read">>,
        <<"reports:handle">>,
        <<"feedback:read">>,
        <<"feedback:reply">>,
        <<"feedback:workflow:read">>,
        <<"feedback:workflow:write">>,
        <<"messages:read">>,
        <<"logout_applications:read">>,
        <<"ux:events:ingest">>,
        <<"storage:view">>,
        <<"storage:disable">>,
        <<"storage:enable">>,
        <<"storage:delete">>,
        <<"storage:cleanup">>,
        <<"settings:view">>,
        <<"settings:update">>,
        <<"settings:version:read">>,
        <<"settings:version:create">>,
        <<"settings:version:update">>,
        <<"settings:version:delete">>,
        <<"settings:ddl:read">>,
        <<"settings:ddl:create">>,
        <<"settings:ddl:update">>,
        <<"settings:ddl:delete">>,
        <<"admins:read">>,
        <<"admins:create">>,
        <<"admins:update">>,
        <<"admins:delete">>,
        <<"admins:assign_role">>,
        <<"roles:view">>,
        <<"roles:create">>,
        <<"roles:update">>,
        <<"logs:view">>,
        <<"plugins:read">>,
        <<"plugins:install">>,
        <<"plugins:enable">>,
        <<"plugins:disable">>,
        <<"plugins:upgrade">>,
        <<"plugins:uninstall">>,
        <<"plugins:reset">>,
        <<"plugins:force_uninstall">>
    ],
    MenuPaths = [
        <<"/dashboard">>,
        <<"/users">>,
        <<"/groups">>,
        <<"/groups/context">>,
        <<"/moments">>,
        <<"/messages">>,
        <<"/logout-applications">>,
        <<"/channels">>,
        <<"/finance">>,
        <<"/reports">>,
        <<"/feedback">>,
        <<"/storage">>,
        <<"/settings">>,
        <<"/admins">>,
        <<"/roles">>,
        <<"/logs">>,
        <<"/plugins">>
    ],
    apply_role_acl_override(1, {RoleName, Permissions, MenuPaths});
role_acl(2) ->
    RoleName = <<"ops_admin">>,
    Permissions = [
        <<"dashboard:view">>,
        <<"users:read">>,
        <<"users:create">>,
        <<"users:update">>,
        <<"users:delete">>,
        <<"groups:read">>,
        <<"groups:create">>,
        <<"groups:update">>,
        <<"groups:delete">>,
        <<"groups:vote:read">>,
        <<"groups:vote:close">>,
        <<"groups:notice:read">>,
        <<"groups:notice:delete">>,
        <<"groups:category:read">>,
        <<"groups:category:delete">>,
        <<"groups:tag:read">>,
        <<"groups:tag:delete">>,
        <<"groups:file:read">>,
        <<"groups:file:delete">>,
        <<"groups:album:read">>,
        <<"groups:album:delete">>,
        <<"groups:schedule:read">>,
        <<"groups:schedule:restore">>,
        <<"groups:schedule:cancel">>,
        <<"groups:task:read">>,
        <<"groups:task:restore">>,
        <<"groups:task:review">>,
        <<"groups:task:close">>,
        <<"groups:task:delete">>,
        <<"channels:read">>,
        <<"channels:create">>,
        <<"channels:update">>,
        <<"channels:delete">>,
        <<"channel_order_refund">>,
        %% finance 运营财务（钱包/充值/支付/SaaS 计费）
        %% TODO: finance 细粒度权限当前仅 read/write 两级，后续按需细化由 RBAC 补充。
        <<"finance:read">>,
        <<"finance:write">>,
        <<"moments:read">>,
        <<"moments:delete">>,
        <<"moments:report:read">>,
        <<"moments:report:handle">>,
        <<"reports:read">>,
        <<"reports:handle">>,
        <<"feedback:read">>,
        <<"feedback:reply">>,
        <<"feedback:workflow:read">>,
        <<"feedback:workflow:write">>,
        <<"messages:read">>,
        <<"logout_applications:read">>,
        <<"ux:events:ingest">>,
        <<"storage:disable">>,
        <<"storage:enable">>,
        <<"storage:delete">>,
        <<"storage:cleanup">>,
        <<"plugins:read">>,
        <<"plugins:install">>,
        <<"plugins:enable">>,
        <<"plugins:disable">>,
        <<"plugins:upgrade">>,
        <<"plugins:uninstall">>,
        <<"plugins:reset">>,
        <<"plugins:force_uninstall">>
    ],
    MenuPaths = [
        <<"/dashboard">>,
        <<"/users">>,
        <<"/groups">>,
        <<"/groups/context">>,
        <<"/moments">>,
        <<"/messages">>,
        <<"/logout-applications">>,
        <<"/channels">>,
        <<"/finance">>,
        <<"/reports">>,
        <<"/feedback">>,
        <<"/plugins">>
    ],
    apply_role_acl_override(2, {RoleName, Permissions, MenuPaths});
role_acl(3) ->
    RoleName = <<"audit_admin">>,
    Permissions = [
        <<"dashboard:view">>,
        <<"messages:read">>,
        <<"logout_applications:read">>,
        <<"ux:events:ingest">>,
        <<"roles:view">>,
        <<"logs:view">>,
        <<"plugins:read">>
    ],
    MenuPaths = [
        <<"/dashboard">>,
        <<"/groups/context">>,
        <<"/messages">>,
        <<"/logout-applications">>,
        <<"/roles">>,
        <<"/logs">>
    ],
    apply_role_acl_override(3, {RoleName, Permissions, MenuPaths});
role_acl(RoleId) when is_integer(RoleId), RoleId > 0 ->
    FallbackName = resolve_role_name(RoleId, <<"readonly">>),
    apply_role_acl_override(RoleId, {FallbackName, [<<"dashboard:view">>], [<<"/dashboard">>]});
role_acl(_RoleId) ->
    {<<"readonly">>, [<<"dashboard:view">>], [<<"/dashboard">>]}.

-spec apply_role_acl_override(integer(), {binary(), list(binary()), list(binary())}) ->
    {binary(), list(binary()), list(binary())}.
apply_role_acl_override(RoleId, {RoleName, Permissions, MenuPaths}) ->
    OverrideName = resolve_role_name(RoleId, RoleName),
    case resolve_role_permissions_override(RoleId) of
        undefined ->
            {OverrideName, Permissions, MenuPaths};
        OverridePermissions ->
            {OverrideName, OverridePermissions, MenuPaths}
    end.

-spec resolve_role_permissions_override(integer()) -> undefined | list(binary()).
resolve_role_permissions_override(RoleId) ->
    Map = load_role_config_map(?ROLE_PERMISSIONS_CONFIG_KEY),
    case role_map_get(Map, RoleId) of
        undefined ->
            undefined;
        Value ->
            normalize_permission_list(Value)
    end.

-spec resolve_role_name(integer(), binary()) -> binary().
resolve_role_name(RoleId, DefaultName) ->
    Map = load_role_config_map(?ROLE_NAMES_CONFIG_KEY),
    case role_map_get(Map, RoleId) of
        Value when is_binary(Value); is_list(Value) ->
            normalize_binary(Value);
        _ ->
            DefaultName
    end.

-spec load_role_config_map(binary()) -> map().
load_role_config_map(Key) ->
    case catch config_ds:get(Key, #{}) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

-spec role_map_get(map(), integer()) -> term().
role_map_get(Map, RoleId) when is_map(Map) ->
    RoleKey = integer_to_binary(RoleId),
    case maps:find(RoleKey, Map) of
        {ok, Value} ->
            Value;
        error ->
            case maps:find(RoleId, Map) of
                {ok, Value2} ->
                    Value2;
                error ->
                    undefined
            end
    end.

-spec normalize_permission_list(term()) -> list(binary()).
normalize_permission_list([]) ->
    [];
normalize_permission_list(Permissions) when is_list(Permissions) ->
    lists:usort(
        [Perm || Item <- Permissions, Perm <- [normalize_binary(Item)], Perm =/= <<>>]
    );
normalize_permission_list(Permissions) when is_binary(Permissions) ->
    Tokens = re:split(Permissions, <<"[,;\\n\\r\\s]+">>, [{return, binary}, trim]),
    lists:usort(
        [Perm || Item <- Tokens, Perm <- [normalize_binary(Item)], Perm =/= <<>>]
    );
normalize_permission_list(_) ->
    [].

-spec normalize_binary(term()) -> binary().
normalize_binary(Value) when is_binary(Value) ->
    unicode:characters_to_binary(string:trim(ec_cnv:to_list(Value)));
normalize_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(string:trim(Value));
normalize_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_binary(_) ->
    <<>>.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
