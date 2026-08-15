-module(adm_role_handler).
-compile([nowarn_deprecated_catch]).

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").

-define(ROLE_PERMISSIONS_CONFIG_KEY, <<"adm_role_permissions">>).
-define(ROLE_NAMES_CONFIG_KEY, <<"adm_role_names">>).
-define(ROLE_DESCRIPTIONS_CONFIG_KEY, <<"adm_role_descriptions">>).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list ->
                list_action(Method, Req0, State);
            create ->
                create_action(Method, Req0, State);
            permissions_save ->
                permissions_save_action(Method, Req0, State);
            disable ->
                disable_action(Method, Req0, State);
            delete ->
                delete_action(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

-spec list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"roles:view">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:int(status, Req0, -1),
            {ok, Keyword0} = elib_param:binary(keyword, Req0, <<>>),
            Keyword = normalize_binary(Keyword0),
            case build_role_page(Page, Size, Status, Keyword) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    elib_response:error(Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
list_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec create_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"roles:create">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            RoleName = normalize_binary(
                maps:get(<<"name">>, PostVals, maps:get(<<"role_name">>, PostVals, <<>>))
            ),
            Description = normalize_binary(maps:get(<<"description">>, PostVals, <<>>)),
            Status = normalize_status(maps:get(<<"status">>, PostVals, 1)),
            Permissions = normalize_permissions(maps:get(<<"permissions">>, PostVals, [])),
            case validate_create_payload(RoleName) of
                ok ->
                    case role_name_exists(RoleName) of
                        true ->
                            elib_response:error(Req0, <<"角色名称已存在"/utf8>>, ?ERR_BAD_REQUEST);
                        false ->
                            case create_role_record(RoleName, Status) of
                                {ok, RoleId} ->
                                    _ = save_role_name(RoleId, RoleName),
                                    _ = save_role_description(RoleId, Description),
                                    _ = save_role_permissions(RoleId, Permissions),
                                    elib_response:success(Req0, #{
                                        <<"role_id">> => RoleId, <<"id">> => RoleId
                                    });
                                {error, Reason} ->
                                    elib_response:error(
                                        Req0, to_error_binary(Reason), ?ERR_BAD_REQUEST
                                    )
                            end
                    end;
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
create_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec permissions_save_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
permissions_save_action(<<"PUT">>, Req0, State) ->
    save_permissions_handle(Req0, State);
permissions_save_action(<<"POST">>, Req0, State) ->
    save_permissions_handle(Req0, State);
permissions_save_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec save_permissions_handle(cowboy_req:req(), map()) -> cowboy_req:req().
save_permissions_handle(Req0, State) ->
    case ensure_permission(State, <<"roles:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            RoleId = normalize_positive_int(maps:get(<<"role_id">>, PostVals, 0)),
            Permissions = normalize_permissions(maps:get(<<"permissions">>, PostVals, [])),
            Description0 = maps:get(<<"description">>, PostVals, undefined),
            Description =
                case Description0 of
                    undefined ->
                        undefined;
                    _ ->
                        normalize_binary(Description0)
                end,
            case role_exists(RoleId) of
                true ->
                    case RoleId of
                        %% 超级管理员权限锚定代码内置全集，禁止通过覆盖降权——
                        %% 否则 super_admin 一旦被配置成空权限集，所有管理员都会被锁在门外
                        %%（本地实测：role 1 被覆盖为 [dashboard:view, users:read] 后全后台 403）
                        1 ->
                            elib_response:error(
                                Req0, <<"超级管理员角色权限不可修改"/utf8>>, ?ERR_BAD_REQUEST
                            );
                        _ ->
                            _ = save_role_permissions(RoleId, Permissions),
                            _ = maybe_save_role_description(RoleId, Description),
                            elib_response:success(Req0, #{})
                    end;
                false ->
                    elib_response:error(Req0, <<"角色不存在"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

%% @doc 软停用角色（status => 0），内置角色（1/2/3）不可停用
-spec disable_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
disable_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"roles:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            RoleId = normalize_positive_int(
                maps:get(<<"role_id">>, PostVals, maps:get(<<"id">>, PostVals, 0))
            ),
            case validate_mutable_role(RoleId) of
                ok ->
                    case update_role_status(RoleId, 0) of
                        {ok, _} ->
                            elib_response:success(Req0, #{});
                        {error, Reason} ->
                            elib_response:error(
                                Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
disable_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 硬删除角色，内置角色不可删；删除前校验该角色下无在用管理员，避免孤儿
-spec delete_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"roles:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            RoleId = normalize_positive_int(
                maps:get(<<"role_id">>, PostVals, maps:get(<<"id">>, PostVals, 0))
            ),
            case validate_mutable_role(RoleId) of
                ok ->
                    delete_role_after_guard(Req0, RoleId);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
delete_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 校验该角色下无在用管理员后再删除
-spec delete_role_after_guard(cowboy_req:req(), integer()) -> cowboy_req:req().
delete_role_after_guard(Req0, RoleId) ->
    case adm_user_ds:count_by_role(RoleId) of
        {ok, 0} ->
            case delete_role_record(RoleId) of
                {ok, _} ->
                    elib_response:success(Req0, #{});
                {error, Reason} ->
                    elib_response:error(Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {ok, Count} when Count > 0 ->
            Msg = unicode:characters_to_binary(
                io_lib:format("该角色下仍有 ~B 名管理员在用，无法删除", [Count])
            ),
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {error, Reason} ->
            elib_response:error(Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 校验角色可停用/删除：内置角色(1/2/3)与不存在角色拒绝
-spec validate_mutable_role(integer()) -> ok | {error, binary()}.
validate_mutable_role(RoleId) when RoleId =< 0 ->
    {error, <<"role_id 无效"/utf8>>};
validate_mutable_role(RoleId) when RoleId =:= 1; RoleId =:= 2; RoleId =:= 3 ->
    {error, <<"内置角色不可停用或删除"/utf8>>};
validate_mutable_role(RoleId) ->
    case role_exists(RoleId) of
        true ->
            ok;
        false ->
            {error, <<"角色不存在"/utf8>>}
    end.

-spec update_role_status(integer(), integer()) -> {ok, term()} | {error, term()}.
update_role_status(RoleId, Status) ->
    elib_pg:update(role_table(), #{<<"status">> => Status}, <<"id = $1">>, [RoleId]).

-spec delete_role_record(integer()) -> {ok, term()} | {error, term()}.
delete_role_record(RoleId) ->
    Tb = role_table(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg:execute(Sql, [RoleId]).

-spec build_role_page(pos_integer(), pos_integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
build_role_page(Page, Size, Status, Keyword) ->
    case fetch_roles() of
        {ok, Roles} ->
            Roles1 = [Role || Role <- Roles, role_status_match(Role, Status)],
            Roles2 = [Role || Role <- Roles1, role_keyword_match(Role, Keyword)],
            Total = length(Roles2),
            List = paginate(Roles2, Page, Size),
            {ok, #{list => List, total => Total, page => Page, size => Size}};
        Error ->
            Error
    end.

-spec fetch_roles() -> {ok, [map()]} | {error, term()}.
fetch_roles() ->
    case fetch_role_rows_from_db() of
        {ok, DbRows} ->
            Builtin = builtin_role_rows(),
            Merged = merge_role_rows(Builtin, DbRows),
            {ok, [normalize_role_row(Row) || Row <- Merged]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec fetch_role_rows_from_db() -> {ok, [map()]} | {error, term()}.
fetch_role_rows_from_db() ->
    Tb = role_table(),
    Sql = iolist_to_binary([
        <<"SELECT id, role_name, status, created_at FROM ">>,
        Tb,
        <<" WHERE status >= -1 ORDER BY sort ASC, id ASC">>
    ]),
    case elib_pg:query(Sql, []) of
        {ok, Rows} when is_list(Rows) ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end.

-spec builtin_role_rows() -> [map()].
builtin_role_rows() ->
    [builtin_role_row(1), builtin_role_row(2), builtin_role_row(3)].

-spec builtin_role_row(integer()) -> map().
builtin_role_row(RoleId) ->
    {RoleName, _Permissions, _MenuPaths} = adm_index_handler:role_acl(RoleId),
    #{
        <<"id">> => RoleId,
        <<"role_name">> => RoleName,
        <<"status">> => 1,
        <<"created_at">> => <<>>
    }.

-spec merge_role_rows([map()], [map()]) -> [map()].
merge_role_rows(BuiltinRows, DbRows) ->
    Map0 = maps:from_list([{maps:get(<<"id">>, Row, 0), Row} || Row <- BuiltinRows]),
    Map1 =
        lists:foldl(
            fun(Row, Acc) ->
                RoleId = maps:get(<<"id">>, Row, 0),
                case RoleId > 0 of
                    true ->
                        Acc#{RoleId => Row};
                    false ->
                        Acc
                end
            end,
            Map0,
            DbRows
        ),
    % Newest roles first so recently created roles appear on page 1.
    RoleIds = lists:reverse(lists:sort(maps:keys(Map1))),
    [maps:get(RoleId, Map1) || RoleId <- RoleIds].

-spec normalize_role_row(map()) -> map().
normalize_role_row(Row) ->
    RoleId = normalize_positive_int(maps:get(<<"id">>, Row, 0)),
    DefaultName =
        case adm_index_handler:role_acl(RoleId) of
            {AclRoleName, _Perms, _Paths} ->
                AclRoleName
        end,
    RoleName0 = normalize_binary(maps:get(<<"role_name">>, Row, DefaultName)),
    RoleName =
        case RoleName0 of
            <<>> ->
                role_name_override(RoleId, DefaultName);
            _ ->
                RoleName0
        end,
    Description = role_description(RoleId),
    Permissions = role_permissions(RoleId),
    Status = normalize_status(maps:get(<<"status">>, Row, 1)),
    CreatedAt = maps:get(<<"created_at">>, Row, <<>>),
    #{
        <<"id">> => RoleId,
        <<"role_id">> => RoleId,
        <<"name">> => RoleName,
        <<"role_name">> => RoleName,
        <<"description">> => Description,
        <<"permissions">> => Permissions,
        <<"status">> => Status,
        <<"created_at">> => CreatedAt
    }.

-spec role_permissions(integer()) -> [binary()].
role_permissions(RoleId) ->
    OverrideMap = load_config_map(?ROLE_PERMISSIONS_CONFIG_KEY),
    case role_map_get(OverrideMap, RoleId) of
        undefined ->
            case adm_index_handler:role_acl(RoleId) of
                {_RoleName, Permissions, _MenuPaths} when is_list(Permissions) ->
                    normalize_permissions(Permissions)
            end;
        Value ->
            normalize_permissions(Value)
    end.

-spec role_description(integer()) -> binary().
role_description(RoleId) ->
    DescMap = load_config_map(?ROLE_DESCRIPTIONS_CONFIG_KEY),
    case role_map_get(DescMap, RoleId) of
        Value when is_binary(Value) ->
            normalize_binary(Value);
        Value when is_list(Value) ->
            normalize_binary(Value);
        _ ->
            <<>>
    end.

-spec role_name_override(integer(), binary()) -> binary().
role_name_override(RoleId, Default) ->
    NameMap = load_config_map(?ROLE_NAMES_CONFIG_KEY),
    case role_map_get(NameMap, RoleId) of
        Value when is_binary(Value); is_list(Value) ->
            normalize_binary(Value);
        _ ->
            Default
    end.

-spec role_status_match(map(), integer()) -> boolean().
role_status_match(_Role, -1) ->
    true;
role_status_match(Role, Status) ->
    normalize_status(maps:get(<<"status">>, Role, 1)) =:= Status.

-spec role_keyword_match(map(), binary()) -> boolean().
role_keyword_match(_Role, <<>>) ->
    true;
role_keyword_match(Role, Keyword) ->
    Name = normalize_binary(maps:get(<<"name">>, Role, maps:get(<<"role_name">>, Role, <<>>))),
    Description = normalize_binary(maps:get(<<"description">>, Role, <<>>)),
    contains_ci(Name, Keyword) orelse contains_ci(Description, Keyword).

-spec contains_ci(binary(), binary()) -> boolean().
contains_ci(Text, Keyword) ->
    TextLower = string:lowercase(ec_cnv:to_list(Text)),
    KeywordLower = string:lowercase(ec_cnv:to_list(Keyword)),
    case string:find(TextLower, KeywordLower) of
        nomatch ->
            false;
        _ ->
            true
    end.

-spec paginate([map()], pos_integer(), pos_integer()) -> [map()].
paginate(List, Page, Size) ->
    Offset = (Page - 1) * Size,
    take(Size, drop(Offset, List)).

-spec drop(non_neg_integer(), [map()]) -> [map()].
drop(N, List) when N =< 0 ->
    List;
drop(_N, []) ->
    [];
drop(N, [_ | Tail]) ->
    drop(N - 1, Tail).

-spec take(non_neg_integer(), [map()]) -> [map()].
take(N, _List) when N =< 0 ->
    [];
take(_N, []) ->
    [];
take(N, [Item | Tail]) ->
    [Item | take(N - 1, Tail)].

-spec validate_create_payload(binary()) -> ok | {error, binary()}.
validate_create_payload(RoleName) when byte_size(RoleName) < 2 ->
    {error, <<"角色名称长度至少 2 位"/utf8>>};
validate_create_payload(_RoleName) ->
    ok.

-spec role_table() -> binary().
role_table() ->
    elib_pg_sql:public_tablename(<<"adm_role">>).

-spec role_name_exists(binary()) -> boolean().
role_name_exists(RoleName) ->
    Tb = role_table(),
    Sql = iolist_to_binary([
        <<"SELECT id FROM ">>,
        Tb,
        <<" WHERE role_name = $1 AND status >= 0 LIMIT 1">>
    ]),
    case elib_pg:query(Sql, [RoleName]) of
        {ok, [_ | _]} ->
            true;
        _ ->
            false
    end.

-spec create_role_record(binary(), integer()) -> {ok, integer()} | {error, term()}.
create_role_record(RoleName, Status) ->
    Tb = role_table(),
    NextSort = next_role_sort(),
    %% adm_role.id bigint NOT NULL 无默认（TSID 迁移遗漏），应用侧生成 id，
    %% 同 adm_channel_handler 的 channel_price 先例。
    RoleId = elib_tsid:generate(adm_role),
    Sql = iolist_to_binary([
        <<"INSERT INTO ">>,
        Tb,
        <<" (id, parent_id, sort, role_name, status) VALUES ($1, $2, $3, $4, $5) RETURNING id">>
    ]),
    case elib_pg:one(Sql, [RoleId, 0, NextSort, RoleName, Status]) of
        {ok, #{<<"id">> := RoleId}} when is_integer(RoleId), RoleId > 0 ->
            {ok, RoleId};
        {ok, Row} ->
            case normalize_positive_int(maps:get(<<"id">>, Row, 0)) of
                Id when Id > 0 ->
                    {ok, Id};
                _ ->
                    {error, <<"create role failed">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec next_role_sort() -> integer().
next_role_sort() ->
    Tb = role_table(),
    Sql = iolist_to_binary([
        <<"SELECT COALESCE(MAX(sort), 99) AS max_sort FROM ">>,
        Tb,
        <<" WHERE status >= 0">>
    ]),
    case elib_pg:one(Sql, []) of
        {ok, #{<<"max_sort">> := MaxSort}} ->
            normalize_positive_int(MaxSort) + 1;
        _ ->
            100
    end.

-spec role_exists(integer()) -> boolean().
role_exists(RoleId) when RoleId =< 0 ->
    false;
role_exists(RoleId) when RoleId =:= 1; RoleId =:= 2; RoleId =:= 3 ->
    true;
role_exists(RoleId) ->
    Tb = role_table(),
    Sql = iolist_to_binary([
        <<"SELECT id FROM ">>,
        Tb,
        <<" WHERE id = $1 AND status >= 0 LIMIT 1">>
    ]),
    case elib_pg:query(Sql, [RoleId]) of
        {ok, [_ | _]} ->
            true;
        _ ->
            false
    end.

-spec save_role_permissions(integer(), [binary()]) -> ok.
save_role_permissions(RoleId, Permissions) ->
    Map0 = load_config_map(?ROLE_PERMISSIONS_CONFIG_KEY),
    Map1 = Map0#{role_key(RoleId) => normalize_permissions(Permissions)},
    config_ds:set(?ROLE_PERMISSIONS_CONFIG_KEY, Map1).

-spec save_role_name(integer(), binary()) -> ok.
save_role_name(RoleId, RoleName) ->
    Map0 = load_config_map(?ROLE_NAMES_CONFIG_KEY),
    Map1 = Map0#{role_key(RoleId) => normalize_binary(RoleName)},
    config_ds:set(?ROLE_NAMES_CONFIG_KEY, Map1).

-spec save_role_description(integer(), binary()) -> ok.
save_role_description(_RoleId, <<>>) ->
    ok;
save_role_description(RoleId, Description) ->
    Map0 = load_config_map(?ROLE_DESCRIPTIONS_CONFIG_KEY),
    Map1 = Map0#{role_key(RoleId) => normalize_binary(Description)},
    config_ds:set(?ROLE_DESCRIPTIONS_CONFIG_KEY, Map1).

-spec maybe_save_role_description(integer(), undefined | binary()) -> ok.
maybe_save_role_description(_RoleId, undefined) ->
    ok;
maybe_save_role_description(RoleId, Description) ->
    save_role_description(RoleId, Description).

-spec load_config_map(binary()) -> map().
load_config_map(Key) ->
    case catch config_ds:get(Key, #{}) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

-spec role_map_get(map(), integer()) -> term().
role_map_get(Map, RoleId) when is_map(Map) ->
    Key = role_key(RoleId),
    case maps:find(Key, Map) of
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

-spec role_key(integer()) -> binary().
role_key(RoleId) ->
    integer_to_binary(RoleId).

-spec normalize_permissions(term()) -> [binary()].
normalize_permissions([]) ->
    [];
normalize_permissions(Value) when is_binary(Value) ->
    normalize_permissions_from_text(Value);
normalize_permissions(Value) when is_list(Value) ->
    case is_charlist(Value) of
        true ->
            normalize_permissions_from_text(unicode:characters_to_binary(Value));
        false ->
            lists:usort([Perm || Item <- Value, Perm <- [normalize_permission(Item)], Perm =/= <<>>])
    end;
normalize_permissions(_) ->
    [].

-spec normalize_permissions_from_text(binary()) -> [binary()].
normalize_permissions_from_text(Text) ->
    Items = re:split(Text, <<"[,;\\n\\r\\s]+">>, [{return, binary}, trim]),
    lists:usort([Perm || Item <- Items, Perm <- [normalize_permission(Item)], Perm =/= <<>>]).

-spec normalize_permission(term()) -> binary().
normalize_permission(Value) when is_binary(Value) ->
    normalize_binary(Value);
normalize_permission(Value) when is_list(Value) ->
    normalize_binary(Value);
normalize_permission(_) ->
    <<>>.

-spec is_charlist(list()) -> boolean().
is_charlist([]) ->
    true;
is_charlist([H | T]) when is_integer(H), H >= 0, H =< 16#10FFFF ->
    is_charlist(T);
is_charlist(_) ->
    false.

-spec normalize_binary(term()) -> binary().
normalize_binary(Value) when is_binary(Value) ->
    unicode:characters_to_binary(string:trim(ec_cnv:to_list(Value)));
normalize_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(string:trim(Value));
normalize_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_binary(_) ->
    <<>>.

-spec normalize_positive_int(term()) -> integer().
normalize_positive_int(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_positive_int(Value) when is_binary(Value); is_list(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Int when is_integer(Int), Int > 0 ->
            Int;
        _ ->
            0
    end;
normalize_positive_int(_) ->
    0.

-spec normalize_status(term()) -> integer().
normalize_status(Value) ->
    case catch ec_cnv:to_integer(Value) of
        0 ->
            0;
        1 ->
            1;
        _ ->
            1
    end.

-spec to_error_binary(term()) -> binary().
to_error_binary(Reason) when is_binary(Reason) ->
    Reason;
to_error_binary(Reason) when is_list(Reason) ->
    unicode:characters_to_binary(Reason);
to_error_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
to_error_binary(Reason) when is_integer(Reason) ->
    integer_to_binary(Reason);
to_error_binary(Reason) ->
    unicode:characters_to_binary(io_lib:format("~p", [Reason])).

-spec ensure_permission(map(), binary(), cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_permission(State, Permission, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_permission(AdmUserId, Permission) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

-spec has_permission(term(), binary()) -> boolean().
has_permission(AdmUserId, Permission) when
    is_integer(AdmUserId), AdmUserId > 0, is_binary(Permission)
->
    UserPermissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:member(Permission, UserPermissions);
has_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> [binary()].
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_role_permission, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,role_id">>, Key) of
        AdmUser when is_map(AdmUser) ->
            RoleIds = normalize_role_ids(maps:get(<<"role_id">>, AdmUser, 0)),
            lists:usort(lists:append([role_acl_permissions(RoleId) || RoleId <- RoleIds]));
        _ ->
            []
    end.

-spec role_acl_permissions(integer()) -> [binary()].
role_acl_permissions(RoleId) ->
    try adm_index_handler:role_acl(RoleId) of
        {_RoleName, Permissions, _MenuPaths} when is_list(Permissions) ->
            Permissions
    catch
        _:_ ->
            []
    end.

-spec normalize_role_ids(term()) -> [integer()].
normalize_role_ids(RoleId) when is_integer(RoleId), RoleId > 0 ->
    [RoleId];
normalize_role_ids(RoleIds) when is_list(RoleIds) ->
    lists:usort([Id || Value <- RoleIds, Id <- [normalize_role_id(Value)], Id > 0]);
normalize_role_ids(Value) ->
    case normalize_role_id(Value) of
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
