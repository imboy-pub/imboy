-module(adm_admin_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").

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
            assign_role ->
                assign_role_action(Method, Req0, State);
            config_features ->
                config_features_action(Method, Req0, State);
            config_policy_meta ->
                config_policy_meta_action(Method, Req0, State);
            config_policy_saved ->
                config_policy_saved_action(Method, Req0, State);
            config_policy ->
                config_policy_action(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

-spec config_features_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_features_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, imboy_feature:all());
        {error, Req1} ->
            Req1
    end;
config_features_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec config_policy_meta_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_policy_meta_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, imboy_policy:meta_view());
        {error, Req1} ->
            Req1
    end;
config_policy_meta_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec config_policy_saved_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_policy_saved_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, imboy_policy:saved_view());
        {error, Req1} ->
            Req1
    end;
config_policy_saved_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec config_policy_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_policy_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, imboy_policy:effective_view());
        {error, Req1} ->
            Req1
    end;
config_policy_action(<<"PUT">>, Req0, State) ->
    save_policy_action(Req0, State);
config_policy_action(<<"POST">>, Req0, State) ->
    save_policy_action(Req0, State);
config_policy_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec save_policy_action(cowboy_req:req(), map()) -> cowboy_req:req().
save_policy_action(Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case imboy_policy:save_admin_config(PostVals) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"admins:read">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Filters = extract_list_filters(Req0),
            case query_admin_page(Page, Size, Filters) of
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
    case ensure_permission(State, <<"admins:create">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Account = normalize_binary(maps:get(<<"account">>, PostVals, <<>>)),
            Password = normalize_binary(
                maps:get(<<"pwd">>, PostVals, maps:get(<<"password">>, PostVals, <<>>))
            ),
            RoleId = normalize_positive_int(maps:get(<<"role_id">>, PostVals, 0)),
            Status = normalize_status(maps:get(<<"status">>, PostVals, 1)),
            Nickname0 = normalize_binary(maps:get(<<"nickname">>, PostVals, <<>>)),
            Nickname =
                case Nickname0 of
                    <<>> ->
                        Account;
                    _ ->
                        Nickname0
                end,
            Email = normalize_binary(maps:get(<<"email">>, PostVals, <<>>)),
            Mobile = normalize_binary(maps:get(<<"mobile">>, PostVals, <<>>)),
            case validate_create_payload(Account, Password, RoleId) of
                ok ->
                    Data0 = #{
                        <<"account">> => Account,
                        <<"password">> => Password,
                        <<"nickname">> => Nickname,
                        <<"role_id">> => [RoleId],
                        <<"status">> => Status
                    },
                    Data1 = maybe_put_binary(Data0, <<"email">>, Email),
                    Data2 = maybe_put_binary(Data1, <<"mobile">>, Mobile),
                    case adm_user_logic:save(Data2) of
                        {ok, _} ->
                            elib_response:success(Req0, #{});
                        {error, Reason} ->
                            elib_response:error(Req0, to_error_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
create_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec assign_role_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
assign_role_action(<<"PUT">>, Req0, State) ->
    assign_role_handle(Req0, State);
assign_role_action(<<"POST">>, Req0, State) ->
    assign_role_handle(Req0, State);
assign_role_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec assign_role_handle(cowboy_req:req(), map()) -> cowboy_req:req().
assign_role_handle(Req0, State) ->
    case ensure_permission(State, <<"admins:assign_role">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            AdminId = parse_hashid_or_int(
                maps:get(<<"admin_id">>, PostVals, maps:get(<<"uid">>, PostVals, 0))
            ),
            RoleId = normalize_positive_int(maps:get(<<"role_id">>, PostVals, 0)),
            case {AdminId > 0, RoleId > 0} of
                {true, true} ->
                    case adm_user_logic:assign_roles(AdminId, [RoleId]) of
                        ok ->
                            flush_admin_permission_cache(AdminId),
                            elib_response:success(Req0, #{});
                        {error, Reason} ->
                            elib_response:error(Req0, to_error_binary(Reason), ?ERR_BAD_REQUEST)
                    end;
                _ ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec extract_list_filters(cowboy_req:req()) -> map().
extract_list_filters(Req0) ->
    {ok, Status} = elib_param:int(status, Req0, -1),
    {ok, RoleId} = elib_param:int(role_id, Req0, -1),
    {ok, Keyword0} = elib_param:binary(keyword, Req0, <<>>),
    Keyword = normalize_binary(Keyword0),
    #{
        status => Status,
        role_id => RoleId,
        keyword => Keyword
    }.

-spec query_admin_page(pos_integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
query_admin_page(Page, Size, Filters) ->
    Tb = adm_user_repo:tablename(),
    BaseWhere = <<" WHERE status >= -1">>,
    {WhereSql, Params} = build_where_sql(Filters, 1, [], []),
    CountSql = iolist_to_binary([
        <<"SELECT COUNT(*) AS count FROM ">>, Tb, BaseWhere, WhereSql
    ]),
    case elib_pg:one(CountSql, Params) of
        {ok, CountRow} when is_map(CountRow) ->
            Total = get_count(CountRow),
            Offset = (Page - 1) * Size,
            LimitPos = integer_to_binary(length(Params) + 1),
            OffsetPos = integer_to_binary(length(Params) + 2),
            DataSql = iolist_to_binary([
                <<"SELECT id,account,mobile,email,nickname,avatar,role_id,">>,
                <<"login_count,last_login_ip,last_login_at,status,created_at ">>,
                <<"FROM ">>,
                Tb,
                BaseWhere,
                WhereSql,
                <<" ORDER BY created_at DESC, id DESC LIMIT $">>,
                LimitPos,
                <<" OFFSET $">>,
                OffsetPos
            ]),
            case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                {ok, Rows} ->
                    Items = [normalize_admin_row(Row) || Row <- Rows],
                    {ok,
                        #{
                            list => Items,
                            total => Total,
                            page => Page,
                            size => Size
                        }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            {ok, #{list => [], total => 0, page => Page, size => Size}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_where_sql(map(), pos_integer(), [binary()], list()) -> {binary(), list()}.
build_where_sql(Filters, Index0, Parts0, Params0) ->
    Status = maps:get(status, Filters, -1),
    RoleId = maps:get(role_id, Filters, -1),
    Keyword = maps:get(keyword, Filters, <<>>),

    {Index1, Parts1, Params1} =
        case Status of
            -1 ->
                {Index0, Parts0, Params0};
            _ ->
                StatusCond = <<" AND status = $", (integer_to_binary(Index0))/binary>>,
                {Index0 + 1, Parts0 ++ [StatusCond], Params0 ++ [Status]}
        end,

    {Index2, Parts2, Params2} =
        case RoleId > 0 of
            true ->
                RoleCond = <<" AND $", (integer_to_binary(Index1))/binary, " = ANY(role_id)">>,
                {Index1 + 1, Parts1 ++ [RoleCond], Params1 ++ [RoleId]};
            false ->
                {Index1, Parts1, Params1}
        end,

    case Keyword =/= <<>> of
        true ->
            Like = <<"%", Keyword/binary, "%">>,
            Pos = integer_to_binary(Index2),
            KeywordCond =
                <<" AND (account ILIKE $", Pos/binary, " OR nickname ILIKE $", Pos/binary,
                  " OR email ILIKE $", Pos/binary, " OR mobile ILIKE $", Pos/binary, ")">>,
            {iolist_to_binary(Parts2 ++ [KeywordCond]), Params2 ++ [Like]};
        false ->
            {iolist_to_binary(Parts2), Params2}
    end.

-spec normalize_admin_row(map()) -> map().
normalize_admin_row(Row) ->
    RoleIds = normalize_role_ids(maps:get(<<"role_id">>, Row, [])),
    PrimaryRoleId =
        case RoleIds of
            [RoleId | _] when is_integer(RoleId), RoleId > 0 ->
                RoleId;
            _ ->
                0
        end,
    Row1 = Row#{
        <<"role_id">> => PrimaryRoleId,
        <<"role_ids">> => RoleIds
    },
    elib_hashids:replace_fields(Row1, [<<"id">>]).

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

-spec validate_create_payload(binary(), binary(), integer()) -> ok | {error, binary()}.
validate_create_payload(Account, _Password, _RoleId) when byte_size(Account) < 3 ->
    {error, <<"账号长度至少 3 位"/utf8>>};
validate_create_payload(_Account, Password, _RoleId) when byte_size(Password) < 6 ->
    {error, <<"密码长度至少 6 位"/utf8>>};
validate_create_payload(_Account, _Password, RoleId) when RoleId =< 0 ->
    {error, <<"role_id 无效"/utf8>>};
validate_create_payload(_Account, _Password, _RoleId) ->
    ok.

-spec maybe_put_binary(map(), binary(), binary()) -> map().
maybe_put_binary(Data, _Key, <<>>) ->
    Data;
maybe_put_binary(Data, Key, Value) ->
    Data#{Key => Value}.

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

-spec parse_hashid_or_int(term()) -> integer().
parse_hashid_or_int(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_hashid_or_int(Value) when is_list(Value) ->
    parse_hashid_or_int(ec_cnv:to_binary(Value));
parse_hashid_or_int(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            normalize_positive_int(Value);
        false ->
            case catch elib_hashids:decode(Value) of
                [Id] when is_integer(Id), Id > 0 ->
                    Id;
                _ ->
                    0
            end
    end;
parse_hashid_or_int(_) ->
    0.

-spec get_count(map()) -> integer().
get_count(Row) ->
    case maps:get(<<"count">>, Row, 0) of
        Val when is_integer(Val), Val >= 0 ->
            Val;
        Val ->
            normalize_positive_int(Val)
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
has_permission(AdmUserId, Permission) when is_integer(AdmUserId), AdmUserId > 0, is_binary(Permission) ->
    UserPermissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:member(Permission, UserPermissions);
has_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> [binary()].
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_admin_permission, AdmUserId},
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

-spec flush_admin_permission_cache(integer()) -> ok.
flush_admin_permission_cache(AdminId) ->
    Keys = [
        {adm_user_current, AdminId},
        {adm_user_rbac, AdminId},
        {adm_user_admin_permission, AdminId},
        {adm_user_group_permission, AdminId},
        {adm_user_moment_permission, AdminId},
        {adm_user_report_permission, AdminId},
        {adm_user_feedback_permission, AdminId},
        {adm_user_stats_permission, AdminId}
    ],
    _ = [imboy_cache:flush(Key) || Key <- Keys],
    ok.
