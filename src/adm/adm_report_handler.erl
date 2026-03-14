-module(adm_report_handler).

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
            create -> create_action(Method, Req0, State, auto);
            list -> list_action(Method, Req0, State, auto);
            resolve -> resolve_action(Method, Req0, State, auto);
            batch_resolve -> batch_resolve_action(Method, Req0, State, auto);
            group_list -> list_action(Method, Req0, State, <<"group">>);
            group_resolve -> resolve_action(Method, Req0, State, <<"group">>);
            group_batch_resolve -> batch_resolve_action(Method, Req0, State, <<"group">>);
            channel_list -> list_action(Method, Req0, State, <<"channel">>);
            channel_resolve -> resolve_action(Method, Req0, State, <<"channel">>);
            channel_batch_resolve -> batch_resolve_action(Method, Req0, State, <<"channel">>);
            user_list -> list_action(Method, Req0, State, <<"user">>);
            user_resolve -> resolve_action(Method, Req0, State, <<"user">>);
            user_batch_resolve -> batch_resolve_action(Method, Req0, State, <<"user">>);
            false -> Req0
        end,
    {ok, Req1, State}.

-spec create_action(binary(), cowboy_req:req(), map(), auto | binary()) -> cowboy_req:req().
create_action(<<"POST">>, Req0, State, TargetOverride) ->
    PostVals = elib_param:post(Req0),
    TargetType = target_type_from_body(PostVals, TargetOverride),
    case ensure_any_permission(State, [<<"reports:handle">>, <<"moments:report:handle">>], Req0) of
        ok ->
            case ensure_target_feature(Req0, TargetType) of
                ok ->
                    AdmUid = maps:get(adm_user_id, State, 0),
                    TargetId = maps:get(<<"target_id">>, PostVals, <<>>),
                    Reason = maps:get(<<"reason">>, PostVals, <<>>),
                    Desc = maps:get(<<"description">>, PostVals, <<>>),
                    case report_logic:create(AdmUid, TargetType, TargetId, Reason, Desc) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
create_action(_, Req0, _State, _TargetOverride) ->
    Req0.

-spec list_action(binary(), cowboy_req:req(), map(), auto | binary()) -> cowboy_req:req().
list_action(<<"GET">>, Req0, State, TargetOverride) ->
    Qs = cowboy_req:parse_qs(Req0),
    TargetType = target_type_from_qs(Qs, TargetOverride),
    case ensure_any_permission(State, [<<"reports:read">>, <<"moments:report:read">>], Req0) of
        ok ->
            case ensure_target_feature(Req0, TargetType) of
                ok ->
                    {Page, Size} = elib_param:page(Req0),
                    Status = parse_qs_int(proplists:get_value(<<"status">>, Qs), -1, -1, 10),
                    TargetId = proplists:get_value(<<"target_id">>, Qs, <<>>),
                    ReporterUid = proplists:get_value(<<"reporter_uid">>, Qs, <<>>),
                    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
                    Filter = #{
                        target_id => TargetId,
                        reporter_uid => ReporterUid,
                        keyword => Keyword
                    },
                    case report_logic:admin_list(TargetType, Status, Page, Size, Filter) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
list_action(_, Req0, _State, _TargetOverride) ->
    Req0.

-spec resolve_action(binary(), cowboy_req:req(), map(), auto | binary()) -> cowboy_req:req().
resolve_action(<<"POST">>, Req0, State, TargetOverride) ->
    PostVals = elib_param:post(Req0),
    TargetType = target_type_from_body(PostVals, TargetOverride),
    case ensure_any_permission(State, [<<"reports:handle">>, <<"moments:report:handle">>], Req0) of
        ok ->
            case ensure_target_feature(Req0, TargetType) of
                ok ->
                    AdmUid = maps:get(adm_user_id, State, 0),
                    ReportId = maps:get(<<"report_id">>, PostVals, <<>>),
                    Result = ec_cnv:to_integer(maps:get(<<"result">>, PostVals, 0)),
                    Note = maps:get(<<"note">>, PostVals, <<>>),
                    case report_logic:admin_resolve(AdmUid, TargetType, ReportId, Result, Note) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
resolve_action(_, Req0, _State, _TargetOverride) ->
    Req0.

-spec batch_resolve_action(binary(), cowboy_req:req(), map(), auto | binary()) -> cowboy_req:req().
batch_resolve_action(<<"POST">>, Req0, State, TargetOverride) ->
    PostVals = elib_param:post(Req0),
    TargetType = target_type_from_body(PostVals, TargetOverride),
    case ensure_any_permission(State, [<<"reports:handle">>, <<"moments:report:handle">>], Req0) of
        ok ->
            case ensure_target_feature(Req0, TargetType) of
                ok ->
                    AdmUid = maps:get(adm_user_id, State, 0),
                    RawIds = maps:get(<<"report_ids">>, PostVals, []),
                    Result = ec_cnv:to_integer(maps:get(<<"result">>, PostVals, 0)),
                    Note = maps:get(<<"note">>, PostVals, <<>>),
                    ReportIds = normalize_report_ids(RawIds, #{}, []),
                    case {ReportIds, lists:member(Result, [1, 2])} of
                        {[], _} ->
                            elib_response:error(Req0, <<"report_ids is empty"/utf8>>, ?ERR_BAD_REQUEST);
                        _ ->
                            case report_logic:admin_batch_resolve(AdmUid, TargetType, ReportIds, Result, Note) of
                                {ok, Payload} ->
                                    elib_response:success(Req0, Payload);
                                {error, Msg} ->
                                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                            end
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
batch_resolve_action(_, Req0, _State, _TargetOverride) ->
    Req0.

-spec target_type_from_qs(list(), auto | binary()) -> binary().
target_type_from_qs(_Qs, TargetType) when is_binary(TargetType) ->
    TargetType;
target_type_from_qs(Qs, auto) ->
    Raw = proplists:get_value(<<"target_type">>, Qs, <<>>),
    normalize_target_type(Raw, <<>>).

-spec target_type_from_body(map(), auto | binary()) -> binary().
target_type_from_body(_PostVals, TargetType) when is_binary(TargetType) ->
    TargetType;
target_type_from_body(PostVals, auto) ->
    Raw = maps:get(<<"target_type">>, PostVals, <<>>),
    normalize_target_type(Raw, <<>>).

-spec normalize_target_type(term(), binary()) -> binary().
normalize_target_type(Value, Default) when is_binary(Value) ->
    normalize_target_type_binary(Value, Default);
normalize_target_type(Value, Default) when is_list(Value) ->
    normalize_target_type_binary(unicode:characters_to_binary(Value), Default);
normalize_target_type(_, Default) ->
    Default.

-spec normalize_target_type_binary(binary(), binary()) -> binary().
normalize_target_type_binary(Value, Default) ->
    Lower = string:lowercase(binary_to_list(Value)),
    case Lower of
        "moment" -> <<"moment">>;
        "moments" -> <<"moment">>;
        "group" -> <<"group">>;
        "groups" -> <<"group">>;
        "channel" -> <<"channel">>;
        "channels" -> <<"channel">>;
        "user" -> <<"user">>;
        "users" -> <<"user">>;
        _ -> Default
    end.

-spec ensure_target_feature(cowboy_req:req(), binary()) -> ok | {error, cowboy_req:req()}.
ensure_target_feature(Req0, TargetType) ->
    case imboy_plugin_registry:required_feature_for_target(admin, adm_report_handler, TargetType) of
        undefined ->
            ok;
        Feature ->
            imboy_feature:ensure_enabled(Req0, Feature)
    end.

-spec ensure_any_permission(map(), [binary()], cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_any_permission(State, Permissions, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_any_permission(AdmUserId, Permissions) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

-spec has_any_permission(term(), [binary()]) -> boolean().
has_any_permission(AdmUserId, PermissionsToCheck) when
    is_integer(AdmUserId),
    AdmUserId > 0,
    is_list(PermissionsToCheck)
->
    UserPermissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:any(fun(Permission) -> lists:member(Permission, UserPermissions) end, PermissionsToCheck);
has_any_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> list(binary()).
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_report_permission, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,role_id">>, Key) of
        AdmUser when is_map(AdmUser) ->
            RoleIds = normalize_role_ids(maps:get(<<"role_id">>, AdmUser, 0)),
            lists:usort(lists:append([role_permissions(RoleId) || RoleId <- RoleIds]));
        _ ->
            []
    end.

-spec role_permissions(integer()) -> list(binary()).
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

-spec parse_qs_int(term(), integer(), integer(), integer()) -> integer().
parse_qs_int(undefined, Default, _Min, _Max) ->
    Default;
parse_qs_int(Value, Default, Min, Max) ->
    case safe_to_integer(Value) of
        {ok, Int} when Int < Min ->
            Min;
        {ok, Int} when Int > Max ->
            Max;
        {ok, Int} ->
            Int;
        error ->
            Default
    end.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(_) ->
    error.

-spec normalize_report_ids(term(), map(), list(binary())) -> list(binary()).
normalize_report_ids([], _Seen, Acc) ->
    lists:reverse(Acc);
normalize_report_ids([Item | Rest], Seen, Acc) ->
    case normalize_report_id(Item) of
        undefined ->
            normalize_report_ids(Rest, Seen, Acc);
        ReportId ->
            case maps:is_key(ReportId, Seen) of
                true ->
                    normalize_report_ids(Rest, Seen, Acc);
                false ->
                    normalize_report_ids(Rest, Seen#{ReportId => true}, [ReportId | Acc])
            end
    end;
normalize_report_ids(_, _Seen, Acc) ->
    lists:reverse(Acc).

-spec normalize_report_id(term()) -> binary() | undefined.
normalize_report_id(Value) ->
    case safe_to_integer(Value) of
        {ok, Int} when Int > 0 ->
            integer_to_binary(Int);
        _ ->
            case Value of
                Bin when is_binary(Bin), Bin =/= <<>> ->
                    case safe_hash_decode(Bin) of
                        Int2 when Int2 > 0 ->
                            integer_to_binary(Int2);
                        _ ->
                            undefined
                    end;
                List when is_list(List), List =/= [] ->
                    case safe_hash_decode(unicode:characters_to_binary(List)) of
                        Int3 when Int3 > 0 ->
                            integer_to_binary(Int3);
                        _ ->
                            undefined
                    end;
                _ ->
                    undefined
            end
    end.

-spec safe_hash_decode(binary()) -> integer().
safe_hash_decode(Hash) ->
    try elib_hashids:decode(Hash) of
        Int when is_integer(Int), Int > 0 ->
            Int;
        _ ->
            0
    catch
        _:_ ->
            0
    end.
