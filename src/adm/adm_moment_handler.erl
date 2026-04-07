-module(adm_moment_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_moment_handler, Action) of
            undefined ->
                case Action of
                    list -> list(Method, Req0, State);
                    detail -> detail(Method, Req0, State);
                    delete -> delete_action(Method, Req0, State);
                    report_list -> report_list(Method, Req0, State);
                    report_resolve -> report_resolve(Method, Req0, State);
                    report_batch_resolve -> report_batch_resolve(Method, Req0, State);
                    false -> Req0
                end;
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        case Action of
                            list -> list(Method, Req0, State);
                            detail -> detail(Method, Req0, State);
                            delete -> delete_action(Method, Req0, State);
                            report_list -> report_list(Method, Req0, State);
                            report_resolve -> report_resolve(Method, Req0, State);
                            report_batch_resolve -> report_batch_resolve(Method, Req0, State);
                            false -> Req0
                        end;
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"moments:read">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Qs = cowboy_req:parse_qs(Req0),
            Keyword = proplists:get_value(<<"keyword">>, Qs, undefined),
            Uid = proplists:get_value(<<"uid">>, Qs, undefined),
            Status = parse_qs_int(proplists:get_value(<<"status">>, Qs), -2, -2, 10),
            case moment_logic:admin_list_posts(Keyword, Uid, Status, Page, Size) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
list(_, Req0, _State) ->
    Req0.

-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"moments:read">>, Req0) of
        ok ->
            MomentId = cowboy_req:binding(moment_id, Req0),
            case MomentId of
                undefined ->
                    elib_response:error(Req0, <<"动态ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case moment_logic:admin_post_detail(MomentId) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
detail(_, Req0, _State) ->
    Req0.

-spec delete_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"moments:delete">>, Req0) of
        ok ->
            AdmUid = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            MomentId = maps:get(<<"moment_id">>, PostVals, <<>>),
            Reason = maps:get(<<"reason">>, PostVals, <<>>),
            case moment_logic:admin_delete_post(AdmUid, MomentId, Reason) of
                ok ->
                    elib_response:success(Req0, #{}, <<"删除成功"/utf8>>);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
delete_action(_, Req0, _State) ->
    Req0.

-spec report_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
report_list(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"moments:report:read">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Qs = cowboy_req:parse_qs(Req0),
            Status = parse_qs_int(proplists:get_value(<<"status">>, Qs), -1, -1, 10),
            case moment_logic:admin_list_reports(Status, Page, Size) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
report_list(_, Req0, _State) ->
    Req0.

-spec report_resolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
report_resolve(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"moments:report:handle">>, Req0) of
        ok ->
            AdmUid = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            ReportId = maps:get(<<"report_id">>, PostVals, <<>>),
            Result = ec_cnv:to_integer(maps:get(<<"result">>, PostVals, 0)),
            Note = maps:get(<<"note">>, PostVals, <<>>),
            case moment_logic:admin_resolve_report(AdmUid, ReportId, Result, Note) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
report_resolve(_, Req0, _State) ->
    Req0.

-spec report_batch_resolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
report_batch_resolve(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"moments:report:handle">>, Req0) of
        ok ->
            AdmUid = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            RawIds = maps:get(<<"report_ids">>, PostVals, []),
            Result = ec_cnv:to_integer(maps:get(<<"result">>, PostVals, 0)),
            Note = maps:get(<<"note">>, PostVals, <<>>),
            ReportIds = normalize_report_ids(RawIds, #{}, []),
            case {ReportIds, lists:member(Result, [1, 2])} of
                {[], _} ->
                    elib_response:error(Req0, <<"report_ids is empty"/utf8>>, ?ERR_BAD_REQUEST);
                {_, false} ->
                    elib_response:error(Req0, <<"result must be 1 or 2"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    {SuccessCount, FailedIds0} =
                        lists:foldl(
                            fun(ReportId, {Succ, FailedAcc}) ->
                                case moment_logic:admin_resolve_report(AdmUid, ReportId, Result, Note) of
                                    ok ->
                                        {Succ + 1, FailedAcc};
                                    {error, _} ->
                                        {Succ, [ReportId | FailedAcc]}
                                end
                            end,
                            {0, []},
                            ReportIds
                        ),
                    FailedIds = lists:reverse(FailedIds0),
                    Payload = #{
                        <<"success_count">> => SuccessCount,
                        <<"failed_count">> => length(FailedIds),
                        <<"failed_ids">> => FailedIds
                    },
                    elib_response:success(Req0, Payload)
            end;
        {error, Req1} ->
            Req1
    end;
report_batch_resolve(_, Req0, _State) ->
    Req0.

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
    Permissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:member(Permission, Permissions);
has_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> list(binary()).
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_moment_permission, AdmUserId},
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
    try ec_cnv:to_integer(Hash) of
        Int when is_integer(Int), Int > 0 ->
            Int;
        _ ->
            0
    catch
        _:_ ->
            0
    end.
