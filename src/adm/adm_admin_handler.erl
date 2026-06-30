-module(adm_admin_handler).
-compile([nowarn_deprecated_catch]).

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
            config_policy_bootstrap ->
                config_policy_bootstrap_action(Method, Req0, State);
            config_policy_meta ->
                config_policy_meta_action(Method, Req0, State);
            config_policy_preview ->
                config_policy_preview_action(Method, Req0, State);
            config_policy_saved ->
                config_policy_saved_action(Method, Req0, State);
            config_policy ->
                config_policy_action(Method, Req0, State);
            config_sidebar ->
                config_sidebar_action(Method, Req0, State);
            config_feedback_workflow ->
                config_feedback_workflow_action(Method, Req0, State);
            muted_users_list ->
                muted_users_list_action(Method, Req0, State);
            muted_users_unmute ->
                muted_users_unmute_action(Method, Req0, State);
            muted_users_unmute_batch ->
                muted_users_unmute_batch_action(Method, Req0, State);
            push_token_list ->
                push_token_list_action(Method, Req0, State);
            compliance_key_list ->
                compliance_key_list_action(Method, Req0, State);
            compliance_key_create ->
                compliance_key_create_action(Method, Req0, State);
            compliance_key_revoke ->
                compliance_key_revoke_action(Method, Req0, State);
            disable ->
                disable_action(Method, Req0, State);
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

-spec config_policy_bootstrap_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_policy_bootstrap_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, imboy_policy:admin_config_view());
        {error, Req1} ->
            Req1
    end;
config_policy_bootstrap_action(_, Req0, _State) ->
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

-spec config_policy_preview_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_policy_preview_action(<<"POST">>, Req0, State) ->
    preview_policy_action(Req0, State);
config_policy_preview_action(<<"PUT">>, Req0, State) ->
    preview_policy_action(Req0, State);
config_policy_preview_action(_, Req0, _State) ->
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

-spec preview_policy_action(cowboy_req:req(), map()) -> cowboy_req:req().
preview_policy_action(Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case imboy_policy:preview_admin_config(PostVals) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Details} ->
                    policy_bad_request(Req0, Msg, Details);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec save_policy_action(cowboy_req:req(), map()) -> cowboy_req:req().
save_policy_action(Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case imboy_policy:save_admin_config(PostVals) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Details} ->
                    policy_bad_request(Req0, Msg, Details);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.

-spec policy_bad_request(cowboy_req:req(), binary(), map()) -> cowboy_req:req().
policy_bad_request(Req0, Msg, Details) ->
    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST, #{<<"details">> => Details}).

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
            AdminId = parse_id(
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
    {WhereSql, Params} = build_where_sql(Filters, 1, [], []),
    case adm_user_ds:page_with_where_sql(WhereSql, Params, Page, Size) of
        {ok, #{list := Rows} = Result} ->
            Items = [normalize_admin_row(Row) || Row <- Rows],
            {ok, Result#{list => Items}};
        Other ->
            Other
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
    Row1.

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

-spec parse_id(term()) -> integer().
parse_id(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_id(Value) when is_list(Value) ->
    parse_id(ec_cnv:to_binary(Value));
parse_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            normalize_positive_int(Value);
        false ->
            case catch ec_cnv:to_integer(Value) of
                Id when is_integer(Id), Id > 0 ->
                    Id;
                _ ->
                    0
            end
    end;
parse_id(_) ->
    0.

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

%% ===================================================================
%% 禁言用户管理
%% ===================================================================

-spec muted_users_list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
muted_users_list_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Now = erlang:system_time(millisecond),
            AllMuted =
                case ets:whereis(msg_rate_muted) of
                    undefined ->
                        [];
                    _ ->
                        ets:foldl(
                            fun({Uid, MuteUntil}, Acc) ->
                                case MuteUntil > Now of
                                    true ->
                                        RemainingMs = MuteUntil - Now,
                                        [
                                            #{
                                                <<"uid">> => Uid,
                                                <<"user_id">> => Uid,
                                                <<"mute_until">> => MuteUntil,
                                                <<"remaining_seconds">> => RemainingMs div 1000
                                            }
                                            | Acc
                                        ];
                                    false ->
                                        Acc
                                end
                            end,
                            [],
                            msg_rate_muted
                        )
                end,
            %% sort by remaining_seconds desc (longest muted first)
            Sorted = lists:sort(
                fun(A, B) ->
                    maps:get(<<"remaining_seconds">>, A) >= maps:get(<<"remaining_seconds">>, B)
                end,
                AllMuted
            ),
            Total = length(Sorted),
            Offset = (Page - 1) * Size,
            PageItems = lists:sublist(Sorted, Offset + 1, Size),
            elib_response:success(Req0, #{
                <<"list">> => PageItems,
                <<"total">> => Total,
                <<"page">> => Page,
                <<"size">> => Size
            });
        {error, Req1} ->
            Req1
    end;
muted_users_list_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec muted_users_unmute_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
muted_users_unmute_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            UidRaw = maps:get(<<"user_id">>, PostVals, maps:get(<<"uid">>, PostVals, <<>>)),
            Uid = parse_id(UidRaw),
            case Uid > 0 of
                true ->
                    msg_rate_logic:unmute(Uid),
                    elib_response:success(Req0, #{});
                false ->
                    elib_response:error(Req0, <<"uid 无效"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
muted_users_unmute_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec muted_users_unmute_batch_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
muted_users_unmute_batch_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Uids = maps:get(<<"uids">>, PostVals, []),
            case is_list(Uids) andalso length(Uids) > 0 of
                true ->
                    Results = lists:map(
                        fun(UidRaw) ->
                            Uid = parse_id(UidRaw),
                            case Uid > 0 of
                                true ->
                                    msg_rate_logic:unmute(Uid),
                                    #{<<"uid">> => UidRaw, <<"ok">> => true};
                                false ->
                                    #{<<"uid">> => UidRaw, <<"ok">> => false}
                            end
                        end,
                        Uids
                    ),
                    SuccessCount = length([1 || #{<<"ok">> := true} <- Results]),
                    elib_response:success(Req0, #{
                        <<"total">> => length(Uids),
                        <<"success">> => SuccessCount
                    });
                false ->
                    elib_response:error(Req0, <<"uids 必须是非空数组"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
muted_users_unmute_batch_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 推送 Token 管理
%% ===================================================================

-spec push_token_list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
push_token_list_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            case push_token_ds:list_page(Page, Size) of
                {ok, #{list := Rows, total := Total}} ->
                    Items = Rows,
                    elib_response:success(Req0, #{
                        <<"list">> => Items,
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"size">> => Size
                    });
                {error, Reason} ->
                    elib_response:error(Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
push_token_list_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 合规密钥管理
%% ===================================================================

-spec compliance_key_list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
compliance_key_list_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            case compliance_key_ds:list_all() of
                {ok, Rows} ->
                    elib_response:success(Req0, #{<<"list">> => Rows});
                {error, Reason} ->
                    elib_response:error(Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
compliance_key_list_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec compliance_key_create_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
compliance_key_create_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            PublicKey = normalize_binary(maps:get(<<"public_key">>, PostVals, <<>>)),
            PrivateKeyEncrypted = normalize_binary(
                maps:get(<<"private_key_encrypted">>, PostVals, <<>>)
            ),
            case {byte_size(PublicKey) > 0, byte_size(PrivateKeyEncrypted) > 0} of
                {true, true} ->
                    KeyId = elib_id:gen(<<"ck_">>),
                    AdmUserId = maps:get(adm_user_id, State, 0),
                    case
                        compliance_key_ds:create(KeyId, PublicKey, PrivateKeyEncrypted, AdmUserId)
                    of
                        {ok, _} ->
                            elib_response:success(Req0, #{<<"key_id">> => KeyId});
                        {error, Reason} ->
                            elib_response:error(
                                Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                _ ->
                    elib_response:error(
                        Req0, <<"public_key 和 private_key_encrypted 不能为空"/utf8>>, ?ERR_BAD_REQUEST
                    )
            end;
        {error, Req1} ->
            Req1
    end;
compliance_key_create_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec compliance_key_revoke_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
compliance_key_revoke_action(<<"POST">>, Req0, State) ->
    compliance_key_revoke_handle(Req0, State);
compliance_key_revoke_action(<<"PUT">>, Req0, State) ->
    compliance_key_revoke_handle(Req0, State);
compliance_key_revoke_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec disable_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
disable_action(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"admins:delete">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            AdminId = parse_id(
                maps:get(<<"admin_id">>, PostVals, maps:get(<<"uid">>, PostVals, 0))
            ),
            case AdminId > 0 of
                true ->
                    case adm_user_ds:update(AdminId, #{<<"status">> => 0}) of
                        {ok, _} ->
                            flush_admin_permission_cache(AdminId),
                            elib_response:success(Req0, #{});
                        {error, Reason} ->
                            elib_response:error(
                                Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                false ->
                    elib_response:error(Req0, <<"admin_id 无效"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
disable_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 侧边栏配置
%% ===================================================================

-spec config_sidebar_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_sidebar_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"settings:view">>, Req0) of
        ok ->
            elib_response:success(Req0, default_sidebar_config());
        {error, Req1} ->
            Req1
    end;
config_sidebar_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 反馈工作流配置
%% ===================================================================

-spec config_feedback_workflow_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config_feedback_workflow_action(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"feedback:read">>, Req0) of
        ok ->
            elib_response:success(Req0, get_feedback_workflow_config());
        {error, Req1} ->
            Req1
    end;
config_feedback_workflow_action(<<"PUT">>, Req0, State) ->
    save_feedback_workflow_action(Req0, State);
config_feedback_workflow_action(<<"POST">>, Req0, State) ->
    save_feedback_workflow_action(Req0, State);
config_feedback_workflow_action(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec save_feedback_workflow_action(cowboy_req:req(), map()) -> cowboy_req:req().
save_feedback_workflow_action(Req0, State) ->
    case ensure_permission(State, <<"feedback:reply">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Templates = maps:get(<<"reply_templates">>, PostVals, []),
            RawSla = normalize_positive_int(maps:get(<<"sla_hours">>, PostVals, 24)),
            SlaHours = max(1, min(720, RawSla)),
            Config = #{
                <<"reply_templates">> => Templates,
                <<"sla_hours">> => SlaHours
            },
            application:set_env(imboy, feedback_workflow_config, Config),
            elib_response:success(Req0, Config);
        {error, Req1} ->
            Req1
    end.

-spec get_feedback_workflow_config() -> map().
get_feedback_workflow_config() ->
    case application:get_env(imboy, feedback_workflow_config) of
        {ok, Config} when is_map(Config) ->
            Config;
        _ ->
            default_feedback_workflow_config()
    end.

-spec default_feedback_workflow_config() -> map().
default_feedback_workflow_config() ->
    #{
        <<"reply_templates">> => [
            <<"感谢反馈，我们已收到并会尽快处理。"/utf8>>,
            <<"问题已记录到修复队列，预计将在后续版本优化。"/utf8>>,
            <<"请补充相关截图和复现步骤，便于我们进一步排查。"/utf8>>,
            <<"该反馈已转交对应业务负责人处理，请留意后续通知。"/utf8>>
        ],
        <<"sla_hours">> => 24
    }.

-spec default_sidebar_config() -> map().
default_sidebar_config() ->
    #{
        <<"title">> => <<"Imboy Admin">>,
        <<"items">> => [
            #{
                <<"path">> => <<"/dashboard">>,
                <<"icon">> => <<"LayoutDashboard">>,
                <<"label">> => <<"仪表盘"/utf8>>,
                <<"roles">> => [1, 2, 3],
                <<"permission">> => <<"dashboard:view">>
            },
            #{
                <<"label">> => <<"运营中心"/utf8>>,
                <<"icon">> => <<"Users">>,
                <<"children">> => [
                    #{
                        <<"path">> => <<"/users">>,
                        <<"icon">> => <<"Users">>,
                        <<"label">> => <<"用户管理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"users:read">>
                    },
                    #{
                        <<"path">> => <<"/groups">>,
                        <<"icon">> => <<"UsersRound">>,
                        <<"label">> => <<"群组管理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"groups:read">>
                    },
                    #{
                        <<"path">> => <<"/channels">>,
                        <<"icon">> => <<"Radio">>,
                        <<"label">> => <<"频道管理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"channels:read">>
                    },
                    #{
                        <<"path">> => <<"/moments">>,
                        <<"icon">> => <<"Camera">>,
                        <<"label">> => <<"朋友圈管理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"moments:read">>
                    }
                ]
            },
            #{
                <<"label">> => <<"治理中心"/utf8>>,
                <<"icon">> => <<"FileText">>,
                <<"children">> => [
                    #{
                        <<"path">> => <<"/reports">>,
                        <<"icon">> => <<"FileText">>,
                        <<"label">> => <<"举报中心"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"reports:read">>
                    },
                    #{
                        <<"path">> => <<"/feedback">>,
                        <<"icon">> => <<"MessageCircle">>,
                        <<"label">> => <<"反馈处理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"feedback:read">>
                    }
                ]
            },
            #{
                <<"label">> => <<"审计中心"/utf8>>,
                <<"icon">> => <<"FileText">>,
                <<"children">> => [
                    #{
                        <<"path">> => <<"/groups/context">>,
                        <<"icon">> => <<"UsersRound">>,
                        <<"label">> => <<"群上下文入口"/utf8>>,
                        <<"roles">> => [1, 2, 3]
                    },
                    #{
                        <<"path">> => <<"/messages">>,
                        <<"icon">> => <<"MessageSquare">>,
                        <<"label">> => <<"消息管理"/utf8>>,
                        <<"roles">> => [1, 2, 3],
                        <<"permission">> => <<"messages:read">>
                    },
                    #{
                        <<"path">> => <<"/logout-applications">>,
                        <<"icon">> => <<"UserMinus">>,
                        <<"label">> => <<"注销申请"/utf8>>,
                        <<"roles">> => [1, 2, 3],
                        <<"permission">> => <<"logout_applications:read">>
                    },
                    #{
                        <<"path">> => <<"/logs">>,
                        <<"icon">> => <<"FileText">>,
                        <<"label">> => <<"日志审计"/utf8>>,
                        <<"roles">> => [1, 3],
                        <<"permission">> => <<"logs:view">>
                    }
                ]
            },
            #{
                <<"label">> => <<"系统配置"/utf8>>,
                <<"icon">> => <<"Settings">>,
                <<"children">> => [
                    #{
                        <<"path">> => <<"/settings">>,
                        <<"icon">> => <<"Settings">>,
                        <<"label">> => <<"系统设置"/utf8>>,
                        <<"roles">> => [1],
                        <<"permission">> => <<"settings:view">>
                    },
                    #{
                        <<"path">> => <<"/admins">>,
                        <<"icon">> => <<"Shield">>,
                        <<"label">> => <<"管理员"/utf8>>,
                        <<"roles">> => [1],
                        <<"permission">> => <<"admins:read">>
                    },
                    #{
                        <<"path">> => <<"/roles">>,
                        <<"icon">> => <<"KeyRound">>,
                        <<"label">> => <<"角色权限"/utf8>>,
                        <<"roles">> => [1, 3],
                        <<"permission">> => <<"roles:view">>
                    },
                    #{
                        <<"path">> => <<"/plugins">>,
                        <<"icon">> => <<"Puzzle">>,
                        <<"label">> => <<"插件管理"/utf8>>,
                        <<"roles">> => [1, 2],
                        <<"permission">> => <<"plugins:read">>
                    },
                    #{
                        <<"path">> => <<"/storage">>,
                        <<"icon">> => <<"HardDrive">>,
                        <<"label">> => <<"存储管理"/utf8>>,
                        <<"roles">> => [1],
                        <<"permission">> => <<"storage:view">>
                    },
                    #{
                        <<"path">> => <<"/system-health">>,
                        <<"icon">> => <<"HeartPulse">>,
                        <<"label">> => <<"系统健康"/utf8>>,
                        <<"roles">> => [1],
                        <<"permission">> => <<"settings:view">>
                    }
                ]
            }
        ]
    }.

-spec compliance_key_revoke_handle(cowboy_req:req(), map()) -> cowboy_req:req().
compliance_key_revoke_handle(Req0, State) ->
    case ensure_permission(State, <<"settings:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            KeyId = normalize_binary(maps:get(<<"key_id">>, PostVals, <<>>)),
            AdmUserId = maps:get(adm_user_id, State, 0),
            case byte_size(KeyId) > 0 of
                true ->
                    case compliance_key_ds:revoke(KeyId, AdmUserId) of
                        {ok, N} when N > 0 ->
                            elib_response:success(Req0, #{});
                        {ok, 0} ->
                            elib_response:error(Req0, <<"密钥不存在或已撤销"/utf8>>, ?ERR_NOT_FOUND);
                        {error, Reason} ->
                            elib_response:error(
                                Req0, to_error_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                false ->
                    elib_response:error(Req0, <<"key_id 不能为空"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end.
