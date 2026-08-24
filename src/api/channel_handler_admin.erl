-module(channel_handler_admin).
-compile([nowarn_deprecated_catch]).
-behavior(cowboy_rest).
-export([init/2, handle_action/3]).
-export([
    admins/2,
    update_admin_role/2,
    remove_subscriber/2,
    create_invitation/2,
    accept_invitation/2,
    reject_invitation/2,
    my_invitations/2,
    sent_invitations/2,
    invitations/2,
    sync/2,
    remove_admin/2
]).
-include("error_code.hrl").
-include("log.hrl").

init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

handle_action(admins, Req, State) -> admins(Req, State);
handle_action(update_admin_role, Req, State) -> update_admin_role(Req, State);
handle_action(remove_subscriber, Req, State) -> remove_subscriber(Req, State);
handle_action(invitations, Req, State) -> invitations(Req, State);
handle_action(create_invitation, Req, State) -> create_invitation(Req, State);
handle_action(accept_invitation, Req, State) -> accept_invitation(Req, State);
handle_action(revoke_invitation, Req, State) -> reject_invitation(Req, State);
handle_action(reject_invitation, Req, State) -> reject_invitation(Req, State);
handle_action(my_invitations, Req, State) -> my_invitations(Req, State);
handle_action(sent_invitations, Req, State) -> sent_invitations(Req, State);
handle_action(sync, Req, State) -> sync(Req, State);
handle_action(remove_admin, Req, State) -> remove_admin(Req, State);
handle_action(false, Req, _State) -> Req.

%% 邀请相关 API（私有频道）
%% ===================================================================

%% @doc 创建邀请
-spec create_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
create_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    InviteeUid = decode_positive_id(maps:get(<<"invitee_uid">>, PostVals, <<>>)),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when InviteeUid =:= 0 ->
            elib_response:error(Req0, <<"被邀请人ID不能为空"/utf8>>);
        _ ->
            case channel_logic:create_invitation(Uid, ChannelId, InviteeUid) of
                {ok, Invitation} ->
                    elib_response:success(Req0, Invitation);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 接受邀请
-spec accept_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
accept_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    InvitationId = decode_positive_id(maps:get(<<"invitation_id">>, PostVals, <<>>)),

    case InvitationId of
        0 ->
            elib_response:error(Req0, <<"邀请ID不能为空"/utf8>>);
        _ ->
            case channel_logic:accept_invitation(Uid, InvitationId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 拒绝邀请
-spec reject_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
reject_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    InvitationId = decode_positive_id(maps:get(<<"invitation_id">>, PostVals, <<>>)),

    case InvitationId of
        0 ->
            elib_response:error(Req0, <<"邀请ID不能为空"/utf8>>);
        _ ->
            case channel_logic:reject_invitation(Uid, InvitationId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取我的邀请列表
-spec my_invitations(cowboy_req:req(), map()) -> cowboy_req:req().
my_invitations(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_my_invitations(Uid) of
        {ok, Invitations} ->
            elib_response:success(Req0, #{list => Invitations});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 获取我发出的邀请列表
-spec sent_invitations(cowboy_req:req(), map()) -> cowboy_req:req().
sent_invitations(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_sent_invitations(Uid) of
        {ok, Invitations} ->
            elib_response:success(Req0, #{list => Invitations});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% ===================================================================

%% 管理员列表与角色更新 API
%% ===================================================================

%% @doc 获取频道管理员列表（仅频道订阅者可查看）
-spec admins(cowboy_req:req(), map()) -> cowboy_req:req().
admins(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            ChannelIdInt = elib_cnv:safe_to_integer(ChannelId),
            % BUG#135（生产实证 8/14+8/16 qa-batch84-admin 管理员列表恒空）：
            % 创建频道只写 channel_admin（role=3），不写 channel_subscription，
            % 创建者/管理员在订阅表无行 → is_subscribed 恒 false → 403 业务码
            % （HTTP 仍 200）→ 客户端把无 list 的响应静默当空列表。
            % 权限放宽为：频道管理员（含创建者，role>0）或订阅者。
            Role = channel_logic_common:get_user_role(ChannelIdInt, Uid),
            case Role > 0 orelse channel_logic_subscription:is_subscribed(ChannelIdInt, Uid) of
                false ->
                    elib_response:error(Req0, <<"无权限查看该频道管理员"/utf8>>, 403);
                true ->
                    case channel_logic:get_admins(ChannelId) of
                        {ok, Admins} ->
                            elib_response:success(Req0, #{list => Admins});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% @doc 更新管理员角色
-spec update_admin_role(cowboy_req:req(), map()) -> cowboy_req:req().
update_admin_role(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Role = elib_cnv:safe_to_integer(maps:get(<<"role">>, PostVals, 1)),
    case Role < 1 orelse Role > 3 of
        true ->
            elib_response:error(Req0, <<"角色值必须在1-3之间"/utf8>>);
        false ->
            ChannelId = resolve_channel_id(Req0, PostVals),
            UserIdBin = resolve_user_id_bin(Req0, PostVals),
            case ChannelId of
                <<>> ->
                    elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
                _ ->
                    case UserIdBin of
                        <<>> ->
                            elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                        _ ->
                            TargetUid = decode_positive_id(UserIdBin),
                            case TargetUid of
                                0 ->
                                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                                _ ->
                                    case
                                        channel_logic:update_admin_role(
                                            Uid, ChannelId, TargetUid, Role
                                        )
                                    of
                                        ok ->
                                            elib_response:success(Req0, #{});
                                        {error, Msg} ->
                                            elib_response:error(Req0, Msg)
                                    end
                            end
                    end
            end
    end.

%% @doc 统一解析频道 ID：路径参数优先，body 兼容回退
-spec resolve_channel_id(cowboy_req:req(), map()) -> binary().
resolve_channel_id(Req0, PostVals) ->
    case binding_or_empty(channel_id, Req0) of
        <<>> -> maps:get(<<"channel_id">>, PostVals, <<>>);
        ChannelId -> ChannelId
    end.

%% @doc 统一解析用户 ID：路径参数优先，body 兼容回退
-spec resolve_user_id_bin(cowboy_req:req(), map()) -> binary().
resolve_user_id_bin(Req0, PostVals) ->
    case binding_or_empty(user_id, Req0) of
        <<>> -> maps:get(<<"user_id">>, PostVals, <<>>);
        UserIdBin -> UserIdBin
    end.

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.

%% ===================================================================
%% 移除订阅者 API
%% ===================================================================

%% @doc 移除频道订阅者（管理员及以上权限）
-spec remove_subscriber(cowboy_req:req(), map()) -> cowboy_req:req().
remove_subscriber(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case cowboy_req:binding(user_id, Req0) of
                undefined ->
                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                UserIdBin ->
                    TargetUid = decode_positive_id(UserIdBin),
                    case TargetUid of
                        0 ->
                            elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                        _ ->
                            case channel_logic:remove_subscriber(Uid, ChannelId, TargetUid) of
                                ok ->
                                    elib_response:success(Req0, #{});
                                {error, Msg} ->
                                    elib_response:error(Req0, Msg)
                            end
                    end
            end
    end.

%% ===================================================================
%% 频道增量同步 API
%% ===================================================================

%% @doc 频道增量同步
-spec sync(cowboy_req:req(), map()) -> cowboy_req:req().
sync(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Since = parse_qs_int(proplists:get_value(<<"since">>, Qs), 0, 0, 16#7fffffff),
    case channel_logic:sync_channels(Uid, Since) of
        {ok, Data} ->
            elib_response:success(Req0, Data);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 移除频道管理员（DELETE）或更新角色（PUT）
-spec remove_admin(cowboy_req:req(), map()) -> cowboy_req:req().
remove_admin(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"PUT">> ->
            update_admin_role(Req0, State);
        <<"DELETE">> ->
            Uid = maps:get(current_uid, State),
            PostVals = elib_param:post(Req0),
            ChannelId = resolve_channel_id(Req0, PostVals),
            AdminUidBin = resolve_user_id_bin(Req0, PostVals),
            AdminUid = decode_positive_id(AdminUidBin),
            case ChannelId of
                <<>> ->
                    elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
                _ when AdminUid =:= 0 ->
                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                _ ->
                    case channel_logic:remove_admin(Uid, ChannelId, AdminUid) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end;
        _ ->
            elib_response:error(Req0, <<"请求方法不支持"/utf8>>)
    end.

%% @doc 获取邀请列表（通用）
-spec invitations(cowboy_req:req(), map()) -> cowboy_req:req().
invitations(Req0, State) ->
    my_invitations(Req0, State).

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

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) when is_integer(Value), Value > 0 ->
    Value;
decode_positive_id(Value) ->
    case catch elib_cnv:safe_to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec normalize_non_empty_binary(binary()) -> binary().
normalize_non_empty_binary(Value) ->
    list_to_binary(string:trim(binary_to_list(Value))).

-spec normalize_error_binary(term(), binary()) -> binary().
normalize_error_binary(Msg, Default) ->
    case Msg of
        Value when is_binary(Value); is_list(Value); is_integer(Value) ->
            case normalize_non_empty_binary(Value) of
                <<>> ->
                    Default;
                Bin ->
                    Bin
            end;
        _ ->
            %% atom / epgsql 错误元组等：不 dump term 给用户，记日志后用中文兜底
            ?ERROR_LOG([<<"channel_handler_admin op failed">>, Msg]),
            Default
    end.
