-module(push_notification_logic).
-dialyzer({nowarn_function, [unregister_token/2]}).
%%%
% push_notification_logic 推送通知业务逻辑层
% 负责判断用户在线状态并触发离线推送
%%%

-include("log.hrl").

-export([register_token/5]).
-export([unregister_token/2]).
-export([notify_offline_user/3]).
-export([notify_offline_users/3]).
-export([maybe_push_for_c2c/4]).
-export([maybe_push_for_c2g/4]).

%% ===================================================================
%% Token 管理 API
%% ===================================================================

%% @doc 注册推送 token（客户端登录/启动时调用）
-spec register_token(integer(), binary(), binary(), binary(), binary()) ->
    ok | {error, term()}.
register_token(Uid, DeviceId, DeviceType, Platform, Token) ->
    case push_token_ds:upsert(Uid, DeviceId, DeviceType, Platform, Token) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG(["push token register failed", Uid, DeviceId, Reason]),
            {error, Reason}
    end.

%% @doc 注销推送 token（客户端登出时调用）
-spec unregister_token(integer(), binary()) -> ok.
unregister_token(Uid, DeviceId) ->
    case push_token_ds:deactivate(Uid, DeviceId) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG(["push_token_deactivate_failed", Uid, DeviceId, Reason]),
            ok
    end.

%% ===================================================================
%% 离线推送 API
%% ===================================================================

%% @doc 检查用户是否离线，如果离线则发送推送通知
-spec notify_offline_user(integer(), binary(), binary()) -> ok.
notify_offline_user(Uid, Title, Body) ->
    case imboy_syn:count_user(Uid) of
        0 ->
            %% 用户完全离线，发送推送
            push_notification_ds:send_to_user(Uid, Title, Body);
        _ ->
            %% 用户有在线设备，不推送
            ok
    end.

%% @doc 批量检查用户离线状态并推送
-spec notify_offline_users([integer()], binary(), binary()) -> ok.
notify_offline_users([], _Title, _Body) ->
    ok;
notify_offline_users(Uids, Title, Body) ->
    OfflineUids = [Uid || Uid <- Uids, imboy_syn:count_user(Uid) =:= 0],
    case OfflineUids of
        [] -> ok;
        _ -> push_notification_ds:send_to_users(OfflineUids, Title, Body)
    end.

%% @doc C2C 消息离线推送入口
%% 在消息发送后异步调用，检查接收方是否离线
-spec maybe_push_for_c2c(integer(), integer(), binary(), binary()) -> ok.
maybe_push_for_c2c(FromUid, ToUid, MsgType, _Payload) ->
    elib_async:async(fun() ->
        case imboy_syn:count_user(ToUid) of
            0 ->
                %% 构建推送内容
                Title = get_push_title(FromUid),
                Body = get_push_body(MsgType),
                push_notification_ds:send_to_user(ToUid, Title, Body);
            _ ->
                ok
        end
    end),
    ok.

%% @doc C2G 消息离线推送入口
%% 向群组中的离线成员发送推送
-spec maybe_push_for_c2g(integer(), integer(), binary(), [integer()]) -> ok.
maybe_push_for_c2g(FromUid, GroupId, MsgType, MemberUids) ->
    elib_async:async(fun() ->
        %% 排除发送者自己
        OtherUids = [Uid || Uid <- MemberUids, Uid =/= FromUid],
        OfflineUids = [Uid || Uid <- OtherUids, imboy_syn:count_user(Uid) =:= 0],
        case OfflineUids of
            [] ->
                ok;
            _ ->
                Title = get_group_push_title(FromUid, GroupId),
                Body = get_push_body(MsgType),
                push_notification_ds:send_to_users(OfflineUids, Title, Body)
        end
    end),
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 获取推送标题（发送者昵称）
get_push_title(FromUid) ->
    case user_ds:find_by_id(FromUid, <<"nickname">>) of
        #{<<"nickname">> := Nickname} when is_binary(Nickname), byte_size(Nickname) > 0 ->
            Nickname;
        _ ->
            <<"新消息"/utf8>>
    end.

%% @doc 获取群组推送标题
get_group_push_title(FromUid, GroupId) ->
    SenderName = get_push_title(FromUid),
    GroupName =
        case group_ds:find_by_id(GroupId, <<"title">>) of
            #{<<"title">> := Name} when is_binary(Name), byte_size(Name) > 0 -> Name;
            _ -> <<"群聊"/utf8>>
        end,
    <<SenderName/binary, " [", GroupName/binary, "]">>.

%% @doc 根据消息类型生成推送正文
get_push_body(<<"text">>) -> <<"发来一条消息"/utf8>>;
get_push_body(<<"image">>) -> <<"[图片]"/utf8>>;
get_push_body(<<"voice">>) -> <<"[语音]"/utf8>>;
get_push_body(<<"video">>) -> <<"[视频]"/utf8>>;
get_push_body(<<"file">>) -> <<"[文件]"/utf8>>;
get_push_body(<<"location">>) -> <<"[位置]"/utf8>>;
get_push_body(<<"e2ee">>) -> <<"发来一条加密消息"/utf8>>;
get_push_body(_) -> <<"发来一条消息"/utf8>>.
