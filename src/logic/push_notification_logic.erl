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

%% 隐私不变量（fail-closed）：推送 title/body 恒为固定常量。
%% 不查询发送者昵称、群名，不携带消息类型、正文、密文片段——
%% 推送通道（FCM/APNs）视为不可信第三方，任何动态内容都构成元数据泄露。
-define(PUSH_TITLE, <<"新消息"/utf8>>).
-define(PUSH_BODY, <<"发来一条消息"/utf8>>).

%% @doc C2C 消息离线推送入口
%% 在消息发送后异步调用，检查接收方是否离线
-spec maybe_push_for_c2c(integer(), integer(), binary(), binary()) -> ok.
maybe_push_for_c2c(_FromUid, ToUid, _MsgType, _Payload) ->
    elib_async:async(fun() ->
        case imboy_syn:count_user(ToUid) of
            0 ->
                push_notification_ds:send_to_user(ToUid, ?PUSH_TITLE, ?PUSH_BODY);
            _ ->
                ok
        end
    end),
    ok.

%% @doc C2G 消息离线推送入口
%% 向群组中的离线成员发送推送
-spec maybe_push_for_c2g(integer(), integer(), binary(), [integer()]) -> ok.
maybe_push_for_c2g(FromUid, _GroupId, _MsgType, MemberUids) ->
    elib_async:async(fun() ->
        %% 排除发送者自己
        OtherUids = [Uid || Uid <- MemberUids, Uid =/= FromUid],
        OfflineUids = [Uid || Uid <- OtherUids, imboy_syn:count_user(Uid) =:= 0],
        case OfflineUids of
            [] ->
                ok;
            _ ->
                push_notification_ds:send_to_users(OfflineUids, ?PUSH_TITLE, ?PUSH_BODY)
        end
    end),
    ok.
