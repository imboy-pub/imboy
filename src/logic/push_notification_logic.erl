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
-export([maybe_push_for_enterprise_c2c/2]).
-export([maybe_push_for_enterprise_c2g/3]).

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

%% 企业托管消息入口的 msg_type 占位常量（FULL-07）：下游 maybe_push_for_c2c/4
%% 与 maybe_push_for_c2g/4 都忽略第 3 参（文案恒为上面两个常量），命名自解释
%% 以免被误读成「企业消息真的是 text」。
-define(ENTERPRISE_ONLY_MSG_TYPE, <<"enterprise">>).

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

%% ===================================================================
%% 企业托管消息离线推送入口（FULL-07）
%% ===================================================================
%%
%% 增量根因：`src/logic/enterprise_message_logic.erl`（OA 代发企业托管消息，
%% INT-09/10）此前**没有任何 push 调用**——发给离线收件人的企业消息永不推送，
%% 企业内部平台的核心通知链是断的（C2C/C2G 有、企业托管消息没有）。
%%
%% 下面两个入口是**薄委托**，不是第二套实现：离线判定、常量 payload、
%% 多设备 fan-out、发送者剔除全部复用 `maybe_push_for_c2c/4` /
%% `maybe_push_for_c2g/4`（同一段代码），因此两边口径不可能漂移。
%%
%% 与 human 入口的**签名差异是刻意的硬边界**：
%%   * 企业入口**不接收** MsgType / Payload 参数 —— 调用方在编译期就没有把
%%     消息正文、密文片段、device_id、uid 送进推送通道的口；企业托管消息
%%     固定非 E2EE，推送文案只能是下面两个常量。
%%   * 因此 `maybe_push_for_enterprise_c2c/3` / `_c2g/4` **不存在**
%%     （`push_notification_logic_tests` 以 undef 负例钉死）。

%% @doc 企业托管 direct 消息离线推送（收件人离线才推）。
-spec maybe_push_for_enterprise_c2c(integer(), integer()) -> ok.
maybe_push_for_enterprise_c2c(FromUid, ToUid) ->
    %% 第 3/4 参在 maybe_push_for_c2c/4 内被忽略（常量文案），此处传常量占位，
    %% 保证「企业消息的 msg_type/payload 不参与推送」在调用点即成立。
    maybe_push_for_c2c(FromUid, ToUid, ?ENTERPRISE_ONLY_MSG_TYPE, <<>>).

%% @doc 企业托管 group 消息离线推送（群 active 成员中的离线者，发送者永不自收）。
-spec maybe_push_for_enterprise_c2g(integer(), integer(), [integer()]) -> ok.
maybe_push_for_enterprise_c2g(FromUid, GroupId, MemberUids) ->
    maybe_push_for_c2g(FromUid, GroupId, ?ENTERPRISE_ONLY_MSG_TYPE, MemberUids).
