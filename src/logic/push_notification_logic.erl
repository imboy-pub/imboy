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
-export([notify_offline_user/4]).
-export([notify_offline_users/3]).
-export([maybe_push_for_c2c/4]).
-export([maybe_push_for_c2g/4]).
-export([maybe_push_for_enterprise_c2c/2]).
-export([maybe_push_for_enterprise_c2g/3]).
-export([notify_org_invitation/1]).

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
    notify_offline_user(Uid, Title, Body, #{}).

%% @doc 离线推送 + 固定常量路由数据（如 notify_type；客户端点击路由用）。
%% Data 禁止动态内容（消息正文/发送者身份等），与常量文案隐私红线同口径。
-spec notify_offline_user(integer(), binary(), binary(), #{binary() => binary()}) -> ok.
%% 不加 is_map(Data) 守卫：对已声明 #{binary() => binary()} 的参数做冗余
%% 守卫细化触发 Gradualizer pick_value(none()) 崩溃（配方 b'，2026-10-04）。
notify_offline_user(Uid, Title, Body, Data) ->
    case imboy_syn:count_user(Uid) of
        0 ->
            %% 用户完全离线且未处于免打扰时段，发送推送
            case dnd_muted(Uid) of
                true -> ok;
                false -> push_notification_ds:send_to_user_with_data(Uid, Title, Body, Data)
            end;
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
    AudibleUids = [Uid || Uid <- OfflineUids, not dnd_muted(Uid)],
    case AudibleUids of
        [] -> ok;
        _ -> push_notification_ds:send_to_users(AudibleUids, Title, Body)
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

%% 组织邀请触达的域常量入口（无内容入参）：org_invite 文案与点击路由常量
%% 收口在定义方——契约测试（push_token_contract_pg_tests
%% notify_offline_apis_unreachable_from_src）机械化保证 `notify_offline_user`
%% 字面量零生产直调方，调用方（organization_invitation_notify）拿不到内容
%% 入参通道，未来任何动态正文都无法在扫描下静默溜入。
-define(ORG_INVITE_TITLE, <<"组织邀请"/utf8>>).
-define(ORG_INVITE_BODY,
    <<"你收到一条新的组织邀请，请打开 App 在「组织 · 我的邀请」中处理"/utf8>>
).
%% 点击路由常量：客户端据 notify_type=org_invite 直达「我的邀请」页。
-define(ORG_INVITE_DATA, #{<<"notify_type">> => <<"org_invite">>}).

%% @doc 组织邀请创建成功后的离线触达入口（异步 fire-and-forget，在线不推）。
-spec notify_org_invitation(integer()) -> ok.
%% 保留 TargetUid > 0 实质细化；仅删冗余 is_integer（对已声明 integer()
%% 的参数重复断言会触发 pick_value(none()) 崩溃，配方 b'，2026-10-04）。
notify_org_invitation(TargetUid) when TargetUid > 0 ->
    elib_async:async(fun() ->
        notify_offline_user(TargetUid, ?ORG_INVITE_TITLE, ?ORG_INVITE_BODY, ?ORG_INVITE_DATA)
    end),
    ok;
notify_org_invitation(_) ->
    ok.

%% @doc C2C 消息离线推送入口
%% 在消息发送后异步调用，检查接收方是否离线
-spec maybe_push_for_c2c(integer(), integer(), binary(), binary()) -> ok.
maybe_push_for_c2c(_FromUid, ToUid, _MsgType, _Payload) ->
    elib_async:async(fun() ->
        case imboy_syn:count_user(ToUid) of
            0 ->
                case dnd_muted(ToUid) of
                    true -> ok;
                    false -> push_notification_ds:send_to_user(ToUid, ?PUSH_TITLE, ?PUSH_BODY)
                end;
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
        %% 免打扰成员剔除（G1）：离线但处于 DND 时段的成员不推
        AudibleUids = [Uid || Uid <- OfflineUids, not dnd_muted(Uid)],
        case AudibleUids of
            [] ->
                ok;
            _ ->
                push_notification_ds:send_to_users(AudibleUids, ?PUSH_TITLE, ?PUSH_BODY)
        end
    end),
    ok.

%% ===================================================================
%% 免打扰（DND）判定（G1 / Q09-AC22#7）
%% ===================================================================
%%
%% 接线说明：user_dnd_rule_ds:is_dnd_at/2 内部走 find_by_uid（depcache
%% TTL 1h + user_dnd_rule 表），规则由 status=1 启用、支持跨午夜时段
%% （23:00-08:00 类）。判定位于**离线判定之后**——在线用户短路返回，
%% 不触发 DND 查询；命中即整条静默（含 org_invite 触达：免打扰期间
%% 全部离线推送静默，口径统一，不做按通知类型的豁免白名单）。
%%
%% 时区口径：App 侧免打扰（群免打扰 KV 源合并）按用户设备本地时区判定；
%% 服务端此处按**服务器本地时区**（calendar:local_time()）换算当日分钟数。
%% 自托管单节点部署下两者通常一致；跨时区多租户的精确对齐属后续增强。

%% @doc 免打扰判定：当前时刻落在用户 DND 时段则返回 true（该用户静默）
-spec dnd_muted(integer()) -> boolean().
dnd_muted(Uid) ->
    {_Date, {H, M, _S}} = calendar:local_time(),
    user_dnd_rule_ds:is_dnd_at(Uid, H * 60 + M).

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
