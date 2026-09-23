-module(organization_invitation_notify).

%% 邀请触达（P1）：邀请创建成功后的离线推送。
%%
%% * 文案固定常量，不携带组织名 / 邀请人身份——与 push_notification_logic
%%   的消息推送同口径（推送通道 FCM/APNs/极光视为不可信第三方，任何动态
%%   内容都构成元数据泄露）。
%% * 只对完全离线的目标用户推送（imboy_syn 在线判定）；在线用户由 App 端
%%   「我的邀请」角标承担触达。
%% * fire-and-forget：spawn 隔离，发送失败/异常绝不影响邀请创建结果。
%% * 两个调用面共用本模块：用户面 organization_invitation_app:create 与
%%   管理面 organization_admin_logic:admin_invitation_create。

-export([notify_created/1]).

-include("log.hrl").

-define(NOTIFY_TITLE, <<"组织邀请"/utf8>>).
-define(NOTIFY_BODY, <<"你收到一条新的组织邀请，请打开 App 在「组织 · 我的邀请」中处理"/utf8>>).

%% @doc 邀请创建成功后触达目标用户（异步，永不抛错）。
-spec notify_created(integer()) -> ok.
notify_created(TargetUid) when is_integer(TargetUid), TargetUid > 0 ->
    spawn(fun() ->
        try
            push_notification_logic:notify_offline_user(TargetUid, ?NOTIFY_TITLE, ?NOTIFY_BODY)
        catch
            Class:Reason:Stack ->
                ?ERROR_LOG([
                    organization_invitation_notify_failed,
                    TargetUid,
                    Class,
                    Reason,
                    Stack
                ])
        end
    end),
    ok;
notify_created(_) ->
    ok.
