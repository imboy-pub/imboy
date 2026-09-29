-module(organization_invitation_notify).

%% 邀请触达（P1）：邀请创建成功后的离线推送。
%%
%% * 文案与点击路由常量收口在 push_notification_logic:notify_org_invitation/1
%%   （域常量入口，无内容入参通道）——契约测试
%%   notify_offline_apis_unreachable_from_src 机械化保证本模块不直调
%%   notify_offline_user，与 push_notification_logic 的消息推送同口径
%%   （推送通道 FCM/APNs/极光视为不可信第三方，任何动态内容都构成元数据泄露）。
%% * 只对完全离线的目标用户推送（imboy_syn 在线判定）；在线用户由 App 端
%%   「我的邀请」角标承担触达。
%% * fire-and-forget：异步隔离，发送失败/异常绝不影响邀请创建结果。
%% * 两个调用面共用本模块：用户面 organization_invitation_app:create 与
%%   管理面 organization_admin_logic:admin_invitation_create。

-export([notify_created/1]).

%% @doc 邀请创建成功后触达目标用户（异步，永不抛错）。
-spec notify_created(integer()) -> ok.
notify_created(TargetUid) when is_integer(TargetUid), TargetUid > 0 ->
    push_notification_logic:notify_org_invitation(TargetUid),
    ok;
notify_created(_) ->
    ok.
