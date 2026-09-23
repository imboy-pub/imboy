-module(organization_invitation_notify_tests).

%% 邀请触达（P1）载荷合同：固定常量文案 + 固定路由数据 notify_type=org_invite。
%% 推送通道行为由 push_provider/push_notification_ds 各自套件覆盖；
%% 本套件只锁 notify 模块的调用形状（fire-and-forget 语义 + 参数合同）。

-include_lib("eunit/include/eunit.hrl").

notify_created_threads_org_invite_test() ->
    ok = meck:new(push_notification_logic, [no_link]),
    Me = self(),
    meck:expect(push_notification_logic, notify_offline_user, 4, fun(Uid, T, B, D) ->
        Me ! {notify_called, Uid, T, B, D},
        ok
    end),
    try
        ok = organization_invitation_notify:notify_created(402),
        receive
            {notify_called, 402, Title, Body, Data} ->
                ?assert(is_binary(Title)),
                ?assert(is_binary(Body)),
                %% 路由数据恰为固定常量：多一分动态内容都算触碰隐私红线
                ?assertEqual(#{<<"notify_type">> => <<"org_invite">>}, Data)
        after 2000 ->
            erlang:error(notify_not_called)
        end
    after
        meck:unload(push_notification_logic)
    end.

notify_created_invalid_uid_noop_test() ->
    ok = meck:new(push_notification_logic, [no_link]),
    meck:expect(push_notification_logic, notify_offline_user, 4, fun(_U, _T, _B, _D) ->
        erlang:error(unexpected_call)
    end),
    try
        ?assertEqual(ok, organization_invitation_notify:notify_created(0)),
        ?assertEqual(ok, organization_invitation_notify:notify_created(<<"x">>))
    after
        meck:unload(push_notification_logic)
    end.
