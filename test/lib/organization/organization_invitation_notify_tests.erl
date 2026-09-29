-module(organization_invitation_notify_tests).

%% 邀请触达（P1）载荷合同：固定常量文案 + 固定路由数据 notify_type=org_invite。
%% 推送通道行为由 push_provider/push_notification_ds 各自套件覆盖；
%% 本套件只锁调用形状（fire-and-forget 语义 + 参数合同）。
%%
%% 2026-09-29 契约收口对齐：文案与路由常量移入
%% push_notification_logic:notify_org_invitation/1（域常量入口，零直调方
%% 契约见 push_token_contract_pg_tests notify_offline_apis_unreachable_from_src）。
%% 本套件相应改为：notify_created 调用形状锁（mock 域常量入口）+
%% notify_org_invitation 的 wire 载荷合同锁（mock push_notification_ds）。

-include_lib("eunit/include/eunit.hrl").

notify_created_threads_org_invite_test() ->
    ok = meck:new(push_notification_logic, [no_link]),
    Me = self(),
    meck:expect(push_notification_logic, notify_org_invitation, 1, fun(Uid) ->
        Me ! {notify_org_invitation_called, Uid},
        ok
    end),
    try
        ok = organization_invitation_notify:notify_created(402),
        receive
            {notify_org_invitation_called, 402} -> ok
        after 2000 ->
            erlang:error(notify_not_called)
        end
    after
        meck:unload(push_notification_logic)
    end.

notify_created_invalid_uid_noop_test() ->
    ok = meck:new(push_notification_logic, [no_link]),
    meck:expect(push_notification_logic, notify_org_invitation, 1, fun(_U) ->
        erlang:error(unexpected_call)
    end),
    try
        ?assertEqual(ok, organization_invitation_notify:notify_created(0)),
        ?assertEqual(ok, organization_invitation_notify:notify_created(<<"x">>))
    after
        meck:unload(push_notification_logic)
    end.

%% 域常量入口的 wire 载荷合同：离线目标收到的 title/body/data 恰为
%% 固定常量——多一分动态内容都算触碰隐私红线（与收口前
%% notify_created_threads_org_invite_test 的断言等价迁移）。
notify_org_invitation_payload_contract_test() ->
    ok = meck:new(push_notification_ds, [no_link]),
    Me = self(),
    meck:expect(push_notification_ds, send_to_user_with_data, 4, fun(Uid, T, B, D) ->
        Me ! {ds_called, Uid, T, B, D},
        ok
    end),
    try
        ok = push_notification_logic:notify_org_invitation(402),
        receive
            {ds_called, 402, Title, Body, Data} ->
                ?assert(is_binary(Title)),
                ?assert(is_binary(Body)),
                ?assertEqual(#{<<"notify_type">> => <<"org_invite">>}, Data)
        after 2000 ->
            erlang:error(ds_not_called)
        end
    after
        meck:unload(push_notification_ds)
    end.
