-module(imboy_sms_provider_tests).
-include_lib("eunit/include/eunit.hrl").

%%% SMS provider 合同 + fake 实现（GZAPP-06）
%%% 覆盖：平台选择（config_ds [sms, platform]）、fake 成功/注入失败/outbox、
%%% not_configured fail-closed（真实 provider 未实现，绝不外发）。

sms_platform_test_() ->
    {foreach,
        fun() ->
            application:set_env(imboy, sms, [{platform, <<"fake">>}]),
            ok = imboy_sms_fake:outbox_clear()
        end,
        fun(_) ->
            application:unset_env(imboy, sms)
        end,
        [
            {"platform=fake → 选 fake", fun() ->
                ?assertEqual(fake, imboy_sms_provider:provider())
            end},
            {"platform=yjsms → not_configured（真实 provider 未实现）", fun() ->
                application:set_env(imboy, sms, [{platform, <<"yjsms">>}]),
                ?assertEqual(not_configured, imboy_sms_provider:provider())
            end},
            {"无配置 → not_configured", fun() ->
                application:unset_env(imboy, sms),
                ?assertEqual(not_configured, imboy_sms_provider:provider())
            end},
            {"fake 发送成功：ok + outbox 记录待发", fun() ->
                ?assertEqual(
                    ok,
                    imboy_sms_provider:send_activation(
                        <<"13800138000">>, <<"tok-abc">>, <<"测试企业"/utf8>>
                    )
                ),
                Snapshot = imboy_sms_fake:outbox_snapshot(),
                ?assertMatch(
                    [{<<"13800138000">>, <<"tok-abc">>, <<"测试企业"/utf8>>, _Ts}],
                    Snapshot
                )
            end},
            {"fake 注入失败：000 前缀 → simulated_failure（D12 驱动缝）", fun() ->
                ?assertEqual(
                    {error, simulated_failure},
                    imboy_sms_provider:send_activation(
                        <<"00000001234">>, <<"tok-x">>, <<"org">>
                    )
                ),
                ?assertEqual([], imboy_sms_fake:outbox_snapshot())
            end},
            {"not_configured：fail-closed 不外发不记录", fun() ->
                application:set_env(imboy, sms, [{platform, <<"yjsms">>}]),
                ?assertEqual(
                    {error, not_configured},
                    imboy_sms_provider:send_activation(<<"13800138000">>, <<"t">>, <<"o">>)
                ),
                ?assertEqual([], imboy_sms_fake:outbox_snapshot())
            end},
            {"参数形状非法 → invalid_args", fun() ->
                ?assertEqual({error, invalid_args}, imboy_sms_provider:send_activation(1, 2, 3))
            end}
        ]}.
