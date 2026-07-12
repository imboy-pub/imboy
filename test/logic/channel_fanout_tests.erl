%% 频道大订阅量 fanout 写扩散抑制测试
%% 验证 channel_fanout_save_threshold 对 broadcast_channel_message 的 save/no_save 门控，
%% 以及状态通知（如撤回）不受阈值影响恒 save。
-module(channel_fanout_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ① 默认阈值 0（未配置）→ save
default_threshold_zero_keeps_save_test_() ->
    ?_test(begin
        SaveMode = run_broadcast(undefined, 11),
        ?assertEqual(save, SaveMode)
    end).

%% ② 阈值 10、订阅者 11 人 → no_save
over_threshold_uses_no_save_test_() ->
    ?_test(begin
        SaveMode = run_broadcast(10, 11),
        ?assertEqual(no_save, SaveMode)
    end).

%% ③ 阈值 10、订阅者 10 人 → save（边界=不超过则 save）
at_threshold_boundary_keeps_save_test_() ->
    ?_test(begin
        SaveMode = run_broadcast(10, 10),
        ?assertEqual(save, SaveMode)
    end).

%% ④ 撤回通知不受阈值影响恒 save
revoke_notification_ignores_threshold_test_() ->
    ?_test(begin
        SaveMode = run_captured(10, 11, <<"channel_message_revoked">>, fun() ->
            channel_logic_notify:notify_message_revoked(11, 99, 1001, <<"2026-07-12T10:00:00Z">>)
        end),
        ?assertEqual(save, SaveMode)
    end).

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% 生成 N 个合法订阅者 uid
uids(N) ->
    lists:seq(1001, 1000 + N).

run_broadcast(EnvValue, SubscriberCount) ->
    Message = #{<<"id">> => <<"msg_1">>, <<"content">> => <<"hello">>},
    run_captured(EnvValue, SubscriberCount, <<"channel_message">>, fun() ->
        channel_logic_notify:broadcast_channel_message(11, Message)
    end).

%% 设置 env → meck 捕获 msg_s2c_ds:send/7 第 7 参（save|no_save）→ 还原 env
run_captured(EnvValue, SubscriberCount, Action, RunFun) ->
    Saved = application:get_env(imboy, channel_fanout_save_threshold),
    set_threshold(EnvValue),
    Self = self(),
    Ref = make_ref(),
    {ok, _} = meck_helper:setup_mock(channel_ds, [
        {'subscriber_uids', 1, fun(11) -> uids(SubscriberCount) end}
    ]),
    {ok, _} = meck_helper:setup_mock(msg_s2c_ds, [
        {'send', 7, fun(0, _Uids, GotAction, <<>>, null, _Payload, SaveMode) ->
            ?assertEqual(Action, GotAction),
            Self ! {Ref, SaveMode},
            ok
        end}
    ]),
    try
        ?assertEqual(ok, RunFun()),
        receive
            {Ref, Mode} -> Mode
        after 1000 -> timeout
        end
    after
        meck_helper:cleanup_mock(msg_s2c_ds),
        meck_helper:cleanup_mock(channel_ds),
        restore_threshold(Saved)
    end.

set_threshold(undefined) ->
    application:unset_env(imboy, channel_fanout_save_threshold);
set_threshold(Value) ->
    application:set_env(imboy, channel_fanout_save_threshold, Value).

restore_threshold({ok, V}) ->
    application:set_env(imboy, channel_fanout_save_threshold, V);
restore_threshold(undefined) ->
    application:unset_env(imboy, channel_fanout_save_threshold).
