-module(push_notification_ds_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try Fun()
        after meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% Tests
%% ===================================================================

send_to_user_no_token_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, [], []} end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>))
    end).

send_to_user_with_token_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [{<<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>}],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, [], Rows} end),
        %% Mock elib_async 直接执行函数，忽略返回值
        meck:expect(elib_async, async, fun(_Fun) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>)),
        %% 验证 async 被调用了（推送被触发）
        ?assert(meck:called(elib_async, async, '_'))
    end).

send_to_users_empty_test() ->
    ?assertEqual(ok, push_notification_ds:send_to_users([], <<"title">>, <<"body">>)).

send_fcm_not_configured_test() ->
    %% 确保 push 配置不存在
    application:unset_env(imboy, push),
    ?assertEqual({error, not_configured}, push_notification_ds:send_fcm(<<"token">>, <<"title">>, <<"body">>)).

send_apns_not_configured_test() ->
    application:unset_env(imboy, push),
    ?assertEqual({error, not_configured}, push_notification_ds:send_apns(<<"token">>, <<"title">>, <<"body">>)).

extract_push_info_tuple4_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [{<<"did1">>, <<"ios">>, <<"apns">>, <<"apns_token">>}],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, [], Rows} end),
        meck:expect(elib_async, async, fun(_Fun) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>)),
        ?assert(meck:called(elib_async, async, '_'))
    end).

send_to_users_with_tokens_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [{1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>}],
        meck:expect(push_token_repo, list_by_uids, fun([1, 2]) -> {ok, [], Rows} end),
        meck:expect(elib_async, async, fun(_Fun) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_users([1, 2], <<"title">>, <<"body">>))
    end).
