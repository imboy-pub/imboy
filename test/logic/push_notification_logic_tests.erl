-module(push_notification_logic_tests).

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
%% Token 管理 Tests
%% ===================================================================

register_token_ok_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, upsert, fun(1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>) ->
            {ok, 1}
        end),
        ?assertEqual(ok, push_notification_logic:register_token(1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>))
    end).

register_token_error_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, upsert, fun(_, _, _, _, _) -> {error, db_error} end),
        ?assertEqual({error, db_error}, push_notification_logic:register_token(1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>))
    end).

unregister_token_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, deactivate, fun(1, <<"did1">>) -> {ok, 1} end),
        ?assertEqual(ok, push_notification_logic:unregister_token(1, <<"did1">>))
    end).

%% ===================================================================
%% 离线推送 Tests
%% ===================================================================

notify_offline_user_online_test() ->
    ?WITH_MECKS([imboy_syn], fun() ->
        meck:expect(imboy_syn, count_user, fun(1) -> 2 end),
        ?assertEqual(ok, push_notification_logic:notify_offline_user(1, <<"title">>, <<"body">>))
    end).

notify_offline_user_offline_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun(1) -> 0 end),
        meck:expect(push_notification_ds, send_to_user, fun(1, <<"title">>, <<"body">>) -> ok end),
        ?assertEqual(ok, push_notification_logic:notify_offline_user(1, <<"title">>, <<"body">>)),
        ?assert(meck:called(push_notification_ds, send_to_user, [1, <<"title">>, <<"body">>]))
    end).

notify_offline_users_all_online_test() ->
    ?WITH_MECKS([imboy_syn], fun() ->
        meck:expect(imboy_syn, count_user, fun(_) -> 1 end),
        ?assertEqual(ok, push_notification_logic:notify_offline_users([1, 2, 3], <<"title">>, <<"body">>))
    end).

notify_offline_users_some_offline_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun
            (1) -> 0;
            (2) -> 1;
            (3) -> 0
        end),
        meck:expect(push_notification_ds, send_to_users, fun([1, 3], <<"title">>, <<"body">>) -> ok end),
        ?assertEqual(ok, push_notification_logic:notify_offline_users([1, 2, 3], <<"title">>, <<"body">>))
    end).

maybe_push_for_c2c_online_test() ->
    ?WITH_MECKS([imboy_syn, elib_async], fun() ->
        meck:expect(elib_async, async, fun(Fun) -> Fun(), self() end),
        meck:expect(imboy_syn, count_user, fun(2) -> 1 end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hello">>))
    end).

maybe_push_for_c2c_offline_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) -> Fun(), self() end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(2, <<"Alice">>, <<"发来一条消息"/utf8>>) -> ok end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hello">>)),
        ?assert(meck:called(push_notification_ds, send_to_user, '_'))
    end).

maybe_push_for_c2g_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, group_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) -> Fun(), self() end),
        meck:expect(imboy_syn, count_user, fun
            (1) -> 1;  % sender online
            (2) -> 0;  % offline
            (3) -> 1   % online
        end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(group_repo, find_by_id, fun(100, <<"title">>) ->
            #{<<"title">> => <<"测试群"/utf8>>}
        end),
        meck:expect(push_notification_ds, send_to_users, fun([2], _, _) -> ok end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [1, 2, 3]))
    end).

notify_offline_users_empty_test() ->
    ?assertEqual(ok, push_notification_logic:notify_offline_users([], <<"title">>, <<"body">>)).

push_body_types_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) -> Fun(), self() end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Bob">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(2, <<"Bob">>, Body) ->
            %% 验证不同消息类型的推送正文
            ?assert(is_binary(Body)),
            ok
        end),
        %% 测试图片消息
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"image">>, <<>>)),
        %% 测试语音消息
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"voice">>, <<>>)),
        %% 测试 e2ee 消息
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"e2ee">>, <<>>))
    end).
