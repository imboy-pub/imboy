-module(push_notification_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% Token 管理 Tests
%% ===================================================================

register_token_ok_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, upsert, fun(
            1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>
        ) ->
            {ok, 1}
        end),
        ?assertEqual(
            ok,
            push_notification_logic:register_token(
                1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>
            )
        )
    end).

register_token_error_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, upsert, fun(_, _, _, _, _) -> {error, db_error} end),
        ?assertEqual(
            {error, db_error},
            push_notification_logic:register_token(
                1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>
            )
        )
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
        ?assertEqual(
            ok, push_notification_logic:notify_offline_users([1, 2, 3], <<"title">>, <<"body">>)
        )
    end).

notify_offline_users_some_offline_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun
            (1) -> 0;
            (2) -> 1;
            (3) -> 0
        end),
        meck:expect(push_notification_ds, send_to_users, fun([1, 3], <<"title">>, <<"body">>) ->
            ok
        end),
        ?assertEqual(
            ok, push_notification_logic:notify_offline_users([1, 2, 3], <<"title">>, <<"body">>)
        )
    end).

maybe_push_for_c2c_online_test() ->
    ?WITH_MECKS([imboy_syn, elib_async], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 1 end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hello">>))
    end).

maybe_push_for_c2c_offline_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(
            2, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
        ) ->
            ok
        end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hello">>)),
        ?assert(meck:called(push_notification_ds, send_to_user, '_')),
        ?assertEqual(0, meck:num_calls(user_repo, find_by_id, 2))
    end).

maybe_push_for_c2g_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, group_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun
            % sender online
            (1) -> 1;
            % offline
            (2) -> 0;
            % online
            (3) -> 1
        end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(group_repo, find_by_id, fun(100, <<"title">>) ->
            #{<<"title">> => <<"测试群"/utf8>>}
        end),
        meck:expect(push_notification_ds, send_to_users, fun(
            [2], <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
        ) ->
            ok
        end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [1, 2, 3])),
        ?assertEqual(0, meck:num_calls(user_repo, find_by_id, 2)),
        ?assertEqual(0, meck:num_calls(group_repo, find_by_id, 2))
    end).

notify_offline_users_empty_test() ->
    ?assertEqual(ok, push_notification_logic:notify_offline_users([], <<"title">>, <<"body">>)).

push_payload_is_constant_across_message_types_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Bob">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(
            2, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
        ) ->
            ok
        end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"image">>, <<>>)),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"voice">>, <<>>)),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"e2ee">>, <<>>)),
        ?assertEqual(3, meck:num_calls(push_notification_ds, send_to_user, 3)),
        ?assertEqual(0, meck:num_calls(user_repo, find_by_id, 2))
    end).

%% ===================================================================
%% FULL-06 · 离线判定负例（plan-full §3.3 多设备 / 登出行为）
%% 判定口径：`imboy_syn:count_user(Uid) =:= 0` 才推送；>0 = 至少一台在线。
%% ===================================================================

%% 单设备在线（count_user=1）即视为在线 → 不推送（边界值，不是 >=2 才算在线）
maybe_push_for_c2c_single_device_online_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 1 end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hi">>)),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_user, '_'))
    end).

%% 多设备在线（count_user=3，同用户三台设备）→ 仍视为在线，不推送
maybe_push_for_c2c_multi_device_online_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 3 end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hi">>)),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_user, '_'))
    end).

%% 单设备离线（count_user=0）→ 推送；且 title/body 是静态常量
notify_offline_user_single_device_offline_uses_constant_payload_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun(1) -> 0 end),
        meck:expect(push_notification_ds, send_to_user, fun(
            1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
        ) ->
            ok
        end),
        ?assertEqual(
            ok,
            push_notification_logic:notify_offline_user(
                1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
            )
        ),
        ?assert(
            meck:called(
                push_notification_ds,
                send_to_user,
                [1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>]
            )
        )
    end).

%% c2g 负例：发送者即使离线，也绝不收到自己发出的群消息推送
maybe_push_for_c2g_never_pushes_sender_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(_) -> 0 end),
        meck:expect(push_notification_ds, send_to_users, fun([2], _T, _B) -> ok end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [1, 2])
        ),
        %% 只有非发送者一个 uid 收到批量推送（发送者 1 被剔除）
        ?assert(meck:called(push_notification_ds, send_to_users, [[2], '_', '_'])),
        ?assertEqual(1, meck:num_calls(push_notification_ds, send_to_users, '_'))
    end).

%% c2g 负例：全体成员（除发送者）都在线 → 零推送
maybe_push_for_c2g_all_members_online_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(_) -> 1 end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [1, 2, 3])
        ),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_users, '_'))
    end).

%% c2g 负例：只有发送者一个成员 → 过滤后为空，不发批量推送（不炸）
maybe_push_for_c2g_sender_only_member_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(_) -> 0 end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [1])
        ),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_users, '_'))
    end).

%% c2g 负例：成员列表为空 → 零推送
maybe_push_for_c2g_empty_members_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2g(1, 100, <<"text">>, [])
        ),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_users, '_'))
    end).

%% c2c 负例：接收方离线但没有任何 token → 走真实 DS 空集分支，不炸
maybe_push_for_c2c_offline_without_any_token_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_token_repo], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(push_token_repo, list_by_uid, fun(2) -> {ok, []} end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_c2c(1, 2, <<"text">>, <<"hi">>)),
        ?assert(meck:called(push_token_repo, list_by_uid, [2]))
    end).

%% c2c 负例：DS 层查 token 失败 → fail-safe 返回 ok（推送故障不影响消息投递）
push_notification_ds_list_failure_is_failsafe_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {error, db_down} end),
        ?assertEqual(
            ok,
            push_notification_ds:send_to_user(1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>)
        )
    end).

%% ===================================================================
%% E2EE 推送隐私守护（零知识不变量：密文永远不出现在 push body）
%% ===================================================================

%% 模拟真实 Olm 密文（base64 编码的 X3DH ciphertext）
-define(FAKE_CIPHERTEXT,
    <<"AwgAEkQjR0FCU0VGR0hJSktMTU5PUFFSU1RVVldYWVphYmNkZWZnaGlqa2xtbm9w">>
).

%% legacy v1: msg_type = <<"e2ee">>，push body 必须是静态字符串
e2ee_push_body_never_leaks_ciphertext_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(
            2, <<"新消息"/utf8>>, Body
        ) ->
            %% 核心隐私断言：body 是静态字符串，不含任何密文片段
            ?assertEqual(<<"发来一条消息"/utf8>>, Body),
            ?assertNot(binary:match(Body, ?FAKE_CIPHERTEXT) =/= nomatch),
            ?assertNot(binary:match(Body, <<"AwgAEk">>) =/= nomatch),
            ok
        end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2c(
                1, 2, <<"e2ee">>, ?FAKE_CIPHERTEXT
            )
        )
    end).

%% v2.0: msg_type = <<"text">>（e2ee 由顶层 e2ee map 标识），push body 仍为通用静态串
e2ee_v2_push_body_generic_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(2) -> 0 end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(push_notification_ds, send_to_user, fun(
            2, <<"新消息"/utf8>>, Body
        ) ->
            %% v2.0 e2ee 消息保留原 msg_type=text，push body 为通用串
            ?assertEqual(<<"发来一条消息"/utf8>>, Body),
            ?assertNot(binary:match(Body, ?FAKE_CIPHERTEXT) =/= nomatch),
            ok
        end),
        %% v2.0: msg_type 仍为 text，密文在 payload 中
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2c(
                1, 2, <<"text">>, ?FAKE_CIPHERTEXT
            )
        )
    end).

%% 群消息 e2ee：push body 同样不含密文
e2ee_c2g_push_body_never_leaks_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, user_repo, group_repo, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun
            (1) -> 1;
            (2) -> 0
        end),
        meck:expect(user_repo, find_by_id, fun(1, <<"nickname">>) ->
            #{<<"nickname">> => <<"Alice">>}
        end),
        meck:expect(group_repo, find_by_id, fun(100, <<"title">>) ->
            #{<<"title">> => <<"Secret Group">>}
        end),
        meck:expect(push_notification_ds, send_to_users, fun(
            [2], <<"新消息"/utf8>>, Body
        ) ->
            ?assertEqual(<<"发来一条消息"/utf8>>, Body),
            ?assertNot(binary:match(Body, ?FAKE_CIPHERTEXT) =/= nomatch),
            ok
        end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2g(
                1, 100, <<"e2ee">>, [1, 2]
            )
        )
    end).
