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
        meck:expect(push_notification_ds, send_to_user_with_data, 4, fun(Uid, T, B, D) ->
            self() ! {ds_called, Uid, T, B, D},
            ok
        end),
        ?assertEqual(ok, push_notification_logic:notify_offline_user(1, <<"title">>, <<"body">>)),
        receive
            %% /3 路径 Data 必须恰为空 map（不携带任何路由数据）
            {ds_called, 1, <<"title">>, <<"body">>, D} -> ?assertEqual(#{}, D)
        after 1000 ->
            erlang:error(ds_not_called_with_empty_data)
        end
    end).

%% /4 变体：自定义路由数据原样贯穿到 DS 层（邀请触达 notify_type 用）。
notify_offline_user_with_data_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun(1) -> 0 end),
        meck:expect(push_notification_ds, send_to_user_with_data, 4, fun(Uid, T, B, D) ->
            self() ! {ds_called, Uid, T, B, D},
            ok
        end),
        Data = #{<<"notify_type">> => <<"org_invite">>},
        ?assertEqual(
            ok, push_notification_logic:notify_offline_user(1, <<"title">>, <<"body">>, Data)
        ),
        receive
            {ds_called, 1, <<"title">>, <<"body">>, Data} -> ok
        after 1000 ->
            erlang:error(data_not_threaded)
        end
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
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_user, '_')),
        ?assertEqual(
            0, meck:num_calls(push_notification_ds, send_to_user_with_data, '_')
        )
    end).

%% 单设备离线（count_user=0）→ 推送；且 title/body 是静态常量、
%% 不携带任何路由数据（Data 恰为空 map，隐私红线回归锚）。
notify_offline_user_single_device_offline_uses_constant_payload_test() ->
    ?WITH_MECKS([imboy_syn, push_notification_ds], fun() ->
        meck:expect(imboy_syn, count_user, fun(1) -> 0 end),
        meck:expect(push_notification_ds, send_to_user_with_data, 4, fun(Uid, T, B, D) ->
            self() ! {ds_called, Uid, T, B, D},
            ok
        end),
        ?assertEqual(
            ok,
            push_notification_logic:notify_offline_user(
                1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
            )
        ),
        receive
            {ds_called, 1, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>, D} ->
                ?assertEqual(#{}, D)
        after 1000 ->
            erlang:error(ds_not_called)
        end
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
%% FULL-07 · 企业托管消息离线推送入口
%%
%% 增量根因：enterprise_message_logic（OA 代发 INT-09/10）此前没有任何 push
%% 调用——发给离线收件人的企业消息永不推送。下面这组用例在修复前**全部失败**
%% （undef / 零推送），修复后全绿；负例反向钉死「不建第二套实现」与
%% 「企业入口没有 payload 入参通道」两条硬边界。
%% ===================================================================

%% 硬边界：企业入口**不接收** msg_type/payload —— 多参重载必须不存在
%% （存在即意味着调用方能把消息正文/密文送进推送通道，编译期封闭被破坏）。
enterprise_push_entries_have_no_payload_channel_test() ->
    {module, push_notification_logic} = code:ensure_loaded(push_notification_logic),
    ?assertNot(erlang:function_exported(push_notification_logic, maybe_push_for_enterprise_c2c, 3)),
    ?assertNot(erlang:function_exported(push_notification_logic, maybe_push_for_enterprise_c2g, 4)),
    ?assert(erlang:function_exported(push_notification_logic, maybe_push_for_enterprise_c2c, 2)),
    ?assert(erlang:function_exported(push_notification_logic, maybe_push_for_enterprise_c2g, 3)).

%% 不建第二套：企业入口必须是**薄委托**——源码面直接钉死「同一次调用」，
%% 且企业段内不得出现任何自己的 fan-out/文案实现（push_notification_ds: 或
%% 常量文案）。这里必须走源码面：meck 无法拦截同模块内的**本地调用**，
%% 拿 meck 断言委托只会得到一个假绿。
%%
%% 行为等价由下面 enterprise_c2c_offline_pushes_constant_payload_test /
%% enterprise_c2g_sender_never_notified_test 证明（meck 的是被委托方的依赖，
%% 因此跑的是真正那份实现）。
enterprise_entries_are_thin_delegations_test() ->
    Src = read_src("src/logic/push_notification_logic.erl"),
    %% 企业段起点（导出/注释块）到文件末尾：委托调用必须成对出现
    {EntIdx, _} = binary:match(Src, [<<"maybe_push_for_enterprise_c2c(FromUid, ToUid) ->">>]),
    EntSection = binary:part(Src, EntIdx, byte_size(Src) - EntIdx),
    ?assertNotEqual(nomatch, binary:match(EntSection, [<<"maybe_push_for_c2c(">>])),
    ?assertNotEqual(nomatch, binary:match(EntSection, [<<"maybe_push_for_c2g(">>])),
    %% 企业段内零自有实现：不得直接触碰 DS 层或重复常量文案
    ?assertEqual(nomatch, binary:match(EntSection, [<<"push_notification_ds:">>])),
    ?assertEqual(nomatch, binary:match(EntSection, [<<"?PUSH_TITLE">>])),
    ?assertEqual(nomatch, binary:match(EntSection, [<<"?PUSH_BODY">>])),
    ?assertEqual(nomatch, binary:match(EntSection, [<<"imboy_syn:">>])),
    %% 项目内企业推送只允许从这两个出口触发（不得绕过 logic 直连 DS）
    {ok, Files} = file:list_dir("src/logic"),
    EnterpriseCallers = [
        F
     || F <- Files,
        lists:suffix(".erl", F),
        F =/= "push_notification_logic.erl",
        binary:match(read_src("src/logic/" ++ F), [<<"push_notification_ds:">>]) =/= nomatch
    ],
    ?assertEqual([], EnterpriseCallers).

%% direct：收件人离线 → 恰一次推送，且文案是常量（企业消息正文不入通道）。
enterprise_c2c_offline_pushes_constant_payload_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(22) -> 0 end),
        meck:expect(push_notification_ds, send_to_user, fun(
            22, <<"新消息"/utf8>>, <<"发来一条消息"/utf8>>
        ) ->
            ok
        end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_enterprise_c2c(11, 22)),
        ?assertEqual(1, meck:num_calls(push_notification_ds, send_to_user, 3))
    end).

%% direct 负例：收件人在线 → 零推送。
enterprise_c2c_online_zero_push_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(22) -> 1 end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_enterprise_c2c(11, 22)),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_user, 3))
    end).

%% group：只有离线且非发送者收到；application 模式下 principal 作为 sender
%% 同样被剔除（发送者永不自收）。
enterprise_c2g_sender_never_notified_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(_) -> 0 end),
        meck:expect(push_notification_ds, send_to_users, fun([44], _T, _B) -> ok end),
        ?assertEqual(ok, push_notification_logic:maybe_push_for_enterprise_c2g(55, 777, [55, 44])),
        %% 发送者 55 被剔除，离线成员 44 恰被推一次
        ?assert(meck:called(push_notification_ds, send_to_users, [[44], '_', '_'])),
        ?assertEqual(1, meck:num_calls(push_notification_ds, send_to_users, 3))
    end).

%% group 负例：全体（除发送者）在线 → 零推送。
enterprise_c2g_all_online_zero_push_test() ->
    ?WITH_MECKS([imboy_syn, elib_async, push_notification_ds], fun() ->
        meck:expect(elib_async, async, fun(Fun) ->
            Fun(),
            self()
        end),
        meck:expect(imboy_syn, count_user, fun(_) -> 2 end),
        ?assertEqual(
            ok, push_notification_logic:maybe_push_for_enterprise_c2g(55, 777, [55, 44, 66])
        ),
        ?assertEqual(0, meck:num_calls(push_notification_ds, send_to_users, 3))
    end).

%% 源码面守卫：企业推送链必须在生产代码里真的接上（修复前 RED）。
enterprise_push_chain_wired_in_src_test() ->
    Logic = read_src("src/logic/enterprise_message_logic.erl"),
    Handler = read_src("src/api/enterprise_message_handler.erl"),
    %% ① logic 必须在 direct/group 两条路径上都调用企业推送入口
    ?assertNotEqual(nomatch, binary:match(Logic, [<<"maybe_push_for_enterprise_c2c(">>])),
    ?assertNotEqual(nomatch, binary:match(Logic, [<<"maybe_push_for_enterprise_c2g(">>])),
    %% ② handler 必须触发提交后推送；`= push_after_commit(` 是唯一调用点
    %%    （另一处 `push_after_commit(` 出现是函数定义头 `-spec push_after_commit(`）。
    ?assertEqual(1, length(binary:matches(Handler, [<<"= push_after_commit(">>]))),
    %% ③ 该调用点必须在 COMMIT 成功分支之后（`{tx_ok, Result} ->`），
    %%    即绝不是事务内触发；其后仍能看到 replay 分支（其内不推送）。
    {TxOkIdx, _} = binary:match(Handler, [<<"{tx_ok, Result} ->">>]),
    {PushIdx, _} = binary:match(Handler, [<<"= push_after_commit(">>]),
    ?assert(PushIdx > TxOkIdx),
    Tail = binary:part(Handler, TxOkIdx, byte_size(Handler) - TxOkIdx),
    ?assertNotEqual(nomatch, binary:match(Tail, [<<"{ok, replay,">>])),
    %% replay 分支到 case 结束之间不得再有推送调用（单调用点已在 ② 钉死，
    %% 此处再确认它不在 replay 分支之后）。
    {ReplayIdx, _} = binary:match(Tail, [<<"{ok, replay,">>]),
    ?assert(PushIdx - TxOkIdx < ReplayIdx),
    %% ④ 企业推送**不得**调用 caller 自定 title/body 的 notify_offline_user(s)
    ?assertEqual(nomatch, binary:match(Logic, [<<"notify_offline_user">>])).

read_src(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> Bin;
        {error, Reason} -> erlang:error({read_failed, Path, Reason})
    end.

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
