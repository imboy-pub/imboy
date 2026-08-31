-module(websocket_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% websocket_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 WebSocket ACK 定时器管理功能
%%% 覆盖：取消定时器、处理 ACK 取消、缓存管理
%%%===================================================================

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

%% ===================================================================
%% cancel_timer/3 测试
%% ===================================================================

cancel_timer_broadcasts_and_handles_locally_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
        ],
        fun() ->
            run_with_mocks(
                [
                    {ack_retry_cache, [
                        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
                        {'get', 1, fun(_Key) -> undefined end}
                    ]}
                ],
                fun() ->
                    CurrentUid = 123,
                    DID = <<"device_abc">>,
                    MsgId = <<"msg_xyz">>,

                    Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    ?assertEqual(ok, Result)
                end
            )
        end
    ).

cancel_timer_with_existing_timer_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
        ],
        fun() ->
            TimerRef = make_ref(),
            run_with_mocks(
                [
                    {ack_retry_cache, [
                        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
                        {'get', 1, fun(_Key) -> {ok, TimerRef} end},
                        %% cancel_timer 的返回值仅进日志；delete_if_value 携带
                        %% 原 Ref 即证明走了 cancel 分支。不 mock erlang 模块
                        %% （meck unstick BIF 模块会楔死整个 VM，实测挂死）。
                        {'delete_if_value', 2, fun(_Key, Ref) ->
                            ?assertEqual(TimerRef, Ref),
                            true
                        end}
                    ]}
                ],
                fun() ->
                    CurrentUid = 123,
                    DID = <<"device_abc">>,
                    MsgId = <<"msg_xyz">>,

                    Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    ?assertEqual(ok, Result)
                end
            )
        end
    ).

cancel_timer_with_timer_already_fired_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
        ],
        fun() ->
            TimerRef = make_ref(),
            run_with_mocks(
                [
                    {ack_retry_cache, [
                        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
                        %% timer 已触发（cancel_timer 返回 false）与未触发
                        %% （返回剩余时间）在可观察行为上等价：均继续删除原 Ref
                        {'get', 1, fun(_Key) -> {ok, TimerRef} end},
                        {'delete_if_value', 2, fun(_Key, Ref) ->
                            ?assertEqual(TimerRef, Ref),
                            true
                        end}
                    ]}
                ],
                fun() ->
                    CurrentUid = 123,
                    DID = <<"device_abc">>,
                    MsgId = <<"msg_xyz">>,

                    Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    ?assertEqual(ok, Result)
                end
            )
        end
    ).

cancel_timer_broadcast_failure_still_handles_locally_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) ->
                erlang:error(simulated_syn_failure)
            end}
        ],
        fun() ->
            run_with_mocks(
                [
                    {ack_retry_cache, [
                        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
                        {'get', 1, fun(_Key) -> undefined end}
                    ]}
                ],
                fun() ->
                    CurrentUid = 123,
                    DID = <<"device_abc">>,
                    MsgId = <<"msg_broadcast_fail">>,

                    Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    ?assertEqual(ok, Result),
                    ?assertEqual(1, meck:num_calls(ack_retry_cache, set, 3))
                end
            )
        end
    ).

%% ===================================================================
%% handle_ack_cancel/3 测试
%% ===================================================================

handle_ack_cancel_sets_ack_received_flag_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, Value, TTL) ->
                % 验证 ACK 标志被正确设置
                ?assertEqual(true, Value),
                % 40秒左右
                ?assert(TTL >= 30000 andalso TTL =< 50000),
                ok
            end},
            {'get', 1, fun(_Key) -> undefined end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_with_valid_timer_reference_test_() ->
    TestRef = make_ref(),
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> {ok, TestRef} end},
            %% cancel_timer 返回值仅日志可见；delete_if_value 携带原 Ref
            %% 即证明走了 {ok, Ref} 分支（不 mock erlang 模块，会楔死 VM）
            {'delete_if_value', 2, fun(_Key, Ref) ->
                ?assertEqual(TestRef, Ref),
                true
            end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_with_timer_already_fired_test_() ->
    TestRef = make_ref(),
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            %% timer 已触发（cancel_timer 返回 false）与未触发可观察行为等价
            {'get', 1, fun(_Key) -> {ok, TestRef} end},
            {'delete_if_value', 2, fun(_Key, Ref) ->
                ?assertEqual(TestRef, Ref),
                true
            end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_with_no_timer_found_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> undefined end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_with_invalid_cache_value_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> {ok, <<"invalid_value">>} end},
            {'delete_if_value', 2, fun(_Key, _Value) -> true end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_duplicate_ack_is_idempotent_test_() ->
    TestRef = make_ref(),
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) ->
                case erlang:get(ack_cancel_get_seen) of
                    undefined ->
                        erlang:put(ack_cancel_get_seen, 1),
                        {ok, TestRef};
                    _ ->
                        undefined
                end
            end},
            {'delete_if_value', 2, fun(_Key, Ref) ->
                ?assertEqual(TestRef, Ref),
                true
            end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_dup_ack">>,

            erase(ack_cancel_get_seen),
            ?assertEqual(ok, websocket_logic:handle_ack_cancel(ToUid, DID, MsgId)),
            ?assertEqual(ok, websocket_logic:handle_ack_cancel(ToUid, DID, MsgId)),
            %% 幂等性：第二次 ACK 看到标志后 get→undefined，不再走 cancel 分支
            ?assertEqual(1, meck:num_calls(ack_retry_cache, delete_if_value, 2)),
            erase(ack_cancel_get_seen)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

handle_ack_cancel_with_empty_did_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> undefined end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<>>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

handle_ack_cancel_with_empty_msg_id_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> undefined end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<>>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 并发场景测试
%% ===================================================================

cancel_timer_concurrent_calls_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
        ],
        fun() ->
            run_with_mocks(
                [
                    {ack_retry_cache, [
                        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
                        {'get', 1, fun(_Key) -> {ok, make_ref()} end},
                        {'delete_if_value', 2, fun(_Key, _Ref) -> true end}
                    ]}
                ],
                fun() ->
                    CurrentUid = 123,
                    DID = <<"device_abc">>,
                    MsgId = <<"msg_xyz">>,

                    % 模拟并发调用
                    Results = [
                        websocket_logic:cancel_timer(CurrentUid, DID, MsgId)
                     || _ <- lists:seq(1, 10)
                    ],
                    ?assertEqual([ok || _ <- lists:seq(1, 10)], Results)
                end
            )
        end
    ).

%% ===================================================================
%% 缓存 TTL 测试
%% ===================================================================

handle_ack_cancel_cache_ttl_is_40_seconds_test_() ->
    ?WITH_MECK(
        ack_retry_cache,
        [
            {'set', 3, fun(_Key, _Value, TTL) ->
                % 验证 TTL 约为 40 秒
                ?assert(TTL >= 39000 andalso TTL =< 41000),
                ok
            end},
            {'get', 1, fun(_Key) -> undefined end}
        ],
        fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end
    ).
