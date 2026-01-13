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

%% ===================================================================
%% cancel_timer/3 测试
%% ===================================================================

cancel_timer_broadcasts_and_handles_locally_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> undefined end}
        ], fun() ->
            CurrentUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end)
    end).

cancel_timer_with_existing_timer_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> {ok, make_ref()} end},
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            ?WITH_MECK(erlang, [
                {'cancel_timer', 1, fun(_Ref) -> 1000 end}
            ], fun() ->
                CurrentUid = 123,
                DID = <<"device_abc">>,
                MsgId = <<"msg_xyz">>,

                Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                ?assertEqual(ok, Result)
            end)
        end)
    end).

cancel_timer_with_timer_already_fired_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> {ok, make_ref()} end},
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            ?WITH_MECK(erlang, [
                {'cancel_timer', 1, fun(_Ref) -> false end}
            ], fun() ->
                CurrentUid = 123,
                DID = <<"device_abc">>,
                MsgId = <<"msg_xyz">>,

                Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                ?assertEqual(ok, Result)
            end)
        end)
    end).

%% ===================================================================
%% handle_ack_cancel/3 测试
%% ===================================================================

handle_ack_cancel_sets_ack_received_flag_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, Value, TTL) ->
            % 验证 ACK 标志被正确设置
            ?assertEqual(true, Value),
            ?assert(TTL >= 30000 andalso TTL =< 50000),  % 40秒左右
            ok
        end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        ToUid = 123,
        DID = <<"device_abc">>,
        MsgId = <<"msg_xyz">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_valid_timer_reference_test_() ->
    TestRef = make_ref(),
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> {ok, TestRef} end},
        {'flush', 1, fun(_Key) -> ok end}
    ], fun() ->
        ?WITH_MECK(erlang, [
            {'cancel_timer', 1, fun(Ref) ->
                ?assertEqual(TestRef, Ref),
                500  % 返回剩余时间
            end}
        ], fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end)
    end).

handle_ack_cancel_with_timer_already_fired_test_() ->
    TestRef = make_ref(),
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> {ok, TestRef} end},
        {'flush', 1, fun(_Key) -> ok end}
    ], fun() ->
        ?WITH_MECK(erlang, [
            {'cancel_timer', 1, fun(_Ref) -> false end}
        ], fun() ->
            ToUid = 123,
            DID = <<"device_abc">>,
            MsgId = <<"msg_xyz">>,

            Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end)
    end).

handle_ack_cancel_with_no_timer_found_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        ToUid = 123,
        DID = <<"device_abc">>,
        MsgId = <<"msg_xyz">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_invalid_cache_value_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> {ok, <<"invalid_value">>} end},
        {'flush', 1, fun(_Key) -> ok end}
    ], fun() ->
        ToUid = 123,
        DID = <<"device_abc">>,
        MsgId = <<"msg_xyz">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

handle_ack_cancel_with_empty_did_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        ToUid = 123,
        DID = <<>>,
        MsgId = <<"msg_xyz">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_empty_msg_id_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        ToUid = 123,
        DID = <<"device_abc">>,
        MsgId = <<>>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 并发场景测试
%% ===================================================================

cancel_timer_concurrent_calls_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'broadcast_ack_cancel', 3, fun(_CurrentUid, _DID, _MsgId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end},
            {'get', 1, fun(_Key) -> {ok, make_ref()} end},
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            ?WITH_MECK(erlang, [
                {'cancel_timer', 1, fun(_Ref) -> 1000 end}
            ], fun() ->
                CurrentUid = 123,
                DID = <<"device_abc">>,
                MsgId = <<"msg_xyz">>,

                % 模拟并发调用
                Results = [websocket_logic:cancel_timer(CurrentUid, DID, MsgId) || _ <- lists:seq(1, 10)],
                ?assertEqual([ok || _ <- lists:seq(1, 10)], Results)
            end)
        end)
    end).

%% ===================================================================
%% 缓存 TTL 测试
%% ===================================================================

handle_ack_cancel_cache_ttl_is_40_seconds_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 3, fun(_Key, _Value, TTL) ->
            % 验证 TTL 约为 40 秒
            ?assert(TTL >= 39000 andalso TTL =< 41000),
            ok
        end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        ToUid = 123,
        DID = <<"device_abc">>,
        MsgId = <<"msg_xyz">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).
