-module(websocket_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% websocket_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 WebSocket ACK 定时器管理功能
%%% 覆盖：cancel_timer/3、handle_ack_cancel/3、定时器状态管理
%%%===================================================================

%% ===================================================================
%% cancel_timer/3 测试
%% ===================================================================

cancel_timer_with_valid_params_succeeds_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<"msg_456">>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_with_existing_timer_cancels_it_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, make_ref()} end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end},
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {erlang, [
            {'cancel_timer', 1, fun(_Ref) -> 100 end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<"msg_456">>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_with_already_fired_timer_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, make_ref()} end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end},
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {erlang, [
            {'cancel_timer', 1, fun(_Ref) -> false end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<"msg_456">>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_with_different_uids_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        Uids = [1, 999, 12345, 0],
        DID = <<"device_test">>,
        MsgId = <<"msg_test">>,

        lists:foreach(fun(CurrentUid) ->
            Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end, Uids)
    end).

%% ===================================================================
%% handle_ack_cancel/3 测试
%% ===================================================================

handle_ack_cancel_with_timer_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_valid_timer_ref_test_() ->
    TimerRef = make_ref(),
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, TimerRef} end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end},
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {erlang, [
            {'cancel_timer', 1, fun(_Ref) -> 100 end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_invalid_cache_value_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, "invalid_value"} end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end},
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_sets_ack_received_flag_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(Key, true, 40000) ->
                % 验证设置了 ACK 标志
                ?assertMatch({ack_received, _, _, _}, Key),
                ok
            end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_already_fired_timer_test_() ->
    TimerRef = make_ref(),
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, TimerRef} end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end},
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {erlang, [
            {'cancel_timer', 1, fun(_Ref) -> false end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

cancel_timer_with_empty_did_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<>>,
        MsgId = <<"msg_456">>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_with_empty_msg_id_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<>>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_zero_uid_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        ToUid = 0,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

handle_ack_cancel_with_large_uid_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, _TTL) -> ok end}
        ]}
    ], fun() ->
        ToUid = 999999999,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 定时器键格式测试
%% ===================================================================

handle_ack_cancel_timer_key_format_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(Key, _Val, _TTL) ->
                % 验证键格式: {ToUid, DID, MsgId}
                ?assertMatch({_, _, _}, Key),
                ok
            end}
        ]}
    ], fun() ->
        ToUid = 789,
        DID = <<"device_key">>,
        MsgId = <<"msg_key">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_ack_received_key_format_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'broadcast_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(Key, true, 40000) ->
                % 验证 ACK 接收键格式: {ack_received, ToUid, DID, MsgId}
                ?assertMatch({ack_received, _, _, _}, Key),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<"msg_456">>,

        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% ACK 接收标志 TTL 测试
%% ===================================================================

handle_ack_cancel_with_40_second_ttl_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end},
            {'set', 3, fun(_Key, _Val, TTL) ->
                % 验证 TTL 为 40 秒（最大重试时间）
                ?assertEqual(40000, TTL),
                ok
            end}
        ]}
    ], fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,

        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).
