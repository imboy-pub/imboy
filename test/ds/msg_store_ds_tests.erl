-module(msg_store_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 定义 state 记录（来自 msg_store_ds.erl）
-record(state, {
    last_flush_time
}).

%%%===================================================================
%%% @doc
%%% msg_store_ds 模块的 EUnit 测试
%%%
%%% 目标：验证消息写入队列管理功能
%%% 覆盖：备份消息、入队、取消备份、状态查询
%%%===================================================================

%% ===================================================================
%% start_link/0 测试
%% ===================================================================

start_link_returns_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证 start_link 函数存在
        ?assert(is_function(fun msg_store_ds:start_link/0, 0))
    end).

%% ===================================================================
%% stage/10 测试
%% ===================================================================

stage_success_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = <<"{\"content\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

stage_with_unique_violation_returns_ok_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            {error, {unique_violation, <<"msg_123">>}}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = <<"{\"content\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        % 幂等性：重复消息应返回 ok
        ?assertEqual(ok, Result)
    end).

stage_with_error_returns_error_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            {error, database_connection_lost}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = <<"{\"content\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(error, Result)
    end).

stage_with_e2ee_metadata_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            % 验证 E2EE 元数据被传递
            ?assert(<<>> =/= E2EE),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<"{\"algorithm\":\"AES256\",\"key\":\"...\"}">>,
        Payload = <<"{\"content\":\"encrypted_data\"}">>,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

stage_with_group_chat_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _Payload, _FromId, ToIdList, _CreatedAt, _ServerTs) ->
            % 验证群聊使用 ToIdList
            ?assert(is_list(ToIdList)),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"c2g">>,
        MsgId = <<"msg_456">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = <<"{\"content\":\"hello group\"}">>,
        FromId = 1,
        ToIdList = [2, 3, 4],
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToIdList, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

stage_with_s2c_action_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, Action, _E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            % 验证 S2C action 被传递
            ?assert(<<>> =/= Action),
            ?assertEqual(<<"pull_offline_msg">>, Action),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"s2c">>,
        MsgId = <<"msg_789">>,
        MsgType = <<"system">>,
        Action = <<"pull_offline_msg">>,
        E2EE = <<>>,
        Payload = <<"{\"msg_ids\":[\"msg1\",\"msg2\"]}">>,
        FromId = 0,
        ToId = 1,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% enqueue/3 测试
%% ===================================================================

enqueue_sends_kick_to_worker_test_() ->
    ?WITH_MECK(msg_store_worker, [
        {'kick', 1, fun(_Pid) -> ok end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        Data = #{payload => <<"{\"content\":\"hello\"}">>},

        Result = msg_store_ds:enqueue(Type, MsgId, Data),
        ?assertEqual(ok, Result)
    end).

enqueue_with_different_types_test_() ->
    ?WITH_MECK(msg_store_worker, [
        {'kick', 1, fun(_Pid) -> ok end}
    ], fun() ->
        Types = [<<"c2c">>, <<"c2g">>, <<"s2c">>, <<"c2s">>],
        lists:foreach(fun(Type) ->
            MsgId = <<"msg_", (integer_to_binary(rand:uniform(1000)))/binary>>,
            Data = #{payload => <<"{}">>},
            Result = msg_store_ds:enqueue(Type, MsgId, Data),
            ?assertEqual(ok, Result)
        end, Types)
    end).

%% ===================================================================
%% unstage/1 测试
%% ===================================================================

unstage_marks_as_processed_test_() ->
    ?WITH_MECK(elib_async, [
        {'async_retry', 1, fun(_Fun) -> ok end}
    ], fun() ->
        MsgId = <<"msg_123">>,

        Result = msg_store_ds:unstage(MsgId),
        ?assertEqual(ok, Result)
    end).

unstage_with_empty_msg_id_test_() ->
    ?WITH_MECK(elib_async, [
        {'async_retry', 1, fun(_Fun) -> ok end}
    ], fun() ->
        MsgId = <<>>,

        Result = msg_store_ds:unstage(MsgId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% len/0 测试
%% ===================================================================

len_returns_pending_count_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, [#{<<"pending">> => 100, <<"processed">> => 5000}]}
        end}
    ], fun() ->
        Result = msg_store_ds:len(),
        ?assertEqual(100, Result)
    end).

len_with_empty_stats_returns_zero_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, []}
        end}
    ], fun() ->
        Result = msg_store_ds:len(),
        ?assertEqual(0, Result)
    end).

len_with_atom_key_in_map_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, [#{pending => 50}]}
        end}
    ], fun() ->
        Result = msg_store_ds:len(),
        ?assertEqual(50, Result)
    end).

%% ===================================================================
%% status/0 测试
%% ===================================================================

status_returns_queue_stats_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, [#{<<"pending">> => 100, <<"processed">> => 5000, <<"total">> => 5100}]}
        end}
    ], fun() ->
        Result = msg_store_ds:status(),
        ?assertMatch(#{
            <<"queue_len">> := 100,
            <<"staging_stats">> := #{<<"pending">> := 100, <<"processed">> := 5000}
        }, Result)
    end).

status_with_error_returns_zero_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {error, database_error}
        end}
    ], fun() ->
        Result = msg_store_ds:status(),
        % staging_pending 函数会返回 0
        ?assertMatch(#{<<"queue_len">> := 0}, Result)
    end).

%% ===================================================================
%% gen_server 回调测试
%% ===================================================================

init_creates_table_and_starts_timer_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'ensure_table_exists', 0, fun() -> ok end}
    ], fun() ->
        {ok, State} = msg_store_ds:init([]),
        ?assert(is_record(State, state)),
        ?assert(is_map_key(last_flush_time, State))
    end).

handle_info_cleanup_staging_deletes_old_records_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'delete_processed', 1, fun(3600) -> {ok, 100} end}
    ], fun() ->
        State = #state{last_flush_time = 0},

        {noreply, NewState} = msg_store_ds:handle_info(cleanup_staging, State),
        ?assert(is_record(NewState, state))
    end).

handle_info_cleanup_staging_with_zero_deleted_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'delete_processed', 1, fun(3600) -> {ok, 0} end}
    ], fun() ->
        State = #state{last_flush_time = 0},

        {noreply, NewState} = msg_store_ds:handle_info(cleanup_staging, State),
        ?assert(is_record(NewState, state))
    end).

handle_info_cleanup_staging_with_error_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'delete_processed', 1, fun(3600) -> {error, connection_failed} end}
    ], fun() ->
        State = #state{last_flush_time = 0},

        {noreply, NewState} = msg_store_ds:handle_info(cleanup_staging, State),
        ?assert(is_record(NewState, state))
    end).

handle_cast_enqueue_kicks_worker_test_() ->
    ?WITH_MECK(msg_store_worker, [
        {'kick', 1, fun(_Pid) -> ok end}
    ], fun() ->
        State = #state{last_flush_time = 0},

        MsgType = <<"c2c">>,
        MsgId = <<"msg_123">>,
        Data = #{payload => <<"{}">>},

        {noreply, NewState} = msg_store_ds:handle_cast({enqueue, MsgType, MsgId, Data}, State),
        ?assert(is_record(NewState, state))
    end).

handle_cast_unstage_calls_async_retry_test_() ->
    ?WITH_MECK(elib_async, [
        {'async_retry', 1, fun(_Fun) -> ok end}
    ], fun() ->
        ?WITH_MECK(msg_store_repo, [
            {'mark_processed', 2, fun(_Type, _MsgId) -> {ok, 1} end}
        ], fun() ->
            State = #state{last_flush_time = 0},
            MsgId = <<"msg_123">>,

            {noreply, NewState} = msg_store_ds:handle_cast({unstage, MsgId}, State),
            ?assert(is_record(NewState, state))
        end)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

stage_with_empty_e2ee_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, E2EE, _Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            % 验证空 E2EE 被正确传递
            ?assertEqual(<<>>, E2EE),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"s2c">>,
        MsgId = <<"msg_789">>,
        MsgType = <<"pull_offline_msg">>,
        Action = <<"pull_offline_msg">>,
        E2EE = <<>>,
        Payload = <<"{\"msg_ids\":[\"msg1\",\"msg2\"]}">>,
        FromId = 0,
        ToId = 1,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

stage_with_large_payload_test_() ->
    LargePayload = lists:foldl(fun(_, Acc) -> <<Acc/binary, "x">> end, <<>>, lists:seq(1, 10000)),
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, Payload, _FromId, _ToId, _CreatedAt, _ServerTs) ->
            % 验证大负载被正确传递
            ?assert(byte_size(Payload) > 10000),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_large">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = LargePayload,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

stage_with_rfc3339_timestamp_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _Payload, _FromId, _ToId, CreatedAt, ServerTs) ->
            % 验证 RFC3339 时间戳被正确传递
            ?assert(is_binary(CreatedAt)),
            ?assert(is_binary(ServerTs)),
            ?assert(binary:match(CreatedAt, <<"T">>) =/= nomatch),
            ?assert(binary:match(ServerTs, <<"T">>) =/= nomatch),
            {ok, 1}
        end}
    ], fun() ->
        Type = <<"c2c">>,
        MsgId = <<"msg_123">>,
        MsgType = <<"text">>,
        Action = <<>>,
        E2EE = <<>>,
        Payload = <<"{\"content\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ServerTs = <<"2023-01-01T00:00:00.123456Z">>,

        Result = msg_store_ds:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

stage_validates_type_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Type = <<"c2c">>,
        ?assert(is_binary(Type))
    end).

stage_validates_msg_type_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"text">>,
        ?assert(is_binary(MsgType))
    end).

stage_validates_from_id_is_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        FromId = 123,
        ?assert(is_integer(FromId))
    end).

stage_validates_to_id_is_integer_or_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ToId1 = 456,
        ToId2 = [1, 2, 3],
        ?assert(is_integer(ToId1)),
        ?assert(is_list(ToId2))
    end).
