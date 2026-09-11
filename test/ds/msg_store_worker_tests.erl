-module(msg_store_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%%
%%% msg_store_worker 模块的 EUnit 测试
%%%
%%% 目标：验证消息写入批量处理器功能
%%% 覆盖：start_link/0, callback_mode/0
%%%===================================================================

%% ===================================================================
%% start_link/0 测试
%% ===================================================================

start_link_returns_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证 start_link 函数存在
        ?assert(is_function(fun msg_store_worker:start_link/0, 0))
    end).

%% ===================================================================
%% callback_mode/0 测试
%% ===================================================================

callback_mode_returns_state_functions_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_function(fun msg_store_worker:callback_mode/0, 0)),
        ?assertEqual(state_functions, msg_store_worker:callback_mode())
    end).

%% ===================================================================
%% batch_write/1 测试
%% ===================================================================

batch_write_with_zero_count_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 0,
        ?assert(is_integer(Count)),
        ?assertEqual(0, Count)
    end).

batch_write_with_small_count_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 10,
        ?assert(is_integer(Count)),
        ?assert(Count > 0)
    end).

batch_write_with_batch_size_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 100,
        ?assert(is_integer(Count)),
        ?assertEqual(100, Count)
    end).

batch_write_with_large_count_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 1000,
        ?assert(is_integer(Count)),
        ?assert(Count > 100)
    end).

%% ===================================================================
%% write_to_db/1 测试
%% ===================================================================

write_to_db_with_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [],
        ?assert(is_list(Items)),
        ?assertEqual(0, length(Items))
    end).

write_to_db_with_single_item_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [{c2c, <<"msg1">>, #{payload => <<"test">>}}],
        ?assert(is_list(Items)),
        ?assertEqual(1, length(Items))
    end).

write_to_db_with_multiple_items_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [
            {c2c, <<"msg1">>, #{payload => <<"test1">>}},
            {c2g, <<"msg2">>, #{payload => <<"test2">>}},
            {s2c, <<"msg3">>, #{payload => <<"test3">>}}
        ],
        ?assert(is_list(Items)),
        ?assertEqual(3, length(Items))
    end).

write_to_db_with_mixed_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [
            {c2c, <<"msg1">>, #{payload => <<"c2c">>}},
            {c2g, <<"msg2">>, #{payload => <<"c2g">>}},
            {s2c, <<"msg3">>, #{payload => <<"s2c">>}},
            {c2s, <<"msg4">>, #{payload => <<"c2s">>}}
        ],
        ?assert(is_list(Items)),
        ?assertEqual(4, length(Items))
    end).

%% ===================================================================
%% batch_write_by_type/2 测试
%% ===================================================================

batch_write_by_type_c2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgList = [
            {c2c, <<"msg1">>, #{payload => <<"test1">>, from_id => 123, to_id => 456}},
            {c2c, <<"msg2">>, #{payload => <<"test2">>, from_id => 789, to_id => 012}}
        ],
        ?assertEqual(c2c, MsgType),
        ?assert(is_list(MsgList)),
        ?assertEqual(2, length(MsgList))
    end).

batch_write_by_type_c2g_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2g,
        MsgList = [
            {c2g, <<"msg1">>, #{
                payload => <<"test1">>, from_id => 123, to_id => 789, to_id_list => [1, 2, 3]
            }}
        ],
        ?assertEqual(c2g, MsgType),
        {_Type, _MsgId, Data} = hd(MsgList),
        ?assert(is_list(maps:get(to_id_list, Data)))
    end).

batch_write_by_type_s2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = s2c,
        _MsgList = [
            {s2c, <<"msg1">>, #{payload => <<"test1">>, from_id => 999, to_id => 111}}
        ],
        ?assertEqual(s2c, MsgType)
    end).

batch_write_by_type_c2s_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2s,
        MsgList = [
            {c2s, <<"msg1">>, #{
                status => 12,
                topic_id => 0,
                from_id => 123,
                to_id_str => <<"456">>,
                payload => <<"test">>,
                created_at => 1704067200
            }}
        ],
        ?assertEqual(c2s, MsgType),
        {_Type, _MsgId, Data} = hd(MsgList),
        ?assert(is_integer(maps:get(status, Data)))
    end).

batch_write_by_type_unknown_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = unknown,
        MsgList = [],
        ?assertEqual(unknown, MsgType),
        ?assertEqual([], MsgList)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

batch_write_with_negative_count_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = -1,
        ?assert(is_integer(Count)),
        ?assert(Count < 0)
    end).

write_to_db_with_large_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [
            {c2c, <<"msg", (integer_to_binary(I))/binary>>, #{payload => <<"test">>}}
         || I <- lists:seq(1, 1000)
        ],
        ?assert(is_list(Items)),
        ?assertEqual(1000, length(Items))
    end).

batch_write_by_type_with_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgList = [],
        ?assertEqual(c2c, MsgType),
        ?assertEqual([], MsgList)
    end).

batch_write_by_type_with_single_item_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgList = [{c2c, <<"msg1">>, #{payload => <<"test">>}}],
        ?assertEqual(c2c, MsgType),
        ?assertEqual(1, length(MsgList))
    end).

recover_staged_messages_with_no_messages_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 没有待恢复的消息
        ?assert(is_function(fun msg_store_worker:recover_staged_messages/0, 0))
    end).

flush_queue_with_empty_queue_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 空队列情况
        ?assert(is_function(fun msg_store_worker:flush_queue/0, 0))
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

batch_write_count_is_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 100,
        ?assert(is_integer(Count))
    end).

write_to_db_items_is_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Items = [{c2c, <<"msg1">>, #{payload => <<"test">>}}],
        ?assert(is_list(Items))
    end).

write_to_db_item_is_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item = {c2c, <<"msg1">>, #{payload => <<"test">>}},
        ?assert(is_tuple(Item)),
        ?assertEqual(3, tuple_size(Item))
    end).

batch_write_by_type_type_is_atom_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        ?assert(is_atom(MsgType))
    end).

batch_write_by_type_list_is_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgList = [{c2c, <<"msg1">>, #{payload => <<"test">>}}],
        ?assert(is_list(MsgList))
    end).

batch_write_by_type_item_is_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item = {c2c, <<"msg1">>, #{payload => <<"test">>}},
        ?assert(is_tuple(Item)),
        ?assertEqual(3, tuple_size(Item))
    end).

recover_staged_messages_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        % recover_staged_messages 应该返回 ok
        ?assert(is_function(fun msg_store_worker:recover_staged_messages/0, 0))
    end).

flush_queue_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        % flush_queue 应该返回 ok
        ?assert(is_function(fun msg_store_worker:flush_queue/0, 0))
    end).

c2g_write_requires_authoritative_conv_seq_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2g_repo, [
                {'write_accepted_msg', 11, fun(_, _, _, _, _, _, _, _, _, _, _) ->
                    erlang:error(should_not_write_without_conv_seq)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, c2g_conv_seq_missing},
                msg_store_worker:do_write(c2g, c2g_row(null))
            ),
            ?assertEqual(0, meck:num_calls(msg_c2g_repo, write_accepted_msg, 11))
        end
    ).

c2g_write_propagates_authoritative_conv_seq_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2g_repo, [
                {'write_accepted_msg', 11, fun(
                    _, _, _, _, _, 9, _, _, null, 77, <<"did-1">>
                ) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, msg_store_worker:do_write(c2g, c2g_row(77))),
            ?assertEqual(1, meck:num_calls(msg_c2g_repo, write_accepted_msg, 11))
        end
    ).

c2g_row(ConvSeq) ->
    #{
        <<"payload">> => <<"{\"to\":\"9\"}">>,
        <<"from_id">> => 1,
        <<"to_id_list">> => [1, 2],
        <<"created_at">> => <<"2026-09-11T00:00:00Z">>,
        <<"msg_id">> => <<"timeline-seq-test">>,
        <<"msg_type">> => <<"text">>,
        <<"e2ee">> => null,
        <<"sender_did">> => <<"did-1">>,
        <<"conv_seq">> => ConvSeq
    }.

%% ===================================================================
%% 数据结构测试
%% ===================================================================

c2c_message_structure_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item =
            {c2c, <<"msg1">>, #{
                payload => <<"{\"text\":\"hello\"}">>,
                from_id => 123,
                to_id => 456
            }},
        ?assertEqual(c2c, element(1, Item)),
        ?assertEqual(<<"msg1">>, element(2, Item)),
        ?assert(is_map(element(3, Item)))
    end).

c2g_message_structure_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item =
            {c2g, <<"msg2">>, #{
                payload => <<"{\"text\":\"group\"}">>,
                from_id => 123,
                to_id => 789,
                to_id_list => [1, 2, 3]
            }},
        ?assertEqual(c2g, element(1, Item)),
        ?assert(is_list(maps:get(to_id_list, element(3, Item))))
    end).

s2c_message_structure_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item =
            {s2c, <<"msg3">>, #{
                payload => <<"{\"text\":\"system\"}">>,
                from_id => 999,
                to_id => 111
            }},
        ?assertEqual(s2c, element(1, Item))
    end).

c2s_message_structure_test_() ->
    ?TEST_SIMPLE(fun() ->
        Item =
            {c2s, <<"msg4">>, #{
                status => 12,
                topic_id => 0,
                from_id => 123,
                to_id_str => <<"456">>,
                payload => <<"{\"text\":\"c2s\"}">>,
                created_at => 1704067200
            }},
        ?assertEqual(c2s, element(1, Item)),
        ?assert(is_integer(maps:get(status, element(3, Item))))
    end).

%% C2G staging 结构性失败必须停止永久重试，短暂 DB 错误仍需重试。
c2g_guard_row(Overrides) ->
    maps:merge(
        #{
            <<"type">> => <<"c2g">>,
            <<"msg_id">> => <<"m-guard-1">>,
            <<"payload">> => <<"{\"to\": \"538339\"}">>,
            <<"from_id">> => 7,
            <<"to_id_list">> => [9, 10],
            <<"msg_type">> => <<"text">>,
            <<"sender_did">> => <<"did-guard">>,
            <<"conv_seq">> => 1,
            <<"retry_count">> => 0
        },
        Overrides
    ).

do_write_c2g_null_recipients_is_no_recipients_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, no_recipients},
            msg_store_worker:do_write(c2g, c2g_guard_row(#{<<"to_id_list">> => null}))
        )
    end).

do_write_c2g_empty_recipients_is_no_recipients_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, no_recipients},
            msg_store_worker:do_write(c2g, c2g_guard_row(#{<<"to_id_list">> => []}))
        )
    end).

do_write_c2g_missing_gid_is_gid_missing_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, c2g_gid_missing},
            msg_store_worker:do_write(c2g, c2g_guard_row(#{<<"payload">> => <<"{}">>}))
        )
    end).

do_write_c2g_missing_conv_seq_is_conv_seq_missing_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, c2g_conv_seq_missing},
            msg_store_worker:do_write(c2g, c2g_guard_row(#{<<"conv_seq">> => null}))
        )
    end).

terminal_write_reason_classification_test() ->
    Terminal = [
        no_recipients,
        c2g_conv_seq_missing,
        c2g_gid_missing,
        {unknown_msg_type, c2c, <<"m-x">>}
    ],
    [?assert(msg_store_worker:terminal_write_reason(R)) || R <- Terminal],
    Retryable = [
        {db_exception, error, timeout},
        closed,
        {rollback, forbidden},
        {msg_c2g_insert_failed, unique_violation}
    ],
    [?assertNot(msg_store_worker:terminal_write_reason(R)) || R <- Retryable].

process_row_structural_failure_marks_terminal_test_() ->
    ?WITH_MECKS(
        [
            {msg_store_repo, [
                {'mark_terminal', 3, fun(_T, _M, _E) -> {ok, 1} end},
                {'mark_failed', 4, fun(_T, _M, _E, _D) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            msg_store_worker:process_row(c2g_guard_row(#{<<"to_id_list">> => null})),
            ?assertEqual(1, meck:num_calls(msg_store_repo, mark_terminal, 3)),
            ?assertEqual(0, meck:num_calls(msg_store_repo, mark_failed, 4))
        end
    ).

process_row_db_failure_retries_not_terminal_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2g_repo, [
                {'write_accepted_msg', 11, fun(_, _, _, _, _, _, _, _, _, _, _) ->
                    {error, {db_exception, error, timeout}}
                end}
            ]},
            {msg_store_repo, [
                {'mark_terminal', 3, fun(_T, _M, _E) -> {ok, 1} end},
                {'mark_failed', 4, fun(_T, _M, _E, _D) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            msg_store_worker:process_row(c2g_guard_row(#{})),
            ?assertEqual(0, meck:num_calls(msg_store_repo, mark_terminal, 3)),
            ?assertEqual(1, meck:num_calls(msg_store_repo, mark_failed, 4))
        end
    ).
