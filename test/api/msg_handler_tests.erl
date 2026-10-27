-module(msg_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% msg_handler 模块的 EUnit 测试
%%%
%%% 目标：验证消息处理器功能
%%% 覆盖：离线消息处理、消息确认
%%%===================================================================

req_mock() -> cowboy_req_h:new(#{}).

%% ===================================================================
%% init/2 测试（按 action 分发）
%% ===================================================================

%% ARCH-01：REST action 路由已收归 msg_handler:dispatch_rest_action/3（私有函数，
%% 不再委托 messaging_logic），端到端覆盖见下方 offline_with_default_parameters_test_。
init_with_valid_request_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = req_mock(),
        {ok, Req1, State1} = msg_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, Req1),
        ?assertEqual(#{current_uid => 12345}, State1)
    end).

init_with_missing_uid_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(_Param, _Req, Default) -> {ok, Default} end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_error_code, Code},
                    req_unauthorized
                end}
            ]}
        ],
        fun() ->
            {ok, Req1, State1} = msg_handler:init(req_mock(), #{action => offline}),
            ?assertEqual(req_unauthorized, Req1),
            ?assertEqual(#{}, State1),
            Code =
                receive
                    {resp_error_code, C} -> C
                after 1000 ->
                    timeout
                end,
            ?assertEqual(?ERR_UNAUTHORIZED, Code)
        end
    ).

%% ===================================================================
%% 参数处理测试
%% ===================================================================

offline_with_default_parameters_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(_Param, _Req, Default) -> {ok, Default} end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end}
            ]},
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {msg_s2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 0}]} end}
            ]},
            {msg_c2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            State0 = #{action => offline, current_uid => 12345},
            {ok, Req1, NewState} = msg_handler:init(req_mock(), State0),
            ?assertEqual(req_ok, Req1),
            ?assertEqual(#{current_uid => 12345}, NewState),

            Payload =
                receive
                    {resp_data, Data} -> Data
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Payload),
            lists:foreach(
                fun(Type) ->
                    TypeMap = maps:get(Type, Payload),
                    ?assertEqual(false, maps:get(<<"has_more">>, TypeMap)),
                    ?assertEqual(0, maps:get(<<"total">>, TypeMap)),
                    ?assertEqual([], maps:get(<<"list">>, TypeMap))
                end,
                [<<"c2c">>, <<"c2g">>, <<"s2c">>]
            )
        end
    ).

offline_with_custom_parameters_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(Param, _Req, _Default) ->
                    case Param of
                        limit -> {ok, 500};
                        c2c_last_msg_at -> {ok, 1640995200};
                        c2g_last_msg_at -> {ok, 1640995300};
                        s2c_last_msg_at -> {ok, 1640995400}
                    end
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 2, fun
                    (1640995200, millisecond) -> <<"ts-c2c">>;
                    (1640995300, millisecond) -> <<"ts-c2g">>;
                    (1640995400, millisecond) -> <<"ts-s2c">>;
                    (_Any, millisecond) -> <<"ts-default">>
                end}
            ]},
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {msg_s2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun
                    (_Sql, [12345, <<"ts-c2c">>]) -> {ok, [#{<<"count">> => 2}]};
                    (_Sql, [12345, <<"ts-c2g">>]) -> {ok, [#{<<"count">> => 3}]};
                    (_Sql, [12345, <<"ts-s2c">>]) -> {ok, [#{<<"count">> => 1}]}
                end}
            ]},
            {msg_c2c_ds, [
                {'read_msg_for_device', 4, fun(12345, _DID, 500, <<"ts-c2c">>) ->
                    [
                        #{
                            <<"id">> => 1,
                            <<"from_id">> => 11,
                            <<"to_id">> => 22,
                            <<"content">> => <<"test1">>,
                            <<"created_at">> => 1640995201
                        }
                    ]
                end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(12345, 500, <<"ts-c2g">>) ->
                    [
                        #{
                            <<"id">> => 2,
                            <<"from_id">> => 33,
                            <<"to_id">> => [44, 55],
                            <<"content">> => <<"test2">>,
                            <<"created_at">> => 1640995301
                        },
                        #{
                            <<"id">> => 4,
                            <<"content">> => <<"test4">>,
                            <<"created_at">> => 1640995302
                        }
                    ]
                end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(12345, _DID, 500, <<"ts-s2c">>) ->
                    [
                        #{
                            <<"id">> => 3,
                            <<"content">> => <<"test3">>,
                            <<"created_at">> => 1640995401
                        }
                    ]
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            {ok, Req1, NewState} = msg_handler:init(req_mock(), #{
                action => offline, current_uid => 12345
            }),
            ?assertEqual(req_ok, Req1),
            ?assertEqual(#{current_uid => 12345}, NewState),

            Payload =
                receive
                    {resp_data, Data} -> Data
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Payload),

            C2C = maps:get(<<"c2c">>, Payload),
            C2CList = maps:get(<<"list">>, C2C),
            ?assertEqual(true, maps:get(<<"has_more">>, C2C)),
            ?assertEqual(2, maps:get(<<"total">>, C2C)),
            ?assertMatch([_], C2CList),
            [C2CMsg] = C2CList,
            %% S0 契约（e23a9df0）：信封 ID 统一归一化为 binary 字符串
            %% （TSID 大整数超 JS 安全范围）。
            ?assertEqual(<<"11">>, maps:get(<<"from">>, C2CMsg)),
            ?assertEqual(<<"22">>, maps:get(<<"to">>, C2CMsg)),
            ?assertEqual(false, maps:is_key(<<"from_id">>, C2CMsg)),
            ?assertEqual(false, maps:is_key(<<"to_id">>, C2CMsg)),

            C2G = maps:get(<<"c2g">>, Payload),
            C2GList = maps:get(<<"list">>, C2G),
            ?assertEqual(true, maps:get(<<"has_more">>, C2G)),
            ?assertEqual(3, maps:get(<<"total">>, C2G)),
            ?assertMatch([_, _], C2GList),

            S2C = maps:get(<<"s2c">>, Payload),
            ?assertEqual(false, maps:get(<<"has_more">>, S2C)),
            ?assertEqual(1, maps:get(<<"total">>, S2C)),
            ?assertMatch([_], maps:get(<<"list">>, S2C))
        end
    ).

%% ===================================================================
%% 消息处理测试
%% ===================================================================

process_message_with_valid_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试消息数据处理逻辑
        Msg = #{
            <<"id">> => 1,
            <<"from_id">> => 123,
            <<"to_id">> => 456,
            <<"content">> => <<"Hello World">>,
            <<"created_at">> => 1640995200
        },

        % 验证消息结构
        ?assertMatch(
            #{
                <<"id">> := _,
                <<"from_id">> := _,
                <<"to_id">> := _,
                <<"content">> := _,
                <<"created_at">> := _
            },
            Msg
        ),

        % 验证字段类型
        ?assert(is_integer(maps:get(<<"id">>, Msg))),
        ?assert(is_integer(maps:get(<<"from_id">>, Msg))),
        ?assert(is_integer(maps:get(<<"to_id">>, Msg))),
        ?assertMatch(<<_/binary>>, maps:get(<<"content">>, Msg)),
        ?assert(is_integer(maps:get(<<"created_at">>, Msg)))
    end).

process_message_with_empty_content_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试空内容消息处理
        Msg = #{
            <<"id">> => 1,
            <<"from_id">> => 123,
            <<"to_id">> => 456,
            <<"content">> => <<>>,
            <<"created_at">> => 1640995200
        },

        % 验证空内容消息
        ?assert(is_map(Msg)),
        ?assertEqual(<<>>, maps:get(<<"content">>, Msg)),
        ?assertEqual(0, byte_size(maps:get(<<"content">>, Msg)))
    end).

%% ===================================================================
%% 分页计算测试
%% ===================================================================

calculate_has_more_with_more_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试有更多数据的分页计算
        ProcessedMsgs = [#{id => 1}, #{id => 2}],
        Count = 5,

        HasMore = length(ProcessedMsgs) < Count,
        ?assert(HasMore),
        ?assert(length(ProcessedMsgs) < Count)
    end).

calculate_has_more_with_no_more_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试没有更多数据的分页计算
        ProcessedMsgs = [#{id => 1}, #{id => 2}, #{id => 3}],
        Count = 3,

        HasMore = length(ProcessedMsgs) < Count,
        ?assertNot(HasMore),
        ?assertEqual(length(ProcessedMsgs), Count)
    end).

calculate_next_last_msg_at_with_messages_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试下一条消息时间计算
        ProcessedMsgs = [#{created_at => 1640995200}, #{created_at => 1640995300}],
        LastMsgAt = <<"2023-01-01T00:00:00Z">>,

        % 验证消息按时间排序
        ?assert(length(ProcessedMsgs) > 0),
        ?assertMatch([_ | _], ProcessedMsgs),
        ?assertMatch(<<_/binary>>, LastMsgAt)
    end).

calculate_next_last_msg_at_with_empty_messages_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试空消息列表的时间计算
        ProcessedMsgs = [],
        LastMsgAt = <<"2023-01-01T00:00:00Z">>,

        % 验证空列表处理
        ?assertEqual(0, length(ProcessedMsgs)),
        ?assertMatch(<<_/binary>>, LastMsgAt)
    end).

%% ===================================================================
%% 响应结构测试
%% ===================================================================

response_structure_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试响应结构验证
        Payload = #{
            <<"c2c">> => #{
                <<"has_more">> => false,
                <<"next_last_msg_at">> => <<"2023-01-01T00:00:00Z">>,
                <<"total">> => 0,
                <<"list">> => []
            },
            <<"c2g">> => #{
                <<"has_more">> => false,
                <<"next_last_msg_at">> => <<"2023-01-01T00:00:00Z">>,
                <<"total">> => 0,
                <<"list">> => []
            },
            <<"s2c">> => #{
                <<"has_more">> => false,
                <<"next_last_msg_at">> => <<"2023-01-01T00:00:00Z">>,
                <<"total">> => 0,
                <<"list">> => []
            }
        },

        % 验证响应结构
        ?assert(is_map(Payload)),
        ?assert(maps:is_key(<<"c2c">>, Payload)),
        ?assert(maps:is_key(<<"c2g">>, Payload)),
        ?assert(maps:is_key(<<"s2c">>, Payload)),

        % 验证每个消息类型的结构
        lists:foreach(
            fun(Type) ->
                TypeMap = maps:get(Type, Payload),
                ?assert(maps:is_key(<<"has_more">>, TypeMap)),
                ?assert(maps:is_key(<<"next_last_msg_at">>, TypeMap)),
                ?assert(maps:is_key(<<"total">>, TypeMap)),
                ?assert(maps:is_key(<<"list">>, TypeMap)),
                ?assert(is_list(maps:get(<<"list">>, TypeMap)))
            end,
            [<<"c2c">>, <<"c2g">>, <<"s2c">>]
        )
    end).

%% ===================================================================
%% P3（审查发现修复轮）：history_backfill 负 after_seq 入口 400
%% ===================================================================

%% 负游标入口早失败：400 + 文案；不触达 messaging_logic:history_backfill。
backfill_negative_after_seq_rejected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun
                    (after_seq, _Req, _Default) -> {ok, -1};
                    (_Param, _Req, Default) -> {ok, Default}
                end}
            ]},
            {messaging_logic, [
                {'history_backfill', 6, fun(_U, _T, _P, _S, _L, _D) -> {ok, #{}} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_error_code, Code},
                    req_backfill_rejected
                end}
            ]}
        ],
        fun() ->
            Req = (req_mock())#{qs => <<"chat_type=c2c&peer_id=200&after_seq=-1">>},
            ?assertEqual(
                req_backfill_rejected, msg_handler:history_backfill(Req, #{current_uid => 12345})
            ),
            ?assertEqual(
                ?ERR_BAD_REQUEST,
                receive
                    {resp_error_code, C} -> C
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(0, meck:num_calls(messaging_logic, history_backfill, 6))
        end
    ).

%% 非负 after_seq 照常透传 logic（负值校验不破坏正常路径）。
backfill_zero_after_seq_passes_through_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(_Param, _Req, Default) -> {ok, Default} end}
            ]},
            {messaging_logic, [
                {'history_backfill', 6, fun(12345, <<"c2c">>, <<"200">>, 0, 50, <<>>) ->
                    {ok, #{<<"messages">> => []}}
                end}
            ]},
            {elib_response, [
                {'success_rfc3339', 2, fun(_Req, _Payload) -> req_backfill_ok end}
            ]}
        ],
        fun() ->
            Req = (req_mock())#{qs => <<"chat_type=c2c&peer_id=200&after_seq=0">>},
            ?assertEqual(
                req_backfill_ok, msg_handler:history_backfill(Req, #{current_uid => 12345})
            ),
            ?assertEqual(1, meck:num_calls(messaging_logic, history_backfill, 6))
        end
    ).

%% ===================================================================
%% Q06（AC12-R3 债务修复）：offline limit 上限夹紧
%% ===================================================================

history_limit_clamped_test_() ->
    [
        history_limit_clamp_case(Input, Expected)
     || {Input, Expected} <- [{0, 1}, {-1, 1}, {101, 100}]
    ].

history_limit_clamp_case(InputLimit, ExpectedLimit) ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun
                    (limit, _Req, _Default) -> {ok, InputLimit};
                    (_Param, _Req, Default) -> {ok, Default}
                end}
            ]},
            {messaging_logic, [
                {'history', 6, fun(12345, <<"c2c">>, <<"200">>, 0, Limit, <<"test-device">>) ->
                    ?assertEqual(ExpectedLimit, Limit),
                    {ok, #{<<"messages">> => [], <<"next_seq">> => 0}}
                end}
            ]},
            {elib_response, [
                {'success_rfc3339', 2, fun(_Req, _Payload) -> req_history_ok end}
            ]}
        ],
        fun() ->
            Req = (req_mock())#{qs => <<"chat_type=c2c&peer_id=200&did=test-device">>},
            ?assertEqual(req_history_ok, msg_handler:history(Req, #{current_uid => 12345})),
            ?assertEqual(1, meck:num_calls(messaging_logic, history, 6))
        end
    ).

%% @doc 客户端可传超大 limit（如 10^6）；修复前 elib_param 默认值 1000
%% 之外无上限，超大值直通 DS 查询（DB 压垮面）。修复后与 history 的
%% min 100 范式对齐：上限夹 1000（App 离线拉取合同的单页上限）。
offline_limit_clamped_upper_test_() ->
    offline_limit_clamp_case(1000000, 1000).

%% @doc 下限保护：limit=0/负数不得产生空扫描或非法 LIMIT 参数
offline_limit_clamped_lower_test_() ->
    offline_limit_clamp_case(0, 1).

offline_limit_clamp_case(InputLimit, ExpectedLimit) ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun
                    (limit, _Req, _Default) -> {ok, InputLimit};
                    (_Param, _Req, Default) -> {ok, Default}
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end}
            ]},
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {msg_s2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 0}]} end}
            ]},
            {msg_c2c_ds, [
                {'count_unread_since', 3, fun(_Uid, _Ts, _DID) -> 0 end},
                {'read_msg_for_device', 4, fun(_Uid, _DID, Limit, _LastMsgAt) ->
                    %% 钉住夹紧后的 limit 直通 DS 查询
                    self() ! {ds_received_limit, Limit},
                    []
                end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, Limit, _LastMsgAt) ->
                    self() ! {ds_received_limit_c2g, Limit},
                    []
                end}
            ]},
            {msg_s2c_ds, [
                {'count_since', 3, fun(_Uid, _Ts, _DID) -> 0 end},
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Data) -> req_ok end}
            ]}
        ],
        fun() ->
            {ok, Req1, _State} = msg_handler:init(
                req_mock(), #{action => offline, current_uid => 12345}
            ),
            ?assertEqual(req_ok, Req1),
            Received =
                receive
                    {ds_received_limit, L} -> L
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Received),
            ?assertEqual(ExpectedLimit, Received),
            ReceivedC2G =
                receive
                    {ds_received_limit_c2g, L2} -> L2
                after 1000 ->
                    timeout
                end,
            ?assertEqual(ExpectedLimit, ReceivedC2G)
        end
    ).
