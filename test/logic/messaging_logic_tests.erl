-module(messaging_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%% ARCH-01：offline/2(Req0,State) 已拆分为 handler 层解析 + 本函数纯参数
%% messaging_logic:offline/6，直接调用纯函数，不再需要 cowboy_req/响应 mock。
offline_returns_expected_shape_test_() ->
    ?WITH_MECKS(
        [
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
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_unread_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]}
        ],
        fun() ->
            Payload = messaging_logic:offline(12345, 1000, 0, 0, 0, <<>>),
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

route_ws_delegates_c2c_to_existing_logic_modules_test_() ->
    ?WITH_MECK(
        msg_c2c_logic,
        [
            {'c2c', 3, fun(MsgId, CurrentUid, Data) ->
                self() ! {c2c_routed, MsgId, CurrentUid, Data},
                ok
            end}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 1001,
            Data = #{<<"payload">> => #{<<"text">> => <<"hello">>}},
            Type = <<"C2C">>,
            OriginalMsg = <<"{\"payload\":{\"text\":\"hello\"}}">>,

            ?assertEqual(ok, messaging_logic:route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg)),
            ?assertEqual(
                {c2c_routed, MsgId, CurrentUid, Data},
                receive
                    Routed -> Routed
                after 1000 ->
                    timeout
                end
            )
        end
    ).

%% ARCH-01：offline_ack/2(Req0,State) 已拆分为 handler 层解析（含 type 小写化）
%% + 本函数纯参数 messaging_logic:offline_ack/4，Type 传入时已是小写。
offline_ack_uses_messaging_boundary_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
            ]},
            {msg_c2c_repo, [
                {'delete_by_msg_ids_and_to_id', 2, fun(MsgIds, 12345) ->
                    self() ! {acked_ids, MsgIds},
                    2
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = messaging_logic:offline_ack(
                12345, <<"c2c">>, [<<"m1">>, <<"m2">>], <<>>
            ),
            ?assertEqual(
                {acked_ids, [<<"m1">>, <<"m2">>]},
                receive
                    Acked -> Acked
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"c2c">>, maps:get(<<"type">>, Payload)),
            ?assertEqual(2, maps:get(<<"processed_count">>, Payload)),
            ?assertEqual(2, maps:get(<<"msg_ids_count">>, Payload))
        end
    ).

%% ARCH-01：reaction_add/2(Req0,State) 已拆分为 handler 层解析（elib_req:body）
%% + 本函数纯参数 messaging_logic:reaction_add/4。
reaction_add_uses_messaging_boundary_test_() ->
    ?WITH_MECK(
        msg_reaction_logic,
        [
            {'add', 4, fun(<<"msg-1">>, <<"c2c">>, 12345, Emoji) ->
                self() ! {reaction_added, Emoji},
                {ok, #{<<"user_id">> => 12345, <<"created_at">> => 1700000000}}
            end}
        ],
        fun() ->
            Emoji = <<240, 159, 145, 141>>,
            {ok, Payload, _Msg} = messaging_logic:reaction_add(
                12345, <<"msg-1">>, <<"c2c">>, Emoji
            ),
            ?assertEqual(
                {reaction_added, Emoji},
                receive
                    Added -> Added
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"msg-1">>, maps:get(<<"msg_id">>, Payload)),
            ?assertEqual(12345, maps:get(<<"user_id">>, Payload))
        end
    ).

%%%===================================================================
%%% history/5 增量同步（conv_seq range fetch）契约测试
%%% 覆盖：游标推进 / 重连幂等 / gap / 空结果 / archive 未开启降级 / 参数校验
%%%===================================================================

%% 正常 range fetch：next_seq 取最后一行 conv_seq，满页 has_more=true。
%% 服务端按 Limit+1 取数判定：mock 返回 3 行（> limit=2）→ has_more=true，
%% 且第 3 行（探测行）不随 messages 下发、next_seq 取截断后末行
history_range_fetch_advances_cursor_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(A, B) ->
                    <<"c2c:", (integer_to_binary(min(A, B)))/binary, ":",
                        (integer_to_binary(max(A, B)))/binary>>
                end},
                {'history', 3, fun(_ConvKey, AfterSeq, Limit) ->
                    ?assertEqual(5, AfterSeq),
                    ?assertEqual(3, Limit),
                    {ok, [
                        #{<<"conv_seq">> => 6, <<"from_id">> => 100, <<"to_id">> => 200},
                        #{<<"conv_seq">> => 7, <<"from_id">> => 200, <<"to_id">> => 100},
                        #{<<"conv_seq">> => 8, <<"from_id">> => 100, <<"to_id">> => 200}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(100, <<"c2c">>, <<"200">>, 5, 2),
            ?assertEqual(7, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(true, maps:get(<<"has_more">>, Res)),
            ?assertEqual(2, length(maps:get(<<"messages">>, Res))),
            % from_id/to_id 重命名为 from/to
            [First | _] = maps:get(<<"messages">>, Res),
            ?assertEqual(100, maps:get(<<"from">>, First)),
            ?assertEqual(false, maps:is_key(<<"from_id">>, First))
        end
    ).

%% 末页恰好满额：ds 不足 Limit+1 条（恰返回 Limit 条）→ has_more=false。
%% 旧判定 >= Limit 在此场景虚报 true，客户端多拉一次空页
history_exact_full_page_has_more_false_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(A, B) ->
                    <<"c2c:", (integer_to_binary(min(A, B)))/binary, ":",
                        (integer_to_binary(max(A, B)))/binary>>
                end},
                {'history', 3, fun(_ConvKey, _AfterSeq, _Limit) ->
                    {ok, [
                        #{<<"conv_seq">> => 6, <<"from_id">> => 100, <<"to_id">> => 200},
                        #{<<"conv_seq">> => 7, <<"from_id">> => 200, <<"to_id">> => 100}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(100, <<"c2c">>, <<"200">>, 5, 2),
            ?assertEqual(false, maps:get(<<"has_more">>, Res)),
            ?assertEqual(2, length(maps:get(<<"messages">>, Res))),
            ?assertEqual(7, maps:get(<<"next_seq">>, Res))
        end
    ).

%% 重连幂等：同一 after_seq 重复拉取结果一致（服务端无隐藏游标状态）
history_reconnect_idempotent_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(Gid) -> <<"c2g:", (integer_to_binary(Gid))/binary>> end},
                {'history', 3, fun(_ConvKey, _AfterSeq, _Limit) ->
                    {ok, [#{<<"conv_seq">> => 3, <<"from_id">> => 1, <<"group_id">> => 9}]}
                end}
            ]}
        ],
        fun() ->
            {ok, R1} = messaging_logic:history(1, <<"c2g">>, <<"9">>, 2, 10),
            {ok, R2} = messaging_logic:history(1, <<"c2g">>, <<"9">>, 2, 10),
            ?assertEqual(R1, R2),
            ?assertEqual(3, maps:get(<<"next_seq">>, R1)),
            % 未满页 has_more=false
            ?assertEqual(false, maps:get(<<"has_more">>, R1))
        end
    ).

%% gap 场景：存储侧 conv_seq 不连续（如 6,9,10），next_seq 取末行；
%% gap 检测是客户端职责（按 conv_seq 连续性判断），服务端如实返回
history_gap_rows_next_seq_is_last_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'history', 3, fun(_ConvKey, _AfterSeq, _Limit) ->
                    {ok, [
                        #{<<"conv_seq">> => 6, <<"from_id">> => 1, <<"to_id">> => 2},
                        #{<<"conv_seq">> => 9, <<"from_id">> => 2, <<"to_id">> => 1},
                        #{<<"conv_seq">> => 10, <<"from_id">> => 1, <<"to_id">> => 2}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(1, <<"c2c">>, <<"2">>, 5, 10),
            ?assertEqual(10, maps:get(<<"next_seq">>, Res)),
            Seqs = [maps:get(<<"conv_seq">>, M) || M <- maps:get(<<"messages">>, Res)],
            ?assertEqual([6, 9, 10], Seqs)
        end
    ).

%% 空结果：游标不回退（next_seq 保持 after_seq）
history_empty_keeps_cursor_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'history', 3, fun(_ConvKey, _AfterSeq, _Limit) -> {ok, []} end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(1, <<"c2c">>, <<"2">>, 42, 10),
            ?assertEqual(42, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(false, maps:get(<<"has_more">>, Res)),
            ?assertEqual([], maps:get(<<"messages">>, Res))
        end
    ).

%% archive 未开启：DS 报错时返回 500 级错误与可读提示
history_archive_disabled_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'history', 3, fun(_ConvKey, _AfterSeq, _Limit) -> {error, disabled} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, _, ?ERR_INTERNAL_SERVER_ERROR},
                messaging_logic:history(1, <<"c2c">>, <<"2">>, 0, 10)
            )
        end
    ).

%% 参数校验：非法 chat_type / 缺 peer_id 均 400
history_param_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch(
            {error, _, ?ERR_BAD_REQUEST},
            messaging_logic:history(1, <<"c2x">>, <<"2">>, 0, 10)
        ),
        ?assertMatch(
            {error, _, ?ERR_BAD_REQUEST},
            messaging_logic:history(1, <<"c2c">>, <<>>, 0, 10)
        )
    end).
